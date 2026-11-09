;;;; cell.lisp --- gemini_image, gemini_video, gemini_models and /gemini-media.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One API root the operator names, in the `gemini-media' section of the
;;;; shared config:
;;;;   "gemini-media": {"api_url": "https://generativelanguage.googleapis.com/v1beta",
;;;;                     "api_key_env": "GEMINI_API_KEY"}
;;;; Everything the cell does is one POST to <api_url>/interactions (or one GET
;;;; of <api_url>/models), and one file written under output_dir. Nothing here
;;;; imports a vendor SDK: the Gemini API speaks HTTP, and NLK:HTTP is the one
;;;; seam.
;;;;
;;;; Rewritten from bytecakelake's Hermes Agent plugin (see NOTICE): the two
;;;; interactions calls and the fields each carries, the aspect / size /
;;;; resolution / duration rules and their adjustments, the model catalog
;;;; behind the picker, the reference-image rules (magic bytes, 15 MB each,
;;;; 18 MB together) and the error classes and hints. The google-genai SDK and
;;;; Hermes's image_generate / video_generate lanes are not here: the same
;;;; calls are three tools and one command.

(in-package #:nodecode-gemini-media)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Gemini media is available through the nodecode-gemini-media cell: the gemini_image and
gemini_video tools, the gemini_models tool that says which models the key offers, and the
/gemini-media command. A person can call the same functions from eval in the gemini-media
package.
  gemini_image  {prompt, aspect, image_size, model, images}
      One POST to the Gemini API's interactions endpoint, one picture written under the cell's
      output_dir. aspect is landscape (16:9), square (1:1) or portrait (9:16); image_size is
      auto, 1K, 2K or 4K, and a model that renders one size only is sent that one. The answer
      names the file, its size in bytes, its pixel size, the model, the aspect ratio sent and
      how many reference pictures were used. Say where the file is; do not paste its bytes.
      images names up to 14 reference pictures - local paths, https URLs, data: URIs - to edit
      one, keep a character, or combine products; PNG, JPEG, WebP, HEIC or HEIF, 15 MB each.
  gemini_video  {prompt, aspect, resolution, duration, model, image, images}
      The same endpoint with a video response format: an MP4 under output_dir, 3 to 10 seconds,
      16:9 or 9:16, 360p to 4k, with Gemini's native audio. image makes that picture the first
      frame; images makes the character in them the one that moves; up to 6 in all. The render
      is synchronous and slow - tens of seconds to a few minutes - so say it is rendering
      before calling. The answer names the file, its bytes, its real duration read out of the
      MP4, and the task the API ran: text_to_video, image_to_video or reference_to_video.
  gemini_models  {kind}
      The image and video models this key can call, from the key's own model list, with the
      built-in list behind it when the list cannot be read.
Write prompts the way these models read them: for a picture, subject, setting, style, light,
framing; for a video, what happens, the camera move, and the light. A failed call says the
HTTP status, Gemini's own message and a hint: a wrong key, a model this key cannot use, a rate
limit, a safety refusal, or a render that outlasted the cell's timeout. From the operator's
side, /gemini-media takes the same arguments: /gemini-media a red fox in snow --landscape;
/gemini-media --video --seconds 6 a fox crossing a snowy field, slow push in;
/gemini-media --video --image ~/pictures/fox.png make the fox turn and look at us. Typed alone
it asks for the prompt."
  "What (help :gemini-media) answers while the cell runs.")

;;; --- what the Gemini API takes ------------------------------------------------

(defparameter +default-api-url+ "https://generativelanguage.googleapis.com/v1beta"
  "The Gemini API root the original plugin talks to. A section that names another root is a
stand-in, and is sent the same paths.")

(defparameter +image-aspects+
  '(("landscape" . "16:9") ("square" . "1:1") ("portrait" . "9:16"))
  "The three aspects a caller may name, to the ratio Gemini takes: the same three-way mapping
the original plugin made for another agent's semantic aspects.")

(defparameter +video-aspects+ '(("landscape" . "16:9") ("portrait" . "9:16"))
  "Gemini video renders 16:9 or 9:16 and no other shape.")

(defparameter +image-sizes+ '("1K" "2K" "4K")
  "The image sizes the API names.")

(defparameter +resolutions+ '("360p" "720p" "1080p" "4k")
  "The video resolutions, smallest first: a value between two is rounded up.")

(defparameter +size-limits+
  '(("gemini-nano-banana-2.1" . ("1K" "2K" "4K"))
    ("gemini-3.1-flash-image" . ("1K" "2K" "4K"))
    ("gemini-3.1-flash-lite-image" . ("1K")))
  "The sizes each curated model renders, as the original plugin's table had them. A model not
here gets the size it is asked for, because nothing states its limits.")

(defparameter +min-duration+ 3 "The shortest video Gemini renders, in seconds.")

(defparameter +max-duration+ 10 "The longest video Gemini renders, in seconds.")

(defparameter +image-max-references+ 14
  "The most reference pictures one image request sends: the original plugin's cap.")

(defparameter +video-max-references+ 6
  "The most reference pictures one video request sends, measured by the original plugin.")

(defparameter +max-image-bytes+ (* 15 1024 1024)
  "The most one reference picture may be, in bytes: the API's inline part limit.")

(defparameter +max-total-bytes+ (* 18 1024 1024)
  "The most all reference pictures together may be: the request body's limit.")

(defparameter +error-chars+ 300
  "How much of Gemini's own error text a refusal quotes back.")

(defparameter +user-agent+ "nodecode-gemini-media/0.1.0"
  "What the cell calls itself to the API. Nothing of another client's is impersonated.")

(defparameter +catalog-ttl+ (* 24 3600)
  "How long a model list is kept, in seconds: the original plugin's cache window.")

(defparameter +block-words+ '("safety" "blocked" "prohibited" "policy"
                              "cannot" "can't" "unable" "refus")
  "What a model's text says when it refused to render: the original plugin's list.")

;;; --- the model catalog ----------------------------------------------------------

(defparameter +curated-models+
  '(("gemini-nano-banana-2.1" "image" "Nano Banana 2.1"
     "Current workhorse: fast, 1K/2K/4K, Google Search grounding, multi-reference")
    ("gemini-3.1-flash-lite-image" "image" "Nano Banana 2 Lite"
     "Fastest and cheapest; 1K only; weak with many references")
    ("gemini-3.1-flash-image" "image" "Nano Banana 2"
     "Previous-generation workhorse; 512px to 4K")
    ("gemini-3-pro-image" "image" "Nano Banana Pro"
     "Premium: complex scenes, text rendering, highest world knowledge")
    ("gemini-2.5-flash-image" "image" "Nano Banana (legacy)"
     "Legacy model; prefer Nano Banana 2.1 or 2 Lite")
    ("gemini-omni-1.1-flash" "video" "Gemini Omni 1.1 Flash"
     "Text/image/reference-to-video with native audio; 16:9 and 9:16; 360p-4K"))
  "The curated table the original plugin shipped: what its picker offers when the live model
list cannot be read, and the order it puts the live one in.")

(defparameter +model-excludes+ '("tts" "embed" "live" "aqa" "audio" "robotics"
                                "computer-use" "learnlm" "gemma")
  "What the original plugin keeps out of the media picker, by id.")

(defparameter +generate-actions+ '("generatecontent" "predict" "predictlongrunning")
  "The model actions that say a model generates something.")

(defvar *catalog* nil
  "The last live model list read, as (FETCHED-AT . ROWS), and NIL before the first read.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (gemini-media-error (condition)
       (nle:failure (format nil "ERROR: ~a" (gemini-media-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: gemini-media: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun contains-p (text fragment)
  (and (stringp text) (search fragment text :test #'char-equal)))

(defun strip-model-prefix (name)
  "NAME without the API's models/ prefix."
  (if (and (stringp name) (uiop:string-prefix-p "models/" name))
      (subseq name 7)
      name))

;;; --- reading the section ---------------------------------------------------------

(defun url-parts (url)
  "(values SCHEME HOST PATH) of an absolute URL, scheme and host lowercased, or NILs."
  (let ((mark (and (stringp url) (search "://" url))))
    (when (and mark (plusp mark))
      (let* ((scheme (string-downcase (subseq url 0 mark)))
             (rest (subseq url (+ mark 3)))
             (end (or (position-if (lambda (character) (find character "/?#")) rest)
                      (length rest)))
             (authority (subseq rest 0 end))
             (host (subseq authority 0 (or (position #\: authority) (length authority)))))
        (values scheme (string-downcase host) (subseq rest end))))))

(defun loopback-host-p (host)
  "True for the hosts a plain-HTTP stand-in may live at: loopback literals and localhost."
  (let ((host (string-downcase (or host ""))))
    (or (member host '("localhost" "127.0.0.1" "::1" "[::1]") :test #'string=)
        (uiop:string-prefix-p "127." host))))

(defun check-api-url (url)
  "URL, without its trailing slash, or a refusal saying what is wrong with it. A bare host
gains the /v1beta the Gemini API answers on."
  (let ((url (nlk:trimmed (or url ""))))
    (when (zerop (length url))
      (nlk:config-error "api_url is empty: set it to the Gemini API root, ~a" +default-api-url+))
    (when (find-if (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return))) url)
      (nlk:config-error "api_url ~s holds whitespace" url))
    (multiple-value-bind (scheme host path) (url-parts url)
      (unless scheme
        (nlk:config-error "api_url ~s is not an absolute URL: give the scheme too, e.g. ~a"
                          url +default-api-url+))
      (unless (member scheme '("http" "https") :test #'string=)
        (nlk:config-error "api_url scheme ~s is not http or https" scheme))
      (when (and (string= scheme "http") (not (loopback-host-p host)))
        (nlk:config-error "api_url ~s is plain HTTP and ~a is not loopback: the key would cross the public internet in clear text. Use https://" url host))
      (let ((url (string-right-trim "/" url)))
        (if (and path (plusp (length (string-left-trim "/" path))))
            url
            (concatenate 'string url "/v1beta"))))))

(defun read-settings (values table)
  "The declared VALUES, plus the key the section names and the folder the media is written
to: read once, here."
  (declare (ignore table))
  (let* ((plist (copy-list values))
         (env (nonblank (getf values :api-key-env)))
         (file (nonblank (getf values :api-key-file))))
    (when (and env file)
      (nlk:config-error "set one of api_key_env and api_key_file, not both"))
    (setf (getf plist :api-url) (check-api-url (or (getf values :api-url) ""))
          (getf plist :output-dir)
          (uiop:ensure-directory-pathname
           (nlk:expand-home (or (nonblank (getf values :output-dir)) "~/.nodecode/gemini-media")))
          (getf plist :api-key)
          (cond ((nonblank (getf values :api-key)) (nonblank (getf values :api-key)))
                (file (or (nonblank (ignore-errors
                                     (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "api_key_file ~a cannot be read, or is empty" file)))
                (t nil)))
    plist))

(defun api-key ()
  "The key: the section's own, the variable it names, or GOOGLE_API_KEY - the fallback the
original plugin accepts."
  (let ((named (or (nonblank (setting :api-key-env)) "GEMINI_API_KEY")))
    (or (nonblank (setting :api-key))
        (ignore-errors (nonblank (nle::credential-env named)))
        (and (not (string= named "GOOGLE_API_KEY"))
             (ignore-errors (nonblank (nle::credential-env "GOOGLE_API_KEY"))))
        (fail "no Gemini API key: set ~a in the environment, or paste a key into the cell's api_key in /settings (one from https://aistudio.google.com/apikey)"
              named))))

(defun api-url (path)
  (concatenate 'string (setting :api-url) path))

;;; --- one exchange with the API -----------------------------------------------------

(defun exchange (method url &key content timeout binary)
  "One exchange with the Gemini API => (values BODY STATUS). The key rides as
x-goog-api-key and nowhere else; it is never put in a URL or a log line."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http method url
                    :headers (list (cons "x-goog-api-key" (api-key))
                                   (cons "user-agent" +user-agent+)
                                   (cons "accept" (if binary "*/*" "application/json"))
                                   (cons "content-type" "application/json"))
                    :content content
                    :binary binary
                    :timeout timeout
                    :connect-timeout (min 20 timeout))
        (values body status))
    (gemini-media-error (condition)
      (fail "~a" (gemini-media-error-detail condition)))
    (error (condition)
      (fail "~(~a~) ~a did not answer: ~a" method url
            (nlk:one-line (princ-to-string condition))))))

(defun post-interaction (payload timeout)
  "One POST to the interactions endpoint => (values BODY STATUS)."
  (exchange :post (api-url "/interactions")
            :content (nlk:encode-json-object payload)
            :timeout timeout))

(defun status-hint (status)
  "The parenthetical hint the model can act on, or NIL."
  (case status
    (400 " (the request's shape was refused: check the aspect, size, resolution and duration the model takes)")
    ((401 403) " (check the key in the variable api_key_env names, and that the Gemini API is enabled for its project)")
    (404 " (that model id does not exist for this key: call gemini_models)")
    (429 " (the key's quota is spent or rate limited: retry later, or raise it in AI Studio)")
    ((500 503) " (Gemini's side failed the render: try once more)")
    (t nil)))

(defun error-message (body)
  "Gemini's own message out of a failed answer."
  (let* ((text (and (stringp body) body))
         (object (and text (plusp (length (nlk:trimmed text)))
                      (ignore-errors (nlk:decode-json text))))
         (inner (and (hash-table-p object) (nlk:json-value object :object "error"))))
    (or (and (hash-table-p inner) (nlk:json-value inner :string "message"))
        (and (hash-table-p object) (nlk:json-value object :string "message"))
        (and text (plusp (length (nlk:trimmed text)))
             (nlk:clip (nlk:one-line text) +error-chars+))
        "no error message")))

(defun replace-text (text part replacement)
  "TEXT with every occurrence of PART replaced, so a key the API echoes back is never shown."
  (if (or (null part) (zerop (length part)) (null (search part text)))
      text
      (with-output-to-string (out)
        (loop with start = 0
              for at = (search part text :start2 start)
              while at
              do (write-string text out :start start :end at)
                 (write-string replacement out)
                 (setf start (+ at (length part)))
              finally (write-string text out :start start)))))

(defun redact (text)
  "TEXT with the key, when there is one, replaced: the API echoes a bad key back."
  (replace-text text (ignore-errors (api-key)) "[key]"))

(defun api-refusal (status body)
  "A failed answer as a refusal: the status, Gemini's words, and the hint that fixes it."
  (fail "the Gemini API answered HTTP ~a: ~a~@[~a~]"
        status (redact (error-message body)) (status-hint status)))

(defun decode-object (body what)
  "BODY decoded as one JSON object, or a refusal naming WHAT."
  (let ((object (and (stringp body) (plusp (length (nlk:trimmed body)))
                      (ignore-errors (nlk:decode-json body)))))
    (unless (hash-table-p object)
      (fail "~a answered something that is not a JSON object" what))
    object))

;;; --- reference pictures ----------------------------------------------------------

(defun octet-string (octets start end)
  (map 'string #'code-char (subseq octets start end)))

(defun image-mime (octets)
  "The MIME type the first bytes declare - image/png, image/jpeg, image/webp, image/heic,
image/heif - or NIL: the original plugin sniffs the same five, and nothing it did not
recognise as a picture is ever read whole or sent."
  (let ((size (length octets)))
    (cond ((and (>= size 8)
                (= (aref octets 0) #x89) (= (aref octets 1) 80)
                (= (aref octets 2) 78) (= (aref octets 3) 71))
           "image/png")
          ((and (>= size 3) (= (aref octets 0) #xFF) (= (aref octets 1) #xD8)
                (= (aref octets 2) #xFF))
           "image/jpeg")
          ((and (>= size 12) (string= "RIFF" (octet-string octets 0 4))
                (string= "WEBP" (octet-string octets 8 12)))
           "image/webp")
          ((and (>= size 12) (string= "ftyp" (octet-string octets 4 8)))
           (let ((brand (octet-string octets 8 12)))
             (cond ((member brand '("heic" "heix" "hevc" "hevx") :test #'string=)
                    "image/heic")
                   ((member brand '("mif1" "msf1" "heim" "heis") :test #'string=)
                    "image/heif")))))))

(defun private-address-p (host)
  "True for a host literal only a local network answers on: the addresses the original
plugin refused to download a reference picture from."
  (let ((host (string-downcase (or host ""))))
    (or (loopback-host-p host)
        (uiop:string-prefix-p "10." host)
        (uiop:string-prefix-p "192.168." host)
        (uiop:string-prefix-p "169.254." host)
        (uiop:string-prefix-p "0." host)
        (and (uiop:string-prefix-p "172." host)
             (let ((second (ignore-errors
                            (parse-integer host :start 4
                                           :end (or (position #\. host :start 4)
                                                    (length host))))))
               (and second (<= 16 second) (<= second 31))))
        (uiop:string-prefix-p "fd" host)
        (uiop:string-prefix-p "fc" host))))

(defun data-uri (source)
  "SOURCE's bytes when it is a base64 data: URI, else NIL."
  (let* ((comma (position #\, source))
         (header (and comma (subseq source 0 comma))))
    (when (and header (contains-p header "base64"))
      (let ((octets (ignore-errors
                     (cl-base64:base64-string-to-usb8-array (subseq source (1+ comma))))))
        (or octets (fail "the reference picture's data: URI is not valid base64"))))))

(defun download-octets (url)
  "URL's bytes, refused from a private host or over 15 MB: the original plugin's guard."
  (multiple-value-bind (scheme host) (url-parts url)
    (unless (member scheme '("http" "https") :test #'string=)
      (fail "the reference picture URL ~s is not http or https" url))
    (when (private-address-p host)
      (fail "the reference picture URL ~s names a private or loopback address" url)))
  (multiple-value-bind (body status) (exchange :get url :binary t :timeout 30)
    (unless (and (integerp status) (< status 400))
      (fail "the reference picture URL ~a answered HTTP ~a" url status))
    (unless (typep body '(vector (unsigned-byte 8)))
      (fail "the reference picture URL ~a answered no bytes" url))
    (when (> (length body) +max-image-bytes+)
      (fail "the reference picture ~a is larger than 15 MB" url))
    body))

(defun local-octets (source)
  "SOURCE read from disk, refused unless a supported picture of at most 15 MB."
  (let ((path (nlk:expand-home source)))
    (unless (probe-file path)
      (fail "the reference picture ~a does not exist" source))
    (handler-case
        (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
          (let* ((declared (file-length in))
                 (size (if declared (min (1+ +max-image-bytes+) declared)
                           (1+ +max-image-bytes+)))
                 (octets (make-array size :element-type '(unsigned-byte 8)))
                 (read (read-sequence octets in)))
            (when (> read +max-image-bytes+)
              (fail "the reference picture ~a is larger than 15 MB" source))
            (subseq octets 0 read)))
      (error (condition)
        (fail "the reference picture ~a cannot be read: ~a" source
              (nlk:one-line (princ-to-string condition)))))))

(defun reference-picture (source)
  "(values MIME BASE64 BYTES) of one reference picture, or a refusal."
  (let* ((source (nlk:trimmed source))
         (octets (cond ((uiop:string-prefix-p "data:" source) (data-uri source))
                       ((or (uiop:string-prefix-p "https://" source)
                            (uiop:string-prefix-p "http://" source))
                        (download-octets source))
                       (t (local-octets source))))
         (mime (and octets (image-mime octets))))
    (unless mime
      (fail "the reference picture ~a is not a PNG, JPEG, WebP, HEIC or HEIF image" source))
    (values mime (cl-base64:usb8-array-to-base64-string octets) (length octets))))

(defun reference-pictures (sources limit)
  "(values PICTURES DROPPED): at most LIMIT of SOURCES, each as (MIME BASE64), and how many
were left out - the cap the original plugin applied rather than an error."
  (let ((sources (remove-if-not #'nonblank
                                 (mapcar (lambda (source) (and source (princ-to-string source)))
                                         (or sources '())))))
    (values (loop with total = 0
                  for source in (subseq sources 0 (min limit (length sources)))
                  collect (multiple-value-bind (mime encoded bytes) (reference-picture source)
                            (incf total bytes)
                            (when (> total +max-total-bytes+)
                              (fail "the reference pictures together are larger than 18 MB"))
                            (list mime encoded)))
            (max 0 (- (length sources) limit)))))

;;; --- the bytes that come back -------------------------------------------------------

(defun big-endian (octets at bytes)
  (loop with value = 0
        for index from 0 below bytes
        do (setf value (+ (* value 256) (aref octets (+ at index))))
        finally (return value)))

(defun png-size (octets)
  (when (and (>= (length octets) 24) (string= "IHDR" (octet-string octets 12 16)))
    (cons (big-endian octets 16 4) (big-endian octets 20 4))))

(defun jpeg-size (octets)
  "The width and height in OCTETS' first SOF segment, or NIL."
  (let ((index 2) (size (length octets)))
    (loop while (< (+ index 9) size)
          do (if (/= (aref octets index) #xFF)
                 (incf index)
                 (let ((marker (aref octets (1+ index))))
                   (cond ((or (= marker #xD8) (= marker #xFF)) (incf index 2))
                         ((<= #xD0 marker #xD9) (incf index 2))
                         ((and (>= marker #xC0) (<= marker #xCF)
                               (not (member marker '(#xC4 #xC8 #xCC))))
                          (return (cons (big-endian octets (+ index 7) 2)
                                        (big-endian octets (+ index 5) 2))))
                         (t (incf index (+ 2 (big-endian octets (+ index 2) 2))))))))))

(defun image-pixels (octets mime)
  "The picture's WxH when its header says, else NIL."
  (let ((pair (cond ((equal mime "image/png") (png-size octets))
                    ((equal mime "image/jpeg") (jpeg-size octets)))))
    (and pair (format nil "~dx~d" (car pair) (cdr pair)))))

(defun mp4-duration (octets)
  "OCTETS' video length in whole seconds, from the MP4's mvhd atom, or NIL - the same
reading the original plugin took."
  (let ((at (search (map '(vector (unsigned-byte 8)) #'char-code "mvhd") octets)))
    (when (and at (< (+ at 36) (length octets)))
      (multiple-value-bind (timescale duration)
          (if (zerop (aref octets (+ at 4)))
              (values (big-endian octets (+ at 16) 4) (big-endian octets (+ at 20) 4))
              (values (big-endian octets (+ at 24) 4) (big-endian octets (+ at 28) 8)))
        (and (plusp timescale) (round duration timescale))))))

(defun extension-for (mime)
  (cond ((equal mime "image/png") "png")
        ((equal mime "image/jpeg") "jpg")
        ((equal mime "image/webp") "webp")
        ((equal mime "video/mp4") "mp4")
        (t "bin")))

(defun save-octets (octets mime &key (stem "gemini"))
  "(values PATH BYTES PIXELS DURATION): OCTETS written under the cell's output_dir, never
over another file there."
  (let ((directory (setting :output-dir)))
    (ensure-directories-exist (merge-pathnames "keep" directory))
    (let ((file (loop for attempt from 0
                       for candidate = (merge-pathnames
                                        (format nil "~a-~d~@[-~d~].~a" stem
                                                (get-universal-time)
                                                (and (plusp attempt) attempt)
                                                (extension-for mime))
                                        directory)
                       unless (probe-file candidate)
                         return candidate)))
      (alexandria:write-byte-vector-into-file octets file :if-exists :supersede)
      (values (namestring file) (length octets)
              (and (uiop:string-prefix-p "image/" mime) (image-pixels octets mime))
              (and (equal mime "video/mp4") (mp4-duration octets))))))

(defun answer-media (object field what)
  "(values OCTETS MIME) of the media the answer carries in FIELD, or a refusal: a model
that rendered nothing says why in its text, and a blocked one says so in words."
  (let* ((output (nlk:json-value object :object field))
         (data (and output (nlk:json-value output :string "data"))))
    (unless data
      (let* ((text (nlk:trimmed (or (nlk:json-value object :string "output_text") "")))
             (status (or (nlk:json-value object :string "status") ""))
             (blocked (or (some (lambda (word) (contains-p text word)) +block-words+)
                          (member status '("failed" "cancelled") :test #'string=)))
             (detail (if (plusp (length text))
                         (nlk:clip text +error-chars+)
                         (format nil "the model returned no ~a~@[ (status: ~a)~]"
                                 what (and (plusp (length status)) status)))))
        (fail "~a~@[ (the model blocked or failed the render)~]" detail (and blocked t))))
    (let ((octets (ignore-errors (cl-base64:base64-string-to-usb8-array data))))
      (unless octets
        (fail "the answer's ~a data is not base64" what))
      (values octets (or (nlk:json-value output :string "mime_type")
                         "application/octet-stream")))))

;;; --- one picture -------------------------------------------------------------------

(defun aspect-ratio (aspect table what)
  "The ratio TABLE maps ASPECT to, or a refusal listing what is allowed."
  (or (cdr (assoc (string-downcase (nlk:trimmed (or aspect ""))) table :test #'string=))
      (fail "~a must be one of ~{~a~^, ~}: ~s" what (mapcar #'car table) aspect)))

(defun image-size-for (model size)
  "(values SIZE ADJUSTED-P): the size the model renders, and whether the one asked for had
to move. auto sends none, and a model the table does not know is sent what was asked."
  (let ((size (and (nonblank size) (string-upcase (nlk:trimmed size)))))
    (cond ((or (null size) (string= size "AUTO")) (values nil nil))
          (t (let ((allowed (cdr (assoc model +size-limits+ :test #'string=))))
               (cond ((null allowed) (values size nil))
                     ((member size allowed :test #'string=) (values size nil))
                     (t (values (first allowed) t))))))))

(defun image-input (prompt pictures)
  "PROMPT alone, or the pictures as inline parts before it."
  (if (null pictures)
      prompt
      (coerce (append (loop for (mime encoded) in pictures
                             collect (nlk:json-object "type" "image" "data" encoded
                                                       "mime_type" mime))
                      (list (nlk:json-object "type" "text" "text" prompt)))
              'vector)))

(defun render-image (prompt &key aspect image-size model images)
  "One image => the answer object: the file, its size, the model, the aspect and size sent,
and the pictures used."
  (let* ((prompt (nlk:trimmed (or prompt "")))
         (model (or (nonblank model) (nonblank (setting :image-model)) "gemini-nano-banana-2.1"))
         (aspect (string-downcase (or (nonblank aspect) "square")))
         (ratio (aspect-ratio aspect +image-aspects+ "aspect")))
    (when (zerop (length prompt))
      (fail "a prompt is required and cannot be empty"))
    (multiple-value-bind (pictures dropped) (reference-pictures images +image-max-references+)
      (multiple-value-bind (size adjusted) (image-size-for model image-size)
        (let* ((format (nlk:make-json-object "type" "image" "aspect_ratio" ratio))
               (payload (nlk:make-json-object "model" model
                                               "input" (image-input prompt pictures)
                                               "response_format" format)))
          (when size
            (setf (gethash "image_size" format) size))
          (multiple-value-bind (body status) (post-interaction payload (setting :image-timeout))
            (unless (and (integerp status) (<= 200 status 299))
              (api-refusal status body))
            (let ((object (decode-object body "the Gemini API")))
              (multiple-value-bind (octets mime) (answer-media object "output_image" "image")
                (multiple-value-bind (path bytes pixels duration)
                    (save-octets octets mime :stem "gemini-image")
                  (declare (ignore duration))
                  (let ((answer (nlk:json-object "image" path "bytes" bytes
                                                 "model" model "aspect" aspect
                                                 "aspect_ratio" ratio
                                                 "mode" (if pictures "edit" "generate")
                                                 "mime_type" mime)))
                    (when pixels (setf (gethash "pixels" answer) pixels))
                    (when size (setf (gethash "image_size" answer) size))
                    (when adjusted (setf (gethash "image_size_adjusted" answer) t))
                    (when (plusp dropped) (setf (gethash "references_dropped" answer) dropped))
                    (when pictures (setf (gethash "references_used" answer) (length pictures)))
                    (let ((id (nlk:json-value object :string "id")))
                      (when id (setf (gethash "interaction_id" answer) id)))
                    answer))))))))))

;;; --- one video -----------------------------------------------------------------------

(defparameter +resolution-pixels+ '(("360p" . 360) ("720p" . 720) ("1080p" . 1080) ("4k" . 2160))
  "What each supported resolution means, so a value between two rounds up.")

(defun video-resolution (resolution)
  "(values RESOLUTION ADJUSTED-P): a supported resolution, rounded up from what was asked."
  (let ((text (string-downcase (nlk:trimmed (or resolution "")))))
    (cond ((member text +resolutions+ :test #'string=) (values text nil))
          ((member text '("2k" "uhd" "2160p") :test #'string=) (values "4k" t))
          (t (let* ((digits (and (plusp (length text))
                                  (ignore-errors (parse-integer text :junk-allowed t))))
                    (label (or (loop for (name . pixels) in +resolution-pixels+
                                     when (or (null digits) (<= digits pixels))
                                       return name)
                               "4k")))
               (values label t))))))

(defun video-duration (duration)
  "(values SECONDS ADJUSTED-P): 3 to 10 seconds, or NIL when unset."
  (cond ((or (null duration) (and (numberp duration) (zerop duration))) (values nil nil))
        ((not (numberp duration)) (values +max-duration+ t))
        (t (let ((seconds (max +min-duration+ (min +max-duration+ (round duration)))))
             (values seconds (/= seconds duration))))))

(defun video-task (pictures primary-p)
  "The task the API runs for these pictures: the original plugin's routing."
  (cond ((null pictures) "text_to_video")
        ((and primary-p (= 1 (length pictures))) "image_to_video")
        (t "reference_to_video")))

(defun render-video (prompt &key aspect resolution duration model image images)
  "One video => the answer object: the file, its bytes, its real length, and the task run."
  (let* ((prompt (nlk:trimmed (or prompt "")))
         (model (or (nonblank model) (nonblank (setting :video-model)) "gemini-omni-1.1-flash"))
         (aspect (string-downcase (or (nonblank aspect) "landscape")))
         (ratio (aspect-ratio aspect +video-aspects+ "aspect"))
         (primary (nonblank image)))
    (when (zerop (length prompt))
      (fail "a prompt is required and cannot be empty"))
    (multiple-value-bind (pictures dropped)
        (reference-pictures (if primary (cons primary (remove nil (or images '()))) images)
                            +video-max-references+)
      (multiple-value-bind (res res-adjusted) (video-resolution resolution)
        (multiple-value-bind (seconds seconds-adjusted) (video-duration duration)
          (let* ((task (video-task pictures (and primary t)))
                 (format (nlk:make-json-object "type" "video"
                                                "aspect_ratio" ratio
                                                "resolution" res))
                 (payload (nlk:make-json-object "model" model
                                                 "input" (image-input prompt pictures)
                                                 "response_format" format)))
            (when seconds
              (setf (gethash "duration" format) (format nil "~ds" seconds)))
            (when pictures
              (setf (gethash "generation_config" payload)
                    (nlk:json-object "video_config" (nlk:json-object "task" task))))
            (multiple-value-bind (body status) (post-interaction payload (setting :video-timeout))
              (unless (and (integerp status) (<= 200 status 299))
                (api-refusal status body))
              (let ((object (decode-object body "the Gemini API")))
                (multiple-value-bind (octets mime) (answer-media object "output_video" "video")
                  (let ((mime (if (equal mime "application/octet-stream") "video/mp4" mime)))
                    (multiple-value-bind (path bytes pixels actual)
                        (save-octets octets mime :stem "gemini-video")
                      (declare (ignore pixels))
                      (let ((answer (nlk:json-object "video" path "bytes" bytes
                                                     "model" model "aspect" aspect
                                                     "aspect_ratio" ratio
                                                     "resolution" res "task" task
                                                     "duration" (or actual seconds 0)
                                                     "audio" "native"
                                                     "mime_type" mime)))
                        (when res-adjusted
                          (setf (gethash "resolution_adjusted" answer) t))
                        (when seconds-adjusted
                          (setf (gethash "duration_adjusted" answer) t))
                        (when (plusp dropped)
                          (setf (gethash "references_dropped" answer) dropped))
                        (when pictures
                          (setf (gethash "references_used" answer) (length pictures)))
                        (let ((id (nlk:json-value object :string "id")))
                          (when id (setf (gethash "interaction_id" answer) id)))
                        answer))))))))))))

;;; --- the model list ------------------------------------------------------------------

(defun model-kind (id actions)
  "image, video or NIL for one model row: the original plugin's classifier."
  (let ((id (string-downcase id)))
    (when (and (notany (lambda (word) (contains-p id word)) +model-excludes+)
               (or (null actions)
                   (some (lambda (action)
                           (member (string-downcase (princ-to-string action))
                                   +generate-actions+ :test #'string=))
                         actions)))
      (cond ((or (contains-p id "omni") (contains-p id "veo")) "video")
            ((or (contains-p id "image") (contains-p id "nano-banana")
                 (contains-p id "imagen"))
             "image")))))

(defun model-actions (item)
  "The actions a model row lists, under any of the names the API has used for them, or NIL
when the row names none."
  (loop for key in '("supportedGenerationMethods" "supportedActions" "supported_actions")
        for actions = (nlk:json-array item key)
        when (plusp (length actions))
          return actions))

(defun parse-models (body)
  "BODY's model rows as (ID KIND DISPLAY STRENGTHS RANK), curated order first."
  (let ((object (decode-object body "the Gemini model list"))
        (rows '()))
    (loop for item across (or (nlk:json-array object "models") #())
          for id = (and (hash-table-p item)
                        (strip-model-prefix (nlk:json-value item :string "name")))
          for actions = (and id (model-actions item))
          for kind = (and id (model-kind id actions))
          when kind
            do (let* ((curated (assoc id +curated-models+ :test #'string=))
                      (display (or (and curated (third curated))
                                   (nlk:json-value item :string "displayName")
                                   id)))
                 (push (list id kind display
                             (or (and curated (fourth curated)) "")
                             (or (and curated (position curated +curated-models+)) 1000))
                       rows)))
    (sort rows (lambda (left right)
                 (if (/= (fifth left) (fifth right))
                     (< (fifth left) (fifth right))
                     (string< (first left) (first right)))))))

(defun curated-rows ()
  (loop for (id kind display strengths) in +curated-models+
        for rank from 0
        collect (list id kind display strengths rank)))

(defun media-models ()
  "(values ROWS SOURCE): the key's live model list, kept a day, else the curated table."
  (let ((now (get-universal-time)))
    (when (and *catalog* (<= (- now (car *catalog*)) +catalog-ttl+))
      (return-from media-models (values (cdr *catalog*) "memory")))
    (handler-case
        (multiple-value-bind (body status)
            (exchange :get (api-url "/models?pageSize=1000") :binary nil :timeout 10)
          (unless (and (integerp status) (<= 200 status 299))
            (api-refusal status body))
          (let ((rows (parse-models body)))
            (if rows
                (progn (setf *catalog* (cons now rows))
                       (values rows "live"))
                (values (curated-rows) "curated"))))
      (error () (values (curated-rows) "curated")))))

;;; --- the tools ---------------------------------------------------------------------

(defun gemini-image-tool (request)
  "The gemini_image tool: one picture, guided by reference pictures when they are named."
  (answering
    (nlk:encode-json-object
     (render-image (image-request-prompt request)
                   :aspect (image-request-aspect request)
                   :image-size (image-request-image-size request)
                   :model (image-request-model request)
                   :images (coerce (image-request-images request) 'list)))))

(defun gemini-video-tool (request)
  "The gemini_video tool: one video, from a prompt, a first frame or reference pictures."
  (answering
    (nlk:encode-json-object
     (render-video (video-request-prompt request)
                   :aspect (video-request-aspect request)
                   :resolution (video-request-resolution request)
                   :duration (video-request-duration request)
                   :model (video-request-model request)
                   :image (video-request-image request)
                   :images (coerce (video-request-images request) 'list)))))

(defun gemini-models-tool (request)
  "The gemini_models tool: which image and video models this key offers."
  (answering
    (nlk:encode-json-object
     (let ((kind (or (nonblank (model-list-request-kind request)) "all"))
           (answer (nlk:json-object "image" #() "video" #())))
       (multiple-value-bind (rows source) (media-models)
         (setf (gethash "source" answer) source)
         (loop for row in rows
               for wanted = (second row)
               when (or (string= kind "all") (string= kind wanted))
                 do (let ((described (nlk:json-object "id" (first row)
                                                      "display" (third row))))
                      (when (plusp (length (fourth row)))
                        (setf (gethash "strengths" described) (fourth row)))
                      (setf (gethash wanted answer)
                            (concatenate 'vector (gethash wanted answer)
                                         (vector described)))))
         answer)))))

;;; --- /gemini-media ------------------------------------------------------------------

(defun command-request (text)
  "(values PROMPT OPTIONS): the leading --flags out of TEXT, then the prompt. --image may
repeat; every other flag takes one value."
  (let ((words (nlk:split-words text))
        (options '())
        (images '()))
    (loop while (and words (uiop:string-prefix-p "--" (first words)))
          do (let ((flag (string-downcase (subseq (pop words) 2))))
               (cond ((member flag '("landscape" "portrait" "square") :test #'string=)
                      (setf (getf options :aspect) flag))
                     ((string= flag "video") (setf (getf options :video) t))
                     ((null words) nil)
                     (t (let ((value (pop words)))
                          (cond ((string= flag "image") (push value images))
                                (t (setf (getf options
                                                (intern (string-upcase
                                                         (if (string= flag "seconds")
                                                             "duration" flag))
                                                        :keyword))
                                         value))))))))
    (values (format nil "~{~a~^ ~}" words) (list* :images (nreverse images) options))))

(defun option-integer (value)
  (and value (ignore-errors (parse-integer value :junk-allowed t))))

(defun media-command (args session-id)
  "One /gemini-media invocation => the answer's text, and typed alone the dialog every
command answers then: one row that asks for an image prompt, one for a video."
  (declare (ignore session-id))
  (let ((line (nlk:trimmed (or args ""))))
    (if (zerop (length line))
        (values "Describe the picture or the clip, then /gemini-media with it."
                (nle:list-dialog
                 :title "Gemini media"
                 :summary "Images (Nano Banana) and video (Omni) through the Gemini API"
                 :rows (list (nle:ask-row "Generate an image" "Prompt" "/gemini-media"
                                           :placeholder "a red fox in snow, wide shot"
                                           :verb "generate")
                             (nle:ask-row "Generate a video" "Prompt" "/gemini-media --video"
                                           :placeholder "a fox crossing a snowy field, slow push in"
                                           :verb "render"))))
        (multiple-value-bind (prompt options) (command-request line)
          (if (zerop (length prompt))
              (values "Nothing to render: give /gemini-media a prompt." nil)
              (handler-case
                  (let ((pictures (getf options :images)))
                    (if (getf options :video)
                        (let ((answer (render-video prompt
                                                    :aspect (or (getf options :aspect) "landscape")
                                                    :resolution (getf options :resolution)
                                                    :duration (option-integer (getf options :duration))
                                                    :model (getf options :model)
                                                    :image (first pictures)
                                                    :images (rest pictures))))
                          (values (format nil "~a · ~a · ~as · ~a"
                                          (nlk:json-value answer :string "video")
                                          (nlk:json-value answer :string "resolution")
                                          (nlk:json-value answer :integer "duration")
                                          (nlk:json-value answer :string "task"))
                                  nil))
                        (let ((answer (render-image prompt
                                                    :aspect (or (getf options :aspect) "square")
                                                    :image-size (getf options :size)
                                                    :model (getf options :model)
                                                    :images pictures)))
                          (values (format nil "~a~@[ · ~a~]~@[ · ~a~]"
                                          (nlk:json-value answer :string "image")
                                          (nlk:json-value answer :string "model")
                                          (nlk:json-value answer :string "pixels"))
                                  nil))))
                (gemini-media-error (condition)
                  (values (format nil "ERROR: ~a" (gemini-media-error-detail condition)) nil))
                (error (condition)
                  (values (format nil "ERROR: gemini-media: ~a"
                                  (nlk:one-line (princ-to-string condition)))
                          nil))))))))

(defun gemini-image (prompt &key (aspect "square") image-size model images)
  "One image from eval, the same way the tool asks for one => the answer object."
  (render-image prompt :aspect aspect :image-size image-size :model model :images images))

(defun gemini-video (prompt &key (aspect "landscape") (resolution "720p") duration model
                                image images)
  "One video from eval, the same way the tool asks for one => the answer object."
  (render-video prompt :aspect aspect :resolution resolution :duration duration
                       :model model :image image :images images))

(defun list-media-models ()
  "The image and video models the key offers, from eval => (values ROWS SOURCE)."
  (media-models))

;;; --- the cell -----------------------------------------------------------------------

(nle:define-cell gemini-media
  (:section ("gemini-media")
    (:guide "the Gemini API root, and a key from AI Studio in the variable named here or pasted as api_key; the models are already chosen; next step: ask for a picture or a clip")
    ("api_url" :string :required t
     :doc "the Gemini API root: https://generativelanguage.googleapis.com/v1beta (a bare host gains /v1beta; plain http only for a loopback stand-in)")
    ("api_key" :secret
     :doc "the Gemini API key itself, pasted into /settings; leave it empty to read the variable api_key_env names")
    ("api_key_env" :env :default "GEMINI_API_KEY"
     :doc "the environment variable holding the Gemini API key; GOOGLE_API_KEY is read when it is unset")
    ("api_key_file" :path
     :doc "a file holding the key, which the file's own permissions guard; set instead of api_key")
    ("image_model" :string :default "gemini-nano-banana-2.1"
     :doc "the Gemini image model gemini_image uses when a call names none")
    ("video_model" :string :default "gemini-omni-1.1-flash"
     :doc "the Gemini video model gemini_video uses when a call names none")
    ("output_dir" :path :default "~/.nodecode/gemini-media"
     :doc "where generated pictures and videos are written")
    ("image_timeout" :integer :default 150 :min 10
     :doc "how long one image request may take, in seconds")
    ("video_timeout" :integer :default 300 :min 10
     :doc "how long one video render may take, in seconds; Gemini's video side can stall for minutes"))
  (:settings #'read-settings)
  (:help :gemini-media
         "gemini-media: gemini_image draws with Nano Banana, gemini_video renders with Gemini Omni"
         *manual*)
  (:tool "gemini_image" (nlk:contract 'image-request) #'gemini-image-tool)
  (:tool "gemini_video" (nlk:contract 'video-request) #'gemini-video-tool)
  (:tool "gemini_models" (nlk:contract 'model-list-request) #'gemini-models-tool)
  (:command "gemini-media" 'media-command
            :description "Generate a Gemini image or video from a prompt"
            :argument-hint "PROMPT [--video] [--image PATH]"))
