;;;; cell.lisp --- minimax_image and /minimax-image, over MiniMax's image API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One API base the operator names, in the `minimax_image' section of the
;;;; shared config:
;;;;   "minimax_image": {"api": "https://api.minimax.io",
;;;;                      "key_env": "MINIMAX_API_KEY", "model": "image-01"}
;;;; Everything the cell does is one POST to <api>/v1/image_generation and one
;;;; file written under output_dir. Nothing hooks the provider path: MiniMax
;;;; speaks HTTP, and NLK:HTTP is the one seam.
;;;;
;;;; Rewritten from rriggs's Hermes Agent plugin (see NOTICE): the two models
;;;; and their upgrade rule, the three aspect ratios in MiniMax's W:H form,
;;;; the config-keyed base URL and env var, the local-path-to-data-URI
;;;; inlining with its 10 MB cap, the 1500-character prompt, the answer image
;;;; cached from its URL because MiniMax's links expire within a day, and the
;;;; base_resp error shape.

(in-package #:nodecode-minimax-image)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Images are available through the nodecode-minimax-image cell: the minimax_image
tool, which a person can also call from eval in the minimax-image package, and the
/minimax-image command.
  minimax_image  {prompt, aspect, model, images}
      One POST to <api>/v1/image_generation, one picture written under the cell's
      output_dir. The answer names the file, its size in bytes, the model, the aspect
      and the ratio asked for, and the decoded pixel size. Say where the file is;
      do not paste its bytes.
      aspect is landscape (16:9), square (1:1) or portrait (9:16). MiniMax takes
      a W:H ratio, so there are three shapes and no arbitrary size.
      model is image-01 (text-to-image) or image-01-live (subject reference); a
      call that names a source image upgrades image-01 to image-01-live on its own.
      images names subject pictures for a restyle: absolute local paths, https URLs
      or data: URIs, each at most 10 MB. A local file is sent base64-inlined, because
      MiniMax fetches a URL from its own side. One subject is what MiniMax documents;
      a call may name up to four and each rides as its own subject_reference entry.
Write the prompt as an image model reads it - subject, setting, style, light, framing - not
as a question, and keep it under 1500 characters, which is all MiniMax reads. The picture
is downloaded as soon as it is drawn, because MiniMax's links expire within a day. A failed
call says the HTTP status or MiniMax's own status code, its message, and a hint: a wrong
key, an api base belonging to another region, a model the key cannot reach, a content
filter, or a rate limit. Type /minimax-image with a prompt for the same thing from the
operator's side: /minimax-image --landscape a red fox in snow; /minimax-image --image
~/pictures/fox.png --portrait the same fox, at night; /minimax-image --model image-01 a
heron. Typed alone it asks for the prompt."
  "What (help :minimax-image) answers while the cell runs.")

;;; --- what MiniMax's image models take --------------------------------------------

(defparameter +ratios+
  '(("landscape" . "16:9") ("square" . "1:1") ("portrait" . "9:16"))
  "The aspect the call names to the ratio MiniMax takes, in its W:H form.")

(defparameter +models+ '("image-01" "image-01-live")
  "The two models the original plugin names.")

(defparameter +default-model+ "image-01"
  "The text-to-image model, when neither the call nor the section names one.")

(defparameter +edit-model+ "image-01-live"
  "The model a source image needs: the fast one is text-to-image only.")

(defparameter +default-api+ "https://api.minimax.io"
  "The API base the original plugin defaults to.")

(defparameter +generation-path+ "/v1/image_generation"
  "The image generation route, relative to the api base.")

(defparameter +default-key-env+ "MINIMAX_API_KEY"
  "The variable the key is read from unless the section names another.")

(defparameter +max-prompt+ 1500
  "The most of a prompt MiniMax reads, in characters.")

(defparameter +max-source-bytes+ (* 10 1024 1024)
  "The most one source image may be, in bytes: the original plugin's inline cap.")

(defparameter +max-sources+ 4
  "The most subject pictures one call sends. MiniMax documents one; a call
here may name up to four and each rides as its own subject_reference entry.")

(defparameter +error-chars+ 300
  "How much of MiniMax's own error text a refusal quotes back.")

(defparameter +user-agent+ "nodecode-minimax-image/0.1.0"
  "What the cell calls itself to MiniMax. Nothing of another client's is
impersonated, and no other product's session header rides along.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (minimax-image-error (condition)
       (nle:failure (format nil "ERROR: ~a" (minimax-image-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: minimax-image: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun answer-detail (body)
  "MiniMax's own words for a failed call, clipped, or NIL."
  (let ((text (nlk:trimmed (or body ""))))
    (and (plusp (length text)) (nlk:clip text +error-chars+))))

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun url-parts (url)
  "(values SCHEME HOST USERINFO-P) of an absolute URL, both lowercased, or NILs."
  (let ((mark (and (stringp url) (search "://" url))))
    (when (and mark (plusp mark))
      (let* ((scheme (string-downcase (subseq url 0 mark)))
             (rest (subseq url (+ mark 3)))
             (end (or (position-if (lambda (character) (find character "/?#")) rest)
                      (length rest)))
             (authority (subseq rest 0 end))
             (at (position #\@ authority :from-end t))
             (hostport (if at (subseq authority (1+ at)) authority))
             (host (cond ((and (plusp (length hostport)) (char= #\[ (char hostport 0)))
                          (subseq hostport 1 (or (position #\] hostport) (length hostport))))
                         (t (subseq hostport 0 (or (position #\: hostport) (length hostport)))))))
        (values scheme (string-downcase host) (and at t))))))

(defun dotted-quad (host)
  "HOST as four numbers when it is an IPv4 literal, else NIL."
  (let ((parts (uiop:split-string host :separator '(#\.))))
    (when (and (= 4 (length parts))
               (every (lambda (part)
                        (and (plusp (length part))
                             (every #'digit-char-p part)))
                      parts))
      (mapcar #'parse-integer parts))))

(defun private-ipv4-p (host)
  "True for loopback, RFC 1918, CGNAT 100.64/10 and link-local: the networks
a plain-HTTP api base may live on."
  (let ((quad (dotted-quad host)))
    (and quad
         (destructuring-bind (a b c d) quad
           (declare (ignore c d))
           (or (= a 127) (= a 10)
               (and (= a 172) (<= 16 b) (<= b 31))
               (and (= a 192) (= b 168))
               (and (= a 169) (= b 254))
               (and (= a 100) (<= 64 b) (<= b 127))
               (zerop a))))))

(defun ipv6-private-p (host)
  "True for ::1 and the fc00::/7 unique-local block."
  (let* ((host (string-downcase host))
         (host (if (uiop:string-prefix-p "::ffff:" host) (subseq host 7) host)))
    (cond ((or (string= host "::1") (string= host "::")) t)
          ((dotted-quad host) (private-ipv4-p host))
          ((find #\: host)
           (let ((first (subseq host 0 (or (position #\: host) (length host)))))
             (or (uiop:string-prefix-p "fc" first) (uiop:string-prefix-p "fd" first)))))))

(defun private-host-p (host)
  "True when HOST may carry a plain-HTTP api base: a loopback or LAN literal,
localhost, or a name only a local network resolves."
  (let ((host (string-downcase (or host ""))))
    (or (member host '("localhost" "localhost.localdomain") :test #'string=)
        (private-ipv4-p host)
        (ipv6-private-p host)
        (some (lambda (suffix) (uiop:string-suffix-p host suffix))
              '(".localhost" ".local" ".internal" ".lan" ".home.arpa" ".ts.net")))))

(defun check-api (url)
  "URL, without its trailing slashes, or a refusal saying what is wrong with it."
  (let ((url (nlk:trimmed (or url ""))))
    (when (zerop (length url))
      (nlk:config-error "api is empty: set it to the MiniMax base the key belongs to, e.g. https://api.minimax.io"))
    (when (find-if (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return))) url)
      (nlk:config-error "api ~s holds whitespace" url))
    (when (nth-value 2 (url-parts url))
      (nlk:config-error "api must not carry credentials in it: name the key in key_env"))
    (multiple-value-bind (scheme host) (url-parts url)
      (unless scheme
        (nlk:config-error "api ~s is not an absolute URL: give the scheme too, e.g. https://api.minimax.io" url))
      (unless (member scheme '("http" "https") :test #'string=)
        (nlk:config-error "api scheme ~s is not http or https" scheme))
      (when (and (string= scheme "http") (not (private-host-p host)))
        (nlk:config-error "api ~s is plain HTTP and ~a is not a loopback, LAN, Tailscale or .local host: the key would cross the public internet in clear text. Use https://" url host))
      (string-right-trim "/" url))))

(defun written-string (table key)
  "KEY in the section TABLE as the operator wrote it => (T . TEXT) when it
was answered at all, (T) when it was answered blank, NIL when it was absent.
CONFIG-STRING cannot tell those two apart, because it folds a blank into its
default."
  (when table
    (multiple-value-bind (value present) (nlk:section-value table key)
      (and present
           (cons t (and (stringp value) (nonblank value)))))))

(defun read-settings (values table)
  "The declared VALUES, plus the key the section names and the folder images
are written to: read once, here."
  (let* ((api (check-api (or (getf values :api) +default-api+)))
         ;; KEY_ENV carries a default, so the section as written is what tells
         ;; an answered variable from an absent one: "" is an answer that
         ;; names nothing, and a key_file stands alone.
         (written (written-string table "key_env"))
         (file (nonblank (getf values :key-file)))
         (env (cond ((and written (null (cdr written)))
                     (nlk:config-error "key_env is empty: name the environment variable holding the MiniMax key, or set key_file"))
                    ((cdr written))
                    (file nil)
                    (t +default-key-env+)))
         (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of key_env and key_file, not both"))
    (setf (getf plist :api) api
          (getf plist :output-dir)
          (uiop:ensure-directory-pathname
           (nlk:expand-home (or (nonblank (getf values :output-dir)) "~/.nodecode/images")))
          (getf plist :api-key)
          (cond (env (nonblank (nle::credential-env env)))
                (file (or (nonblank (ignore-errors
                                     (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "key_file ~a cannot be read, or is empty" file)))
                (t nil)))
    (let ((key (getf plist :api-key)))
      (when (and key (find-if (lambda (character)
                                (member character '(#\Space #\Tab #\Newline #\Return)))
                              key))
        (nlk:config-error "the key ~a names holds whitespace or line breaks" (or env file))))
    plist))

;;; --- the bytes MiniMax answers, and what they are --------------------------------

(defun octet-string (octets start end)
  (map 'string #'code-char (subseq octets start end)))

(defun image-type (octets)
  "\"png\", \"jpg\", \"webp\" or \"gif\" by the first bytes, or NIL: nothing
the cell did not recognise as an image is ever written or sent."
  (let ((size (length octets)))
    (cond ((and (>= size 8)
                (= (aref octets 0) #x89) (= (aref octets 1) 80)
                (= (aref octets 2) 78) (= (aref octets 3) 71))
           "png")
          ((and (>= size 3)
                (= (aref octets 0) #xFF) (= (aref octets 1) #xD8)
                (= (aref octets 2) #xFF))
           "jpg")
          ((and (>= size 12)
                (string= "RIFF" (octet-string octets 0 4))
                (string= "WEBP" (octet-string octets 8 12)))
           "webp")
          ((and (>= size 6) (string= "GIF8" (octet-string octets 0 4)))
           "gif"))))

(defun big-endian (octets at bytes)
  (loop with value = 0
        for index from 0 below bytes
        do (setf value (+ (* value 256) (aref octets (+ at index))))
        finally (return value)))

(defun png-size (octets)
  (when (and (>= (length octets) 24)
             (string= "IHDR" (octet-string octets 12 16)))
    (cons (big-endian octets 16 4) (big-endian octets 20 4))))

(defun jpeg-size (octets)
  "The width and height in OCTETS' first SOF segment, or NIL."
  (let ((index 2)
        (size (length octets)))
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

(defun image-pixels (octets)
  "OCTETS' \"WxH\" when its header says, else NIL."
  (let* ((type (image-type octets))
         (pair (cond ((equal type "png") (png-size octets))
                     ((equal type "jpg") (jpeg-size octets)))))
    (and pair (format nil "~dx~d" (car pair) (cdr pair)))))

(defun save-octets (octets &key (stem "minimax"))
  "(values PATH BYTES PIXELS): OCTETS written under the cell's output_dir,
never over another file there."
  (let ((type (image-type octets)))
    (unless type
      (fail "MiniMax answered bytes that are not a PNG, JPEG, WebP or GIF image"))
    (let ((directory (setting :output-dir)))
      (ensure-directories-exist (merge-pathnames "keep" directory))
      (let ((file
              (loop for attempt from 0
                    for candidate = (merge-pathnames
                                     (format nil "~a-~d~@[-~d~].~a" stem
                                             (get-universal-time)
                                             (and (plusp attempt) attempt) type)
                                     directory)
                    unless (probe-file candidate)
                      return candidate)))
        (alexandria:write-byte-vector-into-file octets file :if-exists :supersede)
        (values (namestring file) (length octets) (image-pixels octets))))))

;;; --- source images ---------------------------------------------------------------

(defun read-file-octets (path limit)
  "PATH's bytes, at most LIMIT of them, so a huge file is not read whole."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let* ((declared (file-length in))
           (size (if declared (min limit declared) limit))
           (octets (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun source-item (source)
  "The `image_file' member one source rides as: an https URL or data URI
verbatim, or a local file base64-inlined as a data URI - MiniMax fetches a
URL from its own side, so a path only the operator's machine can see has to
travel."
  (let ((source (nlk:trimmed source)))
    (cond ((uiop:string-prefix-p "data:" source) source)
          ((or (uiop:string-prefix-p "https://" (string-downcase source))
               (uiop:string-prefix-p "http://" (string-downcase source)))
           source)
          (t
           (let* ((path (nlk:expand-home source))
                  (octets (and (probe-file path)
                               (ignore-errors
                                (read-file-octets path (1+ +max-source-bytes+))))))
             (unless octets
               (fail "source image ~a does not exist, or cannot be read" source))
             (when (> (length octets) +max-source-bytes+)
               (fail "source image ~a is over the ~:d bytes one image may be"
                     source +max-source-bytes+))
             (let ((type (or (image-type octets)
                             (fail "source image ~a is not a PNG, JPEG, WebP or GIF: only image bytes are sent"
                                   source))))
               (format nil "data:image/~a;base64,~a"
                       type
                       (cl-base64:usb8-array-to-base64-string octets))))))))

;;; --- one exchange with MiniMax ---------------------------------------------------

(defun api-url ()
  (concatenate 'string (setting :api) +generation-path+))

(defun exchange (method url &key content (content-type "application/json") binary)
  "One exchange with MiniMax => (values BODY STATUS), BODY the text or the
octets it answered. A dial that fails is a refusal, never an unhandled error.
The key rides as a bearer token and nowhere else; it is never put in a URL or
a log line."
  (handler-case
      (nlk:http method url
                :headers (append (list (cons "user-agent" +user-agent+)
                                       (cons "accept" (if binary "*/*" "application/json")))
                                 (let ((key (setting :api-key)))
                                   (when key
                                     (list (cons "authorization"
                                                 (format nil "Bearer ~a" key)))))
                                 (when content-type
                                   (list (cons "content-type" content-type))))
                :content content
                :binary binary
                :timeout (setting :timeout)
                :connect-timeout (min 15 (setting :timeout)))
    (error (condition)
      (fail "~(~a~) ~a did not answer: ~a" method url
            (nlk:one-line (princ-to-string condition))))))

(defun status-hint (status)
  "The parenthetical hint the model can act on, or NIL."
  (case status
    ((400 422) " (check the model id, the ratio and the source image: MiniMax refuses a body its model does not take)")
    ((401 403) " (check the key in the variable key_env names, and that api is the host that key belongs to)")
    (404 " (check api is the MiniMax base: https://api.minimax.io, or the regional host the key was made on)")
    (429 " (MiniMax is rate limiting or the account is out of quota: retry in a moment)")
    (t nil)))

(defun error-message (body)
  "MiniMax's own words for a failed answer: its base_resp code and message
when the body is the JSON object it answers with, else the text itself."
  (let* ((text (and (stringp body) body))
         (object (and text
                      (plusp (length (nlk:trimmed text)))
                      (ignore-errors (nlk:decode-json text))))
         (inner (and (hash-table-p object) (nlk:json-value object :object "base_resp")))
         (code (and inner (nlk:json-value inner :integer "status_code")))
         (message (and inner (nlk:json-value inner :string "status_msg"))))
    (or (cond ((and message code) (format nil "~a (code ~d)" message code))
              (message message)
              (code (format nil "code ~d" code)))
        (and (hash-table-p object) (nlk:json-value object :string "message"))
        (and text (plusp (length (nlk:trimmed text)))
             (nlk:clip (nlk:one-line text) +error-chars+))
        "no error message")))

(defun replace-text (text part replacement)
  "TEXT with every occurrence of PART replaced, so a key MiniMax echoes back
is never shown."
  (if (or (zerop (length part)) (null (search part text)))
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
  "TEXT with the key, when one is configured, replaced."
  (let ((key (setting :api-key)))
    (if (and key (plusp (length key)))
        (replace-text text key "[key]")
        text)))

(defun http-refusal (status body)
  "A failed answer as a refusal: the status, MiniMax's words, and the hint
that usually fixes it."
  (fail "MiniMax answered HTTP ~a: ~a~@[~a~]"
        status (redact (error-message body)) (status-hint status)))

;;; --- one picture -----------------------------------------------------------------

(defun request-payload (model prompt ratio sources)
  "The image_generation body: the prompt, the ratio MiniMax takes, and one
subject_reference entry per source image."
  (let ((object (nlk:json-object
                 "model" model
                 "prompt" prompt
                 "aspect_ratio" ratio
                 "response_format" "url"
                 "n" 1)))
    (when sources
      (setf (gethash "subject_reference" object)
            (map 'vector (lambda (source)
                           (nlk:json-object "type" "character" "image_file" source))
                 sources)))
    object))

(defun answer-image (object)
  "The image the answer carries, a URL, from data.image_urls, or NIL."
  (let* ((data (nlk:json-value object :object "data"))
         (urls (and data (nlk:json-array data "image_urls"))))
    (and urls
         (plusp (length urls))
         (nonblank (nlk:json-value (aref urls 0) :string)))))

(defun save-answer (reference)
  "(values PATH BYTES PIXELS) of the picture REFERENCE names: a data URI
decoded, or a URL fetched at once because MiniMax's link expires."
  (if (uiop:string-prefix-p "data:" reference)
      (let* ((comma (position #\, reference))
             (encoded (and comma (subseq reference (1+ comma))))
             (octets (and encoded
                          (ignore-errors (cl-base64:base64-string-to-usb8-array encoded)))))
        (unless octets
          (fail "MiniMax answered a data URI that is not base64"))
        (save-octets octets))
      (multiple-value-bind (body status) (exchange :get reference :content-type nil :binary t)
        (unless (and (integerp status) (< status 400))
          (fail "the image URL answered HTTP ~a" status))
        (unless (typep body '(vector (unsigned-byte 8)))
          (fail "the image URL answered no bytes"))
        (save-octets body))))

(defun render (prompt &key (aspect "square") model images)
  "One image, generated or restyled => the answer object: the file, its size,
the model, the aspect and the ratio asked for, the decoded pixel size, and
what the call was."
  (let* ((prompt (nlk:trimmed (or prompt "")))
         (sources (remove-if-not #'nonblank (or images '())))
         (aspect (string-downcase (or (nonblank aspect) "square")))
         (label (or (nonblank model) (nonblank (setting :model)) +default-model+)))
    (when (zerop (length prompt))
      (fail "a prompt is required and cannot be empty"))
    (unless (assoc aspect +ratios+ :test #'string=)
      (fail "aspect must be one of ~{~a~^, ~}: ~s"
            (mapcar #'car +ratios+) aspect))
    (unless (member label +models+ :test #'string=)
      (fail "model must be one of ~{~a~^, ~}: ~s" +models+ label))
    (when (> (length sources) +max-sources+)
      (fail "~d source images, over the ~a one call sends" (length sources) +max-sources+))
    (unless (nonblank (setting :api-key))
      (fail "no MiniMax key: put one in the environment variable ~a names, or name a file in key_file"
            (or (nonblank (setting :key-env)) +default-key-env+)))
    (let* ((clipped (> (length prompt) +max-prompt+))
           (prompt (if clipped (subseq prompt 0 +max-prompt+) prompt))
           (edited (and sources t))
           (model (if (and edited (equal label +default-model+)) +edit-model+ label))
           (ratio (cdr (assoc aspect +ratios+ :test #'string=)))
           (items (mapcar #'source-item sources)))
      (multiple-value-bind (body status)
          (exchange :post (api-url)
                    :content (nlk:encode-json-object
                              (request-payload model prompt ratio items)))
        (unless (and (integerp status) (< status 400))
          (http-refusal status body))
        (let ((object (and (stringp body) (ignore-errors (nlk:decode-json body)))))
          (unless (hash-table-p object)
            (fail "MiniMax answered no JSON object"))
          ;; MiniMax reports its refusals in base_resp, sometimes under a 200.
          (let ((code (nlk:json-value object :integer "base_resp" "status_code")))
            (when (and code (not (zerop code)))
              (fail "MiniMax refused the call: ~a"
                    (or (nlk:json-value object :string "base_resp" "status_msg")
                        (format nil "status ~d" code)))))
          (let ((reference (answer-image object)))
            (unless reference
              (fail "MiniMax answered no image: ~a"
                    (or (answer-detail body) "an empty answer")))
            (multiple-value-bind (path bytes pixels) (save-answer reference)
              (let ((answer (nlk:json-object
                             "image" path "bytes" bytes
                             "model" model "aspect" aspect
                             "ratio" ratio
                             "mode" (if edited "subject" "generate"))))
                (when pixels
                  (setf (gethash "pixels" answer) pixels))
                (when clipped
                  (setf (gethash "prompt_clipped" answer) t))
                answer))))))))

;;; --- the tool --------------------------------------------------------------------

(defun minimax-image-tool (request)
  "The minimax_image tool: one picture, or a restyle when source images are named."
  (answering
    (nlk:encode-json-object
     (render (image-request-prompt request)
             :aspect (image-request-aspect request)
             :model (image-request-model request)
             :images (coerce (image-request-images request) 'list)))))

;;; --- /minimax-image -------------------------------------------------------------

(defun command-request (text)
  "(values PROMPT OPTIONS): the leading --flags out of TEXT, then the prompt.
--image may repeat; every other flag takes one value."
  (let ((words (nlk:split-words text))
        (options '())
        (images '()))
    (loop while (and words (uiop:string-prefix-p "--" (first words)))
          do (let ((flag (string-downcase (subseq (pop words) 2))))
               (cond ((member flag '("landscape" "portrait" "square") :test #'string=)
                      (setf (getf options :aspect) flag))
                     ((null words) nil)
                     (t
                      (let ((value (pop words)))
                        (if (string= flag "image")
                            (push value images)
                            (setf (getf options (intern (string-upcase flag) :keyword))
                                  value)))))))
    (values (format nil "~{~a~^ ~}" words)
            (list* :images (nreverse images) options))))

(defun minimax-image-command (args session-id)
  "One /minimax-image invocation => the answer's text, and typed alone the
dialog every command answers then: one row that asks for the prompt."
  (declare (ignore session-id))
  (let ((line (nlk:trimmed (or args ""))))
    (if (zerop (length line))
        (values "Describe the picture, then /minimax-image with it."
                (nle:list-dialog
                 :title "MiniMax Image"
                 :summary "Image-01 models, through your MiniMax key"
                 :rows (list (nle:ask-row "Generate an image" "Prompt" "/minimax-image"
                                           :placeholder "a red fox in snow, wide shot"
                                           :verb "generate"))))
        (multiple-value-bind (prompt options) (command-request line)
          (if (zerop (length prompt))
              (values "Nothing to draw: give /minimax-image a prompt." nil)
              (handler-case
                  (let ((answer (render prompt
                                        :aspect (or (getf options :aspect) "square")
                                        :model (getf options :model)
                                        :images (getf options :images))))
                    (values (format nil "~a~@[ · ~a~]~@[ · ~a~]"
                                    (nlk:json-value answer :string "image")
                                    (nlk:json-value answer :string "model")
                                    (nlk:json-value answer :string "ratio"))
                            nil))
                (minimax-image-error (condition)
                  (values (format nil "ERROR: ~a" (minimax-image-error-detail condition)) nil))
                (error (condition)
                  (values (format nil "ERROR: minimax-image: ~a"
                                  (nlk:one-line (princ-to-string condition)))
                          nil))))))))

(defun generate-image (prompt &key (aspect "square") model images)
  "One image from eval, the same way the tool asks for one => the answer object."
  (render prompt :aspect aspect :model model :images images))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell minimax-image
  (:section ("minimax_image")
    (:guide "the MiniMax base and the environment variable holding your MiniMax key; a model is already chosen; next step: ask for a picture")
    ("api" :string :default "https://api.minimax.io"
     :doc "the MiniMax API base: https://api.minimax.io, or the regional host the key was made on")
    ("key_env" :string :default "MINIMAX_API_KEY"
     :doc "the environment variable holding the MiniMax key - never the key itself")
    ("key_file" :path
     :doc "a file holding the key, which the file's own permissions guard; set instead of key_env")
    ("model" :choice :options '("image-01" "image-01-live") :default "image-01"
     :doc "image-01 is text-to-image; image-01-live takes a subject reference and is chosen on its own when a source image is named")
    ("output_dir" :path :default "~/.nodecode/images"
     :doc "where generated pictures are written")
    ("timeout" :integer :default 180 :min 1
     :doc "how long one MiniMax request may take, in seconds; a render is slow"))
  (:settings #'read-settings)
  (:help :minimax-image
         "minimax-image: minimax_image draws a picture with MiniMax's Image-01 models, or restyles one"
         *manual*)
  (:tool "minimax_image" (nlk:contract 'image-request) #'minimax-image-tool)
  (:command "minimax-image" 'minimax-image-command
            :description "Draw an image with MiniMax"
            :argument-hint "PROMPT"))
