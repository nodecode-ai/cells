;;;; cell.lisp --- generate_image and /image, over an OpenAI Images gateway.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One API root the operator names, in the `imagegen' section of the shared
;;;; config:
;;;;   "imagegen": {"base_url": "http://127.0.0.1:8317/v1", "model": "gpt-image-2"}
;;;; Everything the cell does is one POST to <base_url>/images/generations, or
;;;; to <base_url>/images/edits when the call names source images, and one file
;;;; written under output_dir. Nothing hooks the provider path: a gateway speaks
;;;; HTTP, and NLK:HTTP is the one seam.
;;;;
;;;; Ported from duu261's Hermes Agent plugin (see NOTICE): the model/quality
;;;; resolution, the three aspect sizes, the gateway-aware error hints, the
;;;; source-image checks (magic bytes, a 50 MB cap, an https URL) and the
;;;; fields the answer carries. The Python openai SDK is not here: the two
;;;; endpoints are spoken directly.

(in-package #:nodecode-imagegen)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Images are available through the nodecode-imagegen cell: the generate_image tool, which a
person can also call from eval in the imagegen package, and the /image command.
  generate_image  {prompt, aspect, model, images}
      One POST to the operator's OpenAI Images-compatible gateway
      (<base_url>/images/generations, or /images/edits when images are given), one picture
      written under the cell's output_dir. The answer names the file, its size in bytes, the
      model, the requested size and quality, the decoded pixel size when the format says it,
      and the gateway's revised prompt when it sends one. Say where the file is; do not paste
      its bytes.
      aspect is landscape (1536x1024), square (1024x1024) or portrait (1024x1536) - ignored
      for a model the gateway defines, which picks its own size. model names a model the
      gateway serves, when it should differ from the operator's setting; on the GPT image
      models a quality may ride the name (gpt-image-2-high, gpt-image-2.5-sunburst-low).
      images names source pictures to edit instead of drawing fresh: absolute local paths, or
      https URLs, up to 16. Only real PNG, JPEG, WebP or GIF bytes are sent, each at most
      50 MB.
Write the prompt as an image model reads it - subject, setting, style, light, framing - not as
a question. A failed call says the HTTP status, the gateway's own message and a hint: a wrong
key, a base_url that does not end at the API root (usually /v1), a model the gateway has not
enabled, a rate limit, or a proxy timeout on a slow render. Type /image with a prompt for the
same thing from the operator's side: /image --landscape a red fox in snow; /image --image
~/pictures/fox.png --portrait make the sky dusk; /image --model gpt-image-1 --quality high a
heron. Typed alone it asks for the prompt."
  "What (help :imagegen) answers while the cell runs.")

;;; --- what the gateway's models take --------------------------------------------

(defparameter +sized-models+
  '("gpt-image-1" "gpt-image-1.5" "gpt-image-2" "gpt-image-2.5"
    "gpt-image-2.5-flare" "gpt-image-2.5-sunburst")
  "Models that take OpenAI's size and quality enums. Any other id is a
gateway-defined model - a Gemini image model behind the gateway, Flux - and is
sent verbatim with neither, because a gateway rejects enum values it does not
know.")

(defparameter +qualities+ '("auto" "low" "medium" "high" "xhigh" "max")
  "The quality values the GPT image models accept.")

(defparameter +default-quality+ "medium"
  "Sent for a sized model when the section does not choose one: gateways that
back these models with Codex render at low when quality is omitted.")

(defparameter +sizes+
  '(("landscape" . "1536x1024") ("square" . "1024x1024") ("portrait" . "1024x1536"))
  "The aspect the call names to the size the models take.")

(defparameter +max-source-bytes+ (* 50 1024 1024)
  "The most one source image may be, in bytes: the original plugin's cap.")

(defparameter +max-sources+ 16
  "The most source images one edit sends: the original plugin's cap.")

(defparameter +error-chars+ 300
  "How much of the gateway's own error text a refusal quotes back.")

(defparameter +user-agent+ "nodecode-imagegen/0.1.0"
  "What the cell calls itself to the gateway. Nothing of another client's is
impersonated, and no OpenAI organization or project id rides along.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (imagegen-error (condition)
       (nle:failure (format nil "ERROR: ~a" (imagegen-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: imagegen: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun answer-detail (body)
  "The gateway's own words for a failed call, clipped, or NIL."
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

(defun ipv6-private-p (host)
  "True for ::1 and the fc00::/7 unique-local block, the addresses a plain-HTTP
gateway may live at."
  (let* ((host (string-downcase host))
         (host (if (uiop:string-prefix-p "::ffff:" host) (subseq host 7) host)))
    (cond ((or (string= host "::1") (string= host "::")) t)
          ((dotted-quad host) (private-ipv4-p host))
          ((find #\: host)
           (let ((first (subseq host 0 (or (position #\: host) (length host)))))
             (or (string= first "fc") (string= first "fd")
                 (uiop:string-prefix-p "fc" first) (uiop:string-prefix-p "fd" first)))))))

(defun private-ipv4-p (host)
  "True for loopback, RFC 1918, CGNAT 100.64/10 and link-local: the networks
the original plugin allowed a plain-HTTP gateway on."
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

(defun private-host-p (host)
  "True when HOST may carry a plain-HTTP gateway: a loopback or LAN literal,
localhost, or a name that only a local network resolves (.local, .internal,
.lan, .home.arpa)."
  (let ((host (string-downcase (or host ""))))
    (or (member host '("localhost" "localhost.localdomain") :test #'string=)
        (private-ipv4-p host)
        (ipv6-private-p host)
        (some (lambda (suffix) (uiop:string-suffix-p host suffix))
              '(".localhost" ".local" ".internal" ".lan" ".home.arpa" ".ts.net")))))

(defun check-base-url (url)
  "URL, without its trailing slash, or a refusal saying what is wrong with it."
  (let ((url (nlk:trimmed (or url ""))))
    (when (zerop (length url))
      (nlk:config-error "base_url is empty: set it to the gateway's API root, usually ending at /v1, e.g. \"http://127.0.0.1:8317/v1\""))
    (when (find-if (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return))) url)
      (nlk:config-error "base_url ~s holds whitespace" url))
    (when (nth-value 2 (url-parts url))
      (nlk:config-error "base_url must not carry credentials in it: name the key in api_key_env"))
    (multiple-value-bind (scheme host) (url-parts url)
      (unless scheme
        (nlk:config-error "base_url ~s is not an absolute URL: give the scheme too, e.g. https://gateway.example/v1" url))
      (unless (member scheme '("http" "https") :test #'string=)
        (nlk:config-error "base_url scheme ~s is not http or https" scheme))
      (when (and (string= scheme "http") (not (private-host-p host)))
        (nlk:config-error "base_url ~s is plain HTTP and ~a is not a loopback, LAN, Tailscale or .local host: the key would cross the public internet in clear text. Use https://, or the gateway's address on your own network." url host))
      (string-right-trim "/" url))))

(defun read-settings (values table)
  "The declared VALUES, plus the key the section names and the folder images
are written to: read once, here."
  (declare (ignore table))
  (let* ((base (check-base-url (or (getf values :base-url) "")))
         (env (nonblank (getf values :api-key-env)))
         (file (nonblank (getf values :api-key-file)))
         (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of api_key_env and api_key_file, not both"))
    (setf (getf plist :base-url) base
          (getf plist :output-dir)
          (uiop:ensure-directory-pathname
           (nlk:expand-home (or (nonblank (getf values :output-dir)) "~/.nodecode/images")))
          (getf plist :api-key)
          (cond (env (nonblank (nle::credential-env env)))
                (file (or (nonblank (ignore-errors
                                     (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "api_key_file ~a cannot be read, or is empty" file)))
                (t nil)))
    (let ((key (getf plist :api-key)))
      (when (and key (find-if (lambda (character)
                                (member character '(#\Space #\Tab #\Newline #\Return)))
                              key))
        (nlk:config-error "the key ~a names holds whitespace or line breaks" (or env file))))
    plist))

;;; --- the model the call names, and what to send with it --------------------------

(defun resolve-model (label quality)
  "(values API-MODEL QUALITY): gpt-image-2-high -> (\"gpt-image-2\" \"high\").
A known GPT image model always gets a quality (the setting, else medium).
Any other id is a gateway-defined model and is sent verbatim with NIL, so no
size or quality enum a gateway rejects rides along."
  (let* ((cut (and (stringp label) (position #\- label :from-end t)))
         (base (and cut (subseq label 0 cut)))
         (suffix (and cut (subseq label (1+ cut)))))
    (cond ((and base
                (member base +sized-models+ :test #'string=)
                (member suffix +qualities+ :test #'string=))
           (values base suffix))
          ((member label +sized-models+ :test #'string=)
           (values label (if (member quality +qualities+ :test #'string=)
                             quality
                             +default-quality+)))
          (t (values label nil)))))

;;; --- the bytes the gateway answers, and what they are -----------------------------

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
  "PICTURE's \"WxH\" when its header says, else NIL."
  (let* ((type (image-type octets))
         (pair (cond ((equal type "png") (png-size octets))
                     ((equal type "jpg") (jpeg-size octets)))))
    (and pair (format nil "~dx~d" (car pair) (cdr pair)))))

(defun save-octets (octets &key (stem "imagegen"))
  "(values PATH BYTES PIXELS): OCTETS written under the cell's output_dir,
never over another file there."
  (let ((type (image-type octets)))
    (unless type
      (fail "the gateway answered bytes that are not a PNG, JPEG, WebP or GIF image"))
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

(defun check-image-url (url)
  "URL, or a refusal: a source or result image is fetched over https, or plain
HTTP only from a local network."
  (multiple-value-bind (scheme host) (url-parts url)
    (cond ((null scheme)
           (fail "the gateway answered an image URL ~s that is not absolute" url))
          ((string= scheme "https") url)
          ((and (string= scheme "http") (private-host-p host)) url)
          (t (fail "refusing to fetch the image URL ~s: plain HTTP to ~a is not a local network" url host)))))

;;; --- one exchange with the gateway ----------------------------------------------

(defun api-url (path)
  (concatenate 'string (setting :base-url) path))

(defun exchange (method url &key content (content-type "application/json") binary)
  "One exchange with the gateway => (values BODY STATUS), BODY the text or the
octets it answered. A dial that fails is a refusal, never an unhandled error.
The key, when the section carries one, rides as a bearer token and nowhere
else; it is never put in a URL or a log line."
  (handler-case
      (multiple-value-bind (body status)
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
        (values body status))
    (error (condition)
      (fail "~(~a~) ~a did not answer: ~a" method url
            (nlk:one-line (princ-to-string condition))))))

(defun status-hint (status)
  "The parenthetical hint the model can act on, or NIL."
  (case status
    ((401 403) " (check the key in the variable api_key_env names, and what the gateway lets it reach)")
    (404 " (check base_url ends at the API root, usually /v1, and that the model is enabled on the gateway)")
    (429 " (the gateway is rate limiting: retry in a moment)")
    (524 " (a proxy cut a slow render short: try a route without the CDN in front, or a faster model)")
    (t nil)))

(defun error-message (body)
  "The gateway's own message out of a failed answer."
  (let* ((text (and (stringp body) body))
         (object (and text
                      (plusp (length (nlk:trimmed text)))
                      (ignore-errors (nlk:decode-json text))))
         (inner (and (hash-table-p object) (nlk:json-value object :object "error"))))
    (or (and (hash-table-p inner) (nlk:json-value inner :string "message"))
        (and (hash-table-p object) (nlk:json-value object :string "message"))
        (and (hash-table-p object) (nlk:json-value object :string "error_name"))
        (and (hash-table-p object) (nlk:json-value object :string "title"))
        (and text (plusp (length (nlk:trimmed text)))
             (nlk:clip (nlk:one-line text) +error-chars+))
        "no error message")))

(defun replace-text (text part replacement)
  "TEXT with every occurrence of PART replaced, so a key a gateway echoes back
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
  "TEXT with the key, when one is configured, replaced: a gateway echoes a bad
key back in its message."
  (let ((key (setting :api-key)))
    (if (and key (plusp (length key)))
        (replace-text text key "[key]")
        text)))

(defun http-refusal (status body)
  "A failed answer as a refusal: the status, the gateway's words, and the hint
that usually fixes it."
  (let ((message (redact (error-message body))))
    (fail "the gateway answered HTTP ~a: ~a~@[~a~]~@[~a~]"
          status message
          (status-hint status)
          (and (search "1010" message)
               " (its WAF blocked the client signature: check its rules)"))))

;;; --- source images ---------------------------------------------------------------

(defun remote-reference-p (reference)
  (let ((reference (string-downcase reference)))
    (or (uiop:string-prefix-p "https://" reference)
        (uiop:string-prefix-p "http://" reference))))

(defun fetch-octets (url what)
  "URL's bytes, refused unless it answers 200 with at most the cap."
  (multiple-value-bind (body status) (exchange :get url :content-type nil :binary t)
    (unless (and (integerp status) (< status 400))
      (fail "~a ~a answered HTTP ~a" what url status))
    (unless (typep body '(vector (unsigned-byte 8)))
      (fail "~a ~a answered no bytes" what url))
    (when (> (length body) +max-source-bytes+)
      (fail "~a ~a answered ~:d bytes, over the ~:d one image may be"
            what url (length body) +max-source-bytes+))
    body))

(defun read-file-octets (path limit)
  "PATH's bytes, at most LIMIT of them, so a huge file is not read whole."
  (with-open-file (in path :direction :input :element-type '(unsigned-byte 8))
    (let* ((declared (file-length in))
           (size (if declared (min limit declared) limit))
           (octets (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence octets in)
      octets)))

(defun source-octets (source)
  "The bytes of one source image: a path under ~ and /, or an https URL."
  (if (remote-reference-p source)
      (fetch-octets (check-image-url source) "source image")
      (let* ((path (nlk:expand-home source))
             (octets (and (probe-file path)
                          (ignore-errors
                           (read-file-octets path (1+ +max-source-bytes+))))))
        (unless octets
          (fail "source image ~a does not exist, or cannot be read" source))
        (when (> (length octets) +max-source-bytes+)
          (fail "source image ~a is over the ~:d bytes one image may be"
                source +max-source-bytes+))
        octets)))

(defun call-with-source-files (sources function)
  "FUNCTION on local files holding SOURCES' bytes, in a scratch folder removed
afterwards: only real image bytes ever reach the gateway."
  (let ((directory (merge-pathnames
                    (format nil "nodecode-imagegen-~d-~d/"
                            (get-universal-time) (random 1000000))
                    (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "keep" directory))
    (unwind-protect
         (funcall
          function
          (loop for source in sources
                for index from 1
                for octets = (source-octets source)
                for type = (or (image-type octets)
                               (fail "source image ~a is not a PNG, JPEG, WebP or GIF: only image bytes are sent"
                                     source))
                collect (let ((file (merge-pathnames
                                     (format nil "source-~d.~a" index type) directory)))
                          (alexandria:write-byte-vector-into-file octets file :if-exists :supersede)
                          file)))
      (ignore-errors (uiop:delete-directory-tree directory :validate t)))))

;;; --- the two requests ------------------------------------------------------------

(defun generation-request (api-model prompt size quality)
  "One POST to /images/generations: the model, the prompt, n 1, and - for a
sized model only - the size and quality it takes."
  (let ((payload (nlk:json-object "model" api-model "prompt" prompt "n" 1)))
    (when quality
      (setf (gethash "size" payload) size
            (gethash "quality" payload) quality))
    (exchange :post (api-url "/images/generations")
              :content (nlk:encode-json-object payload))))

(defun edit-request (api-model prompt size quality sources)
  "One multipart POST to /images/edits: the model, the prompt, and the source
images as files."
  (call-with-source-files
   sources
   (lambda (files)
     (let ((content (append (list (cons "model" api-model)
                                  (cons "prompt" prompt)
                                  (cons "n" "1"))
                            (when quality
                              (list (cons "size" size) (cons "quality" quality)))
                            (if (= 1 (length files))
                                (list (cons "image" (first files)))
                                (mapcar (lambda (file) (cons "image[]" file)) files)))))
       (exchange :post (api-url "/images/edits")
                 :content content
                 :content-type nil)))))

(defun save-answer (item)
  "(values PATH BYTES PIXELS) of the one image the answer carries, decoded
from b64_json or downloaded from its url."
  (let ((encoded (nlk:json-value item :string "b64_json"))
        (url (nlk:json-value item :string "url")))
    (cond (encoded
           (let ((octets (ignore-errors (cl-base64:base64-string-to-usb8-array encoded))))
             (unless octets
               (fail "the gateway's b64_json is not base64"))
             (save-octets octets)))
          (url
           (save-octets (fetch-octets (check-image-url url) "image URL")))
          (t (fail "the gateway answered neither b64_json nor url")))))

;;; --- one picture -----------------------------------------------------------------

(defun render (prompt &key (aspect "square") quality model images)
  "One image, generated or edited => the answer object: the file, its size, the
model, the requested size and quality, the decoded pixel size and the
gateway's revised prompt."
  (let* ((prompt (nlk:trimmed (or prompt "")))
         (sources (remove-if-not #'nonblank (or images '())))
         (aspect (string-downcase (or (nonblank aspect) "square")))
         (label (or (nonblank model) (nonblank (setting :model)) "gpt-image-2")))
    (when (zerop (length prompt))
      (fail "a prompt is required and cannot be empty"))
    (unless (assoc aspect +sizes+ :test #'string=)
      (fail "aspect must be one of ~{~a~^, ~}: ~s"
            (mapcar #'car +sizes+) aspect))
    (when (> (length sources) +max-sources+)
      (fail "~d source images, over the ~a one edit sends" (length sources) +max-sources+))
    (multiple-value-bind (api-model api-quality)
        (resolve-model label (or (nonblank quality) (nonblank (setting :quality))))
      (let* ((edited (and sources t))
             (size (cdr (assoc aspect +sizes+ :test #'string=))))
        (multiple-value-bind (body status)
            (if edited
                (edit-request api-model prompt size api-quality sources)
                (generation-request api-model prompt size api-quality))
          (unless (and (integerp status) (< status 400))
            (http-refusal status body))
          (let* ((object (and (stringp body) (ignore-errors (nlk:decode-json body))))
                 (data (and (hash-table-p object) (nlk:json-array object "data")))
                 (item (and data (plusp (length data)) (aref data 0))))
            (unless item
              (fail "the gateway answered no image data"))
            (multiple-value-bind (path bytes pixels) (save-answer item)
              (let ((answer (nlk:json-object
                             "image" path "bytes" bytes
                             "model" label "api_model" api-model
                             "aspect" aspect
                             "mode" (if edited "edit" "generate"))))
                (when size
                  (setf (gethash "requested_size" answer) size))
                (when api-quality
                  (setf (gethash "requested_quality" answer) api-quality))
                (when pixels
                  (setf (gethash "pixels" answer) pixels))
                (let ((revised (nlk:json-value item :string "revised_prompt")))
                  (when revised
                    (setf (gethash "revised_prompt" answer) revised)))
                answer))))))))

;;; --- the tool --------------------------------------------------------------------

(defun generate-tool (request)
  "The generate_image tool: one picture, or an edit when source images are named."
  (answering
    (nlk:encode-json-object
     (render (image-request-prompt request)
             :aspect (image-request-aspect request)
             :model (image-request-model request)
             :images (coerce (image-request-images request) 'list)))))

;;; --- /image -----------------------------------------------------------------------

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

(defun image-command (args session-id)
  "One /image invocation => the answer's text, and typed alone the dialog every
command answers then: one row that asks for the prompt."
  (declare (ignore session-id))
  (let ((line (nlk:trimmed (or args ""))))
    (if (zerop (length line))
        (values "Describe the picture, then /image with it."
                (nle:list-dialog
                 :title "Image"
                 :summary "Through any OpenAI Images-compatible gateway"
                 :rows (list (nle:ask-row "Generate an image" "Prompt" "/image"
                                           :placeholder "a red fox in snow, wide shot"
                                           :verb "generate"))))
        (multiple-value-bind (prompt options) (command-request line)
          (if (zerop (length prompt))
              (values "Nothing to draw: give /image a prompt." nil)
              (handler-case
                  (let ((answer (render prompt
                                        :aspect (or (getf options :aspect) "square")
                                        :quality (getf options :quality)
                                        :model (getf options :model)
                                        :images (getf options :images))))
                    (values (format nil "~a~@[ · ~a~]~@[ · ~a~]"
                                    (nlk:json-value answer :string "image")
                                    (nlk:json-value answer :string "model")
                                    (nlk:json-value answer :string "requested_size"))
                            nil))
                (imagegen-error (condition)
                  (values (format nil "ERROR: ~a" (imagegen-error-detail condition)) nil))
                (error (condition)
                  (values (format nil "ERROR: imagegen: ~a"
                                  (nlk:one-line (princ-to-string condition)))
                          nil))))))))

(defun generate-image (prompt &key (aspect "square") quality model images)
  "One image from eval, the same way the tool asks for one => the answer object."
  (render prompt :aspect aspect :quality quality :model model :images images))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell imagegen
  (:section ("imagegen")
    (:guide "the API root of a gateway that speaks the OpenAI Images API - CLIProxyAPI, New API, or your own - and the key it wants, if any; a model is already chosen; next step: ask for a picture")
    ("base_url" :string :required t
     :doc "the gateway's API root, usually ending at /v1 (https required unless the host is loopback, LAN, Tailscale or .local)")
    ("api_key_env" :env :default "OPENAI_COMPAT_IMAGE_API_KEY"
     :doc "the environment variable holding the gateway key; leave it naming an unset variable for a keyless local gateway")
    ("api_key_file" :path
     :doc "a file holding the gateway key, which the file's own permissions guard; set instead of api_key_env")
    ("model" :string :default "gpt-image-2"
     :doc "the model id the gateway serves for images; a quality may ride the name, as in gpt-image-2-high")
    ("quality" :choice :options '("auto" "low" "medium" "high" "xhigh" "max") :default "medium"
     :doc "the quality sent for the GPT image models; a gateway-defined model is sent without one")
    ("output_dir" :path :default "~/.nodecode/images"
     :doc "where generated pictures are written")
    ("timeout" :integer :default 180 :min 1
     :doc "how long one gateway request may take, in seconds; a big render is slow"))
  (:settings #'read-settings)
  (:help :imagegen
         "imagegen: generate_image draws or edits a picture through your OpenAI Images-compatible gateway"
         *manual*)
  (:tool "generate_image" (nlk:contract 'image-request) #'generate-tool)
  (:command "image" 'image-command
            :description "Generate or edit an image through your gateway"
            :argument-hint "PROMPT"))
