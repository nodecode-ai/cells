;;;; cell.lisp --- dashscope_image and /dashscope, over Alibaba's model studio.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One API base the operator names, in the `dashscope' section of the shared
;;;; config:
;;;;   "dashscope": {"api": "https://dashscope-intl.aliyuncs.com",
;;;;                 "api_key_env": "DASHSCOPE_API_KEY", "model": "wan2.7-image"}
;;;; Everything the cell does is one POST to
;;;; <api>/api/v1/services/aigc/multimodal-generation/generation - the native
;;;; multimodal-generation wire, not the OpenAI-compatible /images path, which
;;;; the token-plan host does not serve image models on - and one file written
;;;; under output_dir. Nothing hooks the provider path: DashScope speaks HTTP,
;;;; and NLK:HTTP is the one seam.
;;;;
;;;; Ported from rriggs's Hermes Agent plugin (see NOTICE): the two models and
;;;; their upgrade rule, the three aspect sizes in DashScope's W*H form, the
;;;; config-keyed base URL and env var, the local-path-to-data-URI inlining
;;;; with its 10 MB cap, the answer image cached from its URL because the OSS
;;;; links expire, and the API-level error shape.

(in-package #:nodecode-dashscope-image)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Images are available through the nodecode-dashscope-image cell: the dashscope_image
tool, which a person can also call from eval in the dashscope package, and the /dashscope
command.
  dashscope_image  {prompt, aspect, model, images}
      One POST to <api>/api/v1/services/aigc/multimodal-generation/generation, one
      picture written under the cell's output_dir. The answer names the file, its size in
      bytes, the model, the aspect and the size asked for, and the decoded pixel size.
      Say where the file is; do not paste its bytes.
      aspect is landscape (1280x720), square (1024x1024) or portrait (720x1280).
      model is wan2.7-image (fast text-to-image) or wan2.7-image-pro (higher fidelity,
      image-to-image); a call that names a source image upgrades the fast model to the
      pro one on its own.
      images names source pictures to restyle: absolute local paths, https URLs or data:
      URIs, up to 4, each at most 10 MB. A local file is sent base64-inlined in the
      request, because DashScope fetches a URL from its own side.
Write the prompt as an image model reads it - subject, setting, style, light, framing - not
as a question. The picture is downloaded as soon as it is drawn, because DashScope's OSS
links expire. A failed call says the HTTP status, DashScope's own code and message, and a
hint: a wrong key, an api base belonging to another region or plan, a model the key
cannot reach, or a rate limit. Type /dashscope with a prompt for the same thing from the
operator's side: /dashscope --landscape a red fox in snow; /dashscope --image
~/pictures/fox.png --portrait make the sky dusk; /dashscope --model wan2.7-image-pro a
heron. Typed alone it asks for the prompt."
  "What (help :dashscope-image) answers while the cell runs.")

;;; --- what DashScope's Wan models take --------------------------------------------

(defparameter +sizes+
  '(("landscape" . "1280*720") ("square" . "1024*1024") ("portrait" . "720*1280"))
  "The aspect the call names to the size DashScope takes, in its W*H form.")

(defparameter +models+ '("wan2.7-image" "wan2.7-image-pro")
  "The two models the original plugin names.")

(defparameter +default-model+ "wan2.7-image"
  "The fast model, when neither the call nor the section names one.")

(defparameter +edit-model+ "wan2.7-image-pro"
  "The model a source image needs: the fast one is text-to-image only.")

(defparameter +default-api+ "https://token-plan.ap-southeast-1.maas.aliyuncs.com"
  "The token-plan base the original plugin defaults to. A pay-as-you-go key
wants the regional host instead, https://dashscope-intl.aliyuncs.com.")

(defparameter +generation-path+
  "/api/v1/services/aigc/multimodal-generation/generation"
  "The native multimodal-generation route, relative to the api base.")

(defparameter +max-source-bytes+ (* 10 1024 1024)
  "The most one source image may be, in bytes: the original plugin's inline cap.")

(defparameter +max-sources+ 4
  "The most source images one call sends. The original advertised one
reference image; a call here may name up to four and all of them ride as
image members of the one user message.")

(defparameter +error-chars+ 300
  "How much of DashScope's own error text a refusal quotes back.")

(defparameter +user-agent+ "nodecode-dashscope-image/0.1.0"
  "What the cell calls itself to DashScope. Nothing of another client's is
impersonated, and no other product's session header rides along.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (dashscope-image-error (condition)
       (nle:failure (format nil "ERROR: ~a" (dashscope-image-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: dashscope-image: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun answer-detail (body)
  "DashScope's own words for a failed call, clipped, or NIL."
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
      (nlk:config-error "api is empty: set it to the DashScope base the key belongs to, e.g. https://dashscope-intl.aliyuncs.com"))
    (when (find-if (lambda (character) (member character '(#\Space #\Tab #\Newline #\Return))) url)
      (nlk:config-error "api ~s holds whitespace" url))
    (when (nth-value 2 (url-parts url))
      (nlk:config-error "api must not carry credentials in it: name the key in api_key_env"))
    (multiple-value-bind (scheme host) (url-parts url)
      (unless scheme
        (nlk:config-error "api ~s is not an absolute URL: give the scheme too, e.g. https://dashscope-intl.aliyuncs.com" url))
      (unless (member scheme '("http" "https") :test #'string=)
        (nlk:config-error "api scheme ~s is not http or https" scheme))
      (when (and (string= scheme "http") (not (private-host-p host)))
        (nlk:config-error "api ~s is plain HTTP and ~a is not a loopback, LAN, Tailscale or .local host: the key would cross the public internet in clear text. Use https://" url host))
      (string-right-trim "/" url))))

(defun read-settings (values table)
  "The declared VALUES, plus the key the section names and the folder images
are written to: read once, here."
  (declare (ignore table))
  (let* ((api (check-api (or (getf values :api) +default-api+)))
         (env (nonblank (getf values :api-key-env)))
         (file (nonblank (getf values :api-key-file)))
         (plist (copy-list values)))
    (unless (or env file)
      (nlk:config-error "api_key_env is required: name the environment variable holding the DashScope key, or a file in api_key_file"))
    (when (and env file)
      (nlk:config-error "set one of api_key_env and api_key_file, not both"))
    (setf (getf plist :api) api
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

;;; --- the bytes DashScope answers, and what they are ------------------------------

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

(defun save-octets (octets &key (stem "dashscope"))
  "(values PATH BYTES PIXELS): OCTETS written under the cell's output_dir,
never over another file there."
  (let ((type (image-type octets)))
    (unless type
      (fail "DashScope answered bytes that are not a PNG, JPEG, WebP or GIF image"))
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
  "The `image' member one source rides as: an https URL or data URI verbatim,
or a local file base64-inlined as a data URI - DashScope fetches a URL from
its own side, so a path only the operator's machine can see has to travel."
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

;;; --- one exchange with DashScope -------------------------------------------------

(defun api-url ()
  (concatenate 'string (setting :api) +generation-path+))

(defun exchange (method url &key content (content-type "application/json") binary)
  "One exchange with DashScope => (values BODY STATUS), BODY the text or the
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
    ((400 422) " (check the model id, the size and the reference image: DashScope refuses a body its model does not take)")
    ((401 403) " (check the key in the variable api_key_env names, and that api is the host that key belongs to)")
    (404 " (check api is the DashScope base - the token-plan and dashscope-intl hosts serve the same path, not the same keys)")
    (429 " (DashScope is rate limiting or the plan is out of quota: retry in a moment)")
    (t nil)))

(defun error-message (body)
  "DashScope's own words for a failed answer: its code and message when the
body is the JSON object it answers with, else the text itself."
  (let* ((text (and (stringp body) body))
         (object (and text
                      (plusp (length (nlk:trimmed text)))
                      (ignore-errors (nlk:decode-json text))))
         (inner (and (hash-table-p object) (nlk:json-value object :object "error"))))
    (flet ((named (table)
             (let ((code (nlk:json-value table :string "code"))
                   (message (nlk:json-value table :string "message")))
               (cond ((and code message) (format nil "~a: ~a" code message))
                     (message message)
                     (code code)))))
      (or (and (hash-table-p inner) (named inner))
          (and (hash-table-p object) (named object))
          (and text (plusp (length (nlk:trimmed text)))
               (nlk:clip (nlk:one-line text) +error-chars+))
          "no error message"))))

(defun replace-text (text part replacement)
  "TEXT with every occurrence of PART replaced, so a key DashScope echoes back
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
  "A failed answer as a refusal: the status, DashScope's words, and the hint
that usually fixes it."
  (fail "DashScope answered HTTP ~a: ~a~@[~a~]"
        status (redact (error-message body)) (status-hint status)))

;;; --- one picture -----------------------------------------------------------------

(defun request-payload (model prompt size sources)
  "The multimodal-generation body: one user message whose content is the
image members, then the text one - the order the wire expects."
  (nlk:json-object
   "model" model
   "input" (nlk:json-object
            "messages"
            (vector (nlk:json-object
                     "role" "user"
                     "content"
                     (concatenate 'vector
                                  (map 'vector (lambda (source)
                                                 (nlk:json-object "image" source))
                                       sources)
                                  (vector (nlk:json-object "text" prompt))))))
   "parameters" (nlk:json-object "size" size)))

(defun answer-image (object)
  "(values REF KIND): the image the answer carries - a URL, or a data URI -
from output.choices[0].message.content, or output.results, which some models
answer with instead."
  (let* ((output (nlk:json-value object :object "output"))
         (choices (and output (nlk:json-array output "choices")))
         (message (and choices (plusp (length choices))
                       (nlk:json-value (aref choices 0) :object "message")))
         (content (and message (nlk:json-array message "content")))
         (results (and output (nlk:json-array output "results"))))
    (or (loop for item across (or content #())
              for ref = (and (hash-table-p item)
                             (or (nlk:json-value item :string "image")
                                 (nlk:json-value item :string "url")))
              when (nonblank ref) return (cons ref "content"))
        (loop for item across (or results #())
              for ref = (and (hash-table-p item)
                             (nlk:json-value item :string "url"))
              when (nonblank ref) return (cons ref "results")))))

(defun save-answer (reference)
  "(values PATH BYTES PIXELS) of the picture REFERENCE names: a data URI
decoded, or a URL fetched at once because the OSS link expires."
  (if (uiop:string-prefix-p "data:" reference)
      (let* ((comma (position #\, reference))
             (encoded (and comma (subseq reference (1+ comma))))
             (octets (and encoded
                          (ignore-errors (cl-base64:base64-string-to-usb8-array encoded)))))
        (unless octets
          (fail "DashScope answered a data URI that is not base64"))
        (save-octets octets))
      (multiple-value-bind (body status) (exchange :get reference :content-type nil :binary t)
        (unless (and (integerp status) (< status 400))
          (fail "the image URL answered HTTP ~a" status))
        (unless (typep body '(vector (unsigned-byte 8)))
          (fail "the image URL answered no bytes"))
        (save-octets body))))

(defun render (prompt &key (aspect "square") model images)
  "One image, generated or edited => the answer object: the file, its size,
the model, the aspect and the size asked for, the decoded pixel size, and
what the call was."
  (let* ((prompt (nlk:trimmed (or prompt "")))
         (sources (remove-if-not #'nonblank (or images '())))
         (aspect (string-downcase (or (nonblank aspect) "square")))
         (label (or (nonblank model) (nonblank (setting :model)) +default-model+)))
    (when (zerop (length prompt))
      (fail "a prompt is required and cannot be empty"))
    (unless (assoc aspect +sizes+ :test #'string=)
      (fail "aspect must be one of ~{~a~^, ~}: ~s"
            (mapcar #'car +sizes+) aspect))
    (unless (member label +models+ :test #'string=)
      (fail "model must be one of ~{~a~^, ~}: ~s" +models+ label))
    (when (> (length sources) +max-sources+)
      (fail "~d source images, over the ~a one call sends" (length sources) +max-sources+))
    (unless (nonblank (setting :api-key))
      (fail "no DashScope key: set the environment variable ~a names, or name a file in api_key_file"
            (or (nonblank (setting :api-key-env)) "api_key_env")))
    (let* ((edited (and sources t))
           (model (if (and edited (equal label +default-model+)) +edit-model+ label))
           (size (cdr (assoc aspect +sizes+ :test #'string=)))
           (items (mapcar #'source-item sources)))
      (multiple-value-bind (body status)
          (exchange :post (api-url)
                    :content (nlk:encode-json-object
                              (request-payload model prompt size items)))
        (unless (and (integerp status) (< status 400))
          (http-refusal status body))
        (let ((object (and (stringp body) (ignore-errors (nlk:decode-json body)))))
          (unless (hash-table-p object)
            (fail "DashScope answered no JSON object"))
          ;; DashScope reports some refusals inside a 200: a code and a message.
          (let ((code (nlk:json-value object :string "code")))
            (when (nonblank code)
              (fail "DashScope refused the call: ~a: ~a"
                    code (or (nlk:json-value object :string "message") "no message"))))
          (let ((reference (answer-image object)))
            (unless reference
              (fail "DashScope answered no image: ~a"
                    (or (answer-detail body) "an empty answer")))
            (multiple-value-bind (path bytes pixels) (save-answer (car reference))
              (let ((answer (nlk:json-object
                             "image" path "bytes" bytes
                             "model" model "aspect" aspect
                             "size" size
                             "mode" (if edited "edit" "generate"))))
                (when pixels
                  (setf (gethash "pixels" answer) pixels))
                (let ((usage (nlk:json-value object :object "usage")))
                  (when usage
                    (let ((count (nlk:json-value usage :integer "image_count")))
                      (when count
                        (setf (gethash "images_billed" answer) count)))))
                answer))))))))

;;; --- the tool --------------------------------------------------------------------

(defun dashscope-image-tool (request)
  "The dashscope_image tool: one picture, or an edit when source images are named."
  (answering
    (nlk:encode-json-object
     (render (image-request-prompt request)
             :aspect (image-request-aspect request)
             :model (image-request-model request)
             :images (coerce (image-request-images request) 'list)))))

;;; --- /dashscope ------------------------------------------------------------------

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

(defun dashscope-command (args session-id)
  "One /dashscope invocation => the answer's text, and typed alone the dialog
every command answers then: one row that asks for the prompt."
  (declare (ignore session-id))
  (let ((line (nlk:trimmed (or args ""))))
    (if (zerop (length line))
        (values "Describe the picture, then /dashscope with it."
                (nle:list-dialog
                 :title "DashScope"
                 :summary "Wan 2.7 image models, through your Qwen Cloud key"
                 :rows (list (nle:ask-row "Generate an image" "Prompt" "/dashscope"
                                           :placeholder "a red fox in snow, wide shot"
                                           :verb "generate"))))
        (multiple-value-bind (prompt options) (command-request line)
          (if (zerop (length prompt))
              (values "Nothing to draw: give /dashscope a prompt." nil)
              (handler-case
                  (let ((answer (render prompt
                                        :aspect (or (getf options :aspect) "square")
                                        :model (getf options :model)
                                        :images (getf options :images))))
                    (values (format nil "~a~@[ · ~a~]~@[ · ~a~]"
                                    (nlk:json-value answer :string "image")
                                    (nlk:json-value answer :string "model")
                                    (nlk:json-value answer :string "size"))
                            nil))
                (dashscope-image-error (condition)
                  (values (format nil "ERROR: ~a" (dashscope-image-error-detail condition)) nil))
                (error (condition)
                  (values (format nil "ERROR: dashscope-image: ~a"
                                  (nlk:one-line (princ-to-string condition)))
                          nil))))))))

(defun generate-image (prompt &key (aspect "square") model images)
  "One image from eval, the same way the tool asks for one => the answer object."
  (render prompt :aspect aspect :model model :images images))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell dashscope-image
  (:section ("dashscope")
    (:guide "the DashScope base and the environment variable holding your Qwen Cloud key - token plan (sk-sp-*) or pay-as-you-go (sk-ws-*); a model is already chosen; next step: ask for a picture")
    ("api" :string :default "https://token-plan.ap-southeast-1.maas.aliyuncs.com"
     :doc "the DashScope API base: the token-plan host, or https://dashscope-intl.aliyuncs.com for a pay-as-you-go key")
    ("api_key_env" :env :required t
     :doc "the environment variable holding the DashScope key - sk-sp-* on a token plan, sk-ws-* pay-as-you-go - never the key itself")
    ("api_key_file" :path
     :doc "a file holding the key, which the file's own permissions guard; set instead of api_key_env")
    ("model" :choice :options '("wan2.7-image" "wan2.7-image-pro") :default "wan2.7-image"
     :doc "wan2.7-image is the fast one, text-to-image only; wan2.7-image-pro is higher fidelity and takes a source image")
    ("output_dir" :path :default "~/.nodecode/images"
     :doc "where generated pictures are written")
    ("timeout" :integer :default 180 :min 1
     :doc "how long one DashScope request may take, in seconds; a render is slow"))
  (:settings #'read-settings)
  (:help :dashscope-image
         "dashscope-image: dashscope_image draws or restyles a picture with Alibaba's Wan 2.7 models"
         *manual*)
  (:tool "dashscope_image" (nlk:contract 'image-request) #'dashscope-image-tool)
  (:command "dashscope" 'dashscope-command
            :description "Draw or restyle an image with DashScope (Qwen Cloud)"
            :argument-hint "PROMPT"))
