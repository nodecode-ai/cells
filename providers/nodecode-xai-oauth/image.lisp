;;;; image.lisp --- Grok Imagine: one picture from a prompt, on the xAI sign-in.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The xai_oauth_image tool. It sits beside codex_sub_image -- a prompt, a
;;;; leaf filename inside media_root, never an overwrite -- and asks
;;;; POST {base_url}/images/generations with the access token /xai-oauth login
;;;; stored, refreshed first when it is about to expire. Nothing else asks in
;;;; its place: not XAI_OAUTH_TOKEN, not a key /connect saved, not a configured
;;;; API key. xAI's bytes are written as they came (no re-encode), under the
;;;; extension of the format they are, so fox.png asked and a JPEG answered
;;;; lands as fox.jpg; the answer says the path and the mime type.
;;;;
;;;; Quota: the answer carries the usage object and the x-ratelimit-* headers
;;;; exactly as xAI sent them, and says so when xAI sent none. It never
;;;; computes a remaining-images figure; xAI reports none.
;;;;
;;;; Ported from oh-my-pi's openai-images xAI branch (see NOTICE): the
;;;; grok-imagine-image model, aspect_ratio 1:1 and resolution 1k by default,
;;;; response_format b64_json, n 1.

(in-package #:nodecode-xai-oauth)

(defparameter +image-tool+ "xai_oauth_image"
  "The tool's name, beside codex_sub_image.")

(defparameter +image-model+ "grok-imagine-image"
  "The Grok Imagine model asked when image_model names none.")

(defparameter +image-timeout+ 180
  "Seconds one image request may take, the dial included.")

(defparameter +aspect-ratios+
  #("1:1" "3:4" "4:3" "9:16" "16:9" "2:3" "3:2" "9:19.5" "19.5:9" "9:20" "20:9"
    "1:2" "2:1" "21:9" "5:2" "auto")
  "The aspect ratios xAI's images/generations accepts.")

(defparameter +resolutions+ #("1k" "1.5k" "2k")
  "The resolutions xAI's images/generations accepts.")

(defparameter +image-arguments+ '("prompt" "output" "aspect_ratio" "resolution")
  "Every argument the tool takes; any other is refused, an api_key first among them.")

(defparameter +image-types+ '("png" "jpg" "jpeg" "webp")
  "The extensions an output name may carry, and every name a stem is checked under.")

(defun image-schema ()
  "The tool's declaration: what the model is shown, as codex_sub_image shows its own."
  (list :description "Generate one image from a prompt with Grok Imagine through the xAI sign-in (/xai-oauth login), never an API key. output is a leaf filename in media_root ending .png, .jpg, .jpeg or .webp; xAI's own bytes are kept, so the extension follows the format xAI returns (usually JPEG: fox.png becomes fox.jpg). Never overwrites. The answer gives path, mime_type, and the usage and rate-limit headers xAI reported; xAI reports no image quota."
        :parameters
        (nlk:json-object
         "type" "object"
         "required" #("prompt" "output")
         "additionalProperties" :false
         "properties"
         (nlk:json-object
          "prompt" (nlk:json-object "type" "string" "description" "what to picture")
          "output" (nlk:json-object "type" "string"
                                    "description" "a new leaf filename inside media_root")
          "aspect_ratio" (nlk:json-object "type" "string" "enum" +aspect-ratios+ "default" "1:1")
          "resolution" (nlk:json-object "type" "string" "enum" +resolutions+ "default" "1k")))))

;;; --- the arguments -----------------------------------------------------------------

(defun image-argument (arguments name &optional choices default)
  "The string argument NAME of ARGUMENTS, else DEFAULT; refused when it is
not a string or, with CHOICES, not one of them."
  (multiple-value-bind (value present) (gethash name arguments)
    (cond ((or (not present) (null value)) default)
          ((not (stringp value)) (error "~a must be a string" name))
          ((and choices (not (find value choices :test #'string=)))
           (error "~a must be one of ~{~a~^, ~}; not ~s" name (coerce choices 'list) value))
          (t value))))

(defun output-parts (name)
  "(values STEM TYPE) of the output NAME, TYPE downcased; refused unless NAME
is a leaf filename ending in one of +IMAGE-TYPES+."
  (let ((dot (and (stringp name) (position #\. name :from-end t))))
    (unless (and dot (plusp dot)
                 (notany (lambda (c) (or (find c "/\\:") (< (char-code c) 32) (= (char-code c) 127)))
                         name)
                 (member (string-downcase (subseq name (1+ dot))) +image-types+ :test #'string=))
      (error "output must be a leaf filename inside media_root ending .png, .jpg, .jpeg or .webp, not ~s"
             name))
    (values (subseq name 0 dot) (string-downcase (subseq name (1+ dot))))))

(defun media-root ()
  "The media_root directory as a native namestring ending in a slash; it must exist."
  (let ((root (ignore-errors (truename (uiop:ensure-directory-pathname (setting :media-root))))))
    (unless root
      (error "media_root ~s is not an existing directory" (setting :media-root)))
    (sb-ext:native-namestring root)))

(defun taken-p (path)
  "Whether a directory entry is at PATH, a dangling link included."
  (and (ignore-errors (sb-posix:lstat path)) t))

(defun refuse-taken (root name stem)
  "Refuse when NAME, or STEM under any of +IMAGE-TYPES+, is already in ROOT:
whichever format xAI answers, its file must be new."
  (dolist (leaf (cons name (mapcar (lambda (type) (format nil "~a.~a" stem type)) +image-types+)))
    (when (taken-p (concatenate 'string root leaf))
      (error "~a already exists in media_root; choose a new filename" leaf))))

;;; --- the sign-in ---------------------------------------------------------------

(defun image-token ()
  "The stored sign-in's access token, refreshed first when it is about to
expire; refused when there is none. Never a key or a token variable."
  (let ((token (nlk:json-value (fresh-entry nle::*auth-file-path*) :text "access_token")))
    (unless token
      (error "xai_oauth_image needs the xAI sign-in: run /xai-oauth login. It never uses an API key, a saved key or XAI_OAUTH_TOKEN."))
    token))

(defun images-url ()
  "Where images/generations is served: base_url's, which must be https on x.ai,
the only place the sign-in's token is sent."
  (let ((base (string-right-trim "/" (setting :base-url))))
    (unless (xai-host-p base)
      (error "base_url ~s is not an https x.ai address; the sign-in token is only sent to xAI" base))
    (concatenate 'string base "/images/generations")))

;;; --- the request ---------------------------------------------------------------

(defun post-image (url token body)
  "POST the JSON BODY to URL with TOKEN as the bearer. => (values BODY STATUS
HEADERS), BODY decoded as JSON or else its text: a refusal is an answer like
any other; only a transport failure or the deadline signals."
  (multiple-value-bind (answer status headers)
      (with-timeout (+image-timeout+ url)
        (handler-case
            (dex:post url :headers `(("Authorization" . ,(format nil "Bearer ~a" token))
                                     ("Content-Type" . "application/json")
                                     ("Accept" . "application/json"))
                          :content (nlk:encode-json-object body)
                          :connect-timeout 30 :read-timeout +image-timeout+)
          (dex:http-request-failed (condition)
            (values (dex:response-body condition) (dex:response-status condition)
                    (dex:response-headers condition)))))
    (let ((text (nlk:body-text answer)))
      (values (or (ignore-errors (nlk:decode-json text)) text) status headers))))

(defun refusal-words (body)
  "What xAI said in a refused BODY: its error or message, else an excerpt."
  (or (and (hash-table-p body)
           (let ((error (gethash "error" body)))
             (or (and (stringp error) error)
                 (nlk:json-value error :string "message")
                 (nlk:json-value body :string "message"))))
      (excerpt body)))

(defun image-datum (body)
  "The first data entry of an images/generations BODY, or NIL."
  (let ((data (and (hash-table-p body) (gethash "data" body))))
    (and (vectorp data) (plusp (length data)) (hash-table-p (aref data 0)) (aref data 0))))

(defun image-bytes (datum body)
  "The bytes DATUM's b64_json carries; refused when there are none."
  (let ((encoded (nlk:json-value datum :string "b64_json")))
    (or (and encoded (plusp (length encoded))
             (ignore-errors (cl-base64:base64-string-to-usb8-array encoded)))
        (error "xAI's answer carried no image bytes (b64_json); nothing was written: ~a"
               (excerpt body)))))

(defun bytes-at-p (bytes offset signature)
  "Whether BYTES hold SIGNATURE (a list of octets) at OFFSET."
  (and (>= (length bytes) (+ offset (length signature)))
       (every #'= signature (subseq bytes offset (+ offset (length signature))))))

(defun image-format (bytes)
  "(values MIME TYPE) of BYTES by their magic numbers; refused unless PNG,
JPEG or WebP."
  (cond ((bytes-at-p bytes 0 '(137 80 78 71 13 10 26 10)) (values "image/png" "png"))
        ((bytes-at-p bytes 0 '(255 216 255)) (values "image/jpeg" "jpg"))
        ((and (bytes-at-p bytes 0 '(82 73 70 70)) (bytes-at-p bytes 8 '(87 69 66 80)))
         (values "image/webp" "webp"))
        (t (error "xAI's bytes are not a PNG, JPEG or WebP; nothing was written"))))

(defun final-name (name stem asked type)
  "NAME when its extension ASKED already names format TYPE, else STEM.TYPE."
  (if (or (string= asked type) (and (string= type "jpg") (string= asked "jpeg")))
      name
      (format nil "~a.~a" stem type)))

(defun write-new-file (path bytes root)
  "Write BYTES to PATH, which must not exist: into a temp file in ROOT, then a
hard link, which refuses an existing PATH, then the temp file unlinked --
nothing is left behind, whatever happens."
  (multiple-value-bind (fd temp) (sb-posix:mkstemp (concatenate 'string root ".xai-oauth-image-XXXXXX"))
    (unwind-protect
         (let ((stream (sb-sys:make-fd-stream fd :output t :element-type '(unsigned-byte 8)
                                                 :buffering :full :auto-close nil)))
           (unwind-protect
                (progn (write-sequence bytes stream)
                       (finish-output stream)
                       (sb-posix:fchmod fd #o644)
                       (sb-posix:fsync fd))
             ;; closing the fd-stream closes FD
             (close stream))
           (handler-case (sb-posix:link temp path)
             (sb-posix:syscall-error ()
               (error "~a already exists in media_root; choose a new filename" (file-namestring path)))))
      (ignore-errors (sb-posix:unlink temp)))))

;;; --- what the answer says -------------------------------------------------------

(defun rate-limit (headers)
  "The x-ratelimit-* HEADERS xAI sent, as an object, or NIL when none."
  (let ((limits (nlk:json-object)))
    (when (hash-table-p headers)
      (maphash (lambda (name value)
                 (let ((name (string-downcase (string name))))
                   (when (uiop:string-prefix-p "x-ratelimit" name)
                     (setf (gethash name limits) (princ-to-string value)))))
               headers))
    (and (plusp (hash-table-count limits)) limits)))

(defun pairs-text (object)
  "OBJECT's members as name=value, sorted, comma-separated."
  (let ((pairs '()))
    (maphash (lambda (name value) (push (format nil "~a=~a" name (if (hash-table-p value) (nlk:encode-json-object value) value)) pairs))
             object)
    (format nil "~{~a~^, ~}" (sort pairs #'string<))))

(defun limits-sentence (usage limits)
  "What xAI reported about cost and quota, with silence said as silence."
  (format nil "~:[xAI reported no usage~;~:*xAI reported usage ~a~]; ~:[no rate-limit headers~;~:*rate-limit headers ~a~]; xAI reports no image quota or remaining-images figure for the sign-in."
          (and (hash-table-p usage) (plusp (hash-table-count usage)) (pairs-text usage))
          (and limits (pairs-text limits))))

;;; --- the tool ------------------------------------------------------------------

(defun generate-image (arguments)
  "The xai_oauth_image tool: ARGUMENTS (the decoded call) checked, the taken
names refused, then one images/generations request on the sign-in, its bytes
written as a new file. => the answer as JSON text."
  (unless (hash-table-p arguments) (error "the arguments must be a JSON object"))
  (maphash (lambda (name value)
             (declare (ignore value))
             (unless (member name +image-arguments+ :test #'equal)
               (error "unknown argument ~a; ~a takes ~{~a~^, ~}" name +image-tool+ +image-arguments+)))
           arguments)
  (let ((prompt (image-argument arguments "prompt"))
        (name (image-argument arguments "output"))
        (ratio (image-argument arguments "aspect_ratio" +aspect-ratios+ "1:1"))
        (resolution (image-argument arguments "resolution" +resolutions+ "1k"))
        (model (let ((configured (setting :image-model)))
                 (if (and (stringp configured) (plusp (length configured))) configured +image-model+))))
    (unless (and prompt (plusp (length (string-trim '(#\Space #\Tab #\Newline) prompt))))
      (error "prompt must be a nonempty string"))
    (multiple-value-bind (stem asked) (output-parts name)
      (let ((root (media-root)))
        (refuse-taken root name stem)
        (let ((url (images-url)))
          (multiple-value-bind (body status headers)
              (post-image url (image-token)
                          (nlk:json-object "model" model "prompt" prompt "n" 1
                                           "response_format" "b64_json"
                                           "aspect_ratio" ratio "resolution" resolution))
            (unless (ok-p status)
              (error "xAI refused the image request: HTTP ~a: ~a" status (refusal-words body)))
            (let* ((datum (image-datum body))
                   (bytes (image-bytes datum body)))
              (multiple-value-bind (mime type) (image-format bytes)
                (let* ((leaf (final-name name stem asked type))
                       (path (concatenate 'string root leaf))
                       (usage (and (hash-table-p body) (nlk:json-value body :object "usage")))
                       (limits (rate-limit headers))
                       (answer (nlk:json-object "path" path "mime_type" mime "requested" name
                                                "bytes" (length bytes) "model" model
                                                "limits" (limits-sentence usage limits)
                                                "access" "xai-oauth sign-in")))
                  (write-new-file path bytes root)
                  (alexandria:when-let (provider-mime (nlk:json-value datum :string "mime_type"))
                    (setf (gethash "provider_mime_type" answer) provider-mime))
                  (alexandria:when-let (revised (nlk:json-value datum :string "revised_prompt"))
                    (setf (gethash "revised_prompt" answer) revised))
                  (when usage (setf (gethash "usage" answer) usage))
                  (when limits (setf (gethash "rate_limit" answer) limits))
                  (nlk:encode-json-object answer))))))))))
