;;;; cell.lisp --- vision-inline: the session model reads the image itself.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of BrokeSkill/hermes-vision-inline, whose one rule
;;;; this keeps: when the session's model takes image input, the image goes to
;;;; that model as an image and stays in the conversation, rather than being
;;;; handed to a separate captioning backend that answers in prose. The
;;;; original did it from inside a middleware that replaced Hermes's
;;;; vision_analyze handler; here it is the see_image tool the model calls,
;;;; and /vision for the operator.
;;;;
;;;; Nodecode already shows the session model a local file with (look PATH),
;;;; so this cell adds what look cannot do: a URL, a tool call the model makes
;;;; on its own, and the other half of the original's rule -- when the model
;;;; reads text only, or the section says text, the image goes to the
;;;; OpenAI-compatible captioner the section names and its answer comes back.
;;;;
;;;; The decision itself is one pure function, ROUTE-FOR, so it is testable
;;;; without a turn, a provider or a socket.

(defpackage #:nodecode-vision-inline
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:see-image #:run-command
           #:route-for #:sees-images-p #:attach-to-turn #:*attach*
           #:source-bytes #:base64-encode #:caption
           #:file-path #:url-p #:url-scheme #:non-empty))

(in-package #:nodecode-vision-inline)

(nlk:define-peripheral vision-inline :not-running t)

(defparameter +default-question+
  "Describe this image: what it shows, and every detail a reader would need to act on it."
  "What the captioner is asked when the model passes no question of its own.")

(defparameter +image-types+
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("webp" . "image/webp") ("gif" . "image/gif"))
  "The formats a model may be shown, by extension -- a hint only: what the
bytes say (NLE:IMAGE-MEDIA-TYPE) decides.")

;;; --- the section, read back --------------------------------------------------

(defun non-empty (text)
  (and (stringp text) (plusp (length (string-trim '(#\Space #\Tab) text)))
       (string-trim '(#\Space #\Tab) text)))

(defun inline-mode ()
  "auto attaches the image when the model takes images; text always asks the captioner."
  (let ((mode (non-empty (setting :image-input-mode))))
    (if mode (string-downcase mode) "auto")))

(defun caption-base ()
  "The captioner's base URL, without a trailing slash; empty when unset."
  (let ((given (non-empty (setting :api-base))))
    (if given (string-right-trim "/" given) "")))

(defun caption-model ()
  (or (non-empty (setting :model)) ""))

(defun caption-key ()
  "The token the section names, inline or by environment variable."
  (or (non-empty (setting :api-key))
      (let ((variable (non-empty (setting :api-key-env))))
        (and variable (non-empty (uiop:getenv variable))))))

(defun url-scheme (url)
  (let ((end (and (stringp url) (search "://" url))))
    (and end (string-downcase (subseq url 0 end)))))

(defun url-p (text)
  (member (url-scheme text) '("http" "https") :test #'equal))

(defun url-host (url)
  (let ((start (search "://" url)))
    (when start
      (let* ((rest (subseq url (+ start 3)))
             (end (or (position-if (lambda (character)
                                     (member character '(#\/ #\? #\:)))
                                   rest)
                      (length rest))))
        (string-downcase (subseq rest 0 end))))))

(defun loopback-url-p (url)
  (member (url-host url) '("127.0.0.1" "localhost" "[::1]" "::1") :test #'equal))

(defun check-config ()
  "Refuse a section that could never serve the path it would take, naming the member."
  (when (equal (inline-mode) "text")
    ;; Every call goes to the captioner, so there has to be one.
    (when (zerop (length (caption-base)))
      (error 'nlk:config-refusal
             :detail "vision-inline: \"api_base\" is required when \"image_input_mode\" is \"text\": the OpenAI-compatible captioner's base URL, such as https://api.example.com/v1"))
    (when (zerop (length (caption-model)))
      (error 'nlk:config-refusal
             :detail "vision-inline: \"model\" is required when \"image_input_mode\" is \"text\": the captioning model the endpoint serves")))
  (when (plusp (length (caption-base)))
    (when (zerop (length (caption-model)))
      (error 'nlk:config-refusal
             :detail "vision-inline: \"model\" is required beside \"api_base\": the captioning model the endpoint serves"))
    (unless (or (equal "https" (url-scheme (caption-base)))
                (loopback-url-p (caption-base))
                (setting :allow-insecure-http))
      (error 'nlk:config-refusal
             :detail "vision-inline: \"api_base\" is not HTTPS: pass \"allow_insecure_http\": true for a captioner you trust, or use https"))))

(defun endpoint ()
  (concatenate 'string (caption-base) "/chat/completions"))

(defun auth-headers ()
  (let ((key (caption-key)))
    (if key (list (cons "authorization" (format nil "Bearer ~a" key))) '())))

;;; --- which path an image takes ------------------------------------------------

(defun sees-images-p ()
  "Whether this session's model takes image input, by the models.dev row
PROVIDER lands on: T, NIL when the row says text only, :UNKNOWN when there is
no row -- an unknown model is tried and the provider's own answer decides,
as (look PATH) does."
  (handler-case
      (let* ((model (and (boundp 'nodecode.evolved::*model*)
                         (symbol-value 'nodecode.evolved::*model*)))
             (provider (and (boundp 'nodecode.evolved::*provider*)
                            (symbol-value 'nodecode.evolved::*provider*)))
             (verdict (nodecode.evolved::model-reads-images-p model provider)))
        (cond ((eq verdict :unknown) :unknown) (verdict t) (t nil)))
    (error () :unknown)))

(defun route-for (mode sees)
  "MODE -- the section's image_input_mode -- and SEES -- SEES-IMAGES-P's
answer -- as the path one image takes: :INLINE to the session model, :CAPTION
to the configured captioner. The whole rule of the cell."
  (if (and (equal mode "auto") (not (null sees)))
      :inline
      :caption))

(defun route ()
  (route-for (inline-mode) (sees-images-p)))

;;; --- bytes, from a path or a URL ------------------------------------------------

(defun expand-home (text)
  (cond ((equal text "~") (or (uiop:getenv "HOME") text))
        ((and (> (length text) 1) (char= (char text 0) #\~)
              (member (char text 1) '(#\/ #\\) :test #'char=))
         (concatenate 'string (or (uiop:getenv "HOME") "") (subseq text 1)))
        (t text)))

(defun file-path (text)
  "TEXT as a pathname, with a leading ~ expanded."
  (uiop:parse-native-namestring (expand-home text)))

(defun base64-encode (octets)
  (cl-base64:usb8-array-to-base64-string octets))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buffer in)
      buffer)))

(defun max-bytes ()
  (* 1024 1024 (setting :max-file-mb)))

(defun fetch-octets (url)
  "URL's body as octets, under the section's ceiling."
  (let ((body nil) (status nil) (headers nil) (truncated nil))
    (handler-case
        (multiple-value-setq (body status headers truncated)
          (nlk:http :get url
                    :binary t
                    :max-bytes (max-bytes)
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 10 (setting :timeout-seconds))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" url (nle:transport-failure-label condition url))))
    (unless (< status 300)
      (fail "GET ~a: HTTP ~d" url status))
    (when truncated
      (fail "~a is larger than max_file_mb (~d MB)" url (setting :max-file-mb)))
    body))

(defun source-bytes (source)
  "SOURCE -- a path or an http(s) URL -- as (values OCTETS LABEL)."
  (if (url-p source)
      (values (fetch-octets source) source)
      (let ((path (file-path source)))
        (unless (and (probe-file path) (not (uiop:directory-pathname-p path)))
          (fail "no file at ~a" source))
        (let ((octets (read-octets path)))
          (when (> (length octets) (max-bytes))
            (fail "~a is larger than max_file_mb (~d MB)"
                  (file-namestring path) (setting :max-file-mb)))
          (values octets (file-namestring path))))))

(defun image-media-type (octets)
  "The media type OCTETS are, by their bytes; a format no model is shown refuses."
  (or (nle:image-media-type octets)
      (fail "not a PNG, JPEG, GIF or WebP image, by its bytes")))

;;; --- the inline path ------------------------------------------------------------

(defun turn-id ()
  (getf (nle:turn) :turn-id))

(defun attach-to-turn (octets media-type)
  "Stage OCTETS in the running turn, behind the tool call that asked for them,
so the model reads them with its next round and the conversation keeps them.
=> the call id they were anchored to, or NIL when no turn is running."
  (let ((turn (turn-id)))
    (when turn
      (let ((anchor (and (boundp 'nle::*executing-tool-call-id*)
                         nle::*executing-tool-call-id*)))
        (nodecode.evolved::attach-turn-images
         turn (list (cons media-type (base64-encode octets)))
         :anchor anchor)
        (or anchor t)))))

(defvar *attach* #'attach-to-turn
  "How an image reaches the model: ATTACH-TO-TURN in a turn, a recorder in tests.")

;;; --- the captioner path ---------------------------------------------------------

(defun decode-body (answer)
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "error" "message")
        (nlk:json-value body :string "message")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun caption-request-body (octets media-type question)
  (nlk:json-object
   "model" (caption-model)
   "messages" (vector (nlk:json-object
                        "role" "user"
                        "content" (vector
                                   (nlk:json-object
                                    "type" "image_url"
                                    "image_url" (nlk:json-object
                                                 "url" (format nil "data:~a;base64,~a"
                                                                media-type
                                                                (base64-encode octets))))
                                   (nlk:json-object "type" "text" "text" question))))
   "max_tokens" (setting :max-tokens)
   "temperature" 0
   "stream" nil))

(defun answer-text (body)
  (let* ((choices (nlk:json-value body :array "choices"))
         (choice (and choices (plusp (length choices)) (aref choices 0)))
         (message (nlk:json-value choice :object "message"))
         (content (and message (gethash "content" message))))
    (cond ((non-empty content) content)
          ((vectorp content)
           (let ((text (with-output-to-string (out)
                         (loop for part across content
                               do (let ((piece (nlk:json-value part :string "text")))
                                    (when piece (write-string piece out)))))))
             (and (plusp (length text)) text))))))

(defun caption-available-p ()
  (and (plusp (length (caption-base))) (plusp (length (caption-model)))))

(defun caption (octets media-type question)
  "OCTETS read by the configured captioner, its answer as text."
  (unless (caption-available-p)
    (fail "\"image_input_mode\" is ~a and this model reads text only, so the image needs the captioner: set \"api_base\" and \"model\" in the vision-inline section"
          (inline-mode)))
  (let ((answer nil) (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :post (endpoint)
                    :headers (cons '("content-type" . "application/json") (auth-headers))
                    :content (nlk:encode-json-object
                              (caption-request-body octets media-type question))
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 10 (setting :timeout-seconds))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" (endpoint) (nle:transport-failure-label condition (endpoint)))))
    (unless (< status 300)
      (fail "POST ~a: HTTP ~d~@[: ~a~]" (endpoint) status (body-error-text answer)))
    (or (answer-text (decode-body answer))
        (fail "~a answered HTTP ~d with no recognized text" (endpoint) status))))

;;; --- the tool -------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: vision-inline: ~a" condition)))))

(defun see-image (request)
  "See one image: attach it to the session model when it takes images, else
have the captioner read it. => the line the model reads."
  (answering
    (let* ((source (non-empty (gethash "image" request)))
           (question (or (non-empty (gethash "question" request))
                         +default-question+))
           (path (route)))
        (unless source
          (fail "image is required: a local path, or an http(s) URL"))
      (multiple-value-bind (octets label) (source-bytes source)
        (let ((media-type (image-media-type octets)))
          (ecase path
            (:inline
             (if (funcall *attach* octets media-type)
                 (format nil "~a: ~a, ~:d bytes, attached to this turn behind this result. The image reaches you with your next round, stays for the rest of the turn and is kept in the conversation, so read it yourself and answer from what you see; call see_image again only for another image."
                         label media-type (length octets))
                 ;; No turn to attach to (a script, a test): the captioner is the only reader left.
                 (format nil "~a: ~a, ~:d bytes. No turn is running to attach an image to, so the captioner read it:~%~%~a"
                         label media-type (length octets)
                         (caption octets media-type question))))
            (:caption
             (format nil "~a: ~a, ~:d bytes, read by the captioner ~a (this model takes no image).~%~%~a"
                     label media-type (length octets) (caption-model)
                     (caption octets media-type question)))))))))

(defparameter *see-image-schema*
  (list :description
        (format nil "Show yourself one image, from a file or an http(s) URL: PNG, JPEG, GIF or WebP. When this session's model takes image input the image itself is attached to the turn and kept in the conversation, so you read it directly and answer from it; when it takes text only, or the vision-inline section sets image_input_mode to text, a configured captioning model reads it and its description comes back. ~a"
                "Use it when the answer depends on what an image shows and nothing has shown it to you yet.")
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "image" (nlk:json-object
                                "type" "string"
                                "description" "The image to see: a local path, or an http(s) URL.")
                       "question" (nlk:json-object
                                   "type" "string"
                                   "description" "What to ask about the image when a captioner reads it. Ignored when the image itself is attached."))
         "required" (vector "image")))
  "The see_image tool.")

;;; --- the manual and /vision --------------------------------------------------------

(defparameter +primer+
  "Vision Inline puts an image in front of the session's own model instead of a captioner's prose,
whenever that model takes image input: the image stays in the conversation, follow-ups need no
second look, and the model reads the pixels itself. A model models.dev calls text-only, or
image_input_mode = text, sends the image to the OpenAI-compatible captioner the section names.
  see_image {image, question?}   a file path or an http(s) URL, seen by the model or the captioner
/vision shows which path this session takes and whether a captioner is configured; /vision PATH
sees one image at once. The local (look PATH) verb still shows a file to a vision model, and this
cell's see_image is the tool and the URL. Only the captioner path sends the image anywhere, and
only to api_base with its token (api_key, or api_key_env naming the variable); max_file_mb and
timeout_seconds bound one image, max_tokens bounds a caption."
  "What (help :vision-inline) answers while the cell runs.")

(defun capability-text ()
  (case (sees-images-p)
    ((t) "this session's model takes images")
    ((nil) "this session's model reads text only, by models.dev")
    (t "models.dev has no row for this session's model, so the image is tried first")))

(defun status-line ()
  (let ((path (route)))
    (format nil "Vision Inline: image_input_mode ~a; ~a.~%~
                 An image now goes to ~a.~%~
                 captioner ~a; max_file_mb ~d, timeout ~ds, max_tokens ~d.~%~
                 see_image takes a path or an http(s) URL; /vision PATH sees one at once."
            (inline-mode) (capability-text)
            (if (eq path :inline)
                "the session model inline"
                "the captioner (set image_input_mode = auto and use a model with image input to attach it instead)")
            (if (caption-available-p)
                (format nil "~a, model ~a, token ~a" (caption-base) (caption-model)
                        (if (caption-key) "set" "not set"))
                "not configured")
            (setting :max-file-mb) (setting :timeout-seconds) (setting :max-tokens))))

(defun run-command (args session-id)
  "One /vision invocation: PATH (or URL) sees it, nothing shows the routing."
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (if (plusp (length argument))
        (answering
          (let* ((path (route)))
            (multiple-value-bind (octets label) (source-bytes argument)
              (let ((media-type (image-media-type octets)))
                (ecase path
                  (:inline
                   (if (funcall *attach* octets media-type)
                       (format nil "~a: ~a, ~:d bytes, attached to this turn; it reaches the model with its next round."
                               label media-type (length octets))
                       (format nil "~a: ~a, ~:d bytes. No turn is running; the captioner read it.~%~%~a"
                               label media-type (length octets)
                               (caption octets media-type +default-question+))))
                  (:caption
                   (format nil "~a: ~a, ~:d bytes, read by the captioner.~%~%~a"
                           label media-type (length octets)
                           (caption octets media-type +default-question+))))))))
        (handler-case (status-line) (error (e) (format nil "vision-inline: ~a" e))))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell vision-inline
  (:section ("vision-inline")
    (:guide "nothing here is required while the model takes images; api_base and model name the captioner used when it does not; next step: /vision says which path this session takes")
    ("image_input_mode" :choice :options '("auto" "text") :default "auto"
     :doc "auto attaches the image to the session model when it takes images; text always sends it to the captioner")
    ("api_base" :string
     :doc "the OpenAI-compatible captioner's base URL, https://host/v1, HTTPS unless it is loopback or allow_insecure_http is on")
    ("model" :string
     :doc "the captioning model the endpoint serves, used only when the session model reads text only")
    ("api_key" :secret
     :doc "the token sent to the captioner as a bearer header")
    ("api_key_env" :env
     :doc "the environment variable holding that token, when it is not written here")
    ("max_tokens" :integer :default 1024 :min 64
     :doc "the most tokens one caption may take")
    ("max_file_mb" :integer :default 5 :min 1
     :doc "the largest image one call reads, from a file or a URL")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one fetch or one caption's ceiling")
    ("allow_insecure_http" :boolean :default nil
     :doc "let a captioner that is not loopback answer over plain HTTP"))
  (:help :vision-inline "vision-inline: see_image shows the session model an image itself when it takes images, else a captioner's reading" +primer+)
  (:command "vision" 'run-command
            :description "Show which path an image takes, or see one now"
            :argument-hint "PATH")
  (:tool "see_image" *see-image-schema* 'see-image)
  (:start (lambda ()
            (check-config)
            (nle:on-stop (lambda () nil)))))
