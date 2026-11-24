;;;; cell.lisp --- document OCR: two tools, /document-ocr, and the wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of lEWFkRAD's hermes-document-reader (see NOTICE). The
;;;; original is a Windows profile service that watches a folder, renders
;;;; each page and asks a user-configured OpenAI-compatible multimodal
;;;; endpoint to transcribe it; a reader window and a .xlsx export sit on
;;;; top. A cell bundles no Python service and no window, so here the same
;;;; wire is called by the model or the operator:
;;;;
;;;;   document_ocr        one file -- a page image, or a PDF rendered page
;;;;                        by page with the pdftoppm the user has -- read
;;;;                        into Markdown by the configured endpoint;
;;;;                        optionally written beside it as .md/.txt.
;;;;   document_ocr_probe  one authenticated GET of the endpoint's /models,
;;;;                        the original's probe (engine/grm_ocr.py:692).
;;;;   /document-ocr       the endpoint in one line, or PATH read at once.
;;;;
;;;; The page image is the only thing that leaves the machine, and it goes
;;;; only to the endpoint the section names, with that endpoint's token.

(defpackage #:nodecode-document-ocr
  (:use #:cl)
  (:export #:document-ocr-error #:document-ocr-error-detail))

(in-package #:nodecode-document-ocr)

(nlk:define-peripheral document-ocr :not-running t)

(defparameter +default-prompt+
  "Transcribe this document page as GitHub-flavored Markdown. Preserve the reading order, headings, lists and every number, label and field; render tables as Markdown tables. Keep formulas inline as $...$ and display formulas as $$...$$. Do not summarise, translate or comment: answer with the transcription only."
  "The instruction sent with every page, as chandra's ocr_layout prompt is in
the original; the section's `prompt' replaces it.")

(defparameter +alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The base64 alphabet, RFC 4648.")

(defparameter +image-types+
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("webp" . "image/webp") ("gif" . "image/gif") ("bmp" . "image/bmp")
    ("tif" . "image/tiff") ("tiff" . "image/tiff"))
  "The page image formats the endpoint is offered, by extension.")

;;; --- the section, read back --------------------------------------------------

(defun non-empty (text)
  (and (stringp text) (plusp (length (string-trim '(#\Space #\Tab) text)))
       (string-trim '(#\Space #\Tab) text)))

(defun api-base ()
  "The endpoint's base URL, without a trailing slash."
  (let ((given (setting :api-base)))
    (if (non-empty given) (string-right-trim "/" (non-empty given)) "")))

(defun model ()
  (or (non-empty (setting :model)) ""))

(defun api-key ()
  "The token the section names, inline or by environment variable."
  (or (non-empty (setting :api-key))
      (let ((variable (non-empty (setting :api-key-env))))
        (and variable (non-empty (uiop:getenv variable))))))

(defun prompt ()
  (or (non-empty (setting :prompt)) +default-prompt+))

(defun url-scheme (url)
  (let ((end (search "://" url)))
    (and end (string-downcase (subseq url 0 end)))))

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
  "Refuse a section that could never reach a usable endpoint, naming the member."
  (when (zerop (length (api-base)))
    (error 'nlk:config-refusal
           :detail "document-ocr: \"api_base\" is required: the OpenAI-compatible endpoint's base URL, such as https://ocr.example.com/v1"))
  (when (zerop (length (model)))
    (error 'nlk:config-refusal
           :detail "document-ocr: \"model\" is required: the multimodal OCR model identifier the endpoint serves"))
  (unless (or (equal "https" (url-scheme (api-base)))
              (loopback-url-p (api-base))
              (setting :allow-insecure-http))
    (error 'nlk:config-refusal
           :detail "document-ocr: \"api_base\" is not HTTPS: pass \"allow_insecure_http\": true for an endpoint you trust, or use https"))
  (unless (or (api-key) (loopback-url-p (api-base)))
    (error 'nlk:config-refusal
           :detail "document-ocr: a token is required: set \"api_key\", or \"api_key_env\" naming the environment variable that holds it (a loopback endpoint may answer without one)")))

(defun endpoint ()
  (concatenate 'string (api-base) "/chat/completions"))

(defun auth-headers ()
  (let ((key (api-key)))
    (if key (list (cons "authorization" (format nil "Bearer ~a" key))) '())))

;;; --- files and the wire -------------------------------------------------------

(defun expand-home (text)
  (cond ((equal text "~") (or (uiop:getenv "HOME") text))
        ((and (> (length text) 1) (char= (char text 0) #\~)
              (member (char text 1) '(#\/ #\\) :test #'char=))
         (concatenate 'string (or (uiop:getenv "HOME") "") (subseq text 1)))
        (t text)))

(defun file-path (text)
  "TEXT as a pathname, with a leading ~ expanded."
  (uiop:parse-native-namestring (expand-home text)))

(defun base64-char (value)
  (char +alphabet+ value))

(defun base64-encode (octets)
  "OCTETS -- a vector of (unsigned-byte 8) -- as base64, padded."
  (let ((out (make-string-output-stream))
        (length (length octets)))
    (loop for index from 0 below length by 3
          do (let* ((remaining (- length index))
                    (first (aref octets index))
                    (second (if (> remaining 1) (aref octets (1+ index)) 0))
                    (third (if (> remaining 2) (aref octets (+ index 2)) 0))
                    (triple (+ (ash first 16) (ash second 8) third)))
               (write-char (base64-char (ldb (byte 6 18) triple)) out)
               (write-char (base64-char (ldb (byte 6 12) triple)) out)
               (write-char (if (> remaining 1)
                               (base64-char (ldb (byte 6 6) triple))
                               #\=)
                           out)
               (write-char (if (> remaining 2)
                               (base64-char (ldb (byte 6 0) triple))
                               #\=)
                           out)))
    (get-output-stream-string out)))

(defun read-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence buffer in)
      buffer)))

(defun media-type (path)
  (cdr (assoc (string-downcase (or (pathname-type path) "")) +image-types+
              :test #'equal)))

(defun pdf-path-p (path)
  (equal "pdf" (string-downcase (or (pathname-type path) ""))))

(defun file-size (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (file-length in)))

(defun page-octets (path)
  "PATH's bytes as (MEDIA-TYPE . OCTETS), refused when it is not a page image
or is larger than the section's max_file_mb."
  (let ((type (media-type path)))
    (unless type
      (fail "~a is not a page image: ~{~a~^, ~}"
            (file-namestring path) (mapcar #'car +image-types+)))
    (let ((limit (* 1024 1024 (setting :max-file-mb))))
      (when (> (file-size path) limit)
        (fail "~a is larger than the section's ~d MB limit"
              (file-namestring path) (setting :max-file-mb))))
    (cons type (read-octets path))))

(defun page-number (path)
  "PATH's last run of digits -- the page a rendered file belongs to."
  (let ((current "") (last ""))
    (loop for character across (file-namestring path)
          do (if (digit-char-p character)
                 (setf current (concatenate 'string current (string character)))
                 (progn (when (plusp (length current)) (setf last current))
                        (setf current ""))))
    (or (ignore-errors (parse-integer (if (plusp (length current)) current last))) 0)))

(defun pdftoppm-available-p ()
  "Whether poppler's pdftoppm can be run on this machine."
  (handler-case
      (progn (uiop:run-program '("pdftoppm" "-v")
                                :output :string :error-output :string
                                :ignore-error-status t)
             t)
    (error () nil)))

(defun render-pdf-pages (path)
  "PATH's pages as PNG files in a fresh temporary directory, rendered by the
pdftoppm the user has at the section's render_dpi. A machine without it
refuses, naming it."
  (unless (pdftoppm-available-p)
    (fail "~a is a PDF and pdftoppm (poppler) is not on PATH: install poppler, or pass one page image"
          (file-namestring path)))
  (let* ((directory (merge-pathnames
                     (format nil "nodecode-document-ocr-~d/" (get-universal-time))
                     (uiop:temporary-directory)))
         (stem (merge-pathnames "page" directory)))
    (ensure-directories-exist stem)
    (handler-case
        (uiop:run-program (list "pdftoppm"
                                "-r" (princ-to-string (setting :render-dpi))
                                "-png" (namestring path) (namestring stem))
                          :output :string :error-output :string)
      (error (condition)
        (fail "pdftoppm could not render ~a: ~a" (file-namestring path) condition)))
    (sort (directory (merge-pathnames "*.png" directory)) #'< :key #'page-number)))

(defun page-files (path)
  "PATH's pages as the page images to send: the file itself, or a PDF's
rendered pages, at most the section's max_pages."
  (when (zerop (file-size path))
    (fail "~a is empty" (file-namestring path)))
  (let ((pages (if (pdf-path-p path) (render-pdf-pages path) (list path))))
    (when (null pages)
      (fail "~a rendered no pages" (file-namestring path)))
    (when (> (length pages) (setting :max-pages))
      (fail "~a has ~d pages, more than the section's max_pages of ~d"
            (file-namestring path) (length pages) (setting :max-pages)))
    pages))

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- decoded, an empty body as an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "error" "message")
        (nlk:json-value body :string "message")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun request-body (pages prompt)
  "The chat-completions body: every page image, then the instruction."
  (let ((content (append (loop for (media . octets) in pages
                               collect (nlk:json-object
                                        "type" "image_url"
                                        "image_url"
                                        (nlk:json-object
                                         "url" (format nil "data:~a;base64,~a"
                                                        media (base64-encode octets)))))
                         (list (nlk:json-object "type" "text" "text" prompt)))))
    (nlk:json-object
     "model" (model)
     "messages" (vector (nlk:json-object "role" "user"
                                          "content" (coerce content 'vector)))
     "max_tokens" (setting :max-tokens)
     "temperature" 0
     "stream" nil
     "chat_template_kwargs" (nlk:json-object "enable_thinking" nil))))

(defun answer-text (body)
  "BODY's first choice's message text, a string or an array of parts."
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

(defun post-page (page prompt)
  "PAGE -- (MEDIA-TYPE . OCTETS) -- read by the endpoint, as text."
  (let ((answer nil)
        (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :post (endpoint)
                    :headers (cons '("content-type" . "application/json") (auth-headers))
                    :content (nlk:encode-json-object (request-body (list page) prompt))
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 10 (setting :timeout-seconds))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" (endpoint) (nle:transport-failure-label condition (endpoint)))))
    (unless (< status 300)
      (fail "POST ~a: HTTP ~d~@[: ~a~]" (endpoint) status (body-error-text answer)))
    (or (answer-text (decode-body answer))
        (fail "~a answered HTTP ~d with no recognized text" (endpoint) status))))

(defun page-label (path page index total)
  (if (equal path page)
      ""
      (format nil "[page ~d of ~d]~%~%" index total)))

(defun ocr-file (path &key prompt out)
  "PATH -- a page image or a PDF -- read page by page into one Markdown text.
PROMPT replaces the section's instruction. OUT, when given, is a file the
text is written to. => (values TEXT PAGES)."
  (let* ((pages (page-files path))
         (texts (loop for page in pages
                       for index from 1
                       collect (concatenate 'string
                                            (page-label path page index (length pages))
                                            (post-page (page-octets page) (or prompt (prompt))))))
         (text (format nil "~{~a~^~%~%~}" texts))
         (out-path (and (non-empty out) (file-path out))))
    (when out-path
      (ensure-directories-exist out-path)
      (with-open-file (stream out-path :direction :output :if-exists :supersede
                                       :if-does-not-exist :create
                                       :external-format :utf-8)
        (write-string text stream)))
    (values text (length pages))))

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: document-ocr: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (non-empty value)
        (string-trim '(#\Space #\Tab) value)
        (fail "~a is required" name))))

(defun optional-text (args name)
  (let ((value (gethash name args)))
    (and (non-empty value) (string-trim '(#\Space #\Tab) value))))

(defun tool-ocr (args)
  (answering
    (let* ((path (file-path (argument-text args "path")))
           (out (optional-text args "out"))
           (prompt (optional-text args "prompt")))
      (unless (probe-file path)
        (fail "no file at ~a" (argument-text args "path")))
      (multiple-value-bind (text pages) (ocr-file path :prompt prompt :out out)
        (format nil "~a: ~d page~:p, ~d characters~%~%~a~@[~%~%written to ~a~]"
                (file-namestring path) pages (length text) text
                (and out (namestring (file-path out))))))))

(defun tool-probe (args)
  (declare (ignore args))
  (answering
    (let ((answer nil)
          (status nil))
      (handler-case
          (multiple-value-setq (answer status)
            (nlk:http :get (concatenate 'string (api-base) "/models")
                      :headers (auth-headers)
                      :timeout (min 15 (setting :timeout-seconds))
                      :connect-timeout (min 5 (setting :timeout-seconds))))
        ((or error sb-sys:deadline-timeout) (condition)
          (fail "~a: ~a" (api-base) (nle:transport-failure-label condition (api-base)))))
      (unless (< status 300)
        (fail "GET ~a/models: HTTP ~d~@[: ~a~]" (api-base) status (body-error-text answer)))
      (let* ((body (decode-body answer))
             (models (loop for entry across (or (nlk:json-value body :array "data") #())
                           for id = (nlk:json-value entry :string "id")
                           when id collect id)))
        (format nil "~a answers (HTTP ~d)~@[: models: ~{~a~^, ~}~]"
                (api-base) status models)))))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type
                                                                     "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *ocr-schema*
  (tool-schema
   "Read a document page into Markdown with the user's own OCR endpoint: pass a page image (PNG, JPEG, WebP, GIF, BMP or TIFF) or a PDF, which is rendered page by page when pdftoppm is installed. Use it when the answer depends on what a document says and no text was supplied; the page image is sent to the endpoint named in the document-ocr section, so say so when the document is sensitive."
   '(("path" "string" "The file to read: a page image, or a PDF." t)
     ("prompt" "string" "What to ask of each page instead of the built-in transcription instruction." nil)
     ("out" "string" "A file to write the recognized Markdown to, such as report.md beside the source." nil)))
  "The document_ocr tool.")

(defparameter *probe-schema*
  (tool-schema
   "Check that the configured OCR endpoint answers with the configured token, and list the models it serves. Use it when a document_ocr call failed and the cause may be the endpoint, the token or the model name."
   '())
  "The document_ocr_probe tool.")

;;; --- the manual and /document-ocr ------------------------------------------------

(defparameter +primer+
  "Document OCR reads a page image -- or a PDF, through the pdftoppm of poppler -- into Markdown with
the OpenAI-compatible multimodal endpoint the document-ocr section names. Only the page image
leaves this machine, and only to that endpoint, with that endpoint's token.
  document_ocr {path, prompt?, out?}   one file read into Markdown, optionally written out
  document_ocr_probe {}                 the endpoint's /models, to check the address and token
/document-ocr shows the endpoint in one line, or reads a file at once. The section names
api_base (https://host/v1), model, and the token inline (api_key) or by environment variable
(api_key_env); max_tokens, timeout_seconds, max_file_mb, max_pages and render_dpi bound one
call, prompt replaces the built-in instruction, and allow_insecure_http lets a non-loopback
endpoint answer over plain HTTP. A loopback endpoint may answer without a token."
  "What (help :document-ocr) answers while the cell runs.")

(defun status-line ()
  (format nil "Document OCR: ~a, model ~a, token ~a.~%~
               max_tokens ~d, timeout ~ds, ~d MB per file, at most ~d page~:p per call.~%~
               /document-ocr PATH reads one file; the tools document_ocr and document_ocr_probe do the rest."
          (api-base)
          (let ((name (model))) (if (plusp (length name)) name "(unset)"))
          (if (api-key) "set" "not set")
          (setting :max-tokens) (setting :timeout-seconds)
          (setting :max-file-mb) (setting :max-pages)))

(defun run-command (args session-id)
  "One /document-ocr invocation: PATH reads it, no argument the endpoint."
  (declare (ignore session-id))
  (let ((argument (nlk:trimmed (or args ""))))
    (if (plusp (length argument))
        (handler-case
            (let* ((path (file-path argument)))
              (unless (probe-file path)
                (return-from run-command (format nil "No file at ~a." argument)))
              (multiple-value-bind (text pages) (ocr-file path)
                (format nil "~a: ~d page~:p, ~d characters~%~%~a"
                        (file-namestring path) pages (length text) text)))
          ((or error sb-sys:deadline-timeout) (condition)
            (format nil "document-ocr: ~a" condition)))
        (status-line))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell document-ocr
  (:section ("document-ocr")
    (:guide "api_base is the OpenAI-compatible endpoint's base URL (https://host/v1) and model the multimodal model it serves; a token comes from api_key or api_key_env (a loopback endpoint may answer without one); next step: /document-ocr shows the endpoint")
    ("api_base" :string
     :doc "the OCR endpoint's base URL, HTTPS unless it is loopback or allow_insecure_http is on")
    ("model" :string
     :doc "the multimodal model identifier the endpoint serves, such as a page-layout OCR model")
    ("api_key" :secret
     :doc "the token sent to that endpoint as a bearer header")
    ("api_key_env" :env
     :doc "the environment variable holding the token, when it is not written here")
    ("max_tokens" :integer :default 8192 :min 1
     :doc "the most tokens one page's transcription may take")
    ("timeout_seconds" :integer :default 120 :min 1
     :doc "one page's ceiling")
    ("max_file_mb" :integer :default 25 :min 1
     :doc "the largest page image one call sends")
    ("max_pages" :integer :default 10 :min 1
     :doc "the most PDF pages one call reads")
    ("render_dpi" :integer :default 150 :min 50
     :doc "the resolution pdftoppm renders a PDF page at")
    ("allow_insecure_http" :boolean :default nil
     :doc "let a non-loopback endpoint answer over plain HTTP")
    ("prompt" :string
     :doc "the instruction sent with every page; empty uses the built-in transcription prompt"))
  (:help :document-ocr "document-ocr: document_ocr reads a page image or PDF into Markdown through your own OCR endpoint" +primer+)
  (:command "document-ocr" 'run-command
            :description "Show the OCR endpoint, or read a file through it"
            :argument-hint "PATH")
  (:tool "document_ocr" *ocr-schema* 'tool-ocr)
  (:tool "document_ocr_probe" *probe-schema* 'tool-probe)
  (:start (lambda ()
            (check-config)
            (nle:on-stop (lambda () nil)))))
