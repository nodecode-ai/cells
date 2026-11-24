;;;; cell-test.lisp --- lifecycle, both tools, /document-ocr, the wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: a healthy start, a
;;;; section the declaration refuses, a start that could name no endpoint,
;;;; a token or a model refused with the member named, base64 against the
;;;; RFC 4648 vectors, one page going to the configured endpoint with its
;;;; bearer token and its data URL, a PDF read page by page, a .md written
;;;; beside the source, the endpoint's /models probed, and /document-ocr
;;;; with and without an argument. Every request is a stubbed NLK:HTTP, so
;;;; nothing reaches an endpoint or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-ocr-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials an OCR endpoint."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected OCR request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defmacro is-carrying-form (form &body checks)
  "Assert that FORM's text carries every CHECK, as IS-CARRYING does. A check
that lists more than a needle and its description is read as one check per
needle."
  `(let ((answer ,form))
     (is-carrying answer
       ,@(loop for check in checks
               if (and (consp check) (stringp (first check)) (> (length check) 2))
                 append (loop for needle in check collect (list needle))
               else collect check))))

(defun ocr-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun ocr-request-body (request)
  (nlk:decode-json (getf (cddr request) :content)))

(defun ocr-request-header (request name)
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun ocr-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun ocr-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

(defun ocr-scratch-directory ()
  (let ((directory (merge-pathnames (format nil "nodecode-document-ocr-test-~d/"
                                            (random 1000000))
                                    (uiop:temporary-directory))))
    (ensure-directories-exist (merge-pathnames "keep" directory))
    directory))

(defun ocr-scratch-file (name text &optional (directory (ocr-scratch-directory)))
  "TEXT written as bytes at NAME inside DIRECTORY => the pathname."
  (let ((path (merge-pathnames name directory)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create
                              :element-type '(unsigned-byte 8))
      (loop for character across text
            do (write-byte (char-code character) out)))
    path))

(defun ocr-bytes (text)
  (map '(vector (unsigned-byte 8)) #'char-code text))

(defun ocr-message-content (body)
  "BODY's first message content, the array of parts."
  (let* ((messages (nlk:json-value body :array "messages"))
         (message (and messages (aref messages 0))))
    (nlk:json-value message :array "content")))

;;; --- lifecycle ------------------------------------------------------------------

(define-test-slice "document-ocr" "DOCUMENT-OCR-CELL-" :start nodecode-document-ocr:start-cell)

(define-cell-lifecycle-tests "document-ocr"
  (:fixture with-ocr-wire)
  (:config "api_base" "https://ocr.test/v1" "model" "ocr-1" "api_key" "k-test")
  (:help :document-ocr)
  (:command "document-ocr")
  (:running (is (nle::find-tool "document_ocr") "document_ocr is registered")
            (is (nle::find-tool "document_ocr_probe"))
            (is (equal "https://ocr.test/v1" (nodecode-document-ocr::api-base)))
            (is (equal "ocr-1" (nodecode-document-ocr::model)))
            (is (= 8192 (nodecode-document-ocr::setting :max-tokens)) "the default budget")
            (is (equal "Bearer k-test"
                       (cdr (assoc "authorization" (nodecode-document-ocr::auth-headers)
                                   :test #'equal)))))
  (:stopped (is (null (nle::find-tool "document_ocr")) "stop unregisters the tools")
            (is (null (nle::find-tool "document_ocr_probe"))))
  (:refused ("max_tokens" 0) ("timeout_seconds" 0) ("max_pages" 0) ("render_dpi" 10))
  (:idle nodecode-document-ocr:document-ocr-error (nodecode-document-ocr::run-command "" nil)))

(deftest document-ocr-cell-needs-an-endpoint-a-model-and-a-token ()
  (with-ocr-wire
    (is-carrying-form (refusal-text nlk:config-refusal (document-ocr-start "model" "ocr-1"))
                 ("api_base" "the endpoint is named"))
    (is-carrying-form (refusal-text nlk:config-refusal (document-ocr-start "api_base" "https://ocr.test/v1"))
                 ("model" "the model is named"))
    (is-carrying-form (refusal-text nlk:config-refusal
                               (document-ocr-start "api_base" "https://ocr.test/v1" "model" "ocr-1"))
                 ("token is required" "api_key_env"))
    (is-carrying-form (refusal-text nlk:config-refusal
                               (document-ocr-start "api_base" "http://ocr.example.com/v1"
                                                   "model" "ocr-1" "api_key" "k"))
                 ("not HTTPS" "allow_insecure_http"))
    (is (null (nle::find-tool "document_ocr")) "a refused start installed nothing")
    (with-cell-stop ((document-ocr-start "api_base" "http://127.0.0.1:8080/v1" "model" "ocr-1"))
      (is (equal "http://127.0.0.1:8080/v1" (nodecode-document-ocr::api-base)) "a loopback endpoint may answer without a token")
      (is (null (nodecode-document-ocr::auth-headers)) "and no header is sent without one"))))

(deftest document-ocr-cell-a-token-can-come-from-the-environment ()
  (with-ocr-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                                (if (equal name "OCR_TOKEN") "k-env" (funcall original name)))
      (with-cell-stop ((document-ocr-start "api_base" "https://ocr.test/v1" "model" "ocr-1"
                                            "api_key_env" "OCR_TOKEN"))
        (is (equal "Bearer k-env"
                   (cdr (assoc "authorization" (nodecode-document-ocr::auth-headers)
                               :test #'equal))))))))

;;; --- base64 and files -------------------------------------------------------------

(deftest document-ocr-cell-base64-encodes-the-rfc-vectors ()
  (is (equal "" (nodecode-document-ocr::base64-encode #())))
  (is (equal "TWFu" (nodecode-document-ocr::base64-encode (ocr-bytes "Man"))))
  (is (equal "TWE=" (nodecode-document-ocr::base64-encode (ocr-bytes "Ma"))))
  (is (equal "TQ==" (nodecode-document-ocr::base64-encode (ocr-bytes "M"))))
  (is (equal "aGVsbG8="
             (nodecode-document-ocr::base64-encode
              (nodecode-document-ocr::read-octets (ocr-scratch-file "page.png" "hello"))))))

(deftest document-ocr-cell-a-file-that-is-not-a-page-image-is-a-repair ()
  (with-ocr-wire
    (with-cell-stop ((document-ocr-start "api_base" "https://ocr.test/v1" "model" "ocr-1" "api_key" "k"))
      (let ((path (ocr-scratch-file "notes.txt" "plain text")))
        (is-carrying-form (ocr-tool "document_ocr" (nlk:json-object "path" (namestring path)))
                     ("ERROR: document-ocr:" "is not a page image" "png"))
        (is (null (ocr-asked)) "nothing was sent"))
      (is-carrying-form (ocr-tool "document_ocr" (nlk:json-object "path" "/nonexistent/page.png"))
                   ("no file at")))))

;;; --- the wire ---------------------------------------------------------------------

(deftest document-ocr-cell-a-page-goes-to-the-configured-endpoint ()
  (with-ocr-wire
    (with-cell-stop ((document-ocr-start "api_base" "https://ocr.test/v1" "model" "ocr-1"
                                          "api_key" "k-test" "max_tokens" 4096))
      (ocr-answer "{\"choices\":[{\"message\":{\"content\":\"# Invoice\\n\\nTotal 42\"}}]}")
      (let* ((directory (ocr-scratch-directory))
             (path (ocr-scratch-file "page.png" "hello" directory))
             (out (merge-pathnames "page.md" directory))
             (answer (ocr-tool "document_ocr"
                                (nlk:json-object "path" (namestring path)
                                                 "out" (namestring out)))))
        (is-carrying answer ("# Invoice") ("written to") ("page.md"))
        (is (equal "# Invoice

Total 42" (uiop:read-file-string out)) "the text is written where out names")
        (let ((request (first (ocr-asked))))
          (is (= 1 (length (ocr-asked))) "one request for one page")
          (is (eq :post (first request)))
          (is (equal "https://ocr.test/v1/chat/completions" (second request)))
          (is (equal "Bearer k-test" (ocr-request-header request "authorization")))
          (let* ((body (ocr-request-body request))
                 (content (ocr-message-content body)))
            (is (equal "ocr-1" (nlk:json-value body :string "model")))
            (is (= 4096 (nlk:json-value body :integer "max_tokens")) "the section's budget")
            (is (= 2 (length content)) "the image and the instruction")
            (is (equal "image_url" (nlk:json-value (aref content 0) :string "type")))
            (is (equal "data:image/png;base64,aGVsbG8="
                       (nlk:json-value (aref content 0) :string "image_url" "url")))
            (is (equal "text" (nlk:json-value (aref content 1) :string "type")))
            (is (search "Transcribe this document page as GitHub-flavored Markdown"
                        (nlk:json-value (aref content 1) :string "text")))
            (multiple-value-bind (thinking present)
                (gethash "enable_thinking" (nlk:json-value body :object "chat_template_kwargs"))
              (declare (ignore thinking))
              (is present "thinking is disabled, as the original asks"))))))))

(deftest document-ocr-cell-the-prompt-can-be-replaced ()
  (with-ocr-wire
    (with-cell-stop ((document-ocr-start "api_base" "https://ocr.test/v1" "model" "ocr-1"
                                          "api_key" "k" "prompt" "Only the tables."))
      (ocr-answer "{\"choices\":[{\"message\":{\"content\":\"| a |\"}}]}")
      (ocr-tool "document_ocr" (nlk:json-object "path" (namestring (ocr-scratch-file "p.png" "x"))))
      (let* ((content (ocr-message-content (ocr-request-body (first (ocr-asked))))))
        (is (equal "Only the tables." (nlk:json-value (aref content 1) :string "text")))))))

(deftest document-ocr-cell-a-failed-answer-is-a-repair-not-a-crash ()
  (with-ocr-wire
    (with-cell-stop ((document-ocr-start "api_base" "https://ocr.test/v1" "model" "ocr-1" "api_key" "k"))
      (let ((path (namestring (ocr-scratch-file "page.png" "hello"))))
        (ocr-answer "{\"error\":{\"message\":\"Invalid token\"}}" 401)
        (is-carrying-form (ocr-tool "document_ocr" (nlk:json-object "path" path))
                     ("ERROR: document-ocr:" "HTTP 401" "Invalid token"))
        (ocr-answer "{\"choices\":[]}")
        (is-carrying-form (ocr-tool "document_ocr" (nlk:json-object "path" path))
                     ("no recognized text"))))))

(deftest document-ocr-cell-a-pdf-is-read-page-by-page ()
  (with-ocr-wire
    (with-cell-stop ((document-ocr-start "api_base" "https://ocr.test/v1" "model" "ocr-1" "api_key" "k"))
      (with-stubbed-fdefinition (nodecode-document-ocr::render-pdf-pages (path)
                                (let ((directory (ocr-scratch-directory)))
                                  (list (ocr-scratch-file "page-1.png" "one" directory)
                                        (ocr-scratch-file "page-2.png" "two" directory))))
        (let ((answers '("{\"choices\":[{\"message\":{\"content\":\"first page\"}}]}"
                         "{\"choices\":[{\"message\":{\"content\":\"second page\"}}]}")))
          (setf *answer-fn* (lambda (method url args)
                              (declare (ignore method url args))
                              (values (pop answers) 200))))
        (let* ((path (ocr-scratch-file "scan.pdf" "%PDF-1.4"))
               (answer (ocr-tool "document_ocr" (nlk:json-object "path" (namestring path)))))
          (is (search "[page 1 of 2]" answer))
          (is (search "first page" answer))
          (is (search "[page 2 of 2]" answer))
          (is (search "second page" answer))
          (is (= 2 (length (ocr-asked))) "one request per page"))))))

(deftest document-ocr-cell-too-many-pages-is-a-repair ()
  (with-ocr-wire
    (with-cell-stop ((document-ocr-start "api_base" "https://ocr.test/v1" "model" "ocr-1"
                                          "api_key" "k" "max_pages" 1))
      (with-stubbed-fdefinition (nodecode-document-ocr::render-pdf-pages (path)
                                (let ((directory (ocr-scratch-directory)))
                                  (list (ocr-scratch-file "page-1.png" "one" directory)
                                        (ocr-scratch-file "page-2.png" "two" directory))))
        (is-carrying-form (ocr-tool "document_ocr"
                                (nlk:json-object "path"
                                                 (namestring (ocr-scratch-file "scan.pdf" "%PDF"))))
                     ("2 pages" "max_pages of 1"))
        (is (null (ocr-asked)) "nothing was sent")))))

(deftest document-ocr-cell-the-probe-reads-the-models ()
  (with-ocr-wire
    (with-cell-stop ((document-ocr-start "api_base" "https://ocr.test/v1" "model" "ocr-1" "api_key" "k"))
      (ocr-answer "{\"data\":[{\"id\":\"ocr-large\"},{\"id\":\"ocr-small\"}]}")
      (is-carrying-form (ocr-tool "document_ocr_probe" (nlk:json-object))
                   ("https://ocr.test/v1 answers (HTTP 200)" "ocr-large, ocr-small"))
      (let ((request (first (ocr-asked))))
        (is (eq :get (first request)))
        (is (equal "https://ocr.test/v1/models" (second request)))
        (is (equal "Bearer k" (ocr-request-header request "authorization")))))))

;;; --- /document-ocr -----------------------------------------------------------------

(deftest document-ocr-cell-the-command-shows-the-endpoint-or-reads-a-file ()
  (with-ocr-wire
    (with-cell-stop ((document-ocr-start "api_base" "https://ocr.test/v1" "model" "ocr-1" "api_key" "k"))
      (let ((answer (cell-entry "nodecode-document-ocr" "document-ocr" "")))
        (is (search "Document OCR: https://ocr.test/v1, model ocr-1, token set" answer))
        (is (search "/document-ocr PATH" answer))
        (is (null (ocr-asked)) "a status read asks the endpoint nothing"))
      (ocr-answer "{\"choices\":[{\"message\":{\"content\":\"two columns\"}}]}")
      (let ((answer (cell-entry "nodecode-document-ocr" "document-ocr"
                                (namestring (ocr-scratch-file "page.png" "hello")))))
        (is (search "two columns" answer))
        (is (= 1 (length (ocr-asked))))))))
