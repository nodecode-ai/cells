;;;; cell-test.lisp --- lifecycle, the eight operations, the two boundaries, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Markdown.new and
;;;; no network is needed. What is proved is the cell's own wiring: the section
;;;; (a healthy start, a blank address and out-of-range members refused), the
;;;; tool through the seam the model calls it by -- its operation table, its
;;;; endpoints, its bodies and its answers -- the two boundaries (a private or
;;;; credentialed url, a secret store or an oversized upload) refusing before
;;;; any request, and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures ---------------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-markdown-new-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, and no wait is ever real."
  `(let ((*requests* '())
         (nodecode-markdown-new::*retry-delay* 0)
         (nodecode-markdown-new::*sleep* (lambda (seconds) (declare (ignore seconds)) nil))
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected markdown.new request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun mdnew-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun mdnew-reply (text &optional (status 200))
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun mdnew-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun mdnew-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun mdnew-call (arguments)
  "markdown_new called the way the model calls it, as the text it reads back."
  (nle::message-content (execute-wire-call "mdnew-call" "markdown_new" arguments)))

(defun mdnew-invoke (arguments)
  "The registered tool's own function, for the refusals the harness would catch."
  (let ((entry (nle::find-tool "markdown_new")))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (nlk:decode-json arguments)))))

(defun mdnew-refusal (arguments)
  "The text of the refusal ARGUMENTS earn."
  (refusal-text nodecode-markdown-new:markdown-new-error (mdnew-invoke arguments)))

;;; --- lifecycle --------------------------------------------------------------------

(define-test-slice "markdown-new" "MARKDOWN-NEW-CELL-" :start nodecode-markdown-new:start-cell)

(define-cell-lifecycle-tests "markdown-new"
  (:fixture with-markdown-new-wire)
  (:help :markdown-new)
  (:command "markdown-new")
  (:running
   (is-present (entry (nle::find-tool "markdown_new"))
     "markdown_new is registered"
     (is (hash-table-p (nlk:contract-schema (second entry))) "against a declared contract")
     (is (search "url_convert" (nlk:encode-json-object (nlk:contract-schema (second entry)))) "whose schema advertises the operations"))
   (is (= 1 (count-if (lambda (item) (equal "markdown_new" (first item))) nle::*tools*)) "one tool, as the plugin had one")
   (is (equal "https://markdown.new" (nodecode-markdown-new::root)) "the public service by default")
   (is (= 20000 (nodecode-markdown-new::setting :max-chars)) "and the section's own bounds"))
  (:stopped
   (is (null (nle::find-tool "markdown_new")) "and stop takes the tool back"))
  (:refused ("base_url" "markdown.new")
            ("timeout_seconds" 0)
            ("poll_interval_seconds" 0)
            ("max_chars" 10)
            ("max_upload_bytes" 10)))

(deftest markdown-new-cell-an-unusable-service-address-refuses-naming-it (with-markdown-new-wire)
  (let ((text (refusal-text nlk:config-refusal (markdown-new-start "base_url" "markdown.new"))))
    (is (search "base_url" text) "the refusal names the member")
    (is (search "http or https" text) "and the shape it must have")
    (is (search "https://markdown.new" text) "and the address that would work")
    (is (null (nle::find-tool "markdown_new")) "and nothing was installed")))

;;; --- url_convert -----------------------------------------------------------------

(deftest markdown-new-cell-a-url-becomes-markdown (with-markdown-new-wire)
  (with-cell-stop ((markdown-new-start))
    (mdnew-reply "{\"success\": true, \"title\": \"A Page\", \"content\": \"# A Page\\n\\nthe body\"}")
    (let ((answer (mdnew-call "{\"operation\": \"url_convert\", \"url\": \"https://example.org/a\"}")))
      (let ((request (first (mdnew-asked))))
        (is (eq :post (first request)) "one POST")
        (is (equal "https://markdown.new/" (second request)) "to the service's conversion endpoint")
        (is (equal "https://example.org/a" (nlk:json-value (mdnew-request-body request) :text "url")) "carrying the url")
        (is (equal "markdown-new/0.1.0 (nodecode-markdown-new)" (mdnew-request-header request "user-agent")) "under the plugin's own user agent"))
      (is (search "# A Page" answer) "the page's Markdown is the answer")
      (is (search "title: A Page" answer) "with its title")
      (is (search "https://example.org/a" answer) "and the address it was read from"))))

(deftest markdown-new-cell-a-long-page-is-cut-and-says-how-much-is-left (with-markdown-new-wire)
  (with-cell-stop ((markdown-new-start "max_chars" 200))
    (mdnew-reply (format nil "{\"content\": \"~a\"}" (make-string 260 :initial-element #\x)))
    (let ((answer (mdnew-call "{\"operation\": \"url_convert\", \"url\": \"https://example.org/big\"}")))
      (is (search "60 more characters" answer) "the cut is said out loud")
      (is (search "max_chars" answer) "with the way to read the rest"))))

;;; --- search_extract --------------------------------------------------------------

(deftest markdown-new-cell-a-search-posts-the-query-and-reads-the-sources (with-markdown-new-wire)
  (with-cell-stop ((markdown-new-start))
    (mdnew-reply "{\"combined\": \"the extracted content\", \"results\": [{\"title\": \"A\", \"url\": \"https://a.test/1\"}]}")
    (let ((answer (mdnew-call "{\"operation\": \"search_extract\", \"query\": \"lisp\", \"n\": 2}")))
      (let* ((request (first (mdnew-asked)))
             (body (mdnew-request-body request)))
        (is (equal "https://markdown.new/search" (second request)) "the search endpoint")
        (is (equal "lisp" (nlk:json-value body :text "q")) "carrying the query")
        (is (= 2 (nlk:json-value body :integer "n")) "the count the call named")
        (is (equal "us" (nlk:json-value body :text "gl")) "and the locale's defaults")
        (is (equal "json" (nlk:json-value body :text "format")) "asking for JSON back"))
      (is (search "the extracted content" answer) "the extraction is the answer")
      (is (search "1. A" answer) "and the sources are numbered")
      (is (search "https://a.test/1" answer) "with their urls"))))

;;; --- the operation table ----------------------------------------------------------

(deftest markdown-new-cell-an-argument-the-operation-does-not-take-refuses (with-markdown-new-wire)
  (with-cell-stop ((markdown-new-start))
    (let ((text (mdnew-refusal "{\"operation\": \"search_extract\", \"query\": \"x\", \"url\": \"https://example.org\"}")))
      (is (search "takes no url" text) "the argument is named")
      (is (search "query" text) "with what the operation does take"))
    (is (null (mdnew-asked)) "and nothing was sent")))

(deftest markdown-new-cell-an-operation-without-its-argument-refuses (with-markdown-new-wire)
  (with-cell-stop ((markdown-new-start))
    (let ((text (mdnew-refusal "{\"operation\": \"crawl_status\"}")))
      (is (search "needs a job_id" text) "the missing argument is named"))
    (is (null (mdnew-asked)) "and nothing was sent")))

;;; --- the boundaries ---------------------------------------------------------------

(deftest markdown-new-cell-a-private-or-credentialed-url-never-leaves (with-markdown-new-wire)
  (with-cell-stop ((markdown-new-start))
    (is (search "inside the network"
                (mdnew-refusal "{\"operation\": \"url_convert\", \"url\": \"http://192.168.1.5/wiki\"}")) "an address inside the network is refused")
    (is (search "localhost"
                (mdnew-refusal "{\"operation\": \"url_convert\", \"url\": \"http://localhost:8080/x\"}")) "and so is this machine")
    (is (search "access_token"
                (mdnew-refusal "{\"operation\": \"url_convert\", \"url\": \"https://example.org/?access_token=abc\"}")) "a credential in a query parameter is named")
    (is (search "http or https"
                (mdnew-refusal "{\"operation\": \"url_convert\", \"url\": \"file:///etc/passwd\"}")) "and a scheme that is not the web's")
    (is (null (mdnew-asked)) "no request was made for any of them")))

(deftest markdown-new-cell-a-local-document-is-uploaded-and-a-secret-store-is-not (with-markdown-new-wire)
  (let ((document (merge-pathnames "markdown-new-note.txt" (uiop:temporary-directory)))
        (secret (merge-pathnames ".env" (uiop:temporary-directory))))
    (with-open-file (out document :direction :output :if-exists :supersede)
      (write-string "hello from a document" out))
    (with-open-file (out secret :direction :output :if-exists :supersede)
      (write-string "TOKEN=abc" out))
    (nlk:with-cleanup ((ignore-errors (delete-file document))
                       (ignore-errors (delete-file secret)))
      (with-cell-stop ((markdown-new-start))
        (mdnew-reply "{\"data\": {\"content\": \"# note\\n\\nhello\"}}")
        (let ((answer (mdnew-call (format nil "{\"operation\": \"file_convert\", \"source\": ~s}"
                                          (namestring document)))))
          (let ((request (first (mdnew-asked))))
            (is (equal "https://markdown.new/convert" (second request)) "the upload endpoint")
            (is (search "multipart/form-data; boundary=" (mdnew-request-header request "content-type")) "posted as a multipart upload")
            (let ((body (sb-ext:octets-to-string (getf (cddr request) :content) :external-format :latin-1)))
              (is (search "markdown-new-note.txt" body) "naming the file")
              (is (search "hello from a document" body) "and carrying its bytes")))
          (is (search "# note" answer) "the converted Markdown is the answer"))
        (let ((sent (length (mdnew-asked))))
          (is (search "secret store"
                      (mdnew-refusal (format nil "{\"operation\": \"file_convert\", \"source\": ~s}"
                                              (namestring secret)))) "a secret store is refused by name")
          (is (= sent (length (mdnew-asked))) "before anything is sent"))))))

(deftest markdown-new-cell-an-upload-over-the-cap-refuses (with-markdown-new-wire)
  (let ((big (merge-pathnames "markdown-new-big.txt" (uiop:temporary-directory))))
    (with-open-file (out big :direction :output :if-exists :supersede)
      (write-string (make-string 2000 :initial-element #\y) out))
    (nlk:with-cleanup ((ignore-errors (delete-file big)))
      (with-cell-stop ((markdown-new-start "max_upload_bytes" 1024))
        (is (search "over the 1024 byte upload cap"
                    (mdnew-refusal (format nil "{\"operation\": \"file_convert\", \"source\": ~s}"
                                            (namestring big)))))
        (is (null (mdnew-asked)) "and nothing was sent")))))

;;; --- crawls -----------------------------------------------------------------------

(deftest markdown-new-cell-a-crawl-starts-and-names-the-next-step (with-markdown-new-wire)
  (with-cell-stop ((markdown-new-start))
    (mdnew-reply "{\"jobId\": \"job-7\"}")
    (let ((answer (mdnew-call "{\"operation\": \"crawl_start\", \"url\": \"https://docs.test\", \"limit\": 3}")))
      (let ((body (mdnew-request-body (first (mdnew-asked)))))
        (is (equal "https://markdown.new/crawl" (second (first (mdnew-asked)))) "the crawl endpoint")
        (is (= 3 (nlk:json-value body :integer "limit")) "bounded, as the call asked")
        (is (= 1 (nlk:json-value body :integer "depth")) "one link deep by default"))
      (is (search "job job-7" answer) "the job id is the answer's subject")
      (is (search "crawl_status" answer) "and the next operation is named"))))

(deftest markdown-new-cell-a-run-waits-for-the-job-and-reads-its-markdown (with-markdown-new-wire)
  (let ((status-calls 0))
    (with-cell-stop ((markdown-new-start))
      (setf *reply*
            (lambda (method url args)
              (declare (ignore method args))
              (cond
                ((search "?format=json" url)
                 (incf status-calls)
                 (if (= 1 status-calls)
                     (values "{\"result\": {\"status\": \"running\", \"records\": []}}" 200)
                     (values "{\"result\": {\"status\": \"completed\", \"records\": [{\"url\": \"https://docs.test/a\"}]}}" 200)))
                ((search "/crawl/status/job-7" url)
                 (values "# Crawled page" 200))
                ((search "/crawl" url)
                 (values "{\"jobId\": \"job-7\"}" 200))
                (t (error "unexpected request ~a" url)))))
      (let ((answer (mdnew-call "{\"operation\": \"crawl_run\", \"url\": \"https://docs.test\"}")))
        (is (= 4 (length (mdnew-asked))) "a start, two status checks and one download")
        (is (search "# Crawled page" answer) "the crawl's Markdown is the answer")
        (is (search "crawl job-7" answer) "named by its job")))))

(deftest markdown-new-cell-a-refused-status-carries-the-service-s-words (with-markdown-new-wire)
  (with-cell-stop ((markdown-new-start))
    (mdnew-reply "{\"success\": false, \"error\": \"page unreachable\"}" 502)
    (let ((text (mdnew-refusal "{\"operation\": \"url_convert\", \"url\": \"https://example.org/x\"}")))
      (is (search "HTTP 502" text) "the status is named")
      (is (search "page unreachable" text) "with what the service said"))
    (is (= 2 (length (mdnew-asked))) "and a 5xx is retried once before it gives up")))

;;; --- the operator's command -------------------------------------------------------

(deftest markdown-new-cell-the-command-reads-a-url-and-describes-itself (with-markdown-new-wire)
  (with-cell-stop ((markdown-new-start))
    (let ((status (nodecode-markdown-new::run-command "" nil)))
      (is (search "https://markdown.new" status) "no argument reports the connection")
      (is (search "url_convert" status) "and what the tool can be asked")
      (is (null (mdnew-asked)) "and sends nothing"))
    (mdnew-reply "{\"content\": \"# read from the keyboard\"}")
    (let ((answer (nodecode-markdown-new::run-command "  https://example.org/k  " nil)))
      (is (search "read from the keyboard" answer) "an argument reads that page")
      (is (equal "https://example.org/k" (nlk:json-value (mdnew-request-body (first (mdnew-asked))) :text "url")) "the address trimmed, as typed"))))
