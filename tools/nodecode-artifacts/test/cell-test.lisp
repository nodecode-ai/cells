;;;; cell-test.lisp --- the six tools over a stubbed service, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs a service, a credential or the network: every request goes
;;;; through NODECODE-ARTIFACTS::*HTTP*, a stub that records the call it was
;;;; handed and answers what the test chose, and the credential lives in a
;;;; scratch folder under the run's own home.

(in-package #:nodecode.test)

(define-test-slice "artifacts" "ARTIFACTS-CELL-" :start nodecode-artifacts:start-cell)

(define-cell-lifecycle-tests "artifacts"
  (:config "base_url" "https://library.example" "token_file" (namestring (token-path)))
  (:help :artifacts)
  (:command "artifacts")
  (:running
   (is (nle::find-tool "artifact_publish") "the publish tool is registered")
   (is (nle::find-tool "artifact_share") "and the share tool"))
  (:stopped
   (is (null (nle::find-tool "artifact_publish")) "stop takes all six back"))
  (:refused ("timeout_seconds" 0) ("timeout_seconds" "soon"))
  (:idle nodecode-artifacts:artifacts-error (nodecode-artifacts::status-line)))

;;; --- the fixture ------------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . OPTIONS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed *HTTP* answers: a function of METHOD and URL => (values BODY STATUS).")

(defvar *scratch* 0
  "How many scratch folders this run has made, so each test's is its own.")

(defvar *dir* nil
  "The scratch folder of the fixture running now.")

(defun scratch-dir ()
  "A fresh folder under the run's home, holding a credential file."
  (let ((dir (uiop:ensure-directory-pathname
              (nlk:home (format nil "test-artifacts/~d/" (incf *scratch*))))))
    (ensure-directories-exist dir)
    (nlk:write-file-atomically (merge-pathnames "token" dir) "test-token" :mode :private)
    dir))

(defun token-path ()
  (merge-pathnames "token" (scratch-dir)))

(defun scripted (&rest answers)
  "An *ANSWER-FN* answering ANSWERS -- (BODY STATUS) -- in order."
  (lambda (method url)
    (declare (ignore method url))
    (let ((answer (pop answers)))
      (unless answer (error "the test's script has no answer left"))
      (values (first answer) (second answer)))))

(defmacro with-artifacts ((&rest pairs) &body body)
  "BODY with the cell started on a scratch credential and a stubbed service."
  `(let* ((*dir* (scratch-dir))
          (*requests* '())
          (*answer-fn* (lambda (method url)
                         (declare (ignore method url))
                         (error "unexpected request: no script")))
          (nodecode-artifacts::*http*
            (lambda (method url &rest options)
              (push (list* method url options) *requests*)
              (funcall *answer-fn* method url))))
     (with-cell-stop ((artifacts-start "base_url" "https://library.example"
                                        "token_file" (namestring (merge-pathnames "token" *dir*))
                                        ,@pairs))
       ,@body)))

(defun asked ()
  (reverse *requests*))

(defun request-header (request name)
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun request-content-text (request)
  (let ((content (getf (cddr request) :content)))
    (typecase content
      (string content)
      ((vector (unsigned-byte 8)) (sb-ext:octets-to-string content :external-format :utf-8))
      (t ""))))

(defun args (&rest pairs)
  (apply #'nlk:make-json-object pairs))

(defparameter +artifact-id+ "6f1c2f0e-3a4b-4c5d-8e9f-0a1b2c3d4e5f"
  "One id of the service's own shape.")

;;; --- publish --------------------------------------------------------------------

(deftest artifacts-cell-publishes-inline-content-privately ()
  (with-artifacts ()
    (setf *answer-fn*
          (scripted (list (format nil "{\"id\":\"~a\",\"slug\":\"weekly-report\",\"kind\":\"markdown\",\"active_shares\":0}"
                                   +artifact-id+)
                          201)))
    (let ((answer (nodecode-artifacts::tool-publish
                   (args "title" "Weekly report" "content" "# Hello" "kind" "markdown"))))
      (is (search +artifact-id+ answer) "the tool answers the id the service minted")
      (is (search "/artifacts/weekly-report" answer) "and the private library path")
      (is (search "No guest link" answer) "saying that publishing made no link"))
    (let ((request (first (asked))))
      (is (= 1 (length (asked))) "one request, one artifact")
      (is (eq :post (first request)) "a POST")
      (is (uiop:string-prefix-p "https://library.example/internal/v1/artifacts?" (second request)) "to the section's service, on its machine API")
      (is (search "kind=markdown" (second request)) "with the kind as a query member")
      (is (search "title=Weekly%20report" (second request)) "and the title escaped")
      (is (equal "Bearer test-token" (request-header request "authorization")) "carrying the credential file's token")
      (is (equal "text/markdown; charset=utf-8" (request-header request "content-type")) "typed as Markdown")
      (is (equal "# Hello" (request-content-text request)) "the source travels whole"))))

(deftest artifacts-cell-takes-a-file-by-its-extension ()
  (with-artifacts ()
    (let ((page (merge-pathnames "report.html" *dir*)))
      (nlk:write-file-atomically page "<html><body>hi</body></html>")
      (setf *answer-fn*
            (scripted (list (format nil "{\"id\":\"~a\",\"slug\":\"report\",\"kind\":\"html\"}" +artifact-id+)
                            201)))
      (let ((answer (nodecode-artifacts::tool-publish (args "path" (namestring page)))))
        (is (search "report" answer) (format nil "a path alone names the artifact by its file name: ~a" answer)))   (let ((request (first (asked))))
        (is (search "kind=html" (second request)) "the extension decides the kind")
        (is (equal "text/html; charset=utf-8" (request-header request "content-type")) "and the content type"))
      (setf *answer-fn* (scripted))
      (let ((answer (nodecode-artifacts::tool-publish
                     (args "path" (namestring (merge-pathnames "notes.txt" *dir*))))))
        (is (search "ERROR:" answer) "an unsupported extension is a refusal")
        (is (search "webp" answer) "naming the formats the service takes")))))

;;; --- list and get ----------------------------------------------------------------

(deftest artifacts-cell-lists-the-library-newest-first ()
  (with-artifacts ()
    (setf *answer-fn*
          (scripted (list (format nil "{\"items\":[{\"id\":\"~a\",\"kind\":\"html\",\"title\":\"Quarterly\",\"slug\":\"quarterly\",\"active_shares\":1}],\"next_cursor\":\"c2\"}"
                                   +artifact-id+)
                          200)))
    (let ((answer (nodecode-artifacts::tool-list
                   (args "query" "quarter" "kind" "html" "limit" 5))))
      (is (search "Quarterly" answer) "the row carries the title")
      (is (search "1 share" answer) "and how many links it has")
      (is (search "cursor c2" answer) "with the cursor to go on"))
    (let ((url (second (first (asked)))))
      (is (search "q=quarter" url) "the query is passed on")
      (is (search "kind=html" url) "with the kind filter")
      (is (search "limit=5" url) "and the limit")
      (is (uiop:string-prefix-p "https://library.example/internal/v1/artifacts?" url) "to the service"))))

(deftest artifacts-cell-reads-one-artifact-with-its-preview ()
  (with-artifacts ()
    (setf *answer-fn*
          (scripted (list (format nil "{\"artifact\":{\"id\":\"~a\",\"kind\":\"markdown\",\"title\":\"Weekly\",\"slug\":\"weekly\",\"byte_length\":7},\"shares\":[{\"id\":\"s1\",\"status\":\"active\"}]}"
                                   +artifact-id+)
                          200)
                    (list "# Hello" 200)))
    (let ((answer (nodecode-artifacts::tool-get (args "artifact_id" +artifact-id+))))
      (is (search "Weekly" answer) "the read answers the metadata")
      (is (search "s1 active" answer) "every guest link with its status")
      (is (search "# Hello" answer) "and a preview of a text artifact"))
    (is (equal (format nil "https://library.example/internal/v1/artifacts/~a/download" +artifact-id+) (second (second (asked)))) "the preview is the artifact's own bytes")))

;;; --- download -------------------------------------------------------------------

(deftest artifacts-cell-writes-the-bytes-and-keeps-what-is-there ()
  (with-artifacts ()
    (let ((target (merge-pathnames "copy.png" *dir*)))
      (setf *answer-fn* (scripted (list (make-array 4 :element-type '(unsigned-byte 8)
                                                       :initial-element 7)
                                         200)))
      (let ((answer (nodecode-artifacts::tool-download
                     (args "artifact_id" +artifact-id+ "path" (namestring target)))))
        (is (search "Wrote 4 bytes" answer) "the tool says what it wrote")
        (is (search "sha256:" answer) "and the digest of what landed")
        (is (= 4 (length (nodecode-artifacts::file-bytes target))) "the bytes are on disk"))
      (setf *answer-fn* (scripted (list (make-array 2 :element-type '(unsigned-byte 8)) 200)))
      (let ((answer (nodecode-artifacts::tool-download
                     (args "artifact_id" +artifact-id+ "path" (namestring target)))))
        (is (search "ERROR:" answer) "an existing file is not replaced")
        (is (search "overwrite" answer) "unless the call says so"))
      (let ((answer (nodecode-artifacts::tool-download
                     (args "artifact_id" +artifact-id+ "path" (namestring target) "overwrite" t))))
        (is (search "Wrote 2 bytes" answer) "and then it is")
        (is (= 2 (length (nodecode-artifacts::file-bytes target))) "with the new bytes")))))

;;; --- share and revoke ------------------------------------------------------------

(deftest artifacts-cell-shares-one-link-and-revokes-it ()
  (with-artifacts ()
    (setf *answer-fn*
          (scripted (list "{\"share\":{\"id\":\"s-1\",\"status\":\"active\",\"expires_at\":1791486400},\"url\":\"https://share.example/abc\"}"
                          201)))
    (let ((answer (nodecode-artifacts::tool-share
                   (args "artifact_id" +artifact-id+ "expires_in_hours" 24))))
      (is (search "https://share.example/abc" answer) "the guest URL comes back once")
      (is (search "s-1" answer) "with the share id to revoke it"))
    (let ((request (first (asked))))
      (is (equal "application/json" (request-header request "content-type")) "the expiry travels as JSON")
      (is (search "\"expires_in_seconds\":86400" (request-content-text request)) "in seconds, as the service reads it"))
    (setf *answer-fn*
          (scripted (list "{\"share\":{\"id\":\"s-1\",\"status\":\"revoked\"}}" 200)))
    (let ((answer (nodecode-artifacts::tool-revoke (args "share_id" "s-1"))))
      (is (search "revoked" answer) "a revoke answers the link's new status"))
    (is (eq :delete (first (second (asked)))) "as a DELETE of the share")
    (is (equal "https://library.example/internal/v1/shares/s-1" (second (second (asked)))) "on the share's own path")))

;;; --- the refusals ---------------------------------------------------------------

(deftest artifacts-cell-names-the-setting-that-is-missing ()
  (with-artifacts ("base_url" "")
    (setf *answer-fn* (scripted))
    (let ((answer (nodecode-artifacts::tool-list nil)))
      (is (search "ERROR:" answer) "a tool with no service is a refusal")
      (is (search "base_url" answer) "naming the member to set")
      (is (null (asked)) "and nothing was sent")))
  (with-artifacts ()
    (setf *answer-fn* (scripted))
    (delete-file (merge-pathnames "token" *dir*))
    (let ((answer (nodecode-artifacts::tool-list nil)))
      (is (search "ERROR:" answer) "a tool with no credential is a refusal")
      (is (search "token" answer) "naming the credential")
      (is (null (asked)) "and nothing was sent"))))
