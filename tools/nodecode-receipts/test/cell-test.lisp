;;;; cell-test.lisp --- the receipts cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A workspace is a scratch folder under the hermetic home, and the one
;;;; network call -- NODECODE-RECEIPTS::FETCH-URL -- is stubbed, so nothing
;;;; here opens a socket. Each attempt is its own task id, and the lifecycle
;;;; slice proves the section, the manual, /receipts and the tools come off
;;;; with the cell.

(in-package #:nodecode.test)

(define-test-slice "receipts" "RECEIPTS-CELL-" :start nodecode-receipts:start-cell)

(define-cell-lifecycle-tests "receipts"
  (:help :receipts)
  (:command "receipts")
  (:running (is (= 1048576 (nodecode-receipts::setting :read-cap-bytes)) "read_cap_bytes is read"))
  (:refused ("read_cap_bytes" 0) ("command_timeout" 0))
  (:idle receipts:receipts-error (receipts:receipts-get nil)))

;;; --- fixtures ------------------------------------------------------------------

(defvar *receipts-counter* 0
  "How many scratch workspaces and task ids this slice has made.")

(defun receipts-task ()
    "A task id nothing else in the slice uses."
    (format nil "task-~d-~d" (incf *receipts-counter*) (random 100000)))
(defun receipts-workspace (&rest files)
    "A scratch workspace holding FILES, each a cons of name and text."
    (let ((folder (nlk:home (format nil "ws-~d-~d/" (incf *receipts-counter*) (random 100000)))))
      (ensure-directories-exist (merge-pathnames "x" folder))
      (dolist (file files)
        (with-open-file (out (merge-pathnames (car file) folder) :direction :output
                                                          :if-exists :supersede
                                                          :if-does-not-exist :create)
          (write-string (cdr file) out)))
      folder))

(defun receipts-spec (workspace task &rest checks)
  "A spec object: SCHEMA_VERSION 1 over WORKSPACE and TASK with CHECKS."
  (nlk:make-json-object "schema_version" 1 "task_id" task
                         "workspace_root" (namestring workspace)
                         "checks" (coerce checks 'vector)))

(defun receipts-begin (spec)
  "BEGIN through the tool's own contract, as the dispatcher calls it."
  (cell-json (receipts:receipts-begin
               (nodecode-receipts::decode-begin-request (nlk:json-object "spec" spec)))))

(defun receipts-check (attempt)
  "CHECK through the tool's own contract."
  (cell-json (receipts:receipts-check
               (nodecode-receipts::decode-check-request
                (nlk:json-object "attempt_id" attempt)))))

(defun receipts-get-it (task &optional attempt)
  "GET through the tool's own contract."
  (cell-json (receipts:receipts-get
               (nodecode-receipts::decode-get-request
                (if attempt (nlk:json-object "task_id" task "attempt_id" attempt)
                    (nlk:json-object "task_id" task))))))

(defun receipts-export-it (attempt format &optional (public t))
  "EXPORT through the tool's own contract."
  (cell-json (receipts:receipts-export
               (nodecode-receipts::decode-export-request
                (nlk:json-object "attempt_id" attempt "format" format
                                 "public" public)))))

(defun receipts-field (json key)
  "KEY's value in JSON, a hash table as CELL-JSON answers it."
  (gethash key json))

(defun receipts-check-row (json id)
  "The check row called ID in receipt JSON."
  (find id (coerce (gethash "checks" json) 'list) :key (lambda (row) (gethash "id" row))
        :test #'equal))

(defun receipts-workspace-of (json)
  "The workspace_root a receipt carries."
  (gethash "workspace_root" (gethash "spec" json)))

(defun file-check (id path &rest more)
  "A file check object."
  (apply #'nlk:make-json-object "id" id "kind" "file" "path" path more))

(defun page-check (id url &rest more)
  "A website check object."
  (apply #'nlk:make-json-object "id" id "kind" "website" "url" url
         "expected_status" 200 "contains" "Ready" more))

(defun test-check (id command workdir &rest more)
  "A test check object."
  (apply #'nlk:make-json-object "id" id "kind" "test" "command" command
         "workdir" (namestring workdir) "expected_exit" 0 more))

(defmacro with-receipts (&body body)
  "BODY with the cell started, and stopped on the way out."
  `(with-cell-stop ((receipts-start)) ,@body))

(defmacro with-page ((text &key (status 200) (truncated nil)) &body body)
  "BODY with the one network call answering TEXT at STATUS."
  `(with-stubbed-fdefinitions
       ((nodecode-receipts::fetch-url (url timeout)
                                      (declare (ignorable url timeout))
                                      (values ,text ,status ,truncated)))
     ,@body))

;;; --- a file and a page -----------------------------------------------------------

(deftest receipts-cell-verifies-a-file-and-a-page (with-receipts)
  (let* ((workspace (receipts-workspace (cons "index.html" "<h1>Ready to launch</h1>")))
         (task (receipts-task))
         (begin (receipts-begin (receipts-spec workspace task
                                                (file-check "page" "index.html"
                                                            "contains" "Ready to launch")
                                                (page-check "live" "https://example.test/")))))
    (is (eq t (receipts-field begin "ok")) "a spec begins an attempt")
    (is (search "att_" (gethash "attempt_id" begin)) "with an attempt id")
    (is (= 64 (length (gethash "criteria_digest" begin))) "and a criteria digest")
    (with-page ("<html>Ready to launch</html>")
      (let* ((receipt (receipts-check (gethash "attempt_id" begin)))
             (page (receipts-check-row receipt "page")))
        (is (equal "verified" (gethash "state" receipt)) "every check passed")
        (is (equal 24 (gethash "size" (gethash "evidence" page))) "the evidence keeps the size")
        (is (= 64 (length (gethash "sha256" (gethash "evidence" page)))) "and the digest of the bytes")
        (is (equal "passed" (gethash "state" (receipts-check-row receipt "live"))))))))

(deftest receipts-cell-fails-on-absent-text (with-receipts)
  (let* ((workspace (receipts-workspace (cons "index.html" "<h1>Coming soon</h1>")))
         (task (receipts-task))
         (begin (receipts-begin (receipts-spec workspace task
                                                (file-check "page" "index.html"
                                                            "contains" "Ready to launch"))))
         (receipt (receipts-check (gethash "attempt_id" begin)))
         (row (receipts-check-row receipt "page")))
    (is (equal "failed" (gethash "state" receipt)) "a missing requirement fails the receipt")
    (is (equal "required text absent" (gethash "reason" row)))
    (is (equal "index.html" (gethash "relative_path" (gethash "evidence" row))))
    (let ((again (receipts-check (gethash "attempt_id" begin))))
      (is (equal (gethash "checked_at" receipt) (gethash "checked_at" again)) "checking a sealed attempt answers the receipt it sealed"))))

(deftest receipts-cell-checks-json-keys (with-receipts)
  (let* ((workspace (receipts-workspace (cons "manifest.json" "{\"name\": \"x\"}")))
         (task (receipts-task))
         (begin (receipts-begin (receipts-spec workspace task
                                                (file-check "keys" "manifest.json"
                                                            "json_keys" (vector "name" "version")))))
         (row (receipts-check-row (receipts-check (gethash "attempt_id" begin)) "keys")))
    (is (equal "failed" (gethash "state" row)))
    (is (equal "missing JSON keys" (gethash "reason" row)))
    (is (equal '("version")
               (coerce (gethash "missing_keys" (gethash "evidence" row)) 'list)))))

(deftest receipts-cell-judges-a-page-by-status-and-text (with-receipts)
  (let* ((workspace (receipts-workspace (cons "x.txt" "x")))
         (task (receipts-task))
         (begin (receipts-begin (receipts-spec workspace task
                                                (page-check "wrong" "https://example.test/")
                                                (page-check "absent" "https://example.test/ready")))))
    (with-page ("<html>nothing here</html>" :status 404)
      (let* ((receipt (receipts-check (gethash "attempt_id" begin)))
             (wrong (receipts-check-row receipt "wrong"))
             (absent (receipts-check-row receipt "absent")))
        (is (equal "failed" (gethash "state" wrong)) "HTTP 200 alone is not success")
        (is (search "status 404, expected 200" (gethash "reason" wrong)))
        (is (equal "failed" (gethash "state" absent)) "a status match alone is not success either")))))

(deftest receipts-cell-refuses-a-local-target-without-opting-in (with-receipts)
  (let* ((workspace (receipts-workspace (cons "x.txt" "x")))
         (task (receipts-task))
         (begin (receipts-begin (receipts-spec workspace task
                                                (page-check "local" "http://127.0.0.1:8080/")))))
    (with-page ("<html>Ready</html>")
      (let ((row (receipts-check-row (receipts-check (gethash "attempt_id" begin)) "local")))
        (is (equal "unverified" (gethash "state" row)) "a local page is not fetched by default")
        (is (search "allow_loopback_http" (gethash "reason" row)))))))

;;; --- a command ------------------------------------------------------------------

(deftest receipts-cell-runs-a-test-command (with-receipts)
  (let* ((workspace (receipts-workspace (cons "x.txt" "x")))
         (task (receipts-task))
         (begin (receipts-begin (receipts-spec workspace task
                                                (test-check "pass" "printf PASSED" workspace
                                                            "contains" "PASSED")
                                                (test-check "fail" "printf NOPE" workspace
                                                            "expected_exit" 3))))
         (receipt (receipts-check (gethash "attempt_id" begin))))
    (is (equal "failed" (gethash "state" receipt)) "one failed command fails the receipt")
    (is (equal "passed" (gethash "state" (receipts-check-row receipt "pass"))))
    (is (equal "failed" (gethash "state" (receipts-check-row receipt "fail"))))
    (is (equal 0 (gethash "exit_code"
                          (gethash "evidence" (receipts-check-row receipt "pass")))))
    (is (search "PASSED" (gethash "output"
                                   (gethash "evidence"
                                           (receipts-check-row receipt "pass")))))))

(deftest receipts-cell-times-out-a-slow-command (with-receipts)
  (let* ((workspace (receipts-workspace (cons "x.txt" "x")))
         (task (receipts-task))
         (begin (receipts-begin (receipts-spec workspace task
                                                (test-check "slow" "sleep 5" workspace
                                                            "timeout" 1))))
         (row (receipts-check-row (receipts-check (gethash "attempt_id" begin)) "slow")))
    (is (equal "unverified" (gethash "state" row)) "a command that runs out of time is unverified")
    (is (search "timed out" (gethash "reason" row)))))

;;; --- get, export and the slash command ---------------------------------------------

(deftest receipts-cell-reads-back-what-it-sealed (with-receipts)
  (let* ((workspace (receipts-workspace (cons "x.txt" "x")))
         (task (receipts-task))
         (begin (receipts-begin (receipts-spec workspace task
                                                (file-check "page" "x.txt" "contains" "x"))))
         (attempt (gethash "attempt_id" begin))
         (open (receipts-get-it task))
         (sealed (receipts-check attempt))
         (back (receipts-get-it task attempt)))
    (is (equal 1 (gethash "count" open)) "the open attempt is on the record")
    (is (equal "open" (gethash "state" (first (coerce (gethash "attempts" open) 'list)))))
    (is (equal 1 (gethash "count" back)))
    (is (equal "verified" (gethash "state" (first (coerce (gethash "attempts" back) 'list)))))
    (is (equal (gethash "criteria_digest" sealed)
               (gethash "criteria_digest"
                        (first (coerce (gethash "attempts" back) 'list)))) "the receipt read back is the one sealed")
    (is (equal 0 (gethash "count" (receipts-get-it "no-such-task"))) "another task has none")))

(deftest receipts-cell-exports-a-page-and-a-public-document (with-receipts)
  (let* ((workspace (receipts-workspace (cons "x.txt" "x")))
         (task (receipts-task))
         (begin (receipts-begin (receipts-spec workspace task
                                                (test-check "pass" "printf PASSED" workspace
                                                            "contains" "PASSED"))))
         (attempt (gethash "attempt_id" begin)))
    (receipts-check attempt)
    (let ((page (receipts-export-it attempt "html"))
          (document (receipts-export-it attempt "json")))
      (is (eq t (receipts-field page "ok")) "a page is written")
      (is (search "receipt-" (gethash "filename" page)) "under a generated name")
      (is (plusp (gethash "bytes" page)))
      (is (search "verified"
                  (uiop:read-file-string
                   (merge-pathnames (gethash "filename" page)
                                    (nodecode-receipts::exports-directory)))))
      (let ((text (uiop:read-file-string
                   (merge-pathnames (gethash "filename" document)
                                    (nodecode-receipts::exports-directory)))))
        (is (search "raw_command_output_omitted" text) "a public export says what it dropped")
        (is (null (search (namestring workspace) text)) "and carries no absolute path")
        (is (null (search "printf PASSED" text)) "nor the command line"))
      (is (search "not found" (gethash "error" (receipts-export-it "att_missing" "json"))) "an attempt that was never sealed refuses"))))

(deftest receipts-cell-lists-receipts-with-the-command (with-receipts)
  (let* ((workspace (receipts-workspace (cons "x.txt" "x")))
         (task (receipts-task))
         (begin (receipts-begin (receipts-spec workspace task
                                                (file-check "page" "x.txt" "contains" "x"))))
         (attempt (gethash "attempt_id" begin)))
    (is (search (format nil "Receipts for ~a: 1" task) (receipts:run-slash task nil)))
    (receipts-check attempt)
    (let ((listed (receipts:run-slash (format nil "~a ~a" task attempt) nil)))
      (is (search attempt listed))
      (is (search "verified" listed)))
    (is (equal "usage: /receipts <task-id> [attempt-id]"
               (receipts:run-slash "" nil)))))

;;; --- what it refuses ---------------------------------------------------------------

(deftest receipts-cell-refuses-a-spec-outside-the-contract (with-receipts)
  (let* ((workspace (receipts-workspace (cons "x.txt" "x")))
         (task (receipts-task))
         (check (file-check "page" "x.txt" "contains" "x")))
    (flet ((refusal (spec) (gethash "error" (receipts-begin spec))))
      (is (search "unknown fields"
                  (refusal (nlk:make-json-object "schema_version" 1 "task_id" task
                                                 "workspace_root" (namestring workspace)
                                                 "checks" (vector check) "extra" 1))))
      (is (search "workspace-relative"
                  (refusal (receipts-spec workspace task
                                           (file-check "page" "../x.txt" "contains" "x")))))
      (is (search "duplicate check id"
                  (refusal (receipts-spec workspace task check check))))
      (is (search "one test check per exact command/workdir pair"
                  (refusal (receipts-spec workspace task
                                           (test-check "a" "true" workspace)
                                           (test-check "b" "true" workspace)))))
      (is (search "task_id" (refusal (receipts-spec workspace "not/a/task" check))))
      (receipts-begin (receipts-spec workspace task check))
      (is (search "still open"
                  (refusal (receipts-spec workspace task
                                           (file-check "other" "x.txt" "contains" "x")))) "one attempt is open per task id")
      (is (search "not an existing directory"
                  (refusal (receipts-spec (nlk:home "nothing-here/")
                                          (receipts-task) check)))))))
