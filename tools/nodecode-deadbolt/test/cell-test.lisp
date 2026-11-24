;;;; cell-test.lisp --- the deadbolt cell, offline.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every exchange with the sidecar is a stubbed NODEcode-DEADBOLT::HTTP,
;;;; every credential a stub of the cell's own ENV-VALUE, every workspace and
;;;; review directory a directory under the platform's temporary one: nothing
;;;; here touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "deadbolt" "DEADBOLT-CELL-" :start nodecode-deadbolt:start-cell)

(define-cell-lifecycle-tests "deadbolt"
  (:config "workspace" (namestring (deadbolt-cell-directory "lifecycle"))
           "run_id" "deadbolt-cell-run"
           "admission_token" "dbw1-inert")
  (:help :deadbolt)
  (:command "deadbolt")
  (:refused ("sidecar_port" 0) ("workspace" #(1)) ("max_bytes" 1))
  (:idle deadbolt:deadbolt-error
         (deadbolt:prepare "a.txt" "x")
         (deadbolt:commit "00000000000000000000000000000000")
         (deadbolt:pending)))

;;; --- fixtures ---------------------------------------------------------------------

(defparameter *deadbolt-cell-root*
  (merge-pathnames (format nil "deadbolt-cell-~d-~d/" (get-universal-time) (random 1000000))
                   (uiop:temporary-directory))
  "Where this slice's workspaces and review directories live: fresh per run, so no
earlier run's artifact is in the way of this one's exclusive create.")

(defun deadbolt-cell-directory (&optional (name "out"))
  "The directory NAME under the test root, made when it is absent."
  (let ((directory (merge-pathnames (format nil "~a/" name) *deadbolt-cell-root*)))
    (ensure-directories-exist (merge-pathnames "probe" directory))
    directory))

(defun deadbolt-cell-healthy (&key (workspace (deadbolt-cell-directory "out"))
                                    (review (deadbolt-cell-directory "requests"))
                                    (run "deadbolt-cell-run"))
  "The cell started on a whole section: WORKSPACE, REVIEW and RUN."
  (deadbolt-start "workspace" (namestring workspace)
                  "run_id" run
                  "admission_token" "dbw1-inert"
                  "review_dir" (namestring review)))

(defmacro with-deadbolt-sidecar ((requests &key (body "{\"decision\":\"allow\",\"code\":\"admitted\"}")
                                             (status 200)) &body forms)
  "FORMS with the sidecar answering BODY and STATUS, each exchange kept in
REQUESTS as (METHOD URL HEADERS CONTENT), oldest first."
  `(let ((,requests '()))
     (with-stubbed-fdefinition (nodecode-deadbolt::http (method url &rest arguments)
                                (setf ,requests
                                      (append ,requests
                                              (list (list method url (getf arguments :headers)
                                                          (getf arguments :content)))))
                                (values ,body ,status nil nil))
       ,@forms)))

(defun deadbolt-cell-answer (text)
  "TEXT, one of the cell's JSON answers, as the object it is."
  (nlk:decode-json text :whole t))

(defun deadbolt-cell-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun deadbolt-cell-bytes (path)
  "PATH's bytes."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

;;; --- what a prepare does ------------------------------------------------------------

(deftest deadbolt-cell-prepare-saves-a-request-and-creates-nothing ()
  (let ((workspace (deadbolt-cell-directory "prepare")))
    (with-cell-stop ((deadbolt-cell-healthy :workspace workspace))
      (let* ((answer (deadbolt-cell-answer
                       (deadbolt:prepare "report.md" "A fictional reviewed report.")))
             (id (gethash "request_id" answer))
             (file (gethash "review_file" answer)))
        (is (eq t (gethash "prepared" answer)) "a request that could be saved says so")
        (is (eq nil (gethash "output_created" answer)) "and that nothing was created")
        (is (and (stringp id) (= 32 (length id))) "a 32-hex request ID comes back")
        (is (stringp file) "with the review file a person reads")
        (is (and (stringp file) (probe-file file)) "the review file is on disk")
        #+sbcl (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat file)))) "the review request is private: mode 0600")
        (is (not (probe-file (merge-pathnames "report.md" workspace))) "the destination is not created by a prepare")
        (when (and (stringp file) (probe-file file))
          (let ((envelope (nlk:decode-json (deadbolt-cell-bytes file) :whole t)))
            (is (hash-table-p envelope) "the review file is one JSON object")
            (is (eql 1 (gethash "version" envelope)))
            (is (equal "hermes_artifact_write_v1" (gethash "tool" envelope)) "the engine's own tool id, as its policy is written")
            (is (equal "deadbolt-cell-run" (gethash "agent_id" envelope)))
            (is (eq :null (gethash "dest" envelope)) "the envelope names no destination id")
            (is (equal id (gethash "nonce" envelope)) "the nonce is the request ID")
            (is (equal "report.md" (nlk:json-value envelope :string "arguments" "path")))
            (is (equal "A fictional reviewed report."
                       (nlk:json-value envelope :string "arguments" "content")) "the exact text a person is asked to approve")
            (is (integerp (gethash "expires_at" envelope)) "with a deadline")))))))

(deftest deadbolt-cell-a-path-outside-the-workspace-is-refused ()
  (with-cell-stop ((deadbolt-cell-healthy))
    (dolist (path '("../escape.md" "/etc/escape.md" "a//b.md" "./a.md" ""))
      (let ((answer (deadbolt-cell-answer (deadbolt:prepare path "x"))))
        (is (eq nil (gethash "prepared" answer))
            (format nil "~s is no relative path inside the workspace" path))
        (is (stringp (gethash "detail" answer)) "and the refusal says why")))))

;;; --- what a commit does --------------------------------------------------------------

(deftest deadbolt-cell-commit-asks-the-sidecar-then-creates-the-file ()
  (let ((workspace (deadbolt-cell-directory "commit")))
    (with-cell-stop ((deadbolt-cell-healthy :workspace workspace))
      (let* ((prepared (deadbolt-cell-answer (deadbolt:prepare "report.md" "exact text")))
             (id (gethash "request_id" prepared)))
        (with-deadbolt-sidecar (requests)
          (let ((answer (deadbolt-cell-answer (deadbolt:commit id))))
            (is (eq t (gethash "admitted" answer)))
            (is (eq t (gethash "completed" answer)))
            (is (eql 10 (nlk:json-value answer :integer "result" "bytes")) "the answer counts the bytes written")
            (is (equal "exact text"
                       (uiop:read-file-string (merge-pathnames "report.md" workspace))) "the reviewed text is the file's text")
            #+sbcl (is (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (merge-pathnames "report.md" workspace)))))) "and the artifact is created mode 0600")
            (destructuring-bind (method url headers content) (first requests)
              (is (eq :post method))
              (is (equal "http://127.0.0.1:9782/admit-action" url) "the one call is the sidecar's admission endpoint, on loopback")
              (is (equal "dbw1-inert" (deadbolt-cell-header headers "X-Deadbolt-Admission")) "carrying the run's scoped credential")
              (is (equal "application/json" (deadbolt-cell-header headers "content-type")))
              (let ((sent (nlk:decode-json content :whole t)))
                (is (equal id (gethash "nonce" sent)) "the envelope reviewed is the envelope sent")
                (is (equal "report.md" (nlk:json-value sent :string "arguments" "path")))))))))))

(deftest deadbolt-cell-a-denial-creates-no-file ()
  (let ((workspace (deadbolt-cell-directory "denied")))
    (with-cell-stop ((deadbolt-cell-healthy :workspace workspace))
      (let* ((prepared (deadbolt-cell-answer (deadbolt:prepare "report.md" "exact text")))
             (id (gethash "request_id" prepared)))
        (with-deadbolt-sidecar (requests :body "{\"decision\":\"deny\",\"code\":\"policy\"}")
          (let ((answer (deadbolt-cell-answer (deadbolt:commit id))))
            (is (eq nil (gethash "admitted" answer)))
            (is (eq nil (gethash "completed" answer)))
            (is (equal "policy" (nlk:json-value answer :string "decision" "code")) "the sidecar's own code is carried back")
            (is (not (probe-file (merge-pathnames "report.md" workspace))) "a denial writes nothing")
            (is (search "operator" (gethash "next_action" answer)) "and tells the model to stop and ask")))))))

(deftest deadbolt-cell-an-unreachable-sidecar-writes-nothing ()
  (let ((workspace (deadbolt-cell-directory "unreachable")))
    (with-cell-stop ((deadbolt-cell-healthy :workspace workspace))
      (let* ((prepared (deadbolt-cell-answer (deadbolt:prepare "report.md" "exact text")))
             (id (gethash "request_id" prepared)))
        (with-stubbed-fdefinition (nodecode-deadbolt::http (method url &rest arguments)
                                   (error "~a and ~a refused" method url))
          (let ((answer (deadbolt-cell-answer (deadbolt:commit id))))
            (is (eq nil (gethash "admitted" answer)))
            (is (equal "request_or_sidecar_unavailable" (gethash "error" answer)))
            (is (not (probe-file (merge-pathnames "report.md" workspace))) "no admission, no file")))))))

(deftest deadbolt-cell-an-existing-file-is-never-overwritten ()
  (let ((workspace (deadbolt-cell-directory "existing")))
    (with-cell-stop ((deadbolt-cell-healthy :workspace workspace))
      (let* ((prepared (deadbolt-cell-answer (deadbolt:prepare "report.md" "exact text")))
             (id (gethash "request_id" prepared))
             (target (merge-pathnames "report.md" workspace)))
        (with-open-file (out target :direction :output :if-exists :supersede)
          (write-string "older text" out))
        (with-deadbolt-sidecar (requests)
          (let ((answer (deadbolt-cell-answer (deadbolt:commit id))))
            (is (eq t (gethash "admitted" answer)))
            (is (eq nil (gethash "completed" answer)))
            (is (equal "unknown" (gethash "outcome" answer)) "a write that failed after admission is an unknown outcome")
            (is (equal "older text" (uiop:read-file-string target)) "and the file that was there is untouched")))))))

(deftest deadbolt-cell-a-request-is-bound-to-its-run-and-its-workspace ()
  (let ((workspace (deadbolt-cell-directory "bound"))
        (review (deadbolt-cell-directory "bound-requests")))
    (let ((id (with-cell-stop ((deadbolt-cell-healthy :workspace workspace :review review))
                (gethash "request_id"
                         (deadbolt-cell-answer (deadbolt:prepare "report.md" "exact text"))))))
      (let ((other (deadbolt-cell-directory "bound-other")))
        (with-cell-stop ((deadbolt-cell-healthy :workspace other :review review :run "another-run"))
          (with-deadbolt-sidecar (requests)
            (let ((answer (deadbolt-cell-answer (deadbolt:commit id))))
              (is (eq nil (gethash "admitted" answer)) "a request another run prepared is not this run's to commit")
              (is (null requests) "and the sidecar is never asked about it")
              (is (not (probe-file (merge-pathnames "report.md" other)))))))))))

;;; --- the tools, the command, the missing section -----------------------------------

(deftest deadbolt-cell-the-tools-and-the-command-answer ()
  (with-cell-stop ((deadbolt-cell-healthy))
    (is (assoc "deadbolt_prepare_write" nle::*tools* :test #'string=) "the prepare tool is registered")
    (is (assoc "deadbolt_commit_write" nle::*tools* :test #'string=) "and the commit tool")
    (let* ((record (nodecode-deadbolt::decode-deadbolt-write-request
                    (nlk:json-object "path" "tool.md" "content" "x")))
           (answer (deadbolt-cell-answer (nodecode-deadbolt::prepare-tool record))))
      (is (eq t (gethash "prepared" answer)) "the tool body runs the verb"))
    (let ((text (cell-entry "nodecode-deadbolt" "deadbolt" "" "deadbolt-cell-status")))
      (is (search "deadbolt: workspace" text) "/deadbolt says where its settings stand"))
    (let ((text (cell-entry "nodecode-deadbolt" "deadbolt" "pending" "deadbolt-cell-status")))
      (is (search "deadbolt:" text) "/deadbolt pending says what waits"))))

(deftest deadbolt-cell-a-missing-setting-is-named ()
  (with-cell-stop ((deadbolt-start "run_id" "deadbolt-cell-run"))
    (let ((answer (deadbolt-cell-answer (deadbolt:prepare "report.md" "x"))))
      (is (eq nil (gethash "prepared" answer)) "without a workspace nothing is prepared")
      (is (search "workspace" (gethash "detail" answer)) "and the refusal names the setting that is missing"))
    (let ((answer (deadbolt-cell-answer
                   (deadbolt:commit "00000000000000000000000000000000"))))
      (is (eq nil (gethash "admitted" answer)))
      (is (equal "request_or_sidecar_unavailable" (gethash "error" answer))))
    (let ((text (cell-entry "nodecode-deadbolt" "deadbolt" "" "deadbolt-cell-missing")))
      (is (search "still needed" text) "/deadbolt names what the section still needs"))))
