;;;; cell-test.lisp --- sluice against a scratch workspace and a stubbed shell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs a git install, a network or a real command: every run goes
;;;; through NODECODE-SLUICE::*RUN*, and the workspace is a folder under the
;;;; temporary directory that goes away with the test.

(in-package #:nodecode.test)

(define-cell-lifecycle-tests "sluice"
  (:help :sluice)
  (:command "sluice")
  (:running
   (is (nle::find-tool "sluice_run") "the run tool is registered")
   (is (nle::find-tool "sluice_apply") "and the apply tool"))
  (:stopped
   (is (null (nle::find-tool "sluice_run")) "stop takes the tools back"))
  (:refused ("max_file_mb" 0)
            ("max_workspace_mb" 0)
            ("max_file_mb" "soon")))

(deftest sluice-cell-missing-workspace-refuses
  (with-cell-stop ((sluice-start))
    (is (signals-error nodecode-sluice:sluice-error (nodecode-sluice::workspace-root)) "a required setting left out is a refusal, not a guess")))

(deftest sluice-cell-run-rewrites-the-workspace-into-the-stage
  (with-workspace (workspace ("a.txt" (format nil "one~%two~%")))
    (let ((*calls* nil)
          (nodecode-sluice::*run* (stub-run (format nil "ran~%") 0)))
      (let ((answer (tool-answer "sluice_run"
                                  (nlk:json-object
                                   "command" (format nil "cd ~a && make test" workspace)))))
        (is (search "exit 0" answer) "the answer says the exit status")
        (is (search "ran" answer) "and carries the command's output")
        (let ((command (first (first *calls*))))
          (is (search (nodecode-sluice::stage-path (nodecode-sluice::ensure-stage*)) command) "the workspace path was rewritten into the stage")
          (is (null (search workspace command)) "and the real path is not in the command"))))))

(deftest sluice-cell-diff-then-apply-lands-only-what-was-approved
  (with-workspace (workspace ("a.txt" (format nil "one~%two~%")))
    (let ((stage (nodecode-sluice::ensure-stage*)))
      (with-open-file (out (format nil "~a/a.txt" (nodecode-sluice::stage-path stage))
                           :direction :output :if-exists :supersede :external-format :utf-8)
        (write-string (format nil "one~%TWO~%") out))
      (let ((review (tool-answer "sluice_diff" (nlk:json-object))))
        (is (search "[1]" review) (format nil "the changed block is numbered: ~a" review))
        (is (search "TWO" review) "and shown with its new line"))
      (let ((answer (tool-answer "sluice_apply" (nlk:json-object "selection" "all"))))
        (is (search "applied 1 change" answer) "the apply reports what landed"))
      (let ((text (with-open-file (in (format nil "~aa.txt" workspace) :external-format :utf-8)
                     (let ((line (make-string (file-length in))))
                       (read-sequence line in)
                       line))))
        (is (search "TWO" text) "and the real file now carries the approved line")))))

(deftest sluice-cell-apply-refuses-a-file-that-moved-underneath-it
  (with-workspace (workspace ("a.txt" (format nil "one~%")))
    (let ((stage (nodecode-sluice::ensure-stage*)))
      (with-open-file (out (format nil "~a/a.txt" (nodecode-sluice::stage-path stage))
                           :direction :output :if-exists :supersede :external-format :utf-8)
        (write-string (format nil "ONE~%") out))
      (tool-answer "sluice_diff" (nlk:json-object))
      (with-open-file (out (format nil "~aa.txt" workspace)
                           :direction :output :if-exists :supersede :external-format :utf-8)
        (write-string (format nil "somebody else edited this~%") out))
      (let ((answer (tool-answer "sluice_apply" (nlk:json-object "selection" "all"))))
        (is (search "CONFLICT" answer) "a file that moved since the stage was taken conflicts")
        (is (search "applied 0 change" answer) "and nothing of it lands")))))

(deftest sluice-cell-holds-what-a-copy-cannot-contain
  (with-workspace (workspace ("a.txt" (format nil "one~%")))
    (let ((*calls* nil)
          (nodecode-sluice::*run* (stub-run (format nil "ok~%") 0)))
      (let ((answer (tool-answer "sluice_run" (nlk:json-object "command" "git push origin main"))))
        (is (search "NOT CONTAINED" answer) "a push is labelled not contained")
        (is (null *calls*) "and nothing ran"))
      (is (search "NOT CONTAINED"
                  (tool-answer "sluice_run" (nlk:json-object "command" "pip install requests"))) "so is an install")
      (is (null (search "NOT CONTAINED"
                        (tool-answer "sluice_run" (nlk:json-object "command" "make test")))) "a plain command is not held"))))

(deftest sluice-cell-classifier-reads-the-command
  (let ((root "/work/project"))
    (flet ((categories (command)
             (mapcar #'nodecode-sluice::finding-category
                     (nodecode-sluice::classify-command command root))))
      (is (member :push (categories "git push origin main")) "a push")
      (is (member :install (categories "npm install left-pad")) "an install")
      (is (member :outside-write (categories "echo x > /etc/hosts")) "a write outside")
      (is (member :outside-delete (categories "rm -rf /var/tmp/x")) "a delete outside")
      (is (member :network (categories "curl -X POST https://example.test/hook -d @x")) "a network write")
      (is (member :privilege (categories "sudo apt-get install -y git")) "and an elevated one")
      (is (null (categories "cd sub && make test")) "a build inside the workspace is not held")
      (is (null (categories "echo hi > out.txt")) "nor a redirect inside it"))))

(deftest sluice-cell-digests-are-sha256-through-what-a-release-carries
  ;; SHA-256 of "abc" (FIPS 180-2); the MD5 this once took needed sb-md5,
  ;; an SBCL contrib no release carries, so the cell loaded nowhere but on a
  ;; machine with SBCL installed beside it.
  (let ((abc "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"))
    (is (equal abc (nodecode-sluice::digest-string "abc")) "a text's digest is the SHA-256 of its UTF-8 bytes")
    (with-workspace (workspace ("a.txt" "abc"))
      (let ((file (nodecode-sluice::path-join workspace "a.txt")))
        (is (equal abc (nodecode-sluice::digest-file file)) "and a file's is the SHA-256 of its bytes")))))
