;;;; cell-test.lisp --- the gate, the tool, the command, the ledger.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is this cell's own behaviour: a push with no receipt
;;;; for the tree it publishes is refused and a push with one runs; the tool
;;;; takes the verdict from the check's exit status and the fingerprint, never
;;;; from what the model says; the command refuses to run a check nobody set.

(in-package #:nodecode.test)

(defparameter +root+ "/repo" "The repository the stubbed git answers for.")
(defparameter +commit+ "c0ffee1234567890" "The commit the stubbed git resolves.")
(defparameter +tree+ "5f3a1b2c3d4e5f60718293a4b5c6d7e8f9012345" "Its tree.")

(defun push-answers (&rest extra) "The stubbed git's answers for one repository, its HEAD and a push of it."
  (append extra
          (list (list "rev-parse --show-toplevel" +root+ 0)
                (list "rev-parse --verify -q --end-of-options HEAD^{commit}" +commit+ 0)
                (list (format nil "rev-parse --verify -q --end-of-options ~a^{tree}" +commit+) +tree+ 0)
                (list "rev-parse --verify -q HEAD^{tree}" +tree+ 0)
                (list "rev-parse --verify -q HEAD^{commit}" +commit+ 0)
                (list "add -A" "" 0)
                (list "write-tree" +tree+ 0))))

(defun verdict (form &rest strings) "The installed hook's answer for FORM with the strings object STRINGS
(name text ...): :ALLOWED, or the refusal text."
  (funcall (cell-hook "nodecode-pushgate" :tool)
           (list :name "eval"
                 :arguments (if strings
                                (nlk:json-object "form" form "strings" (let ((object (nlk:json-object)))
                                             (loop for (name text) on strings by #'cddr
                                                   do (setf (gethash name object) text))
                                             object))
                                (nlk:json-object "form" form))
                 :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(define-cell-lifecycle-tests "pushgate"
  (:hooks :tool)
  (:help :pushgate)
  (:command "pushgate")
  (:running (is (equal "make test" (nodecode-pushgate::check-for +root+)) "the section's check is the one a repository without its own uses"))
  (:refused ("max_receipts" 0)))
(deftest pushgate-cell-refuses-a-push-with-no-receipt (with-cell-stop ((pushgate-start))
  (with-stubs (:answers (apply #'push-answers '()))
    (let ((refusal (verdict "(sh \"git push origin HEAD\")")))
      (is (stringp refusal) "a push of a commit with no receipt is refused")
      (is (search "refused by nodecode-pushgate" refusal))
      (is (search "no passing run of `make test`" refusal)))
    (is (equal :allowed (verdict "(sh \"make test\")")) "the check itself is not a push: it runs")
    (is (equal :allowed (verdict "(sh \"rg push src\")")) "a command that merely says push is not one"))))
(deftest pushgate-cell-allows-a-push-whose-tree-has-passed (with-cell-stop ((pushgate-start))
  (with-stubs (:answers (apply #'push-answers '()))
    (nodecode-pushgate::record-receipt +root+ "make test" +tree+ "passed" 0)
    (is (equal :allowed (verdict "(sh \"git push origin HEAD\")")) "the commit's own tree carries a passing receipt")
    (nodecode-pushgate::record-receipt +root+ "make test" +tree+ "failed" 1)
    (is (stringp (verdict "(sh \"git push origin HEAD\")")) "a failed run for the same content supersedes the earlier pass"))))
(deftest pushgate-cell-reads-the-command-out-of-the-call (with-cell-stop ((pushgate-start))
  (with-stubs (:answers (apply #'push-answers '()))
    (is (stringp (verdict "(sh \"cd /repo && git -C /repo push origin HEAD\")")) "a static cd and -C are followed to the repository")
    (is (stringp (verdict "(uiop:run-program (list \"git\" \"push\" \"origin\" \"HEAD\"))")) "an argv call is read the same way")
    (is (stringp (verdict "(sh \"gh pr create --head HEAD\")")) "gh pr create is gated by the commit it would publish"))))
(deftest pushgate-cell-tool-takes-the-verdict-from-the-exit-status (with-cell-stop ((pushgate-start))
  (with-stubs (:answers (apply #'push-answers '()) :check (0 "1 test passed"))
    (let ((answer (princ-to-string (nle::message-content
                                    (execute-wire-call "call-1" "pushgate" "{\"action\":\"run\"}")))))
      (is (search "passed" answer) "the tool reports the run")
      (is (equal "passed" (nodecode-pushgate::receipt-field
                            (nodecode-pushgate::latest-receipt +root+ +tree+ "make test") "status")) "and the ledger binds it to the tree that was fingerprinted")
      (is (equal :allowed (verdict "(sh \"git push origin HEAD\")")) "so the push of that content now runs")))
  (with-stubs (:answers (apply #'push-answers '()) :check (1 "1 test failed"))
    (let ((answer (princ-to-string (nle::message-content
                                    (execute-wire-call "call-2" "pushgate" "{\"action\":\"run\"}")))))
      (is (search "failed" answer) "a failing check says so")
      (is (search "stays blocked" answer) "and that the push does")))))
(deftest pushgate-cell-tool-refuses-a-run-with-no-check (with-cell-stop ((pushgate-start "check" ""))
  (with-stubs (:answers (apply #'push-answers '()))
    (let ((answer (princ-to-string (nle::message-content
                                    (execute-wire-call "call-3" "pushgate" "{\"action\":\"run\"}")))))
      (is (search "no check is configured" answer) "nothing to run")
      (is (equal :allowed (verdict "(sh \"git push origin HEAD\")")) "and a repository with no check is never refused")))))
(deftest pushgate-cell-command-sets-the-check-and-reports-status (with-cell-stop ((pushgate-start))
  (with-stubs (:answers (apply #'push-answers '()))
    (is (search "is now `npm test`" (nodecode-pushgate::run-slash "check npm test" nil)) "the operator sets the check")
    (is (equal "npm test" (nodecode-pushgate::check-for +root+)) "and the gate reads it from then on")
    (is (search "receipt: NONE" (nodecode-pushgate::run-slash "status" nil)) "the status says what is missing")
    (is (search "is now `make test`" (nodecode-pushgate::run-slash "check make test" nil)) "and it can be set back"))))
