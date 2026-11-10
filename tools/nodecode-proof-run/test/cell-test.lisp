;;;; cell-test.lisp --- Proof Run: the recording, the gate, the command, the file.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Offline: no network. One tool call is one op through the installed
;;;; :TOOL hook with a NEXT of our own (the stub, answering the answer the
;;;; test would have got), and a contract is a JSON file under the run's
;;;; scratch home. What is proved is the cell's own wiring and its verdict.

(in-package #:nodecode.test)

(define-test-slice "proof-run" "PROOF-RUN-" :recipe "proof-run"
  :start nodecode-proof-run:start-cell)

(defparameter +proof-run-session+ "proof-run-test"
  "The session these calls are filed under: no turn runs in a test.")

(defun proof-run-call (form &key (answer "{\"ok\":true}") failed (call-id "c1"))
  "One eval call through the cell's :TOOL hook, NEXT answering ANSWER and
the engine's failure flag set when FAILED, exactly as the dispatch binds
them; the hook's answer, which a refusal replaces."
  (let ((nodecode.evolved::*tool-call-failed* nil)
        (nodecode-proof-run::*session-override* +proof-run-session+))
    (funcall (cell-hook "nodecode-proof-run" :tool)
             (list :name "eval"
                   :arguments (nlk:json-object "form" form)
                   :call-id call-id)
             (lambda (op)
               (declare (ignore op))
               (when failed (setf nodecode.evolved::*tool-call-failed* t))
               answer))))

(defun proof-run-typed (args)
  "The /proof-run command's answer to ARGS, as the operator types it."
  (cell-entry "nodecode-proof-run" "proof-run" args +proof-run-session+))

(defun proof-run-refused-p (answer)
  (and (stringp answer) (search "ERROR: proof-run refused this call" answer) t))

(defmacro with-proof-run ((&rest pairs) &body body)
  "BODY inside a run of the cell started on PAIRS, its contract folder
emptied first so one test never reads another's."
  `(let ((nodecode-proof-run::*session-override* +proof-run-session+))
     (with-cell-stop ((proof-run-start ,@pairs))
       (let ((directory (nodecode-proof-run::contracts-directory)))
         (when (uiop:directory-exists-p directory)
           (uiop:delete-directory-tree directory :validate t)))
       (clrhash nodecode-proof-run::*contracts*)
       ,@body)))

(defun demonstrate-deploy ()
  "The run the tests reuse: one read, then one mutating call, then /proof-run stop."
  (proof-run-typed "start deploy")
  (proof-run-call "(config-get '(\"app\"))" :answer "{\"version\":\"1.0\"}")
  (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"app.conf\"}")
  (proof-run-typed "stop"))

;;; --- lifecycle -------------------------------------------------------------

(define-cell-lifecycle-tests "proof-run"
  (:config "mode" "enforce")
  (:hooks :tool)
  (:command "proof-run")
  (:help :proof-run)
  (:running (is (equal "enforce" (nodecode-proof-run::mode)) "the section's mode"))
  (:refused ("mode" "loud") ("max_age" 0)))

;;; --- the contract a demonstration leaves ------------------------------------

(deftest proof-run-cell-records-and-gates-the-same-call (with-proof-run ()
  (is (search "is live" (demonstrate-deploy)) "a recording becomes a live contract")
  (is (= 1 (hash-table-count nodecode-proof-run::*contracts*)) "one contract, on disk and in memory")
  (proof-run-call "(config-get '(\"app\"))" :answer "{\"version\":\"1.0\"}")
  (is (equal "{\"path\":\"app.conf\"}"
             (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"app.conf\"}")) "the demonstrated call runs again once its read is fresh")))

(deftest proof-run-cell-refuses-changed-arguments (with-proof-run ()
  (demonstrate-deploy)
  (proof-run-call "(config-get '(\"app\"))" :answer "{\"version\":\"1.0\"}")
  (let ((answer (proof-run-call "(write-file \"app.conf\" \"v=3\")" :answer "{\"path\":\"app.conf\"}")))
    (is (proof-run-refused-p answer) "other arguments are not the demonstrated action")
    (is (search "do not match a demonstrated eval call" answer) "and the refusal names why"))))

(deftest proof-run-cell-refuses-a-call-without-its-read (with-proof-run ()
  (demonstrate-deploy)
  (let ((answer (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"app.conf\"}")))
    (is (proof-run-refused-p answer) "no fresh read, no covered call")
    (is (search "unknown" answer) "and the missing observation is named"))))

(deftest proof-run-cell-refuses-a-changed-observation (with-proof-run ()
  (demonstrate-deploy)
  (proof-run-call "(config-get '(\"app\"))" :answer "{\"version\":\"9.0\"}")
  (let ((answer (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"app.conf\"}")))
    (is (proof-run-refused-p answer) "a read that answers differently blocks the step")
    (is (search "demonstrated" answer) "with what was demonstrated beside what is now"))))

(deftest proof-run-cell-consumes-a-read-once (with-proof-run ()
  (demonstrate-deploy)
  (proof-run-call "(config-get '(\"app\"))" :answer "{\"version\":\"1.0\"}")
  (is (equal "{\"path\":\"app.conf\"}"
             (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"app.conf\"}")) "the first use spends the read")
  (is (proof-run-refused-p
       (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"app.conf\"}")) "the second needs its own")))

(deftest proof-run-cell-a-different-answer-marks-it-unproven (with-proof-run ()
  (demonstrate-deploy)
  (proof-run-call "(config-get '(\"app\"))" :answer "{\"version\":\"1.0\"}")
  (is (equal "{\"path\":\"other.conf\"}"
             (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"other.conf\"}")) "the call itself ran")
  (let ((contract (gethash "deploy" nodecode-proof-run::*contracts*)))
    (is (equal "unproven" (gethash "evidence_status" contract)) "an answer unlike the demonstration marks the contract unproven"))
  (is (search "unproven" (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"app.conf\"}")) "and the covered call is refused until rebind")))

(deftest proof-run-cell-check-evidence-off-gates-on-arguments (with-proof-run ("check_evidence" nil)
  (demonstrate-deploy)
  (proof-run-call "(config-get '(\"app\"))" :answer "{\"version\":\"1.0\"}")
  (is (equal "{\"path\":\"other.conf\"}"
             (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"other.conf\"}")) "with the check off a different answer still runs")
  (is (equal "proven" (gethash "evidence_status" (gethash "deploy" nodecode-proof-run::*contracts*))) "and the contract stays proven")))

;;; --- the command ------------------------------------------------------------

(deftest proof-run-cell-learns-from-the-session-buffer (with-proof-run ()
  (proof-run-call "(config-get '(\"app\"))" :answer "{\"version\":\"1.0\"}")
  (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"app.conf\"}")
  (is (search "learned from 2 calls" (proof-run-typed "learn quick 2")) "the buffer becomes a contract")
  (proof-run-call "(config-get '(\"app\"))" :answer "{\"version\":\"1.0\"}")
  (is (equal "{\"path\":\"app.conf\"}"
             (proof-run-call "(write-file \"app.conf\" \"v=2\")" :answer "{\"path\":\"app.conf\"}")) "and it gates the same way")))

(deftest proof-run-cell-status-unbind-and-the-tool (with-proof-run ()
  (demonstrate-deploy)
  (is (search "bound=T" (proof-run-typed "status")) "status names the live contract")
  (is (search "unbound" (proof-run-typed "unbind deploy")) "unbind takes it out of the gate")
  (is (equal "{\"path\":\"app.conf\"}"
             (proof-run-call "(write-file \"app.conf\" \"v=3\")" :answer "{\"path\":\"app.conf\"}")) "an unbound contract no longer refuses anything")
  (let ((report (funcall (third (nle::find-tool "proof_status"))
                         (nlk:contract-decode (nlk:contract 'nodecode-proof-run::proof-status-request)
                                              (nlk:json-object)))))
    (is (search "\"name\":\"deploy\"" report) "the tool answers the same contracts")
    (is (search "\"bound\":false" report) "with its current bound state"))))

(deftest proof-run-cell-rebind-exempts-only-its-own-recording (with-proof-run ()
  (demonstrate-deploy)
  (is (search "Rebinding" (proof-run-typed "rebind deploy")) "a rebind records again")
  (is (equal "{\"path\":\"app.conf\"}"
             (proof-run-call "(write-file \"app.conf\" \"v=3\")" :answer "{\"path\":\"app.conf\"}")) "the contract being re-demonstrated does not refuse its own session")
  (is (search "is live" (proof-run-typed "stop")) "and the new version is saved")
  (is (= 2 (gethash "version" (gethash "deploy" nodecode-proof-run::*contracts*))) "version two")))
