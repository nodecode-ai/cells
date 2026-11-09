;;;; cell-test.lisp --- ProofGate: the verdict, the hook, the tool, the ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Offline: no provider, no network. A tool call is one op through the
;;;; installed :TOOL hook with a NEXT of our own (the stub), and the ledger is
;;;; a file in the run's scratch home. What is proved is the cell's own
;;;; wiring and its verdict, never the turn loop's.

(in-package #:nodecode.test)

(define-test-slice "proofgate" "PROOFGATE-" :recipe "proofgate"
  :start nodecode-proofgate:start-cell)

;;; --- the stubbed call seam ------------------------------------------------

(defparameter +session+ "proofgate-test-session"
  "The session these calls are filed under: no turn runs in a test.")

(defun proofgate-call (form &key (answer "ok") failed)
  "One eval call through the cell's :TOOL hook, NEXT answering ANSWER and the
engine's failure flag set when FAILED, exactly as the dispatch binds them."
  (let ((nodecode.evolved::*tool-call-failed* nil)
        (nodecode-proofgate::*session-override* +session+))
    (funcall (cell-hook "nodecode-proofgate" :tool)
             (list :name "eval"
                   :arguments (nlk:json-object "form" form)
                   :call-id "call-1")
             (lambda (op)
               (declare (ignore op))
               (when failed (setf nodecode.evolved::*tool-call-failed* t))
               answer))
    form))

(defun proofgate-check (reply)
  "The registered proof_check tool's answer for REPLY, through its own
boundary: the contract refuses a malformed call before the body runs."
  (funcall (third (nle::find-tool "proof_check"))
           (nlk:contract-decode (nlk:contract 'nodecode-proofgate::proof-check-request)
                                (nlk:json-object "reply" reply))))

(defun proofgate-verdict (reply)
  "The verdict word in the proof_check answer for REPLY, as the answer carries it."
  (let ((report (proofgate-check reply)))
    (or (and (search "\"verdict\":\"PASS\"" report) "PASS")
        (and (search "\"verdict\":\"BLOCK\"" report) "BLOCK")
        (and (search "\"verdict\":\"DOWNGRADE\"" report) "DOWNGRADE")
        report)))

(defmacro with-proofgate ((&rest pairs) &body body)
  "BODY inside a run of the cell started on PAIRS, its ledger wiped first."
  `(let ((nodecode-proofgate::*session-override* +session+))
     (with-cell-stop ((proofgate-start ,@pairs))
       (let ((path (nodecode-proofgate::ledger-path)))
         (when (probe-file path) (delete-file path)))
       ,@body)))

;;; --- lifecycle ------------------------------------------------------------

(define-cell-lifecycle-tests "proofgate"
  (:config "mode" "warn")
  (:hooks :tool)
  (:command "proof")
  (:help :proofgate)
  (:running (let ((mode (nodecode-proofgate::mode))) (is (equal "warn" mode) "the section's mode")))
  (:refused ("mode" "loud") ("max_evidence" 0)))

;;; --- the verdict ----------------------------------------------------------

(deftest proofgate-cell-passes-a-backed-claim (with-proofgate ("mode" "warn"))
  (proofgate-call "(write-file \"notes.md\" \"hello\")" :answer "wrote 1 file")
  (let ((report (proofgate-check "Saved the notes to notes.md.")))
    (is (search "PASS" report) "a write the session saw proves the claim")
    (is (search "\"checked_tool_calls\":1" report) "and the evidence is counted")))

(deftest proofgate-cell-blocks-an-unbacked-claim (with-proofgate ("mode" "warn"))
  (proofgate-call "(sh \"ls\")" :answer "a.txt")
  (let ((report (proofgate-check "Created report.md and posted it to the team.")))
    (is (search "\"verdict\":\"BLOCK\"" report) "nothing the session did backs it")
    (is (search "report.md" report) "and the claim is named back to the model")))

(deftest proofgate-cell-enforce-makes-a-block-the-calls-failure (with-proofgate ("mode" "enforce"))
  (let ((report (proofgate-check "All tests passed and the build succeeded.")))
    (is (search "ERROR: ProofGate withheld" report) "enforce answers the call failed")))

(deftest proofgate-cell-a-failed-call-contradicts-the-claim (with-proofgate ("mode" "warn"))
  (proofgate-call "(write-file \"gone.md\" \"x\")" :answer "ERROR: no space left" :failed t)
  (let ((report (proofgate-check "Written gone.md.")))
    (is (search "\"verdict\":\"BLOCK\"" report) "the attempt that failed is the contradiction")
    (is (search "\"contradicted\":true" report) "and it is marked as one")))

(deftest proofgate-cell-a-figure-must-be-in-the-evidence (with-proofgate ("mode" "warn"))
  (proofgate-call "(sh \"df -h .\")" :answer "Use% 40%")
  (is (search "PASS" (proofgate-verdict "Disk use is down to 40%.")) "a printed figure")
  (is (search "BLOCK" (proofgate-verdict "Disk use is down to 91%.")) "an unprinted one"))

(deftest proofgate-cell-refuses-a-check-without-a-reply ()
  (is (signals-error nlk:contract-refusal
        (nlk:contract-decode (nlk:contract 'nodecode-proofgate::proof-check-request)
                             (nlk:json-object "other" "x"))) "the reply is required"))

;;; --- the ledger and the command -------------------------------------------

(deftest proofgate-cell-ledger-records-and-answers (with-proofgate ("mode" "warn"))
  (proofgate-check "Nothing claimed, nothing to prove.")
  (let ((text (nodecode-proofgate::ledger-text))
        (status (funcall (third (nle::find-tool "proof_status")) (nlk:json-object))))
    (is (search "1 checked" text) "the ledger holds the check")
    (is (search "PASS=1" text) "with its verdict counted")
    (is (search "\"checked_replies\":1" status) "and proof_status reads the same record")))
