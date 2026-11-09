;;;; cell-test.lisp --- the lifecycle, the digest the signer binds, and every
;;;; verdict of the :TOOL hook against a scripted signer.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

;;; --- lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "tracekit"
  (:fixture with-tracekit-signer ())
  (:hooks :tool)
  (:help :tracekit)
  (:command "tracekit")
  (:running (is (equal "nodecode" (nodecode-tracekit::setting :agent))
                "the run is registered as nodecode's")
            (is (null nodecode-tracekit::*run*) "no run until a call is decided"))
  (:refused ("ask_seconds" 1) ("timeout_seconds" 0)))

(deftest tracekit-cell-refuses-without-a-signer ()
  ;; The one required setting: nothing to decide against, so the cell refuses
  ;; rather than running every call ungated.
  (is (signals-error nlk:config-refusal
        (nodecode-tracekit:start-cell
         (nlk:json-object "tracekit" (nlk:json-object "agent" "nodecode"))))
      "a section with no signer refuses"))

;;; --- the digest the signer binds ----------------------------------------------------

(deftest tracekit-cell-writes-canonical-json ()
  ;; The signer recomputes every digest over canonical JSON, so the cell's own
  ;; writer must be RFC 8785 to the byte: sorted keys, shortest numbers.
  (is (equal "{\"a\":1,\"b\":2}"
             (tracekit::json-text (nlk:json-object "b" 2 "a" 1)))
      "keys sorted, no whitespace")
  (is (equal "{\"x\":1}" (tracekit::json-text (nlk:json-object "x" 1.0d0)))
      "an integral float prints as an integer")
  (is (equal "{\"x\":0.0001}" (tracekit::json-text (nlk:json-object "x" 1.0d-4)))
      "a small float prints in fixed notation")
  (is (equal "{\"x\":1e-7}" (tracekit::json-text (nlk:json-object "x" 1.0d-7)))
      "and a very small one in exponent notation")
  (is (equal (nlk:sha256-text "{\"a\":1,\"b\":2}")
             (tracekit::event-digest (nlk:json-object "b" 2 "a" 1)))
      "the event hash is sha256 of the canonical bytes, as Tracekit writes it"))

;;; --- the hook ------------------------------------------------------------------------

(deftest tracekit-cell-a-denied-call-does-not-run ()
  (with-tracekit-signer ((list :decision "deny" :rules (vector "TK-SHELL-RM")))
    (with-cell-stop ((tracekit-start))
      (multiple-value-bind (answer ran) (tracekit-hook (tracekit-arguments "form" "(sh \"rm -rf /\")"))
        (is (stringp answer) "the model reads a refusal")
        (is (search "Tracekit policy" answer) "naming the signer's verdict")
        (is (search "TK-SHELL-RM" answer) "and the rule that blocked it")
        (is (not ran) "the call did not run")
        (is (= 1 (length (tracekit-stub-sent *tracekit-stub* "decide"))) "one decision")
        (is (null (tracekit-stub-sent *tracekit-stub* "approval_consume"))
            "a denied call is never consumed")
        (is (null (tracekit-stub-sent *tracekit-stub* "complete")) "nor completed")))))

(deftest tracekit-cell-an-allowed-call-runs-and-is-recorded ()
  (with-tracekit-signer ((list :decision "allow"))
    (with-cell-stop ((tracekit-start))
      (let ((arguments (tracekit-arguments "form" "(+ 1 2)")))
        (multiple-value-bind (answer ran) (tracekit-hook arguments :next-answer "3")
          (is (equal "3" answer) "the tool's own answer, unchanged")
          (is ran "the call ran")
          (is (= 1 (length (tracekit-stub-sent *tracekit-stub* "register_run")))
              "one run for the process")
          (let ((decide (first (tracekit-stub-sent *tracekit-stub* "decide")))
                (consume (first (tracekit-stub-sent *tracekit-stub* "approval_consume")))
                (complete (first (tracekit-stub-sent *tracekit-stub* "complete"))))
            (is (equal "call-1" (gethash "tool_call_id" decide)) "decided under the call's id")
            (is (equal "eval" (gethash "tool" decide)))
                (is (equal "call-1" (gethash "tool_call_id" consume)) "consumed under the call's id")
            (is (search "eval" (format nil "~a" (gethash "tool" consume))) "and its tool")
            (is (equal "ok" (gethash "status" complete)) "the outcome recorded as ok")
            (is (equal (tracekit::event-digest
                        (nlk:json-object "tool" "eval" "args" arguments))
                       (gethash "args_digest" complete))
                "the digest of the very arguments it decided")
            (is (equal (tracekit::event-digest "3") (gethash "result" complete))
                "a result the policy did not ask for is recorded as its digest")))))))

(deftest tracekit-cell-a-held-call-waits-for-a-person ()
  (with-tracekit-signer ((list :decision "ask" :rules (vector "TK-HOLD")))
    (with-cell-stop ((tracekit-start))
      (multiple-value-bind (answer ran) (tracekit-hook (tracekit-arguments "form" "1"))
        (is (equal "done" answer) "an approved call runs")
        (is ran)
        (is (= 1 (length (tracekit-stub-sent *tracekit-stub* "approval_request"))) "the approval was asked for")
        (is (= 1 (length (tracekit-stub-sent *tracekit-stub* "approval_wait"))) "and waited on")
        (is (= 1 (length (tracekit-stub-sent *tracekit-stub* "approval_consume"))) "then consumed")
        (is (equal (gethash "approval_id_hint"
                            (first (tracekit-stub-sent *tracekit-stub* "approval_consume")))
                   "approval-1")
            "with the approval it waited on")))))

(deftest tracekit-cell-a-rejected-call-does-not-run ()
  (with-tracekit-signer ((list :decision "ask" :rules (vector "TK-HOLD")))
    (with-cell-stop ((tracekit-start))
      (setf (stub-approval-state *tracekit-stub*) "rejected")
      (multiple-value-bind (answer ran) (tracekit-hook (tracekit-arguments "form" "1"))
        (is (search "not approved: rejected" answer) "a rejected call says so")
        (is (not ran) "and does not run")
        (is (null (tracekit-stub-sent *tracekit-stub* "complete"))
            "nor is a call that never ran recorded")))))

(deftest tracekit-cell-fails-closed-when-the-signer-is-gone ()
  (with-cell-stop ((tracekit-start "signer" "http://127.0.0.1:1"))
    (multiple-value-bind (answer ran) (tracekit-hook (tracekit-arguments "form" "1"))
      (is (search "could not decide it" answer) "the refusal says the signer was not there")
      (is (not ran) "and the call does not run"))))

(deftest tracekit-cell-the-command-says-the-run ()
  (with-tracekit-signer ((list :decision "allow"))
    (with-cell-stop ((tracekit-start))
      (tracekit-hook (tracekit-arguments "form" "1"))
      (let ((text (cell-entry "nodecode-tracekit" "tracekit" "")))
        (is (search "Tracekit: signer http://127.0.0.1:" text) "the signer's address")
        (is (search "run run-1: 1 allowed" text) "and what this run decided")))))
