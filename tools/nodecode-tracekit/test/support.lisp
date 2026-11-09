;;;; support.lisp --- Tracekit test runner and the scripted signer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Tracekit tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a TRACEKIT- name
;;;; prefix; RUN-TRACEKIT-TESTS runs exactly that slice.
;;;;
;;;; The signer is a clack app on an ephemeral loopback port speaking the
;;;; wire the cell speaks: `hello`, `register_run`, `decide`, the approval
;;;; calls, `complete` and `close_run`. Each test scripts the decisions it
;;;; wants and reads back the frames the cell sent, so the assertions are
;;;; about the cell's own behaviour, never a real signer's.

(in-package #:nodecode.test)

(defvar *tracekit-port* nil
  "The stubbed signer's port, bound for the length of one test.")
(defvar *tracekit-stub* nil
  "The stubbed signer itself, so a test can read what the cell sent it.")

(define-test-slice "tracekit" "TRACEKIT" :start nodecode-tracekit:start-cell
  :defaults (("signer" (format nil "http://127.0.0.1:~d" *tracekit-port*))
             ("timeout_seconds" 5)
             ("ask_seconds" 5)))

(defstruct (tracekit-stub (:conc-name stub-))
  (decisions '())                ; plists, one per decide: (:decision :rules :reason :send-result)
  (approval-state "approved")   ; the state an approval_wait answers
  (fail-mode "closed")          ; register_run's fail_modes default
  (frames '()))                  ; (method . frame), newest first

(defun tracekit-stub-body (env)
  "The request body ENV carries, as text."
  (let* ((length (or (getf env :content-length) 0))
         (buffer (make-array length :element-type '(unsigned-byte 8)))
         (stream (getf env :raw-body)))
    (when (and stream (plusp length)) (read-sequence buffer stream))
    (flexi-streams:octets-to-string buffer :external-format :utf-8)))

(defun tracekit-stub-answer (stub method)
  "The answer frame the stub gives METHOD."
  (cond
    ((equal method "hello")
     (nlk:json-object "proto" (vector 15 19) "version" "1.4.2" "pid" 4242))
    ((equal method "register_run")
     (nlk:json-object "run_id" "run-1" "run_token" "token-1" "tenant" "dev"
                      "tenant_attested" t "principal_attested" nil
                      "fail_modes" (nlk:json-object "default" (stub-fail-mode stub))))
    ((equal method "decide")
     (let ((script (or (pop (stub-decisions stub)) (list :decision "allow"))))
       (nlk:json-object "decision" (getf script :decision)
                        "decision_id" "decision-1"
                        "rule_ids" (or (getf script :rules) #())
                        "reason" (getf script :reason)
                        "run_seq" 1
                        "send_result" (getf script :send-result))))
    ((equal method "approval_request")
     (nlk:json-object "approval_id" "approval-1" "state" "requested"))
    ((equal method "approval_wait")
     (nlk:json-object "approval_id" "approval-1" "state" (stub-approval-state stub)))
    ((equal method "approval_abandon")
     (nlk:json-object "approval_id" "approval-1" "state" "expired"))
    ((equal method "approval_consume")
     (nlk:json-object "ok" t "rule_ids" #()))
    ((equal method "complete")
     (nlk:json-object "run_seq" 1))
    ((equal method "close_run")
     (nlk:json-object "run_seq" 1))
    (t (nlk:json-object "error" (nlk:json-object "code" "invalid_request"
                                                  "message" (format nil "no method ~a" method))))))

(defun tracekit-stub-app (stub)
  "A clack app answering the signer's frames from STUB, recording each one."
  (lambda (env)
    (if (not (eq :post (getf env :request-method)))
        (list 405 '(:content-type "text/plain") '("post only"))
        (let* ((frame (handler-case (shasht:read-json (tracekit-stub-body env))
                        (error () (nlk:json-object))))
               (method (gethash "method" frame)))
          (push (cons method frame) (stub-frames stub))
          (list 200 '(:content-type "application/json")
                (list (tracekit::json-text (tracekit-stub-answer stub method))))))))

(defun tracekit-stub-sent (stub method)
  "The frames the cell sent METHOD, oldest first."
  (loop for (sent . frame) in (reverse (stub-frames stub))
        when (equal sent method) collect frame))

(defmacro with-tracekit-signer ((&rest decisions) &body body)
  "BODY with a scripted signer up on a loopback port: DECISIONS, one plist per
decide call (:decision \"allow\"|\"ask\"|\"deny\", :rules a vector of ids,
:send-result); every further decide is an allow."
  `(let ((stub (make-tracekit-stub
               :decisions (list ,@(if decisions decisions '((list :decision "allow")))))))
     (multiple-value-bind (handler port) (nle:serve-local (tracekit-stub-app stub))
       (unwind-protect (let ((*tracekit-port* port) (*tracekit-stub* stub)) ,@body)
         (when handler (nle:stop-clack-handler handler))))))

(defun tracekit-arguments (&rest pairs)
  "A tool call's arguments as the dispatch seam hands the hook: PAIRS as JSON."
  (apply #'nlk:make-json-object pairs))

(defun tracekit-hook (arguments &key (next-answer "done") (name "eval") (call-id "call-1"))
  "Run one call through the installed :TOOL hook.
=> (values the hook's answer, whether next ran)."
  (let ((ran nil))
    (values (funcall (cell-hook "nodecode-tracekit" :tool)
                     (list :name name :arguments arguments :call-id call-id)
                     (lambda (op) (declare (ignore op)) (setf ran t) next-answer))
            ran)))
