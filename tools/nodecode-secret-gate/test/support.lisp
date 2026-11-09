;;;; support.lisp --- the secret-gate test runner and its helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Secret-gate tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic posture) under a SECRET-GATE-CELL- name prefix,
;;;; so RUN-SECRET-GATE-TESTS runs exactly that slice. Nothing here reaches
;;;; outside the image: the scanner reads a string, the tools are called the
;;;; way the engine's body calls them, and the audit advice runs against a
;;;; `next' the test writes.

(in-package #:nodecode.test)

(define-test-slice "secret-gate" "SECRET-GATE-CELL-" :start nodecode-secret-gate:start-cell)

(defun gate-hook ()
  "The cell's advice on :TOOL, or NIL while it is not started."
  (cell-hook "nodecode-secret-gate" :tool))

(defun gate-tool (name request)
  "NAME's registered tool body, called with its decoded contract the way the
engine calls it."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry) request)))

(defun gate-scan (text)
  "secret_scan over TEXT."
  (gate-tool "secret_scan"
             (nodecode-secret-gate:decode-secret-scan-request
              (nlk:json-object "text" text))))

(defun gate-check (text &optional destination)
  "secret_gate_check over TEXT, bound for DESTINATION."
  (gate-tool "secret_gate_check"
             (nodecode-secret-gate:decode-secret-gate-check-request
              (if destination
                  (nlk:json-object "text" text "destination" destination)
                  (nlk:json-object "text" text)))))

(defun gate-receipts-tool (&optional (limit 10))
  "secret_gate_receipts, asking for LIMIT."
  (gate-tool "secret_gate_receipts"
             (nodecode-secret-gate:decode-secret-gate-receipts-request
              (nlk:json-object "limit" limit))))

(defun gate-slash (args)
  "/secret-gate ARGS in a session, answering its text."
  (nodecode-secret-gate:run-slash args "s1"))

(defun gate-audit (text &optional (tool "eval"))
  "TEXT through the audit advice as the engine runs it: => what the call
answered, unchanged."
  (funcall (gate-hook)
           (list :name tool :arguments (nlk:json-object) :call-id "c-1")
           (lambda (op) (declare (ignore op)) text)))

(defun gate-receipts ()
  "Every receipt the cell holds, newest first."
  (nodecode-secret-gate:receipts 50))

(defparameter +leaky+ "AWS_ACCESS_KEY=AKIAIOSFODNN7EXAMPLE"
  "A line carrying a known provider format.")

(defparameter +randomish+ "token: k3Jd9Xk2Lm4Qp7Rt1Vw8Yz3Bc6Nf0Gh5"
  "A line carrying a long high-entropy string and nothing else.")
