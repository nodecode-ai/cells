;;;; support.lisp --- the pii test runner and its helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; PII tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic posture) under a PII-CELL- name prefix, so
;;;; RUN-PII-TESTS runs exactly that slice. Nothing here reaches outside the
;;;; image: the detectors read a string, the advice runs against a `next' the
;;;; test writes, and the tool is called the way the engine's body calls it.

(in-package #:nodecode.test)

(define-test-slice "pii" "PII-CELL-" :start nodecode-pii:start-cell)

(defun pii-wire-hook ()
  "The cell's advice on the fold, or NIL while it is not started."
  (cell-hook "nodecode-pii" 'nodecode.evolved::wire-messages-from-events))

(defun pii-message (role content)
  "A folded provider message in the shape the fold answers with."
  (nlk:json-object "role" role "content" content))

(defun pii-tool-call (text &optional terms)
  "redact_text through the registered tool, the way the engine's body calls it."
  (let ((entry (nle::find-tool "redact_text")))
    (funcall (third entry)
             (nodecode-pii::decode-redact-text-request
              (if terms
                  (nlk:json-object "text" text "terms" terms)
                  (nlk:json-object "text" text))))))

(defun pii-slash (args)
  "/pii ARGS in a session, answering its text."
  (nodecode-pii::run-slash args "s1"))

(defun pii-wire-answer (messages)
  "The cell's advice run over MESSAGES as the fold would answer them:
(values REDACTED CLEAN-P)."
  (funcall (pii-wire-hook)
           (lambda (&rest args) (declare (ignore args)) (values messages :clean))))
