;;;; support.lisp --- the mask test runner and its helpers.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; MASK tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic posture) under a MASK-CELL- name prefix, so
;;;; RUN-MASK-TESTS runs exactly that slice. Nothing here reaches outside the
;;;; image: the detectors read a string, the advice runs against a `next' the
;;;; test writes, and the tool is called the way the engine's body calls it.

(in-package #:nodecode.test)

(define-test-slice "mask" "MASK-CELL-" :start nodecode-mask:start-cell)

(defun mask-wire-hook ()
  "The cell's advice on the fold, or NIL while it is not started."
  (cell-hook "nodecode-mask" 'nodecode.evolved::wire-messages-from-events))

(defun mask-message (role content)
  "A folded provider message in the shape the fold answers with."
  (nlk:json-object "role" role "content" content))

(defun mask-tool-call (text)
  "mask_test through the registered tool, the way the engine's body calls it."
  (let ((entry (nle::find-tool "mask_test")))
    (funcall (third entry)
             (nodecode-mask::decode-mask-test-request (nlk:json-object "text" text)))))

(defun mask-slash (args)
  "/mask ARGS in a session, answering its text."
  (nodecode-mask::run-slash args "s1"))

(defun mask-wire-answer (messages)
  "The cell's advice run over MESSAGES as the fold would answer them:
(values MASKED CLEAN-P)."
  (funcall (mask-wire-hook)
           (lambda (&rest args) (declare (ignore args)) (values messages :clean))))
