;;;; support.lisp --- the slice runner and the one way a test runs a tool call.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A dispatch is a call of the installed :TOOL chain with a NEXT that answers
;;;; the tool's own text, on a thread that reads as one session's turn: nothing
;;;; of the cell is stubbed, so the row /break-status lists and the note a
;;;; mark rewrites into are the ones the cell really makes.

(in-package #:nodecode.test)

(define-test-slice "tool-break" "TOOL-BREAK-CELL-" :recipe "tool-break"
  :start nodecode-tool-break:start-cell)

(defmacro with-tool-break ((&rest pairs) &body body)
  "BODY with the cell started on its defaults -- or on the PAIRS a section
would carry -- and stopped on unwind."
  `(with-cell-stop ((tool-break-start ,@pairs)) ,@body))

(defun tool-break-call (name body &key arguments (session "") (call-id "c1"))
  "One dispatch as the turn thread runs it: the installed :TOOL chain around
BODY, which answers the tool's own text. => the text the model reads, and
BODY sees the call while it is still in flight."
  (let ((nle::*live-turn* (if (plusp (length session))
                               (nle::%make-live-turn :session-id session :turn-id "t1")
                               nil)))
    (funcall (cell-hook "nodecode-tool-break" :tool)
             (list :name name :arguments arguments :call-id call-id)
             (lambda (op) (declare (ignore op)) (funcall body)))))

(defun tool-break-command (name args &optional session)
  "NAME's answer, run with ARGS as the shell runs it, for SESSION."
  (cell-entry "nodecode-tool-break" name args session))

(defun tool-break-slow-arguments (&optional (form "(sh \"make test\")"))
  "The arguments a slow eval carries."
  (nlk:json-object "form" form))
