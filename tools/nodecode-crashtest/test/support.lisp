;;;; support.lisp --- the slice runner, and the three seams driven as the turn runs them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A call here is the installed :TOOL chain around a body, on a thread that
;;;; reads as one session's turn; a turn start is the installed :ROUND chain at
;;;; round one; the reply is the advice on the assistant-message record with a
;;;; stub NEXT, since the durable record needs a store. Nothing of the cell is
;;;; stubbed: the faults, the rating and the scorecard are the cell's own.

(in-package #:nodecode.test)

(define-test-slice "crashtest" "CRASHTEST-CELL-" :recipe "crashtest"
  :start nodecode-crashtest:start-cell)

(defmacro with-crashtest ((&rest pairs) &body body)
  "BODY with the cell started on its defaults -- or on the PAIRS a section
would carry -- and stopped on unwind."
  `(with-cell-stop ((crashtest-start ,@pairs)) ,@body))

(defun crashtest-in-turn (session body)
  "BODY with the turn thread reading as SESSION's turn."
  (let ((nle::*live-turn* (if (plusp (length session))
                               (nle::%make-live-turn :session-id session :turn-id "t1")
                               nil)))
    (funcall body)))

(defun crashtest-turn (session &key (round 1))
  "One :ROUND dispatch, as AGENT-LOOP makes it."
  (funcall (cell-hook "nodecode-crashtest" :round)
           (list :session-id session :turn nil :round round)
           (constantly nil)))

(defun crashtest-call (name body &key arguments (session "") (call-id "c1"))
  "One :TOOL dispatch => (values TEXT SETTLE): the text the model reads, and
BODY answers the tool's own text."
  (crashtest-in-turn session
                      (lambda ()
                        (funcall (cell-hook "nodecode-crashtest" :tool)
                                 (list :name name :arguments arguments :call-id call-id)
                                 (lambda (op) (declare (ignore op)) (funcall body))))))

(defun crashtest-reply (session text)
  "The turn's last assistant message through the record advice => the message."
  (crashtest-in-turn session
                      (lambda ()
                        (let ((message (nlk:json-object "role" "assistant" "content" text)))
                          (funcall (cell-hook "nodecode-crashtest" 'nlk:record-assistant-message)
                                   (lambda (&rest arguments)
                                     (declare (ignore arguments))
                                     message)
                                   nil message)
                          message))))

(defun crashtest-content (message)
  "MESSAGE's content, where the scorecard rides."
  (gethash "content" message))

(defun crashtest-command (name args &optional session)
  "NAME's answer, run with ARGS as the shell runs it, for SESSION."
  (cell-entry "nodecode-crashtest" name args session))

(defun crashtest-arguments (form)
  "The arguments an EVAL call carries."
  (nlk:json-object "form" form))

(defun crashtest-read-arguments (path)
  "The arguments a read-only call carries: a path nothing writes."
  (nlk:json-object "path" path))
