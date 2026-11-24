;;;; support.lisp --- escalation-governor test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The governor's tests register into the SAME nodecode.test registry (the
;;;; core DEFTEST, with its hermetic machine-state posture) under an
;;;; ESCALATION-GOVERNOR-CELL- name prefix; RUN-ESCALATION-GOVERNOR-TESTS runs
;;;; exactly that slice.

(in-package #:nodecode.test)

(define-test-slice "escalation-governor" "ESCALATION-GOVERNOR-CELL-"
  :start nodecode-escalation-governor:start-cell)

(defvar *notices* '()
  "What the cell said while a test had NLE:NOTICE stubbed, newest first.")

(defun governor-hook (name)
  "The cell's installed function on NAME — a point keyword or the advised
provider call — or NIL when it is not running."
  (cell-hook "nodecode-escalation-governor" name))

(defmacro with-quiet-governor (&body body)
  "Run BODY with the turn and the notice stubbed: the signal engine is the
whole subject here, and nothing it does needs a conversation behind it."
  `(let ((*notices* '()))
     (with-stubbed-fdefinitions ((nle:turn () '(:session-id "s1"))
                                 (nle:notice (text &key level key)
                                   (push (list text level key) *notices*)
                                   nil))
       ,@body)))

(defun governor-call (name arguments &key (answer "ok") failed session)
  "The installed :TOOL hook funcalled by hand on one call whose body answers
ANSWER (an `ERROR: ...' text when FAILED), as the dispatch seam would."
  (declare (ignore session))
  (funcall (governor-hook :tool)
           (list :name name :arguments arguments :call-id "c1")
           (lambda (op)
             (declare (ignore op))
             (if failed (format nil "ERROR: ~a" answer) answer))))

(defun governor-arguments (&rest pairs)
  "A tool call's arguments as the wire decodes them: a JSON object."
  (let ((object (nlk:make-json-object)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun governor-round (&key (turn "t1") (round 1))
  "The installed :ROUND hook funcalled by hand for one round of the session,
as the boundary before a provider round calls it."
  (funcall (governor-hook :round)
           (list :session-id "s1" :turn turn :round round)
           (constantly nil)))

(defun governor-provider-error (&rest args)
  "The installed advice on the provider call, funcalled with a body that
signals, as a refused round does."
  (declare (ignore args))
  (funcall (governor-hook 'nodecode.evolved::call-provider)
           (lambda (&rest ignored)
             (declare (ignore ignored))
             (error "provider refused"))
           :context))

(defun governor-state (&optional (session "s1"))
  "The cell's record of SESSION, or NIL when it never saw one."
  (gethash session nodecode-escalation-governor::*sessions*))
