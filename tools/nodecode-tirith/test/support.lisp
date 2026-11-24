;;;; support.lisp --- the tirith test runner, the stubbed program, and the
;;;; hook's answer for a form.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Tirith tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a TIRITH- name
;;;; prefix; RUN-TIRITH-TESTS runs exactly that slice. The program is never
;;;; run here: *RUN* is the cell's one seam onto it, and WITH-STUBBED-TIRITH
;;;; sets it to a function answering canned verdicts.

(in-package #:nodecode.test)

(define-test-slice "tirith" "TIRITH" :start nodecode-tirith:start-cell)

(defparameter *tirith-answers* nil
  "command text -> (RC OUT ERR TIMED-OUT ERROR) the stubbed program answers.")

(defparameter *tirith-version* nil
  "What the stubbed `tirith --version` answers: (RC OUT ERR TIMED-OUT ERROR).")

(defun tirith-verdict-json (action &optional findings &key approval)
  "A verdict line as tirith writes it; FINDINGS are (RULE SEVERITY TITLE) lists."
  (format nil "{\"action\":~s,\"requires_approval\":~a,\"findings\":[~{~a~^,~}]}"
          action (if approval "true" "false")
          (mapcar (lambda (finding)
                    (format nil "{\"rule_id\":~s,\"severity\":~s,\"title\":~s}"
                            (first finding) (second finding) (third finding)))
                  findings)))

(defun tirith-answer (command rc out &key (err "") timed-out)
  "Make the stubbed program answer OUT with RC for COMMAND."
  (setf (gethash command *tirith-answers*) (list rc out err timed-out nil)))

(defun tirith-missing ()
  "Make the stubbed program not exist."
  (setf *tirith-version* (list nil "" "" nil "no such program: tirith")))

(defun stubbed-run (argv input &key directory timeout)
  "The program, stubbed: --version answers *TIRITH-VERSION*, a check answers
*TIRITH-ANSWERS* for the command on standard input, and an unknown command allows."
  (declare (ignore directory timeout))
  (if (member "--version" argv :test #'equal)
      (values-list *tirith-version*)
      (values-list (or (and *tirith-answers*
                            (gethash (string-trim '(#\Space #\Newline #\Tab #\Return) input)
                                     *tirith-answers*))
                       (list 0 (tirith-verdict-json "allow") "" nil nil)))))

(defmacro with-stubbed-tirith (&body body)
  "BODY with the program stubbed, no probe remembered, and no answer canned."
  `(with-saved-globals (nodecode-tirith::*run* nodecode-tirith::*probe*
                        *tirith-answers* *tirith-version*)
     (setf nodecode-tirith::*run* 'stubbed-run
           nodecode-tirith::*probe* nil
           *tirith-answers* (make-hash-table :test #'equal)
           *tirith-version* (list 0 "tirith 0.5.0" "" nil nil))
     ,@body))

(defun tirith-hook-answer (form)
  "The installed hook's answer for FORM: :ALLOWED when the call runs, or the
refusal string that stood in its place."
  (funcall (cell-hook "nodecode-tirith" :tool)
           (list :name "eval" :arguments (nlk:json-object "form" form) :call-id "c")
           (lambda (op) (declare (ignore op)) :allowed)))

(defun tirith-hook-result (form &optional (result "ran"))
  "The text the hook answers for FORM when the call runs: RESULT plus
whatever the hook appended to it."
  (funcall (cell-hook "nodecode-tirith" :tool)
           (list :name "eval" :arguments (nlk:json-object "form" form) :call-id "c")
           (lambda (op) (declare (ignore op)) result)))
