;;;; support.lisp --- the check-in test slice: a fixed clock, a clean state, a stubbed ingress.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What reaches outside this image is NLE:SUBMIT, the one ingress a check-in
;;;; goes through, and the clock: WITH-CHECKIN stubs both and points the state
;;;; at a file of its own under the scratch home, so no test dials, no test
;;;; writes the operator's state, and no test depends on the hour it runs at.

(in-package #:nodecode.test)

(define-test-slice "checkin" "CHECKIN-CELL-" :start nodecode-checkin:start-cell
  :defaults (("session" "chat-1")))

(defvar *now* nil
  "The universal time the cell reads while a test runs.")

(defvar *submitted* '()
  "((SESSION . PROMPT) ...) the stubbed ingress received, newest first.")

(defvar *noted* '()
  "Every context prompt the stubbed RECORD-TURN-CONTEXT-PROMPT was handed.")

(defun at-hour (hour &key (day-offset 0))
  "A universal time whose UTC hour is HOUR, so a window test is the same
every run."
  (let ((midnight (- (floor (get-universal-time) 86400) (* day-offset 86400))))
    (+ (* midnight 86400) (* hour 3600) 600)))

(defun checkin-state-path ()
  "The state file a test run keeps, under the scratch home."
  (nlk:home "checkins/test-state.sexp"))

(defmacro with-checkin ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS (session chat-1 when PAIRS
leave it out), its clock at *NOW*, its state in a file of its own, and every
prompt it puts in recorded in *SUBMITTED* instead of admitted."
  `(let ((*now* (or *now* (at-hour 12)))
         (*submitted* '())
         (*noted* '()))
     (with-saved-globals (nodecode-checkin::*state* nodecode-checkin::*live*
                          nodecode-checkin::*our-turn* nodecode-checkin::*seen-turn*)
       (let ((nodecode-checkin::*state-file* (checkin-state-path))
             (nodecode-checkin::*clock* (lambda () *now*)))
         (nlk:with-cleanup ((ignore-errors (delete-file (checkin-state-path))))
           (with-stubbed-fdefinition (nle:submit (session prompt &key &allow-other-keys)
                                      (push (cons session prompt) *submitted*)
                                      :started)
             (with-stubbed-fdefinition (nlk:record-turn-context-prompt (turn &key text)
                                        (declare (ignore turn))
                                        (push text *noted*))
               (with-cell-stop ((checkin-start ,@pairs))
                 ,@body))))))))

(defun prompts ()
  "The prompts the ingress received, oldest first."
  (mapcar #'cdr (reverse *submitted*)))

(defun frame (turn &key (session "chat-1"))
  "A live frame OP of SESSION in TURN."
  (list :session-id session :kind "item_delta"
        :payload (nlk:make-json-object "turn_id" turn)))

(defun checkin-tool (&rest pairs)
  "The checkin tool as the engine calls it: the declared contract decoded,
then the body."
  (let ((entry (nle::find-tool "checkin")))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun tool-answer (&rest pairs)
  "The checkin tool's answer, decoded."
  (nlk:decode-json (apply #'checkin-tool pairs)))
