;;;; support.lisp --- the minimizer test slice: a stubbed reminder seam, the hook and the command by hand.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing of the cell is stubbed but the one seam it writes through:
;;;; NLK:RECORD-TURN-CONTEXT-PROMPT, where a turn's reminder text would land.
;;;; The :ROUND hook and /minimizer are funcalled as the engine calls them.

(in-package #:nodecode.test)

(define-test-slice "minimizer" "MINIMIZER-CELL-" :start nodecode-minimizer:start-cell)

(defmacro with-minimizer ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS besides the section's own defaults."
  `(with-cell-stop ((minimizer-start ,@pairs))
     ,@body))

(defmacro with-reminders ((collected) &body body)
  "BODY with every reminder the cell records pushed onto COLLECTED, oldest first."
  `(let ((,collected '()))
     (with-stubbed-fdefinition (nlk:record-turn-context-prompt (request-turn &key text)
                                (push text ,collected))
       ,@body)
     (reverse ,collected)))

(defun round-op (&key (session "s1") (turn "t1") (round 1))
  "A model request -- the :ROUND point -- of ROUND in TURN."
  (list :session-id session :turn turn :round round))

(defun record-round (op)
  "The cell's :ROUND hook run by hand, with a NEXT that does nothing."
  (funcall (cell-hook "nodecode-minimizer" :round) op
           (lambda (op) (declare (ignore op)) nil)))

(defun minimizer-command (&optional (args ""))
  "The registered /minimizer, called as a front end with no dialog would."
  (let* ((entry (nle::find-registered-command "minimizer"))
         (run (and entry (nle::slash-command-headless entry))))
    (funcall (or run #'nodecode-minimizer::run-slash) args nil)))
