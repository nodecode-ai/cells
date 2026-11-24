;;;; support.lisp --- follow-up test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a FOLLOWUPS-CELL-
;;;; name prefix; RUN-FOLLOWUPS-TESTS runs exactly that slice. Every test
;;;; gets a scratch home, so the board file lands nowhere near the operator's.
;;;;
;;;; Nothing is stubbed and nothing is dialled: the cell's one I/O is a JSON
;;;; file under the home, and the reminder is a string built from it.

(in-package #:nodecode.test)

(define-test-slice "followups" "FOLLOWUPS-CELL-" :start nodecode-followups:start-cell)

(defmacro with-followups ((&rest config) &body body)
  "BODY with the cell started on its defaults, or on the section pairs CONFIG
gives, and stopped on unwind."
  `(with-cell-stop ((followups-start ,@config))
     ,@body))

(defun followup-board-raw ()
  "The board as the cell reads it."
  (nodecode-followups::read-board))

(defun followup-count ()
  "How many items the board holds."
  (length (getf (followup-board-raw) :items)))

(defun followup-add-due (subject &rest keys)
  "A follow-up on SUBJECT that is due now."
  (apply #'nodecode-followups:add :subject subject :why "care" :at "now" keys))
