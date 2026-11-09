;;;; support.lisp --- ledger test runner and the scratch file every test writes to.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core DEFTEST,
;;;; with its hermetic machine-state posture) under an AUX-LEDGER-CELL- name
;;;; prefix; RUN-AUX-LEDGER-TESTS runs exactly that slice.
;;;;
;;;; Nothing of the cell is stubbed: the hooks are called with a NEXT the test
;;;; supplies, so one row is proved without a provider or a network.

(in-package #:nodecode.test)

(define-test-slice "aux-ledger" "AUX-LEDGER-CELL-" :start nodecode-aux-ledger:start-cell)

(defun aux-ledger-scratch-file ()
  "A ledger file of this run's own, its folder made and the file not yet there."
  (let ((file (temp-path "aux-ledger" "jsonl")))
    (ensure-directories-exist file)
    (when (probe-file file) (delete-file file))
    file))

(defun aux-ledger-rows (file)
  "The rows FILE holds now, oldest first."
  (nodecode-aux-ledger::read-rows file))

(defun aux-ledger-write (file &rest rows)
  "Append ROWS to FILE the way the cell writes them, and answer how many it holds."
  (dolist (row rows) (nodecode-aux-ledger::append-row file row 5000))
  (length (aux-ledger-rows file)))

(defun aux-ledger-a-row (&rest arguments)
  "One row, timestamped now, from ARGUMENTS as MAKE-ROW takes them."
  (apply #'nodecode-aux-ledger::make-row :ts (get-universal-time) arguments))

(defun aux-ledger-side-call (next &rest arguments)
  "The cell's own recorder, run with NEXT as the auxiliary call."
  (apply #'nodecode-aux-ledger::observe-side-call next arguments))

(defun aux-ledger-provider-answer (content &optional usage)
  "A NEXT that answers CONTENT and USAGE the way a provider lane does."
  (lambda (&rest arguments)
    (declare (ignore arguments))
    (values content usage)))
