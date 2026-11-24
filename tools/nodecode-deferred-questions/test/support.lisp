;;;; support.lisp --- deferred-question test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic machine-state posture) under a
;;;; DEFERRED-QUESTIONS-CELL- name prefix; RUN-DEFERRED-QUESTIONS-TESTS runs
;;;; exactly that slice. Every test gets a scratch home, so the ledger file
;;;; lands nowhere near the operator's.
;;;;
;;;; Nothing is stubbed and nothing is dialled: the cell's one I/O is a JSON
;;;; file under the home, and what a model request carries is a string built
;;;; from it.

(in-package #:nodecode.test)

(define-test-slice "deferred-questions" "DEFERRED-QUESTIONS-CELL-"
  :start nodecode-deferred-questions:start-cell)

(defmacro with-questions ((&rest config) &body body)
  "BODY with the cell started on its defaults, or on the section pairs CONFIG
gives, and stopped on unwind."
  `(with-cell-stop ((deferred-questions-start ,@config))
     ,@body))

(defun question-ledger ()
  "The ledger as the cell reads it."
  (nodecode-deferred-questions::read-ledger))

(defun question-count ()
  "How many questions the ledger holds."
  (length (getf (question-ledger) :items)))

(defun question-by-id (id)
  "The ledger's question ID."
  (nodecode-deferred-questions::item-by-id (question-ledger) id))
