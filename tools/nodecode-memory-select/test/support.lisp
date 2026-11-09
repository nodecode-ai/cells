;;;; support.lisp --- the test slice: the cell started, its tool called, nothing dialled.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; MEMORY-SELECT-CELL- name prefix; RUN-MEMORY-SELECT-TESTS runs exactly that
;;;; slice. There is no network here at all: the selection is the cell's own,
;;;; so a test that wanted a stub would be a test of the wrong thing.

(in-package #:nodecode.test)

(define-test-slice "memory-select" "MEMORY-SELECT-CELL-" :start nodecode-memory-select:start-cell)

(defmacro with-memory-select ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, stopped on unwind."
  `(with-cell-stop ((memory-select-start ,@pairs))
     ,@body))

(defun memory-select-memory (id text &rest pairs)
  "One candidate memory as a provider sends it: ID and TEXT, then PAIRS."
  (apply #'nlk:make-json-object "id" id "text" text pairs))

(defun memory-select-tool-call (arguments)
  "memory_select called through the wire, as a provider sends it; => its text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" "memory_select" arguments))))

(defun memory-select-dropped (selection id)
  "The reason SELECTION dropped ID, or NIL when it kept it."
  (memory-select:dropped-memory-reason
   (find id (memory-select:selection-dropped selection)
         :key #'memory-select:dropped-memory-id :test #'string=)))

(defun memory-select-kept (selection)
  "The ids SELECTION kept, in the order it kept them."
  (mapcar #'memory-select:selected-memory-id (memory-select:selection-selected selection)))
