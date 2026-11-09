;;;; support.lisp --- the test slice, and a store of its own for every start.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here reaches outside the image: the store is one file the slice
;;;; names in a temporary directory, so no test reads or writes anything of
;;;; the operator's, and the round test feeds the hook the turn it wants.
;;;; The lifecycle tests below start the whole cell.

(in-package #:nodecode.test)

(defun holographic-test-file ()
  "A facts file of this test's own, which nothing else has written."
  (namestring (uiop:tmpize-pathname
               (uiop:merge-pathnames* "holographic-test.sexp" (uiop:temporary-directory)))))

(define-test-slice "holographic" "HOLOGRAPHIC-CELL-" :start nodecode-holographic:start-cell
  :defaults (("db_path" (holographic-test-file))))

(defun holographic-call (&rest pairs)
  "A decoded FACT-STORE-CALL from PAIRS, as the tool boundary would make one."
  (nodecode-holographic::decode-fact-store-call
   (apply #'nlk:make-json-object pairs)))

(defun holographic-ask (&rest pairs)
  "Run the cell's own fact_store body on PAIRS."
  (nodecode-holographic::fact-store-tool
   (nodecode-holographic::decode-fact-store-call (apply #'nlk:make-json-object pairs))))

(defun holographic-rate (action id)
  "Run the cell's own fact_feedback body."
  (nodecode-holographic::fact-feedback-tool
   (nodecode-holographic::decode-fact-feedback-call
    (nlk:make-json-object "action" action "fact_id" id))))

(defmacro with-holographic ((&rest pairs) &body body)
  "BODY with the cell started on the section PAIRS, its store a fresh file."
  `(with-cell-stop ((holographic-start ,@pairs))
     ,@body))
