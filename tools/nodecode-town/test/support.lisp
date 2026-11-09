;;;; support.lisp --- the town's slice, and the two seams that reach it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The cell's tests register into the SHARED nodecode.test registry under a
;;;; TOWN-CELL- name prefix and RUN-TOWN-TESTS runs exactly that slice. The
;;;; cell reaches nothing outside itself -- no HTTP, no file, no process -- so
;;;; the only seam is the hook chain: the frame hook is driven by hand through
;;;; CELL-PUBLISH and the tool hook through the chain, as the shipped tree
;;;; already does for every observer.

(in-package #:nodecode.test)

(define-test-slice "town" "TOWN-CELL-"
  :start nodecode-town:start-cell
  :defaults (("salt" "sugar")))

(defmacro with-town ((&rest pairs) &body body)
  "Run BODY with the town started: PAIRS are section members beyond the
slice's defaults; stopped on unwind."
  `(with-cell-stop ((town-start ,@pairs))
     ,@body))

(defun start-a-turn (&optional (session "s1") (turn "t1"))
  "Publish the fact that opens TURN in SESSION through the cell's frame hook."
  (cell-publish "nodecode-town"
                (cell-frame-op "turn.started" :session session :turn turn)))

(defun end-the-turn (&optional (session "s1") (turn "t1") (type "turn.completed"))
  "Publish the fact that ends TURN through the cell's frame hook."
  (cell-publish "nodecode-town"
                (cell-frame-op type :session session :turn turn)))

(defun run-tool-call (name arguments result &key (call-id "c1"))
  "Run NAME's call through the cell's :TOOL hook chain, with NEXT answering
RESULT, and answer what the chain answered."
  (funcall (cell-hook "nodecode-town" :tool)
           (list :name name :arguments arguments :call-id call-id)
           (lambda (op) (declare (ignore op)) result)))

(defun town-of (snapshot &optional (field "residents"))
  "SNAPSHOT's FIELD as a list, the shape the page reads."
  (coerce (nlk:json-value snapshot :array field) 'list))
