;;;; support.lisp --- the toolaria test slice and its helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; TOOLARIA- prefix and RUN-TOOLARIA-TESTS runs exactly that slice. Every
;;;; start gets a store under the temporary directory, so no test reads or
;;;; writes the operator's own ~/.nodecode/toolaria, and no test dials
;;;; anything: the cell's only I/O is the filesystem.

(in-package #:nodecode.test)

(defvar *store-counter* 0
  "How many stores this run has made; the number keeps two starts from sharing one.")

(defun fresh-store-path ()
  "A store path no other start of this run uses."
  (format nil "~a/toolaria-cell-test-~d-~d/"
          (uiop:temporary-directory) (get-universal-time) (incf *store-counter*)))

(define-test-slice "toolaria" "TOOLARIA" :start nodecode-toolaria:start-cell
  :defaults (("store_path" (fresh-store-path))
             ("max_result_chars" 600)
             ("head_lines" 3)
             ("tail_lines" 2)
             ("excerpt_max_chars" 400)))

(defun run-tool-call (name result &key (call-id "c1"))
  "Run NAME's call through the cell's :TOOL hook, NEXT answering RESULT, and
answer what the chain answered: what the model would read."
  (funcall (cell-hook "nodecode-toolaria" :tool)
           (list :name name :arguments (nlk:json-object) :call-id call-id)
           (lambda (op) (declare (ignore op)) result)))

(defun sample-text (&optional (lines 120))
  "A result long enough to be rescued, with markers a fetch has to find."
  (with-output-to-string (out)
    (loop for i below lines
          do (format out "line ~d: ~a~%"
                     i
                     (cond ((= i 7) "payment failed: card declined")
                           ((= i 60) "error: gateway timeout on retry")
                           (t "ordinary row of the result"))))))

(defun handle-id (handle)
  "The blob handle HANDLE names, or NIL."
  (let ((at (search "blob=" handle)))
    (and at (subseq handle (+ at 5) (min (length handle) (+ at 17))))))
