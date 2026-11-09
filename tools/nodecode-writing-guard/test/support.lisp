;;;; support.lisp --- the writing-guard test runner and its helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic posture) under a WRITING-GUARD name prefix,
;;;; so RUN-WRITING-GUARD-TESTS runs exactly that slice. Nothing here
;;;; dials anything: the rules read a string, and the one seam the tests
;;;; reach for is the cell's own advice on the stream.

(in-package #:nodecode.test)

(define-test-slice "writing-guard" "WRITING-GUARD" :start nodecode-writing-guard:start-cell)

(defun wg-parts (kind &rest initargs)
  "The stream parts NLE::EMIT-STREAM-PART hands an observer for one KIND
part, with the cell running: what one lane fragment becomes."
  (let ((parts '()))
    (apply #'nle::emit-stream-part (lambda (part) (push part parts)) kind initargs)
    (nreverse parts)))

(defun wg-answer (text)
  "TEXT as one whole answer through the stream => the parts a consumer sees."
  (append (wg-parts :stream-start)
          (wg-parts :text-delta :id "s1" :text text)
          (wg-parts :finish)))

(defun wg-text (parts)
  "The answer PARTS carry, concatenated."
  (format nil "~{~a~}"
          (loop for part in parts
                when (eq :text-delta (nle::stream-part-kind part))
                  collect (or (nle::stream-part-text part) ""))))

(defun wg-tool (json)
  "writing_guard_check as the provider's call calls it, on the call's JSON."
  (nle::message-content (execute-wire-call "wg-call" "writing_guard_check" json)))

(defun wg-slash (args)
  "/writing-guard ARGS in a session, answering its text."
  (nodecode-writing-guard:run-slash args "s1"))
