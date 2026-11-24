;;;; support.lisp --- the ja-writing-guard test runner and its helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the SAME nodecode.test registry (the core
;;;; DEFTEST, with its hermetic posture) under a JA-WRITING-GUARD name
;;;; prefix, so RUN-JA-WRITING-GUARD-TESTS runs exactly that slice.
;;;; Nothing here dials anything: the checker reads a string.

(in-package #:nodecode.test)

(define-test-slice "ja-writing-guard" "JA-WRITING-GUARD" :start nodecode-ja-writing-guard:start-cell)

(defun jg-parts (kind &rest initargs)
  "The stream parts NLE::EMIT-STREAM-PART hands an observer for one KIND part."
  (let ((parts '()))
    (apply #'nle::emit-stream-part (lambda (part) (push part parts)) kind initargs)
    (nreverse parts)))

(defun jg-answer (text)
  "TEXT as one whole answer through the stream => the parts a consumer sees."
  (append (jg-parts :stream-start)
          (jg-parts :text-delta :id "s1" :text text)
          (jg-parts :finish)))

(defun jg-text (parts)
  "The answer PARTS carry, concatenated."
  (format nil "~{~a~}"
          (loop for part in parts
                when (eq :text-delta (nle::stream-part-kind part))
                  collect (or (nle::stream-part-text part) ""))))

(defun jg-report (text &key (surface "chat"))
  "The report on TEXT, as the checker answers it."
  (nodecode-ja-writing-guard:check-text text :surface surface))

(defun jg-shown (text &key (surface "chat"))
  "TEXT checked and formatted, as the tool and the command show it."
  (nodecode-ja-writing-guard:format-report (jg-report text :surface surface)))

(defun jg-tool (json)
  "ja_writing_check as the provider's call calls it, on the call's JSON."
  (nle::message-content (execute-wire-call "jg-call" "ja_writing_check" json)))

(defun jg-slash (args)
  "/ja-writing-guard ARGS in a session, answering its text."
  (nodecode-ja-writing-guard:run-slash args "s1"))
