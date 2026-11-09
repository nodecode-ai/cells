;;;; package.lisp --- the NODECODE-DECISIONS package: two tools and their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it.

(defpackage #:nodecode-decisions
  (:use #:cl)
  (:export #:start-cell #:stop-cell))

(in-package #:nodecode-decisions)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; client.lisp reads the section through the settings plist it
;;; derives. FAIL and DECISIONS-ERROR, which a call that cannot answer
;;; signals, and the WITH-DECISIONS-LOCK the job registry is guarded by come
;;; from here too. The key never rides a result: it is read once, at start.
(nlk:define-peripheral decisions :not-running t :lock "nodecode-decisions jobs")

(defparameter +default-endpoint+ "https://api.openai.com/v1/decisions"
  "The endpoint the section points at until the operator names another.")

(defparameter +default-model+ "gpt-6-luna"
  "The model the section asks until the operator names another.")

(defparameter +max-questions+ 64
  "Questions one submission may carry, as the original bound it.")

(defparameter +max-request-bytes+ 262144
  "The largest request the cell sends, in bytes of input and question text.")

(defparameter +max-response-bytes+ 1048576
  "The largest reply the cell reads; past it the call is bounded and says so.")

(defparameter +max-timeout+ 600
  "The longest one submission may take, in seconds, whatever the section says.")

(defparameter +capacity+ 4
  "Submissions running at once; the next one is refused, not queued.")

(defparameter +retention+ 32
  "Finished jobs kept, newest first: the rest are dropped, as the original did.")

(defvar *http* #'nlk:http
  "The one network call the cell makes. A test binds it to a scripted answer.")
