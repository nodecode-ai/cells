;;;; package.lisp --- Staff's names, and the condition and lock it runs under.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-staff
  (:use #:cl)
  (:nicknames #:staff)
  (:export #:staff-error #:staff-error-detail
           #:specialists #:find-specialist #:specialist-system-prompt
           #:now-iso #:new-id #:trim
           #:validate-webhook-url #:redact-url #:post-webhook
           #:write-inbox-handoff #:read-inbox-result
           #:jobs-file #:read-jobs #:list-jobs #:find-job #:update-job))

(in-package #:nodecode-staff)

;; STAFF-ERROR, FAIL, *STAFF*, RUNNING-SETTINGS and WITH-STAFF-LOCK -- the
;; last one the lock a job file's read-modify-write runs under, so two tools
;; of one turn cannot write the board from two stale reads.
(nlk:define-peripheral staff :not-running t :lock "staff")

(defparameter +space-chars+ '(#\Space #\Tab #\Newline #\Return)
  "What TRIM takes off both ends of a piece of text a model or a person wrote.")

(defun trim (text)
  "TEXT as a string with its edges' whitespace taken off; NIL reads as empty."
  (string-trim +space-chars+ (or text "")))
