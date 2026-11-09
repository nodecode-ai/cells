;;;; package.lisp --- the FRESHDESK package: eleven tools, their conditions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed FRESHDESK so an eval form reads
;;;; (freshdesk:list-tickets ...). The eleven verbs are tools the model calls;
;;;; a person can call the same functions from eval.

(defpackage #:nodecode-freshdesk
  (:use #:cl)
  (:nicknames #:freshdesk)
  (:export #:start-cell
           #:stop-cell
           #:list-tickets #:get-ticket #:search-tickets #:list-conversations
           #:get-contact #:download-attachment #:add-private-note
           #:create-ticket #:reply-to-ticket #:add-time-entry #:update-ticket
           #:freshdesk-error #:freshdesk-error-detail
           #:uncertain-write #:uncertain-write-operation #:uncertain-write-created-id
           #:canonical-time-spent))

(in-package #:nodecode-freshdesk)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and FRESHDESK-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral freshdesk :not-running t)

(define-condition uncertain-write (error)
  ((operation :initarg :operation :reader uncertain-write-operation)
   (created-id :initarg :created-id :initform nil :reader uncertain-write-created-id)
   (reason :initarg :reason :initform nil :reader uncertain-write-reason))
  (:report (lambda (condition stream)
             (format stream "Freshdesk ~a~@[ ID ~a~] may have succeeded but its result could not be verified~@[: ~a~]; do not retry blindly"
                     (uncertain-write-operation condition)
                     (uncertain-write-created-id condition)
                     (uncertain-write-reason condition))))
  (:documentation "A write Freshdesk may have accepted: the answer was a 408, a
429, a 5xx or no answer at all. The exact result could not be verified, so the
call names what it tried and the id when it is known, and says not to retry
blindly."))
