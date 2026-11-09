;;;; package.lisp --- the NODECODE-ANTIGRAVITY-AGY package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it. The cell is a provider id, antigravity-agy: its models
;;;; are the ones the operator's own agy CLI is signed in for.

(defpackage #:nodecode-antigravity-agy
  (:use #:cl))

(in-package #:nodecode-antigravity-agy)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the other files read the section through it.
(nlk:define-peripheral antigravity-agy :not-running t)

(defparameter +provider+ "antigravity-agy"
  "The provider id: what /models lists models under, what the catalog row is
keyed by, and what a turn's frozen config names.")

(define-condition agy-error (error)
  ((message :initarg :message :reader agy-message))
  (:report (lambda (condition stream) (write-string (agy-message condition) stream)))
  (:documentation "agy could not answer: no CLI, a refused round, or a timeout."))
