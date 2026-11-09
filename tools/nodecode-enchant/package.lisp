;;;; package.lisp --- the ENCHANT package: three tools, one command, the review.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed ENCHANT so an eval form reads
;;;; (enchant:propose "..."). The verbs a person reaches from eval are the
;;;; ones the tools and the command run.

(defpackage #:nodecode-enchant
  (:use #:cl)
  (:nicknames #:enchant)
  (:export
   ;; the cell
   #:start-cell #:stop-cell
   ;; the review: a pure function of the two texts
   #:tokens #:change-groups #:compose #:group-line
   ;; the five enhancers
   #:+enhancers+ #:enhancer-names #:enhancer-body #:instruction
   ;; the verbs
   #:propose #:accept #:revise
   ;; the one condition
   #:enchant-error #:enchant-error-detail))

(in-package #:nodecode-enchant)

;;; The condition, FAIL, *ENCHANT*, RUNNING-SETTINGS and WITH-ENCHANT-LOCK
;;; come from here; SETTING comes from the NLE:DEFINE-CELL declaration in
;;; cell.lisp, which loads last. The lock guards the proposals standing and
;;; the revisions kept, both keyed by the draft.
(nlk:define-peripheral enchant :not-running t :lock "enchant drafts")
