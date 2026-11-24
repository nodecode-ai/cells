;;;; package.lisp --- the NODECODE-UX3 package: the three verbs the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed UX3 so an eval form reads
;;;; (ux3:evaluate ...). Every refusal the model sees is UX3-ERROR, which the
;;;; eval snippet renders as "ERROR: UX3-ERROR: <detail>".

(defpackage #:nodecode-ux3
  (:use #:cl)
  (:nicknames #:ux3)
  (:export
   ;; the kernel's verbs, every one a plist (GATE-TEXT renders one as prose)
   #:route #:evaluate #:decide #:gate-text #:review-text
   ;; the vocabulary, so a layer can reword it
   #:+verdicts+ #:+tiers+ #:+next-steps+ #:+tier-notes+
   ;; the one condition
   #:ux3-error))

(in-package #:nodecode-ux3)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; the verbs and the tool read the section through it.
(nlk:define-peripheral ux3 :not-running t)
