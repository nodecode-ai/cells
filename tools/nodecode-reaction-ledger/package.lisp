;;;; package.lisp --- the NODECODE-REACTION-LEDGER package: the verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed REACTION-LEDGER so an eval form
;;;; reads (reaction-ledger:summary). Every failure the model sees is
;;;; REACTION-LEDGER-ERROR, which the eval snippet renders as
;;;; "ERROR: REACTION-LEDGER-ERROR: <detail>".

(defpackage #:nodecode-reaction-ledger
  (:use #:cl)
  (:nicknames #:reaction-ledger)
  (:export
   ;; the verbs, every one a string
   #:summary #:recent #:clear
   ;; the manual, so a layer can reword it
   #:+manual+
   ;; the one condition
   #:reaction-ledger-error))

(in-package #:nodecode-reaction-ledger)

;;; The condition, the settings variable and RUNNING-SETTINGS come from here;
;;; the section itself and the settings struct are declared in cell.lisp,
;;; which loads last. LEDGER.LISP reads the section through SETTING and
;;; refuses as "not running" while the cell is stopped.
(nlk:define-peripheral reaction-ledger :not-running t)
