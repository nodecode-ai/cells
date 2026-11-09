;;;; package.lisp --- the NODECODE-INTERIM-LEDGER package: the verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed INTERIM-LEDGER so an eval form
;;;; reads (interim-ledger:summary). Every failure the model sees is
;;;; INTERIM-LEDGER-ERROR, which the eval snippet renders as
;;;; "ERROR: INTERIM-LEDGER-ERROR: <detail>".

(defpackage #:nodecode-interim-ledger
  (:use #:cl)
  (:nicknames #:interim-ledger)
  (:export
   ;; the verbs, every one a string
   #:summary #:recent #:clear
   ;; the one condition
   #:interim-ledger-error))

(in-package #:nodecode-interim-ledger)

;;; The condition, the settings variable and RUNNING-SETTINGS come from here;
;;; the section itself and the settings struct are declared in cell.lisp,
;;; which loads last. LEDGER.LISP reads the section through SETTING and
;;; refuses as "not running" while the cell is stopped.
(nlk:define-peripheral interim-ledger :not-running t)
