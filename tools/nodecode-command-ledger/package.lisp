;;;; package.lisp --- the NODECODE-COMMAND-LEDGER package: the verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed CMDLOG so an eval form reads
;;;; (cmdlog:summary). Every failure the model sees is COMMAND-LEDGER-ERROR,
;;;; which the eval snippet renders as "ERROR: COMMAND-LEDGER-ERROR: <detail>".

(defpackage #:nodecode-command-ledger
  (:use #:cl)
  (:nicknames #:cmdlog)
  (:export
   ;; the verbs, every one a string
   #:summary #:recent #:clear
   ;; the one condition
   #:command-ledger-error))

(in-package #:nodecode-command-ledger)

;;; The condition, the settings variable and RUNNING-SETTINGS come from here;
;;; the section itself and the settings struct are declared in cell.lisp,
;;; which loads last. LEDGER.LISP reads the section through SETTING and
;;; refuses as "not running" while the cell is stopped.
(nlk:define-peripheral command-ledger :not-running t)
