;;;; package.lisp --- the COMMAND-TRAY package: the verbs, the title, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed COMMAND-TRAY so an eval form reads
;;;; (command-tray:runs). Every export answers a value or a string; every
;;;; refusal the model reads is COMMAND-TRAY-ERROR.
;;;;
;;;; The settings this cell runs on derive from the one `command-tray'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :max-items).

(defpackage #:nodecode-command-tray
  (:use #:cl)
  (:nicknames #:command-tray)
  (:export
   ;; verbs
   #:runs #:clear
   ;; the words, so a layer can reword them
   #:*primer* #:*empty* #:*usage*
   ;; the one condition
   #:command-tray-error))

(in-package #:nodecode-command-tray)

(nlk:define-peripheral command-tray :not-running t :lock "command-tray")
