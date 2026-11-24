;;;; package.lisp --- the WINDOWS-SERVICES package: two verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, which is how the folder loader
;;;; finds START-CELL in it; WINDOWS-SERVICES is a nickname, the slash
;;;; command and the manual topic. SETTING comes from the NLE:DEFINE-CELL
;;;; declaration in cell.lisp, which loads last; the files between read the
;;;; section through it. FAIL and WINDOWS-SERVICES-ERROR come from
;;;; NLK:DEFINE-PERIPHERAL: what a verb signals when the service manager
;;;; cannot be read, and what every verb says while the cell is not running.

(defpackage #:nodecode-windows-services
  (:use #:cl)
  (:nicknames #:windows-services)
  (:export
   ;; verbs, every one a string
   #:table #:detail
   ;; the manual's text, and the words a layer can reword
   #:+primer+ #:+binary-chars+ #:+message-chars+
   ;; the one condition
   #:windows-services-error))

(in-package #:nodecode-windows-services)

(nlk:define-peripheral windows-services :not-running t)
