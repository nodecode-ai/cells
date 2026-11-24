;;;; package.lisp --- the NODECODE-TRIPO package, its condition and FAIL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed TRIPO so an eval form reads (tripo:tool-balance ...).
;;;; The settings this cell runs on derive from the one `tripo' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :base-url).

(defpackage #:nodecode-tripo
  (:use #:cl)
  (:nicknames #:tripo)
  (:export
   ;; verbs: the three that read
   #:tool-task #:tool-download #:tool-balance
   ;; the one condition, rendered as "ERROR: tripo: ..."
   #:tripo-error))

(in-package #:nodecode-tripo)

(nlk:define-peripheral tripo :not-running t :lock "tripo")
