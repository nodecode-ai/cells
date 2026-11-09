;;;; package.lisp --- the NODECODE-TABULAR package: table profiling, splitting and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed TABULAR so an eval form reads
;;;; (tabular:profile-file "data.csv"). Every failure the model sees is
;;;; TABULAR-ERROR, whose detail is the whole report.

(defpackage #:nodecode-tabular
  (:use #:cl)
  (:nicknames #:tabular)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:profile-file #:split-file #:save-profile
   ;; the one condition
   #:tabular-error))

(in-package #:nodecode-tabular)

;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;; loads last; table.lisp and profile.lisp read the section through it.
(nlk:define-peripheral tabular :not-running t)
