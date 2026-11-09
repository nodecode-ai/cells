;;;; package.lisp --- the NODECODE-STORM package and its one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed STORM so an eval form reads
;;;; (storm:panel ...). Every refusal the model sees is STORM-ERROR.

(defpackage #:nodecode-storm
  (:use #:cl)
  (:nicknames #:storm)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:panel
   ;; the one condition: the cell is not running, or a round cannot start
   #:storm-error))

(in-package #:nodecode-storm)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; every function below reads the section through it.
(nlk:define-peripheral storm :not-running t)
