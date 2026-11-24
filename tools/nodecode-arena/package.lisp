;;;; package.lisp --- the NODECODE-ARENA package: the tournament state machine.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed ARENA so an eval form reads
;;;; (arena:run "plan" :agents 16). Every failure the model sees is
;;;; ARENA-ERROR: a missing run, a bad phase, a match that is not there.
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives the package its condition, FAIL, the
;;;; settings variable *ARENA* and RUNNING-SETTINGS; the declaration that
;;;; fills them is in cell.lisp, which loads last.

(defpackage #:nodecode-arena
  (:use #:cl)
  (:nicknames #:arena)
  (:export
   ;; model-facing vocabulary (see the manual in cell.lisp)
   #:run #:plan-totals #:bracket-sizes
   ;; the one condition
   #:arena-error))

(in-package #:nodecode-arena)

(nlk:define-peripheral arena :not-running t)
