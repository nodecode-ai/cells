;;;; package.lisp --- the GLASSER package: what the model and the operator reach.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed GLASSER so an eval form reads
;;;; (glasser:call "balance"). The seven tools carry the server's own names
;;;; under the cell's prefix: glasser_search, glasser_inspect, glasser_run,
;;;; glasser_runs_get, glasser_runs_list, glasser_runs_stop, glasser_balance.

(defpackage #:nodecode-glasser
  (:use #:cl)
  (:nicknames #:glasser)
  (:export #:call #:glasser-error))

(in-package #:nodecode-glasser)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last; wire.lisp and tools.lisp read the section through it.
;;; FAIL and GLASSER-ERROR, which a refused call signals, and the running
;;; settings, whose absence refuses, come from here too.
(nlk:define-peripheral glasser :not-running t)
