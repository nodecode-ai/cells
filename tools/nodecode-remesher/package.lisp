;;;; package.lisp --- the REMESHER package: four tools, their condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, nicknamed REMESHER so an eval form reads
;;;; (remesher:job-state "..."). The four verbs are tools the model calls; a
;;;; person can call the same functions from eval.

(defpackage #:nodecode-remesher
  (:use #:cl)
  (:nicknames #:remesher)
  (:export #:start-cell
           #:stop-cell
           #:health #:generate #:job #:assets
           #:remesher-error #:remesher-error-detail))

(in-package #:nodecode-remesher)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and REMESHER-ERROR, which a call that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; four tools answer it as the call's failure rather than as an unhandled
;;; error.
(nlk:define-peripheral remesher :not-running t)
