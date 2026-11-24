;;;; package.lisp --- the WHOOP package: five tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed WHOOP so an eval form reads
;;;; (whoop:daily-summary (nlk:make-json-object)). The five verbs are tools
;;;; the model calls; a person can call the same functions from eval.

(defpackage #:nodecode-whoop
  (:use #:cl)
  (:nicknames #:whoop)
  (:export #:start-cell
           #:stop-cell
           #:daily-summary #:recovery-trend #:sleep-trend #:list-workouts #:get-profile
           #:whoop-error #:whoop-error-detail))

(in-package #:nodecode-whoop)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and WHOOP-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral whoop :not-running t)
