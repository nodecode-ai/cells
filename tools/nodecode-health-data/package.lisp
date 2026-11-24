;;;; package.lisp --- the HEALTH-DATA package: three tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed HEALTH-DATA so an eval form reads
;;;; (health-data:sync ...). The three verbs are tools the model calls; a
;;;; person can call the same functions from eval.

(defpackage #:nodecode-health-data
  (:use #:cl)
  (:nicknames #:health-data)
  (:export #:start-cell
           #:stop-cell
           #:sync-now #:query-now #:log-food
           #:health-data-error #:health-data-error-detail))

(in-package #:nodecode-health-data)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and HEALTH-DATA-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral health-data :not-running t)
