;;;; package.lisp --- the APPLE-HEALTH package: seven tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed APPLE-HEALTH so an eval form reads
;;;; (apple-health:daily-summary ...). The verbs are tools the model calls; a
;;;; person can call the same functions from eval.

(defpackage #:nodecode-apple-health
  (:use #:cl)
  (:nicknames #:apple-health)
  (:export #:start-cell
           #:stop-cell
           #:connection-status #:data-inventory #:daily-summary #:weekly-summary
           #:list-records #:list-workouts #:privacy-audit
           #:apple-health-error #:apple-health-error-detail))

(in-package #:nodecode-apple-health)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and APPLE-HEALTH-ERROR, which a read that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral apple-health :not-running t)
