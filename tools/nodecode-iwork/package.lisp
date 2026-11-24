;;;; package.lisp --- the IWORK package: five tools, one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader
;;;; finds START-CELL in it, and nicknamed IWORK so an eval form reads
;;;; (iwork:set-cell ...). The five verbs are tools the model calls; a person
;;;; can call the same functions from eval. Every one answers a string, and a
;;;; refusal is IWORK-ERROR, which the tool wrapper renders.

(defpackage #:nodecode-iwork
  (:use #:cl)
  (:nicknames #:iwork)
  (:export #:start-cell
           #:stop-cell
           #:read-document #:set-cell #:slide-op #:export-document #:backups
           #:iwork-error #:iwork-error-detail))

(in-package #:nodecode-iwork)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and IWORK-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of them, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral iwork :not-running t)
