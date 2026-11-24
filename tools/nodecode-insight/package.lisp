;;;; package.lisp --- the INSIGHT package: one tool, one command, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed INSIGHT so an eval form reads
;;;; (insight:define "idempotent"). The verb is a tool the model calls and a
;;;; command a person types; both reach the same function.

(defpackage #:nodecode-insight
  (:use #:cl)
  (:nicknames #:insight)
  (:export #:start-cell
           #:stop-cell
           #:define #:define-term #:run-slash
           #:insight-error #:insight-error-detail))

(in-package #:nodecode-insight)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and INSIGHT-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tool answers it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral insight :not-running t)
