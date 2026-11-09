;;;; package.lisp --- the TALARIA package: four tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed TALARIA so an eval form reads
;;;; (talaria:search-history ...). The four verbs are tools the model calls; a
;;;; person can call the same functions from eval.

(defpackage #:nodecode-talaria
  (:use #:cl)
  (:nicknames #:talaria)
  (:export #:start-cell
           #:stop-cell
           #:capabilities #:search-history #:reply-details #:context-usage
           #:talaria-error #:talaria-error-detail))

(in-package #:nodecode-talaria)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and TALARIA-ERROR, which a request that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral talaria :not-running t)
