;;;; package.lisp --- the PAYOPS package: two tools, their condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, nicknamed PAYOPS so an eval form
;;;; reads (payops:reconcile "2026-10-01" "2026-10-31"). The tool bodies are
;;;; the exported verbs; a person can call the same functions from eval.

(defpackage #:nodecode-payops
  (:use #:cl)
  (:nicknames #:payops)
  (:export #:start-cell
           #:stop-cell
           #:transactions #:reconcile
           ;; the argument contracts, so an eval form can build one
           #:transactions-query #:reconcile-query
           #:currency-decimals #:format-amount
           #:payops-error #:payops-error-detail))

(in-package #:nodecode-payops)

;;; SETTING comes from the NLE:DEFINE-CELL declaration in cell.lisp, which
;;; loads last. FAIL and PAYOPS-ERROR, which a call that cannot go on
;;; signals, come from here: every refusal starts as one of these, and the
;;; tools answer it as the call's failure rather than as an unhandled error.
(nlk:define-peripheral payops :not-running t)
