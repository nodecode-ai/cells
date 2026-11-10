;;;; package.lisp --- the TAINT-SENTRY package: one :TOOL hook, two tools,
;;;; one slash command and the manual topic.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it, and nicknamed TAINT-SENTRY so an eval form reads
;;;; (taint-sentry:status-text ...). The verbs a model calls are two tools
;;;; (taint_status, taint_receipts); a person types /taint-sentry.

(defpackage #:nodecode-taint-sentry
  (:use #:cl)
  (:nicknames #:taint-sentry)
  (:export #:start-cell
           #:stop-cell
           #:guard-call
           #:classify-source
           #:strip-invisible
           #:scan-signatures
           #:evaluate
           #:tainted-p
           #:mark-untrusted
           #:snapshot
           #:nearest-tainted-session
           #:describe-sources
           #:sensitive-path-p
           #:instruction-path-p
           #:paths-of
           #:egress-of
           #:reset-store
           #:status-text
           #:receipts-text
           #:run-slash
           #:current-session
           #:taint-sentry-error
           #:taint-sentry-error-detail))

(in-package #:nodecode-taint-sentry)

;;; SETTING is the function DEFINE-CELL defines in cell.lisp, which loads
;;; last; the layers before it read it, so its type is declared here.
(declaim (ftype (function (keyword) t) setting))

;;; FAIL and TAINT-SENTRY-ERROR come from here, as in every cell.
(nlk:define-peripheral taint-sentry :not-running t)
