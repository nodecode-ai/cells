;;;; package.lisp --- the GATEKEEPER package: the policy verbs, the two tools, the ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed GATEKEEPER so an eval form reads
;;;; (gatekeeper:check ...). Every verb answers a STRING and every refusal is
;;;; GATEKEEPER-ERROR, rendered by the eval snippet as
;;;; "ERROR: GATEKEEPER-ERROR: <detail>".

(defpackage #:nodecode-gatekeeper
  (:use #:cl)
  (:nicknames #:gatekeeper)
  (:export
   ;; the cell
   #:start-cell #:stop-cell #:setting
   ;; verbs, every one a string
   #:check #:consent #:optout #:revoke #:note-reply #:note-outcome #:variables #:decisions
   ;; the one condition
   #:gatekeeper-error))

(in-package #:nodecode-gatekeeper)

(nlk:define-peripheral gatekeeper :not-running t)
