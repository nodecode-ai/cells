;;;; package.lisp --- the GATEWAY-GUARDIAN package: two verbs, one condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed GATEWAY-GUARDIAN so an eval form reads
;;;; (gateway-guardian:status). Every verb answers a string and every refusal
;;;; is GATEWAY-GUARDIAN-ERROR, rendered as "ERROR: GATEWAY-GUARDIAN-ERROR: ...".
;;;;
;;;; The settings the cell runs on derive from the one `gateway-guardian'
;;;; section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :tail-lines).

(defpackage #:nodecode-gateway-guardian
  (:use #:cl)
  (:nicknames #:gateway-guardian)
  (:export
   ;; the verbs, every one a string
   #:status #:recover
   ;; their tool handlers, as the dispatcher calls them
   #:status-tool #:recover-tool
   ;; the one condition
   #:gateway-guardian-error))

(in-package #:nodecode-gateway-guardian)

(nlk:define-peripheral gateway-guardian :not-running t)
