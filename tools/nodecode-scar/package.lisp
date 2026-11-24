;;;; package.lisp --- the NODECODE-SCAR package: the verbs and the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed SCAR so an eval form reads (scar:list-scars).
;;;; Every verb answers a string, and every refusal is SCAR-ERROR, rendered
;;;; "ERROR: <detail>" by the verb that catches it.
;;;;
;;;; The settings the cell runs on derive from the one `scar' section in
;;;; cell.lisp (NLE:DEFINE-CELL), read with (SETTING :MAX-SCARS).

(in-package #:cl-user)

(defpackage #:nodecode-scar
  (:use #:cl)
  (:nicknames #:scar)
  (:export
   ;; verbs
   #:check #:list-scars #:deny #:forget
   ;; the manual, so a layer can reword it
   #:+manual+
   ;; the one condition
   #:scar-error))

(in-package #:nodecode-scar)

;; The condition, FAIL, *SCAR*, RUNNING-SETTINGS and DEFINE-VERB: a verb
;; called with nothing started refuses in words a model can act on.
(nlk:define-peripheral scar :not-running t)
