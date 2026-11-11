;;;; package.lisp --- the sealed package, its one condition and its settings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed SEALED so an eval form reads
;;;; (sealed:value "OPENAI_API_KEY"): every exported verb answers a STRING,
;;;; and every refusal is SEALED-ERROR, which the tool renders as
;;;; "ERROR: SEALED-ERROR: <detail>" and /sealed as "sealed: <detail>".
;;;;
;;;; The settings the cell runs on are NOT declared here: they derive from the
;;;; one `sealed' section in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :store), and *SEALED* below is the variable they land in.

(defpackage #:nodecode-sealed
  (:use #:cl)
  (:nicknames #:sealed)
  (:export
   ;; the verbs; the four the model calls are also tools
   #:init #:set-value #:unset #:names #:value #:status #:rotate
   #:load-secrets #:unload-secrets
   ;; the one condition
   #:sealed-error))

(in-package #:nodecode-sealed)

(nlk:define-peripheral sealed :not-running t)
