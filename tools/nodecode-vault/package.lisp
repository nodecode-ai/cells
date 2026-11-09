;;;; package.lisp --- the vault package, its one condition and its settings.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed VAULT so an eval form reads (vault:secret
;;;; "hv:openai:key"): every exported verb answers a STRING and every refusal
;;;; is VAULT-ERROR, rendered by the tool or the command as
;;;; "ERROR: VAULT-ERROR: <detail>".
;;;;
;;;; The settings the cell runs on are NOT declared here: they derive from the
;;;; one `vault' section declaration in cell.lisp (NLE:DEFINE-CELL), read with
;;;; (SETTING :store), and *VAULT* below is the variable they land in.

(defpackage #:nodecode-vault
  (:use #:cl)
  (:nicknames #:vault)
  (:export
   ;; the verbs; the five the model calls are also tools
   #:setup #:items #:secret #:put #:forget #:mapping #:run
   ;; the one condition
   #:vault-error))

(in-package #:nodecode-vault)

(nlk:define-peripheral vault :not-running t)
