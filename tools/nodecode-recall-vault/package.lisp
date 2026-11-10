;;;; package.lisp --- the RECALL-VAULT package: the rules, the ledger, and the verbs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed RECALL-VAULT, so an eval form reads
;;;; (recall-vault:check 0.4 "someone.testnet"): the same decision the
;;;; recall_vault_check tool makes, for the operator or the agent who would
;;;; rather type one.
;;;;
;;;; Every verb answers a STRING -- the JSON the matching tool answers, or the
;;;; lines /recall-vault prints. A refusal is RECALL-VAULT-ERROR, which the
;;;; eval snippet and a tool result both render as "ERROR: ...".
;;;;
;;;; The settings these run on derive from the one `recall-vault' section
;;;; declaration in cell.lisp, read with (SETTING :STORE-FILE) and the like.

(defpackage #:nodecode-recall-vault
  (:use #:cl)
  (:nicknames #:recall-vault)
  (:export
   ;; the verbs, every one a string
   #:check #:policy #:set-policy #:ledger #:remember
   ;; the rules as data, so a layer can reword them
   #:*primer* #:*default-allowlist* #:*currency*
   ;; the one condition
   #:recall-vault-error))

(in-package #:nodecode-recall-vault)

(nlk:define-peripheral recall-vault :not-running t)
