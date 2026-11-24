;;;; package.lisp --- the BY2KB package: the verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed BY2KB so an eval form reads
;;;; (by2kb:search "TiDB vector search") or (by2kb:video "https://...").
;;;; Every exported verb answers a STRING and every refusal is BY2KB-ERROR,
;;;; rendered by the eval snippet as "ERROR: BY2KB-ERROR: <detail>".
;;;;
;;;; SEARCH shadows CL:SEARCH: the verb is what a person types.
;;;;
;;;; The settings this cell runs on derive from the one `by2kb' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :by2kb).

(defpackage #:nodecode-by2kb
  (:use #:cl)
  (:nicknames #:by2kb)
  (:shadow #:search)
  (:export
   ;; verbs, every one a string
   #:search #:select #:video #:next #:submit #:status
   ;; data, so a layer can reword it
   #:*primer* #:*standing* #:*select-hint* #:*pending-hint*
   ;; the one condition
   #:by2kb-error))

(in-package #:nodecode-by2kb)

(nlk:define-peripheral by2kb :not-running t)
