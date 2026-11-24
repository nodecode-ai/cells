;;;; package.lisp --- the PATSNAP package: the verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PATSNAP, so an eval form reads
;;;; (patsnap:search-patent :semantic-query "..."): the same calls the tools
;;;; make, and the same text they answer, for the operator who would rather
;;;; type one.
;;;;
;;;; Every verb answers a STRING, and every refusal is PATSNAP-ERROR, which a
;;;; tool result renders as "ERROR: ..." and (help :patsnap) explains.
;;;;
;;;; The settings this cell runs on derive from the one `patsnap' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :ENDPOINT).

(defpackage #:nodecode-patsnap
  (:use #:cl)
  (:nicknames #:patsnap)
  (:export
   ;; the verbs, every one a string
   #:search-patent #:search-literature #:build-boolean-query
   #:describe-capability #:standard-assignee #:corptree
   #:fetch-patent #:patent-lifecycle #:fetch-literature
   #:tools
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:patsnap-error))

(in-package #:nodecode-patsnap)

(nlk:define-peripheral patsnap :not-running t)
