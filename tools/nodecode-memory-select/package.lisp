;;;; package.lisp --- the MEMORY-SELECT package: one verb, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MEMORY-SELECT so an eval form reads
;;;; (memory-select:select "where is the db" '((:id "db" :text "PostgreSQL"))) :
;;;; the same call the tool makes, answering the same selection, for the
;;;; operator who would rather type one.

(defpackage #:nodecode-memory-select
  (:use #:cl)
  (:nicknames #:memory-select)
  (:export
   ;; the verb, answering a SELECTION
   #:select
   #:selection
   #:selection-selected
   #:selection-dropped
   #:selection-total-tokens
   #:selected-memory
   #:selected-memory-id
   #:selected-memory-text
   #:selected-memory-score
   #:selected-memory-tokens
   #:selected-memory-reasons
   #:dropped-memory
   #:dropped-memory-id
   #:dropped-memory-reason
   ;; the tool's own answer
   #:to-json
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:memory-select-error))

(in-package #:nodecode-memory-select)

(nlk:define-peripheral memory-select :not-running t)
