;;;; package.lisp --- the BRAVE package: one verb, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed BRAVE so an eval form reads (brave:search "..."):
;;;; the same call the tool makes, answering the same text, for the operator
;;;; who would rather type one.

(defpackage #:nodecode-brave-search
  (:use #:cl)
  (:nicknames #:brave)
  ;; SEARCH is the verb the original's tool is named for and a name
  ;; COMMON-LISP owns: shadowed here, so a substring test in this cell says
  ;; CL:SEARCH to be unambiguous.
  (:shadow #:search)
  (:export
   ;; the verb, answering a string
   #:search
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:brave-search-error))

(in-package #:nodecode-brave-search)

(nlk:define-peripheral brave-search :not-running t)
