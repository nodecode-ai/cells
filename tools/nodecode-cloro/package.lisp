;;;; package.lisp --- the CLORO package: one verb, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CLORO so an eval form reads (cloro:search "..."):
;;;; the same call the tool makes, answering the same text, for the operator
;;;; who would rather type one.

(defpackage #:nodecode-cloro
  (:use #:cl)
  (:nicknames #:cloro)
  ;; SEARCH is the verb this cell gives the operator and a name COMMON-LISP
  ;; owns: shadowed here, so a substring test in this cell says CL:SEARCH to
  ;; be unambiguous.
  (:shadow #:search)
  (:export
   ;; the verb, answering a string
   #:search
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:cloro-error))

(in-package #:nodecode-cloro)

(nlk:define-peripheral cloro :not-running t)
