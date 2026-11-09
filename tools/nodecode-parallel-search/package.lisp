;;;; package.lisp --- the PARALLEL package: two verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PARALLEL so an eval form reads
;;;; (parallel:search "..."): the same two calls the tools make, answering the
;;;; same text, for the operator who would rather type one.
;;;;
;;;; SEARCH is the verb the original's first tool is named for and a name
;;;; COMMON-LISP owns: shadowed here, so every substring test in this cell says
;;;; CL:SEARCH to be unambiguous. FETCH is nobody's.

(defpackage #:nodecode-parallel-search
  (:use #:cl)
  (:nicknames #:parallel)
  (:shadow #:search)
  (:export
   ;; the two verbs, each answering a string
   #:search #:fetch
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:parallel-search-error))

(in-package #:nodecode-parallel-search)

(nlk:define-peripheral parallel-search :not-running t)
