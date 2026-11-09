;;;; package.lisp --- the CORPUS package: the verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed CORPUS so an eval form reads
;;;; (corpus:search "..."): the same calls the tools make, answering the same
;;;; text, for the operator who would rather type one.
;;;;
;;;; SEARCH is a name COMMON-LISP owns and is shadowed here, so every substring
;;;; test in this cell says CL:SEARCH to be unambiguous.

(defpackage #:nodecode-corpus
  (:use #:cl)
  (:nicknames #:corpus)
  (:shadow #:search)
  (:export
   ;; the verbs, each answering a string
   #:search #:provision #:coverage #:account #:call-tool
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:corpus-error))

(in-package #:nodecode-corpus)

(nlk:define-peripheral corpus :not-running t)
