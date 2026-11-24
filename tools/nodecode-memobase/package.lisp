;;;; package.lisp --- the MEMOBASE package: three verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MEMOBASE so an eval form reads
;;;; (memobase:search "..."): the same call the tool makes, answering the
;;;; same text, for the operator who would rather type one.

(defpackage #:nodecode-memobase
  (:use #:cl)
  (:nicknames #:memobase)
  ;; SEARCH is the verb the original's tool is named for and a name
  ;; COMMON-LISP owns: shadowed here, so a substring test in this cell says
  ;; CL:SEARCH to be unambiguous.
  (:shadow #:search)
  (:export
   ;; the verbs, answering a string
   #:search
   #:profile
   #:remember
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:memobase-error))

(in-package #:nodecode-memobase)

(nlk:define-peripheral memobase :not-running t)
