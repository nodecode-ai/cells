;;;; package.lisp --- the KEENABLE package: two verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed KEENABLE so an eval form reads
;;;; (keenable:search "..."): the same calls the tools make, answering the
;;;; same text, for the operator who would rather type one.

(defpackage #:nodecode-keenable
  (:use #:cl)
  (:nicknames #:keenable)
  ;; SEARCH is a name COMMON-LISP owns: shadowed here, so a substring test in
  ;; this cell says CL:SEARCH to be unambiguous.
  (:shadow #:search)
  (:export
   ;; the two verbs, each answering a string
   #:search #:fetch
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:keenable-error))

(in-package #:nodecode-keenable)

(nlk:define-peripheral keenable :not-running t)
