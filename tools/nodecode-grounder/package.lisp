;;;; package.lisp --- the GROUNDER package: one verb, the condition, the primer.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed GROUNDER so an eval form reads
;;;; (grounder:search "..."): the same call the tool makes, answering the same
;;;; text, for the operator who would rather type one.

(defpackage #:nodecode-grounder
  (:use #:cl)
  (:nicknames #:grounder)
  ;; SEARCH is the verb the original's backend answers and a name COMMON-LISP
  ;; owns: shadowed here, so a substring test in this cell says CL:SEARCH.
  (:shadow #:search)
  (:export
   ;; the verb, answering a string
   #:search
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:grounder-error))

(in-package #:nodecode-grounder)

;;; FAIL and GROUNDER-ERROR, which a search that cannot go on signals, and
;;; DEFINE-VERB, whose functions refuse while the cell is not running, come
;;; from here. SETTING comes from the NLE:DEFINE-CELL declaration in
;;; cell.lisp, which loads last; search.lisp reads the section through it.
(nlk:define-peripheral grounder :not-running t)
