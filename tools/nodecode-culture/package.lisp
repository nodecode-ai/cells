;;;; package.lisp --- the CULTURE package: the three verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CULTURE so an eval form reads
;;;; (culture:brief "Japan"): the same answers the three tools give, for the
;;;; operator who would rather type one.
;;;;
;;;; The settings this cell runs on derive from the one `culture' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :MAX-GAPS).

(defpackage #:nodecode-culture
  (:use #:cl)
  (:nicknames #:culture)
  (:export
   ;; the verbs, each answering a string
   #:brief #:gap-analysis #:countries
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:culture-error))

(in-package #:nodecode-culture)

(nlk:define-peripheral culture :not-running t)
