;;;; package.lisp --- the KIWI package: one condition, the acronym nobody shares.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed KIWI, so an eval form reads (kiwi:tools). The
;;;; nickname is the cell's slug and nothing else's: a second cell with the
;;;; same nickname cannot load beside this one.
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package its condition KIWI-ERROR, FAIL,
;;;; the settings variable *KIWI* and RUNNING-SETTINGS: every refusal names
;;;; what to fix, and a verb called while the cell is stopped says so.

(defpackage #:nodecode-kiwi
  (:use #:cl)
  (:nicknames #:kiwi)
  (:export
   ;; the refusal every tool raises, as text
   #:kiwi-error
   ;; the names and descriptions of the tools the runtime answers
   #:tools
   #:tools-text
   ;; data, so a layer can reword it
   #:*primer*))

(in-package #:nodecode-kiwi)

(nlk:define-peripheral kiwi :not-running t)
