;;;; package.lisp --- the ANKI package: one condition, the words, the client.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed ANKI so an eval form reads (anki:due "Spanish").
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package its condition ANKI-ERROR, FAIL,
;;;; the settings variable *ANKI* and RUNNING-SETTINGS: every refusal names
;;;; what to fix, and a verb called while the cell is stopped says so.

(defpackage #:nodecode-anki
  (:use #:cl)
  (:nicknames #:anki)
  (:export
   ;; the refusal every verb and tool raises
   #:anki-error
   ;; data, so a layer can reword it
   #:*primer*
   ;; one card field as plain text
   #:strip-html))

(in-package #:nodecode-anki)

(nlk:define-peripheral anki :not-running t)
