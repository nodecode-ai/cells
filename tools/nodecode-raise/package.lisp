;;;; package.lisp --- the RAISE package: the desk's verbs, the one refusal.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-raise
  (:use #:cl)
  (:nicknames #:raise)
  (:export
   ;; the cell itself
   #:start-cell #:stop-cell
   ;; verbs a layer calls through eval; each answers a string
   #:focus #:can-draft #:lint-draft #:brief #:status-text
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:raise-error))

(in-package #:nodecode-raise)

(nlk:define-peripheral raise :not-running t)
