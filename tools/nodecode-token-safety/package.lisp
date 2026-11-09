;;;; package.lisp --- the TOKEN-SAFETY package: two verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed TOKEN-SAFETY so an eval form reads
;;;; (token-safety:report "0x..."): the same GET the tools make, answering the
;;;; same text, for the operator who would rather type one.

(defpackage #:nodecode-token-safety
  (:use #:cl)
  (:nicknames #:token-safety)
  (:export
   ;; the two verbs, each answering a string
   #:report
   #:gate
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:token-safety-error))

(in-package #:nodecode-token-safety)

;;; FAIL, TOKEN-SAFETY-ERROR, *TOKEN-SAFETY* and RUNNING-SETTINGS land here;
;;; DEFINE-VERB, whose functions refuse while the cell is not running, comes
;;; from it too.
(nlk:define-peripheral token-safety :not-running t)
