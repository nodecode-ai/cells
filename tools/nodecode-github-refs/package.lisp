;;;; package.lisp --- the GITHUB-REFS package: one verb, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed GH-REF so an eval form reads
;;;; (gh-ref:reference "owner/repo#123"): the same call the tool makes,
;;;; answering the same block, for the operator who would rather type one.

(defpackage #:nodecode-github-refs
  (:use #:cl)
  (:nicknames #:gh-ref)
  (:export
   ;; the verb, answering a string
   #:reference
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:github-refs-error))

(in-package #:nodecode-github-refs)

(nlk:define-peripheral github-refs :not-running t)
