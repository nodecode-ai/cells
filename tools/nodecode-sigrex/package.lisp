;;;; package.lisp --- the SIGREX package: the verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed SIGREX so an eval form reads
;;;; (sigrex:strategies): the same call the tool makes, answering the same
;;;; text, for the operator who would rather type one.

(defpackage #:nodecode-sigrex
  (:use #:cl)
  (:nicknames #:sigrex)
  ;; SIGNAL is the verb the platform's own word is, and a name COMMON-LISP
  ;; owns: shadowed here, so a test that means CL:SIGNAL says so.
  (:shadow #:signal)
  (:export
   ;; the verbs, each answering a string
   #:strategies
   #:api
   #:signal
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:sigrex-error))

(in-package #:nodecode-sigrex)

(nlk:define-peripheral sigrex :not-running t)
