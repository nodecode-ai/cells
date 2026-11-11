;;;; package.lisp --- the SMOL package: one command run in a microVM, and the
;;;; machines this computer holds.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed SMOL so an eval form reads (smol:run "uname -a").
;;;; Every verb answers a STRING, and every refusal is SMOL-ERROR, rendered by
;;;; the eval snippet as "ERROR: SMOL-ERROR: <detail>".

(defpackage #:nodecode-smol
  (:use #:cl)
  (:nicknames #:smol)
  (:export
   ;; the two questions, and each one's answer as text
   #:run #:machines
   ;; the seams a layer or a test rebinds
   #:*run* #:*which*
   ;; the one condition
   #:smol-error #:smol-error-detail))

(in-package #:nodecode-smol)

(nlk:define-peripheral smol :not-running t)
