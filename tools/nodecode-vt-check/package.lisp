;;;; package.lisp --- the vt-check package: four verbs and the word for their failure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed VT-CHECK so the manual reads (vt-check:check-file
;;;; "~/Downloads/setup.exe"). Each verb answers a STRING -- the block the
;;;; model reads -- and every failure is VT-CHECK-ERROR, which eval renders as
;;;; "ERROR: VT-CHECK-ERROR: <detail>".

(defpackage #:nodecode-vt-check
  (:use #:cl)
  (:nicknames #:vt-check)
  (:export
   ;; the verbs, each a string
   #:check-file
   #:check-url
   #:check-ip
   #:status
   ;; the one condition
   #:vt-check-error))

(in-package #:nodecode-vt-check)
