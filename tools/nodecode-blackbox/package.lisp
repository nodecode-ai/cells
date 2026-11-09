;;;; package.lisp --- the BLACKBOX package: two verbs, the rules, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed BLACKBOX so an eval form reads (blackbox:scan ...).
;;;; Every exported verb answers a STRING and every refusal is BLACKBOX-ERROR,
;;;; rendered by the eval snippet as "ERROR: BLACKBOX-ERROR: <detail>".

(defpackage #:nodecode-blackbox
  (:use #:cl)
  (:nicknames #:blackbox)
  (:export
   ;; verbs, every one a string
   #:scan #:check
   ;; the rules the cell reads, for a layer that wants to add to them
   #:*injection-rules* #:*secret-rules* #:*shell-rules*
   ;; the one condition
   #:blackbox-error))

(in-package #:nodecode-blackbox)

(nlk:define-peripheral blackbox :not-running t :lock "blackbox")
