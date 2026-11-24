;;;; package.lisp --- the IROH package: the condition, the settings, one lock.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed IROH so an eval form reads (iroh:peers). Every
;;;; exported verb answers a string and every refusal is IROH-ERROR, rendered
;;;; by the eval snippet as "ERROR: IROH-ERROR: <detail>".
;;;;
;;;; NLK:DEFINE-PERIPHERAL gives this package IROH-ERROR, FAIL, the settings
;;;; variable *IROH*, RUNNING-SETTINGS (the refusal a verb gives before the
;;;; cell started), DEFINE-VERB and WITH-IROH-LOCK around the peer file.

(defpackage #:nodecode-iroh
  (:use #:cl)
  (:nicknames #:iroh)
  (:export
   ;; verbs, every one a string
   #:status #:peers #:ticket #:pair #:call
   ;; the one condition
   #:iroh-error))

(in-package #:nodecode-iroh)

(nlk:define-peripheral iroh :not-running t :lock "iroh")
