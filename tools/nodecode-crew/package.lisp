;;;; package.lisp --- the CREW package: the cards, the verbs, the condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed CREW so an eval form reads (crew:verdict "crew-1").
;;;; Every exported verb answers a STRING and every refusal is CREW-ERROR,
;;;; rendered by the eval snippet as "ERROR: CREW-ERROR: <detail>".

(defpackage #:nodecode-crew
  (:use #:cl)
  (:nicknames #:crew)
  (:export
   ;; verbs, every one a string
   #:plan #:contract #:verdict #:progress #:diagnose #:proof-check #:board
   ;; data, so a layer can reword it
   #:+primer+
   ;; the one condition
   #:crew-error))

(in-package #:nodecode-crew)

(nlk:define-peripheral crew :not-running t :lock "crew" :recursive t)
