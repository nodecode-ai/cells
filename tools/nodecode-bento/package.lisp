;;;; package.lisp --- the BENTO package: the verbs, the one condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed BENTO, so an eval form reads (bento:stats) -- the
;;;; same call the model's tool makes.

(defpackage #:nodecode-bento
  (:use #:cl)
  (:nicknames #:bento)
  (:export
   ;; the verbs, each answering the text its tool answers
   #:subscriber #:tags #:fields #:broadcasts #:stats
   #:upsert #:tag #:consent #:create-tag #:create-field
   #:track #:purchase #:broadcast #:schedule #:send-email
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:bento-error))

(in-package #:nodecode-bento)

(nlk:define-peripheral bento :not-running t)
