;;;; package.lisp --- the MYRADIO package: the deck's verbs, the words, the condition.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; One package, nicknamed MYRADIO so an eval form reads (myradio:status).
;;;; Every exported verb answers a string, and every refusal is
;;;; MYRADIO-ERROR, whose detail is the whole report. The settings the cell
;;;; runs on derive from the one `myradio' section declaration in cell.lisp
;;;; (NLE:DEFINE-CELL), read with (SETTING :port).

(defpackage #:nodecode-myradio
  (:use #:cl)
  (:nicknames #:myradio)
  (:export
   ;; verbs, every one a string
   #:status #:library-search #:queue-track #:queue-remove
   #:skip #:fade-next #:start-playout #:make-audio
   #:operations #:run-operation #:card
   ;; data, so a layer can reword it
   #:*primer* #:*radio-help* #:*owner-only*
   ;; the one condition
   #:myradio-error))

(in-package #:nodecode-myradio)

(nlk:define-peripheral myradio :not-running t)
