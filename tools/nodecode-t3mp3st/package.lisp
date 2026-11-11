;;;; package.lisp --- the t3mp3st package: five verbs and the word for their failure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed T3MP3ST so the manual reads (t3mp3st:status).
;;;; Each verb answers a STRING -- the block the model reads -- and every
;;;; refusal is T3MP3ST-ERROR, whose detail is the whole report.

(defpackage #:nodecode-t3mp3st
  (:use #:cl)
  (:nicknames #:t3mp3st)
  (:export
   ;; the verbs, each a string
   #:status
   #:launch
   #:control
   #:findings
   #:approvals
   ;; the one condition
   #:t3mp3st-error))

(in-package #:nodecode-t3mp3st)
