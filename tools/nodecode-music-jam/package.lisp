;;;; package.lisp --- the MUSIC-JAM package: the band's data, engine, page and tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed MUSIC-JAM, so a verb reads (music-jam:start ...) in
;;;; an eval form. Every refusal is MUSIC-JAM-ERROR, rendered by the eval
;;;; snippet as "ERROR: MUSIC-JAM-ERROR: <detail>".

(defpackage #:nodecode-music-jam
  (:use #:cl)
  (:nicknames #:music-jam)
  (:export
   ;; verbs
   #:start #:inspect #:stop #:direct #:configure #:schedule #:undo #:save #:restore #:export-midi
   ;; the page, and where it answers
   #:page-url
   ;; data, so a layer can reword it
   #:*roles*
   ;; the one condition
   #:music-jam-error))

(in-package #:nodecode-music-jam)

(nlk:define-peripheral music-jam :not-running t :lock "music-jam")

(defvar *port* 0
  "The loopback port the page is served on, as the section set it at start.")
