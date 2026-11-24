;;;; package.lisp --- the SPOTIFY package: seven verbs, the manual, the condition.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed SPOTIFY so a call reads (spotify:spotify-search ...).
;;;; Every tool body answers a STRING: the JSON of what Spotify said, or a
;;;; failure text beginning "ERROR: ", which is how the engine records a call
;;;; that did not do what it was asked.

(defpackage #:nodecode-spotify
  (:use #:cl)
  (:nicknames #:spotify)
  (:export
   ;; the seven tools, each (ARGS) => a string
   #:spotify-playback #:spotify-devices #:spotify-queue #:spotify-search
   #:spotify-playlists #:spotify-albums #:spotify-library
   ;; the command's verbs
   #:login #:status #:logout
   ;; what a test and a layer may rebind
   #:*auth-file* #:*now*
   ;; the one condition
   #:spotify-error))

(in-package #:nodecode-spotify)

(nlk:define-peripheral spotify :not-running t)
