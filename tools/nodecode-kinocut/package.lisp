;;;; package.lisp --- the cell's package, its condition, and the data the
;;;; tools share.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The package is named after the system, as a peripheral's always is: the
;;;; loader finds START-CELL there (NLE:DEFINE-CELL exports it), and every
;;;; contribution is filed under this package's name downcased,
;;;; "nodecode-kinocut", which is this folder's name.

(defpackage #:nodecode-kinocut
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:*kinocut*
           #:kinocut-error #:kinocut-error-detail))

(in-package #:nodecode-kinocut)

(nlk:define-peripheral kinocut :not-running t)

;;; --- what the five tools offer, as data ---------------------------------------

(defparameter +aspects+ '("9:16" "1:1" "4:5" "16:9")
  "The frames kino_reformat makes: a phone, a square post, a portrait feed
card, and the widescreen the source probably already is.")

(defparameter +targets+ '(("9:16" 1080 1920) ("1:1" 1080 1080)
                          ("4:5" 1080 1350) ("16:9" 1920 1080))
  "Each aspect's output frame as (ASPECT WIDTH HEIGHT).")

(defparameter +trim-modes+ '("accurate" "copy")
  "trim's two modes: a re-encode cut on the exact frame, or a stream copy
that can only land on at a keyframe.")

(defparameter +fits+ '("crop" "pad")
  "reformat's two ways to reach a frame: fill it and lose the edges, or keep
everything and add bars.")
