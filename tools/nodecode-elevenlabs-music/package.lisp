;;;; package.lisp --- the NODECODE-ELEVENLABS-MUSIC package.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, named after the system, which is how the folder loader finds
;;;; START-CELL in it. The tools it registers are not read from here: the
;;;; model calls them by the names the declaration gives, and their bodies are
;;;; internal functions.

(defpackage #:nodecode-elevenlabs-music
  (:use #:cl)
  (:export #:elevenlabs-music-error))

(in-package #:nodecode-elevenlabs-music)

;;; FAIL and ELEVENLABS-MUSIC-ERROR, which every refusal signals, and
;;; *ELEVENLABS-MUSIC* with RUNNING-SETTINGS, which the tools read the section
;;; through, come from here. SETTING is derived from the NLE:DEFINE-CELL
;;; declaration in cell.lisp, which loads last.
(nlk:define-peripheral elevenlabs-music :not-running t)
