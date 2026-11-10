;;;; package.lisp --- the PODCAST package: the three tools and /podcast.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed PODCAST so an eval form reads (podcast:...).
;;;; The settings the cell runs on derive from the one `podcast' section
;;;; declaration in cell.lisp (NLE:DEFINE-CELL), read with (SETTING :PAUSE-MS).

(defpackage #:nodecode-podcast
  (:use #:cl)
  (:nicknames #:podcast)
  (:export
   ;; the three tools the model calls
   #:produce-tool #:voices-tool #:episodes-tool
   ;; the slash command and the manual
   #:podcast-command
   ;; the script, read on its own by the tests and by a layer
   #:parse-script #:script-title #:script-description #:script-hosts #:script-lines
   #:script-chapters #:script-chars
   #:host-name #:host-voice #:host-rate #:host-pitch
   #:line-host #:line-text #:line-chapter
   ;; episode storage
   #:episodes-root #:list-episodes #:episode-folder
   ;; the seams a test stubs: speech synthesis and the voice list
   #:*synth* #:*fetch-voices* #:*synth-attempts*
   ;; the conditions
   #:script-error #:synth-error #:mix-error))

(in-package #:nodecode-podcast)

;;; The condition PODCAST-ERROR, FAIL, *PODCAST* and RUNNING-SETTINGS.
(nlk:define-peripheral podcast :not-running t)
