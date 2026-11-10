;;;; package.lisp --- the package, and the condition every refusal is.
;;;;
;;;; SPDX-License-Identifier: MIT

(defpackage #:nodecode-voicestudio
  (:use #:cl)
  (:nicknames #:voicestudio)
  (:documentation "VoiceStudio's six tools: speak, design a voice, list voices,
render a chaptered audiobook, dub a video, and say whether the server is ready.
The work is HTTP against the VoiceStudio app's own local server."))

(in-package #:nodecode-voicestudio)

(nlk:define-peripheral voicestudio :not-running t)
