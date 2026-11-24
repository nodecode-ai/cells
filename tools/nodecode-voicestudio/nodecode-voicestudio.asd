;;;; nodecode-voicestudio.asd --- VoiceStudio's sidecar API, as six tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery. It
;;;; is a rewrite of aska-digital's VoiceStudio plugin for Hermes Agent -- see
;;;; NOTICE.

(defsystem "nodecode-voicestudio"
  :description "VoiceStudio: speak, design voices, render chaptered audiobooks and dub videos through a local VoiceStudio server"
  :license "MIT"
  :author "aska-digital (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-voicestudio/test"))))

(defsystem "nodecode-voicestudio/test"
  :description "Tests for the voicestudio cell; RUN-VOICESTUDIO-TESTS filters the shared registry by the VOICESTUDIO- prefix."
  :license "MIT"
  :author "aska-digital (original), Nodecode (rewrite)"
  :depends-on ("nodecode-voicestudio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-voicestudio-tests)))
