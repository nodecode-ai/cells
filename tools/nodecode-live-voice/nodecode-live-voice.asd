;;;; nodecode-live-voice.asd --- live voice as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `voice' section says which key, models, voice, commands and folder every
;;;; call uses. It is a rewrite of Synero's Live Voice plugin for Hermes --
;;;; see NOTICE.

(defsystem "nodecode-live-voice"
  :description "Live voice: dictate from the microphone and hear the agent's answers spoken"
  :license "MIT"
  :author "Synero (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "voice")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-live-voice/test"))))

(defsystem "nodecode-live-voice/test"
  :description "Tests for the live-voice cell; RUN-LIVE-VOICE-TESTS filters the shared registry by the LIVE-VOICE- prefix."
  :license "MIT"
  :author "Synero (original), Nodecode (rewrite)"
  :depends-on ("nodecode-live-voice" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-live-voice-tests)))
