;;;; nodecode-openrouter-voice.asd --- OpenRouter speech in both directions, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openrouter-voice' section and registers transcribe_audio,
;;;; speak_text and /voice, and puts (help :openrouter-voice) on the manual.
;;;;
;;;; A Nodecode rewrite of esketids/hermes-openrouter-voice (see NOTICE).

(defsystem "nodecode-openrouter-voice"
  :description "OpenRouter voice: transcribe an audio file to text and speak text as an mp3, on a key"
  :license "MIT"
  :version "0.1.0"
  :author "esketids (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openrouter-voice/test"))))

(defsystem "nodecode-openrouter-voice/test"
  :description "OpenRouter voice tests. Registered into the shared nodecode.test registry; RUN-OPENROUTER-VOICE-TESTS filters by the OPENROUTER-VOICE-CELL- name prefix."
  :license "MIT"
  :author "esketids (original), Nodecode (rewrite)"
  :depends-on ("nodecode-openrouter-voice" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openrouter-voice-tests)))
