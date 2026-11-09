;;;; nodecode-deepgram-voice.asd --- Deepgram speech and transcription as
;;;; Nodecode tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `deepgram' section says which key, voice, model and folder every call
;;;; uses. It is a rewrite of jaysonsantos's Deepgram voice plugin for Hermes
;;;; Agent -- see NOTICE.

(defsystem "nodecode-deepgram-voice"
  :description "Deepgram Aura speech and Nova transcription: text becomes audio, a recording becomes text"
  :license "MIT"
  :author "jaysonsantos (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "audio")
               (:file "speech")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-deepgram-voice/test"))))

(defsystem "nodecode-deepgram-voice/test"
  :description "Tests for the deepgram-voice cell; RUN-DEEPGRAM-VOICE-TESTS filters the shared registry by the DEEPGRAM-VOICE- prefix."
  :license "MIT"
  :author "jaysonsantos (original), Nodecode (rewrite)"
  :depends-on ("nodecode-deepgram-voice" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-deepgram-voice-tests)))
