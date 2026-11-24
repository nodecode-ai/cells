;;;; nodecode-ttsbuddy.asd --- TTS Buddy speech as a Nodecode tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `ttsbuddy' section says which key, voice, pace and folder every call
;;;; uses. It is a rewrite of ngelik's TTS Buddy plugin for Hermes Agent --
;;;; see NOTICE.

(defsystem "nodecode-ttsbuddy"
  :description "TTS Buddy speech: synthesize an MP3, pick a voice, recover a job"
  :license "MIT"
  :author "ngelik (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ttsbuddy/test"))))

(defsystem "nodecode-ttsbuddy/test"
  :description "Tests for the TTS Buddy cell; RUN-TTSBUDDY-TESTS filters the shared registry by the TTSBUDDY- prefix."
  :license "MIT"
  :author "ngelik (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ttsbuddy" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ttsbuddy-tests)))

