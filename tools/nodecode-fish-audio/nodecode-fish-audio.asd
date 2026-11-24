;;;; nodecode-fish-audio.asd --- Fish Audio speech, dictation and voices.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `fish-audio' section says which key, models, voice and folder every call
;;;; uses. It is a rewrite of 100yenadmin's Fish Audio plugin for Hermes Agent
;;;; -- see NOTICE.

(defsystem "nodecode-fish-audio"
  :description "Fish Audio: text to speech, dictation and the voice library, as tools a model calls"
  :license "Apache-2.0"
  :author "100yenadmin (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "api")
               (:file "speech")
               (:file "voices")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fish-audio/test"))))

(defsystem "nodecode-fish-audio/test"
  :description "Tests for the fish-audio cell; RUN-FISH-AUDIO-TESTS filters the shared registry by the FISH-AUDIO- prefix."
  :license "Apache-2.0"
  :author "100yenadmin (original), Nodecode (rewrite)"
  :depends-on ("nodecode-fish-audio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-fish-audio-tests)))
