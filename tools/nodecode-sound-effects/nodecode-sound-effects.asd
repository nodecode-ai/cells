;;;; nodecode-sound-effects.asd --- ElevenLabs sound effects as a Nodecode tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `sound-effects' section says which key, model, format and folder every
;;;; call uses. It is a rewrite of NousResearch's ElevenLabs Sound Effects
;;;; plugin for Hermes Agent -- see NOTICE.

(defsystem "nodecode-sound-effects"
  :description "ElevenLabs sound effects: a described sound becomes an audio file, kept in this profile's library"
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "audio")
               (:file "library")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-sound-effects/test"))))

(defsystem "nodecode-sound-effects/test"
  :description "Tests for the sound-effects cell; RUN-SOUND-EFFECTS-TESTS filters the shared registry by the SOUND-EFFECTS- prefix."
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode-sound-effects" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-sound-effects-tests)))
