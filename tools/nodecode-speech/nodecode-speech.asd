;;;; nodecode-speech.asd --- AllModels speech as a Nodecode tool.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/, and the gateway calls START-CELL after recovery; the
;;;; `allmodels-speech' section says which key, models, voice, format and
;;;; folder every call uses. It is a rewrite of allmodels-io's AllModels
;;;; Speech plugin for Hermes Agent -- see NOTICE.

(defsystem "nodecode-speech"
  :description "AllModels speech: one key for text-to-speech, speech-to-text and voice search"
  :license "MIT"
  :author "allmodels-io (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-speech/test"))))

(defsystem "nodecode-speech/test"
  :description "Tests for the speech cell; RUN-SPEECH-TESTS filters the shared registry by the SPEECH- prefix."
  :license "MIT"
  :author "allmodels-io (original), Nodecode (rewrite)"
  :depends-on ("nodecode-speech" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-speech-tests)))
