;;;; nodecode-gmi-speech.asd --- GMI Cloud speech as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gmi-speech' section, registers gmi_speech_say,
;;;; gmi_speech_voices and gmi_speech_models, puts /gmi-speech in the slash
;;;; catalog and (help :gmi-speech) on the manual.
;;;;
;;;; A Nodecode rewrite of zkasuran/hermes-gmi-media's gmi-speech plugin --
;;;; see NOTICE.

(defsystem "nodecode-gmi-speech"
  :description "GMI Cloud speech: say text into an audio file, a model's voices, and the model ids"
  :license "Apache-2.0"
  :author "zkasuran (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gmi-speech/test"))))

(defsystem "nodecode-gmi-speech/test"
  :description "GMI Cloud speech tests; RUN-GMI-SPEECH-TESTS filters the shared registry by the GMI-SPEECH-CELL- prefix."
  :license "Apache-2.0"
  :author "zkasuran (original), Nodecode (rewrite)"
  :depends-on ("nodecode-gmi-speech" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gmi-speech-tests)))
