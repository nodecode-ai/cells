;;;; nodecode-kokoro-tts.asd --- Kokoro, a local voice, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `kokoro' section, registers kokoro_speak and kokoro_voices,
;;;; puts /kokoro in the slash catalog and (help :kokoro) on the manual.
;;;;
;;;; A Nodecode rewrite of brunosouzas/hermes-kokoro-tts (see NOTICE).

(defsystem "nodecode-kokoro-tts"
  :description "Kokoro: local text-to-speech through a server on loopback, one voice per language"
  :license "MIT"
  :version "0.1.0"
  :author "brunosouzas (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-kokoro-tts/test"))))

(defsystem "nodecode-kokoro-tts/test"
  :description "Kokoro tests. Registered into the shared nodecode.test registry; RUN-KOKORO-TTS-TESTS filters by the KOKORO-TTS-CELL- name prefix."
  :license "MIT"
  :author "brunosouzas (original), Nodecode (rewrite)"
  :depends-on ("nodecode-kokoro-tts" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-kokoro-tts-tests)))
