;;;; nodecode-alice-voice.asd --- a Yandex Alice skill's voice requests, as turns.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of bablobanov's hermes-alice-voice (MIT) for Nodecode: the
;;;; private skill's webhook is one gateway route, the injection is the
;;;; kernel's own in-process ingress, and the answer arrives wherever the
;;;; named session speaks. Nothing here dials out.

(defsystem "nodecode-alice-voice"
  :description "A Yandex Alice voice skill's requests, answered as turns in one session"
  :license "MIT"
  :author "bablobanov (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-alice-voice/test"))))

(defsystem "nodecode-alice-voice/test"
  :description "The skill's gates and one utterance's path; registered into the shared nodecode.test registry under ALICE-VOICE-CELL-."
  :license "MIT"
  :depends-on ("nodecode-alice-voice" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-alice-voice-tests)))
