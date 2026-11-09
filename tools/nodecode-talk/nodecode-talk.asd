;;;; nodecode-talk.asd --- voice out and voice in, through OpenAI's audio API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it at
;;;; boot when its directory sits under the cells folder; the gateway calls
;;;; START-CELL after recovery, which reads the `talk' section, registers
;;;; talk_speak, talk_transcribe and talk_listen, the /talk command and the
;;;; manual topic.
;;;;
;;;; Rewritten from TheSmokeDev's hermes-talk; see NOTICE. Every dependency
;;;; rides with nodecode: nlk:http dials the API, uiop runs the player and
;;;; the recorder.

(defsystem "nodecode-talk"
  :description "Speak text aloud, transcribe an audio file, or record and transcribe a question"
  :author "TheSmokeDev (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-talk/test"))))

(defsystem "nodecode-talk/test"
  :description "Talk tests. Registered into the shared nodecode.test registry; RUN-TALK-TESTS filters by the TALK-CELL- name prefix."
  :author "TheSmokeDev (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-talk" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-talk-tests)))
