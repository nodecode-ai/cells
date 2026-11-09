;;;; nodecode-emotion-chip.asd --- the agent's bracket delivery score, rendered.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A tool cell, not part of the organism core: nothing in src/ names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `emotion-chip' section, registers the two tools, /emotion-chip
;;;; and (help emotion-chip).

(defsystem "nodecode-emotion-chip"
  :description "Emotion Chip: the agent's bracket delivery cues, rendered for Fish Audio or ElevenLabs, or stripped."
  :license "Apache-2.0"
  :author "100yenadmin (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "score")
               (:file "render")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-emotion-chip/test"))))

(defsystem "nodecode-emotion-chip/test"
  :description "Emotion Chip tests. Registered into the shared nodecode.test registry; RUN-EMOTION-CHIP-TESTS filters by the EMOTION-CHIP- name prefix."
  :license "Apache-2.0"
  :depends-on ("nodecode-emotion-chip" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "tests"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-emotion-chip-tests)))
