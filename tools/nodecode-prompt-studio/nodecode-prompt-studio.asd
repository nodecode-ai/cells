;;;; nodecode-prompt-studio.asd --- a prompt built step by step for its reader.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `prompt-studio' section, registers the prompt_studio tool and
;;;; the /prompt, /prompt-steps and /prompt-edit commands, and puts the
;;;; manual on (help :prompt-studio).
;;;;
;;;; A rewrite of bwoliveira's Prompt Studio; see NOTICE. Every dependency
;;;; rides with nodecode: the build is local arithmetic over the answers, and
;;;; the one optional polishing call is NLK's own side call.

(defsystem "nodecode-prompt-studio"
  :description "Builds a prompt step by step for the model that will read it"
  :license "MIT"
  :author "bwoliveira (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-prompt-studio/test"))))

(defsystem "nodecode-prompt-studio/test"
  :description "Prompt Studio tests, registered into the shared nodecode.test registry."
  :license "MIT"
  :author "bwoliveira (original), Nodecode (rewrite)"
  :depends-on ("nodecode-prompt-studio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-prompt-studio-tests)))
