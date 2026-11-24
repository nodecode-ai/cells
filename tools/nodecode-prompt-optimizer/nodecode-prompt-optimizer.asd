;;;; nodecode-prompt-optimizer.asd --- a prompt rewritten for the model that reads it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core. The folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/; the
;;;; gateway calls START-CELL after recovery, which reads the
;;;; `prompt-optimizer' section, registers /optimize and /prompt-stats, the
;;;; optimize_prompt tool, and puts the manual on (help :prompt-optimizer).

(defsystem "nodecode-prompt-optimizer"
  :description "Rewrites a prompt shorter and clearer for the model that will read it"
  :license "MIT"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-prompt-optimizer/test"))))

(defsystem "nodecode-prompt-optimizer/test"
  :description "Prompt optimizer tests, registered into the shared nodecode.test registry."
  :license "MIT"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-prompt-optimizer" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-prompt-optimizer-tests)))
