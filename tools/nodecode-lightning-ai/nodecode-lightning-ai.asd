;;;; nodecode-lightning-ai.asd --- the Lightning AI request gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, not part of the organism core: a cell is a folder
;;;; carrying its .asd, and the gateway calls this system's START-CELL after
;;;; recovery. The cell wraps the one transport bracket every streaming lane
;;;; shares, so the requests Nodecode sends to Lightning AI stay inside the
;;;; provider's own per-minute limit.

(defsystem "nodecode-lightning-ai"
  :description "Holds Lightning AI model calls to their per-minute limit with a wall-clock token bucket"
  :license "MIT"
  :author "dinosauria123 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-lightning-ai/test"))))

(defsystem "nodecode-lightning-ai/test"
  :description "Lightning AI gate tests. Registered into the shared nodecode.test registry; RUN-LIGHTNING-AI-TESTS filters by the LIGHTNING-AI name prefix."
  :license "MIT"
  :depends-on ("nodecode-lightning-ai" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-lightning-ai-tests)))
