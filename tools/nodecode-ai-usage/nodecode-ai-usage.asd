;;;; nodecode-ai-usage.asd --- how much of each AI subscription is left.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ai-usage' section, registers the ai_usage tool and /quotas,
;;;; and puts (help :ai-usage) on the manual.

(defsystem "nodecode-ai-usage"
  :description "How much of each AI subscription is left, read from the plans themselves"
  :license "MIT"
  :version "0.1.0"
  :author "lvabarajithan (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ai-usage/test"))))

(defsystem "nodecode-ai-usage/test"
  :description "AI usage tests. Registered into the shared nodecode.test registry; RUN-AI-USAGE-TESTS filters by the AI-USAGE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-ai-usage" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ai-usage-tests)))
