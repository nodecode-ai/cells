;;;; nodecode-openrouter-spend.asd --- OpenRouter's own reported key spend.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openrouter-spend' section, registers the openrouter_spend
;;;; tool and /openrouter-spend, and puts (help :openrouter-spend) on the
;;;; manual.

(defsystem "nodecode-openrouter-spend"
  :description "OpenRouter's own reported key spend in USD and INR, as a tool and /openrouter-spend"
  :license "MIT"
  :author "AllenRobinHubert (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openrouter-spend/test"))))

(defsystem "nodecode-openrouter-spend/test"
  :description "OpenRouter spend tests. Registered into the shared nodecode.test registry; RUN-OPENROUTER-SPEND-TESTS filters by the OPENROUTER-SPEND-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-openrouter-spend" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openrouter-spend-tests)))
