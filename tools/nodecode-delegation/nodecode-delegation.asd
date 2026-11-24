;;;; nodecode-delegation.asd --- named model routes for subagents.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of NousResearch's advanced-delegation plugin for Hermes Agent:
;;;; the `model_map' delegation backend, as a `delegate' tool whose `route' is
;;;; a name the operator wrote, and /delegation to see or switch the mode.
;;;; See NOTICE.

(defsystem "nodecode-delegation"
  :description "Named model routes for subagents: one delegate tool and /delegation"
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-delegation/test"))))

(defsystem "nodecode-delegation/test"
  :description "Delegation tests: the lifecycle, the routes, the tool over a stubbed subagent, the command."
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :depends-on ("nodecode-delegation" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-delegation-tests)))
