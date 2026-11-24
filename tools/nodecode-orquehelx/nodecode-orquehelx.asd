;;;; nodecode-orquehelx.asd --- routes across subscriptions: one delegate_to tool and /ohx.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of TicoraX's OrqueHelx plugin for Hermes Agent: the named
;;;; routes a subagent runs on, the out-of-quota answer with its policy, and
;;;; /ohx. See NOTICE.

(defsystem "nodecode-orquehelx"
  :description "Routes across subscriptions: delegate_to sends a subagent to a named provider and model, and /ohx says what ran out of quota"
  :license "MIT"
  :author "TicoraX (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-orquehelx/test"))))

(defsystem "nodecode-orquehelx/test"
  :description "OrqueHelx tests: the routes, the tool over a stubbed subagent, the policy, the command."
  :license "MIT"
  :author "TicoraX (original), Nodecode (rewrite)"
  :depends-on ("nodecode-orquehelx" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-orquehelx-tests)))
