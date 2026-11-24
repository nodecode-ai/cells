;;;; nodecode-fleet-policy.asd --- news only: what an MCP read tool just returned.
;;;;
;;;; SPDX-License-Identifier: MIT
(defsystem "nodecode-fleet-policy"
  :description "One MCP read tool, its ids, and only the items nobody has seen: a wake gate for cron."
  :license "MIT"
  :version "0.1.0"
  :author "Julientalbot (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-fleet-policy/test"))))

(defsystem "nodecode-fleet-policy/test"
  :depends-on ("nodecode-fleet-policy" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-fleet-policy-tests)))
