;;;; nodecode-super-artifacts.asd --- Super Artifacts: pages and forms at a live private URL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell, not part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `super-artifacts' section and registers the five tools
;;;; and /super-artifacts.
;;;;
;;;; A Nodecode rewrite of Super-Artifacts/plugins (see NOTICE).

(defsystem "nodecode-super-artifacts"
  :description "Super Artifacts: publish a page, form or tracker as a live private URL, and read back what visitors submitted"
  :author "Super-Artifacts (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "signin")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-super-artifacts/test"))))

(defsystem "nodecode-super-artifacts/test"
  :description "Super Artifacts tests: the tools over a stubbed transport, the sign-in, the refusals."
  :license "MIT"
  :author "Super-Artifacts (original), Nodecode (rewrite)"
  :depends-on ("nodecode-super-artifacts" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-super-artifacts-tests)))
