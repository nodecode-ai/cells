;;;; nodecode-hypotize.asd --- Hypotize's MCP server as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT

(defsystem "nodecode-hypotize"
  :description "Hypotize: a growth experiment backlog with an enforced lifecycle and recorded learnings"
  :license "MIT"
  :author "hypotizeai (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hypotize/test"))))

(defsystem "nodecode-hypotize/test"
  :depends-on ("nodecode-hypotize" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-hypotize-tests)))
