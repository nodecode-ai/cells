;;;; SPDX-License-Identifier: MIT
(defsystem "nodecode-nerve"
  :description "Nerve: a supervisory decision layer that reviews risky tool calls"
  :license "MIT"
  :author "keeltrace (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-nerve/test"))))

(defsystem "nodecode-nerve/test"
  :depends-on ("nodecode-nerve" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-nerve-tests)))
