;;;; nodecode-camel-guard.asd --- a capability guard on the :tool point.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of hermes-camel-guard-plugin (nativ3ai): once a turn has read
;;;; tool output, a call that changes something must be in the operator's own
;;;; message. The cell is one hook on the core's :TOOL point, one /camel
;;;; command, and the capability table the two read.

(defsystem "nodecode-camel-guard"
  :description "Gates changing tool calls on the operator's own words after tool output"
  :license "MIT"
  :version "0.1.0"
  :author "nativ3ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-camel-guard/test"))))

(defsystem "nodecode-camel-guard/test"
  :description "camel-guard tests. Registered into the shared nodecode.test registry; RUN-CAMEL-GUARD-TESTS filters by the CAMEL-GUARD- name prefix."
  :license "MIT"
  :depends-on ("nodecode-camel-guard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-camel-guard-tests)))
