;;;; nodecode-agent-si.asd --- sourced memory and a search over past sessions.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of joelbrilliant/hermes-agent-si (see NOTICE): a brain of
;;;; facts that cite a source and the user's own quote, behind an evidence
;;;; gate, and a search over the agent sessions already on this machine.

(defsystem "nodecode-agent-si"
  :description "Sourced memory: facts with quotes and sources, plus search over past agent sessions"
  :license "Apache-2.0"
  :author "joelbrilliant (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agent-si/test"))))

(defsystem "nodecode-agent-si/test"
  :description "Agent SI's tests. Registered into the shared nodecode.test registry; RUN-AGENT-SI-TESTS filters by the AGENT-SI-CELL- prefix."
  :license "Apache-2.0"
  :author "joelbrilliant (original), Nodecode (rewrite)"
  :depends-on ("nodecode-agent-si" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agent-si-tests)))
