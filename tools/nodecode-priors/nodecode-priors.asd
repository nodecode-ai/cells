;;;; nodecode-priors.asd --- Priors credit on Robinhood Chain, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `priors' section, registers the twelve tools cell.lisp lists
;;;; and puts what Priors is for on the manual as (help :priors).
;;;;
;;;; Ported from priors-agents/priors's Hermes plugin (see NOTICE). The
;;;; original shipped two MCP servers and one skill: the hosted read-only
;;;; server (agent records, sellers, pool data) and the local wallet command
;;;; that signs and moves USDG. This cell speaks both wires itself -- MCP over
;;;; streamable HTTP for the first, the same JSON-RPC over a child process's
;;;; stdin for the second -- so neither Node nor the original code is bundled.

(defsystem "nodecode-priors"
  :description "Priors: read agents' USDG credit records and pay x402 APIs on credit"
  :author "priors-agents (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-priors/test"))))

(defsystem "nodecode-priors/test"
  :description "Priors tests. Registered into the shared nodecode.test registry; RUN-PRIORS-TESTS filters by the PRIORS-CELL- name prefix."
  :author "priors-agents (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-priors" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-priors-tests)))
