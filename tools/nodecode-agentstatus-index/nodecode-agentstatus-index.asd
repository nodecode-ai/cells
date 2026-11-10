;;;; nodecode-agentstatus-index.asd --- a public index of MCP servers, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `agentstatus_index' section, registers the seven tools cell.lisp
;;;; lists and the /agentstatus-index command, and puts the workflow on the
;;;; manual as (help :agentstatus-index).
;;;;
;;;; Rewritten from the AgentStatus MCP Index Agent Plugins package; see
;;;; NOTICE. The original was an MCP server declaration and one skill; this
;;;; cell speaks the same server's streamable HTTP itself, through nlk:http.

(defsystem "nodecode-agentstatus-index"
  :description "AgentStatus Index: search a public index of MCP servers and call a tool on one you pick"
  :author "Carmel-Labs-Inc (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agentstatus-index/test"))))

(defsystem "nodecode-agentstatus-index/test"
  :description "AgentStatus Index cell tests. Registered into the shared nodecode.test registry; RUN-AGENTSTATUS-INDEX-TESTS filters by the AGENTSTATUS-INDEX-CELL- name prefix."
  :author "Carmel-Labs-Inc (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-agentstatus-index" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agentstatus-index-tests)))
