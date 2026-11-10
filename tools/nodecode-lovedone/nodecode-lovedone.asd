;;;; nodecode-lovedone.asd --- a family's shared LovedOne inbox as model tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `lovedone' section, registers the tools the original plugin's
;;;; MCP server offers, puts /lovedone and the manual topic on, and hands the
;;;; model a primer.
;;;;
;;;; The original is a Hermes Agent plugin for the LovedOne Family Inbox: a
;;;; portable manifest (plugin.json + mcp.json + a skill) that points the
;;;; harness at a hosted MCP server over Streamable HTTP, with the family's
;;;; MCP key in an Authorization header. This cell speaks the same JSON-RPC
;;;; 2.0 to the same endpoint through the image's own HTTP call -- no Python,
;;;; no Node, no MCP runtime, nothing fetched at run time -- and answers the
;;;; same results as the text a model reads.
;;;;
;;;; The .asd sits INSIDE its folder: install by putting -- or symlinking --
;;;; the directory under ~/.nodecode/cells/.

(defsystem "nodecode-lovedone"
  :description "LovedOne Family Inbox: a family's shared inbox for an aging parent, as model tools"
  :license "MIT"
  :version "0.1.0"
  :author "Terahertz-Inc (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-lovedone/test"))))

(defsystem "nodecode-lovedone/test"
  :description "LovedOne tests. Registered into the shared nodecode.test registry; RUN-LOVEDONE-TESTS filters by the LOVEDONE-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Terahertz-Inc (original), Nodecode (rewrite)"
  :depends-on ("nodecode-lovedone" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-lovedone-tests)))
