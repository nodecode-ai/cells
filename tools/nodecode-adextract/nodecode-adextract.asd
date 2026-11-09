;;;; nodecode-adextract.asd --- competitor ad research and creative analysis, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `adextract' section, registers the five tools cell.lisp lists
;;;; and puts the plugin's skills on the manual as (help :adextract).
;;;;
;;;; A rewrite of ad-vertly's adextract-mcp-server (see NOTICE): the original
;;;; shipped a hosted MCP server through a portable manifest and five skills.
;;;; This cell speaks JSON-RPC to the same endpoint itself, so it needs no MCP
;;;; transport and no Hermes runtime.

(defsystem "nodecode-adextract"
  :description "Competitor ad research across the Meta, Google, LinkedIn and TikTok ad libraries, and ad creative analysis, as five tools the model calls"
  :author "ad-vertly (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-adextract/test"))))

(defsystem "nodecode-adextract/test"
  :description "adextract cell tests. Registered into the shared nodecode.test registry; RUN-ADEXTRACT-TESTS filters by the ADEXTRACT-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-adextract" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-adextract-tests)))
