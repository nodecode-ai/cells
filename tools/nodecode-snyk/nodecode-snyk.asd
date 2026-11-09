;;;; nodecode-snyk.asd --- Snyk vulnerability scans as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `snyk' section, registers the ten tools cell.lisp lists and
;;;; puts the workflow on the manual as (help :snyk).
;;;;
;;;; Ported from NousResearch's hermes-official-plugins `snyk' (see NOTICE).
;;;; The original shipped Snyk's first-party MCP server (a pinned `npx snyk
;;;; mcp`) and a workflow skill; every one of its tools wrapped one Snyk CLI
;;;; subcommand, so this cell runs that CLI directly. The CLI itself is the
;;;; operator's, like the language servers of nodecode-lsp.

(defsystem "nodecode-snyk"
  :description "Snyk vulnerability scans of code, dependencies, containers and IaC, as tools the model calls"
  :author "NousResearch (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cli")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-snyk/test"))))

(defsystem "nodecode-snyk/test"
  :description "Snyk cell tests. Registered into the shared nodecode.test registry; RUN-SNYK-TESTS filters by the SNYK-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-snyk" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-snyk-tests)))
