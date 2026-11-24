;;;; nodecode-quegly.asd --- Quegly: social posts scheduled, published and measured.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `quegly' section, registers the eight tools cell.lisp lists and
;;;; puts the guidance on the manual as (help :quegly).
;;;;
;;;; A rewrite of Quegly's quegly/agent Hermes plugin (see NOTICE): the
;;;; original connected an MCP client to Quegly's hosted MCP server and
;;;; carried one skill. Its seventeen tools are the endpoints of Quegly's
;;;; public REST API v1, so this cell calls that API directly and needs
;;;; neither Hermes nor an MCP client of its own.

(defsystem "nodecode-quegly"
  :description "Schedule, publish and measure social posts on Quegly: workspaces, accounts, uploads, posts and analytics, as eight tools the model calls"
  :author "quegly (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-quegly/test"))))

(defsystem "nodecode-quegly/test"
  :description "Quegly cell tests. Registered into the shared nodecode.test registry; RUN-QUEGLY-TESTS filters by the QUEGLY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-quegly" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-quegly-tests)))
