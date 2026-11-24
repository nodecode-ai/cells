;;;; nodecode-you.asd --- You.com search, contents, research and finance as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `you' section, registers the four tools cell.lisp lists and puts
;;;; the routing advice on the manual as (help :you).
;;;;
;;;; Ported from youdotcom-oss/agent-skills (see NOTICE): the plugin was
;;;; three MCP server entries and five skills. The entries are the four tools
;;;; below, spoken to over MCP's streamable HTTP; the skills are +PRIMER+.

(defsystem "nodecode-you"
  :description "You.com search, contents, research and finance as tools the model calls"
  :author "youdotcom-oss (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "mcp")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-you/test"))))

(defsystem "nodecode-you/test"
  :description "You cell tests. Registered into the shared nodecode.test registry; RUN-YOU-TESTS filters by the YOU-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-you" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-you-tests)))
