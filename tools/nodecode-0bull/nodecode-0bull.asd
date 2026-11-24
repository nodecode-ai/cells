;;;; nodecode-0bull.asd --- real rented iPhones, as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `0bull' section, registers the seven tools cell.lisp lists and
;;;; the /0bull command, and puts the workflow on the manual as (help :0bull).
;;;;
;;;; Rewritten from 0bull's Agent Plugins package; see NOTICE. The original
;;;; was an MCP server declaration and three skills; this cell speaks the same
;;;; server's streamable HTTP itself, through nlk:http.

(defsystem "nodecode-0bull"
  :description "0bull: drive a real rented iPhone and publish to TikTok or YouTube Shorts"
  :author "0bull (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-0bull/test"))))

(defsystem "nodecode-0bull/test"
  :description "0bull cell tests. Registered into the shared nodecode.test registry; RUN-0BULL-TESTS filters by the 0BULL-CELL- name prefix."
  :author "0bull (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-0bull" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-0bull-tests)))
