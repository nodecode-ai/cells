;;;; nodecode-equipdash.asd --- EquipDash's MCP server as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `equipdash' section, registers the three tools and the
;;;; /equipdash command, and puts the original's skill on the manual as
;;;; (help :equipdash).
;;;;
;;;; Rewritten from equipdash's Agent Plugins package; see NOTICE. The
;;;; original was an MCP server declaration and one skill; this cell speaks
;;;; the same server's streamable HTTP itself, through nlk:http.

(defsystem "nodecode-equipdash"
  :description "EquipDash: a rental or activity shop's day, bookings, availability and payments, as tools the model calls"
  :author "equipdash (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-equipdash/test"))))

(defsystem "nodecode-equipdash/test"
  :description "EquipDash cell tests. Registered into the shared nodecode.test registry; RUN-EQUIPDASH-TESTS filters by the EQUIPDASH-CELL- name prefix."
  :author "equipdash (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-equipdash" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-equipdash-tests)))
