;;;; nodecode-qrblox.asd --- Qrblox merchant codes as tools the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `qrblox' section, registers the seven tools cell.lisp lists and
;;;; the /qrblox command, and puts the workflow on the manual as (help :qrblox).
;;;;
;;;; The original is an Agent Plugins package with no code of its own: a
;;;; plugin.json, an mcp.json declaring Qrblox's hosted MCP server, and one
;;;; skill. This cell speaks that server's JSON-RPC over HTTP itself, through
;;;; nlk:http -- no Python, nothing fetched at run time, no dependency of its
;;;; own. See NOTICE.

(defsystem "nodecode-qrblox"
  :description "Qrblox: draft QR codes, audio codes and menus on a merchant account"
  :author "Terahertz-Inc (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-qrblox/test"))))

(defsystem "nodecode-qrblox/test"
  :description "Qrblox cell tests. Registered into the shared nodecode.test registry; RUN-QRBLOX-TESTS filters by the QRBLOX-CELL- name prefix."
  :author "Terahertz-Inc (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-qrblox" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-qrblox-tests)))
