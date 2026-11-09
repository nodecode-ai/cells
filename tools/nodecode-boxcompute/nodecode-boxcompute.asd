;;;; nodecode-boxcompute.asd --- BoxCompute's cloud Linux VMs, as six tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `boxcompute' section, registers the six tools cell.lisp lists
;;;; and puts the plugin's skill on the manual as (help :boxcompute).
;;;;
;;;; A rewrite of boxcompute's boxcompute/hermes-plugin (see NOTICE): the
;;;; original shipped no code, only a manifest, a skill and the command that
;;;; added BoxCompute's hosted MCP server to Hermes. This cell is the MCP
;;;; client itself, over nlk:http, so the tools work with nothing installed
;;;; but the cell.

(defsystem "nodecode-boxcompute"
  :description "BoxCompute cloud Linux VMs: create one, run commands, edit files and share preview links, as six tools the model calls"
  :author "boxcompute (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-boxcompute/test"))))

(defsystem "nodecode-boxcompute/test"
  :description "BoxCompute cell tests. Registered into the shared nodecode.test registry; RUN-BOXCOMPUTE-TESTS filters by the BOXCOMPUTE-CELL- name prefix."
  :author "boxcompute (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-boxcompute" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-boxcompute-tests)))
