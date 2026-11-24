;;;; nodecode-penny.asd --- Penny's hosted core memory, as three tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `penny' section, registers the three tools cell.lisp lists and
;;;; puts the memory primer on the manual as (help :penny).
;;;;
;;;; A rewrite of HelperLabs's penny-hermes memory provider (see NOTICE): the
;;;; plugin was one Hermes memory provider speaking MCP to Penny's hosted
;;;; server; this cell speaks the same MCP endpoint itself, so it needs no
;;;; Hermes runtime and no Python.

(defsystem "nodecode-penny"
  :description "Penny memory: read your core memory, search your private notes and save a fact, through Penny's hosted MCP server"
  :author "HelperLabs (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "tools")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-penny/test"))))

(defsystem "nodecode-penny/test"
  :description "Penny cell tests. Registered into the shared nodecode.test registry; RUN-PENNY-TESTS filters by the PENNY-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-penny" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-penny-tests)))
