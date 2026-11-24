;;;; nodecode-rea.asd --- REA's reverse-engineering MCP server as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `rea' section, puts (rea:tools ...) and (rea:call ...) on the
;;;; manual and registers the two tools and the /rea command cell.lisp lists.
;;;;
;;;; Ported from NousResearch/hermes-official-plugins' rea plugin (see NOTICE):
;;;; the pinned server, its two forwarded environment variables and the
;;;; reverse-engineering workflow. No Node source rides in the folder: the
;;;; server is the user's own npx, spoken to over stdio.

(defsystem "nodecode-rea"
  :description "Reverse engineer binaries, apps and core dumps through REA's MCP server"
  :license "MIT"
  :author "NousResearch (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "transport")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-rea/test"))))

(defsystem "nodecode-rea/test"
  :description "REA tests: the lifecycle, both tools through a stubbed server, the /rea command and the refusal."
  :license "MIT"
  :depends-on ("nodecode-rea" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-rea-tests)))
