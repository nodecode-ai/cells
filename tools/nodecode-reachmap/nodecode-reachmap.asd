;;;; nodecode-reachmap.asd --- what this machine can reach, before the model
;;;; says it cannot.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `reachmap' section, registers the reachmap tool, /reachmap and
;;;; the manual topic, hooks the :FRAME point so each turn's prompt carries a
;;;; compact map, and answers (reachmap:report) through eval.

(defsystem "nodecode-reachmap"
  :description "What this machine can reach right now: browsers, services, MCP, CLIs, drives"
  :license "MIT"
  :version "0.1.0"
  :author "MindSproutStudios (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-reachmap/test"))))

(defsystem "nodecode-reachmap/test"
  :description "Reachmap tests. Registered into the shared nodecode.test registry; RUN-REACHMAP-TESTS filters by the REACHMAP-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-reachmap" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-reachmap-tests)))
