;;;; nodecode-plan-canvas.asd --- the plan the agent draws, wired to its real work.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `plan-canvas' section, registers the original's four
;;;; tools, hooks the points the canvas learns from, registers /plan-canvas
;;;; and puts the manual on (help :plan-canvas). Every dependency is already
;;;; in the serving image.

(defsystem "nodecode-plan-canvas"
  :description "The agent's plan as boxes, whose status comes from real writes and checks"
  :license "MIT"
  :author "0xbyt4 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-plan-canvas/test"))))

(defsystem "nodecode-plan-canvas/test"
  :description "Plan-canvas tests. Registered into the shared nodecode.test registry; RUN-PLAN-CANVAS-TESTS filters by the PLAN-CANVAS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-plan-canvas" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-plan-canvas-tests)))
