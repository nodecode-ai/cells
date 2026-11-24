;;;; nodecode-canvas.asd --- a board the agent draws on, saved as one SVG page.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `canvas' section and registers the six board_* tools and /canvas.
;;;;
;;;; A Nodecode rewrite of skk2010/hermes-board-plugin (see NOTICE).

(defsystem "nodecode-canvas"
  :description "A board the agent draws on with ops, rendered as one HTML/SVG page you open"
  :license "MIT"
  :version "0.1.0"
  :author "skk2010 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package") (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-canvas/test"))))

(defsystem "nodecode-canvas/test"
  :description "Canvas tests. Registered into the shared nodecode.test registry; RUN-CANVAS-TESTS filters by the CANVAS-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "skk2010 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-canvas" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-canvas-tests)))
