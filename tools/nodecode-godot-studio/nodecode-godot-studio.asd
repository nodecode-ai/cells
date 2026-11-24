;;;; nodecode-godot-studio.asd --- headless Godot 4 checks, exports and build state.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `godot-studio' section, registers the four tools, puts the manual on
;;;; (help :godot-studio) and registers /godot-studio.
;;;;
;;;; Rewritten from Foolafroos's Hermes Agent plugin of the same name; see
;;;; NOTICE. Everything it needs rides with nodecode: uiop runs the Godot
;;;; executable a machine already has, and nlk reads and writes the config,
;;;; the presets and the answers.

(defsystem "nodecode-godot-studio"
  :description "Godot 4: headless check a project, export from a preset, and read the state of its builds"
  :author "Foolafroos (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-godot-studio/test"))))

(defsystem "nodecode-godot-studio/test"
  :description "Godot tests. Registered into the shared nodecode.test registry; RUN-GODOT-TESTS filters by the GODOT-STUDIO-CELL- name prefix."
  :author "Foolafroos (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-godot-studio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-godot-studio-tests)))
