;;;; nodecode-godot.asd --- a live Godot 4 editor, as Nodecode tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `godot' section and registers the editor tools and /godot.
;;;;
;;;; A Nodecode rewrite of Tormen28/argos (see NOTICE).

(defsystem "nodecode-godot"
  :description "godot: a live Godot 4 editor - scene tree, nodes, batches, animation, play and screenshots"
  :license "MIT"
  :version "0.1.0"
  :author "Tormen28 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-godot/test"))))

(defsystem "nodecode-godot/test"
  :description "godot tests. Registered into the shared nodecode.test registry; RUN-GODOT-TESTS filters by the GODOT-CELL- name prefix."
  :license "MIT"
  :author "Tormen28 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-godot" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-godot-tests)))
