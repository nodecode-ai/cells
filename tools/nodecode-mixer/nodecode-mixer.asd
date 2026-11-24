;;;; nodecode-mixer.asd --- four dials on how much a call may change.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Sahil-SS9's hermes-agent-mixer as a Nodecode cell: the same
;;;; four dials (edits, scope, lines, deps) enforced by one hook on the
;;;; organism's :TOOL point. See CELLS.md, "Declaring a cell".

(defsystem "nodecode-mixer"
  :description "Four dials on how much a call may change: edit permission, write scope, per-turn line budget, new dependencies"
  :license "MIT"
  :version "0.1.0"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "engine")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mixer/test"))))

(defsystem "nodecode-mixer/test"
  :description "Mixer tests: the four dials, the grammar, and the gate on a call."
  :license "MIT"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mixer" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mixer-tests)))
