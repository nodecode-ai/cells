;;;; nodecode-redteam-gates.asd --- the stage gates a red-team playbook closes on,
;;;; as a tool the model must call rather than grade itself.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when this directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://redteam-gates' is accepted. It reads the
;;;; `redteam-gates' config section and contributes two tools (stage_gate,
;;;; gates_list), one command (/redteam-gates) and one manual topic
;;;; (help :redteam-gates). Nothing starts a thread, dials a host, or reads a
;;;; credential; the artifacts and the gate log under the section's workspace
;;;; are all it touches.

(defsystem "nodecode-redteam-gates"
  :description "Red-team stage gates checked by a tool: what a stage's artifacts hold, and what is missing"
  :license "MIT"
  :author "SeaOf0 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "gates")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-redteam-gates/test"))))

(defsystem "nodecode-redteam-gates/test"
  :description "Tests for the redteam-gates cell; RUN-REDTEAM-GATES-TESTS filters the shared registry by the REDTEAM-GATES- name prefix."
  :license "MIT"
  :depends-on ("nodecode-redteam-gates" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-redteam-gates-tests)))
