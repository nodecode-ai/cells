;;;; nodecode-alans-way.asd --- computer use through Alan's Workspace.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `alans-way' section and registers the nine computer_* tools and
;;;; /computer.
;;;;
;;;; A Nodecode rewrite of capthvnsen/alans-way-agents's alans-way-computer
;;;; Hermes plugin (see NOTICE).

(defsystem "nodecode-alans-way"
  :description "Alan's Way computer use: a Mac app's controls, clicked, typed into and scrolled from a session"
  :license "MIT"
  :version "0.1.0"
  :author "capthvnsen (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "router")
               (:file "computer")
               (:file "actions")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-alans-way/test"))))

(defsystem "nodecode-alans-way/test"
  :description "Alan's Way computer-use tests. Registered into the shared nodecode.test registry; RUN-ALANS-WAY-TESTS filters by the ALANS-WAY-CELL- name prefix."
  :license "MIT"
  :author "capthvnsen (original), Nodecode (rewrite)"
  :depends-on ("nodecode-alans-way" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-alans-way-tests)))
