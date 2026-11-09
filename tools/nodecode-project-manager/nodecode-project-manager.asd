;;;; nodecode-project-manager.asd --- the git repositories under your own folders.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `project-manager' section and registers the five tools, /projects
;;;; and (help :projects).
;;;;
;;;; Ported from AiSolve-me's hermes-project-manager (see NOTICE). The one
;;;; program it runs is the git the operator already has; every dependency
;;;; rides with nodecode.

(defsystem "nodecode-project-manager"
  :description "The git repositories under your own folders: branches, commits and what changed"
  :license "MIT"
  :author "AiSolve-me (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-project-manager/test"))))

(defsystem "nodecode-project-manager/test"
  :description "Project manager tests. Registered into the shared nodecode.test registry; RUN-PROJECT-MANAGER-TESTS filters by the PROJECT-MANAGER-CELL- name prefix."
  :license "MIT"
  :author "AiSolve-me (original), Nodecode (rewrite)"
  :depends-on ("nodecode-project-manager" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-project-manager-tests)))
