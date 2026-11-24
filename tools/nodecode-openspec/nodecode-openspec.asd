;;;; nodecode-openspec.asd --- OpenSpec's spec-driven development, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `openspec' section, registers the twenty openspec_* tools and
;;;; /openspec, and puts the manual on (help :openspec).
;;;;
;;;; A Nodecode rewrite of FelineStateMachine/hermes-openspec (see NOTICE).

(defsystem "nodecode-openspec"
  :description "OpenSpec: spec-driven development over a project's openspec/ folder"
  :license "MIT"
  :version "0.1.0"
  :author "FelineStateMachine (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "model")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-openspec/test"))))

(defsystem "nodecode-openspec/test"
  :description "OpenSpec tests. Registered into the shared nodecode.test registry; RUN-OPENSPEC-TESTS filters by the OPENSPEC-CELL- name prefix."
  :license "MIT"
  :author "FelineStateMachine (original), Nodecode (rewrite)"
  :depends-on ("nodecode-openspec" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-openspec-tests)))
