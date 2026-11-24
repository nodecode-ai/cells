;;;; nodecode-blueprint.asd --- six gated phases from a raw idea to a plan you can build.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `blueprint' section, registers the six blueprint_* tools and the
;;;; /blueprint command, and puts the method on the manual as (help :blueprint).
;;;;
;;;; Rewritten from winchxyz's hermes-blueprint plugin; see NOTICE. The
;;;; method is prose in the manual; what the code keeps is the rules the
;;;; original enforced -- no source, no check mark; a gate per phase; a critic
;;;; that commits before it is shown; agent work in hours, months only from
;;;; named waits.

(defsystem "nodecode-blueprint"
  :description "Six gated phases from a raw idea to a plan you can build"
  :author "winchxyz (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-blueprint/test"))))

(defsystem "nodecode-blueprint/test"
  :description "Blueprint cell tests. Registered into the shared nodecode.test registry; RUN-BLUEPRINT-TESTS filters by the BLUEPRINT-CELL- name prefix."
  :author "winchxyz (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-blueprint" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-blueprint-tests)))
