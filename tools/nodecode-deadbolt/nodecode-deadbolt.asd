;;;; nodecode-deadbolt.asd --- one reviewed new text file, through a local sidecar.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `deadbolt' section and registers the two tools and /deadbolt.
;;;;
;;;; A rewrite of seanebones-lang/deadbolt-hermes-plugin (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-deadbolt"
  :description "DeadBolt: review and approve an exact new text file before the agent writes it"
  :author "seanebones-lang (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "review")
               (:file "sidecar")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-deadbolt/test"))))

(defsystem "nodecode-deadbolt/test"
  :description "DeadBolt tests. Registered into the shared nodecode.test registry; RUN-DEADBOLT-TESTS filters by the DEADBOLT-CELL- name prefix."
  :author "seanebones-lang (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-deadbolt" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-deadbolt-tests)))
