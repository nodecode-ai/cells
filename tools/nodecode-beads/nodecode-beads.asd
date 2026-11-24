;;;; nodecode-beads.asd --- a bd (Beads) work graph as six agent tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `beads' section and registers the six beads_* tools, /beads and
;;;; the manual topic (help :beads).
;;;;
;;;; The one thing outside this folder is the bd CLI itself, which the user
;;;; installs (brew install beads, or a release tarball; bd 1.3 or newer) and
;;;; runs `bd init' with in a project. See NOTICE: this folder is a rewrite of
;;;; jacobhausler/hermes-beads.

(defsystem "nodecode-beads"
  :description "beads: read, claim and update a bd (Beads) work graph from the agent"
  :license "MIT"
  :version "0.1.0"
  :author "jacobhausler (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "bd")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-beads/test"))))

(defsystem "nodecode-beads/test"
  :description "beads tests. Registered into the shared nodecode.test registry; RUN-BEADS-TESTS filters by the BEADS-CELL- name prefix."
  :license "MIT"
  :author "jacobhausler (original), Nodecode (rewrite)"
  :depends-on ("nodecode-beads" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-beads-tests)))
