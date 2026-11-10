;;;; nodecode-buildpilot.asd --- projects, their tasks and their checkpoints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `buildpilot' section and registers the six tools, /buildpilot
;;;; and (help :buildpilot).
;;;;
;;;; State is one plain JSON document under the Nodecode home, written
;;;; atomically, read by every tool and the command alike. Nothing here calls
;;;; a model, a network or a program.
;;;;
;;;; Ported from Justnevo's buildpilot (see NOTICE).

(defsystem "nodecode-buildpilot"
  :description "Projects, their tasks and their progress checkpoints, kept locally"
  :license "MIT"
  :author "Justnevo (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-buildpilot/test"))))

(defsystem "nodecode-buildpilot/test"
  :description "Buildpilot tests. Registered into the shared nodecode.test registry; RUN-BUILDPILOT-TESTS filters by the BUILDPILOT-CELL- name prefix."
  :license "MIT"
  :author "Justnevo (original), Nodecode (rewrite)"
  :depends-on ("nodecode-buildpilot" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-buildpilot-tests)))
