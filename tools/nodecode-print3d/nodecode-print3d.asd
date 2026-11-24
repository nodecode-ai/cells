;;;; nodecode-print3d.asd --- an idea to a checked, sliced Bambu project.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it at boot
;;;; when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL, which reads the `print3d' section, registers the three tools
;;;; and the /print3d command, and puts the manual on (help :print3d).
;;;;
;;;; The one thing outside this folder is the user's own Blender and Bambu
;;;; Studio, which the cell runs on the job's model.py and STL. See NOTICE:
;;;; this folder is a rewrite of mfranzon/print3d's Hermes plugin.

(defsystem "nodecode-print3d"
  :description "print3d: a Blender model, checked and sliced for a Bambu Lab X2D"
  :license "MIT"
  :version "0.1.0"
  :author "mfranzon (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-print3d/test"))))

(defsystem "nodecode-print3d/test"
  :description "print3d tests. Registered in the shared nodecode.test registry; RUN-PRINT3D-TESTS filters by the PRINT3D-CELL- prefix."
  :license "MIT"
  :author "mfranzon (original), Nodecode (rewrite)"
  :depends-on ("nodecode-print3d" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-print3d-tests)))
