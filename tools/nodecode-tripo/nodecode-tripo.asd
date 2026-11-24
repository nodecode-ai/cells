;;;; nodecode-tripo.asd --- Tripo 3D generation as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `tripo' section and registers the tripo_* tools, /tripo and
;;;; (help :tripo).
;;;;
;;;; A Nodecode rewrite of Adolanium/hermes-tripo (see NOTICE).

(defsystem "nodecode-tripo"
  :description "Tripo 3D: text, image or multiview to 3D, texturing, rigging, animation, retopology and conversion"
  :license "MIT"
  :version "0.1.0"
  :author "Adolanium (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "plans")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tripo/test"))))

(defsystem "nodecode-tripo/test"
  :description "Tripo tests. Registered into the shared nodecode.test registry; RUN-TRIPO-TESTS filters by the TRIPO-CELL- name prefix."
  :license "MIT"
  :author "Adolanium (original), Nodecode (rewrite)"
  :depends-on ("nodecode-tripo" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tripo-tests)))
