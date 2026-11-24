;;;; nodecode-construct.asd --- a description of a thing into geometry, a model,
;;;; a mesh and a page.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in src/ names this system.
;;;; The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL, which reads the
;;;; `construct' section, registers the six tools and the /construct command,
;;;; and puts the manual on (help :construct).
;;;;
;;;; This folder is a rewrite of the Hermes plugin of rtmandreyev/construct.
;;;; See NOTICE: the scene format and the provenance of its parts are the
;;;; original's; its engine, its exporters and its viewers are reimplemented.

(defsystem "nodecode-construct"
  :description "Construct: a described thing into a checked scene, glTF, OBJ, STL and a hologram page"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "rtmandreyev (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "shapes")
               (:file "scene")
               (:file "export")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-construct/test"))))

(defsystem "nodecode-construct/test"
  :description "Construct tests. Registered in the shared nodecode.test registry; RUN-CONSTRUCT-TESTS filters by the CONSTRUCT-CELL- prefix."
  :license "Apache-2.0"
  :author "rtmandreyev (original), Nodecode (rewrite)"
  :depends-on ("nodecode-construct" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-construct-tests)))
