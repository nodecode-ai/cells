;;;; nodecode-remesher.asd --- image-to-GLB jobs through a Remesher CLI.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `remesher' section, registers the four tools and puts the
;;;; primer on (help :remesher).
;;;;
;;;; Rewritten from remesher/remesher's Hermes Agent plugin; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:run-bounded starts the CLI in
;;;; a process group of its own, bounded; nlk:sha256-text stamps an artifact.

(defsystem "nodecode-remesher"
  :description "Remesher: one image into a GLB through your own Remesher CLI, with persistent job state and hashed artifacts"
  :author "remesher (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-remesher/test"))))

(defsystem "nodecode-remesher/test"
  :description "Remesher tests. Registered into the shared nodecode.test registry; RUN-REMESHER-TESTS filters by the REMESHER-CELL- name prefix."
  :author "remesher (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-remesher" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-remesher-tests)))
