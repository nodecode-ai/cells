;;;; nodecode-woof.asd --- WOOF weather simulations on this machine's GPU.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; An optional cell: nothing in Nodecode names this system. The folder
;;;; loader loads it at boot when its directory sits under ~/.nodecode/cells/,
;;;; and the gateway calls START-CELL, which reads the `woof' section and
;;;; registers twelve tools, /woof and the :woof manual. Every one of them
;;;; drives the WOOF command line the machine already has; nothing here speaks
;;;; HTTP, and the documentation it searches is shipped beside it in docs/.

(defsystem "nodecode-woof"
  :description "WOOF weather simulations on this machine's GPU: plan, launch, watch and show the maps"
  :license "Apache-2.0"
  :author "recastsystems (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "docs")
               (:file "campaign")
               (:file "score")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-woof/test"))))

(defsystem "nodecode-woof/test"
  :description "WOOF cell tests. Registered into the shared nodecode.test registry; RUN-WOOF-TESTS filters by the WOOF-CELL- name prefix."
  :license "Apache-2.0"
  :author "recastsystems (original), Nodecode (rewrite)"
  :depends-on ("nodecode-woof" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-woof-tests)))
