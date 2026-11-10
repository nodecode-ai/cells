;;;; nodecode-hyperactive.asd --- goals, promises and cards that speak first.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of justin06lee/hyperactive for Nodecode. Optional ASDF cell,
;;;; not part of the organism core: the folder loader loads it at boot when
;;;; its directory sits under ~/.nodecode/cells/, and the gateway calls
;;;; START-CELL, which reads the `hyperactive' section, loads the store,
;;;; starts the pulse worker, registers the six ha_* tools, /hyperactive and
;;;; (help :hyperactive).

(defsystem "nodecode-hyperactive"
  :description "Goals, promises and tap cards that speak first"
  :license "MIT"
  :version "0.1.0"
  :author "justin06lee (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hyperactive/test"))))

(defsystem "nodecode-hyperactive/test"
  :description "hyperactive tests. Registered into the shared nodecode.test registry; RUN-HYPERACTIVE-TESTS filters by the HYPERACTIVE-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-hyperactive" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hyperactive-tests)))
