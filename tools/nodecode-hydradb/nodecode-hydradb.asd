;;;; nodecode-hydradb.asd --- HydraDB memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `hydradb' section, registers the three hydradb_* tools and
;;;; /hydradb, and starts the worker that sends each finished turn to HydraDB.
;;;;
;;;; A Nodecode rewrite of hydra-db/hydradb-hermes-plugin (see NOTICE).

(defsystem "nodecode-hydradb"
  :description "HydraDB memory: recall before you answer, remember a note, forget one, and capture every turn"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "hydra-db (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hydradb/test"))))

(defsystem "nodecode-hydradb/test"
  :description "HydraDB tests. Registered into the shared nodecode.test registry; RUN-HYDRADB-TESTS filters by the HYDRADB-CELL- name prefix."
  :license "Apache-2.0"
  :author "hydra-db (original), Nodecode (rewrite)"
  :depends-on ("nodecode-hydradb" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hydradb-tests)))
