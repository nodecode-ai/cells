;;;; nodecode-optmem.asd --- OptMem memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `optmem' section, registers the eight optmem_* tools and
;;;; /optmem, and rides every request with the store's wake digest.
;;;;
;;;; A Nodecode rewrite of rarf/optmem-hermes-plugin (see NOTICE).

(defsystem "nodecode-optmem"
  :description "OptMem memory: an append-only local log with a decay tree, recall, compression and a bounded digest"
  :license "MIT"
  :version "0.1.0"
  :author "rarf (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "engine") (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-optmem/test"))))

(defsystem "nodecode-optmem/test"
  :description "OptMem tests. Registered into the shared nodecode.test registry; RUN-OPTMEM-TESTS filters by the OPTMEM-CELL- name prefix."
  :license "MIT"
  :author "rarf (original), Nodecode (rewrite)"
  :depends-on ("nodecode-optmem" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-optmem-tests)))
