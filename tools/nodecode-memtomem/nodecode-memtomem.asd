;;;; nodecode-memtomem.asd --- memtomem memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `memtomem' section and registers the five memtomem tools and
;;;; /memtomem. Between calls the cell holds nothing: every read goes to the
;;;; notes on disk, which is the store the original shares with other agents.
;;;;
;;;; A Nodecode rewrite of memtomem's Hermes plugin (see NOTICE).

(defsystem "nodecode-memtomem"
  :description "memtomem: markdown notes the model saves, searches and recalls, shared with your other agents"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "memtomem (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-memtomem/test"))))

(defsystem "nodecode-memtomem/test"
  :description "memtomem tests. Registered into the shared nodecode.test registry; RUN-MEMTOMEM-TESTS filters by the MEMTOMEM-CELL- name prefix."
  :license "Apache-2.0"
  :author "memtomem (original), Nodecode (rewrite)"
  :depends-on ("nodecode-memtomem" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-memtomem-tests)))
