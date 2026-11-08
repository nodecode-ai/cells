;;;; nodecode-mem0.asd --- Mem0 memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `mem0' section, registers the four mem0_* tools and /mem0, and
;;;; starts the worker that sends each finished turn to Mem0 for extraction.
;;;;
;;;; A Nodecode rewrite of mem0ai/mem0's Hermes plugin (see NOTICE).

(defsystem "nodecode-mem0"
  :description "Mem0 memory: semantic recall and durable facts, through Mem0's cloud or a self-hosted server"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "mem0ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mem0/test"))))

(defsystem "nodecode-mem0/test"
  :description "Mem0 tests. Registered into the shared nodecode.test registry; RUN-MEM0-TESTS filters by the MEM0-CELL- name prefix."
  :license "Apache-2.0"
  :author "mem0ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mem0" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mem0-tests)))
