;;;; nodecode-memory-bridge.asd --- search and read a folder of markdown the operator approved.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `memory-bridge' section, registers memory_bridge_search,
;;;; memory_bridge_read, the /memory-bridge command and the manual on
;;;; (help :memory-bridge). See NOTICE: this folder is a rewrite of
;;;; summero/memory-bridge's Hermes plugin.

(defsystem "nodecode-memory-bridge"
  :description "Shared markdown memory: permission-scoped, read-only search and reading"
  :license "MIT"
  :version "0.1.0"
  :author "summero (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-memory-bridge/test"))))

(defsystem "nodecode-memory-bridge/test"
  :description "memory-bridge tests, registered into the shared nodecode.test registry; RUN-MEMORY-BRIDGE-TESTS filters by the MEMORY-BRIDGE-CELL- prefix."
  :license "MIT"
  :version "0.1.0"
  :author "summero (original), Nodecode (rewrite)"
  :depends-on ("nodecode-memory-bridge" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-memory-bridge-tests)))
