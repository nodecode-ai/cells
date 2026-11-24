;;;; nodecode-qdrant-memory.asd --- durable, searchable memory in a Qdrant store.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `qdrant_memory' section, registers the four qdrant_memory_*
;;;; tools and /qdrant-memory, recalls for the newest ask before each turn,
;;;; and files finished turns in the background.
;;;;
;;;; A Nodecode rewrite of cnkang's hermes-plugin-qdrant-memory (see NOTICE).
;;;; Every dependency rides with nodecode: nlk:http speaks to Qdrant and to the
;;;; embedding endpoint, and the contracts type the tools' arguments.

(defsystem "nodecode-qdrant-memory"
  :description "Qdrant memory: durable facts in a Qdrant store, recalled by meaning"
  :author "cnkang (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-qdrant-memory/test"))))

(defsystem "nodecode-qdrant-memory/test"
  :description "Qdrant memory tests. Registered into the shared nodecode.test registry; RUN-QDRANT-MEMORY-TESTS filters by the QDRANT-MEMORY-CELL- name prefix."
  :author "cnkang (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-qdrant-memory" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-qdrant-memory-tests)))
