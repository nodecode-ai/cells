;;;; nodecode-nexus.asd --- durable memory in a Nexus Memory collection on Qdrant.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `nexus' section, registers the nexus_* tools and /nexus,
;;;; recalls for the newest ask before each turn, and files finished turns in
;;;; the background.
;;;;
;;;; A Nodecode rewrite of Neboy72's nexus-memory (see NOTICE). Every
;;;; dependency rides with nodecode: nlk:http speaks to Qdrant and to an
;;;; embedding endpoint, and the contracts type the tools' arguments.

(defsystem "nodecode-nexus"
  :description "Nexus Memory: durable facts on Qdrant, guardrails and entities"
  :author "Neboy72 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-nexus/test"))))

(defsystem "nodecode-nexus/test"
  :description "Nexus Memory tests. Registered into the shared nodecode.test registry; RUN-NEXUS-TESTS filters by the NEXUS-CELL- name prefix."
  :author "Neboy72 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-nexus" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-nexus-tests)))
