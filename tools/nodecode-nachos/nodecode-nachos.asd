;;;; nodecode-nachos.asd --- durable memory: a manifest every turn, prefetch, recall.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `nachos' section, registers the three nachos_memory_* tools and
;;;; /nachos, puts the manifest in every request's harness sections and the
;;;; ranked entries in each round's request, and puts the manual on
;;;; (help :nachos).
;;;;
;;;; Rewritten from Nacho Labs' Hermes memory provider; see NOTICE. Its store
;;;; is the same titled-markdown file, its manifest and lexical prefetch are
;;;; the same three tiers, and its tools keep their names.

(defsystem "nodecode-nachos"
  :description "Nachos: durable local memory with a manifest every turn, prefetch and recall"
  :author "Nacho-Labs-LLC (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-nachos/test"))))

(defsystem "nodecode-nachos/test"
  :description "Nachos cell tests. Registered into the shared nodecode.test registry; RUN-NACHOS-TESTS filters by the NACHOS-CELL- name prefix."
  :author "Nacho-Labs-LLC (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-nachos" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-nachos-tests)))
