;;;; nodecode-file-lineage.asd --- a receipt for every file the agent writes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. A Nodecode rewrite of P0GL0L's hermes-lineage (see NOTICE). One
;;;; dependency beyond nodecode: cl-ppcre, the serving image's pattern engine.

(defsystem "nodecode-file-lineage"
  :description "Provenance receipts for the files the agent writes, and the graph back to their sources"
  :author "P0GL0L (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "lineage")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-file-lineage/test"))))

(defsystem "nodecode-file-lineage/test"
  :description "Receipts tests: the hook on a stubbed call, the command, the tools and verify, with no network."
  :author "P0GL0L (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-file-lineage" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-file-lineage-tests)))
