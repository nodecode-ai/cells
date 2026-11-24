;;;; nodecode-receipts.asd --- explicit check evidence, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, not part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `receipts' section and puts the four tools, /receipts and
;;;; (help :receipts) on the harness.
;;;;
;;;; Ported from the hermes-receipts plugin (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-receipts"
  :description "Receipts: timestamped evidence of explicit file, test and website checks"
  :license "MIT"
  :version "0.1.0"
  :author "iket72183-collab (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "json")
               (:file "spec")
               (:file "checks")
               (:file "store")
               (:file "export")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-receipts/test"))))

(defsystem "nodecode-receipts/test"
  :description "Receipts tests. Registered into the shared nodecode.test registry; RUN-RECEIPTS-TESTS filters by the RECEIPTS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-receipts" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-receipts-tests)))
