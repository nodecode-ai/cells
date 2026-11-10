;;;; nodecode-invinoveritas-receipts.asd --- signed receipts for decisions.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `invinoveritas-receipts' section, registers the three receipt
;;;; tools and /invinoveritas-receipts, and puts the manual on
;;;; (help :invinoveritas-receipts).
;;;;
;;;; A rewrite of babyblueviper1's hermes-invinoveritas (see NOTICE). Every
;;;; dependency is already in the serving image.

(defsystem "nodecode-invinoveritas-receipts"
  :description "Signed, salted receipts for decisions: commit, verify for free, open later"
  :license "MIT"
  :version "0.1.0"
  :author "babyblueviper1 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "client")
               (:file "store")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-invinoveritas-receipts/test"))))

(defsystem "nodecode-invinoveritas-receipts/test"
  :description "Receipts tests. Registered into the shared nodecode.test registry; RUN-INVINOVERITAS-RECEIPTS-TESTS filters by the INVINOVERITAS-RECEIPTS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-invinoveritas-receipts" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-invinoveritas-receipts-tests)))
