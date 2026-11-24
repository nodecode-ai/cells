;;;; nodecode-approval-ledger.asd --- one row for every approval question asked.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `approval-ledger' section, advises the one question seam
;;;; (ASK-OPERATOR), puts (help :approval-ledger) on the manual and registers
;;;; /approval-log.
;;;;
;;;; A rewrite of olric3015's hermes-plugins (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-approval-ledger"
  :description "Approval Ledger: which approval questions were asked, how they ended and how long people took"
  :author "olric3015 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "ledger")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-approval-ledger/test"))))

(defsystem "nodecode-approval-ledger/test"
  :description "Approval Ledger tests. Registered into the shared nodecode.test registry; RUN-APPROVAL-LEDGER-TESTS filters by the APPROVAL-LEDGER-CELL- name prefix."
  :author "olric3015 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-approval-ledger" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-approval-ledger-tests)))
