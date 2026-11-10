;;;; nodecode-payops.asd --- Paystack transactions and reconciliation, read-only.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `payops' section, registers the two tools and puts the primer on
;;;; (help :payops) and the /payops command in the catalog.
;;;;
;;;; Rewritten from BigWave45's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http is the one seam,
;;;; and one host answers (api.paystack.co, or whatever base_url names).

(defsystem "nodecode-payops"
  :description "Paystack: read-only transaction listing and reconciliation reports"
  :author "BigWave45 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "wire")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-payops/test"))))

(defsystem "nodecode-payops/test"
  :description "Paystack tests. Registered into the shared nodecode.test registry; RUN-PAYOPS-TESTS filters by the PAYOPS-CELL- name prefix."
  :author "BigWave45 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-payops" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-payops-tests)))
