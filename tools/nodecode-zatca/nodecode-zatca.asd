;;;; nodecode-zatca.asd --- Saudi VAT math and ZATCA Phase 2 invoice checks for the model.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when this directory sits
;;;; under ~/.nodecode/cells/, or the gateway installs it live when
;;;; `nodecode add nc://zatca' is accepted. It reads the `zatca' config
;;;; section and contributes two tools (ksa_vat_calculate and
;;;; zatca_validate_invoice), one command (/zatca) and one manual topic
;;;; (help :zatca); nothing starts a thread, dials a host or reads a file.

(defsystem "nodecode-zatca"
  :description "Saudi 15% VAT math and ZATCA Phase 2 invoice field checks"
  :license "MIT"
  :author "qeemah-cloud (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "vat")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-zatca/test"))))

(defsystem "nodecode-zatca/test"
  :description "Tests for the zatca cell; RUN-ZATCA-TESTS filters the shared registry by the ZATCA name prefix."
  :license "MIT"
  :depends-on ("nodecode-zatca" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "vat-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-zatca-tests)))
