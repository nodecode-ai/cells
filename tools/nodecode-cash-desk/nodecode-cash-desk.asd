;;;; nodecode-cash-desk.asd --- chase overdue invoices, approve every letter.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of FutureInvoked/cash-desk (MIT).
;;;;
;;;; Optional ASDF cell, NOT part of the organism core. The folder loader
;;;; loads it at boot; the gateway calls START-CELL after recovery, which
;;;; reads the `cash-desk' section, registers the ten cashdesk_* tools and
;;;; /cashdesk, and puts the manual on (help :cash-desk). The ledger is one
;;;; JSON file under the home; nothing here dials anything.

(defsystem "nodecode-cash-desk"
  :description "Chase overdue invoices from CSV exports: match payments, draft reminders, and send only after a person approves"
  :license "MIT"
  :version "0.1.0"
  :author "FutureInvoked (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "text")
               (:file "ledger")
               (:file "chase")
               (:file "verbs")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cash-desk/test"))))

(defsystem "nodecode-cash-desk/test"
  :description "Cash Desk tests. Registered into the shared nodecode.test registry; RUN-CASH-DESK-TESTS filters by the CASH-DESK-CELL- name prefix."
  :license "MIT"
  :author "FutureInvoked (original), Nodecode (rewrite)"
  :depends-on ("nodecode-cash-desk" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cash-desk-tests)))
