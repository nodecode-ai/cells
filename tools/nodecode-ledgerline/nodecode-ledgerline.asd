;;;; nodecode-ledgerline.asd --- what every session costs, by day, model and session.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ledgerline' section, registers the three tools and the
;;;; /ledger command, and puts the manual on (help :ledgerline).
;;;;
;;;; It stands on nothing outside this folder: the one read is over the
;;;; event log the organism already keeps (NLK:EVENTS), the one condition
;;;; the cell's own. See NOTICE: this folder is a rewrite of Adolanium's
;;;; hermes-ledgerline desktop plugin.

(defsystem "nodecode-ledgerline"
  :description "What every session costs, by day, model and session, with budgets"
  :license "MIT"
  :version "0.1.0"
  :author "Adolanium (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ledgerline/test"))))

(defsystem "nodecode-ledgerline/test"
  :description "Ledgerline tests. Registered into the shared nodecode.test registry; RUN-LEDGERLINE-TESTS filters by the LEDGERLINE-CELL- name prefix."
  :license "MIT"
  :author "Adolanium (original), Nodecode (rewrite)"
  :depends-on ("nodecode-ledgerline" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ledgerline-tests)))
