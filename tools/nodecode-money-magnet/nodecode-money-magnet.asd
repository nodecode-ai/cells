;;;; nodecode-money-magnet.asd --- a ledger, a budget, a portfolio and a spread scan.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `money-magnet' section, registers the five finance_* tools and
;;;; /money, and puts (help :money) on the manual.
;;;;
;;;; A Nodecode rewrite of Whydiriki700/money-magnet (see NOTICE).

(defsystem "nodecode-money-magnet"
  :description "Money Magnet: expenses and income tracked, a monthly budget, crypto portfolio PnL and a spread scan"
  :license "MIT"
  :version "0.1.0"
  :author "Whydiriki700 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-money-magnet/test"))))

(defsystem "nodecode-money-magnet/test"
  :description "Money Magnet tests. Registered into the shared nodecode.test registry; RUN-MONEY-MAGNET-TESTS filters by the MONEY-MAGNET-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Whydiriki700 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-money-magnet" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-money-magnet-tests)))
