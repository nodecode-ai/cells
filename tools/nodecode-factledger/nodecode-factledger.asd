;;;; nodecode-factledger.asd --- Factledger: typed facts, superseded with history.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `factledger' section, registers the factledger tool and
;;;; /factledger, and puts (help :factledger) on the manual.
;;;;
;;;; A Nodecode rewrite of explorelifehus-eng/factledger (see NOTICE).

(defsystem "nodecode-factledger"
  :description "Factledger: a durable ledger of typed facts, a new value closing the old row"
  :license "MIT"
  :version "0.1.0"
  :author "explorelifehus-eng (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "ledger")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-factledger/test"))))

(defsystem "nodecode-factledger/test"
  :description "Factledger tests. Registered into the shared nodecode.test registry; RUN-FACTLEDGER-TESTS filters by the FACTLEDGER-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "explorelifehus-eng (original), Nodecode (rewrite)"
  :depends-on ("nodecode-factledger" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-factledger-tests)))
