;;;; nodecode-interim-ledger.asd --- a ledger of the messages models write mid-turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `interim-ledger' section, puts (help :interim-ledger) on the
;;;; manual, registers /interim-log, and advises the function every completed
;;;; provider round passes through, so each message a model writes beside a
;;;; tool call becomes one row of a JSON-lines ledger under the home.
;;;;
;;;; Ported from the Hermes plugin interim-ledger (see NOTICE).

(defsystem "nodecode-interim-ledger"
  :description "Interim Ledger: the messages models write mid-turn, per model, with /interim-log"
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "ledger")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-interim-ledger/test"))))

(defsystem "nodecode-interim-ledger/test"
  :description "Interim Ledger tests. Registered into the shared nodecode.test registry; RUN-INTERIM-LEDGER-TESTS filters by the INTERIM-LEDGER-CELL- name prefix."
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-interim-ledger" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-interim-ledger-tests)))
