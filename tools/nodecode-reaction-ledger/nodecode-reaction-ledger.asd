;;;; nodecode-reaction-ledger.asd --- a local ledger of the emoji reactions left in your chats.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `reaction-ledger' section, puts (help :reaction-ledger) on the
;;;; manual, registers /reaction-log, and hooks the reaction seam of the
;;;; channel kit, so every reaction someone leaves on a bot message becomes
;;;; one row of a JSON-lines ledger under the home.
;;;;
;;;; Ported from the Hermes plugin reaction-ledger (see NOTICE).

(defsystem "nodecode-reaction-ledger"
  :description "Reaction Ledger: the emoji reactions left in your chats, by emoji, chat and day"
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "ledger")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-reaction-ledger/test"))))

(defsystem "nodecode-reaction-ledger/test"
  :description "Reaction Ledger tests. Registered into the shared nodecode.test registry; RUN-REACTION-LEDGER-TESTS filters by the REACTION-LEDGER-CELL- name prefix."
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-reaction-ledger" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-reaction-ledger-tests)))
