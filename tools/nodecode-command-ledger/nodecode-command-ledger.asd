;;;; nodecode-command-ledger.asd --- a local ledger of the slash commands you run.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `command-ledger' section, puts (help :command-ledger) on the
;;;; manual, registers /command-log, and hooks the two functions a slash line
;;;; passes through, so every command run becomes one row of a JSON-lines
;;;; ledger under the home.
;;;;
;;;; Ported from the Hermes plugin command-ledger (see NOTICE).

(defsystem "nodecode-command-ledger"
  :description "Command Ledger: which slash commands you run, how often and when last"
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "ledger")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-command-ledger/test"))))

(defsystem "nodecode-command-ledger/test"
  :description "Command Ledger tests. Registered into the shared nodecode.test registry; RUN-COMMAND-LEDGER-TESTS filters by the COMMAND-LEDGER-CELL- name prefix."
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-command-ledger" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-command-ledger-tests)))
