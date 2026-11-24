;;;; nodecode-edit-ledger.asd --- a ledger of the turns that changed files.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of olric3015's edit-ledger plugin for Hermes Agent. Optional
;;;; cell, not part of the organism core: the folder loader loads it at boot
;;;; when its directory sits under ~/.nodecode/cells/, and the gateway calls
;;;; START-CELL after recovery, which installs one :FRAME observer, appends a
;;;; row for every turn that changed files, and registers /edit-log.

(defsystem "nodecode-edit-ledger"
  :description "Logs turns where the agent changed files and shows /edit-log by model, day and file type"
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "ledger")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-edit-ledger/test"))))

(defsystem "nodecode-edit-ledger/test"
  :description "Edit-ledger tests. Registered into the shared nodecode.test registry; RUN-EDIT-LEDGER-TESTS filters by the EDIT-LEDGER-CELL- name prefix."
  :license "MIT"
  :author "olric3015 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-edit-ledger" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-edit-ledger-tests)))
