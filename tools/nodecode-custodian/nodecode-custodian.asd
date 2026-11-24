;;;; nodecode-custodian.asd --- known-failure fingerprints over the logs you watch.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `custodian' section, registers the three tools and the
;;;; /custodian command, and puts the manual on (help :custodian).
;;;;
;;;; See NOTICE: this folder is a rewrite of indigokarasu/hermes-custodian-plugin.

(defsystem "nodecode-custodian"
  :description "Custodian: known-failure fingerprints over the log files you watch, and an issue ledger"
  :author "indigokarasu (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-custodian/test"))))

(defsystem "nodecode-custodian/test"
  :description "Custodian cell tests. Registered into the shared nodecode.test registry; RUN-CUSTODIAN-TESTS filters by the CUSTODIAN-CELL- name prefix."
  :author "indigokarasu (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-custodian" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-custodian-tests)))
