;;;; nodecode-gatekeeper.asd --- consent, opt-out, quiet hours and wording for sends.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `gatekeeper' section, registers the gatekeeper_check and
;;;; gatekeeper_record tools, puts /gatekeeper and the manual on, and derives
;;;; its stop from them.
;;;;
;;;; No network, no subprocess, no model call: the ledger is one local file.

(defsystem "nodecode-gatekeeper"
  :description "Gatekeeper: consent, opt-out, quiet-hours and wording rules for outbound messages"
  :license "MIT"
  :version "0.1.0"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "policy")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-gatekeeper/test"))))

(defsystem "nodecode-gatekeeper/test"
  :description "Gatekeeper tests. Registered into the shared nodecode.test registry; RUN-GATEKEEPER-TESTS filters by the GATEKEEPER-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "percyhendrux123-prog (original), Nodecode (rewrite)"
  :depends-on ("nodecode-gatekeeper" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-gatekeeper-tests)))
