;;;; nodecode-monid.asd --- Monid's paid tool marketplace as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `monid' section, registers the six monid_* tools and /monid,
;;;; and puts the primer on (help :monid).
;;;;
;;;; A Nodecode rewrite of monid-ai/hermes-plugin (see NOTICE).

(defsystem "nodecode-monid"
  :description "Monid: a marketplace of paid third-party tools, discovered, inspected and run from one balance"
  :license "MIT"
  :version "0.1.0"
  :author "monid-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-monid/test"))))

(defsystem "nodecode-monid/test"
  :description "Monid tests. Registered into the shared nodecode.test registry; RUN-MONID-TESTS filters by the MONID-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "monid-ai (original), Nodecode (rewrite)"
  :depends-on ("nodecode-monid" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-monid-tests)))
