;;;; nodecode-freshdesk.asd --- Freshdesk tickets as eleven tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `freshdesk' section, registers the eleven tools and puts the
;;;; primer on (help :freshdesk).
;;;;
;;;; Rewritten from aedelgod's Hermes Agent plugin of the same name; see
;;;; NOTICE. Every dependency rides with nodecode: nlk:http speaks to the
;;;; account's own origin, quri encodes the query, ppcre reads the patterns.

(defsystem "nodecode-freshdesk"
  :description "Freshdesk: list, search and read tickets, contacts and conversations, and write notes, replies and time entries"
  :author "aedelgod (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-freshdesk/test"))))

(defsystem "nodecode-freshdesk/test"
  :description "Freshdesk tests. Registered into the shared nodecode.test registry; RUN-FRESHDESK-TESTS filters by the FRESHDESK-CELL- name prefix."
  :author "aedelgod (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-freshdesk" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-freshdesk-tests)))
