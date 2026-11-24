;;;; nodecode-hermes-herald.asd --- one Hermes Agent API server as six tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `hermes' section, registers the six tools and puts the primer on
;;;; (help :hermes-herald).
;;;;
;;;; Rewritten from bennybuoy's Hermes Agent plugin hermes-herald; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to the API server
;;;; and the JSON waist reads what it answers.

(defsystem "nodecode-hermes-herald"
  :description "Hermes Herald: dispatch tasks to a Hermes Agent API server, poll, cancel, chat and list its models"
  :author "bennybuoy (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-hermes-herald/test"))))

(defsystem "nodecode-hermes-herald/test"
  :description "Hermes Herald tests. Registered into the shared nodecode.test registry; RUN-HERMES-HERALD-TESTS filters by the HERMES-HERALD-CELL- name prefix."
  :author "bennybuoy (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-hermes-herald" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-hermes-herald-tests)))
