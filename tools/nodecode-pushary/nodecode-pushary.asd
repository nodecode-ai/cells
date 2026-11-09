;;;; nodecode-pushary.asd --- Pushary on the operator's phone: six tools and a command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `pushary' section, registers the six pushary_* tools and the
;;;; /pushary command, and puts the primer on (help :pushary).
;;;;
;;;; Rewritten from Pushary's Hermes Agent plugin of the same name; see NOTICE.
;;;; Every dependency rides with nodecode: nlk:http speaks to Pushary's MCP
;;;; endpoint and nlk's JSON waist builds and reads each answer.

(defsystem "nodecode-pushary"
  :description "Pushary: push notifications, questions and scope approvals on your phone"
  :author "Pushary (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pushary/test"))))

(defsystem "nodecode-pushary/test"
  :description "Pushary tests. Registered into the shared nodecode.test registry; RUN-PUSHARY-TESTS filters by the PUSHARY-CELL- name prefix."
  :author "Pushary (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-pushary" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pushary-tests)))
