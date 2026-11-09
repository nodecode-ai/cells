;;;; nodecode-eventforge.asd --- one EventForge control plane, as ten tools and a command.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core. The folder loader loads it at
;;;; boot when its directory sits under ~/.nodecode/cells/; the gateway calls
;;;; START-CELL after recovery, which reads the `eventforge' section,
;;;; registers the ten tools and the /eventforge command, and puts the
;;;; primer on (help :eventforge).
;;;;
;;;; Rewritten from sinequix's Hermes Agent plugin hermes-eventforge; see
;;;; NOTICE. nlk:http speaks the control plane's HTTP routes and the JSON
;;;; waist reads what it answers.

(defsystem "nodecode-eventforge"
  :description "EventForge: verified provider events, workspace memory, pending action approvals and workflows from one self-hosted control plane"
  :author "sinequix (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :version "0.1.0"
  :depends-on ("nodecode" "quri")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-eventforge/test"))))

(defsystem "nodecode-eventforge/test"
  :description "EventForge tests. Registered into the shared nodecode.test registry; RUN-EVENTFORGE-TESTS filters by the EVENTFORGE-CELL- name prefix."
  :author "sinequix (original), Nodecode (rewrite)"
  :license "Apache-2.0"
  :depends-on ("nodecode-eventforge" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-eventforge-tests)))
