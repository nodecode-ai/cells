;;;; nodecode-cognition.asd --- cognition memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cognition' section, hooks the ingress and the harness sections,
;;;; registers cognition_search, cognition_status and cognition_dream, puts
;;;; (help :cognition) on the manual and registers /cognition.
;;;;
;;;; A Nodecode rewrite of aska-digital's cognition-memory Hermes plugin (see NOTICE).

(defsystem "nodecode-cognition"
  :description "Cognition memory: durable facts from your own messages, kept locally with provenance and recalled under a budget"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "aska-digital (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cognition/test"))))

(defsystem "nodecode-cognition/test"
  :description "Cognition tests. Registered into the shared nodecode.test registry; RUN-COGNITION-TESTS filters by the COGNITION-CELL- name prefix."
  :license "Apache-2.0"
  :version "0.1.0"
  :author "aska-digital (original), Nodecode (rewrite)"
  :depends-on ("nodecode-cognition" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cognition-tests)))
