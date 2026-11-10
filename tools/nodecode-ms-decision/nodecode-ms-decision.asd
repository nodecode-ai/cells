;;;; nodecode-ms-decision.asd --- Microsoft-Decision-1 typed decisions, as tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `ms-decision' section, registers four decision tools, puts
;;;; (help :ms-decision) on the manual and registers /ms-decision.
;;;;
;;;; A rewrite of guglxni's ms-decision-provider (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-ms-decision"
  :description "Microsoft-Decision-1: typed decisions (yes/no, choice, score) over Azure AI Foundry, as tools"
  :license "MIT"
  :version "0.1.0"
  :author "guglxni (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "decision")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ms-decision/test"))))

(defsystem "nodecode-ms-decision/test"
  :description "ms-decision tests. Registered into the shared nodecode.test registry; RUN-MS-DECISION-TESTS filters by the MS-DECISION-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-ms-decision" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ms-decision-tests)))
