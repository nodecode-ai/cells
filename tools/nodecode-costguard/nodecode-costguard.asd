;;;; nodecode-costguard.asd --- a brake pedal for model spend.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `costguard' section, registers the costguard_status tool and
;;;; /cost, puts (help :costguard) on the manual, and hooks the round point
;;;; (the warning) and the tool point (the brake).
;;;;
;;;; Rewritten from Whydiriki700/costguard (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-costguard"
  :description "CostGuard: meters token spend per session and day, warns near a budget, and refuses tool calls past it"
  :license "MIT"
  :author "Whydiriki700 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-costguard/test"))))

(defsystem "nodecode-costguard/test"
  :description "CostGuard tests. Registered into the shared nodecode.test registry; RUN-COSTGUARD-TESTS filters by the COSTGUARD-CELL- name prefix."
  :license "MIT"
  :author "Whydiriki700 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-costguard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-costguard-tests)))
