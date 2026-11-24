;;;; nodecode-cron-gate.asd --- a Jev gate before a scheduled agent run.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `cron-gate' section, puts (help :cron-gate) on the manual,
;;;; registers the cron_gate tool and /cron-gate.
;;;;
;;;; Ported from superlowburn's hermes-jev-cron-gate (see NOTICE). Every
;;;; dependency rides with nodecode.

(defsystem "nodecode-cron-gate"
  :description "Cron gate: ask TypeSafe Jev whether a scheduled run would do anything"
  :license "MIT"
  :author "superlowburn (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "gate")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cron-gate/test"))))

(defsystem "nodecode-cron-gate/test"
  :description "Cron gate tests; RUN-CRON-GATE-TESTS filters by the CRON-GATE-CELL- name prefix."
  :license "MIT"
  :author "superlowburn (original), Nodecode (rewrite)"
  :depends-on ("nodecode-cron-gate" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cron-gate-tests)))
