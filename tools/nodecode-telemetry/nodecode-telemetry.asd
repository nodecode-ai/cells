;;;; nodecode-telemetry.asd --- what a session spent, and the money gate.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `telemetry' section, hooks the :frame, :tool and :round
;;;; points and registers /stats, /budget and the telemetry tool. See
;;;; README.md beside this file.

(defsystem "nodecode-telemetry"
  :description "Spend telemetry: tokens, cost, latency and tool calls per session and day in a local ledger, with daily and monthly budget caps"
  :license "MIT"
  :author "nujovich (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-telemetry/test"))))

(defsystem "nodecode-telemetry/test"
  :description "Telemetry tests. Registered into the shared nodecode.test registry; RUN-TELEMETRY-TESTS filters by the TELEMETRY-CELL- name prefix."
  :license "MIT"
  :author "nujovich (original), Nodecode (rewrite)"
  :depends-on ("nodecode-telemetry" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-telemetry-tests)))
