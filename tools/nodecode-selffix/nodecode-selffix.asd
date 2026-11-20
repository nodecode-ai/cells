;;;; nodecode-selffix.asd --- idle self-fixes, observe level: a failure that recurs becomes a finding with a failing test.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional cell, not part of the organism core. It rides the cron cell's
;;;; wake job: no clock, gate on money or stop of its own beyond what the wake
;;;; already holds, only one more source on *WAKE-SOURCES* that, when the home
;;;; is idle, turns the most frequent failure the home recorded into a finding
;;;; with a failing test, written under ~/.nodecode/selffix/. It changes no code.

(defsystem "nodecode-selffix"
  :description "Idle self-fixes, observe level: findings with a failing test"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode" "nodecode-cron" "sb-posix" "cl-base64")
  :serial t
  :components ((:file "package")
               (:file "backlog")
               (:file "ledger")
               (:file "capture")
               (:file "repro")
               (:file "runner")
               (:file "gate")
               (:file "cycle")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-selffix/test"))))

(defsystem "nodecode-selffix/test"
  :description "Selffix tests. Registered into the shared nodecode.test registry under the SELFFIX-CELL- prefix."
  :license "MIT"
  :depends-on ("nodecode-selffix" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "backlog-test")
               (:file "ledger-test")
               (:file "capture-test")
               (:file "repro-test")
               (:file "runner-test")
               (:file "gate-test")
               (:file "cycle-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-selffix-tests)))
