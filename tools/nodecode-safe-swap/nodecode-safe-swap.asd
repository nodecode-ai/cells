;;;; nodecode-safe-swap.asd --- the cell, and its tests.
;;;;
;;;; SPDX-License-Identifier: MIT

(defsystem "nodecode-safe-swap"
  :description "Swap destructive shell commands for reversible twins: rm -r to quarantine, force push to a lease."
  :license "MIT"
  :author "itsolaph (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "swap")
               (:file "receipts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-safe-swap/test"))))

(defsystem "nodecode-safe-swap/test"
  :description "The safe-swap cell's own tests, offline."
  :license "MIT"
  :depends-on ("nodecode-safe-swap" "nodecode/test")
  :components ((:file "test/cell-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-safe-swap-tests)))
