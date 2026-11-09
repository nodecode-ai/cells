;;;; nodecode-tokenjuice.asd --- terminal output compaction, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of vincentkoc/tokenjuice's Hermes plugin, which
;;;; registers one hook on captured terminal output and answers it shortened.
;;;; Here the hook is the core's :TOOL point: a call that runs a shell command
;;;; answers through COMPACT-TOOL-CALL, which folds the output the model reads.

(defsystem "nodecode-tokenjuice"
  :description "Compact successful terminal output before the model reads it"
  :license "MIT"
  :version "0.1.0"
  :author "vincentkoc (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tokenjuice/test"))))

(defsystem "nodecode-tokenjuice/test"
  :description "Tokenjuice cell tests. Registered into the shared nodecode.test registry; RUN-TOKENJUICE-TESTS filters by the TOKENJUICE name prefix."
  :license "MIT"
  :author "vincentkoc (original), Nodecode (rewrite)"
  :depends-on ("nodecode-tokenjuice" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tokenjuice-tests)))
