;;;; nodecode-memlock.asd --- pinned standing instructions, re-asserted every turn.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of hermes-memlock as a Nodecode cell: a pin is one instruction
;;;; the operator wants kept, kept in a JSON file under the home, and put in
;;;; front of the model at the first request of every turn -- so it outlives
;;;; the conversation being folded, thinned or edited away.

(defsystem "nodecode-memlock"
  :description "Standing instructions the operator pins, re-asserted at every turn"
  :license "MIT"
  :version "0.1.0"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-memlock/test"))))

(defsystem "nodecode-memlock/test"
  :description "MemLock tests. Registered into the shared nodecode.test registry; RUN-MEMLOCK-TESTS filters by the MEMLOCK-CELL- name prefix."
  :license "MIT"
  :author "Sahil-SS9 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-memlock" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-memlock-tests)))
