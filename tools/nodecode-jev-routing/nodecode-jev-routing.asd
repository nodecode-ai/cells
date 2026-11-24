;;;; nodecode-jev-routing.asd --- per-prompt model routing from TypeSafe Jev.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. Ported from the Hermes Agent plugin hermes-jev-routing (see
;;;; NOTICE): the four routing questions, the tier composition, the chain
;;;; pick and the spend ledger, as functions the model calls through eval.

(defsystem "nodecode-jev-routing"
  :description "Jev routing: the model tier a prompt deserves, and the model your routing table names for it"
  :license "MIT"
  :version "0.1.0"
  :author "apoapostolov (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "routing")
               (:file "decide")
               (:file "jev")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jev-routing/test"))))

(defsystem "nodecode-jev-routing/test"
  :description "Jev routing tests. Registered into the shared nodecode.test registry; RUN-JEV-ROUTING-TESTS filters by the JEV-ROUTING- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "apoapostolov (original), Nodecode (rewrite)"
  :depends-on ("nodecode-jev-routing" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jev-routing-tests)))
