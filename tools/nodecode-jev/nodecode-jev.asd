;;;; nodecode-jev.asd --- Jev's typed judgments, as functions the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `jev' section, puts (help :jev) on the manual and registers
;;;; /jev.
;;;;
;;;; A rewrite of ourines/hermes-jev (see NOTICE). Every dependency rides with
;;;; nodecode.

(defsystem "nodecode-jev"
  :description "Jev: advisory typed judgments (triage, next step, relevance) called through eval"
  :license "MIT"
  :version "0.1.0"
  :author "ourines (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "jev")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jev/test"))))

(defsystem "nodecode-jev/test"
  :description "Jev tests. Registered into the shared nodecode.test registry; RUN-JEV-TESTS filters by the JEV-CELL- name prefix."
  :license "MIT"
  :author "ourines (original), Nodecode (rewrite)"
  :depends-on ("nodecode-jev" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jev-tests)))
