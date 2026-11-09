;;;; nodecode-effort-router.asd --- one route a turn: model and reasoning effort.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `effort-router' section, registers the two tools and
;;;; /effort-router, puts the primer on (help :effort-router) and advises
;;;; NLE::SNAPSHOT-CONTEXT-REQUEST with the route a turn should take.
;;;;
;;;; A Nodecode rewrite of AlphaPerseii3000's jev-effort-router (see NOTICE).
;;;; Every dependency rides with nodecode: nlk:http is the decision call.

(defsystem "nodecode-effort-router"
  :description "Effort routing: each turn goes to the model and reasoning effort a decision model picks for it"
  :author "AlphaPerseii3000 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-effort-router/test"))))

(defsystem "nodecode-effort-router/test"
  :description "Effort router tests. Registered into the shared nodecode.test registry; RUN-EFFORT-ROUTER-TESTS filters by the EFFORT-ROUTER-CELL- name prefix."
  :author "AlphaPerseii3000 (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-effort-router" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-effort-router-tests)))
