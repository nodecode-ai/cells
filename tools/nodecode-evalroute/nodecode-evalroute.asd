;;;; nodecode-evalroute.asd --- Evalroute: a task goes to a model and an effort, with a ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `evalroute' section, registers the evalroute_route tool, /route
;;;; and /rate, and puts (help :evalroute) on the manual.
;;;;
;;;; A Nodecode rewrite of keppy/hermes-plugin-evalroute (see NOTICE).

(defsystem "nodecode-evalroute"
  :description "Evalroute: route a task to a model and a reasoning effort, and label the outcome"
  :license "MIT"
  :version "0.1.0"
  :author "keppy (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-evalroute/test"))))

(defsystem "nodecode-evalroute/test"
  :description "Evalroute tests. Registered into the shared nodecode.test registry; RUN-EVALROUTE-TESTS filters by the EVALROUTE-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "keppy (original), Nodecode (rewrite)"
  :depends-on ("nodecode-evalroute" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-evalroute-tests)))
