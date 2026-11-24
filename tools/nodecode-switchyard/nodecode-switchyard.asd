;;;; nodecode-switchyard.asd --- Switchyard's routing decisions, as a tool the model calls.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `switchyard' section, puts (help :switchyard) on the manual,
;;;; registers /switchyard and the switchyard_route tool.
;;;;
;;;; Rewritten from bgrablin/hermes-switchyard (see NOTICE). Every dependency
;;;; rides with nodecode.

(defsystem "nodecode-switchyard"
  :description "Switchyard: which skill fits this task, and how much reasoning effort it needs"
  :license "MIT"
  :author "bgrablin (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "route")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-switchyard/test"))))

(defsystem "nodecode-switchyard/test"
  :description "Switchyard tests. Registered into the shared nodecode.test registry; RUN-SWITCHYARD-TESTS filters by the SWITCHYARD-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-switchyard" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-switchyard-tests)))
