;;;; nodecode-system-one.asd --- Local System One's routing gates, as a cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `system-one' section, registers the system_one_route and
;;;; system_one_notify tools and the /system-one command, and puts
;;;; (help :system-one) on the manual.
;;;;
;;;; A Nodecode rewrite of integrations/hermes/local-system-one-hermes in
;;;; NMTZ-z/system-one-benchmark-lab (see NOTICE).

(defsystem "nodecode-system-one"
  :description "Local System One gates: whether a task needs the web, its model tier, and whether an event should notify"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "NMTZ-z (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-system-one/test"))))

(defsystem "nodecode-system-one/test"
  :description "Local System One cell tests. Registered into the shared nodecode.test registry; RUN-SYSTEM-ONE-TESTS filters by the SYSTEM-ONE-CELL- name prefix."
  :license "Apache-2.0"
  :author "NMTZ-z (original), Nodecode (rewrite)"
  :depends-on ("nodecode-system-one" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-system-one-tests)))
