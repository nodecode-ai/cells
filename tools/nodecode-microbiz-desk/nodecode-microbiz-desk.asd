;;;; nodecode-microbiz-desk.asd --- customers, sales, debts and a weekly report.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `microbiz-desk' section, registers the five microbiz_* tools
;;;; and /microbiz-desk, and puts (help :microbiz-desk) on the manual.
;;;;
;;;; A Nodecode rewrite of Danzilla0/microbiz-desk (see NOTICE).

(defsystem "nodecode-microbiz-desk"
  :description "MicroBiz Desk: customers, sales, debts and a weekly report, offline in one local file"
  :license "MIT"
  :version "0.1.0"
  :author "Danzilla0 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-microbiz-desk/test"))))

(defsystem "nodecode-microbiz-desk/test"
  :description "MicroBiz Desk tests. Registered into the shared nodecode.test registry; RUN-MICROBIZ-DESK-TESTS filters by the MICROBIZ-DESK-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Danzilla0 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-microbiz-desk" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-microbiz-desk-tests)))
