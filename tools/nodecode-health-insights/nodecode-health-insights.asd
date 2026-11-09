;;;; nodecode-health-insights.asd --- Apple Health trends, concerns and coverage.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of mwdearing/hermes-health-insights: one
;;;; NLE:DEFINE-CELL form declares the `health_insights' section, the
;;;; health_insights tool, /vitals and the manual, and REPORT.LISP holds the
;;;; numbers -- daily means from the receiver's SQLite database, a 7-day
;;;; window against a 28-day MAD baseline, and the concern rules.

(defsystem "nodecode-health-insights"
  :description "Apple Health trends, concern checks and data coverage from a receiver database"
  :license "Apache-2.0"
  :author "mwdearing (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "report")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-health-insights/test"))))

(defsystem "nodecode-health-insights/test"
  :description "Health-insights tests. Registered into the shared nodecode.test registry; RUN-HEALTH-INSIGHTS-TESTS filters by the HEALTH-INSIGHTS-CELL- name prefix."
  :license "Apache-2.0"
  :author "mwdearing (original), Nodecode (rewrite)"
  :depends-on ("nodecode-health-insights" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-health-insights-tests)))
