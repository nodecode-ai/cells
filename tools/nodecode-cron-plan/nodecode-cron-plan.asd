;;;; nodecode-cron-plan.asd --- a read-only plan for tidying a crowded cron list.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/src
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `cron-plan' section, registers /cron-plan and
;;;; puts the manual on (help :cron-plan).
;;;;
;;;; A Nodecode rewrite of Moodtuner997's hermes-cron-consolidator: the same
;;;; read-only report, in Common Lisp, with no Python and no Hermes. It reads
;;;; a cron folder's jobs.json and usage_audit.jsonl and changes nothing.
;;;; Every dependency is already in the serving image.

(defsystem "nodecode-cron-plan"
  :description "A read-only plan for merging and rescheduling cron jobs"
  :license "MIT"
  :author "Moodtuner997 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cronfields")
               (:file "plan")
               (:file "report")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-cron-plan/test"))))

(defsystem "nodecode-cron-plan/test"
  :description "Cron plan tests. Registered into the shared nodecode.test registry; RUN-CRON-PLAN-TESTS filters by the CRON-PLAN-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-cron-plan" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cronfields-test")
               (:file "plan-test")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-cron-plan-tests)))
