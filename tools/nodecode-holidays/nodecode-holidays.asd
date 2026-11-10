;;;; nodecode-holidays.asd --- public holidays and workday math, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `holidays' section, registers holidays_list, workday_check,
;;;; workdays_count and workdays_add, adds /holidays, and puts
;;;; (help :holidays) on the manual.
;;;;
;;;; A Nodecode rewrite of Akloenx123/hermes-public-holidays (see NOTICE).

(defsystem "nodecode-holidays"
  :description "Public holidays for a country and year, with workday checks, counts and deadline math"
  :license "MIT"
  :version "0.1.0"
  :author "Akloenx123 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-holidays/test"))))

(defsystem "nodecode-holidays/test"
  :description "Holidays cell tests. Registered into the shared nodecode.test registry; RUN-HOLIDAYS-TESTS filters by the HOLIDAYS-CELL- name prefix."
  :license "MIT"
  :author "Akloenx123 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-holidays" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-holidays-tests)))
