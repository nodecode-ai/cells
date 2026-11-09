;;;; nodecode-garmin.asd --- Garmin Connect health data, read-only, through
;;;; Garmin's own Personal API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `garmin' section, registers the fourteen read-only tools
;;;; cell.lisp lists, puts (help :garmin) on the manual and registers
;;;; /garmin. Every dependency rides with nodecode (NLK:HTTP is its HTTP
;;;; seam: nothing here is bundled).

(defsystem "nodecode-garmin"
  :description "Garmin Connect: sleep, HRV, Body Battery, stress, steps, heart rate, activities and trends, read-only"
  :license "MIT"
  :author "adrianlaborda (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "tools")
               (:file "trends")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-garmin/test"))))

(defsystem "nodecode-garmin/test"
  :description "Garmin tests. Registered into the shared nodecode.test registry; RUN-GARMIN-TESTS filters by the GARMIN-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-garmin" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-garmin-tests)))
