;;;; nodecode-apple-health.asd --- Apple Health: your export, as seven tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `applehealth' section, registers the seven tools and puts the
;;;; primer on (help :apple-health).
;;;;
;;;; Rewritten from davidmosiah's Hermes Agent plugin of the same name; see
;;;; NOTICE. The export is read from disk, one element at a time, and nothing
;;;; here dials anything: a cell with no network path at all.

(defsystem "nodecode-apple-health"
  :description "Apple Health: daily and weekly summaries, records and workouts from your own Health export"
  :author "davidmosiah (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "contracts")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-apple-health/test"))))

(defsystem "nodecode-apple-health/test"
  :description "Apple Health tests. Registered into the shared nodecode.test registry; RUN-APPLE-HEALTH-TESTS filters by the APPLE-HEALTH-CELL- name prefix."
  :author "davidmosiah (original), Nodecode (rewrite)"
  :license "MIT"
  :depends-on ("nodecode-apple-health" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-apple-health-tests)))
