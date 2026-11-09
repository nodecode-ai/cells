;;;; nodecode-flightlog.asd --- a flight recorder for a session's turns.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, not part of the organism core: the folder loader
;;;; loads it at boot when its directory sits under ~/.nodecode/cells/, and
;;;; the gateway calls START-CELL after recovery. See README.md beside this
;;;; file.

(defsystem "nodecode-flightlog"
  :description "A flight recorder: every turn recorded locally, and the runs that finished green but went wrong flagged"
  :license "MIT"
  :author "piyushpradhan (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "store")
               (:file "rules")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-flightlog/test"))))

(defsystem "nodecode-flightlog/test"
  :description "Flightlog tests. Registered into the shared nodecode.test registry; RUN-FLIGHTLOG-TESTS filters by the FLIGHTLOG-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-flightlog" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-flightlog-tests)))
