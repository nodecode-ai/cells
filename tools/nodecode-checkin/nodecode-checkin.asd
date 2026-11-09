;;;; nodecode-checkin.asd --- idle check-ins as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in src/ names
;;;; this system. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `checkins' section, binds the session it names, starts
;;;; the ticker, and registers the checkin tool, /checkin, (help :checkin)
;;;; and the two hooks. Every dependency is already in the serving image.

(defsystem "nodecode-checkin"
  :description "Idle check-ins: after a chat goes quiet, one prompt into the bound session, the wait doubling per unanswered one and waking hours kept"
  :author "capthvnsen (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "state")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-checkin/test"))))

(defsystem "nodecode-checkin/test"
  :description "Check-in tests. Registered into the shared nodecode.test registry; RUN-CHECKIN-TESTS filters by the CHECKIN-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-checkin" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-checkin-tests)))
