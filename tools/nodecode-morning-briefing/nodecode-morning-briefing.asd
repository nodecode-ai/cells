;;;; nodecode-morning-briefing.asd --- one newsprint page a morning: the day's
;;;; weather and the last day and a half of headlines.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `morning-briefing' section, puts (help :briefing) on the manual,
;;;; and registers the morning_briefing tool and the /briefing command.

(defsystem "nodecode-morning-briefing"
  :description "Morning briefing: the day's weather and the last 36 hours of headlines as one printable newsprint page"
  :license "MIT"
  :author "drkpxl (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-morning-briefing/test"))))

(defsystem "nodecode-morning-briefing/test"
  :description "Morning briefing tests. Registered into the shared nodecode.test registry; RUN-MORNING-BRIEFING-TESTS filters by the MORNING-BRIEFING-CELL- name prefix."
  :license "MIT"
  :author "drkpxl (original), Nodecode (rewrite)"
  :depends-on ("nodecode-morning-briefing" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-morning-briefing-tests)))
