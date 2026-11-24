;;;; nodecode-agenda-guardian.asd --- a day planner that does not rely on discipline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `agenda-guardian' section, registers the five agenda_* tools
;;;; and /agenda-guardian, and puts (help :agenda-guardian) on the manual.
;;;;
;;;; A Nodecode rewrite of Moodtuner997/hermes-agenda-guardian (see NOTICE).

(defsystem "nodecode-agenda-guardian"
  :description "Agenda Guardian: a day planned around your rules, your calendar and your own real times"
  :license "MIT"
  :version "0.1.0"
  :author "Moodtuner997 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-agenda-guardian/test"))))

(defsystem "nodecode-agenda-guardian/test"
  :description "Agenda Guardian tests. Registered into the shared nodecode.test registry; RUN-AGENDA-GUARDIAN-TESTS filters by the AGENDA-GUARDIAN-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Moodtuner997 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-agenda-guardian" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-agenda-guardian-tests)))
