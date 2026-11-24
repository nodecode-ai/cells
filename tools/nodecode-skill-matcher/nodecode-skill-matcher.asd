;;;; nodecode-skill-matcher.asd --- a task-to-skill router, as a cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `skill-matcher' section and puts match_skills, /skill-matcher and
;;;; (help :skill-matcher) in reach. A rewrite of xXLODXx's hermes-skill-router;
;;;; see NOTICE.

(defsystem "nodecode-skill-matcher"
  :description "Route a task to the installed cells and skills that fit it, with the evidence for each match"
  :license "MIT"
  :author "xXLODXx (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-skill-matcher/test"))))

(defsystem "nodecode-skill-matcher/test"
  :description "Routing, scoring, logging, lifecycle. Registered into the shared nodecode.test registry; RUN-SKILL-MATCHER-TESTS filters by the SKILL-MATCHER- name prefix."
  :license "MIT"
  :author "xXLODXx (original), Nodecode (rewrite)"
  :depends-on ("nodecode-skill-matcher" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-skill-matcher-tests)))
