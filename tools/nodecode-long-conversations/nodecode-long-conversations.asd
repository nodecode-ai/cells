;;;; nodecode-long-conversations.asd --- handoff briefs across sessions, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `handoff' section, registers the four handoff_* tools and
;;;; /long-conversations, puts (help :handoff) on the manual, and hooks the :ROUND point
;;;; so a saved brief reaches the next session.
;;;;
;;;; A Nodecode rewrite of cspears32/hermes-long-conversations (see NOTICE).

(defsystem "nodecode-long-conversations"
  :description "Handoff briefs that carry a session's state past its context limit"
  :license "MIT"
  :version "0.1.0"
  :author "cspears32 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-long-conversations/test"))))

(defsystem "nodecode-long-conversations/test"
  :description "Handoff tests. Registered into the shared nodecode.test registry; RUN-HANDOFF-TESTS filters by the HANDOFF-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "cspears32 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-long-conversations" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-long-conversations-tests)))
