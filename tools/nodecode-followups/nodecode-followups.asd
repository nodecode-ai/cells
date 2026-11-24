;;;; nodecode-followups.asd --- a follow-up board that comes back at the right moment.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of xiaok/hermes-autonomous (MIT).
;;;;
;;;; Optional ASDF cell, NOT part of the organism core: nothing in Nodecode's
;;;; src names this system. The folder loader loads it at boot when its
;;;; directory sits under ~/.nodecode/cells/; the gateway calls START-CELL
;;;; after recovery, which reads the `followups' section, registers the four
;;;; follow-up tools and /followups, puts the manual on (help :followups),
;;;; and hooks the :ROUND point so a follow-up whose time has come arrives as
;;;; one reminder at the next model request. The board is one JSON file under
;;;; the home. Every dependency is already in the serving image.

(defsystem "nodecode-followups"
  :description "A follow-up board that comes back at the right moment"
  :license "MIT"
  :version "0.1.0"
  :author "xiaok (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-followups/test"))))

(defsystem "nodecode-followups/test"
  :description "Follow-up board tests. Registered into the shared nodecode.test registry; RUN-FOLLOWUPS-TESTS filters by the FOLLOWUPS-CELL- name prefix."
  :license "MIT"
  :author "xiaok (original), Nodecode (rewrite)"
  :depends-on ("nodecode-followups" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-followups-tests)))
