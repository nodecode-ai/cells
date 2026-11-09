;;;; nodecode-jot.asd --- Jot: notes beside the chat, as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `jot' section, registers the six jot_* tools and /jot, and
;;;; puts (help :jot) on the manual.
;;;;
;;;; A Nodecode rewrite of Totoro-qaq/hermes-jot (see NOTICE).

(defsystem "nodecode-jot"
  :description "Jot: notes, checklists and documents the model can read and update, with revision checks"
  :license "MIT"
  :version "0.1.0"
  :author "Totoro-qaq (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-jot/test"))))

(defsystem "nodecode-jot/test"
  :description "Jot tests. Registered into the shared nodecode.test registry; RUN-JOT-TESTS filters by the JOT-CELL- name prefix."
  :license "MIT"
  :version "0.1.0"
  :author "Totoro-qaq (original), Nodecode (rewrite)"
  :depends-on ("nodecode-jot" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-jot-tests)))
