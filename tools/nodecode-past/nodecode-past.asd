;;;; nodecode-past.asd --- past.dev memory as a Nodecode cell.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `past' section, puts what past remembers in front of each turn,
;;;; registers past_recall and /past, and keeps each session in past, cut into
;;;; sittings, from the :frame point.
;;;;
;;;; A Nodecode rewrite of pastdotdev's Hermes plugin (see NOTICE).

(defsystem "nodecode-past"
  :description "past.dev memory: recalled before each turn, and conversations kept there in sittings"
  :license "MIT"
  :version "0.1.0"
  :author "pastdotdev (original), Nodecode (rewrite)"
  :depends-on ("nodecode" "cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "client")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-past/test"))))

(defsystem "nodecode-past/test"
  :description "past.dev memory tests. Registered into the shared nodecode.test registry; RUN-PAST-TESTS filters by the PAST-CELL- name prefix."
  :license "MIT"
  :author "pastdotdev (original), Nodecode (rewrite)"
  :depends-on ("nodecode-past" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-past-tests)))
