;;;; nodecode-mood.asd --- a small evolving mood the model colours its tone with.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode cell, NOT part of the organism core: nothing in Nodecode names
;;;; this system. A rewrite of huy-sec/hermes-mood's Hermes plugin (see
;;;; NOTICE). The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls START-CELL after recovery,
;;;; which reads the `mood' section, loads the mood from its file under the
;;;; home, hooks the :ROUND point so the line rides the first request of every
;;;; turn, and registers /mood and the manual on (help :mood).

(defsystem "nodecode-mood"
  :description "A small evolving mood, rapport and tone traits, as one line a turn"
  :license "MIT"
  :author "huy-sec (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-mood/test"))))

(defsystem "nodecode-mood/test"
  :description "Mood tests. Registered into the shared nodecode.test registry; RUN-MOOD-TESTS filters by the MOOD-CELL- name prefix."
  :license "MIT"
  :author "huy-sec (original), Nodecode (rewrite)"
  :depends-on ("nodecode-mood" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-mood-tests)))
