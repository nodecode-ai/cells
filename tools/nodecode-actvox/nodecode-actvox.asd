;;;; nodecode-actvox.asd --- the ActVox inbox, read through your own CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ActVox's Hermes plugin. Optional ASDF cell, not part of the
;;;; organism core: the folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/, and the gateway calls START-CELL after recovery,
;;;; which reads the `actvox' section, hooks the :ROUND point so a turn opens
;;;; with what waits, registers the actvox_inbox tool and /actvox, and puts the
;;;; manual on (help :actvox).

(defsystem "nodecode-actvox"
  :description "ActVox inbox: tasks and mentions that wait for you, at the start of a turn and on demand"
  :license "MIT"
  :version "0.1.0"
  :author "ActVox (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-actvox/test"))))

(defsystem "nodecode-actvox/test"
  :description "ActVox tests. Registered into the shared nodecode.test registry; RUN-ACTVOX-TESTS filters by the ACTVOX-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-actvox" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-actvox-tests)))
