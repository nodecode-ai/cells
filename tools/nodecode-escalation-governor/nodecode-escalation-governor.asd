;;;; nodecode-escalation-governor.asd --- a stuck-session escalator.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of jusst01/escalation-governor. Optional ASDF cell, not part of
;;;; the organism core: the folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/, and the gateway calls START-CELL after
;;;; recovery, which reads the `escalation-governor' section, watches the
;;;; :TOOL, :ROUND and provider-call seams for a session that is measurably
;;;; stuck, and puts an escalation packet in the model's context.

(defsystem "nodecode-escalation-governor"
  :description "Escalates a session the moment it is measurably stuck, with a packet of what was tried"
  :license "MIT"
  :author "jusst01 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-escalation-governor/test"))))

(defsystem "nodecode-escalation-governor/test"
  :description "Escalation-governor tests. Registered into the shared nodecode.test registry; RUN-ESCALATION-GOVERNOR-TESTS filters by the ESCALATION-GOVERNOR-CELL- name prefix."
  :license "MIT"
  :author "jusst01 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-escalation-governor" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-escalation-governor-tests)))
