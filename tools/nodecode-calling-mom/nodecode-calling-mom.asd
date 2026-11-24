;;;; nodecode-calling-mom.asd --- a cheap advisor that answers when the work is stuck.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of SlyBase/calling-mom. A peripheral, not part of the
;;;; organism core: the folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/, and the gateway calls START-CELL after
;;;; recovery, which reads the `calling-mom' config section, hooks the :TOOL
;;;; point, and registers the second_opinion and risk_check tools and
;;;; /calling-mom.

(defsystem "nodecode-calling-mom"
  :description "A cheap advisor answers a stuck or risky tool call; a rare escalation reaches an expensive one"
  :license "MIT"
  :author "SlyBase (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "rules")
               (:file "signals")
               (:file "advisor")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-calling-mom/test"))))

(defsystem "nodecode-calling-mom/test"
  :description "Calling-mom tests. Registered into the shared nodecode.test registry; RUN-CALLING-MOM-TESTS filters by the CALLING-MOM-CELL- name prefix."
  :license "MIT"
  :author "SlyBase (original), Nodecode (rewrite)"
  :depends-on ("nodecode-calling-mom" "nodecode/test")
  :pathname "test/"
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-calling-mom-tests)))
