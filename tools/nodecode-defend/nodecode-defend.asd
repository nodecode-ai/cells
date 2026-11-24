;;;; nodecode-defend.asd --- injection, jailbreak and secret scanning cell.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of PerryLink's dsh-defend (Apache-2.0). Nothing in the
;;;; organism core names this system: the folder loader loads it when its
;;;; directory sits under ~/.nodecode/cells/, and the gateway calls this
;;;; system's START-CELL after recovery, which registers two tools, the /defend
;;;; command, the :defend help topic and one :tool hook.

(defsystem "nodecode-defend"
  :description "Injection, jailbreak and secret scanning: scans a tool call's text before it runs, with allow, ask and block tiers"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "PerryLink (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-defend/test"))))

(defsystem "nodecode-defend/test"
  :description "Defend tests. Registered into the shared nodecode.test registry; RUN-DEFEND-TESTS filters by the DEFEND- name prefix."
  :license "Apache-2.0"
  :author "PerryLink (original), Nodecode (rewrite)"
  :depends-on ("nodecode-defend" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-defend-tests)))
