;;;; nodecode-pushgate.asd --- a push gate backed by verification receipts.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of cirwel/hermes-receipts. The folder loader loads
;;;; this system at boot when its directory sits under ~/.nodecode/cells/; the
;;;; gateway calls START-CELL after recovery, which installs one :TOOL hook
;;;; (the push gate), one model-facing tool (`pushgate') and one slash command
;;;; (/pushgate) from the one NLE:DEFINE-CELL declaration in cell.lisp.

(defsystem "nodecode-pushgate"
  :description "Blocks git push and gh pr create until your check has passed on exactly the content being pushed"
  :license "Apache-2.0"
  :version "0.1.0"
  :author "cirwel (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pushgate/test"))))

(defsystem "nodecode-pushgate/test"
  :description "Push gate tests. Registered into the shared nodecode.test registry; RUN-PUSHGATE-TESTS filters by the PUSHGATE- name prefix."
  :license "Apache-2.0"
  :author "cirwel (original), Nodecode (rewrite)"
  :depends-on ("nodecode-pushgate" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pushgate-tests)))
