;;;; nodecode-clearance.asd --- per-sender tool permissions, checked in code.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral: one :TOOL hook, one slash command, one manual topic and one
;;;; config section. The folder loader loads it at boot when its directory sits
;;;; under ~/.nodecode/cells/; the gateway calls this system's START-CELL after
;;;; recovery, which installs the hook, the command and the topic from the one
;;;; NLE:DEFINE-CELL declaration in cell.lisp.

(defsystem "nodecode-clearance"
  :description "Per-sender tool tiers: every tool call is checked in code against who sent the message"
  :license "MIT"
  :version "0.1.0"
  :author "yuzuru99 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-clearance/test"))))

(defsystem "nodecode-clearance/test"
  :description "Clearance tests. Registered into the shared nodecode.test registry; RUN-CLEARANCE-TESTS filters by the CLEARANCE- name prefix."
  :license "MIT"
  :depends-on ("nodecode-clearance" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-clearance-tests)))
