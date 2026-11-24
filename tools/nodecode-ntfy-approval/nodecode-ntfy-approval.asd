;;;; nodecode-ntfy-approval.asd --- approve a risky shell call from the phone.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of AhmetArif0/hermes-ntfy-approval as a Nodecode cell: one
;;;; :TOOL hook, one /ntfy command, and the ntfy round trip the original
;;;; transport made -- publish a notification with three buttons, read the
;;;; answer off the reply topic, deny on silence.

(defsystem "nodecode-ntfy-approval"
  :description "Answers risky shell commands from a phone through ntfy push buttons"
  :license "MIT"
  :version "0.1.0"
  :author "AhmetArif0 (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-ntfy-approval/test"))))

(defsystem "nodecode-ntfy-approval/test"
  :description "The ntfy approval cell's tests: the rule table, the round trip against a fake ntfy server, and the refusal paths."
  :license "MIT"
  :depends-on ("nodecode-ntfy-approval" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-ntfy-approval-tests)))
