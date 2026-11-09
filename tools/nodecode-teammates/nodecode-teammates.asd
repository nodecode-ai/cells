;;;; nodecode-teammates.asd --- a named roster of teammates the agent works with.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of 100yenadmin's hermes-teammates plugin for Hermes Agent: its
;;;; named roster, the follow-ups that carry earlier work, live steering and a
;;;; stop, over durable sessions. See NOTICE.

(defsystem "nodecode-teammates"
  :description "Named teammates: a roster the agent assigns work to, then checks, waits on, steers and stops"
  :license "MIT"
  :author "100yenadmin (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-teammates/test"))))

(defsystem "nodecode-teammates/test"
  :description "Teammates tests: the lifecycle, the roster, the six tools over stubbed runs, and the command."
  :license "MIT"
  :author "100yenadmin (original), Nodecode (rewrite)"
  :depends-on ("nodecode-teammates" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-teammates-tests)))
