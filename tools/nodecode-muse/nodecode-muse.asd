;;;; nodecode-muse.asd --- a companion that keeps goals, reminders and a quiet Feed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of hermes-muse for Nodecode: one store under the home, one
;;;; tool, one slash command, and the turn's own request for a due reminder.
;;;; Optional ASDF cell, NOT part of the organism core.

(defsystem "nodecode-muse"
  :description "Muse: a companion with goals, expiring interests, a quiet Feed and reminders that arrive at your next request"
  :license "MIT"
  :author "xiaohei-info (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-muse/test"))))

(defsystem "nodecode-muse/test"
  :description "Tests for the muse cell; RUN-MUSE-TESTS filters the shared registry by the MUSE-CELL- prefix."
  :license "MIT"
  :depends-on ("nodecode-muse" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-muse-tests)))
