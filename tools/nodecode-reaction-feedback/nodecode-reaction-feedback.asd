;;;; nodecode-reaction-feedback.asd --- the reactions a user leaves on the agent's messages.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of AhmetArif0's hermes-reaction-feedback for Nodecode: one tool
;;;; a chat surface (or the model) reports through, one /reaction-feedback
;;;; command, and a harness section that carries the note a session's waiting
;;;; reactions make at its next turn. See src/CELLS.md for the cell contract.

(defsystem "nodecode-reaction-feedback"
  :description "The emoji reactions a user leaves on the agent's messages, read as a short note at the next turn"
  :license "MIT"
  :author "AhmetArif0 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-reaction-feedback/test"))))

(defsystem "nodecode-reaction-feedback/test"
  :description "The reaction-feedback cell's tests, registered in the shared nodecode.test registry; RUN-REACTION-FEEDBACK-TESTS filters by the REACTION-FEEDBACK-CELL- prefix."
  :license "MIT"
  :author "AhmetArif0 (original), Nodecode (rewrite)"
  :depends-on ("nodecode-reaction-feedback" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-reaction-feedback-tests)))
