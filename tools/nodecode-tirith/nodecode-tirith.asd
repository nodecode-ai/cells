;;;; nodecode-tirith.asd --- the tirith cell: every shell command checked
;;;; before it runs, by the user's own tirith.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral with one hook, one command, one tool and one manual topic,
;;;; all declared in cell.lisp's single NLE:DEFINE-CELL form; the test
;;;; system registers its slice in the shared nodecode.test registry.

(defsystem "nodecode-tirith"
  :description "Checks every shell command with your installed tirith before it runs"
  :license "MIT"
  :author "sheeki03 (original), Nodecode (rewrite)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-tirith/test"))))

(defsystem "nodecode-tirith/test"
  :description "Tirith tests: the hook's verdicts, the command and the tool, and the lifecycle. Registered into the shared nodecode.test registry; RUN-TIRITH-TESTS filters by the TIRITH- name prefix."
  :license "MIT"
  :depends-on ("nodecode-tirith" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-tirith-tests)))
