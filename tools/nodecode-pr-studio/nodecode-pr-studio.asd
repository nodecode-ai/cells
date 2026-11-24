;;;; nodecode-pr-studio.asd --- GitHub pull requests, read and answered from the chat.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `pr-studio' section, registers the six
;;;; model-facing tools cell.lisp declares, puts (help :pr-studio) on the
;;;; manual and registers /prs. Every command it runs is the user's own gh
;;;; CLI: gh search prs, gh pr view, gh pr diff and gh api.

(defsystem "nodecode-pr-studio"
  :description "Review GitHub pull requests from the chat: the inbox, one PR read whole, its diff and threads, then comments, replies and reviews"
  :license "MIT"
  :version "0.1.0"
  :author "Murali-lns (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-pr-studio/test"))))

(defsystem "nodecode-pr-studio/test"
  :description "PR Studio tests. Registered into the shared nodecode.test registry; RUN-PR-STUDIO-TESTS filters by the PR-STUDIO-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-pr-studio" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-pr-studio-tests)))
