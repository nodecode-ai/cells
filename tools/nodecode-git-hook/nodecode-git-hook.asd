;;;; nodecode-git-hook.asd --- pull before reads, commit and push what a turn changed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of aean0x/hermes-git-hook: two hook points instead of four
;;;; callbacks, no tool of its own, and git run through the kernel's bounded
;;;; subprocess seam.

(defsystem "nodecode-git-hook"
  :description "Pulls clean worktrees before reads and commits and pushes only the files a turn changed"
  :license "MIT"
  :version "0.1.0"
  :author "aean0x (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-git-hook/test"))))

(defsystem "nodecode-git-hook/test"
  :description "git-hook tests. Registered into the shared nodecode.test registry; RUN-GIT-HOOK-TESTS filters by the GIT-HOOK- name prefix."
  :license "MIT"
  :author "aean0x (original), Nodecode (rewrite)"
  :depends-on ("nodecode-git-hook" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "support")
               (:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-git-hook-tests)))
