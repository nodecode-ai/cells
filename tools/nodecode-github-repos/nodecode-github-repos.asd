;;;; nodecode-github-repos.asd --- your GitHub repositories, as text.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `github-repos' section and registers the two
;;;; model-facing tools and /repos cell.lisp declares.
;;;;
;;;; The one program it runs is the user's own `gh' CLI, signed in with
;;;; `gh auth login': the cell holds no token and reads no credential file.

(defsystem "nodecode-github-repos"
  :description "Your GitHub repositories as text: list, search and total them through the gh CLI"
  :license "MIT"
  :version "0.1.0"
  :author "tomekdot (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-github-repos/test"))))

(defsystem "nodecode-github-repos/test"
  :description "GitHub repos tests. Registered into the shared nodecode.test registry; RUN-GITHUB-REPOS-TESTS filters by the GITHUB-REPOS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-github-repos" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-github-repos-tests)))
