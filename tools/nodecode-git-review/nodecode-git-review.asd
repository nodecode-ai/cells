;;;; nodecode-git-review.asd --- every changed file of a repository, in one
;;;; scroll of text the model reads and can cite.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A peripheral cell, NOT part of the organism core: nothing in Nodecode
;;;; names this system. The folder loader loads it at boot when its directory
;;;; sits under ~/.nodecode/cells/; the gateway calls START-CELL after
;;;; recovery, which reads the `git-review` section, registers the two
;;;; model-facing tools cell.lisp declares, puts (help :git-review) on the
;;;; manual and registers /review. Every command it runs is a local program:
;;;; git, and gh or glab only when a pull or merge request is asked for.

(defsystem "nodecode-git-review"
  :description "Review every changed file in one unified diff, then stage, commit, push and open a pull request"
  :license "MIT"
  :version "0.1.0"
  :author "mister-ideas (original), Nodecode (rewrite)"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-git-review/test"))))

(defsystem "nodecode-git-review/test"
  :description "Git review tests. Registered into the shared nodecode.test registry; RUN-GIT-REVIEW-TESTS filters by the GIT-REVIEW-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-git-review" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-git-review-tests)))
