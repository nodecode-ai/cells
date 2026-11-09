;;;; nodecode-diff-review.asd --- a git diff read for what should not land.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cell, NOT part of the organism core: nothing in Nodecode names this
;;;; system. The folder loader loads it at boot when its directory sits under
;;;; ~/.nodecode/cells/; the gateway calls START-CELL after recovery, which
;;;; reads the `diff-review' section, registers the diff_review tool, puts
;;;; /diff-review in the one slash catalog and the review on the manual as
;;;; (help :diff-review).
;;;;
;;;; Ported from sprmn24's hermes-plugin-diff-review (see NOTICE). The
;;;; original was one Python tool over `git diff'; this cell runs the same git
;;;; commands and reads the same three modes, in Lisp, with no dependency
;;;; beyond the image.

(defsystem "nodecode-diff-review"
  :description "Review a git diff for debug prints, hardcoded secrets, bare excepts and TODOs"
  :author "sprmn24 (original), Nodecode (rewrite)"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "diff")
               (:file "git")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-diff-review/test"))))

(defsystem "nodecode-diff-review/test"
  :description "Diff review tests. Registered into the shared nodecode.test registry; RUN-DIFF-REVIEW-TESTS filters by the DIFF-REVIEW-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-diff-review" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-diff-review-tests)))
