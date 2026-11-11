;;;; nodecode-model-tiers.asd --- the Claude model hierarchy, as Claude Code's own
;;;; prompt describes it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One source: ANTHROPIC/CLAUDE-CODE-FABLE-5.2.txt in elder-plinius/CL4R1T4S,
;;;; 957,315 bytes, 7,832 lines, sha256 d54e74e3. Every quoted sentence in
;;;; DATA.LISP stands at a line of that file. The code is ours. See NOTICE.
;;;;
;;;; Three files beside this one: DATA.LISP, the hierarchy, the timeline and the
;;;; quotes; QUERY.LISP, the filters, the blocks a person reads, and the one verb
;;;; that reads the file itself; CELL.LISP, the declaration. The .asd sits INSIDE
;;;; its folder: install by putting -- or symlinking -- this directory under
;;;; ~/.nodecode/cells/.

(defsystem "nodecode-model-tiers"
  :description "The Claude model hierarchy as the leaked Claude Code prompt tells it: the Mythos tier above Opus, with line numbers"
  :license "MIT"
  :author "elder-plinius (the leak), Nodecode (this cell)"
  :version "0.1.0"
  :depends-on ("nodecode")
  :serial t
  :components ((:file "package")
               (:file "data")
               (:file "query")
               (:file "cell"))
  :in-order-to ((test-op (test-op "nodecode-model-tiers/test"))))

(defsystem "nodecode-model-tiers/test"
  :description "model-tiers tests. Registered into the shared nodecode.test registry; RUN-MODEL-TIERS-TESTS filters by the MODEL-TIERS-CELL- name prefix."
  :license "MIT"
  :depends-on ("nodecode-model-tiers" "nodecode/test")
  :pathname "test/"
  :serial t
  :components ((:file "cell-test"))
  :perform (test-op (operation component)
                    (uiop:symbol-call '#:nodecode.test '#:run-model-tiers-tests)))
