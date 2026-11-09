;;;; nodecode-shipframe.asd --- a knowledge cell: ShipFrame's skills, read never run.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; ShipFrame keeps 44 skills as Markdown, one folder each, a SKILL.md with
;;;; frontmatter in every one. Nodecode reads a folder like that where it
;;;; sits: (knowledge.lisp LINKED-SKILL-FILES) takes each SKILL.md under a
;;;; cell -- one level down, or two -- as a prose skill, its frontmatter
;;;; name and description the line the index lists, the text after the
;;;; frontmatter what (help 'name) reads. So skills/ ships here exactly as
;;;; ShipFrame wrote it, and nothing is converted.
;;;;
;;;; The one file of our own is shipframe.lisp at the top: the hub skill
;;;; that says what the pack is and in what order its skills run. A file at
;;;; the top is read as definitions (KNOWLEDGE.LISP FILE-KNOWLEDGE-ENTRIES);
;;;; a folder below it is read as skills.
;;;;
;;;; :class nodecode.kernel:layer with (:knowledge . t) is what makes this a
;;;; knowledge cell: recorded, never loaded, no START-CELL and no section, so
;;;; it needs no setting and cannot fail to start. The /test system is how
;;;; its own tests run (.github/test-cell.sh), and is the only thing here
;;;; ASDF ever compiles.

;; The layer class rides with Nodecode; a test run reads this file before
;; anything is loaded.
(unless (find-package "NODECODE.KERNEL")
  (asdf:load-system "nodecode"))

(defsystem "nodecode-shipframe"
  :description "ShipFrame's 44 coding-workflow skills: discovery, planning, TDD, review, release evidence, PRs"
  :license "MIT"
  :author "juanitourquiza (original), Nodecode (rewrite)"
  :version "0.1.0"
  :class nodecode.kernel:layer
  :depends-on ("nodecode")
  :properties ((:knowledge . t))
  :components nil
  :in-order-to ((test-op (test-op "nodecode-shipframe/test"))))

(defsystem "nodecode-shipframe/test"
  :description "nodecode-shipframe's own tests"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("nodecode-shipframe" "nodecode/test")
  :components ((:file "test/shipframe-test"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:nodecode.test '#:run-shipframe-tests)))
