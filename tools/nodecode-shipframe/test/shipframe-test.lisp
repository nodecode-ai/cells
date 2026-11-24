;;;; shipframe-test.lisp --- the pack reads as skills, whole, from where it sits.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here, offline, in a scratch home, over the cell folder as
;;;; a home has it: the folder is a knowledge cell Nodecode records and never
;;;; loads, so there is nothing to start, nothing to stop and no setting to
;;;; miss; every SKILL.md ShipFrame wrote is read as a prose skill, its
;;;; frontmatter the line the index lists and the text after it the whole
;;;; procedure (help 'name) answers with; the hub skill says what the pack is;
;;;; and a word from a skill's body finds it as well as its name does.

(in-package #:nodecode.test)

(define-test-slice "shipframe" "SHIPFRAME-")

;;; --- the fixture ---------------------------------------------------------------------

(defun shipframe-source ()
  "The cell folder this test runs from."
  (asdf:system-source-directory "nodecode-shipframe"))

(defun shipframe-copy-tree (from to)
  "FROM's files and folders, copied under TO."
  (ensure-directories-exist (merge-pathnames "keep" to))
  (loop for file in (uiop:directory-files from)
        do (uiop:copy-file file (merge-pathnames (file-namestring file) to)))
  (loop for child in (uiop:subdirectories from)
        for name = (first (last (pathname-directory child)))
        do (shipframe-copy-tree child (merge-pathnames (format nil "~a/" name) to)))
  to)

(defun shipframe-install ()
  "The cell folder copied into the fixture's cells directory, as a home has it."
  (shipframe-copy-tree (shipframe-source)
                        (merge-pathnames "nodecode-shipframe/" nlk::*cells-directory*)))

(defun shipframe-entries ()
  "Every definition the fixture home reads, its one cell installed."
  (shipframe-install)
  (nlk:knowledge-entries))

(defun shipframe-entry (name entries)
  "ENTRIES' definition named NAME."
  (find name entries :test #'string= :key #'nlk::knowledge-entry-name))

(defun shipframe-skills (entries)
  "ENTRIES without the hub skill: the 44 SKILL.md folders."
  (remove "shipframe" entries :test #'string= :key #'nlk::knowledge-entry-name))

(defun shipframe-saying (entries needle)
  "The names of ENTRIES whose prose holds NEEDLE, case folded: what the index
finds by a word a skill says."
  (loop for entry in entries
        when (search needle (nlk::knowledge-prose entry) :test #'char-equal)
          collect (nlk::knowledge-entry-name entry)))

(defparameter +shipframe-skills+
  '("a11y-auditor" "api-contract-review" "backend-release" "bug-diagnosis"
    "client-copy-review" "code-review" "codebase-design" "create-issue"
    "create-pr" "create-task" "dependency-upgrade" "deploy-evidence"
    "design-expert" "design-system-docs" "design-system-setup" "domain-modeling"
    "e2e-verify" "evidence-audit" "feature-discovery" "frontend-release"
    "generate-readme" "handoff" "implement-task" "incident-response"
    "init-project" "live-docs" "mcp-debugging" "memory-curator" "plan-expert"
    "planning-features" "project-memory-init" "project-memory-refresh"
    "project-profile" "project-release" "proof-runner" "release-checklist"
    "research" "security-hardening" "security-review" "tdd" "wiki-forge"
    "wiki-init" "wiki-query" "wiki-sync")
  "The skills ShipFrame ships, one folder each, sorted as the folders are.")

;;; --- the folder ----------------------------------------------------------------------

(deftest shipframe-cell-is-knowledge-recorded-run-never ()
  (with-knowledge-fixture ()
    (let ((cell (shipframe-install)))
      (is (eq :knowledge (nlk::kept-folder-kind cell)) "the loader records the folder as a knowledge cell and never loads it")
      (is (null (asdf:component-children (asdf:find-system "nodecode-shipframe"))) "no component to compile, so no start and no stop to get wrong")
      (is (null (search "define-cell" (nlk:read-text (merge-pathnames "nodecode-shipframe.asd" cell)))) "it declares no config section: there is no setting to be missing")
      (is (= 44 (length (directory (merge-pathnames "skills/*/SKILL.md" cell)))) "and it carries the 44 SKILL.md folders ShipFrame wrote"))))

;;; --- the skills ----------------------------------------------------------------------

(deftest shipframe-every-skill-shipframe-wrote-reads ()
  (with-knowledge-fixture ()
    (let* ((entries (shipframe-entries))
           (skills (shipframe-skills entries)))
      (is (= 45 (length entries)) "44 skills and the hub skill over them")
      (is (equal +shipframe-skills+ (sort (mapcar #'nlk::knowledge-entry-name skills) #'string<)) "each folder is a skill of its own, named as its folder is")
      (is (every (lambda (entry) (let ((description (nlk::knowledge-entry-description entry))) (and (stringp description) (plusp (length description))))) skills) "the index lists every one, from its frontmatter description")
      (is (every (lambda (entry) (let ((prose (nlk::knowledge-prose entry))) (and (stringp prose) (> (length prose) 400)))) skills) "and each reads a whole procedure, not a pointer to a file"))))

(deftest shipframe-a-skill-reads-whole-and-says-where-it-came-from ()
  (with-knowledge-fixture ()
    (let* ((entries (shipframe-entries))
           (tdd (shipframe-entry "tdd" entries))
           (help (nlk:knowledge-help-text tdd))
           (prose (nlk::knowledge-prose tdd)))
      (is (search "tdd — skill" help) "help names it a skill")
      (is (search "red-green-refactor" help) "with the frontmatter line that says when to use it")
      (is (search "Test-Driven Development" prose) "and the body the SKILL.md opens with")
      (is (search "What a good test is" prose) "down to the sections further in")
      (is (search "the anti-patterns" prose) "and the last of them")
      (is (search "skills/tdd/SKILL.md" (uiop:native-namestring (nlk::knowledge-entry-path tdd))) "read where ShipFrame wrote it, nothing converted and nothing copied")
      (is (equal "nodecode-shipframe" (nlk::knowledge-entry-label tdd)) "filed under this cell, the folder that holds it"))))

(deftest shipframe-the-hub-skill-says-what-the-pack-is ()
  (with-knowledge-fixture ()
    (let* ((entries (shipframe-entries))
           (prose (nlk::knowledge-prose (shipframe-entry "shipframe" entries))))
      (is (search "Refresh context" prose) "the chain the skills walk, in order")
      (is (search "create-pr" prose) "and where it ends")
      (is (search "project-memory-refresh" prose) "naming the skills that walk it")
      (is (search "Never store secrets" prose) "with the rules the pack keeps, said once"))))

;;; --- what the index finds ------------------------------------------------------------

(deftest shipframe-the-index-finds-a-skill-by-what-it-says ()
  (with-knowledge-fixture ()
    (let* ((entries (shipframe-entries))
           (rollback (shipframe-saying entries "rollback")))
      (is (member "release-checklist" rollback :test #'string=) "a word from a skill's body finds that skill")
      (is (member "project-release" rollback :test #'string=) "and every other skill that says the same thing")
      (is (not (member "tdd" rollback :test #'string=)) "and nothing that does not say it")
      (is (null (shipframe-saying entries "a phrase no skill says")) "while a phrase nothing says finds nothing"))))

(deftest shipframe-a-second-read-finds-the-same-skills ()
  (with-knowledge-fixture ()
    (shipframe-install)
    (let ((first (mapcar #'nlk::knowledge-entry-name (nlk:knowledge-entries)))
          (again (progn (clrhash nlk::*knowledge-memo*) (mapcar #'nlk::knowledge-entry-name (nlk:knowledge-entries)))))
      (is (equal first again) "the scan is a read of the files, the same twice")
      (is (= 45 (length first)) "and finds them all the second time too"))))
