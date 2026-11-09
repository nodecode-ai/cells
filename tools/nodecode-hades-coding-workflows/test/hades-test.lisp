;;;; hades-test.lisp --- this cell read the way the index reads it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A knowledge cell has nothing to start and no tool to call, so what a
;;;; test can prove is the thing that could be broken: that the folder is
;;;; recognised as knowledge, that the three SKILL.md files beside its .asd
;;;; are found where they are, parsed for their name and description, and
;;;; read whole -- and that a cells folder holding a copy of it lists all
;;;; three in the index with their prose. Nothing here loads the cell's code
;;;; because it has none; every assertion reads the folder as data.

(in-package #:nodecode.test)

(define-test-slice "hades" "HADES-CELL-" :recipe "hades")

(defun hades-folder ()
  "This cell's folder, from its own system."
  (uiop:ensure-directory-pathname
   (asdf:system-source-directory "nodecode-hades-coding-workflows")))

(defparameter *hades-skill-names* '("dual-model-harness" "subagent-driven-development" "writing-plans"))

(defparameter *hades-headings*
  '(("dual-model-harness" . "# Dual-Model Development Harness")
    ("subagent-driven-development" . "# Subagent-Driven Development")
    ("writing-plans" . "# Writing Implementation Plans"))
  "The first heading of each SKILL.md: the proof the body, not just the frontmatter, is read.")

(defun hades-skill-files ()
  "((NAME . PATH) ...) for each SKILL.md this cell holds, NAME the folder it
sits in -- what the index would take for the skill if its frontmatter named
none."
  (loop for entry in (nodecode.kernel::linked-skill-files (hades-folder))
        for path = (first entry)
        collect (cons (nodecode.kernel::folder-name (uiop:pathname-directory-pathname path))
                      path)))

(deftest hades-cell-is-a-knowledge-cell ()
  (let ((folder (hades-folder)))
    (is (eq :knowledge (nodecode.kernel::kept-folder-kind folder)) "the folder's .asd says :properties ((:knowledge . t)), so the loader records it and never loads it")
    (is (probe-file (merge-pathnames "nodecode-hades-coding-workflows.asd" folder)) "the .asd sits at the top of the folder, which is all a cell needs")
    (is (equal '("nodecode") (nodecode.kernel::folder-depends-on (nodecode.kernel::make-cell-folder folder))) "nothing outside nodecode is needed: a knowledge cell carries prose, not dependencies")))

(deftest hades-cell-holds-three-skills ()
  (let ((files (hades-skill-files)))
    (is (= 3 (length files)) "three SKILL.md files, no more and no fewer")
    (is (equal *hades-skill-names* (sort (mapcar #'car files) #'string<)) "each one a folder of its own under skills/, as the original repository lays them out")
    (dolist (entry files)
      (is (probe-file (cdr entry)) (format nil "~a/SKILL.md is there" (car entry))))))

(deftest hades-cell-reads-each-skill-whole ()
  (let ((folder (hades-folder)))
    (dolist (path (sort (mapcar #'cdr (hades-skill-files)) #'string< :key #'namestring))
      (let ((entries (nodecode.kernel::skill-file-entries
                      path folder "skills" (nodecode.kernel::folder-name (uiop:pathname-directory-pathname path)) 0)))
        (is (= 1 (length entries)) (format nil "~a reads as one skill" (file-namestring (uiop:pathname-directory-pathname path))))
        (let* ((entry (first entries))
               (name (nodecode.kernel::knowledge-entry-name entry))
               (description (nodecode.kernel::knowledge-entry-description entry))
               (prose (nodecode.kernel::knowledge-help-text entry)))
          (is (member name *hades-skill-names* :test #'string=)
              (format nil "~a's frontmatter names it" name))
          (is (and (stringp description) (plusp (length description)))
              (format nil "~a has the one-sentence description the index lists" name))
          (is (not (find #\Newline description))
              (format nil "~a's description is one line: a folded YAML description collapses" name))
          (is (and (stringp prose) (< 2000 (length prose)))
              (format nil "~a's whole body is read, not its first paragraph" name))
          (is (search (cdr (assoc name *hades-headings* :test #'string=)) prose)
              (format nil "~a's body carries its own heading" name)))))))

(deftest hades-cell-carries-its-references ()
  (let ((folder (hades-folder)))
    (dolist (relative '("skills/dual-model-harness/references/glm-provider-quirks.md"
                        "skills/subagent-driven-development/references/context-budget-discipline.md"
                        "skills/subagent-driven-development/references/gates-taxonomy.md"
                        "skills/writing-plans/references/server-action-result-pattern.md"))
      (let ((path (merge-pathnames relative folder)))
        (is (probe-file path) (format nil "~a travels with the skill that names it" relative))
        (is (plusp (length (uiop:read-file-string path))) (format nil "~a is not empty" relative))))))

(deftest hades-cell-lists-in-the-index ()
  ;; The one end-to-end reading: a cells folder holding a copy of this folder
  ;; lists the three skills, which is what a booted organism does.
  (let* ((root (merge-pathnames "hades-cells/" (uiop:temporary-directory)))
         (copy (merge-pathnames "hades-memory/" root)))
    (ignore-errors (uiop:delete-directory-tree root :validate t))
    (ensure-directories-exist copy)
    (uiop:copy-file (merge-pathnames "nodecode-hades-coding-workflows.asd" (hades-folder))
                     (merge-pathnames "nodecode-hades-coding-workflows.asd" copy))
    (dolist (relative '("skills/dual-model-harness/SKILL.md"
                        "skills/subagent-driven-development/SKILL.md"
                        "skills/writing-plans/SKILL.md"))
      (ensure-directories-exist (merge-pathnames relative copy))
      (uiop:copy-file (merge-pathnames relative (hades-folder)) (merge-pathnames relative copy)))
    (let* ((nodecode.kernel::*cells-directory* root)
           (entries (remove-if-not (lambda (entry)
                                     (equal (namestring copy)
                                            (namestring (uiop:ensure-directory-pathname
                                                         (nodecode.kernel::knowledge-entry-folder entry)))))
                                   (nodecode.kernel::knowledge-entries))))
      (is (= 3 (length entries)) "a cells folder holding this cell lists its three skills")
      (is (equal *hades-skill-names* (sort (mapcar #'nodecode.kernel::knowledge-entry-name entries) #'string<)) "under the names their frontmatter declares, not the folder's")
      (is (every (lambda (entry) (plusp (length (or (nodecode.kernel::knowledge-entry-description entry) "")))) entries) "every one of them documented, so the index shows it")
      (ignore-errors (uiop:delete-directory-tree root :validate t)))))
