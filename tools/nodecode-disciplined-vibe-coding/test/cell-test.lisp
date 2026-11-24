;;;; cell-test.lisp --- the folder reads as a knowledge cell, and every skill
;;;; answers its own prose through the index the organism pins.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here starts a cell, because a knowledge cell has nothing to start:
;;;; the loader records the folder and the index reads its files as data. What
;;;; is proved, then, is that reading -- the folder is a knowledge cell by its
;;;; .asd, each of the eleven files holds one prose skill with a sentence and
;;;; its whole text, the index finds all eleven, (help 'name) answers each one
;;;; with its own words, the guides they name sit beside them, and a form with
;;;; no sentence is refused rather than listed.

(in-package #:nodecode.test)

(define-test-slice "disciplined-vibe-coding" "DVC-CELL-")

(defparameter +folder+ (asdf:system-source-directory "nodecode-disciplined-vibe-coding")
  "The cell's own folder, as ASDF found it: what the index reads.")

(defparameter +skills+ '("brainstorm" "debug" "disciplined-vibe-coding" "execute" "intake"
                         "plan" "research" "test-first" "verify" "version" "write-docs")
  "The eleven prose skills, by the name each is read under.")

(defparameter +markers+
  '(("brainstorm" . "Batch independent forks")
    ("debug" . "Two-failure stop")
    ("disciplined-vibe-coding" . "Economy gate")
    ("execute" . "Workers (max 5)")
    ("intake" . "stakes tier")
    ("plan" . "Budget line for long tasks")
    ("research" . ".pipeline/read.log")
    ("test-first" . "RED:")
    ("verify" . "proves it")
    ("version" . "Narrative commits")
    ("write-docs" . "One idea per sentence"))
  "One phrase per skill, taken from that skill's own text and no other's.")

(defparameter +guides+
  '(("economy-ladder.md" . "one-liner")
    ("to-spec.md" . "Acceptance criteria")
    ("writing-plan.md" . "Verify criteria")
    ("wayfinder.md" . "Result:"))
  "The guide each skill names, and a heading it must carry.")

(defun dvc-path (name)
  "The file the skill NAME is kept in."
  (merge-pathnames (format nil "~a.lisp" name) +folder+))

(defun dvc-read (path)
  "The definitions PATH holds, read as the index reads them: as data, never
evaluated, a file of prose skills as the entries a conversation lists."
  (nodecode.kernel::file-knowledge-entries
   path +folder+ "disciplined-vibe-coding" (or (file-write-date path) 0)))

(defun dvc-entry (name)
  "The definition read out of the file the skill NAME is kept in, or NIL."
  (find name (dvc-read (dvc-path name)) :key #'nlk:knowledge-entry-name :test #'string=))

;;; --- the folder ----------------------------------------------------------------

(deftest dvc-cell-is-a-knowledge-folder-with-nothing-to-load ()
  (is (uiop:directory-exists-p +folder+) "the cell's folder is where ASDF found its .asd")
  (is (nlk:knowledge-folder-p +folder+) "the folder reads as a knowledge cell, not a peripheral")
  (let ((system (asdf:find-system "nodecode-disciplined-vibe-coding")))
    (is (typep system 'nodecode.kernel:layer) "its system is the kernel's LAYER class")
    (is (null (asdf:component-children system)) "it lists no component: nothing is loaded or started"))
  (is (= 11 (length (directory (merge-pathnames "*.lisp" +folder+)))) "eleven files at the folder's top level are what the index reads"))

;;; --- one prose skill a file ----------------------------------------------------

(deftest dvc-cell-each-file-holds-one-prose-skill ()
  (dolist (name +skills+)
    (let* ((path (dvc-path name))
           (entries (dvc-read path)))
      (is (uiop:file-exists-p path) (format nil "~a is a file of the folder" name))
      (is (= 1 (length entries)) (format nil "~a holds exactly one definition" name))
      (let ((entry (first entries)))
        (when entry
          (is (string= name (nlk:knowledge-entry-name entry)) (format nil "~a is read under its own name" name))
          (is (string= "DEFINE-SKILL" (nlk:knowledge-entry-operator entry))
              (format nil "~a is a define-skill" name))
          (is (and (nlk:knowledge-entry-description entry)
                   (> (length (nlk:knowledge-entry-description entry)) 40))
              (format nil "~a carries the sentence the index lists" name))
          (is (string= "github.com/bmtrnavsky/disciplined-vibe-coding" (nlk:knowledge-entry-origin entry))
              (format nil "~a says where it came from" name))
          (is (search "(in-package :nodecode.evolved)" (uiop:read-file-string path))
              (format nil "~a names the package it is read in" name)))))))

(deftest dvc-cell-the-index-reads-the-eleven-skills ()
  (let ((nodecode.kernel::*cells-directory*
          (uiop:pathname-parent-directory-pathname (uiop:ensure-directory-pathname +folder+))))
    (is (nlk:knowledge-folder-p +folder+) "the folder is one of the cells the index walks")
    (let ((entries (nlk:knowledge-entries)))
      (dolist (name +skills+)
        (let ((entry (find name entries :key #'nlk:knowledge-entry-name :test #'string=)))
          (is entry (format nil "the index finds ~a beside the other cells" name))
          (is (and entry (nlk:knowledge-entry-description entry))
              (format nil "~a is listed with its sentence" name)))))))

(deftest dvc-cell-each-skill-answers-with-its-own-prose ()
  (dolist (name +skills+)
    (let ((entry (dvc-entry name)))
      (when (is entry (format nil "~a is read" name))
        (let ((text (nlk:knowledge-help-text entry))
              (marker (cdr (assoc name +markers+ :test #'string=))))
          (is (search name (string-downcase text)) (format nil "~a's answer names it" name))
          (is (search (subseq (nlk:knowledge-entry-description entry) 0 40) text)
              (format nil "~a's answer leads with its sentence" name))
          (is (and marker (search marker text))
              (format nil "~a's answer carries its own text (~a)" name marker)))))))

;;; --- the guides beside them -----------------------------------------------------

(defun dvc-guide-check (guide)
  "Assert one guide sits beside the skills and carries its own section, GUIDE a
(name . marker) pair."
  (let ((path (merge-pathnames (format nil "guides/~a" (car guide)) +folder+))
        (name (car guide))
        (marker (cdr guide)))
    (is (uiop:file-exists-p path) (format nil "guides/~a is there" name))
    (when (uiop:file-exists-p path)
      (let ((text (uiop:read-file-string path)))
        (is (> (length text) 100) (format nil "guides/~a is the whole guide" name))
        (is (and (search marker text) t) (format nil "guides/~a carries ~a" name marker))))))

(deftest dvc-cell-the-guides-the-skills-name-are-beside-them ()
  (dolist (guide +guides+) (dvc-guide-check guide))
  (dolist (name +skills+)
    (is (search "guides/" (uiop:read-file-string (dvc-path name)))
        (format nil "~a says where the guides sit" name))))

;;; --- a skill with no sentence ---------------------------------------------------

(deftest dvc-cell-a-skill-with-no-sentence-is-refused ()
  (is (nlk:knowledge-form-p '(define-skill one-sentence "One sentence, the line the index lists.")) "a prose skill reads as knowledge")
  (is (not (nlk:knowledge-form-p '(define-skill no-sentence))) "a define-skill with no sentence is not knowledge the index reads")
  (signals-error error (eval '(nodecode.evolved:define-skill no-sentence)))
  ;; And a file holding one is read, yet not listed, as an undocumented
  ;; definition never is.
  (let ((path (merge-pathnames "dvc-no-sentence.lisp" (nlk:home))))
    (nlk:with-cleanup ((ignore-errors (delete-file path)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
        (write-string "(in-package :nodecode.evolved)

(define-skill no-sentence)
" out))
      (let ((entries (dvc-read path)))
        (is (= 1 (length entries)) "the form is read as a definition")
        (is (null (nlk:knowledge-entry-description (first entries))) "with no sentence to list")
        (is (null (remove-if-not #'nlk:knowledge-entry-description entries)) "so the index leaves it out")))))
