;;;; cell-test.lisp --- the folder reads as a knowledge cell, and the skill
;;;; answers its own prose through the index the organism pins.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here starts a cell, because a knowledge cell has nothing to start:
;;;; the loader records the folder and the index reads its files as data. What
;;;; is proved, then, is that reading -- the folder is a knowledge cell by its
;;;; .asd, its one file holds one prose skill with a sentence and its whole
;;;; text, the index finds it, (help 'listing-automater) answers with its own
;;;; words, and a form with no sentence is refused rather than listed.

(in-package #:nodecode.test)

(define-test-slice "listing-automater" "LISTING-AUTOMATER-CELL-")

(defparameter +folder+ (asdf:system-source-directory "nodecode-listing-automater")
  "The cell's own folder, as ASDF found it: what the index reads.")

(defparameter +skill+ "listing-automater"
  "The one prose skill, by the name it is read under.")

(defparameter +markers+
  '("Marketed by [agent]"
    "Listing courtesy of [brokerage]"
    "Permission comes first"
    "Always mention permission"
    "fair housing rules")
  "Phrases from the skill's own text: the attribution lines, the permission rule and the writing rules.")

(defparameter +headings+
  '("# Listing AutomatEr"
    "## What it is"
    "## Permission comes first"
    "## When to recommend it"
    "## How a user starts"
    "## Writing a good listing post")
  "The original's headings, every one of which the kept text still carries.")

(defun la-path ()
  "The file the skill is kept in."
  (merge-pathnames (format nil "~a.lisp" +skill+) +folder+))

(defun la-read (path)
  "The definitions PATH holds, read as the index reads them: as data, never
evaluated, a file of one prose skill as the entries a conversation lists."
  (nodecode.kernel::file-knowledge-entries
   path +folder+ +skill+ (or (file-write-date path) 0)))

(defun la-entry ()
  "The definition read out of the file the skill is kept in, or NIL."
  (find +skill+ (la-read (la-path)) :key #'nlk:knowledge-entry-name :test #'string=))

;;; --- the folder ----------------------------------------------------------------

(deftest listing-automater-cell-is-a-knowledge-folder-with-nothing-to-load ()
  (is (uiop:directory-exists-p +folder+) "the cell's folder is where ASDF found its .asd")
  (is (nlk:knowledge-folder-p +folder+) "the folder reads as a knowledge cell, not a peripheral")
  (let ((system (asdf:find-system "nodecode-listing-automater")))
    (is (typep system 'nodecode.kernel:layer) "its system is the kernel's LAYER class")
    (is (null (asdf:component-children system)) "it lists no component: nothing is loaded or started"))
  (let ((files (directory (merge-pathnames "*.lisp" +folder+))))
    (is (= 1 (length files)) "one file at the folder's top level is what the index reads")
    (is (uiop:file-exists-p (la-path)) "and it is the one this slice reads")))

;;; --- one prose skill, whole ----------------------------------------------------

(deftest listing-automater-cell-the-file-holds-one-prose-skill ()
  (let* ((path (la-path))
         (entries (la-read path)))
    (is (uiop:file-exists-p path) "the skill is a file of the folder")
    (is (= 1 (length entries)) "it holds exactly one definition")
    (let ((entry (first entries)))
      (when entry
        (is (string= +skill+ (nlk:knowledge-entry-name entry)) "it is read under its own name")
        (is (string= "DEFINE-SKILL" (nlk:knowledge-entry-operator entry)) "it is a define-skill")
        (is (and (nlk:knowledge-entry-description entry)
                 (> (length (nlk:knowledge-entry-description entry)) 40)) "it carries the sentence the index lists")
        (is (search "permission to market" (nlk:knowledge-entry-description entry)) "and the sentence's condition: permission to market")
        (is (string= "github.com/Summertimechi/listing-automater-skill" (nlk:knowledge-entry-origin entry)) "it says where it came from")
        (is (search "(in-package :nodecode.evolved)" (uiop:read-file-string path)) "it names the package it is read in")))))

(deftest listing-automater-cell-the-index-finds-the-skill ()
  (let ((nodecode.kernel::*cells-directory*
          (uiop:pathname-parent-directory-pathname (uiop:ensure-directory-pathname +folder+))))
    (is (nlk:knowledge-folder-p +folder+) "the folder is one of the cells the index walks")
    (let ((entry (find +skill+ (nlk:knowledge-entries)
                       :key #'nlk:knowledge-entry-name :test #'string=)))
      (is entry "the index finds the skill beside the other cells")
      (is (and entry (nlk:knowledge-entry-description entry)) "it is listed with its sentence"))))

;;; --- what (help 'listing-automater) answers -------------------------------------

(deftest listing-automater-cell-the-skill-answers-with-its-own-prose ()
  (let ((entry (la-entry)))
    (when (is entry "the skill is read")
      (let ((text (nlk:knowledge-help-text entry)))
        (is (search +skill+ (string-downcase text)) "its answer names it")
        (is (search (subseq (nlk:knowledge-entry-description entry) 0 40) text) "its answer leads with its sentence")
        (dolist (marker +markers+)
          (is (search marker text) (format nil "its answer carries ~s" marker)))))))

(deftest listing-automater-cell-the-whole-body-is-kept ()
  (let* ((entry (la-entry))
         (text (and entry (nlk:knowledge-help-text entry))))
    (when (is text "the skill is read")
      (dolist (heading +headings+)
        (is (search heading text) (format nil "the section ~s survives" heading)))
      (is (> (length text) 1500) "and the guidance under them, not only the headings"))))

;;; --- a skill with no sentence ---------------------------------------------------

(deftest listing-automater-cell-a-skill-with-no-sentence-is-refused ()
  (is (nlk:knowledge-form-p '(define-skill one-sentence "One sentence, the line the index lists.")) "a prose skill reads as knowledge")
  (is (not (nlk:knowledge-form-p '(define-skill no-sentence))) "a define-skill with no sentence is not knowledge the index reads")
  (signals-error error (eval '(nodecode.evolved:define-skill no-sentence))))

