;;;; cell-test.lisp --- the folder reads as a knowledge cell, the skill answers
;;;; its own prose through the index the organism pins, and the commands it
;;;; names sit beside it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here starts a cell, because a knowledge cell has nothing to start:
;;;; the loader records the folder and the index reads its files as data. What
;;;; is proved, then, is that reading -- the folder is a knowledge cell by its
;;;; .asd, its one file holds one prose skill with a sentence and its whole
;;;; text, the index finds it beside the other cells, (help 'windows-ops)
;;;; answers it in its own words, the command reference it names is there and
;;;; keeps its destructive lines commented out, and a skill with no sentence is
;;;; refused rather than listed.

(in-package #:nodecode.test)

(define-test-slice "windows-ops" "WINDOWS-OPS-CELL-")

(defparameter +folder+ (asdf:system-source-directory "nodecode-windows-ops")
  "The cell's own folder, as ASDF found it: what the index reads.")

(defparameter +skill-file+ (merge-pathnames "windows-ops.lisp" +folder+)
  "The one file the index reads: the prose skill.")

(defparameter +reference+ (merge-pathnames "references/diagnostic-commands.md" +folder+)
  "The command reference the skill names, beside it.")

(defparameter +markers+ '("cmd.exe" "wslpath" "JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK"
                          "references/diagnostic-commands.md")
  "Phrases the skill's own text must carry: one per pitfall class, and where
its commands are.")

(defun windows-ops-read (path)
  "The definitions PATH holds, read as the index reads them: as data, never
evaluated."
  (nodecode.kernel::file-knowledge-entries
   path +folder+ "nodecode-windows-ops" (or (file-write-date path) 0)))

(defun windows-ops-entry ()
  "The one definition the cell's file holds, or NIL."
  (first (windows-ops-read +skill-file+)))

;;; --- the folder ----------------------------------------------------------------

(deftest windows-ops-cell-is-a-knowledge-folder-with-nothing-to-load ()
  (is (uiop:directory-exists-p +folder+) "the cell's folder is where ASDF found its .asd")
  (is (nlk:knowledge-folder-p +folder+) "the folder reads as a knowledge cell, not a peripheral")
  (let ((system (asdf:find-system "nodecode-windows-ops")))
    (is (typep system 'nodecode.kernel:layer) "its system is the kernel's LAYER class")
    (is (null (asdf:component-children system)) "it lists no component: nothing is loaded or started"))
  (is (= 1 (length (directory (merge-pathnames "*.lisp" +folder+)))) "one file at the folder's top level is what the index reads"))

;;; --- one prose skill -----------------------------------------------------------

(deftest windows-ops-cell-reads-as-one-prose-skill ()
  (is (uiop:file-exists-p +skill-file+) "windows-ops.lisp is a file of the folder")
  (let ((entries (windows-ops-read +skill-file+)))
    (is (= 1 (length entries)) "the file holds exactly one definition")
    (let ((entry (first entries)))
      (when entry
        (is (string= "windows-ops" (nlk:knowledge-entry-name entry)) "it is read under its own name, the help topic")
        (is (string= "DEFINE-SKILL" (nlk:knowledge-entry-operator entry)) "it is a define-skill")
        (let ((description (nlk:knowledge-entry-description entry)))
          (is (and description (> (length description) 40)) "it carries the sentence the index lists")
          (is (and description (search "Windows" description) (search "WSL" description)) "the sentence says what it is for: Windows and WSL"))
        (is (string= "github.com/Yicheng-Mark/hermes-windows-plugins" (nlk:knowledge-entry-origin entry)) "it says where it came from")
        (is (search "(in-package :nodecode.evolved)" (uiop:read-file-string +skill-file+)) "it names the package it is read in")))))

(deftest windows-ops-cell-the-index-finds-it-beside-the-other-cells ()
  ;; The index walks the cells directory's folders; give it one of its own,
  ;; holding this cell, so the walk is this folder's and nothing else's.
  (let* ((root (merge-pathnames "windows-ops-index/" (nlk:home)))
         (cells (merge-pathnames "cells/" root))
         (mine (merge-pathnames "nodecode-windows-ops/" cells)))
    (nlk:with-cleanup ((ignore-errors (uiop:delete-directory-tree root :validate t)))
      (ensure-directories-exist (merge-pathnames "x" mine))
      (dolist (name '("nodecode-windows-ops.asd" "windows-ops.lisp"))
        (uiop:copy-file (merge-pathnames name +folder+) (merge-pathnames name mine)))
      (let ((nlk::*cells-directory* cells))
        (is (nlk:knowledge-folder-p mine) "the folder is one of the cells the index walks")
        (let ((entry (find "windows-ops" (nlk:knowledge-entries)
                          :key #'nlk:knowledge-entry-name :test #'string=)))
          (is entry "the index finds windows-ops")
          (is (and entry (nlk:knowledge-entry-description entry)) "it is listed with its sentence"))))))

(deftest windows-ops-cell-the-skill-answers-with-its-own-prose ()
  (let ((entry (windows-ops-entry)))
    (when (is entry "the skill is read")
      (let ((text (nlk:knowledge-help-text entry)))
        (is (search "windows-ops" (string-downcase text)) "its answer names it")
        (is (search (subseq (nlk:knowledge-entry-description entry) 0 40) text) "its answer leads with its sentence")
        (dolist (marker +markers+)
          (is (and (search marker text) t)
              (format nil "its answer carries its own text (~a)" marker)))
        (is (search "the parent dies first" text) "its answer carries the pitfall sections")))))

;;; --- the commands it names -----------------------------------------------------

(deftest windows-ops-cell-the-commands-it-names-are-beside-it ()
  (is (uiop:file-exists-p +reference+) "references/diagnostic-commands.md is there")
  (when (uiop:file-exists-p +reference+)
    (let ((text (uiop:read-file-string +reference+)))
      (is (> (length text) 1000) "it is the whole reference")
      (dolist (marker '("Get-CimInstance Win32_Process" "Format-Hex" "wslpath -w" "chcp"))
        (is (and (search marker text) t) (format nil "it carries the ~a check" marker)))
      (is (search "$env:USERPROFILE\\.nodecode" text) "it names this harness's home, not another's")
      ;; The one destructive pair is commented out: a sweep that reads, not one
      ;; that kills, is what an agent runs unattended.
      (flet ((live-line-p (line)
               (let ((line (string-left-trim " " line)))
                 (and (or (uiop:string-prefix-p "Stop-Process" line)
                          (uiop:string-prefix-p "taskkill" line))
                      t))))
        (let ((live (loop for line in (uiop:split-string text :separator '(#\Newline))
                          when (live-line-p line) collect line)))
          (is (null live) "no destructive line stands uncommented"))))))

;;; --- a skill with no sentence ---------------------------------------------------

(deftest windows-ops-cell-a-skill-with-no-sentence-is-refused ()
  (is (nlk:knowledge-form-p '(define-skill one-sentence "One sentence, the line the index lists.")) "a prose skill reads as knowledge")
  (is (not (nlk:knowledge-form-p '(define-skill no-sentence))) "a define-skill with no sentence is not knowledge the index reads")
  (signals-error error (eval '(nodecode.evolved:define-skill no-sentence)))
  (let ((path (merge-pathnames "windows-ops-no-sentence.lisp" (nlk:home))))
    (nlk:with-cleanup ((ignore-errors (delete-file path)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede
                                 :if-does-not-exist :create)
        (write-string "(in-package :nodecode.evolved)

(define-skill no-sentence)
" out))
      (let ((entries (windows-ops-read path)))
        (is (= 1 (length entries)) "the form is read as a definition")
        (is (null (nlk:knowledge-entry-description (first entries))) "with no sentence to list")
        (is (null (remove-if-not #'nlk:knowledge-entry-description entries)) "so the index leaves it out")))))
