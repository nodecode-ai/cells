;;;; support.lisp --- project manager test runner and shared helpers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Project manager tests register into the SAME nodecode.test registry (the
;;;; core DEFTEST) under a PROJECT-MANAGER-CELL- name prefix;
;;;; RUN-PROJECT-MANAGER-TESTS runs exactly that slice.
;;;;
;;;; The filesystem is real: one temp root holds two repositories (one a
;;;; plain .git folder, one the .git file a worktree keeps) and a folder that
;;;; is no repository, so the walk is the one under test. The one stub is git
;;;; itself, the cell's *GIT* seam: every read answers canned output and no
;;;; program runs, so the run needs neither git nor the network.

(in-package #:nodecode.test)

(defparameter *projects-root*
  (namestring (temp-path "pm-projects"))
  "A temp directory fixture: api/ and tools/web/ are repositories, plain/ is not.")

(defun build-projects-root (root)
  "ROOT holding two repositories and one plain folder."
  (write-temp-file (concatenate 'string root "api/.git/HEAD") "ref: refs/heads/main")
  (write-temp-file (concatenate 'string root "api/README.md") "# api")
  (write-temp-file (concatenate 'string root "tools/web/.git") "gitdir: ../..")
  (write-temp-file (concatenate 'string root "plain/notes.txt") "no repository here")
  root)

(define-test-slice "project-manager" "PROJECT-MANAGER-CELL-"
  :start nodecode-project-manager:start-cell
  :defaults (("watch_paths" (vector *projects-root*))))

(build-projects-root *projects-root*)

;;; --- git, stubbed ---------------------------------------------------------------

(defun pm-git-seam (answers)
  "A *GIT* seam answering the canned (ARGUMENTS . OUTPUT) in ANSWERS, keyed
by what follows `git -C DIRECTORY', and failing every other read."
  (lambda (arguments directory)
    (declare (ignore directory))
    (let ((hit (assoc (format nil "~{~a~^ ~}" (nthcdr 3 arguments)) answers :test #'equal)))
      (if hit (values (cdr hit) 0) (values "" 1)))))

(defun pm-config (&rest pairs)
  "A whole shared config section: the fixture root two levels deep, no cache,
small caps -- then PAIRS, replacing a default of the same name."
  (let* ((defaults (list "watch_paths" (vector *projects-root*)
                          "depth" 2 "cache_seconds" 0 "commits" 3 "branches" 3
                          "changed_files" 3 "max_projects" 10))
         (named (loop for (key) on pairs by #'cddr collect key)))
    (append (loop for (key value) on defaults by #'cddr
                  unless (member key named :test #'string=)
                    append (list key value))
            pairs)))

(defmacro with-projects ((answers) &body body)
  "BODY with the cell started on the fixture root and *GIT* answering ANSWERS,
stopped on unwind."
  `(let ((nodecode-project-manager:*git* (pm-git-seam ,answers)))
     (with-cell-stop ((project-manager-start ,@(pm-config)))
       ,@body)))

(defun pm-tool (name &rest pairs)
  "The tool NAME's answer to alternating KEY VALUE PAIRS, decoded through its
contract as a call is."
  (let* ((entry (nle::find-tool name))
         (object (apply #'nlk:make-json-object pairs)))
    (unless entry (error "no tool named ~a" name))
    (funcall (third entry) (nlk:contract-decode (second entry) object))))

(defparameter *pm-log*
  (format nil "a1b2c3d~c2026-11-01T09:30:00+00:00~cAda~cfix the login flow"
          #\Tab #\Tab #\Tab)
  "One commit as the cell reads it: hash, date, author, subject.")

(defparameter *pm-answers*
  (list (cons "rev-parse --abbrev-ref HEAD" "main")
        (cons "log -1 --format=%h%x09%aI%x09%an%x09%s" *pm-log*)
        (cons "status --porcelain" "")
        (cons "config --get remote.origin.url" "https://example.invalid/api.git"))
  "What git answers in the fixture, which is clean and tracks nothing.")
