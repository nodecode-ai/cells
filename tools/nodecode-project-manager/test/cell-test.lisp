;;;; cell-test.lisp --- the walk, the reads, the five tools, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's wiring over a real temp tree and a
;;;; stubbed git: that the walk finds a repository by its .git folder and by
;;;; the .git file a worktree keeps, and never a folder without one; that the
;;;; detail, the branches and the commits say what git said, with the caps
;;;; the section sets; that a name no project answers is an error the model
;;;; reads; and that /projects answers the same list with a row a project.

(in-package #:nodecode.test)

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "project-manager"
  (:config "watch_paths" (vector *projects-root*)
           "depth" 2
           "cache_seconds" 0)
  (:help :projects)
  (:command "projects")
  (:running (is (= 1 (length (nodecode-project-manager::watch-roots))) "one watch path")
            (is (= 2 (nodecode-project-manager::setting :depth)) "the section's depth"))
  (:refused ("watch_paths" nil)
            ("depth" -1)
            ("cache_seconds" -1))
  (:idle nodecode-project-manager:project-manager-error
         (projects:projects)
         (projects:branches "api")
         (projects:watch-paths)))

;;; --- the walk, and what a list says -----------------------------------------------

(deftest project-manager-cell-lists-only-the-repositories-under-the-watch-paths ()
  (with-projects (*pm-answers*)
    (let ((text (pm-tool "list_projects")))
      (is (search "api" text) "a folder holding .git is a project")
      (is (search "web" text) "so is the .git file a worktree keeps, two levels down")
      (is (null (search "plain" text)) "a folder without one is no project")
      (is (search "main" text) "the branch is named")
      (is (search "fix the login flow" text) "and the last commit's subject")
      (is (search "2 projects" text) "the count, over the watch path it read"))))

(deftest project-manager-cell-a-search-keeps-the-projects-it-names ()
  (with-projects (*pm-answers*)
    (let ((text (pm-tool "list_projects" "query" "api")))
      (is (search "api" text) "the one that matches")
      (is (null (search "web" text)) "and no other"))))

;;; --- one project ------------------------------------------------------------------

(deftest project-manager-cell-a-detail-names-the-branch-and-the-changed-files ()
  (with-projects ((cons (cons "status --porcelain" (format nil " M a.lisp~%?? b.lisp"))
                         (cons (cons "rev-list --left-right --count @{upstream}...HEAD" (format nil "2~c1" #\Tab))
                               *pm-answers*)))
    (let ((text (pm-tool "get_project" "name" "api")))
      (is (search "branch: main" text) "the checked-out branch")
      (is (search "1 ahead, 2 behind" text) "how far it is from its upstream")
      (is (search "2 changed files" text) "what is uncommitted")
      (is (search "M a.lisp" text) "each of them")
      (is (search "fix the login flow" text) "and the last commit")
      (is (search "origin: https://example.invalid/api.git" text) "and where it pushes"))))

(deftest project-manager-cell-a-name-no-project-answers-is-an-error ()
  (with-projects (*pm-answers*)
    (let ((text (pm-tool "get_project" "name" "nothing")))
      (is (search "ERROR:" text) "the tool answers text, it does not signal")
      (is (search "nothing" text) "naming what was asked for"))))

;;; --- branches and commits -----------------------------------------------------------

(defparameter *pm-ref-format*
  "--format=%(refname:short)%09%(objectname:short)%09%(committerdate:iso-strict)%09%(subject)"
  "The one for-each-ref format the cell reads branches with.")

(deftest project-manager-cell-branches-mark-the-one-checked-out ()
  (with-projects ((cons (cons (format nil "for-each-ref --sort=-committerdate ~a refs/heads" *pm-ref-format*)
                               (format nil "main~ca1b2c3d~c2026-11-01T09:30:00+00:00~cfix the login flow~%feature/api~c77aa001~c2026-10-20T08:00:00+00:00~cadd the api"
                                       #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab))
                         *pm-answers*))
    (let ((text (pm-tool "list_branches" "name" "api")))
      (is (search "2 branches" text) "both branches, in one read")
      (is (search "* main" text) "the checked-out one marked")
      (is (search "feature/api" text) "and the other named")
      (is (search "add the api" text) "each with the tip commit's subject"))))

(deftest project-manager-cell-commits-honour-the-limit-and-the-branch ()
  (with-projects ((cons (cons "log -2 --format=%h%x09%aI%x09%an%x09%s main"
                               (format nil "a1b2c3d~c2026-11-01T09:30:00+00:00~cAda~cfix the login flow~%9f8e7d6~c2026-10-30T12:00:00+00:00~cAda~cadd the api"
                                       #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab))
                         (cons (cons "log -2 --format=%h%x09%aI%x09%an%x09%s"
                                     (format nil "9f8e7d6~c2026-10-30T12:00:00+00:00~cAda~cadd the api"
                                             #\Tab #\Tab #\Tab))
                               *pm-answers*)))
    (let ((text (pm-tool "get_recent_commits" "name" "api" "limit" 2 "branch" "main")))
      (is (search "2 commits on main" text) "asked for two, on main")
      (is (search "a1b2c3d" text) "the newest first")
      (is (search "9f8e7d6" text) "then the one before it"))
    (let ((text (pm-tool "get_recent_commits" "name" "api" "limit" 2)))
      (is (search "1 commit (newest first)" text) "the checked-out branch when none is named"))))

;;; --- the watch paths, and the command -----------------------------------------------

(deftest project-manager-cell-watch-paths-count-what-each-root-holds ()
  (with-projects (*pm-answers*)
    (let ((text (pm-tool "list_watch_paths")))
      (is (search (string-right-trim "/" *projects-root*) text) "the root it reads")
      (is (search "2 projects" text) "and how many it found there"))))

(deftest project-manager-cell-the-command-answers-the-list-and-a-row-a-project ()
  (with-projects (*pm-answers*)
    (multiple-value-bind (text dialog) (nodecode-project-manager::run-slash "" nil)
      (is (search "2 projects" text) "the same list the tool answers")
      (is (equal '("api" "web")
                 (mapcar (lambda (row) (getf row :label)) (getf dialog :rows)))
          "a row a project, in path order")
      (is (stringp (getf (first (getf dialog :rows)) :value)) "each row carrying its path"))))

(deftest project-manager-cell-the-command-keeps-what-its-search-names ()
  (with-projects (*pm-answers*)
    (multiple-value-bind (text dialog) (nodecode-project-manager::run-slash "web" nil)
      (is (search "web" text) "the one that matches")
      (is (= 1 (length (getf dialog :rows))) "one row"))))
