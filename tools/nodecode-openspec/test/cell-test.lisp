;;;; cell-test.lisp --- the config surface, and every verb over a real folder.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here: that the cell installs its twenty tools, /openspec
;;;; and (help :openspec) and takes them all back; that a section naming
;;;; neither a workdir nor a project refuses, and so does a ceiling under its
;;;; floor; that a change goes from scaffold to archived and back with its
;;;; checkboxes followed the whole way; that a spec parses into requirements
;;;; and scenarios and diffs against its baseline by name; that an idea is
;;;; captured, enriched and promoted with the idea kept in the proposal; that
;;;; validation names each problem with its file; and that the CLI passthrough
;;;; is an argv list, stubbed, never a shell.
;;;;
;;;; Every folder is a scratch one under the temporary directory: no test
;;;; reads the operator's project.

(in-package #:nodecode.test)

(define-test-slice "openspec" "OPENSPEC-CELL-" :start nodecode-openspec:start-cell)

;;; --- a scratch project ----------------------------------------------------------------

(defvar *scratch-count* 0)

(defun openspec-scratch-project ()
  "A fresh project folder holding an empty openspec/ layout."
  (let* ((root (merge-pathnames (format nil "openspec-test-~a-~a/"
                                         (get-universal-time) (incf *scratch-count*))
                                 (uiop:temporary-directory)))
         (openspec (merge-pathnames "openspec/" root)))
    (dolist (sub '("changes/archive/" "specs/" "ideas/"))
      (ensure-directories-exist (merge-pathnames sub openspec)))
    (namestring root)))

(defmacro with-openspec-project ((root &rest config) &body body)
  "BODY with ROOT a new project folder and the cell started over it."
  `(let ((,root (openspec-scratch-project)))
     (unwind-protect (with-cell-stop ((openspec-start "workdir" ,root ,@config)) ,@body)
       (ignore-errors (uiop:delete-directory-tree (pathname ,root) :validate t)))))

(defun ask (tool &rest pairs)
  "TOOL called with PAIRS as its JSON arguments; its answer as a plist of
(:text TEXT :object TABLE)."
  (let* ((text (funcall tool (apply #'nlk:make-json-object pairs)))
         (object (nlk:decode-json text)))
    (list :text text :object object)))

(deftest openspec-cell-a-missing-project-refuses ()
  (with-cell-stop ((openspec-start))
    (let ((answer (answered 'nodecode-openspec::tool-list)))
      (is (search "no project" answer) "a section naming neither a workdir nor a project refuses"))))

(defun asked (tool &rest pairs)
  (getf (apply #'ask tool pairs) :object))

(defun answered (tool &rest pairs)
  (getf (apply #'ask tool pairs) :text))

(defun field (object name)
  (nlk:json-value object :string name))

(defun ok-p (object)
  (eq t (nlk:json-value object :boolean "ok")))

(defun project (root &rest parts)
  (merge-pathnames (apply #'concatenate 'string parts) (pathname root)))

;;; --- lifecycle ------------------------------------------------------------------------

(define-cell-lifecycle-tests "openspec"
  (:config "workdir" (openspec-scratch-project))
  (:help :openspec)
  (:command "openspec")
  (:running (is (nle::find-tool "openspec_list") "openspec_list is registered")
            (is (nle::find-tool "openspec_context"))
            (is (nle::find-tool "openspec_show"))
            (is (nle::find-tool "openspec_status"))
            (is (nle::find-tool "openspec_instructions"))
            (is (nle::find-tool "openspec_task_list"))
            (is (nle::find-tool "openspec_task_set_status"))
            (is (nle::find-tool "openspec_change_create"))
            (is (nle::find-tool "openspec_change_promote"))
            (is (nle::find-tool "openspec_change_archive"))
            (is (nle::find-tool "openspec_change_unarchive"))
            (is (nle::find-tool "openspec_idea_create"))
            (is (nle::find-tool "openspec_idea_enrich"))
            (is (nle::find-tool "openspec_idea_promote"))
            (is (nle::find-tool "openspec_spec_list"))
            (is (nle::find-tool "openspec_spec_show"))
            (is (nle::find-tool "openspec_spec_create"))
            (is (nle::find-tool "openspec_spec_diff"))
            (is (nle::find-tool "openspec_validate"))
            (is (nle::find-tool "openspec_cli"))
            (is (= 50000 (nodecode-openspec::setting :max-chars)) "a ceiling with its default"))
  ;; neither a workdir nor a project: the section refuses, and a ceiling
  ;; under its floor does too
  (:refused ("max_chars" 5))
  (:idle nodecode-openspec:openspec-error (nodecode-openspec:status)))

;;; --- a change, end to end --------------------------------------------------------------

(deftest openspec-cell-a-change-is-scaffolded-worked-and-archived ()
  (with-openspec-project (root)
    (let ((created (asked 'nodecode-openspec::tool-change-create
                          "change" "add-login" "tasks" #("Write the spec" "Ship it"))))
      (is (ok-p created) "a change is created")
      (is (equal "todo" (field created "status")) "two open boxes read todo")
      (is (uiop:file-exists-p (project root "openspec/changes/add-login/proposal.md")))
      (is (uiop:file-exists-p (project root "openspec/changes/add-login/tasks.md")))
      (is (uiop:file-exists-p (project root "openspec/changes/add-login/specs/add-login/spec.md")) "a delta spec placeholder goes with it"))
    (let ((listing (asked 'nodecode-openspec::tool-list "kind" "changes")))
      (is (= 1 (length (gethash "changes" listing))))
      (is (equal "add-login" (field (aref (gethash "changes" listing) 0) "name")))
      (is (zerop (nlk:json-value (gethash "tasks" (aref (gethash "changes" listing) 0)) :integer "done")) "no box is ticked yet")
      (is (= 2 (nlk:json-value (gethash "tasks" (aref (gethash "changes" listing) 0)) :integer "total"))))
    (let ((task-list (asked 'nodecode-openspec::tool-task-list "change" "add-login")))
      (is (equal "1.1" (field (aref (gethash "tasks" task-list) 0) "id")) "the first task's id")
      (is (equal "Write the spec" (field (aref (gethash "tasks" task-list) 0) "text"))))
    (let ((set (asked 'nodecode-openspec::tool-task-set "change" "add-login"
                                                         "tasks" #("1.1") "status" "done")))
      (is (ok-p set))
      (is (equal "in_progress" (field set "status")) "one of two boxes: in progress")
      (is (search "- [x] 1.1 Write the spec"
                  (uiop:read-file-string (project root "openspec/changes/add-login/tasks.md"))) "the checkbox is ticked in place, its id and text kept"))
    (let ((archive (asked 'nodecode-openspec::tool-change-archive "change" "add-login")))
      (is (not (ok-p archive)) "an unfinished change is refused")
      (is (search "not complete" (field archive "error"))))
    (asked 'nodecode-openspec::tool-task-set "change" "add-login"
                                            "tasks" #("Ship it") "status" "done")
    (let ((archive (asked 'nodecode-openspec::tool-change-archive "change" "add-login")))
      (is (ok-p archive) "every box ticked: it archives")
      (is (equal "archived" (field archive "status")))
      (is (uiop:directory-exists-p (project root "openspec/changes/archive/add-login/"))))
    (let ((status (asked 'nodecode-openspec::tool-status "change" "add-login")))
      (is (ok-p status))
      (is (equal "archived" (field (aref (gethash "changes" status) 0) "status"))))
    (let ((back (asked 'nodecode-openspec::tool-change-unarchive "change" "add-login")))
      (is (ok-p back))
      (is (equal "done" (field back "status")) "unarchived, its boxes still ticked")
      (is (uiop:directory-exists-p (project root "openspec/changes/add-login/"))))))

(deftest openspec-cell-a-bad-change-id-refuses-and-nothing-is-written ()
  (with-openspec-project (root)
    (let ((bad (asked 'nodecode-openspec::tool-change-create "change" "Not A Slug!")))
      ;; the id is slugified, so this one is created as not-a-slug
      (is (ok-p bad))
      (is (equal "not-a-slug" (field bad "change"))))
    (let ((again (asked 'nodecode-openspec::tool-change-create "change" "not-a-slug")))
      (is (not (ok-p again)))
      (is (search "already exists" (field again "error"))))
    (is (uiop:directory-exists-p (project root "openspec/changes/not-a-slug/")))))

;;; --- specs ----------------------------------------------------------------------------

(defparameter +baseline-spec+
  "# Agent Tools

## Purpose

Tools that let an agent inspect a project.

## Requirements

### Requirement: List changes

The tool SHALL list every active change.

#### Scenario: A project with changes
- **GIVEN** a project with one change
- **WHEN** the tool is called
- **THEN** the change is listed

### Requirement: Show a change

The tool SHALL return one change whole.

#### Scenario: A named change
- **GIVEN** a change named add-login
- **WHEN** the tool is called with that name
- **THEN** its proposal and tasks are returned
")

(defparameter +change-spec+
  "# Agent Tools

## Purpose

Tools that let an agent inspect a project.

## ADDED Requirements

### Requirement: List changes

The tool SHALL list every active change, newest first.

#### Scenario: A project with changes
- **GIVEN** a project with one change
- **WHEN** the tool is called
- **THEN** the change is listed

### Requirement: Archive a change

The tool SHALL move a finished change into the archive.

#### Scenario: A finished change
- **GIVEN** a change whose boxes are all ticked
- **WHEN** the change is archived
- **THEN** it reads archived
")

(deftest openspec-cell-a-spec-parses-and-diffs-by-requirement-name ()
  (with-openspec-project (root)
    (let ((baseline (project root "openspec/specs/agent-tools/spec.md"))
          (delta (project root "openspec/changes/add-login/specs/agent-tools/spec.md")))
      (nodecode-openspec::write-text baseline +baseline-spec+)
      (nodecode-openspec::write-text delta +change-spec+)
      (let* ((shown (asked 'nodecode-openspec::tool-spec-show "spec" "agent-tools"))
             (requirements (gethash "requirements" shown))
             (scenario (aref (gethash "scenarios" (aref requirements 0)) 0))
             (step (aref (gethash "steps" scenario) 0)))
        (is (ok-p shown))
        (is (equal "Agent Tools" (field shown "title")))
        (is (= 2 (length requirements)) "two requirements parsed")
        (is (equal "List changes" (field (aref requirements 0) "name")))
        (is (equal "A project with changes" (field scenario "name")))
        (is (equal "GIVEN" (field step "type")) "the step's type")
        (is (equal "a project with one change" (field step "text")) "and its text"))
      (let* ((diff (asked 'nodecode-openspec::tool-spec-diff "spec" "agent-tools" "change" "add-login"))
             (requirements (gethash "requirements" diff)))
        (is (ok-p diff))
        (is (equal "modified" (field diff "status")))
        (is (equal "Archive a change" (field (aref (gethash "added" requirements) 0) "name")) "the new requirement is added")
        (is (equal "List changes" (field (aref (gethash "modified" requirements) 0) "name")) "the retitled one is modified")
        (is (search "openspec/specs/agent-tools/spec.md" (field diff "baseline")) "the baseline is named")))))

(deftest openspec-cell-a-spec-is-written-from-structured-input ()
  (with-openspec-project (root)
    (let ((made (asked 'nodecode-openspec::tool-spec-create
                       "spec" "agent-tools" "title" "Agent Tools"
                       "purpose" "Tools that let an agent inspect a project."
                       "requirements"
                       (vector (nlk:json-object
                                "name" "List changes"
                                "description" "The tool SHALL list every active change."
                                "scenarios"
                                (vector (nlk:json-object
                                         "name" "A project with changes"
                                         "steps" (vector (nlk:json-object "type" "GIVEN" "text" "a project")
                                                         (nlk:json-object "type" "THEN" "text" "it is listed")))))))))
      (is (ok-p made) "the spec is written")
      (let ((text (uiop:read-file-string (project root "openspec/specs/agent-tools/spec.md"))))
        (is (search "### Requirement: List changes" text))
        (is (search "- **THEN** it is listed" text)))
      (let ((again (asked 'nodecode-openspec::tool-spec-create
                          "spec" "agent-tools" "title" "x" "purpose" "y"
                          "requirements" (vector (nlk:json-object "name" "z")))))
        (is (not (ok-p again)) "an existing spec is refused without force")
        (is (search "already exists" (field again "error")))))))

;;; --- ideas ----------------------------------------------------------------------------

(deftest openspec-cell-an-idea-is-captured-enriched-and-promoted ()
  (with-openspec-project (root)
    (let ((idea (asked 'nodecode-openspec::tool-idea-create
                       "title" "Search the board" "prompt" "let me filter the cards"
                       "tags" #("board" "ui"))))
      (is (ok-p idea))
      (is (equal "search-the-board" (field idea "slug")))
      (is (uiop:file-exists-p (project root "openspec/ideas/search-the-board.md"))))
    (let ((bad (asked 'nodecode-openspec::tool-idea-enrich "idea" "search-the-board"
                                                          "feasibility" "maybe")))
      (is (not (ok-p bad)) "feasibility outside its set refuses")
      (is (search "feasibility must be one of" (field bad "error"))))
    (let ((enriched (asked 'nodecode-openspec::tool-idea-enrich
                           "idea" "search-the-board" "feasibility" "high" "tshirt_size" "s"
                           "problem" "twenty cards" "risks" #("the filter is slow"))))
      (is (ok-p enriched))
      (let ((text (uiop:read-file-string (project root "openspec/ideas/search-the-board.md"))))
        (is (search "### Feasibility" text))
        (is (search "Feasibility: high" text))
        (is (search "- the filter is slow" text))))
    (let ((promoted (asked 'nodecode-openspec::tool-idea-promote "idea" "search-the-board"
                                                               "change" "board-search")))
      (is (ok-p promoted))
      (is (equal "board-search" (field promoted "change")))
      (let ((proposal (uiop:read-file-string (project root "openspec/changes/board-search/proposal.md"))))
        (is (search "## Source Idea" proposal))
        (is (search "openspec/ideas/search-the-board.md" proposal) "the idea stays traceable")
        (is (search "### Requirement:" (uiop:read-file-string
                                        (project root "openspec/changes/board-search/specs/board-search/spec.md")))))
      (let ((tasks (asked 'nodecode-openspec::tool-task-list "change" "board-search")))
        (is (= 6 (length (gethash "tasks" tasks))) "the promoted checklist")
        (is (equal "todo" (field tasks "status")))))))

;;; --- validation -----------------------------------------------------------------------

(deftest openspec-cell-validation-names-the-problem-and-its-file ()
  (with-openspec-project (root)
    (asked 'nodecode-openspec::tool-change-create "change" "bare" "tasks" #("one"))
    (uiop:delete-file-if-exists (project root "openspec/changes/bare/specs/bare/spec.md"))
    (let* ((report (asked 'nodecode-openspec::tool-validate))
           (change (aref (gethash "changes" report) 0))
           (errors (gethash "errors" change)))
      (is (not (ok-p report)) "a change with no delta spec does not validate")
      (is (equal "bare" (field change "name")))
      (is (some (lambda (problem) (equal "missing_specs" (field problem "code"))) (coerce errors 'list)) "the missing delta spec is named"))
    (let ((clean (asked 'nodecode-openspec::tool-validate "change" "bare")))
      (is (not (nlk:json-value (aref (gethash "changes" clean) 0) :boolean "valid"))))))

;;; --- the CLI --------------------------------------------------------------------------

(deftest openspec-cell-the-cli-is-an-argv-list-through-a-stub ()
  (with-openspec-project (root)
    (let* ((seen '())
           (nodecode-openspec::*run-command*
            (lambda (argv &key directory)
              (declare (ignore directory))
              (push argv seen)
              (list "{\"changes\":[]}" "" 0))))
      (let ((answer (asked 'nodecode-openspec::tool-cli "command" "list --changes")))
        (is (ok-p answer))
        (is (equal "{\"changes\":[]}" (field answer "stdout")))
        (is (equal '("openspec" "list" "--changes" "--json") (first seen)) "an argv list, the JSON flag appended, no shell"))
      (let* ((without-json (lambda (argv &key directory)
                              (declare (ignore directory))
                              (push argv seen)
                              (list "" "" 0)))
             (nodecode-openspec::*run-command* without-json)
             (plain (asked 'nodecode-openspec::tool-cli "command" "status" "json_output" nil)))
        (is (ok-p plain))
        (is (notany (lambda (word) (equal "--json" word)) (first seen)) "json_output false leaves the flag off"))
      (let ((nodecode-openspec::*run-command*
              (lambda (argv &key directory)
                (declare (ignore argv directory))
                (list "" "no such file" 1))))
        (let ((failed (asked 'nodecode-openspec::tool-cli "command" "status")))
          (is (not (ok-p failed)))
          (is (equal "no such file" (field failed "stderr")))
          (is (= 1 (nlk:json-value failed :integer "exit_code"))))))))

;;; --- the identifiers ------------------------------------------------------------------

(deftest openspec-cell-identifiers-resolve-by-project-name ()
  (with-openspec-project (root "projects" (vector (format nil "mine=~a" root)))
    (asked 'nodecode-openspec::tool-change-create "change" "add-login")
    (let ((board (asked 'nodecode-openspec::tool-context "identifier" "mine")))
      (is (ok-p board))
      (is (equal "mine" (field board "name")))
      (is (= 1 (length (gethash "changes" board)))))
    (let ((one (asked 'nodecode-openspec::tool-context "identifier" "mine/add-login")))
      (is (ok-p one))
      (is (equal "add-login" (field (gethash "change" one) "name")))
      (is (search "## Why" (field (gethash "change" one) "proposal"))))
    (let ((missing (asked 'nodecode-openspec::tool-context "identifier" "elsewhere")))
      (is (not (ok-p missing)))
      (is (search "no project named" (field missing "error"))))))

;;; --- the slash command ----------------------------------------------------------------

(deftest openspec-cell-the-slash-command-reads-the-board ()
  (with-openspec-project (root)
    (is (search "no active change" (nodecode-openspec:run-slash "" "s1")))
    (asked 'nodecode-openspec::tool-change-create "change" "add-login" "tasks" #("one"))
    (let ((text (nodecode-openspec:run-slash "" "s1")))
      (is (search "add-login" text))
      (is (search "todo" text))
      (is (search "0/1 tasks" text)))))
