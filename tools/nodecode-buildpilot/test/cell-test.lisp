;;;; cell-test.lisp --- six tools, the store on disk, /buildpilot.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing is stubbed and nothing dials: the cell's state is one JSON file
;;;; in a temp folder this run owns, so every test writes and reads a real
;;;; store. What is proved is the wiring the loader makes -- the six tools,
;;;; the command, the manual and the refusals -- the shape of what the tools
;;;; answer, and that a project outlives a stop and a start.

(in-package #:nodecode.test)

(defparameter *bp-root* (namestring (temp-path "bp-state"))
  "A temp directory for the store, so one test never reads another's projects.")

(define-test-slice "buildpilot" "BUILDPILOT-CELL-"
  :start nodecode-buildpilot:start-cell
  :defaults (("state_dir" *bp-root*)
             ("checkpoints_kept" 5)))

(define-cell-lifecycle-tests "buildpilot"
  (:help :buildpilot)
  (:command "buildpilot")
  (:refused ("state_dir" 42)
            ("checkpoints_kept" 0))
  (:idle nodecode-buildpilot:buildpilot-error
         (buildpilot:projects)
         (buildpilot:context 1)))

;;; --- helpers --------------------------------------------------------------------

(defun bp-fresh-store ()
  "An empty store folder for one test."
  (let ((dir (uiop:ensure-directory-pathname *bp-root*)))
    (when (uiop:directory-exists-p dir)
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))
  *bp-root*)

(defmacro with-projects (&body body)
  "BODY with the cell started over an empty store, stopped on unwind."
  `(progn
     (bp-fresh-store)
     (with-cell-stop ((buildpilot-start))
       ,@body)))

(defun bp-tool (name &rest pairs)
  "The tool NAME's answer to alternating KEY VALUE PAIRS, decoded through its
contract as a call is."
  (let ((entry (nle::find-tool name)))
    (unless entry (error "no tool named ~a" name))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun bp-schema (name)
  (second (nle::find-tool name)))

(defun bp-command-text (&optional (argument ""))
  "/buildpilot ARGUMENT, run the way a shell with no model runs it."
  (let ((entry (nle::find-registered-command "buildpilot")))
    (unless entry (error "/buildpilot is not registered"))
    (funcall (nle::slash-command-headless entry) argument "s1")))

(defun bp-id (text)
  "The first #N in TEXT, as an integer."
  (let* ((hash (position #\# text))
         (start (and hash (1+ hash))))
    (and start
         (parse-integer text :start start
                             :junk-allowed t))))

(defun bp-project ()
  "A project with two tasks, one of them completed, through the tools."
  (let* ((answer (bp-tool "buildpilot_add_project" "name" "parser" "description" "port the old parser"))
         (id (bp-id answer)))
    (bp-tool "buildpilot_add_task" "project_id" id "title" "read the old one")
    (let ((task (bp-id (bp-tool "buildpilot_add_task" "project_id" id "title" "port it"))))
      (bp-tool "buildpilot_update_task" "task_id" task "status" "completed"))
    id))

(deftest buildpilot-cell-a-project-its-tasks-and-its-context-answer-through-the-contracts ()
  (with-projects
    (let ((id (bp-project)))
      (let ((text (bp-tool "buildpilot_list_projects")))
        (is (search "parser" text) "the project is listed by the name it was given")
        (is (search "2 tasks (1 done)" text) "with its task tally"))
      (let ((text (bp-tool "buildpilot_get_project_context" "project_id" id)))
        (is (search "port the old parser" text) "the description comes back beside the project")
        (is (search "[x] port it" text) "a completed task is marked done")
        (is (search "[ ] read the old one" text) "and one still pending is marked open")
        (is (search "2 tasks: 1 done" text) "with the tally over all of them")))))

(deftest buildpilot-cell-a-checkpoint-is-the-latest-one-the-context-carries ()
  (with-projects
    (let* ((id (bp-project))
           (answer (bp-tool "buildpilot_checkpoint" "project_id" id
                             "summary" "the lexer is ported")))
      (is (search "checkpoint #" answer) "the checkpoint answers its id")
      (bp-tool "buildpilot_checkpoint" "project_id" id "summary" "the parser is next")
      (let ((text (bp-tool "buildpilot_get_project_context" "project_id" id)))
        (is (search "latest checkpoint" text) "the context carries a checkpoint")
        (is (search "the parser is next" text) "the newest one")
        (is (null (search "the lexer is ported" text)) "and not the one it replaced"))
      (is (search "checkpoint: " (bp-tool "buildpilot_list_projects")) "the list names when it was last checkpointed"))))

(deftest buildpilot-cell-the-store-keeps-only-the-newest-checkpoints-it-is-told-to ()
  (bp-fresh-store)
  (with-cell-stop ((buildpilot-start "checkpoints_kept" 2))
    (let ((id (bp-project)))
      (dolist (summary '("first" "second" "third"))
        (bp-tool "buildpilot_checkpoint" "project_id" id "summary" summary))
      (let ((state (nodecode-buildpilot::load-state)))
        (is (= 2 (length (nodecode-buildpilot::rows state "checkpoints"))) "two kept, the oldest dropped")
        (is (null (search "first" (bp-tool "buildpilot_get_project_context" "project_id" id))) "the oldest is gone")))))

(deftest buildpilot-cell-a-project-outlives-a-stop-and-a-start ()
  (bp-fresh-store)
  (let ((id nil))
    (with-cell-stop ((buildpilot-start))
      (setf id (bp-project)))
    (with-cell-stop ((buildpilot-start))
      (let ((text (bp-tool "buildpilot_get_project_context" "project_id" id)))
        (is (search "parser" text) "the project is read off disk by the next start")
        (is (search "[x] port it" text) "with the task state it reached")))))

(deftest buildpilot-cell-a-call-that-leaves-out-a-required-value-is-refused ()
  (with-projects
    (is (signals-error nlk:contract-refusal
          (nlk:contract-decode (bp-schema "buildpilot_add_project")
                                (nlk:make-json-object "description" "no name here"))) "a project with no name is refused before the body runs")
    (is (signals-error nlk:contract-refusal
          (nlk:contract-decode (bp-schema "buildpilot_update_task")
                                (nlk:make-json-object "task_id" 1 "status" "done"))) "and so is a status outside the three the store keeps")))

(deftest buildpilot-cell-a-refusal-names-what-it-could-not-find ()
  (with-projects
    (let ((text (bp-tool "buildpilot_get_project_context" "project_id" 99)))
      (is (search "ERROR: " text) "an unknown project is a failed call, not a signal")
      (is (search "no project #99" text) "naming the id asked for"))
    (let ((text (bp-tool "buildpilot_add_task" "project_id" 99 "title" "orphan")))
      (is (search "no project #99" text) "a task needs a project that exists"))
    (let ((text (bp-tool "buildpilot_update_task" "task_id" 42 "status" "completed")))
      (is (search "no task #42" text) "and a task is moved by an id that exists"))))

(deftest buildpilot-cell-a-search-keeps-only-the-projects-it-names ()
  (with-projects
    (bp-project)
    (bp-tool "buildpilot_add_project" "name" "release notes" "description" "what shipped")
    (let ((text (bp-tool "buildpilot_list_projects" "search" "release")))
      (is (search "release notes" text) "the one that matches")
      (is (null (search "parser" text)) "and no other"))))

(deftest buildpilot-cell-the-command-lists-the-projects-and-opens-one-by-its-id ()
  (with-projects
    (let ((id (bp-project)))
      (bp-tool "buildpilot_add_project" "name" "release notes")
      (multiple-value-bind (text dialog) (nodecode-buildpilot::run-slash "" nil)
        (is (search "2 projects" text) "the same list the tool answers")
        (is (equal '("#1 parser" "#2 release notes")
                   (mapcar (lambda (row) (getf row :label)) (getf dialog :rows))) "a row a project, oldest first"))
      (let ((text (bp-command-text (princ-to-string id))))
        (is (search "project #1" text) "an id shows that project in full")
        (is (search "port it" text) "with its tasks"))
      (is (search "buildpilot_add_project" (bp-command-text "help")) "and help, the manual"))))
