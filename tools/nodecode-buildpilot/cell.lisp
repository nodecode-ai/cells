;;;; cell.lisp --- the store of projects, tasks and checkpoints, six tools, /buildpilot.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A project has an id, a name, an optional description and the moment it
;;;; was opened. A task belongs to one project and is pending, in progress or
;;;; completed. A checkpoint is one line of progress filed against a project;
;;;; the newest one comes back beside the project and its tasks, so a session
;;;; that resumes work reads where it stood without asking.
;;;;
;;;; One JSON document under the Nodecode home holds all three lists and the
;;;; id counters; every write is atomic (a temp file, then a rename), and
;;;; nothing here calls a model, a network or a program.
;;;;
;;;; Rewritten from Justnevo/buildpilot, whose six tools and three tables
;;;; this keeps; the SQLite file became one JSON document under the home.

(in-package #:nodecode-buildpilot)

;;; --- the manual ---------------------------------------------------------------

(defparameter *primer*
  "buildpilot keeps the projects you and the model work on apart from the
conversation: a project with its tasks, and checkpoints of where the work got
to, so a later session resumes instead of restarting. Six tools, and the same
reads as Lisp functions in the buildpilot: package through eval:
  buildpilot_add_project        name, description -> a project id
  buildpilot_list_projects      every project, with its task tally; an optional
                                 search keeps those whose name or description has it
  buildpilot_add_task           project_id, title -> a task id
  buildpilot_update_task        task_id, status (pending, in_progress, completed)
  buildpilot_checkpoint         project_id, summary -> a checkpoint id
  buildpilot_get_project_context  project_id -> the project, its tasks and the
                                 newest checkpoint in one answer
Call the context tool when you resume a project, and file a checkpoint after
each stretch of work: it is the one line the next session reads first.
  (buildpilot:projects [\"search\"])    every project, one line each
  (buildpilot:context 3)               one project in full
  (buildpilot:add-project \"name\" \"why\")
  (buildpilot:add-task 3 \"title\")
  (buildpilot:update-task 7 \"completed\")
  (buildpilot:checkpoint 3 \"what got done\")
State is one JSON document under the Nodecode home; nothing here calls a
model or a network."
  "What (help :buildpilot) answers while the cell runs.")

(defparameter +statuses+ '("pending" "in_progress" "completed")
  "A task's three states, as the store keeps them.")

(defparameter +marks+
  '(("pending" . "[ ]") ("in_progress" . "[>]") ("completed" . "[x]"))
  "One line's mark a status, the way a task list reads back.")

(defvar *state-file-override* nil
  "A file to read and write the store from instead of the section's, or NIL.
Tests set it; nothing else does.")

(defvar *last-error* nil
  "The last refusal a tool answered, so /buildpilot can show what went wrong.")

;;; --- where the store lives ---------------------------------------------------------

(defun now ()
  "The moment, in UTC, as the store keeps it."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month date hour minute second)))

(defun state-root ()
  "The state directory: the section's state_dir, or buildpilot/ under the home."
  (let ((dir (nlk:trimmed (princ-to-string (or (setting :state-dir) "")))))
    (if (plusp (length dir))
        (uiop:ensure-directory-pathname (nlk:expand-home dir))
        (nlk:home "buildpilot/"))))

(defun state-path ()
  "The one file the store is."
  (or *state-file-override* (merge-pathnames "state.json" (state-root))))

;;; --- reading and writing the one file ---------------------------------------------

(defun write-json-file (path object)
  "OBJECT at PATH as JSON, atomically: a temp file beside it, then a rename."
  (ensure-directories-exist path)
  (let ((tmp (make-pathname :name (format nil "~a.tmp" (pathname-name path))
                             :type (pathname-type path)
                             :defaults path)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:pretty-json object) out))
    (uiop:rename-file-overwriting-target tmp path))
  path)

(defun read-json-file (path)
  "PATH as a JSON object, or NIL when it is absent or unreadable."
  (when (probe-file path)
    (handler-case (nlk:decode-json (nlk:read-text path) :whole t)
      (error () nil))))

(defun jarr (object key)
  "OBJECT's member KEY as an array of rows, empty when it holds none."
  (or (nlk:json-value object :array key) #()))

(defun jint (object key)
  "OBJECT's member KEY as an integer, or NIL."
  (nlk:json-value object :integer key))

(defun jstr (object key)
  "OBJECT's member KEY as a string, or NIL."
  (nlk:json-value object :string key))

(defun rows (state key)
  "The rows of STATE's list KEY, as a list."
  (coerce (jarr state key) 'list))

(defun empty-state ()
  "A store with nothing in it."
  (nlk:make-json-object "counters" (nlk:make-json-object)
                         "projects" (vector) "tasks" (vector) "checkpoints" (vector)))

(defun load-state ()
  "The store, as it stands on disk; an absent or unreadable file is a new one."
  (let ((object (read-json-file (state-path))))
    (if (hash-table-p object)
        (progn
          (dolist (key '("projects" "tasks" "checkpoints"))
            (unless (nlk:json-value object :array key)
              (setf (gethash key object) (vector))))
          (unless (nlk:json-value object :object "counters")
            (setf (gethash "counters" object) (nlk:make-json-object)))
          object)
        (empty-state))))

(defun save-state (state)
  "STATE written as the store; its keep is pruned first."
  (write-json-file (state-path) state)
  state)

(defun counter (state kind)
  "The last id STATE handed out for KIND."
  (or (nlk:json-value (nlk:json-value state :object "counters") :integer kind) 0))

(defun bump-counter (state kind)
  "The next id for KIND, recorded in STATE."
  (let ((counters (nlk:json-value state :object "counters")))
    (let ((next (1+ (counter state kind))))
      (setf (gethash kind counters) next)
      next)))

(defun row-by-id (state key id)
  "The row of STATE's list KEY whose id is ID, or NIL."
  (and (integerp id) (find id (jarr state key) :key (lambda (row) (jint row "id")))))

(defun clip (text limit)
  "TEXT, cut to LIMIT characters."
  (let ((text (princ-to-string (or text ""))))
    (if (> (length text) limit) (subseq text 0 limit) text)))

(defun trimmed (value)
  (nlk:trimmed (princ-to-string (or value ""))))

;;; --- one project's parts ------------------------------------------------------------

(defun tasks-of (state project-id)
  "PROJECT-ID's tasks, oldest first."
  (remove-if-not (lambda (row) (eql (jint row "project_id") project-id))
                  (rows state "tasks")))

(defun checkpoints-of (state project-id)
  "PROJECT-ID's checkpoints, oldest first."
  (remove-if-not (lambda (row) (eql (jint row "project_id") project-id))
                  (rows state "checkpoints")))

(defun latest-checkpoint (state project-id)
  "PROJECT-ID's newest checkpoint, or NIL."
  (car (last (checkpoints-of state project-id))))

(defun project-or-fail (state project-id status)
  "The project ID names, or a refusal; STATUS says what asked for it."
  (or (row-by-id state "projects" project-id)
      (fail "no project #~a~@[ (asked for by ~a)~] -- list_projects shows the ids"
            project-id status)))

(defun tally (tasks)
  "TASKS' counts => the completed, the total, the in progress."
  (let ((done 0) (active 0) (total 0))
    (dolist (task tasks)
      (incf total)
      (let ((status (or (jstr task "status") "pending")))
        (cond ((string= status "completed") (incf done))
              ((string= status "in_progress") (incf active)))))
    (values done total active)))

(defun mark (status)
  (or (cdr (assoc status +marks+ :test #'string=)) "[ ]"))

(defun when-text (iso)
  "An ISO-8601 timestamp as a report writes it: its date and minute."
  (let ((iso (and iso (nlk:trimmed iso))))
    (if (and iso (>= (length iso) 16))
        (substitute #\Space #\T (subseq iso 0 16))
        (or iso ""))))

;;; --- the writes ----------------------------------------------------------------------

(defun open-project (name description)
  "A new project NAMEd NAME with DESCRIPTION. => its id, with the store written."
  (let* ((state (load-state))
         (name (trimmed name))
         (description (trimmed description)))
    (when (zerop (length name)) (fail "a project needs a name"))
    (let ((id (bump-counter state "project")))
      (setf (gethash "projects" state)
            (concatenate 'vector (jarr state "projects")
                         (vector (nlk:make-json-object
                                  "id" id "name" name "description" description
                                  "status" "active" "created_at" (now)))))
      (save-state state)
      id)))

(defun file-task (project-id title)
  "A task TITLEd TITLE in PROJECT-ID. => its id, with the store written."
  (let* ((state (load-state))
         (title (trimmed title)))
    (when (zerop (length title)) (fail "a task needs a title"))
    (project-or-fail state project-id "add_task")
    (let ((id (bump-counter state "task")))
      (setf (gethash "tasks" state)
            (concatenate 'vector (jarr state "tasks")
                         (vector (nlk:make-json-object
                                  "id" id "project_id" project-id "title" title
                                  "status" "pending" "created_at" (now)))))
      (save-state state)
      id)))

(defun move-task (task-id status)
  "TASK-ID moved to STATUS. => the task's row, with the store written."
  (let* ((state (load-state))
         (status (trimmed status)))
    (unless (member status +statuses+ :test #'string=)
      (fail "status must be one of ~{~a~^, ~}; ~s is not" +statuses+ status))
    (let ((task (or (row-by-id state "tasks" task-id)
                    (fail "no task #~a -- get_project_context shows the task ids" task-id))))
      (setf (gethash "status" task) status)
      (save-state state)
      task)))

(defun file-checkpoint (project-id summary)
  "A checkpoint of SUMMARY filed against PROJECT-ID. => its id, with the store written."
  (let* ((state (load-state))
         (summary (trimmed summary)))
    (when (zerop (length summary)) (fail "a checkpoint needs a summary: what got done"))
    (project-or-fail state project-id "checkpoint")
    (let* ((id (bump-counter state "checkpoint"))
           (kept (max 1 (or (setting :checkpoints-kept) 50)))
           (all (concatenate 'vector (jarr state "checkpoints")
                              (vector (nlk:make-json-object
                                       "id" id "project_id" project-id
                                       "summary" (clip summary 2000) "created_at" (now)))))
           (mine (remove-if-not (lambda (row) (eql (jint row "project_id") project-id))
                                 (coerce all 'list)))
           (others (remove-if (lambda (row) (eql (jint row "project_id") project-id))
                              (coerce all 'list)))
           (pruned (append others (last mine (min (length mine) kept)))))
      ;; the list is kept oldest-first, so the newest KEPT checkpoints of one
      ;; project are the ones left at its tail.
      (setf (gethash "checkpoints" state)
            (coerce (sort pruned #'< :key (lambda (row) (jint row "id"))) 'vector))
      (save-state state)
      id)))

;;; --- the reads, as text ----------------------------------------------------------------

(defun project-line (state project)
  "One project as a list writes it: its id, name and task tally."
  (multiple-value-bind (done total) (tally (tasks-of state (jint project "id")))
    (let ((latest (latest-checkpoint state (jint project "id"))))
      (format nil "  #~d  ~a  ~d task~:p (~d done)~@[  checkpoint: ~a~]"
              (jint project "id") (or (jstr project "name") "?")
              total done (and latest (when-text (jstr latest "created_at")))))))

(defun matching-projects (state search)
  "STATE's projects whose name or description contains SEARCH, or all of them."
  (let ((needle (trimmed search)))
    (if (plusp (length needle))
        (remove-if-not (lambda (project)
                         (or (search needle (or (jstr project "name") "") :test #'char-equal)
                             (search needle (or (jstr project "description") "") :test #'char-equal)))
                       (rows state "projects"))
        (rows state "projects"))))

(defun projects-text (&optional (search ""))
  "Every project, one line each. => a string."
  (let* ((state (load-state))
         (needle (trimmed search))
         (projects (matching-projects state needle)))
    (if (null projects)
        (if (plusp (length needle))
            (format nil "no project matches ~s -- buildpilot_list_projects without one lists all of them." needle)
            "no projects yet -- buildpilot_add_project starts one.")
        (with-output-to-string (out)
          (format out "~d project~:p~@[ matching ~s~]:~%" (length projects) (and (plusp (length needle)) needle))
          (dolist (project projects)
            (format out "~a~%" (project-line state project)))))))

(defun context-text (project-id)
  "One project in full: its tasks and its newest checkpoint. => a string."
  (let* ((state (load-state))
         (project (project-or-fail state project-id "get_project_context"))
         (tasks (tasks-of state project-id))
         (latest (latest-checkpoint state project-id)))
    (with-output-to-string (out)
      (format out "project #~d  ~a  (~a, opened ~a)~%"
              (jint project "id") (or (jstr project "name") "?")
              (or (jstr project "status") "active") (when-text (jstr project "created_at")))
      (let ((description (or (jstr project "description") "")))
        (when (plusp (length description)) (format out "  ~a~%" description)))
      (multiple-value-bind (done total active) (tally tasks)
        (format out "  ~d task~:p: ~d done, ~d in progress, ~d pending~%"
                total done active (- total done active)))
      (if (null tasks)
          (format out "  no tasks yet -- buildpilot_add_task files one.~%")
          (dolist (task tasks)
            (format out "    #~d ~a ~a~%" (jint task "id")
                    (mark (or (jstr task "status") "pending"))
                    (or (jstr task "title") "?"))))
      (if latest
          (format out "  latest checkpoint (~a): ~a~%"
                  (when-text (jstr latest "created_at")) (or (jstr latest "summary") ""))
          (format out "  no checkpoint yet -- buildpilot_checkpoint records where the work stands.~%")))))

;;; --- the boundary --------------------------------------------------------------------

(nlk:define-contract bp-add-project
  (:doc "Open a project to track work under: a name, and a line on why it exists.")
  ("name" :text :required t :doc "the project's name")
  ("description" :string :default "" :doc "one line on what the project is for"))

(nlk:define-contract bp-list-projects
  (:doc "List the projects kept here, one line each, with how many of each one's tasks are done and when it was last checkpointed.")
  ("search" :string :slot search-term :default "" :doc "keep only projects whose name or description contains this"))

(nlk:define-contract bp-add-task
  (:doc "File a task under a project. It starts pending; move it with buildpilot_update_task.")
  ("project_id" :integer :required t :doc "the project's id, as buildpilot_list_projects shows it")
  ("title" :text :required t :doc "what the task is"))

(nlk:define-contract bp-update-task
  (:doc "Move a task to pending, in_progress or completed.")
  ("task_id" :integer :required t :doc "the task's id, as buildpilot_get_project_context shows it")
  ("status" :text :required t :options +statuses+
   :doc "pending, in_progress or completed"))

(nlk:define-contract bp-add-checkpoint
  (:doc "Record where a project's work stands, in a line or two, so the next session resumes from it instead of guessing.")
  ("project_id" :integer :required t :doc "the project's id")
  ("summary" :text :required t :doc "what got done and what is next"))

(nlk:define-contract bp-project-context
  (:doc "One project in full: its tasks with their states, and its newest checkpoint. Call it when you resume a project.")
  ("project_id" :integer :required t :doc "the project's id"))

;;; --- the tools ------------------------------------------------------------------------

(defun answering (function)
  "FUNCTION's answer as a tool answers: a refusal is text the model repairs
from, not a signal out of the call."
  (handler-case (progn (setf *last-error* nil) (funcall function))
    (buildpilot-error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: ~a" condition)))
    (error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: BUILDPILOT-ERROR: ~a" condition)))))

(defun tool-add-project (call)
  (answering (lambda ()
               (let ((id (open-project (bp-add-project-name call)
                                      (bp-add-project-description call))))
                 (format nil "project #~d created: ~a" id (bp-add-project-name call))))))

(defun tool-list-projects (call)
  (answering (lambda () (projects-text (bp-list-projects-search-term call)))))

(defun tool-add-task (call)
  (answering (lambda ()
               (let ((id (file-task (bp-add-task-project-id call) (bp-add-task-title call))))
                 (format nil "task #~d filed in project #~d: ~a"
                         id (bp-add-task-project-id call) (bp-add-task-title call))))))

(defun tool-update-task (call)
  (answering (lambda ()
               (let ((task (move-task (bp-update-task-task-id call)
                                        (bp-update-task-status call))))
                 (format nil "task #~d is now ~a: ~a"
                         (jint task "id") (jstr task "status") (jstr task "title"))))))

(defun tool-checkpoint (call)
  (answering (lambda ()
               (let ((id (file-checkpoint (bp-add-checkpoint-project-id call)
                                      (bp-add-checkpoint-summary call))))
                 (format nil "checkpoint #~d saved for project #~d"
                         id (bp-add-checkpoint-project-id call))))))

(defun tool-project-context (call)
  (answering (lambda () (context-text (bp-project-context-project-id call)))))

;;; --- /buildpilot ----------------------------------------------------------------------

(defun run-slash (args session-id)
  "/buildpilot lists the projects; /buildpilot 3 shows one in full."
  (declare (ignore session-id))
  (let ((argument (trimmed args)))
    (handler-case
        (cond
          ((and (plusp (length argument)) (every #'digit-char-p argument))
           (context-text (parse-integer argument)))
          ((string-equal argument "help") *primer*)
          (t (let* ((state (load-state))
                    (projects (matching-projects state argument))
                    (text (projects-text argument)))
               (values text
                       (when projects
                         (nle:list-dialog
                          :title "Projects" :searchable t
                          :search-placeholder "filter by name"
                          :summary (format nil "~d project~:p" (length projects))
                          :rows (loop for project in projects
                                      collect (let ((id (jint project "id")))
                                                (list :label (format nil "#~d ~a" id (or (jstr project "name") "?"))
                                                      :detail (project-line state project)
                                                      :value (princ-to-string id))))))))))
      (buildpilot-error (condition) (format nil "ERROR: ~a" condition)))))

;;; --- the verbs an eval calls ----------------------------------------------------------

(define-verb projects (&optional (search ""))
  "Every project, one line each with its task tally, or just those SEARCH names."
  (projects-text search))

(define-verb context (project-id)
  "One project in full: its tasks and its newest checkpoint."
  (context-text project-id))

(define-verb add-project (name &optional (description ""))
  "Open a project NAMEd NAME. => a line naming its id."
  (let ((id (open-project name description)))
    (format nil "project #~d created: ~a" id (trimmed name))))

(define-verb add-task (project-id title)
  "File a task TITLEd TITLE under PROJECT-ID. => a line naming its id."
  (let ((id (file-task project-id title)))
    (format nil "task #~d filed in project #~d: ~a" id project-id (trimmed title))))

(define-verb update-task (task-id status)
  "Move TASK-ID to STATUS (pending, in_progress, completed). => the task's line."
  (let ((task (move-task task-id status)))
    (format nil "task #~d is now ~a: ~a" (jint task "id") (jstr task "status") (jstr task "title"))))

(define-verb checkpoint (project-id summary)
  "File SUMMARY as PROJECT-ID's newest checkpoint. => a line naming its id."
  (let ((id (file-checkpoint project-id summary)))
    (format nil "checkpoint #~d saved for project #~d" id project-id)))

;;; --- the declaration --------------------------------------------------------------------

(nle:define-cell buildpilot
  (:section ("buildpilot")
    (:guide "nothing is required: an empty state_dir keeps the store under the Nodecode home; next step: /buildpilot")
    ("state_dir" :string :default ""
     :doc "where the store JSON lives; empty means buildpilot/ in the Nodecode home")
    ("checkpoints_kept" :integer :default 50 :min 1
     :doc "how many checkpoints one project keeps; the oldest are dropped past it"))
  (:tool "buildpilot_add_project" (nlk:contract 'bp-add-project) 'tool-add-project)
  (:tool "buildpilot_list_projects" (nlk:contract 'bp-list-projects) 'tool-list-projects)
  (:tool "buildpilot_add_task" (nlk:contract 'bp-add-task) 'tool-add-task)
  (:tool "buildpilot_update_task" (nlk:contract 'bp-update-task) 'tool-update-task)
  (:tool "buildpilot_checkpoint" (nlk:contract 'bp-add-checkpoint) 'tool-checkpoint)
  (:tool "buildpilot_get_project_context" (nlk:contract 'bp-project-context) 'tool-project-context)
  (:command "buildpilot" 'run-slash
            :description "The projects you track: tasks done, where the work stands"
            :argument-hint "ID")
  (:help :buildpilot "buildpilot: projects, tasks and checkpoints - (buildpilot:projects), (buildpilot:context 1)" *primer*)
  (:start (lambda ()
            (setf *state-file-override* nil *last-error* nil)
            (nle:on-stop (lambda () (setf *last-error* nil))))))
