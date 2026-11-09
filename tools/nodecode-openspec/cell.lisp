;;;; cell.lisp --- the openspec tools, and the cell that registers them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Twenty tools over one project's openspec/ folder, the same surface the
;;;; original Hermes plugin gave a model: list and show changes and specs,
;;;; derive a change's status from its checkboxes, scaffold a change, toggle
;;;; its tasks, promote it, archive it, create, enrich and promote ideas, write
;;;; and diff specs, validate, and hand out the authoring guides. Every one
;;;; answers a JSON string; a refusal answers {"ok": false, "error": ...}.
;;;;
;;;; Nothing here needs Node, Python or the openspec CLI: the artifacts are
;;;; plain markdown, read and written directly. openspec_cli is the one tool
;;;; that runs the CLI, for the subcommands the files do not carry, and it is
;;;; a passthrough the caller opted into.

(in-package #:nodecode-openspec)

;;; --- answers --------------------------------------------------------------------------

(defun answer (object)
  (nlk:encode-json-object object))

(defun answer-error (text)
  (answer (nlk:json-object "ok" nil "error" text)))

(defmacro answering (&body body)
  "BODY's JSON text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (openspec-error (condition)
       (answer (nlk:json-object "ok" nil "error" (openspec-error-detail condition))))
     (error (condition)
       (answer (nlk:json-object "ok" nil
                                 "error" (format nil "openspec: ~a"
                                                  (nlk:one-line (princ-to-string condition))))))))

(defun clip (text)
  "TEXT, cut to the section's ceiling."
  (let ((ceiling (or (ignore-errors (setting :max-chars)) 50000)))
    (if (> (length text) ceiling)
        (concatenate 'string (subseq text 0 ceiling) (format nil "~%[cut at ~a characters]" ceiling))
        text)))

(defun clip-content (object)
  "OBJECT with each \"content\" clipped: a show never carries a whole file twice."
  object)

;;; --- arguments ------------------------------------------------------------------------

(defun arg (arguments name)
  (nlk:json-value arguments :string name))

(defun arg-text (arguments name)
  (nlk:trimmed (or (nlk:json-value arguments :string name) "")))

(defun arg-list (arguments name)
  (coerce (or (nlk:json-array arguments name) #()) 'list))

(defun arg-boolean (arguments name)
  (and (eq t (nlk:json-value arguments :boolean name)) t))

(defun arg-integer (arguments name)
  (nlk:json-value arguments :integer name))

(defun specified (value)
  (and value (plusp (length (nlk:trimmed value))) value))

;;; --- sources --------------------------------------------------------------------------

(defun project-sources ()
  "The projects this cell can name, as (NAME PATH): the section's `projects'
entries, then its workdir under its folder's own name."
  (let ((out '()))
    (dolist (entry (let ((named (ignore-errors (setting :projects))))
                     (if (vectorp named) (coerce named 'list) named)))
      (let* ((text (nlk:trimmed (or entry "")))
             (equals (position #\= text)))
        (when (plusp (length text))
          (let* ((name (if equals (string-trim " " (subseq text 0 equals)) ""))
                 (path (directory-pathname (if equals (subseq text (1+ equals)) text))))
            (when path
              (push (list (if (plusp (length name)) name (folder-name path)) path) out))))))
    (let ((default (directory-pathname (ignore-errors (setting :workdir)))))
      (when default
        (pushnew (list (folder-name default) default) out :test #'equal :key #'second)))
    (nreverse out)))

(defun source-named (name)
  (or (find name (project-sources) :key #'first :test #'string=)
      (fail "no project named ~s: name one in the openspec section's projects (name=path), or set workdir" name)))

;;; --- changes --------------------------------------------------------------------------

(defun tasks-json (tasks)
  "TASKS as the wire objects a tool answers."
  (coerce (mapcar (lambda (task)
                     (nlk:json-object "id" (getf task :id)
                                      "text" (getf task :text)
                                      "status" (getf task :status)
                                      "line" (getf task :line)))
                   tasks)
          'vector))

(defun change-entry (root folder dir archived)
  (let* ((tasks (change-tasks dir))
         (counts (task-counts tasks)))
    (nlk:json-object "name" folder
                      "title" (change-title dir folder)
                      "status" (change-status dir :archived archived)
                      "archived" archived
                      "token" folder
                      "path" (relative dir root)
                      "tasks" (nlk:json-object "total" (getf counts :total)
                                                "done" (getf counts :done)
                                                "todo" (getf counts :todo)))))

(defun op-list (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (kind (or (arg arguments "kind") "changes"))
         (sort (or (arg arguments "sort") "recent")))
    (cond
      ((member kind '("specs" "spec") :test #'string=)
       (nlk:json-object "ok" t "kind" "specs" "workdir" (namestring root)
                        "specs" (coerce (mapcar (lambda (name) (nlk:json-object "name" name "path" (format nil "~a/spec.md" name)))
                                                (spec-names root))
                                        'vector)))
      (t
       (let* ((folders (change-folders root))
              (entries (mapcar (lambda (tuple) (apply #'change-entry root tuple)) folders))
              (sorted (if (string= sort "name")
                          (sort entries #'string< :key (lambda (entry) (gethash "name" entry)))
                          (stable-sort-by-recency entries folders))))
         (nlk:json-object "ok" t "kind" "changes" "workdir" (namestring root) "count" (length sorted)
                          "changes" (coerce sorted 'vector)))))))

(defun stable-sort-by-recency (entries folders)
  "ENTRIES (one per FOLDERS tuple) newest proposal.md first."
  (let ((dates (make-hash-table :test #'equal)))
    (dolist (tuple folders)
      (let ((proposal (merge-pathnames "proposal.md" (second tuple))))
        (setf (gethash (first tuple) dates)
              (if (uiop:file-exists-p proposal) (file-write-date proposal) 0))))
    (stable-sort entries #'>
                 :key (lambda (entry) (or (gethash (gethash "name" entry) dates) 0)))))

(defun change-json (root folder dir archived &aux (relative-path (relative dir root)))
  (let ((specs (loop for path in (recursive-markdown-files (merge-pathnames "specs/" dir))
                     collect (nlk:json-object "path" (relative path dir)
                                               "content" (clip (read-text path))))))
    (nlk:json-object "name" folder
                      "title" (change-title dir folder)
                      "token" folder
                      "path" relative-path
                      "archived" archived
                      "status" (change-status dir :archived archived)
                      "tasks" (tasks-json (change-tasks dir))
                      "counts" (let ((counts (task-counts (change-tasks dir))))
                                 (nlk:json-object "total" (getf counts :total)
                                                  "done" (getf counts :done)
                                                  "todo" (getf counts :todo)))
                      "proposal" (clip (read-text (merge-pathnames "proposal.md" dir)))
                      "design" (clip (read-text (merge-pathnames "design.md" dir)))
                      "tasks_file" (clip (read-text (merge-pathnames "tasks.md" dir)))
                      "specs" (coerce specs 'vector))))

(defun op-show (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (name (specified (arg-text arguments "name")))
         (kind (or (arg arguments "type") "")))
    (unless name (fail "name is required: a change folder's name, or a spec's"))
    (if (member kind '("spec" "specs") :test #'string=)
        (let* ((change (specified (arg-text arguments "change")))
               (path (spec-path root name change)))
          (unless (uiop:file-exists-p path) (fail "spec not found: ~a" (relative path root)))
          (nlk:json-object "ok" t "kind" "spec" "spec" name "path" (relative path root)
                            "parsed" (parsed-json (parse-spec (read-text path)))
                            "content" (clip (read-text path))))
        (multiple-value-bind (dir archived-p) (find-change root name)
          (cond
            (dir (nlk:json-object "ok" t "kind" "change" "workdir" (namestring root)
                                   "change" (change-json root name dir archived-p)))
            ((uiop:file-exists-p (spec-path root name))
             (let ((path (spec-path root name)))
               (nlk:json-object "ok" t "kind" "spec" "spec" name "path" (relative path root)
                                 "parsed" (parsed-json (parse-spec (read-text path)))
                                 "content" (clip (read-text path)))))
            (t (fail "no change or spec named ~s under ~a" name (namestring root))))))))

(defun parsed-json (parsed)
  (nlk:json-object "title" (getf parsed :title)
                    "purpose" (getf parsed :purpose)
                    "requirements"
                    (coerce (mapcar (lambda (requirement)
                                      (nlk:json-object "name" (getf requirement :name)
                                                       "description" (getf requirement :description)
                                                       "scenarios"
                                                       (coerce (mapcar (lambda (scenario)
                                                                         (nlk:json-object "name" (getf scenario :name)
                                                                                          "steps"
                                                                                          (coerce (mapcar (lambda (step)
                                                                                                            (nlk:json-object "type" (getf step :type)
                                                                                                                             "text" (getf step :text)))
                                                                                                          (getf scenario :steps))
                                                                                                  'vector)))
                                                                       (getf requirement :scenarios))
                                                               'vector)))
                                    (getf parsed :requirements))
                            'vector)))

(defun op-status (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (name (specified (arg-text arguments "change")))
         (folders (if name
                      (multiple-value-bind (dir archived-p) (find-change root name)
                        (unless dir (fail "change not found: ~a" name))
                        (list (list name dir archived-p)))
                      (change-folders root))))
    (nlk:json-object "ok" t "workdir" (namestring root)
                      "changes"
                      (coerce (mapcar (lambda (tuple)
                                        (destructuring-bind (folder dir archived) tuple
                                          (let ((artifacts (change-artifacts dir)))
                                            (nlk:json-object
                                             "name" folder
                                             "status" (change-status dir :archived archived)
                                             "archived" archived
                                             "present" (coerce (loop for (key . path) in artifacts
                                                                     when (or (uiop:file-exists-p path)
                                                                              (uiop:directory-exists-p path))
                                                                       collect key)
                                                               'vector)
                                             "missing" (coerce (loop for (key . path) in artifacts
                                                                      unless (or (uiop:file-exists-p path)
                                                                                 (uiop:directory-exists-p path))
                                                                        collect key)
                                                               'vector)))))
                                      folders)
                              'vector))))

;;; --- tasks ----------------------------------------------------------------------------

(defun op-tasks (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (name (specified (arg-text arguments "change"))))
    (unless name (fail "change is required: the change folder's name"))
    (multiple-value-bind (dir archived) (find-change root name)
      (unless dir (fail "change not found: ~a" name))
      (let ((tasks (change-tasks dir))
            (counts (task-counts (change-tasks dir))))
        (nlk:json-object "ok" t "change" name "archived" archived
                          "path" (relative (merge-pathnames "tasks.md" dir) root)
                          "tasks" (tasks-json tasks)
                          "counts" (nlk:json-object "total" (getf counts :total)
                                                     "done" (getf counts :done)
                                                     "todo" (getf counts :todo))
                          "status" (change-status dir :archived archived))))))

(defun op-task-set (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (name (specified (arg-text arguments "change")))
         (asked (arg-list arguments "tasks"))
         (wanted (string-downcase (or (arg arguments "status") "")))
         (mark (cond ((member wanted '("open" "todo" "pending") :test #'string=) #\Space)
                     ((member wanted '("done" "complete" "completed") :test #'string=) #\x)
                     (t (fail "status must be 'todo' or 'done'")))))
    (unless name (fail "change is required"))
    (unless asked (fail "tasks is required: task ids (1.2) or text to match"))
    (multiple-value-bind (dir archived) (find-change root name)
      (unless dir (fail "change not found: ~a" name))
      (when archived (fail "~a is archived: unarchive it first" name))
      (let* ((path (merge-pathnames "tasks.md" dir))
             (text (read-text path))
             (parsed (change-tasks dir))
             (targets '())
             (missing '()))
        (dolist (requested asked)
          (let ((needle (nlk:trimmed requested)))
            (let ((by-id (remove-if-not (lambda (task) (string= (getf task :id) needle)) parsed)))
              (cond (by-id (dolist (task by-id) (pushnew (getf task :line) targets)))
                    (t (let ((by-text (remove-if-not (lambda (task)
                                                        (search (string-downcase needle)
                                                                (string-downcase (getf task :text))))
                                                      parsed)))
                         (if by-text
                             (dolist (task by-text) (pushnew (getf task :line) targets))
                             (push needle missing))))))))
        (when missing (fail "task ids not found: ~{~a~^, ~}" missing))
        (let ((out '()) (updated '()))
          (loop for line in (lines text)
                for number from 1
                for parts = (when (member number targets) (parse-task-line line))
                do (if parts
                       (progn (push (or (getf parts :id) (princ-to-string number)) updated)
                              (push (render-task-line parts mark) out))
                       (push line out)))
          (write-text path (format nil "~{~a~%~}" (nreverse out)))
          (let ((after (change-tasks dir))
                (counts (task-counts (change-tasks dir))))
            (nlk:json-object "ok" t "change" name "set_status" (if (char= mark #\x) "done" "todo")
                              "updated" (coerce (nreverse updated) 'vector)
                              "path" (relative path root)
                              "tasks" (tasks-json after)
                              "counts" (nlk:json-object "total" (getf counts :total)
                                                         "done" (getf counts :done)
                                                         "todo" (getf counts :todo))
                              "status" (change-status dir))))))))

;;; --- writing a change ----------------------------------------------------------------

(defun format-tasks (items)
  (with-output-to-string (out)
    (format out "## 1. Tasks~%~%")
    (loop for text in items for index from 1
          do (format out "- [ ] 1.~a ~a~%" index text))
    (terpri out)))

(defun placeholder-spec (change title &key (source "change"))
  (format nil "# ~a~%~%## ADDED Requirements~%~%### Requirement: ~a scope~%The system SHALL implement the approved behavior described by this ~a after this placeholder is refined into concrete requirements.~%~%#### Scenario: Placeholder refinement~%- **GIVEN** `~a` has been created as an OpenSpec change~%- **WHEN** implementation begins~%- **THEN** this placeholder requirement is replaced or refined with concrete, testable behavior before the change is completed~%"
          title title source change))

(defun write-placeholder-spec (dir change title &key (source "change"))
  (let ((path (merge-pathnames (format nil "specs/~a/spec.md" change) dir)))
    (unless (uiop:file-exists-p path)
      (write-text path (placeholder-spec change title :source source)))
    path))

(defun scaffold-change (root change title summary &key tasks with-spec force)
  (ensure-layout root)
  (let ((dir (change-dir root change)))
    (when (and (uiop:directory-exists-p dir) (not force))
      (fail "change already exists: ~a" change))
    (ensure-directories-exist (merge-pathnames "seen" dir))
    (write-text (merge-pathnames "proposal.md" dir)
                (format nil "## Why~%~%~a~%~%## What Changes~%~%- Create the `~a` OpenSpec change scaffold for review and refinement.~%~%## Capabilities~%~%### New Capabilities~%- `~a`: initial capability placeholder, refined before implementation.~%~%### Modified Capabilities~%- None yet.~%~%## Impact~%~%- TBD: the code, APIs and systems this change touches.~%"
                        summary change change))
    (when tasks
      (write-text (merge-pathnames "tasks.md" dir) (format-tasks tasks)))
    (let ((spec (when (or with-spec tasks) (write-placeholder-spec dir change title))))
      (write-text (merge-pathnames ".openspec.yaml" dir)
                  (format nil "schema: spec-driven~%createdAt: \"~a\"~%" (now-iso)))
      (let ((counts (task-counts (change-tasks dir))))
        (nlk:json-object "ok" t "change" change "workdir" (namestring root)
                          "change_path" (relative dir root)
                          "status" (change-status dir)
                          "paths" (nlk:json-object "proposal" (relative (merge-pathnames "proposal.md" dir) root)
                                                    :when spec "spec" (relative spec root))
                          "counts" (nlk:json-object "total" (getf counts :total)
                                                     "done" (getf counts :done)))))))

(defun op-change-create (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (change (kebab (or (specified (arg-text arguments "change")) "")))
         (title (or (specified (arg-text arguments "title"))
                    (string-capitalize (substitute #\Space #\- change))))
         (summary (or (specified (arg-text arguments "summary"))
                      (format nil "Create change: ~a" title))))
    (scaffold-change root change title summary
                     :tasks (arg-list arguments "tasks")
                     :with-spec (arg-boolean arguments "with_spec")
                     :force (arg-boolean arguments "force"))))

(defun op-change-promote (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (change (kebab (or (specified (arg-text arguments "change")) ""))))
    (multiple-value-bind (dir archived) (find-change root change)
      (unless dir (fail "change not found: ~a" change))
      (when archived (fail "~a is archived: unarchive it first" change))
      (let* ((tasks-path (merge-pathnames "tasks.md" dir))
             (title (change-title dir change)))
        (when (or (not (uiop:file-exists-p tasks-path))
                  (arg-boolean arguments "replace_tasks"))
          (write-text tasks-path (format-tasks (or (arg-list arguments "tasks") +default-tasks+))))
        (let ((spec (write-placeholder-spec dir change title))
              (counts (task-counts (change-tasks dir))))
          (nlk:json-object "ok" t "change" change "workdir" (namestring root)
                            "status" (change-status dir)
                            "paths" (nlk:json-object "tasks" (relative tasks-path root)
                                                      "spec" (relative spec root))
                            "counts" (nlk:json-object "total" (getf counts :total)
                                                       "done" (getf counts :done))))))))

(defun op-change-archive (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (change (kebab (or (specified (arg-text arguments "change")) "")))
         (force (arg-boolean arguments "force"))
         (dir (change-dir root change)))
    (unless (uiop:directory-exists-p dir) (fail "active change not found: ~a" change))
    (let ((status (change-status dir)))
      (when (and (not force) (not (string= status "done")))
        (fail "change is not complete: ~a (~a)" change status)))
    (let ((target (archived-dir root change)))
      (when (and (uiop:directory-exists-p target) (not force))
        (fail "archived change already exists: ~a" change))
      (move-directory dir target)
      (nlk:json-object "ok" t "change" change "status" "archived"
                        "archived_path" (relative target root) "workdir" (namestring root)))))

(defun op-change-unarchive (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (change (kebab (or (specified (arg-text arguments "change")) "")))
         (force (arg-boolean arguments "force"))
         (dir (archived-dir root change))
         (target (change-dir root change)))
    (unless (uiop:directory-exists-p dir) (fail "archived change not found: ~a" change))
    (when (and (uiop:directory-exists-p target) (not force))
      (fail "active change already exists: ~a" change))
    (move-directory dir target)
    (nlk:json-object "ok" t "change" change "status" (change-status target)
                      "change_path" (relative target root) "workdir" (namestring root))))

;;; --- ideas ----------------------------------------------------------------------------

(defun idea-file (root idea)
  (merge-pathnames (format nil "~a.md" (slugify idea "idea")) (ideas-root root)))

(defun unique-idea-file (root slug)
  (let ((candidate (format nil "~a.md" slug)) (index 2) (dir (ideas-root root)))
    (loop while (uiop:file-exists-p (merge-pathnames candidate dir))
          do (setf candidate (format nil "~a-~a.md" slug index))
             (incf index))
    (values (pathname-name (pathname candidate)) (merge-pathnames candidate dir))))

(defun markdown-list (items)
  (if items (format nil "~{- ~a~%~}" items) "- None~%"))

(defun op-idea-create (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (title (specified (arg-text arguments "title")))
         (prompt (specified (arg-text arguments "prompt"))))
    (unless title (fail "title is required"))
    (unless prompt (fail "prompt is required"))
    (ensure-layout root)
    (multiple-value-bind (slug path) (unique-idea-file
                                       root
                                       (slugify (if (specified (arg-text arguments "slug"))
                                                    (arg-text arguments "slug")
                                                    title)))
      (let ((origin (or (specified (arg-text arguments "origin")) "unspecified"))
            (tags (arg-list arguments "tags"))
            (notes (arg-text arguments "notes"))
            (created (now-iso)))
        (write-text path
                    (with-output-to-string (out)
                      (format out "# ~a~%~%## Source~%- Origin: ~a~%- Created: ~a~%- Tags: ~a~%~%## Prompt~%~a~%"
                              title origin created (if tags (format nil "~{~a~^, ~}" tags) "None") prompt)
                      (when (plusp (length notes))
                        (format out "~%## Notes~%~a~%" notes))))
        (nlk:json-object "ok" t "slug" slug "title" title "workdir" (namestring root)
                          "path" (relative path root)
                          "metadata" (nlk:json-object "origin" origin "created" created
                                                       :when tags "tags" (coerce tags 'vector)))))))

(defun op-idea-enrich (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (idea (specified (arg-text arguments "idea")))
         (feasibility (arg-text arguments "feasibility"))
         (size (arg-text arguments "tshirt_size")))
    (unless idea (fail "idea is required"))
    (let ((path (idea-file root idea)))
      (unless (uiop:file-exists-p path) (fail "idea not found: ~a" (namestring (relative path root))))
      (when (plusp (length feasibility))
        (unless (member (string-downcase feasibility) +feasibilities+ :test #'string=)
          (fail "feasibility must be one of: Low, Medium, High")))
      (when (plusp (length size))
        (unless (member (string-downcase size) +tshirt-sizes+ :test #'string=)
          (fail "tshirt_size must be one of: XS, S, M, L, XL")))
      (let* ((generated (now-iso))
             (report (with-output-to-string (out)
                      (format out "~a~%## Enrichment Report~%~%Generated: ~a~%~%~%### Problem~%~a~%~%### Proposed Direction~%~a~%~%### Key Questions~%~a### Feasibility~%Feasibility: ~a~%~%### T-Shirt Size~%T-Shirt Size: ~a~%~%### Size Justification~%~a~%~%### Risks~%~a### Suggested Next Step~%~a~%~a~%"
                              +enrichment-start+ generated
                              (or (specified (arg-text arguments "problem")) "TBD")
                              (or (specified (arg-text arguments "proposed_direction")) "TBD")
                              (markdown-list (arg-list arguments "key_questions"))
                              (or (specified feasibility) "TBD")
                              (or (specified size) "TBD")
                              (or (specified (arg-text arguments "size_justification")) "TBD")
                              (markdown-list (arg-list arguments "risks"))
                              (or (specified (arg-text arguments "suggested_next_step")) "TBD")
                              +enrichment-end+))))
        (let ((content (read-text path)))
          (multiple-value-bind (stripped found) (between content +enrichment-start+ +enrichment-end+)
            (write-text path
                        (if found
                            (format nil "~a~%~a~%" (string-right-trim '(#\Space #\Newline #\Tab) stripped) report)
                            (format nil "~a~%~a" (string-right-trim '(#\Space #\Newline #\Tab) content) report)))))
        (nlk:json-object "ok" t "idea" (slugify idea "idea") "path" (relative path root)
                          "workdir" (namestring root)
                          "metadata" (nlk:json-object "generated" generated
                                                       :when (plusp (length feasibility)) "feasibility" feasibility
                                                       :when (plusp (length size)) "tshirt_size" size))))))

(defun op-idea-promote (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (idea (specified (arg-text arguments "idea")))
         (change (kebab (or (specified (arg-text arguments "change")) "")))
         (force (arg-boolean arguments "force")))
    (unless idea (fail "idea is required"))
    (let ((path (idea-file root idea)))
      (unless (uiop:file-exists-p path) (fail "idea not found: ~a" (namestring (relative path root))))
      (ensure-layout root)
      (let ((dir (change-dir root change)))
        (when (and (uiop:directory-exists-p dir) (not force))
          (fail "change already exists: ~a" change))
        (ensure-directories-exist (merge-pathnames "seen" dir))
        (let* ((content (read-text path))
               (title (or (loop for line in (lines content)
                                when (uiop:string-prefix-p "# " line) return (string-trim " " (subseq line 2)))
                          (slugify idea "idea")))
               (summary (or (specified (arg-text arguments "summary"))
                            (format nil "Promote idea: ~a" title)))
               (rel (relative path root))
               (created (now-iso))
               (spec-dir (merge-pathnames (format nil "specs/~a/" change) dir))
               (spec (merge-pathnames "spec.md" spec-dir)))
          (write-text (merge-pathnames "proposal.md" dir)
                      (format nil "## Why~%~%~a~%~%## What Changes~%~%- Promote the reviewed idea `~a` into an implementation-ready OpenSpec change.~%- Keep traceability to the source idea while proposal, specs and tasks are refined.~%~%## Source Idea~%~%- Path: `~a`~%- Promoted: ~a~%~%### Idea Content~%~%```md~%~a~%```~%~%## Capabilities~%~%### New Capabilities~%- `~a`: initial capability placeholder from the promoted idea.~%~%### Modified Capabilities~%- None yet.~%~%## Impact~%- TBD: the code, APIs and systems this change touches.~%"
                              summary (pathname-name path) rel created
                              (string-right-trim '(#\Newline) content) change))
          (write-text (merge-pathnames "tasks.md" dir)
                      (format nil "## 1. Proposal Refinement~%~%- [ ] 1.1 Review the source idea and clarify scope.~%- [ ] 1.2 Identify affected capabilities and write spec deltas.~%- [ ] 1.3 Add design notes when trade-offs are non-trivial.~%~%## 2. Implementation~%~%- [ ] 2.1 Implement the approved behavior.~%- [ ] 2.2 Add or update tests.~%- [ ] 2.3 Validate the artifacts and the test suite.~%"))
          (write-text spec (placeholder-spec change title :source "promoted idea"))
          (write-text (merge-pathnames ".openspec.yaml" dir)
                      (format nil "schema: spec-driven~%sourceIdea: \"~a\"~%promotedAt: \"~a\"~%" rel created))
          (nlk:json-object "ok" t "idea" (pathname-name path) "change" change
                            "workdir" (namestring root) "change_path" (relative dir root)
                            "paths" (nlk:json-object "proposal" (relative (merge-pathnames "proposal.md" dir) root)
                                                      "tasks" (relative (merge-pathnames "tasks.md" dir) root)
                                                      "spec" (relative spec root))))))))

;;; --- specs ----------------------------------------------------------------------------

(defun op-spec-list (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (change (specified (arg-text arguments "change")))
         (names (spec-names root change)))
    (nlk:json-object "ok" t "change" change "count" (length names)
                      "specs" (coerce names 'vector) "workdir" (namestring root))))

(defun op-spec-show (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (spec (specified (arg-text arguments "spec")))
         (change (specified (arg-text arguments "change"))))
    (unless spec (fail "spec is required, e.g. 'agent-tools'"))
    (let ((path (spec-path root spec change)))
      (unless (uiop:file-exists-p path) (fail "spec not found: ~a" (relative path root)))
      (let ((parsed (parse-spec (read-text path))))
        (nlk:json-object "ok" t "spec" spec "change" change "workdir" (namestring root)
                          "path" (relative path root)
                          "title" (getf parsed :title)
                          "purpose" (getf parsed :purpose)
                          "requirements" (gethash "requirements" (parsed-json parsed)))))))

(defun spec-markdown (title purpose requirements)
  (with-output-to-string (out)
    (format out "# ~a~%~%## Purpose~%~%~a~%~%## Requirements~%" title purpose)
    (dolist (requirement requirements)
      (format out "~%### Requirement: ~a~%~%~a~%" (getf requirement :name) (getf requirement :description))
      (dolist (scenario (getf requirement :scenarios))
        (format out "~%#### Scenario: ~a~%" (getf scenario :name))
        (dolist (step (getf scenario :steps))
          (format out "- **~a** ~a~%" (getf step :type) (getf step :text)))))))

(defun read-requirements (arguments)
  "The call's requirements, as plists."
  (let ((asked (nlk:json-array arguments "requirements")))
    (unless (and asked (plusp (length asked)))
      (fail "requirements must be a non-empty array of {name, description, scenarios}"))
    (map 'list
         (lambda (requirement)
           (let ((name (specified (or (nlk:json-value requirement :string "name") ""))))
             (unless name (fail "every requirement needs a name"))
             (list :name name
                   :description (or (nlk:json-value requirement :string "description") "")
                   :scenarios (map 'list
                                   (lambda (scenario)
                                     (let ((scenario-name (specified (or (nlk:json-value scenario :string "name") ""))))
                                       (unless scenario-name (fail "every scenario needs a name"))
                                       (list :name scenario-name
                                             :steps (map 'list
                                                         (lambda (step)
                                                           (list :type (string-upcase (or (nlk:json-value step :string "type") "GIVEN"))
                                                                 :text (or (nlk:json-value step :string "text") "")))
                                                         (or (nlk:json-array scenario "steps") #())))))
                                   (or (nlk:json-array requirement "scenarios") #())))))
         asked)))

(defun op-spec-create (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (spec (specified (arg-text arguments "spec")))
         (title (specified (arg-text arguments "title")))
         (purpose (specified (arg-text arguments "purpose")))
         (change (specified (arg-text arguments "change"))))
    (unless spec (fail "spec is required, e.g. 'agent-tools'"))
    (unless (kebab-case-p spec) (fail "spec must be kebab-case (letters, numbers, hyphens): ~a" spec))
    (unless change (ensure-layout root))
    (unless title (fail "title is required"))
    (unless purpose (fail "purpose is required"))
    (let* ((requirements (read-requirements arguments))
           (path (spec-path root spec change)))
      (when (and (uiop:file-exists-p path) (not (arg-boolean arguments "force")))
        (fail "spec already exists at ~a: pass force to overwrite" (relative path root)))
      (write-text path (spec-markdown title purpose requirements))
      (nlk:json-object "ok" t "spec" spec "change" change "path" (relative path root)
                        "workdir" (namestring root)))))

;;; --- the semantic spec diff -------------------------------------------------------------

(defun same-steps-p (left right)
  (and (= (length left) (length right))
       (every (lambda (one other)
                (and (equal (getf one :type) (getf other :type))
                     (equal (getf one :text) (getf other :text))))
              left right)))

(defun same-scenarios-p (left right)
  (and (= (length left) (length right))
       (every (lambda (one other)
                (and (equal (getf one :name) (getf other :name))
                     (same-steps-p (getf one :steps) (getf other :steps))))
              left right)))

(defun same-requirement-p (left right)
  (and (equal (getf left :description) (getf right :description))
       (same-scenarios-p (getf left :scenarios) (getf right :scenarios))))

(defun scenario-names (scenarios)
  (mapcar (lambda (scenario) (getf scenario :name)) scenarios))

(defun semantic-spec-diff (before-text after-text)
  "BEFORE-TEXT and AFTER-TEXT compared at the requirement and scenario level,
the way the original's parser did: by name, then by text."
  (let* ((before (parse-spec before-text))
         (after (parse-spec after-text))
         (before-requirements (getf before :requirements))
         (after-requirements (getf after :requirements))
         (added '()) (modified '()) (removed '()) (unchanged '()))
    (dolist (requirement after-requirements)
      (let ((was (find (getf requirement :name) before-requirements :key (lambda (r) (getf r :name))
                                                                  :test #'equal)))
        (cond ((null was) (push requirement added))
              ((same-requirement-p was requirement) (push (getf requirement :name) unchanged))
              (t (push (list :name (getf requirement :name)
                             :before was :after requirement
                             :scenarios_added (set-difference (scenario-names (getf requirement :scenarios))
                                                              (scenario-names (getf was :scenarios))
                                                              :test #'equal)
                             :scenarios_removed (set-difference (scenario-names (getf was :scenarios))
                                                                (scenario-names (getf requirement :scenarios))
                                                                :test #'equal))
                       modified)))))
    (dolist (requirement before-requirements)
      (unless (find (getf requirement :name) after-requirements :key (lambda (r) (getf r :name))
                                                           :test #'equal)
        (push requirement removed)))
    (list :status (cond ((and (null added) (null modified) (null removed)) "unchanged")
                        ((and (null before-requirements) after-requirements) "added")
                        ((and before-requirements (null after-requirements)) "deleted")
                        (t "modified"))
          :requirements (list :added (nreverse added) :modified (nreverse modified)
                              :removed (nreverse removed) :unchanged (nreverse unchanged)))))

(defun requirement-brief (requirement)
  "A requirement (a plist) as the wire object a diff carries."
  (nlk:json-object "name" (getf requirement :name)
                    "description" (getf requirement :description)
                    "scenarios" (coerce (mapcar (lambda (scenario)
                                                   (nlk:json-object "name" (getf scenario :name)))
                                                 (getf requirement :scenarios))
                                         'vector)))

(defun op-spec-diff (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (spec (specified (arg-text arguments "spec")))
         (change (specified (arg-text arguments "change"))))
    (unless spec (fail "spec is required, e.g. 'agent-tools'"))
    (let ((before-path (spec-path root spec nil)))
      (unless change
        (setf change (loop for (folder dir nil) in (change-folders root)
                           when (uiop:file-exists-p (spec-path root spec folder))
                             return folder)))
      (unless change
        (fail "no change carries a spec named ~s: pass change" spec))
      (let ((after-path (spec-path root spec change)))
        (unless (uiop:file-exists-p after-path) (fail "spec not found: ~a" (relative after-path root)))
        (let ((delta (semantic-spec-diff (read-text before-path) (read-text after-path)))
              (before-exists (uiop:file-exists-p before-path)))
          (nlk:json-object "ok" t "spec" spec "change" change
                            "baseline" (when before-exists (relative before-path root))
                            "change_spec" (relative after-path root)
                            "status" (getf delta :status)
                            "requirements"
                            (destructuring-bind (&key added modified removed unchanged)
                                (getf delta :requirements)
                              (nlk:json-object
                               "added" (coerce (mapcar #'requirement-brief added) 'vector)
                               "removed" (coerce (mapcar #'requirement-brief removed) 'vector)
                               "unchanged" (coerce unchanged 'vector)
                               "modified" (coerce (mapcar (lambda (entry)
                                                             (nlk:json-object
                                                              "name" (getf entry :name)
                                                              "before" (requirement-brief (getf entry :before))
                                                              "after" (requirement-brief (getf entry :after))
                                                              "scenarios_added" (coerce (getf entry :scenarios_added) 'vector)
                                                              "scenarios_removed" (coerce (getf entry :scenarios_removed) 'vector)))
                                                           modified)
                                                   'vector)))))))))

;;; --- validation -----------------------------------------------------------------------

(defun problem (code text &optional path)
  "One validation problem: its code, its file and what to fix."
  (nlk:json-object "code" code "message" text "path" (or path "")))

(defun validate-spec-file (path relative-path)
  "PROBLEM list for one spec.md."
  (let* ((parsed (parse-spec (read-text path)))
         (problems '()))
    (when (zerop (length (getf parsed :title)))
      (push (problem "spec_no_title" "the spec has no `# Title' heading" relative-path) problems))
    (when (and (zerop (length (getf parsed :purpose)))
               (null (getf parsed :requirements)))
      (push (problem "spec_no_purpose" "the spec has no `## Purpose' text" relative-path) problems))
    (when (null (getf parsed :requirements))
      (push (problem "spec_no_requirements" "no `### Requirement:' line" relative-path) problems))
    (dolist (requirement (getf parsed :requirements))
      (when (null (getf requirement :scenarios))
        (push (problem "requirement_no_scenario"
                       (format nil "requirement ~s has no `#### Scenario:'" (getf requirement :name))
                       relative-path)
              problems))
      (dolist (scenario (getf requirement :scenarios))
        (when (null (getf scenario :steps))
          (push (problem "scenario_no_steps"
                         (format nil "scenario ~s has no GIVEN/WHEN/THEN step" (getf scenario :name))
                         relative-path)
                problems))))
    (reverse problems)))

(defun validate-change-folder (root name dir archived)
  "(:errors ... :warnings ...) for one change folder."
  (let ((errors '()) (warnings '()))
    (let ((proposal (merge-pathnames "proposal.md" dir))
          (tasks (merge-pathnames "tasks.md" dir)))
      (unless (uiop:file-exists-p proposal)
        (push (problem "missing_proposal" "proposal.md is missing"
                       (relative proposal root)) errors))
      (when (and (uiop:file-exists-p proposal)
                 (not (search "## Why" (read-text proposal))))
        (push (problem "proposal_no_why" "proposal.md has no `## Why' section"
                       (relative proposal root)) errors))
      (cond ((not (uiop:file-exists-p tasks))
             (push (problem "missing_tasks" "tasks.md is missing" (relative tasks root)) errors))
            ((null (change-tasks dir))
             (push (problem "no_tasks" "tasks.md carries no `- [ ]' item" (relative tasks root)) errors))
            (t
             (dolist (task (change-tasks dir))
               (when (zerop (length (getf task :text)))
                 (push (problem "task_no_text"
                                (format nil "task ~a has no text" (getf task :id))
                                (relative tasks root))
                       warnings)))))
      (let ((specs (recursive-markdown-files (merge-pathnames "specs/" dir))))
        (when (null specs)
          (push (problem "missing_specs" "specs/ carries no spec.md"
                         (relative (merge-pathnames "specs/" dir) root)) errors))
        (dolist (path specs)
          (setf errors (append errors (validate-spec-file path (relative path dir)))))
        ))
    (list :name name :archived archived
          :errors (reverse errors) :warnings (reverse warnings)
          :status (change-status dir :archived archived))))

(defun op-validate (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (name (specified (arg-text arguments "change")))
         (folders (if name
                      (multiple-value-bind (dir archived) (find-change root name)
                        (unless dir (fail "change not found: ~a" name))
                        (list (list name dir archived)))
                      (remove-if #'third (change-folders root)))))
    (let* ((reports (mapcar (lambda (tuple) (apply #'validate-change-folder root tuple)) folders))
           (failed (remove-if-not (lambda (report) (plusp (length (getf report :errors)))) reports)))
      (nlk:json-object "ok" (null failed) "workdir" (namestring root)
                        "checked" (length reports) "failed" (length failed)
                        "changes"
                        (coerce (mapcar (lambda (report)
                                          (nlk:json-object
                                           "name" (getf report :name)
                                           "status" (getf report :status)
                                           "valid" (zerop (length (getf report :errors)))
                                           "errors" (coerce (getf report :errors) 'vector)
                                           "warnings" (coerce (getf report :warnings) 'vector)))
                                        reports)
                                'vector)))))

;;; --- the authoring guides -------------------------------------------------------------

(defparameter +guides+
  '(("proposal"
     "proposal.md: `## Why' (the problem, in two sentences), `## What Changes' (a short bullet list of the user-visible change), `## Capabilities' (`### New Capabilities' and `### Modified Capabilities', naming each capability), `## Impact' (the code, APIs, dependencies and systems it touches). Write it with the file tools, then openspec_validate.")
    ("design"
     "design.md: the trade-offs a reader would otherwise ask about — the options considered, why this one, what it costs, what it rules out, and how it is tested. Write it only when the change is not obvious from the proposal and the specs.")
    ("tasks"
     "tasks.md: `## 1. <phase>' headings, then one `- [ ] 1.1 <verb phrase>' per unit of work, numbered, small enough to be done and checked in one step. Mark them with openspec_task_set_status; a change is `done' when every box is checked.")
    ("specs"
     "specs/<capability>/spec.md: `# <Title>', `## Purpose' (why the capability exists), `## Requirements', then `### Requirement: <name>' with one SHALL sentence, and one `#### Scenario: <name>' per concrete case with `- **GIVEN** ...', `- **WHEN** ...', `- **THEN** ...' steps. In a change, use `## ADDED Requirements' or `## MODIFIED Requirements'.")
    ("apply"
     "Apply a change: openspec_show it to read the proposal, design and delta specs; openspec_task_list to see what is left; implement the requirements and their scenarios in the code; openspec_task_set_status each task as it is done; openspec_validate until it is clean; then openspec_change_archive.")))

(defun op-instructions (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (artifact (string-downcase (or (arg arguments "artifact") "proposal")))
         (change (specified (arg-text arguments "change")))
         (guide (cdr (assoc artifact +guides+ :test #'string=))))
    (unless guide (fail "artifact must be one of: ~{~a~^, ~}" (mapcar #'car +guides+)))
    (nlk:json-object "ok" t "artifact" artifact "workdir" (namestring root)
                      :when change "change" change
                      "guide" guide
                      :when (and change (member artifact '("proposal" "design" "tasks" "specs") :test #'string=))
                      "target" (format nil "~a/~a~a"
                                        (relative (changes-root root) root)
                                        change
                                        (cond ((string= artifact "proposal") "/proposal.md")
                                              ((string= artifact "design") "/design.md")
                                              ((string= artifact "tasks") "/tasks.md")
                                              (t "/specs/<capability>/spec.md"))))))

;;; --- identifiers ----------------------------------------------------------------------

(defun op-context (arguments)
  (let* ((identifier (arg-text arguments "identifier"))
         (slash (position #\/ identifier))
         (source-name (if slash (subseq identifier 0 slash) identifier))
         (reference (if slash (subseq identifier (1+ slash)) nil)))
    (unless (plusp (length identifier))
      (fail "identifier is required, e.g. 'myproject' or 'myproject/add-login'"))
    (let* ((source (source-named source-name))
           (root (second source))
           (name (first source)))
      (unless (uiop:directory-exists-p (openspec-root root))
        (fail "no openspec/ folder in ~a" (namestring root)))
      (if (plusp (length (or reference "")))
          (multiple-value-bind (dir archived) (find-change root reference)
            (cond
              (dir (nlk:json-object "ok" t "name" name "path" (namestring root)
                                     "workdir" (namestring root)
                                     "change" (change-json root reference dir archived)))
              ((uiop:file-exists-p (spec-path root reference))
               (nlk:json-object "ok" t "name" name "path" (namestring root) "workdir" (namestring root)
                                 "spec" (nlk:json-object "path" (format nil "~a/spec.md" reference)
                                                          "content" (clip (read-text (spec-path root reference))))))
              ((uiop:file-exists-p (idea-file root reference))
               (nlk:json-object "ok" t "name" name "path" (namestring root) "workdir" (namestring root)
                                 "idea" (nlk:json-object "name" (pathname-name (idea-file root reference))
                                                          "content" (clip (read-text (idea-file root reference))))))
              (t (fail "no change, spec or idea named ~s under ~a" reference (namestring root)))))
          (nlk:json-object
           "ok" t "name" name "path" (namestring root) "workdir" (namestring root)
           "hint" (format nil "call openspec_context with '~a/<name>' to load one change, spec or idea, or pass workdir to the other tools" name)
           "changes" (coerce (mapcar (lambda (tuple) (apply #'change-entry root tuple))
                                      (remove-if #'third (change-folders root)))
                              'vector)
           "specs" (coerce (mapcar (lambda (spec) (nlk:json-object "name" spec "token" spec))
                                    (spec-names root))
                           'vector)
           "ideas" (coerce (mapcar (lambda (path)
                                      (nlk:json-object "name" (pathname-name path)
                                                       "filename" (file-namestring path)))
                                    (markdown-files (ideas-root root)))
                           'vector))))))

;;; --- the CLI, when the caller wants it ------------------------------------------------

(defun default-run-command (argv &key directory)
  "ARGV run in DIRECTORY: (VALUES STDOUT STDERR STATUS)."
  (let ((out (make-string-output-stream)) (err (make-string-output-stream)))
    (let ((status (uiop:run-program argv :output out :error-output err
                                          :directory directory :ignore-error-status t)))
      (list (get-output-stream-string out) (get-output-stream-string err) status))))

(defvar *run-command* #'default-run-command
  "How openspec_cli runs the CLI: (FUNCTION (ARGV &KEY DIRECTORY) => (VALUES OUT ERR STATUS)).")

(defun openspec-binary ()
  (or (specified (ignore-errors (setting :cli)))
      (specified (uiop:getenv "OPENSPEC_BIN"))
      "openspec"))

(defun op-cli (arguments)
  (let* ((root (project-root (specified (arg arguments "workdir"))))
         (command (arg-text arguments "command"))
         ;; an absent flag means yes; a false one means no, and JSON null
         ;; reads as absent, so presence is asked of the table directly
         (json (multiple-value-bind (value present) (gethash "json_output" arguments)
                 (if present (and value t) t))))
    (unless (plusp (length command)) (fail "command is required, e.g. 'status --change add-login'"))
    (let* ((argv (append (list (openspec-binary))
                          (uiop:split-string command :separator '(#\Space))
                          (if json (list "--json") '())))
           (result (handler-case (funcall *run-command* argv :directory root)
                     (error (condition)
                       (fail "could not run ~a: ~a" (openspec-binary)
                             (nlk:one-line (princ-to-string condition)))))))
      (destructuring-bind (out err status) result
        (nlk:json-object "ok" (and (integerp status) (zerop status))
                          "exit_code" status
                          "command" (coerce argv 'vector)
                          "stdout" (clip (or out ""))
                          "stderr" (nlk:trimmed (or err ""))
                          "workdir" (namestring root))))))

;;; --- the tool schemas ----------------------------------------------------------------

(defun property (type description &key enum)
  "One JSON-Schema property."
  (nlk:json-object "type" type "description" description
                    :when enum "enum" (coerce enum 'vector)))

(defun schema (description properties &optional required)
  "A tool schema: its DESCRIPTION, its ((NAME . PROPERTY) ...) and the
members a call must carry."
  (list :description description
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (apply #'nlk:make-json-object
                                         (loop for (name . value) in properties
                                               append (list name value)))
                     :when required "required" (coerce required 'vector))))

(defparameter +workdir-property+
  (cons "workdir" (property "string" "project folder holding the openspec/ folder; defaults to the openspec section's workdir")))

(defparameter +change-property+
  (cons "change" (property "string" "the change folder's kebab-case name, e.g. 'add-login'")))

(defparameter +schema-list+
  (schema "List the OpenSpec changes or specs of a project, with each change's derived status and task progress. Call it before choosing a change to show, validate or work on."
          (list +workdir-property+
                (cons "kind" (property "string" "changes (default) or specs" :enum '("changes" "specs")))
                (cons "sort" (property "string" "recent (default) or name" :enum '("recent" "name"))))))

(defparameter +schema-show+
  (schema "Read one change or spec whole, as JSON: a change's proposal, design, tasks.md, its parsed tasks and delta specs, with its status; or a spec's parsed title, purpose, requirements and scenarios."
          (list +workdir-property+
                (cons "name" (property "string" "a change folder's name, or a spec's"))
                (cons "type" (property "string" "change or spec; guessed when left out" :enum '("change" "spec")))
                (cons "change" (property "string" "the change whose delta carries the spec, when reading one")))))

(defparameter +schema-status+
  (schema "Which artifacts a change carries and which are missing, and its derived status: draft (no tasks), todo, in_progress, done, archived. Omit change for every change in the project."
          (list +workdir-property+ +change-property+)))

(defparameter +schema-instructions+
  (schema "The authoring guide for one artifact: how to write the proposal, the design, the tasks, the specs, and how to apply a change. It answers the guide, not the file: write the file with the file tools afterwards."
          (list +workdir-property+
                (cons "artifact" (property "string" "which guide" :enum '("proposal" "design" "tasks" "specs" "apply")))
                +change-property+)
          '("artifact")))

(defparameter +schema-task-list+
  (schema "A change's tasks with their ids, text, status and completion counts."
          (list +workdir-property+ +change-property+)
          '("change")))

(defparameter +schema-task-set+
  (schema "Tick or untick a change's tasks in tasks.md. Name them by their id ('1.2') or by text they contain; every match is set."
          (list +workdir-property+ +change-property+
                (cons "tasks" (property "array" "task ids ('1.2') or text fragments to match"))
                (cons "status" (property "string" "todo or done" :enum '("todo" "done"))))
          '("tasks" "status")))

(defparameter +schema-change-create+
  (schema "Create a change's scaffold: proposal.md, tasks.md when tasks are given, a placeholder delta spec and .openspec.yaml. It refuses a change that already exists unless force is set."
          (list +workdir-property+
                (cons "change" (property "string" "the kebab-case change id, e.g. 'add-login'"))
                (cons "title" (property "string" "its title; defaults to the id in words"))
                (cons "summary" (property "string" "one sentence for the proposal's `## Why'"))
                (cons "tasks" (property "array" "the first checklist items"))
                (cons "with_spec" (property "boolean" "write a placeholder delta spec"))
                (cons "force" (property "boolean" "replace a change that already exists")))
          '("change")))

(defparameter +schema-change-promote+
  (schema "Make a draft change ready to implement: give it a task list when it has none, and a placeholder delta spec when its specs/ folder is empty."
          (list +workdir-property+ +change-property+
                (cons "tasks" (property "array" "the checklist to write when tasks.md is missing"))
                (cons "replace_tasks" (property "boolean" "overwrite an existing tasks.md")))
          '("change")))

(defparameter +schema-change-archive+
  (schema "Move a finished change from changes/ into changes/archive/. It refuses a change whose boxes are not all ticked unless force is set."
          (list +workdir-property+ +change-property+
                (cons "force" (property "boolean" "archive a change that is not done")))
          '("change")))

(defparameter +schema-change-unarchive+
  (schema "Move an archived change back into changes/, so its tasks can be worked on again."
          (list +workdir-property+ +change-property+
                (cons "force" (property "boolean" "replace an active change of the same name")))
          '("change")))

(defparameter +schema-idea-create+
  (schema "Capture a raw idea as openspec/ideas/<slug>.md: its origin, tags and the prompt that started it."
          (list +workdir-property+
                (cons "title" (property "string" "the idea in a line"))
                (cons "prompt" (property "string" "the prompt or note the idea came from"))
                (cons "origin" (property "string" "where it came from: a conversation, an issue, a review"))
                (cons "tags" (property "array" "a few words to find it by"))
                (cons "notes" (property "string" "anything else worth keeping"))
                (cons "slug" (property "string" "the file name; defaults to the title")))
          '("title" "prompt")))

(defparameter +schema-idea-enrich+
  (schema "Add or replace an idea's structured evaluation: problem, proposed direction, key questions, feasibility, T-shirt size, risks and the next step."
          (list +workdir-property+
                (cons "idea" (property "string" "the idea file's name, without .md"))
                (cons "problem" (property "string" "what hurts"))
                (cons "proposed_direction" (property "string" "the shape of the answer"))
                (cons "key_questions" (property "array" "what must be answered first"))
                (cons "feasibility" (property "string" "low, medium or high" :enum '("low" "medium" "high")))
                (cons "tshirt_size" (property "string" "xs, s, m, l or xl" :enum '("xs" "s" "m" "l" "xl")))
                (cons "size_justification" (property "string" "why that size"))
                (cons "risks" (property "array" "what could go wrong"))
                (cons "suggested_next_step" (property "string" "the first thing to do")))
          '("idea")))

(defparameter +schema-idea-promote+
  (schema "Turn a reviewed idea into a change: a proposal carrying the idea whole, a two-phase task list, a placeholder delta spec and .openspec.yaml with the idea's path."
          (list +workdir-property+
                (cons "idea" (property "string" "the idea file's name, without .md"))
                (cons "change" (property "string" "the kebab-case change id to create"))
                (cons "summary" (property "string" "one sentence for the proposal's `## Why'"))
                (cons "force" (property "boolean" "replace a change that already exists")))
          '("idea" "change")))

(defparameter +schema-spec-list+
  (schema "The spec names under a change's specs/ folder, or the project's baseline openspec/specs/ when no change is named."
          (list +workdir-property+ +change-property+)))

(defparameter +schema-spec-show+
  (schema "One spec parsed into JSON: its title, its purpose, and each requirement with its description, scenarios and GIVEN/WHEN/THEN steps."
          (list +workdir-property+
                (cons "spec" (property "string" "the spec's kebab-case name, e.g. 'agent-tools'"))
                +change-property+)
          '("spec")))

(defparameter +schema-spec-create+
  (schema "Write a spec.md from structured input: title, purpose, and requirements with their scenarios and steps. Omit change for the baseline, pass it for a change's delta; refuses an existing spec unless force is set."
          (list +workdir-property+
                (cons "spec" (property "string" "the kebab-case spec name"))
                (cons "title" (property "string" "the spec's title"))
                (cons "purpose" (property "string" "why the capability exists"))
                (cons "requirements" (property "array" "each {name, description, scenarios: [{name, steps: [{type, text}]}]}"))
                +change-property+
                (cons "force" (property "boolean" "overwrite a spec that exists")))
          '("spec" "title" "purpose" "requirements")))

(defparameter +schema-spec-diff+
  (schema "Compare a change's delta spec against the baseline at the requirement and scenario level: what it adds, modifies and removes. Read-only."
          (list +workdir-property+
                (cons "spec" (property "string" "the spec's name"))
                (cons "change" (property "string" "the change to compare; found by name when left out")))
          '("spec")))

(defparameter +schema-validate+
  (schema "Check changes against OpenSpec's shape: a proposal with `## Why', a task list with items, a delta spec per change, and each requirement with a scenario whose steps are written. Answers every problem with its file. Omit change to check all active changes."
          (list +workdir-property+ +change-property+)))

(defparameter +schema-context+
  (schema "Resolve an identifier into working context. 'myproject' answers the project's path, its active changes, its specs and its ideas; 'myproject/add-login' answers that change whole, or the spec or idea of that name. Use the project names in the openspec section's projects list, or its workdir."
          (list (cons "identifier" (property "string" "a project's name, or name/change, name/spec, name/idea")))
          '("identifier")))

(defparameter +schema-cli+
  (schema "Run the openspec command line directly and answer its output, for the subcommands the files do not carry. It needs the CLI installed, or a path in the section's cli."
          (list +workdir-property+
                (cons "command" (property "string" "the subcommand, e.g. 'status --change add-login'"))
                (cons "json_output" (property "boolean" "append --json; defaults to true")))
          '("command")))

;;; --- the tools ------------------------------------------------------------------------

(defmacro defop (name operation)
  "TOOL_NAME's body: OPERATION's JSON, its refusals answered as JSON."
  `(defun ,name (arguments)
     (answering (answer (,operation arguments)))))

(defop tool-context op-context)
(defop tool-list op-list)
(defop tool-show op-show)
(defop tool-status op-status)
(defop tool-instructions op-instructions)
(defop tool-task-list op-tasks)
(defop tool-task-set op-task-set)
(defop tool-change-create op-change-create)
(defop tool-change-promote op-change-promote)
(defop tool-change-archive op-change-archive)
(defop tool-change-unarchive op-change-unarchive)
(defop tool-idea-create op-idea-create)
(defop tool-idea-enrich op-idea-enrich)
(defop tool-idea-promote op-idea-promote)
(defop tool-spec-list op-spec-list)
(defop tool-spec-show op-spec-show)
(defop tool-spec-create op-spec-create)
(defop tool-spec-diff op-spec-diff)
(defop tool-validate op-validate)
(defop tool-cli op-cli)

;;; --- /openspec ------------------------------------------------------------------------

(defun status-line (name status counts)
  (format nil "~a  ~a  ~a/~a tasks"
          name status (getf counts :done) (getf counts :total)))

(defun run-slash (arguments session-id)
  "The project's board: every active change with its status, then what to call."
  (declare (ignore arguments session-id))
  (handler-case
      (let* ((root (project-root nil))
             (folders (remove-if #'third (change-folders root))))
        (if (null folders)
            (format nil "OpenSpec: ~a has no active change. openspec_change_create or /openspec ideas~%"
                    (namestring root))
            (format nil "OpenSpec: ~a~%~{~a~%~}~%openspec_show <change>, openspec_task_list <change>, openspec_validate"
                    (namestring root)
                    (mapcar (lambda (tuple)
                              (destructuring-bind (name dir archived) tuple
                                (declare (ignore archived))
                                (status-line name (change-status dir)
                                             (task-counts (change-tasks dir)))))
                            folders))))
    (openspec-error (condition)
      (format nil "OpenSpec: ~a" (openspec-error-detail condition)))
    (error (condition)
      (format nil "OpenSpec: ~a" (nlk:one-line (princ-to-string condition))))))

;;; --- the cell -------------------------------------------------------------------------

(nle:define-cell openspec
  (:section ("openspec")
    (:guide "workdir is the project folder holding openspec/; projects names more as name=path; the rest is optional")
    (:any-of "workdir" "projects")
    ("workdir" :path
               :doc "the project whose openspec/ folder the tools read and write; every tool also takes a workdir of its own")
    ("projects" :list
                :doc "more projects to resolve by name, each written 'name=path', for openspec_context")
    ("cli" :string
           :doc "the openspec command, when it is not on PATH; openspec_cli is the only tool that runs it")
    ("max_chars" :integer :default 50000 :min 1000
                 :doc "the longest text one answer carries, before it is cut"))
  (:tool "openspec_context" +schema-context+ 'tool-context)
  (:tool "openspec_list" +schema-list+ 'tool-list)
  (:tool "openspec_show" +schema-show+ 'tool-show)
  (:tool "openspec_status" +schema-status+ 'tool-status)
  (:tool "openspec_instructions" +schema-instructions+ 'tool-instructions)
  (:tool "openspec_task_list" +schema-task-list+ 'tool-task-list)
  (:tool "openspec_task_set_status" +schema-task-set+ 'tool-task-set)
  (:tool "openspec_change_create" +schema-change-create+ 'tool-change-create)
  (:tool "openspec_change_promote" +schema-change-promote+ 'tool-change-promote)
  (:tool "openspec_change_archive" +schema-change-archive+ 'tool-change-archive)
  (:tool "openspec_change_unarchive" +schema-change-unarchive+ 'tool-change-unarchive)
  (:tool "openspec_idea_create" +schema-idea-create+ 'tool-idea-create)
  (:tool "openspec_idea_enrich" +schema-idea-enrich+ 'tool-idea-enrich)
  (:tool "openspec_idea_promote" +schema-idea-promote+ 'tool-idea-promote)
  (:tool "openspec_spec_list" +schema-spec-list+ 'tool-spec-list)
  (:tool "openspec_spec_show" +schema-spec-show+ 'tool-spec-show)
  (:tool "openspec_spec_create" +schema-spec-create+ 'tool-spec-create)
  (:tool "openspec_spec_diff" +schema-spec-diff+ 'tool-spec-diff)
  (:tool "openspec_validate" +schema-validate+ 'tool-validate)
  (:tool "openspec_cli" +schema-cli+ 'tool-cli)
  (:help :openspec "openspec: spec-driven development over a project's openspec/ folder - /openspec" +primer+)
  (:command "openspec" 'run-slash
            :description "The project's OpenSpec changes, their status and task progress"
            :argument-hint ""))

;;; --- the same verbs, for eval and a layer -----------------------------------------------

(defun verb-arguments (pairs)
  "PAIRS as a tool call's arguments object: (:workdir \"x\" :change \"y\")."
  (apply #'nlk:make-json-object
         (loop for (key value) on pairs by #'cddr
               append (list (string-downcase (substitute #\_ #\- (string key))) value))))

(defmacro define-verb (name operation)
  "NAME as a function of (:key value ...) pairs, answering the tool's JSON
text; it refuses while the cell is not running, as every verb here does."
  `(defun ,name (&rest pairs)
     (running-settings)
     (answering (answer (,operation (verb-arguments pairs))))))

(define-verb status op-status)
(define-verb changes op-list)
(define-verb tasks op-tasks)
(define-verb task-set op-task-set)
(define-verb validate op-validate)
(define-verb spec-diff op-spec-diff)
(define-verb instructions op-instructions)
(define-verb context op-context)
(define-verb cli op-cli)
