;;;; plan.lisp --- the plan file, its phasing rules, and the dispatch record.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept one JSON document under its own data root
;;;; (<data>/plan.json) and a second (<data>/runs.json) recording what it had
;;;; dispatched, because its panel and its agent tools had to agree on what a
;;;; phased plan looked like. A cell keeps the same two documents, under
;;;; (nlk:home "agent-batch/") or the section's state_dir, and the same rules:
;;;; a task list, phases of tasks in execution order, a "pending" status per
;;;; phase that a dispatch turns to "running", and every stored task in
;;;; exactly one phase.

(in-package #:nodecode-agent-batch)

(defparameter +plan-file+ "plan.json"
  "The plan itself: tasks, phases, per-phase status, the repo it targets.")

(defparameter +runs-file+ "runs.json"
  "What this cell dispatched, one record each: the original's runs.json.")

;;; --- where the state lives ----------------------------------------------------

(defun state-dir ()
  "The folder holding plan.json and runs.json: the section's state_dir, else
(nlk:home \"agent-batch/\"), so a profile keeps its own."
  (let ((given (setting :state-dir)))
    (uiop:ensure-directory-pathname
     (if (and given (plusp (length given)))
         (nlk:expand-home given)
         (nlk:home "agent-batch/")))))

(defun state-path (name)
  (merge-pathnames name (state-dir)))

;;; --- json on disk -------------------------------------------------------------

(defun items (value)
  "VALUE as a list: a JSON array arrives as a vector, a Lisp-built one may be a list."
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) value)
        (t '())))

(defun texts (value)
  "VALUE's non-empty strings, trimmed: what a stored task list is made of."
  (loop for item in (items value)
        for text = (and (stringp item) (nlk:trimmed item))
        when (and text (plusp (length text))) collect text))

(defun read-file-json (path default)
  "PATH decoded, or DEFAULT when it is absent or unreadable: a plan file a person
edited by hand must not stop the cell."
  (if (probe-file path)
      (handler-case (or (nlk:decode-json (uiop:read-file-string path)) default)
        (error () default))
      default))

(defun write-file-json (path object)
  "OBJECT at PATH as JSON, atomically: a half-written plan is never read back."
  (ensure-directories-exist path)
  (let ((tmp (merge-pathnames (format nil "~a.tmp" (file-namestring path))
                               (uiop:pathname-directory-pathname path))))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                             :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:encode-json-object object) out)
      (terpri out))
    (uiop:rename-file-overwriting-target tmp path)
    path))

;;; --- the plan as a plist --------------------------------------------------------

(defun empty-plan ()
  (list :tasks '() :phases '() :status '() :repo "" :model ""))

(defun load-plan ()
  "The stored plan as a plist of tasks, phases, status, repo and model, the
defaults where the file is absent or silent."
  (let* ((object (read-file-json (state-path +plan-file+) (nlk:make-json-object)))
         (object (if (hash-table-p object) object (nlk:make-json-object))))
    (list :tasks (texts (nlk:json-value object :array "tasks"))
          :phases (loop for group in (items (nlk:json-value object :array "phases"))
                        collect (texts group))
          :status (texts (nlk:json-value object :array "phase_status"))
          :repo (or (nlk:json-value object :string "repo") "")
          :model (or (nlk:json-value object :string "model") ""))))

(defun save-plan (plan)
  (write-file-json
   (state-path +plan-file+)
   (nlk:make-json-object "tasks" (coerce (getf plan :tasks) 'vector)
                          "phases" (coerce (loop for group in (getf plan :phases)
                                                  collect (coerce group 'vector))
                                           'vector)
                          "phase_status" (coerce (getf plan :status) 'vector)
                          "repo" (or (getf plan :repo) "")
                          "model" (or (getf plan :model) ""))))

(defun load-runs ()
  "The dispatch records this cell kept, newest last, each a plist."
  (let* ((object (read-file-json (state-path +runs-file+) (nlk:make-json-object)))
         (object (if (hash-table-p object) object (nlk:make-json-object))))
    (loop for entry in (items (nlk:json-value object :array "runs"))
          when (hash-table-p entry)
            collect (list :phase (nlk:json-value entry :integer "phase")
                          :tasks (texts (nlk:json-value entry :array "tasks"))
                          :repo (or (nlk:json-value entry :string "repo") "")
                          :model (or (nlk:json-value entry :string "model") "")
                          :base-branch (or (nlk:json-value entry :string "base_branch") "")
                          :dispatched-at (or (nlk:json-value entry :string "dispatched_at") "")))))

(defun save-runs (runs)
  (write-file-json
   (state-path +runs-file+)
   (nlk:make-json-object
    "runs" (coerce (loop for run in runs
                          collect (nlk:make-json-object
                                   "phase" (getf run :phase)
                                   "tasks" (coerce (getf run :tasks) 'vector)
                                   "repo" (getf run :repo)
                                   "model" (getf run :model)
                                   "base_branch" (getf run :base-branch)
                                   "dispatched_at" (getf run :dispatched-at)))
                    'vector))))

(defun utc-stamp ()
  "Now as the original's datetime.isoformat(timespec=\"seconds\") wrote it."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

;;; --- the phasing rules -----------------------------------------------------------

(defun resolve-task (tasks item)
  "ITEM -- a 0-based index or a task's own text -- as (VALUES TEXT INDEX)."
  (cond
    ((numberp item)
     (let ((index (if (integerp item) item (floor item))))
       (unless (<= 0 index (1- (length tasks)))
         (fail "task index ~d out of range (0..~d)" index (1- (length tasks))))
       (values (nth index tasks) index)))
    ((stringp item)
     (let ((wanted (nlk:trimmed item)))
       (let ((index (position wanted tasks :test #'string=)))
         (unless index (fail "task not in the stored plan: ~s" (subseq wanted 0 (min 80 (length wanted)))))
         (values (nth index tasks) index))))
    (t (fail "a phase names tasks by index (0) or by their text (\"...\"), not ~s" item))))

(defun normalize-phases (plan groups)
  "GROUPS -- a task index or a task's text, grouped by the order the groups run in
-- as the stored phases' texts. Every stored task lands in exactly one group, so a
dropped or invented task fails loudly instead of silently never running."
  (let ((tasks (getf plan :tasks))
        (groups (items groups)))
    (unless groups (fail "phases must be a non-empty list of groups"))
    (unless tasks (fail "no tasks stored -- save the task list first"))
    (let ((used '()) (phases '()))
      (dolist (group groups)
        (let ((group (items group)))
          (unless group (fail "each phase must be a non-empty list of tasks"))
          (let ((resolved '()))
            (dolist (item group)
              (multiple-value-bind (text index) (resolve-task tasks item)
                (when (member index used)
                  (fail "task ~d (~s) appears in two phases" index
                        (subseq text 0 (min 60 (length text)))))
                (push index used)
                (push text resolved)))
            (push (nreverse resolved) phases))))
      (let ((missing (loop for index from 0 below (length tasks)
                           unless (member index used) collect index)))
        (when missing (fail "tasks not placed in any phase: ~{~d~^, ~}" missing)))
      (nreverse phases))))

(defun set-phases (plan groups)
  "The plan with GROUPS normalized and a pending status per phase."
  (let ((phases (normalize-phases plan groups)))
    (setf (getf plan :phases) phases
          (getf plan :status) (make-list (length phases) :initial-element "pending"))
    plan))

(defun phase-status (plan index)
  "PHASE INDEX's status, pending when the file carries none."
  (or (nth index (getf plan :status)) "pending"))

(defun set-phase-status (plan index value)
  "PLAN's status list, long enough for every phase, with INDEX set to VALUE."
  (let* ((phases (length (getf plan :phases)))
         (status (append (getf plan :status) (make-list (max 0 (- phases (length (getf plan :status))))
                                                        :initial-element "pending"))))
    (when (< index (length status)) (setf (nth index status) value))
    status))

;;; --- what a plan reads like ------------------------------------------------------

(defun tasks-text (plan)
  "The stored tasks, numbered: the indices agent_batch_phase takes."
  (let ((tasks (getf plan :tasks)))
    (if (null tasks)
        "  (none stored yet: agent_batch_tasks saves a task list)"
        (with-output-to-string (out)
          (loop for task in tasks for index from 0
                do (format out "  [~d] ~a~%" index task))))))

(defun phases-text (plan)
  (let ((phases (getf plan :phases)))
    (if (null phases)
        "  (none: agent_batch_phase groups the tasks into phases)"
        (with-output-to-string (out)
          (loop for group in phases for index from 0
                do (format out "  phase ~d  ~a  ~d task~:p: ~{~a~^, ~}~%"
                           index (phase-status plan index) (length group)
                           (loop for text in group collect (format nil "~s" text))))))))

(defun dispatches-text (plan)
  (let ((runs (load-runs)))
    (if (null runs)
        "  (nothing dispatched from this machine)"
        (with-output-to-string (out)
          (dolist (run (last runs (min 10 (length runs))))
            (format out "  phase ~a at ~a on ~a (~d task~:p)~%"
                    (getf run :phase) (getf run :dispatched-at) (getf run :repo)
                    (length (getf run :tasks))))))))

(defun repository (repo)
  "REPO when it reads as owner/name, else the section's, else a refusal naming
where one goes."
  (let ((repo (nlk:trimmed (or repo ""))))
    (when (zerop (length repo)) (setf repo (nlk:trimmed (or (setting :repo) ""))))
    (cond ((zerop (length repo))
           (fail "no repo set -- name owner/name in the agent-batch section, or pass repo"))
          ((not (repo-name-p repo)) (fail "repo must look like owner/name, got ~s" repo))
          (t repo))))

(defun repo-name-p (repo)
  "T when REPO is owner/name: the shape the API paths and the dispatch URL need."
  (let ((slash (position #\/ repo)))
    (and slash (plusp slash) (< slash (1- (length repo)))
         (null (position #\/ repo :start (1+ slash)))
         (every (lambda (character)
                  (or (alphanumericp character) (find character "_.-/")))
                repo))))
