;;;; cell.lisp --- the four tools, the hook around a tool call, and /chronos.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin contributed four model tools, a pre-tool-call hook, a
;;;; post-call hint and a /chronos command. Here the four tools keep their
;;;; names, the hook is Nodecode's :tool point (one call, before and after),
;;;; the hint is appended to a result that reads as a failure, and /chronos
;;;; lists, diffs and restores.
;;;;
;;;; A checkpoint is taken before a call that can change the workspace and
;;;; nothing else; a call that changed nothing records no checkpoint, as the
;;;; original's did not. Every failure in the hook is swallowed: a snapshot
;;;; that cannot be taken must never stop the tool the operator asked for.

(in-package #:nodecode-chronos)

;;; --- the manual -----------------------------------------------------------------

(defparameter +primer+
  "Every file-editing or shell tool call can be undone. Before one runs, the
chronos cell records a checkpoint of the workspace in a private git repository
inside it (<workspace>/.nodecode-chronos/), so the project's own history never
moves and nothing leaves this machine. The model has four tools:
  chronos_checkpoint(description)        take one before a risky step.
  chronos_list_checkpoints(limit)        what is recorded: id, time, tool, description.
  chronos_diff(checkpoint_id|from_checkpoint|to_checkpoint, max_chars)
      what changed in one checkpoint, between two, or from one to the files
      on disk now; `to_checkpoint: \"WORKTREE\"' is the files as they are.
  chronos_rollback(checkpoint_id, clean)
      put the files back the way that checkpoint left them. A safety
      checkpoint of the state now is taken first, so a rollback can itself be
      rolled back. clean (the default) deletes files created since the
      checkpoint; clean: false keeps them.
The operator reads the same with /chronos, /chronos diff [ID] and
/chronos rollback ID [--no-clean]. Snapshots cover every file the section's
`ignored' patterns do not exclude -- .env and other untracked files included,
which is why the shadow repository keeps them local to the workspace. Ignored
files, empty directories and file modes are not restored. git must be on PATH;
a restore can be slow on a large workspace and runs without a prompt."
  "What (help :chronos) answers while the cell runs.")

;;; --- answers ---------------------------------------------------------------------

(defun jsonify (value)
  "VALUE as the JSON structure the encoder writes: a plist becomes an object
keyed by its keywords, a list a vector."
  (cond
    ((null value) nil)
    ((and (consp value) (keywordp (first value)))
     (let ((table (nlk:make-json-object)))
       (loop for (key item) on value by #'cddr
             do (setf (gethash (string-downcase (string key)) table) (jsonify item)))
       table))
    ((consp value) (coerce (mapcar #'jsonify value) 'vector))
    (t value)))

(defun answer (thunk)
  "THUNK's plist as the JSON text a tool answers; a refusal or any error as
the same shape with status error, never as a condition the model cannot read."
  (handler-case
      (nlk:encode-json-object (jsonify (funcall thunk)))
    (error (condition)
      (nlk:encode-json-object
       (jsonify (list :status "error" :error (princ-to-string condition)))))))

;;; --- the tools ------------------------------------------------------------------

(nlk:define-contract checkpoint-arguments
  (:doc "Record a checkpoint of the workspace before a risky step: a refactor, a
migration, a dependency change, a long shell command. The checkpoint is a
private commit in the workspace's own shadow repository, so the project's git
history is untouched; a call that changed nothing since the last checkpoint
records none. Returns JSON with status, created, id and hash.")
  ("description" :text :required t
   :doc "short description of the operation about to run")
  ("tool_name" :string
   :doc "the tool or workflow this checkpoint precedes"))

(nlk:define-contract list-arguments
  (:doc "List the workspace's recent checkpoints, newest first: id, time, the
tool that took it and its description. Use it to pick an id for chronos_diff or
chronos_rollback. Returns JSON with status, count and checkpoints.")
  ("limit" :integer :default 15
   :doc "how many checkpoints to list"))

(nlk:define-contract diff-arguments
  (:doc "Show what changed in the workspace: in one checkpoint, between two of
them, or from a checkpoint to the files on disk now (to_checkpoint WORKTREE,
the default when from_checkpoint is given). Returns JSON with status, the diff
text, its stat and the untracked files a WORKTREE diff did not cover.")
  ("from_checkpoint" :string
   :doc "the checkpoint to diff from; omitted, the change is read from the latest one")
  ("to_checkpoint" :string
   :doc "the checkpoint to diff to, or WORKTREE for the files on disk")
  ("checkpoint_id" :string
   :doc "show only the change this one checkpoint introduced")
  ("max_chars" :integer :default 400000
   :doc "how much diff text to return; the rest is cut and truncated says so"))

(nlk:define-contract rollback-arguments
  (:doc "Put the workspace's files back the way a checkpoint left them, after a
failed test, a broken build, a corrupted config or a bad refactor. A safety
checkpoint of the state now is taken first, so the rollback can itself be
undone. Returns JSON naming the checkpoint restored and the safety one.")
  ("checkpoint_id" :text :required t
   :doc "the checkpoint id (8 characters, full hash, or latest)")
  ("clean" :boolean :default t
   :doc "delete files created since the checkpoint so the workspace matches it; false keeps them"))

(defun checkpoint-tool (report)
  "chronos_checkpoint."
  (answer (lambda ()
            (checkpoint (workspace) (checkpoint-arguments-description report)
                        :tool (or (checkpoint-arguments-tool-name report) "agent")))))

(defun list-tool (report)
  "chronos_list_checkpoints."
  (answer (lambda ()
            (let* ((root (workspace))
                   (found (checkpoints root :limit (max 1 (list-arguments-limit report)))))
              (list :status "ok" :count (length found) :workspace root :checkpoints found)))))

(defun diff-tool (report)
  "chronos_diff."
  (answer (lambda ()
            (diff-answer (workspace)
                         :from (diff-arguments-from-checkpoint report)
                         :to (diff-arguments-to-checkpoint report)
                         :id (diff-arguments-checkpoint-id report)
                         :max-chars (max 1000 (or (diff-arguments-max-chars report)
                                                   (setting :max-diff-chars)))))))

(defun rollback-tool (report)
  "chronos_rollback."
  (answer (lambda ()
            (rollback (workspace) (rollback-arguments-checkpoint-id report)
                      :clean (rollback-arguments-clean report)))))

;;; --- the hook -------------------------------------------------------------------

(defparameter +modifying-tools+
  '("eval" "sh" "shell" "run_program" "write_file" "write-file" "edit" "patch"
    "apply_patch" "delete_file" "delete-file" "rename_file" "copy_file")
  "The tools whose call can change the workspace. Eval and a shell command are
the two that reach everything else; the file verbs are named beside them for
the hosts that expose them as tools of their own.")

(defparameter +own-tools+
  '("chronos_checkpoint" "chronos_rollback" "chronos_diff" "chronos_list_checkpoints")
  "This cell's own tools, which never checkpoint: a call that does would
checkpoint the checkpoint.")

(defun call-description (op)
  "One tool call as the checkpoint's description: the tool, and the argument
that says what it touched."
  (let* ((name (getf op :name))
         (arguments (getf op :arguments))
         (named (and (hash-table-p arguments)
                     (loop for key in '("form" "command" "path" "file_path" "target")
                           for value = (gethash key arguments)
                           when (and (stringp value) (plusp (length value)))
                             return (clip value 80)))))
    (format nil "Before ~a~@[: ~a~]" (or name "a tool") named)))

(defun failed-result-p (result)
  "Does RESULT read as a failed call?"
  (let ((text (string-downcase result)))
    (or (search "error" text) (search "traceback" text) (search "[exit" text))))

(defun around-tool (op next)
  "The :tool point: take a checkpoint before a call that can change the
workspace, and point at it when the call comes back looking failed. A
checkpoint that cannot be taken is logged by its absence: the call runs."
  (let* ((name (string-downcase (string (or (getf op :name) ""))))
         (id (when (and (setting :auto-snapshot)
                        (member name +modifying-tools+ :test #'string=)
                        (not (member name +own-tools+ :test #'string=)))
               (ignore-errors
                 (getf (checkpoint (workspace) (call-description op) :tool name) :id))))
         (result (funcall next op)))
    (if (and id (stringp result) (failed-result-p result))
        (concatenate 'string result
                     (format nil "~%[chronos] this call looks failed. Checkpoint ~a was taken ~
before it ran: chronos_diff(checkpoint_id: \"~a\") shows what it changed, ~
chronos_rollback(checkpoint_id: \"~a\") puts the workspace back." id id id))
        result)))

;;; --- /chronos -------------------------------------------------------------------

(defun checkpoints-text (root limit)
  (let ((found (checkpoints root :limit limit)))
    (if found
        (with-output-to-string (out)
          (format out "~d checkpoint~:p in ~a, newest first~%/chronos diff ID | /chronos rollback ID~%"
                  (length found) root)
          (dolist (entry found)
            (format out "  ~a  ~@[~a  ~]~a  ~a~%"
                    (getf entry :id)
                    (getf entry :time)
                    (getf entry :tool)
                    (getf entry :description))))
        (format nil "no checkpoint yet in ~a: one is taken before the next file edit or shell call~%"
                root))))

(defun diff-text-of (root id)
  (let* ((answer (diff-answer root :id (and id (plusp (length id)) id)
                               :max-chars 20000))
         (diff (getf answer :diff)))
    (with-output-to-string (out)
      (format out "~a: from ~a to ~a~%~a~@[~%untracked: ~{~a~^, ~}~]~%"
              (or id "latest")
              (getf answer :from) (getf answer :to) diff
              (getf answer :untracked)))))

(defun rollback-text (root id clean)
  (if (null id)
      (format nil "name a checkpoint: /chronos rollback ID (see /chronos)~%")
      (let ((report (rollback root id :clean clean)))
        (with-output-to-string (out)
          (format out "~a~%~@[~d file~:p created since were removed~%~]~@[a safety checkpoint of what was there: ~a~%~]"
                  (getf report :message)
                  (let ((removed (getf report :untracked-removed)))
                    (and removed (plusp removed) removed))
                  (getf report :safety))))))

(defun run-command (arguments session-id)
  "The /chronos command: list, diff one checkpoint, restore one."
  (declare (ignore session-id))
  (let* ((words (remove "" (uiop:split-string (or arguments "") :separator '(#\Space #\Tab))
                        :test #'equal))
         (verb (string-downcase (or (first words) "list")))
         (rest (rest words)))
    (handler-case
        (cond
          ((member verb '("list" "ls" "") :test #'string=)
           (checkpoints-text (workspace)
                             (or (ignore-errors (parse-integer (or (first rest) "") :junk-allowed t))
                                 (setting :max-checkpoints))))
          ((string= verb "diff") (diff-text-of (workspace) (first rest)))
          ((string= verb "rollback")
           (rollback-text (workspace) (first rest)
                          (not (member "--no-clean" rest :test #'string=))))
          ((member verb '("help" "?") :test #'string=) +primer+)
          (t (format nil "unknown /chronos verb ~s: list | diff [ID] | rollback ID [--no-clean]~%" verb)))
      (error (condition) (format nil "chronos: ~a~%" condition)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell chronos
  (:section ("chronos")
    (:guide "git on PATH is all it needs; workspace picks the directory, and every other setting has a default")
    ("workspace" :path
     :doc "the directory checkpoints cover, absolute; this process's own directory when unset")
    ("auto_snapshot" :boolean :default t
     :doc "take a checkpoint before every file-editing or shell tool call")
    ("max_checkpoints" :integer :default 50 :min 1
     :doc "how many checkpoints /chronos lists")
    ("clean_untracked" :boolean :default t
     :doc "a rollback deletes files created since the checkpoint; false keeps them")
    ("ignored" :list
     :doc "extra paths no checkpoint covers, as gitignore patterns (.git and the shadow repository always are)")
    ("max_diff_chars" :integer :default 400000 :min 1000
     :doc "how much diff text one answer carries before it is cut")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one git run's ceiling; its whole process tree is killed past it"))
  (:hook :tool #'around-tool)
  (:tool "chronos_checkpoint" (nlk:contract 'checkpoint-arguments) #'checkpoint-tool)
  (:tool "chronos_list_checkpoints" (nlk:contract 'list-arguments) #'list-tool)
  (:tool "chronos_diff" (nlk:contract 'diff-arguments) #'diff-tool)
  (:tool "chronos_rollback" (nlk:contract 'rollback-arguments) #'rollback-tool)
  (:command "chronos" 'run-command
            :description "List, diff or restore a workspace checkpoint"
            :argument-hint "[list | diff [ID] | rollback ID [--no-clean]]")
  (:help :chronos "chronos: snapshot the workspace before a step, roll it back after - /chronos rollback latest" +primer+))
