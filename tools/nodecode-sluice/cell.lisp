;;;; cell.lisp --- the primer, the stage's verbs, the tools and the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hermes let a plugin replace the terminal the agent's tools ran in, so every
;;;; command and file edit landed in a staged copy. Nodecode's shell is its own,
;;;; so the stage is reached through a tool instead: sluice_run runs one command
;;;; in the copy, sluice_diff numbers what changed, sluice_apply lands what the
;;;; operator approved. The review and the apply are the plugin's, hunk for hunk.

(in-package #:nodecode-sluice)

(defparameter +primer+
  "sluice runs the agent's work in a staged copy of one project folder, so the
real files stay as they were until the operator approves. Four tools:
  sluice_run(command)          run a shell command in the copy. The workspace's
      own paths are rewritten to the copy and back, so the answer reads like
      the real folder. allow: true lets a command the copy cannot contain run.
  sluice_diff()                every change, numbered: each block of edited
      lines, and each created, deleted, renamed, replaced or mode-changed file.
      Run this before sluice_apply: apply refuses a review it has not seen.
  sluice_apply(selection)      land the changes you name: \"all\", \"1,3-5\",
      or a changed file's path. Per file, all or nothing, written atomically,
      and a file that moved in the real folder meanwhile is a conflict,
      reported and left alone. discard_rest: true drops the rest.
  sluice_discard()             throw the staged copy away; the real folder was
      never touched.
A command a copy cannot contain -- an install, a git push, a network write, a
write or delete outside the workspace, sudo, curl | sh -- is NOT CONTAINED and
sluice_run holds it: it answers with the finding and runs nothing until the call
says allow: true. The operator reads the same review with /sluice (status, diff,
apply, discard, log). Nothing is a trap for the model: say what you changed and
ask the operator to run /sluice diff."
  "What (help :sluice) answers while the cell runs.")

;;; --- the folder the stage is of ---------------------------------------------

(defun workspace-root ()
  "The project folder this cell stages, or a refusal naming the setting."
  (let ((value (setting :workspace)))
    (or (and value
             (plusp (length (string-trim '(#\Space #\Tab) value)))
             (real-directory value))
        (fail "the sluice section names no workspace: set workspace to the project folder to stage, in the sluice section of the config"))))

(defun stage-options ()
  (list :copy-git (if (null (setting :copy-git)) t (setting :copy-git))
        :max-file-bytes (* 1024 1024 (or (setting :max-file-mb) 25))
        :max-total-bytes (* 1024 1024 (or (setting :max-workspace-mb) 1024))))

(defun ensure-stage* ()
  (apply #'open-or-create (workspace-root) (stage-options)))

;;; --- running one command ------------------------------------------------------

(defun sh-run (command directory)
  "COMMAND run by sh in DIRECTORY => (values OUTPUT EXIT)."
  (let ((out (make-string-output-stream)) (err (make-string-output-stream)))
    (let ((code (nth-value 2 (uiop:run-program (list "sh" "-c" command)
                                               :directory directory
                                               :output out :error-output err
                                               :ignore-error-status t))))
      (values (format nil "~a~a" (get-output-stream-string out)
                             (get-output-stream-string err))
              code))))

(defvar *run* #'sh-run
  "What runs a staged command: the tests bind this and answer instead.")

(defun replace-all (text from to)
  (let ((start 0) (position (and (plusp (length from)) (search from text))))
    (if (null position)
        text
        (with-output-to-string (out)
          (loop while position
                do (write-string text out :start start :end position)
                   (write-string to out)
                   (setf start (+ position (length from))
                         position (search from text :start2 start)))
          (write-string text out :start start)))))

(defun log-line (stage line)
  (ignore-errors
    (with-open-file (out (format nil "~alog.sexp" (stage-dir stage))
                         :direction :output :if-exists :append :if-does-not-exist :create
                         :external-format :utf-8)
      (let ((*print-length* 60) (*print-level* 6))
        (with-standard-io-syntax (prin1 line out)))
      (terpri out))))

(defun read-log (stage)
  (let ((file (format nil "~alog.sexp" (stage-dir stage))))
    (when (uiop:file-exists-p file)
      (with-open-file (in file :external-format :utf-8 :if-does-not-exist nil)
        (when in
          (loop for form = (let ((*read-eval* nil)) (read in nil :eof))
                until (eq form :eof)
                when (listp form) collect form))))))

(defun held-lines (stage)
  (remove-if-not (lambda (line) (getf line :held)) (read-log stage)))

(defun command-lines (stage)
  (remove-if-not (lambda (line) (getf line :cmd)) (read-log stage)))

;;; --- the review ----------------------------------------------------------------

(defun render-diff (units &optional only)
  (if (null units)
      "sluice: no pending changes: the stage matches the workspace as it was taken."
      (with-output-to-string (out)
        (format out "sluice: ~d change~:p in ~d file~:p, not applied yet~%"
                (length units) (length (remove-duplicates (mapcar #'unit-path units) :test #'equal)))
        (let ((current nil))
          (dolist (unit units)
            (when (and (or (null only) (equal only (unit-path unit)))
                       (not (equal current (unit-path unit))))
              (setf current (unit-path unit))
              (format out "~%~a~%" current))
            (when (or (null only) (equal only (unit-path unit)))
              (format out "[~d] ~a~@[ (~a)~]~%" (unit-id unit) (unit-summary unit)
                      (and (unit-old-path unit) (format nil "from ~a" (unit-old-path unit))))
              (dolist (line (unit-body unit)) (format out "    ~a~%" line)))))
        (format out "~%apply with sluice_apply(selection: \"all\") or /sluice apply all~%"))))

(defun render-status (stage units)
  (with-output-to-string (out)
    (format out "sluice: workspace ~a~%" (stage-root stage))
    (format out "  ~d pending change~:p~@[ in ~d file~:p~]~%" (length units)
            (and units (length (remove-duplicates (mapcar #'unit-path units) :test #'equal))))
    (format out "  commands run in the stage: ~d~%" (length (command-lines stage)))
    (let ((held (held-lines stage)))
      (when held (format out "  held as NOT CONTAINED: ~d (see /sluice log)~%" (length held))))
    (when (stage-skipped stage)
      (format out "  not copied: ~d (large files or outside symlinks)~%" (length (stage-skipped stage))))
    (let ((moved (drift stage)))
      (when moved
        (format out "  changed in the workspace since the stage was taken: ~d file~:p~%" (length moved))))
    (when units (format out "  /sluice diff reviews them.~%"))))

(defun parse-selection (text units)
  "TEXT as a list of unit ids: all, 1,3-5, or a changed file's path."
  (let ((trimmed (string-downcase (string-trim '(#\Space #\Tab) text)))
        (valid (mapcar #'unit-id units))
        (chosen '()))
    (cond
      ((equal trimmed "all") (setf chosen valid))
      (t (dolist (part (remove-if (lambda (one) (zerop (length one)))
                                   (uiop:split-string trimmed
                                                       :separator '(#\Space #\Comma))))
           (let ((by-file (remove-if-not (lambda (unit) (string-equal (unit-path unit) part)) units)))
             (cond
               (by-file (dolist (unit by-file) (pushnew (unit-id unit) chosen)))
               (t (let* ((dash (position #\- part))
                         (low (if dash (subseq part 0 dash) part))
                         (high (if dash (subseq part (1+ dash)) part)))
                    (unless (every #'digit-char-p part)
                      (unless (and (every #'digit-char-p low) (every #'digit-char-p high))
                        (fail "cannot read ~s: use numbers like 1,3-5, a changed file's path, or all" part)))
                    (loop for number from (parse-integer low) to (parse-integer high)
                          do (if (member number valid) (pushnew number chosen)
                                 (fail "there is no change number ~d" number)))))))))
    (sort chosen #'<))))

;;; --- the verbs the tools and /sluice share ----------------------------------

(defun run-text (command cwd allow)
  "One shell command in the staged copy, the plugin's rewriting and its gate."
  (let* ((stage (ensure-stage*))
         (root (stage-root stage))
         (findings (classify-command command root (setting :allow-paths))))
    (if (and findings (not allow))
        (progn
          (dolist (finding findings)
            (log-line stage (list :held t :category (finding-category finding)
                                  :detail (finding-detail finding) :tool "sluice_run")))
          (format nil "~{~a~%~}Nothing ran. Call sluice_run again with allow: true if it should really happen."
                  (mapcar #'finding-message findings)))
        (let ((directory (if (and cwd (plusp (length cwd)))
                             (or (real-directory (path-join (stage-path stage) cwd))
                                 (stage-path stage))
                             (stage-path stage))))
          (multiple-value-bind (output code)
              (funcall *run* (replace-all command root (stage-path stage)) directory)
            (log-line stage (list :cmd command :cwd directory :exit code))
            (format nil "exit ~a, run in the stage (the real workspace is unchanged)~%~a"
                    code (replace-all output (stage-path stage) root)))))))

(defun diff-text (&optional path)
  (let* ((stage (ensure-stage*))
         (only (and path (plusp (length path)) path)))
    (multiple-value-bind (units rev) (changed-units stage)
      (when units
        (setf (stage-last-rev stage) rev)
        (save-stage stage))
      (render-diff units only))))

(defun apply-text (selection discard-rest)
  (let ((stage (ensure-stage*)))
    (multiple-value-bind (units rev) (changed-units stage)
      (cond ((null units) "sluice: no pending changes.")
            ((zerop (length (stage-last-rev stage)))
             "sluice: review first: call sluice_diff, then apply what you saw.")
            ((not (equal (stage-last-rev stage) rev))
             "sluice: the staged changes moved since you last looked. Run sluice_diff again, then apply what you see.")
            (t (let ((result (apply-units stage units (parse-selection selection units))))
                 (with-output-to-string (out)
                   (format out "sluice: applied ~d change~:p to ~d file~:p.~%"
                           (getf result :applied) (length (getf result :files)))
                   (dolist (conflict (getf result :conflicts))
                     (format out "CONFLICT ~a: ~a. Left unchanged.~%"
                             (car conflict) (cdr conflict)))
                   (cond (discard-rest
                          (discard-stage stage)
                          (format out "Discarded the rest.~%"))
                         ((getf result :conflicts)
                          (format out "Conflicted files are still staged; /sluice diff shows them again.~%"))))))))))

(defun status-text ()
  (let ((stage (ensure-stage*)))
    (multiple-value-bind (units rev) (changed-units stage)
      (declare (ignore rev))
      (render-status stage units))))

(defun discard-text ()
  (discard-stage (ensure-stage*))
  "sluice: discarded the staged copy. The workspace was not changed.")

(defun log-text ()
  (let* ((stage (ensure-stage*))
         (lines (read-log stage)))
    (if (null lines)
        "sluice: nothing logged yet."
        (with-output-to-string (out)
          (format out "sluice log (commands run in the stage, and what was held):~%")
          (dolist (line (last lines (min 40 (length lines))))
            (if (getf line :held)
                (format out "  HELD  [~a] ~a~%" (getf line :category) (getf line :detail))
                (format out "  exit ~a  ~a~%" (getf line :exit) (getf line :cmd))))))))

(defun run-command (arguments session-id)
  "One /sluice invocation: status, diff, apply, discard or log."
  (declare (ignore session-id))
  (let* ((words (remove-if (lambda (one) (zerop (length one)))
                           (uiop:split-string (or arguments "") :separator '(#\Space #\Tab))))
         (verb (string-downcase (or (first words) "status")))
         (rest (format nil "~{~a~^ ~}" (rest words))))
    (cond ((member verb '("status" "") :test #'equal) (status-text))
          ((equal verb "diff") (diff-text (and (plusp (length rest)) rest)))
          ((equal verb "apply")
           (let ((discard-rest (search "--discard-rest" rest)))
             (apply-text (string-trim '(#\Space #\Tab)
                                       (if discard-rest
                                           (replace-all rest "--discard-rest" "")
                                           rest))
                         (and discard-rest t))))
          ((equal verb "discard") (discard-text))
          ((equal verb "log") (log-text))
          (t (format nil "sluice: unknown command ~a.~%  /sluice [status|diff|apply all|apply 1,3-5|discard|log]" verb)))))

;;; --- what the model is shown and calls -------------------------------------------

(nlk:define-contract sluice-run
  (:doc "Run one shell command in the staged copy of the workspace. The
workspace's own paths in the command are rewritten to the copy and back, so
the answer reads like the real folder; the real folder is not touched. A
command a copy cannot contain -- an install, a git push, a network write,
a write or delete outside the workspace, sudo, curl | sh -- is held: it
answers NOT CONTAINED and runs nothing until the call says allow: true.")
  ("command" :text :required t :doc "the shell command, as the terminal would take it")
  ("cwd" :string :doc "a folder inside the workspace to run in; the stage root when absent")
  ("allow" :boolean :default nil :doc "run it even though a staged copy cannot contain it"))

(nlk:define-contract sluice-diff
  (:doc "Every change in the staged copy, numbered: each block of edited
lines, and each created, deleted, renamed, replaced or mode-changed file.
Run this before sluice_apply: apply refuses a review it has not seen.")
  ("path" :string :doc "one changed file to show; every file when absent"))

(nlk:define-contract sluice-apply
  (:doc "Land the staged changes the operator approved. Per file, all or
nothing, written atomically; a file that changed in the real workspace since
the stage was taken is a conflict, reported and left alone. Call sluice_diff
first: apply refuses a review it has not seen, and a stage that moved since.")
  ("selection" :text :required t :doc "\"all\", \"1,3-5\", or a changed file's path")
  ("discard_rest" :boolean :default nil :doc "drop the changes not applied, then the stage"))

(nlk:define-contract sluice-discard
  (:doc "Throw the staged copy away. The real workspace was never changed.")
  ("confirm" :boolean :default t :doc "discard the staged copy"))

(defun run-in-stage (report)
  (run-text (sluice-run-command report) (sluice-run-cwd report) (sluice-run-allow report)))

(defun diff-tool (report)
  (diff-text (sluice-diff-path report)))

(defun apply-tool (report)
  (apply-text (sluice-apply-selection report) (sluice-apply-discard-rest report)))

(defun discard-tool (report)
  (declare (ignore report))
  (discard-text))

(nle:define-cell sluice
  (:section ("sluice")
    (:guide "workspace is the project folder to stage; every other setting has a default")
    ("workspace" :path :required t
     :doc "the project folder the agent works in; its real files are not touched until sluice_apply")
    ("copy_git" :boolean :default t
     :doc "copy .git into the stage, so git status and git diff work there and the real history is not touched")
    ("max_file_mb" :integer :default 25 :min 1
     :doc "the largest file copied into the stage; bigger ones are listed as skipped")
    ("max_workspace_mb" :integer :default 1024 :min 1
     :doc "the largest workspace staged; a bigger one is refused")
    ("allow_paths" :list
     :doc "outside paths that are written without the NOT CONTAINED label, e.g. [\"/tmp\"]"))
  (:help :sluice
         "sluice: run the agent's work in a staged copy, review it, apply what you approve - (help :sluice)"
         +primer+)
  (:command "sluice" 'run-command
            :description "Review, apply or discard the agent's staged changes"
            :argument-hint "[status|diff|apply|discard|log]")
  (:tool "sluice_run" (nlk:contract 'sluice-run) #'run-in-stage)
  (:tool "sluice_diff" (nlk:contract 'sluice-diff) #'diff-tool)
  (:tool "sluice_apply" (nlk:contract 'sluice-apply) #'apply-tool)
  (:tool "sluice_discard" (nlk:contract 'sluice-discard) #'discard-tool))

