;;;; cell.lisp --- the peripheral: a :FRAME observer and /edit-log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; edit-ledger watches a turn's tool results for the files that call
;;;; changed, keeps them until the turn reaches its terminal fact, and then
;;;; appends one row: when, which model, how many files, of which types.
;;;; The paths themselves are dropped there, so the ledger never holds a
;;;; file name. /edit-log reads it back by model, day or file type.
;;;;
;;;; Where a Hermes turn reports its changed paths in one hook payload, a
;;;; Nodecode turn reports them per tool call, in the result fact's metadata:
;;;; workspace_diff.files are the files that call changed on disk, and a
;;;; write-file or edit receipt names the one it touched. Both are read here,
;;;; deduplicated per turn, and folded at turn.completed.

(in-package #:nodecode-edit-ledger)

(nlk:define-peripheral edit-ledger :not-running t :lock "edit-ledger")

(defparameter +verbs+ '("write-file" "edit")
  "The eval verbs whose receipt names a file they wrote, for a call whose
result carries no workspace diff.")

(defvar *turns* (make-hash-table :test #'equal)
  "The running turns that changed files, by (SESSION . TURN): a plist of
:model, :paths and :seen (the paths already counted).")

(defvar *flushed* (make-hash-table :test #'equal)
  "The (SESSION . TURN) keys already appended, so a replayed frame or a
second terminal fact adds no second row.")

;;; --- settings, read leniently ----------------------------------------------------

(defun option (key fallback)
  "KEY's setting, FALLBACK when the cell is stopped."
  (let ((value (ignore-errors (setting key))))
    (if (null value) fallback value)))

(defun ledger-file ()
  "The ledger's path: the section's own when it names one, else the cell's
file under the home."
  (let ((given (option :file nil)))
    (if (and (stringp given) (plusp (length (string given))))
        (pathname (string given))
        (nlk:home "edit-ledger/edits.jsonl"))))

;;; --- what a turn changed ---------------------------------------------------------

(defun payload-metadata (payload)
  (let ((metadata (nlk:json-value payload :object "metadata")))
    (if (hash-table-p metadata) metadata (make-hash-table :test #'equal))))

(defun path-of (entry)
  (let ((path (nlk:json-value entry :string "path")))
    (and path (plusp (length path)) path)))

(defun changed-paths (payload)
  "The files PAYLOAD, one tool result, changed: its workspace diff's files,
and the source of each write-file or edit receipt it carries."
  (let* ((metadata (payload-metadata payload))
         (files (nlk:json-value metadata :array "workspace_diff" "files"))
         (calls (nlk:json-value metadata :array "calls"))
         (paths (loop for entry across (if (vectorp files) files #())
                       for path = (path-of entry)
                       when path collect path)))
    (loop for stamp across (if (vectorp calls) calls #())
          for verb = (nlk:json-value stamp :string "verb")
          for source = (nlk:json-value stamp :string "source")
          when (and verb source (member verb +verbs+ :test #'string=)
                    (plusp (length source)))
            do (pushnew source paths :test #'string=))
    (nreverse paths)))

(defstruct (ledger-turn (:conc-name turn-))
  "One running turn that changed files, until its terminal fact."
  (model nil)
  (paths '())
  (seen (make-hash-table :test #'equal)))

(defun turn-entry (key)
  (or (gethash key *turns*) (setf (gethash key *turns*) (make-ledger-turn))))

(defun note-changes (session turn payload)
  "Keep the files PAYLOAD changed for TURN, once each."
  (when (and turn (plusp (length turn)))
    (let ((key (cons (or session "") turn)))
      (with-edit-ledger-lock
        (let ((entry (turn-entry key)))
          (dolist (path (changed-paths payload))
            (unless (gethash path (turn-seen entry))
              (setf (gethash path (turn-seen entry)) t)
              (push path (turn-paths entry)))))))))

(defun note-model (session turn payload)
  "Remember the model TURN's round named."
  (when (and turn (plusp (length turn)))
    (let ((model (nlk:json-value payload :string "model")))
      (when (and model (plusp (length model)))
        (with-edit-ledger-lock
          (let ((entry (turn-entry (cons (or session "") turn))))
            (unless (turn-model entry) (setf (turn-model entry) model))))))))

(defun record-turn (session turn)
  "Append one row for TURN when it changed files, and forget it either way."
  (let* ((key (cons (or session "") turn))
         (entry (gethash key *turns*)))
    (when entry
      (with-edit-ledger-lock
        (remhash key *turns*)
        (unless (or (null (turn-paths entry)) (gethash key *flushed*))
          (setf (gethash key *flushed*) t)
          (handler-case
              (append-row (ledger-file)
                          (make-row :at (nlk:unix-now) :session session
                                    :model (turn-model entry) :paths (turn-paths entry))
                          :keep (option :keep-rows 5000)
                          :max-bytes (option :max-bytes 2000000))
            ;; A ledger that cannot be written is the cell's problem, never a turn's.
            (error (condition)
              (nle:notice (format nil "edit-ledger: ~a" condition)
                          :level :warning :key "nodecode-edit-ledger"))))))))


(defun observe-frame (op)
  "The :FRAME observer: fold one durable fact into the turn it belongs to."
  (multiple-value-bind (kind payload metadata turn-id) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when kind
      (let ((session (getf op :session-id))
            (turn (or turn-id (nlk:json-value payload :string "turn_id"))))
        (cond ((string= kind "turn.tool_result") (note-changes session turn payload))
              ((string= kind "turn.usage") (note-model session turn payload))
              ((member kind '("turn.completed" "turn.failed" "turn.cancelled")
                       :test #'string=)
               (record-turn session turn)))))))

;;; --- /edit-log -------------------------------------------------------------------

(defparameter +help+
  "/edit-log: which models change files, how many per turn and of which types

  /edit-log             last 24 hours, by model
  /edit-log session     the most recent session
  /edit-log all         everything in the ledger
  /edit-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
  /edit-log types       add to any of the above to list file types (or days)
  /edit-log recent      add to any of the above to list the newest 10 turns instead
  /edit-log recent 25   the newest 25 instead (1 to 100)
  /edit-log clear       delete the ledger"
  "What /edit-log with no argument prints, and the manual's page.")

(defparameter +group-words+
  '(("models" . :model) ("platforms" . :model) ("types" . :type) ("days" . :day))
  "The grouping words, as the original's /edit-log spells them. Nodecode
records no platform per turn, so platforms groups by model.")

(defun default-hours () (max 1 (option :default-hours 24)))

(defun window-title (window span-word)
  (cond ((string= window "session") "latest session")
        ((string= window "all") "all recorded")
        (span-word (span-title span-word))
        (t (format nil "last ~d hour~:p" (default-hours)))))

(defun window-span (span-word)
  "The seconds a window reads: the section's default hours, or SPAN-WORD."
  (if span-word
      (parse-span span-word)
      (* 3600.0 (default-hours))))

(defun run-command (args session-id)
  "One /edit-log invocation. Answers (VALUES TEXT DIALOG)."
  (declare (ignore session-id))
  (handler-case
      (let* ((words (mapcar #'string-downcase (nlk:split-words (or args ""))))
             (spans (remove-if-not #'parse-span words))
             (recent-p (member "recent" words :test #'string=))
             (counts (and recent-p (remove-if-not #'parse-count words)))
             (known '("session" "all" "day" "clear" "recent"))
             (groups (mapcar #'car +group-words+))
             (unknown (remove-if (lambda (word)
                                   (or (member word known :test #'string=)
                                       (member word groups :test #'string=)
                                       (member word spans :test #'string=)
                                       (member word counts :test #'string=)))
                                 words)))
        (when unknown
          (return-from run-command
            (if (member (first unknown) '("help" "-h" "--help") :test #'string=)
                +help+
                (format nil "Unknown option: ~a~%~%~a" (first unknown) +help+))))
        (when (member "clear" words :test #'string=)
          (return-from run-command
            (format nil "edit-ledger: deleted ~d recorded row(s)."
                    (clear-ledger (ledger-file)))))
        (let* ((window (cond ((member "session" words :test #'string=) "session")
                             ((member "all" words :test #'string=) "all")
                             (t "day")))
               (span-word (first spans))
               (title (window-title window span-word))
               (now (float (nlk:unix-now) 1.0))
               (rows (select-rows (read-ledger (ledger-file)) window now
                                   (window-span span-word))))
          (if recent-p
              (recent-text rows now :title title
                                :limit (if counts (parse-count (first counts)) +recent-rows+))
              (summarize rows now :title title
                              :by (or (cdr (assoc (find-if (lambda (word) (assoc word +group-words+ :test #'string=))
                                                           words)
                                                  +group-words+ :test #'string=))
                                      :model)))))
    (error (condition) (nle:failure (format nil "ERROR: edit-log: ~a" condition)))))

(nle:define-cell edit-ledger
  (:section ("edit-ledger")
    (:guide "nothing has to be set: the cell records by itself; file picks another ledger, default_hours the window /edit-log shows")
    ("file" :path :doc "the ledger file; empty keeps it in the cell's own folder under the home")
    ("keep_rows" :integer :default 5000 :min 1
                 :doc "rows kept once the ledger passes max_bytes")
    ("max_bytes" :integer :default 2000000 :min 1000
                 :doc "the size at which the ledger is trimmed to keep_rows")
    ("default_hours" :integer :default 24 :min 1
                     :doc "the window /edit-log shows when no 6h or 7d is typed"))
  (:help :edit-log "edit-ledger: which models change files, and of which types" +help+)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-edit-ledger"))
  (:command "edit-log" 'run-command
            :description "Which models change files, how many per turn and of which types"
            :argument-hint "[session|all|7d] [types|days|recent [N]] | clear"))
