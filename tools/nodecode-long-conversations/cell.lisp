;;;; cell.lisp --- handoff briefs that carry a session's state past its context limit.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of cspears32/hermes-long-conversations (see NOTICE). The
;;;; original watched the context window through a Hermes hook and, at a
;;;; watermark, wrote a structured brief -- topic, work in progress,
;;;; decisions, active files, assumptions, open questions, dead ends, next
;;;; step -- which the next session read at its first turn. The brief and
;;;; the way it is rendered into a fresh session are the original's; the
;;;; tools, their JSON answers and the /long-conversations command are Nodecode's
;;;; NLE:DEFINE-CELL registrations.
;;;;
;;;; A brief is one JSON file in the folder the section names, so it
;;;; outlives the process that wrote it and two Nodecode homes never share
;;;; one by accident. A saved brief also leaves a pending marker: the first
;;;; model request of any other session reads it once and the marker goes.

(defpackage #:nodecode-long-conversations
  (:use #:cl)
  (:export #:long-conversations-error #:long-conversations-error-detail))

(in-package #:nodecode-long-conversations)

(nlk:define-peripheral long-conversations :not-running t :lock "long-conversations")

(defparameter *format-version* 1
  "The brief file's own format, so a later one can tell.")

(defconstant +max-topic+ 300 "A topic, in characters: the one line a list shows.")
(defconstant +max-text+ 4000 "One prose field -- work in progress, next step -- in characters.")
(defconstant +max-items+ 40 "How many entries one list field carries.")
(defconstant +max-item+ 500 "One entry of a list field, in characters.")
(defconstant +list-limit+ 200 "The most briefs one handoff_list call returns.")

;;; --- where a brief lives ---------------------------------------------------------

(defun data-folder ()
  "The folder the briefs are kept in, as the section names it."
  (uiop:ensure-directory-pathname (setting :data-dir)))

(defun brief-file (id)
  (merge-pathnames (format nil "~a.json" id) (data-folder)))

(defun pending-file ()
  (merge-pathnames "pending.json" (data-folder)))

(defun brief-id (id)
  "ID as a brief's -- letters, digits, - _ and . -- or a refusal."
  (let ((id (string-downcase (nlk:trimmed (princ-to-string id)))))
    (unless (and (plusp (length id))
                 (<= (length id) 80)
                 (every (lambda (c) (or (alphanumericp c) (find c "-_."))) id))
      (fail "~s is no brief id: use the id handoff_list answered, letters, digits, - _ and . at most 80"
            id))
    id))

(defun new-brief-id ()
  "An id no brief in the folder has yet: the moment, lowercased."
  (let* ((stamp (string-downcase (remove #\: (nlk:iso-time (nlk:unix-now) :millis nil))))
         (base (format nil "brief-~a" stamp)))
    (loop for n from 1
          for candidate = (if (= n 1) base (format nil "~a-~d" base n))
          unless (probe-file (brief-file candidate))
            return candidate)))

;;; --- reading and writing the file ------------------------------------------------

(defun brief-of (object)
  "One brief read back from its file, as a plist."
  (flet ((strings (name)
           (loop for item across (or (nlk:json-array object name) #())
                 for text = (and (stringp item) (nlk:trimmed item))
                 when (plusp (length text)) collect text)))
    (list :format (or (nlk:json-value object :integer "format") *format-version*)
          :id (or (nlk:json-value object :string "id") "")
          :topic (or (nlk:json-value object :string "topic") "")
          :work-in-progress (or (nlk:json-value object :string "workInProgress") "")
          :decisions-made (strings "decisionsMade")
          :active-state-files (strings "activeStateFiles")
          :key-assumptions (strings "keyAssumptions")
          :open-questions (strings "openQuestions")
          :dead-ends (strings "deadEnds")
          :next-step (or (nlk:json-value object :string "nextStep") "")
          :saved-at (or (nlk:json-value object :string "savedAt") ""))))

(defun brief-json (brief)
  "BRIEF as the JSON object the file holds and the tools answer."
  (nlk:json-object
   "format" (getf brief :format *format-version*)
   "id" (getf brief :id)
   "topic" (getf brief :topic)
   "workInProgress" (getf brief :work-in-progress)
   "decisionsMade" (coerce (getf brief :decisions-made) 'vector)
   "activeStateFiles" (coerce (getf brief :active-state-files) 'vector)
   "keyAssumptions" (coerce (getf brief :key-assumptions) 'vector)
   "openQuestions" (coerce (getf brief :open-questions) 'vector)
   "deadEnds" (coerce (getf brief :dead-ends) 'vector)
   "nextStep" (getf brief :next-step)
   "savedAt" (getf brief :saved-at)))

(defun read-brief-file (file)
  "FILE read as a brief, or NIL when it is not one."
  (when (probe-file file)
    (ignore-errors (brief-of (nlk:decode-json (uiop:read-file-string file))))))

(defun read-brief (id)
  "The brief ID as it was saved, or a refusal naming what to call instead."
  (let ((brief (read-brief-file (brief-file (brief-id id)))))
    (unless brief
      (fail "no brief ~a: handoff_list answers the ids this folder holds" (brief-id id)))
    brief))

(defun write-brief (brief)
  "BRIEF to its own file, atomically and under the folder's lock. => the path."
  (let ((file (brief-file (getf brief :id))))
    (ensure-directories-exist file)
    (nlk:with-folder-lock ((data-folder))
      (nlk:write-file-atomically file
                                  (format nil "~a~%" (nlk:encode-json-object (brief-json brief)))
                                  :mode :private))
    file))

(defun brief-records ()
  "Every brief in the folder, newest first."
  (let ((briefs '()))
    (dolist (file (directory (merge-pathnames "*.json" (data-folder))))
      (unless (equal (pathname-name file) "pending")
        (let ((brief (read-brief-file file)))
          (when brief (push brief briefs)))))
    (sort briefs #'string> :key (lambda (brief) (getf brief :saved-at)))))

(defun prune-briefs (&optional (days (setting :keep-days)))
  "Delete the briefs older than DAYS days; the pending marker is not one.
=> how many went."
  (let ((cutoff (- (get-universal-time) (* 86400 days)))
        (pruned 0))
    (dolist (file (directory (merge-pathnames "*.json" (data-folder))))
      (unless (equal (pathname-name file) "pending")
        (let ((written (ignore-errors (file-write-date file))))
          (when (and written (<= written cutoff) (ignore-errors (delete-file file)))
            (incf pruned)))))
    pruned))

;;; --- the pending marker ------------------------------------------------------------

(defun write-pending (id session)
  "Mark brief ID as the one the next session reads, ID having been saved by SESSION."
  (let ((file (pending-file)))
    (ensure-directories-exist file)
    (nlk:with-folder-lock ((data-folder))
      (nlk:write-file-atomically
       file
       (format nil "~a~%"
               (nlk:encode-json-object
                (nlk:json-object "id" id
                                 "session" (or session :null)
                                 "at" (nlk:iso-time (nlk:unix-now) :millis nil))))
       :mode :private))))

(defun read-pending ()
  "The pending marker as (:id ... :session ...), or NIL."
  (let ((file (pending-file)))
    (when (probe-file file)
      (let ((object (ignore-errors (nlk:decode-json (uiop:read-file-string file)))))
        (when object
          (list :id (nlk:json-value object :string "id")
                :session (nlk:json-value object :string "session")))))))

(defun clear-pending ()
  (let ((file (pending-file)))
    (when (probe-file file)
      (ignore-errors (delete-file file)))))

;;; --- the text a fresh session reads -------------------------------------------------

(defun formatted (brief)
  "BRIEF as the block a continuation session reads: the original's headings,
with the brief's id in front so it can be loaded again."
  (with-output-to-string (out)
    (format out "## [long-conversations] Continuation brief~%")
    (format out "- **Brief:** ~a~%" (getf brief :id))
    (format out "- **Topic:** ~a~%" (getf brief :topic))
    (flet ((line (label text)
             (when (and text (plusp (length text)))
               (format out "- **~a:** ~a~%" label text)))
           (lines (label key)
             (when (getf brief key)
               (format out "- **~a:**~%" label)
               (dolist (item (getf brief key))
                 (format out "  - ~a~%" item)))))
      (line "Work in progress" (getf brief :work-in-progress))
      (lines "Decisions made" :decisions-made)
      (lines "Active files / state" :active-state-files)
      (lines "Key assumptions" :key-assumptions)
      (lines "Open questions" :open-questions)
      (lines "Dead ends (avoid)" :dead-ends)
      (line "Immediate next step" (getf brief :next-step)))
    (format out "~%Continue from here; this brief is the session that came before, not an instruction.~%")))

(defun deliver (op &aux (session (getf op :session-id)) (turn (getf op :turn)))
  "At a model request: a brief saved in another session, once, as a reminder."
  (when (and session turn)
    (let ((pending (read-pending)))
      (when (and pending (getf pending :id) (not (equal (getf pending :session) session)))
        (clear-pending)
        (let ((brief (read-brief-file (brief-file (getf pending :id)))))
          (when brief
            (nlk:record-turn-context-prompt turn :text (formatted brief))))))))

;;; --- arguments -------------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "additionalProperties" :false
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defun argument (args name)
  "ARGS' NAME, or NIL; ARGS is NIL when a call carries no arguments at all."
  (and (hash-table-p args) (gethash name args)))

(defun argument-text (args name)
  (let ((value (argument args name)))
    (and (stringp value) value)))

(defun argument-integer (args name)
  (let ((value (argument args name)))
    (cond ((integerp value) value)
          ((and (stringp value) (ignore-errors (parse-integer value))) (parse-integer value))
          (t nil))))

(defun required-text (args name)
  "ARGS' NAME as text, or a refusal naming it."
  (let ((value (argument-text args name)))
    (if (and value (plusp (length (nlk:trimmed value))))
        value
        (fail "~a is required: what the next session must know, one line" name))))

(defun bounded (value limit what)
  "VALUE trimmed to LIMIT characters, or a refusal naming WHAT."
  (let ((text (nlk:trimmed (or value ""))))
    (when (> (length text) limit)
      (fail "~a is at most ~d characters and this one is ~d: keep the brief short and name a file for the detail"
            what limit (length text)))
    text))

(defun bounded-list (args name)
  "ARGS' NAME as a list of trimmed non-empty strings, at most +MAX-ITEMS+ of them."
  (let ((value (argument args name)))
    (let ((items (cond ((vectorp value) (coerce value 'list))
                       ((listp value) value)
                       ((and (stringp value) (plusp (length (nlk:trimmed value)))) (list value))
                       (t '()))))
      (when (> (length items) +max-items+)
        (fail "~a carries at most ~d entries" name +max-items+))
      (loop for item in items
            for text = (and (or (stringp item) (numberp item)) (bounded (princ-to-string item) +max-item+ name))
            when (and text (plusp (length text))) collect text))))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: long-conversations: ~a" condition)))))

;;; --- the tools -------------------------------------------------------------------

(defparameter *save-schema*
  (tool-schema
   "Save a handoff brief before the context window fills or a session ends, so a fresh session can continue: the topic, what is in progress, the decisions already settled, the files in play, the assumptions, the open questions, the dead ends to avoid, and the immediate next step. Keep it small; name files for the detail. A saved brief is read by the next session's first request when the section's pending switch is on."
   '(("topic" "string" "Core subject or objective, one line." t)
     ("work_in_progress" "string" "Where the work stands and what is active now.")
     ("decisions_made" "array" "Choices already settled, one per entry.")
     ("active_state_files" "array" "Files and configs in play.")
     ("key_assumptions" "array" "Working assumptions the next session must keep.")
     ("open_questions" "array" "What is still unresolved.")
     ("dead_ends" "array" "Approaches that failed, so they are not tried again.")
     ("next_step" "string" "The immediate next action.")
     ("id" "string" "Name the brief yourself; without one it is the moment it was saved.")))
  "The handoff_save tool, as the original's HANDOFF_SCHEMA has it.")

(defparameter *load-schema*
  (tool-schema
   "Read a saved handoff brief back as the block a continuation session reads. id is the brief's id, or \"latest\" for the newest one. handoff_list answers the ids."
   '(("id" "string" "The brief's id, from handoff_list or handoff_save, or \"latest\"." t)))
  "The handoff_load tool.")

(defparameter *list-schema*
  (tool-schema
   "List the saved handoff briefs, newest first, with their ids, topics and next steps."
   '(("limit" "integer" "1-200, default 20.")))
  "The handoff_list tool.")

(defparameter *prune-schema*
  (tool-schema
   "Delete the handoff briefs older than a number of days, the way the original's prune_days did. Without the argument the section's keep_days is used."
   '(("older_than_days" "integer" "0 deletes every brief now; default is the section's keep_days.")))
  "The handoff_prune tool.")

(defun tool-save (args)
  "handoff_save: write one brief, and mark it for the next session."
  (answering
   (let* ((topic (bounded (required-text args "topic") +max-topic+ "topic"))
          (brief (list :format *format-version*
                       :id (let ((id (argument-text args "id")))
                             (if (and id (plusp (length (nlk:trimmed id))))
                                 (brief-id id)
                                 (new-brief-id)))
                       :topic topic
                       :work-in-progress (bounded (argument-text args "work_in_progress") +max-text+
                                                   "work_in_progress")
                       :decisions-made (bounded-list args "decisions_made")
                       :active-state-files (bounded-list args "active_state_files")
                       :key-assumptions (bounded-list args "key_assumptions")
                       :open-questions (bounded-list args "open_questions")
                       :dead-ends (bounded-list args "dead_ends")
                       :next-step (bounded (argument-text args "next_step") +max-text+ "next_step")
                       :saved-at (nlk:iso-time (nlk:unix-now) :millis nil))))
     (write-brief brief)
     (when (setting :pending)
       (write-pending (getf brief :id) nlk:*scribe-session-id*))
     (nlk:encode-json-object
      (nlk:json-object
       "success" :true
       "message" (format nil "Brief ~a saved.~@[ The next session reads it at its first request.~]"
                          (getf brief :id) (and (setting :pending) t))
       "id" (getf brief :id)
       "topic" (getf brief :topic)
       "brief" (brief-json brief))))))

(defun tool-load (args)
  "handoff_load: one saved brief, rendered as a continuation block."
  (answering
   (let* ((wanted (required-text args "id"))
          (brief (if (member (string-downcase (nlk:trimmed wanted)) '("latest" "newest") :test #'string=)
                     (or (first (brief-records))
                         (fail "no brief is saved yet: handoff_save writes one"))
                     (read-brief wanted))))
     (nlk:encode-json-object
      (nlk:json-object
       "success" :true
       "id" (getf brief :id)
       "topic" (getf brief :topic)
       "savedAt" (getf brief :saved-at)
       "context" (formatted brief))))))

(defun tool-list (args)
  "handoff_list: the briefs this folder holds, newest first."
  (answering
   (let* ((asked (argument-integer args "limit"))
          (limit (min +list-limit+ (if (and asked (plusp asked)) asked 20)))
          (briefs (brief-records)))
     (nlk:encode-json-object
      (nlk:json-object
       "success" :true
       "count" (length briefs)
       "briefs" (coerce (loop for brief in (subseq briefs 0 (min limit (length briefs)))
                               collect (nlk:json-object
                                        "id" (getf brief :id)
                                        "topic" (getf brief :topic)
                                        "savedAt" (getf brief :saved-at)
                                        "nextStep" (getf brief :next-step)))
                         'vector))))))

(defun tool-prune (args)
  "handoff_prune: delete the briefs older than a day count."
  (answering
   (let* ((asked (argument-integer args "older_than_days"))
          (days (if (and asked (not (minusp asked))) asked (setting :keep-days)))
          (before (length (brief-records))))
     (nlk:encode-json-object
      (nlk:json-object
       "success" :true
       "pruned" (prune-briefs days)
       "kept" (length (brief-records))
       "olderThanDays" days
       "briefsBefore" before)))))

;;; --- /long-conversations --------------------------------------------------------------------

(defun list-text (briefs)
  "BRIEFS as /long-conversations says them, one line each."
  (if briefs
      (format nil "long-conversations: ~d brief~:p in ~a~%~{  ~a~^~%~}"
              (length briefs) (namestring (data-folder))
              (loop for brief in briefs
                    collect (format nil "~a  ~a  (~a)~@[  next: ~a~]"
                                    (getf brief :id) (getf brief :topic) (getf brief :saved-at)
                                    (let ((next (getf brief :next-step)))
                                      (and (plusp (length next)) next)))))
      "long-conversations: no brief is saved yet; ask for one before the context fills, or /long-conversations save TOPIC"))

(defun handoff-panel (briefs)
  "The dialog bare /long-conversations answers: one row per brief, and pruning."
  (nle:list-dialog
   :title "Handoff"
   :summary (if briefs
                (format nil "~d brief~:p" (length briefs))
                "No briefs yet")
   :rows (append
          (loop for brief in (subseq briefs 0 (min 8 (length briefs)))
                collect (nle:run-row (format nil "~a -- ~a" (getf brief :topic) (getf brief :saved-at))
                                     (format nil "/long-conversations show ~a" (getf brief :id))
                                     :detail (or (let ((next (getf brief :next-step)))
                                                   (and (plusp (length next)) next))
                                                 (getf brief :id))))
          (when briefs
            (list (nle:run-row (format nil "Prune briefs older than ~d days" (setting :keep-days))
                               "/long-conversations prune"
                               :detail "the files go; nothing else is touched"))))))

(defun run-command (args session-id)
  "One /long-conversations invocation: no argument the briefs, `show ID` one of them,
`latest` the newest, `prune` the old ones away, `save TOPIC` a brief of your
own words for the model to fill in."
  (declare (ignore session-id))
  (running-settings)
  (let* ((typed (nlk:trimmed (or args "")))
         (space (position #\Space typed))
         (head (string-downcase (subseq typed 0 space)))
         (rest (nlk:trimmed (if space (subseq typed (1+ space)) ""))))
    (cond
      ((zerop (length typed))
       (let ((briefs (brief-records)))
         (values (list-text briefs) (handoff-panel briefs))))
      ((member head '("show" "load") :test #'string=)
       (if (plusp (length rest))
           (formatted (read-brief rest))
           (list-text (brief-records))))
      ((member head '("latest" "newest") :test #'string=)
       (formatted (or (first (brief-records)) (fail "no brief is saved yet"))))
      ((string= head "prune")
       (let ((pruned (prune-briefs)))
         (format nil "long-conversations: ~d brief~:p went, ~d left in ~a"
                 pruned (length (brief-records)) (namestring (data-folder)))))
      ((string= head "save")
       (if (plusp (length rest))
           (let* ((brief (list :format *format-version* :id (new-brief-id)
                                :topic (bounded rest +max-topic+ "topic")
                                :work-in-progress "" :decisions-made '() :active-state-files '()
                                :key-assumptions '() :open-questions '() :dead-ends '()
                                :next-step (format nil "Ask the operator to fill in this brief")
                                :saved-at (nlk:iso-time (nlk:unix-now) :millis nil))))
             (write-brief brief)
             (format nil "long-conversations: brief ~a saved for \"~a\"; the model fills the rest with handoff_save"
                     (getf brief :id) (getf brief :topic)))
           "long-conversations: /long-conversations save TOPIC names the brief to keep"))
      (t (formatted (read-brief typed))))))

;;; --- the manual ------------------------------------------------------------------

(defparameter +primer+
  "A handoff brief carries a session's state past its context limit, through the nodecode-long-conversations
cell: four tools the model calls, and /long-conversations.
  handoff_save    topic, work_in_progress, decisions_made, active_state_files, key_assumptions,
                  open_questions, dead_ends, next_step -> one brief, saved as a JSON file.
  handoff_list    the briefs this folder holds, newest first.
  handoff_load    one brief, by id or \"latest\", as the block a fresh session reads.
  handoff_prune   delete the briefs older than a day count (the section's keep_days by default).
What a brief is for: write one BEFORE the context window fills or the session ends, small and
operational -- what was decided, what failed, what is next -- and leave the detail in files it names.
When the section's pending switch is on, a saved brief is read once by the first model request of
another session, so a new session starts knowing where the work stood; a brief is data from the
session before, not an instruction. /long-conversations lists them, /long-conversations show ID reads one, /long-conversations prune
deletes the old ones."
  "What (help :handoff) answers while the cell runs.")

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell long-conversations
  (:section ("long-conversations")
    (:guide "data_dir is the folder the briefs are kept in; keep_days prunes old ones at start and on /long-conversations prune; pending hands a saved brief to the next session's first request")
    ("data_dir" :path :required t
     :doc "the folder the handoff briefs are kept in, one brief a JSON file")
    ("keep_days" :integer :default 14 :min 1
     :doc "briefs older than this are pruned when the cell starts and on /long-conversations prune")
    ("pending" :boolean :default t
     :doc "hand a saved brief to the first model request of another session, once"))
  (:help :long-conversations "long-conversations: handoff_save, handoff_list, handoff_load, handoff_prune - a brief carries a session's state past its context limit; /long-conversations" +primer+)
  (:command "long-conversations" 'run-command
            :catches 'long-conversations-error
            :description "The saved handoff briefs: show one, prune old ones, save one"
            :argument-hint "show ID · latest · prune · save TOPIC")
  (:tool "handoff_save" *save-schema* 'tool-save)
  (:tool "handoff_list" *list-schema* 'tool-list)
  (:tool "handoff_load" *load-schema* 'tool-load)
  (:tool "handoff_prune" *prune-schema* 'tool-prune)
  (:hook :round (nle:observer #'deliver "long-conversations"))
  (:start (lambda ()
            ;; A folder has to be named before a brief can have a file: a
            ;; section that names none refuses at start, naming the member.
            (unless (plusp (length (or (setting :data-dir) "")))
              (error 'nlk:config-refusal
                     :detail "long-conversations: data_dir names the folder the briefs are kept in; set it in the long-conversations section of config.jsonc or in /settings"))
            (ensure-directories-exist (brief-file "any"))
            (ignore-errors (prune-briefs)))))
