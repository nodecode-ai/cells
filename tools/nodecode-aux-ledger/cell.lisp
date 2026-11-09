;;;; cell.lisp --- one row per auxiliary call, /aux, the tool and the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nodecode makes two sorts of model request: the turn the operator asked
;;;; for, and the side calls a session does not ask for in so many words -- a
;;;; title, a note, a classification. Side work goes through NLE:COMPLETE on
;;;; the auxiliary target, and only there, so one advice on that function sees
;;;; every one of them. This cell records each one as a line of a local JSONL
;;;; ledger and prints what they cost.

(in-package #:nodecode-aux-ledger)

(defparameter +primer+
  "Auxiliary LLM calls (titles, notes, classifications) recorded one line each in a local JSONL ledger with their model, provider, retries, wall time and token counts, and printed by /aux. Every verb here answers a string. (aux-ledger:report \"7d providers\") reads the last seven days split by provider; (aux-ledger:report \"recent 25\") lists the newest 25 calls; (aux-ledger:clear) empties the ledger. Wrap a side call in (aux-ledger:with-task \"title\") so the ledger groups it under a name rather than 'side'. The ledger holds identifiers and counts only: never the instruction, the message or the answer.")

(defparameter +help+
  (format nil "/aux: what the auxiliary (side) LLM calls cost~%~%

  /aux             last 24 hours, by task~%
  /aux session     the most recent session that made a side call~%
  /aux all         everything in the ledger~%
  /aux 7d          the last 7 days (any number of hours or days: 6h, 30d)~%
  /aux models      add to any of the above to group by model instead of task~%
  /aux providers   add to any of the above to group by provider instead of task~%
  /aux days        add to any of the above to group by day, newest first~%
  /aux recent      add to any of the above to list the newest 10 calls instead~%
  /aux recent 25   the newest 25 instead (1 to 100)~%
  /aux clear       delete the ledger~%"))

;;; --- settings ---------------------------------------------------------------

(defun ledger-path ()
  "The file the `aux-ledger. section names, absolute when it can be read as a folder-relative one."
  (let ((file (setting :file)))
    (cond ((pathnamep file) file)
          ((and (stringp file) (plusp (length (nlk:trimmed file))))
           (let ((path (uiop:parse-native-namestring (nlk:trimmed file))))
             (if (uiop:absolute-pathname-p path)
                 path
                 (uiop:merge-pathnames* path (uiop:getcwd)))))
          (t (uiop:parse-native-namestring "calls.jsonl")))))

;;; --- one auxiliary call, as it happens ---------------------------------------

(defstruct (call-record (:copier nil))
  "The auxiliary call this thread is inside: what it is for, and what it cost."
  (task "side")
  (session-id "")
  (provider "")
  (model "")
  (started 0)
  (attempts 0)
  (usage nil))

(defvar *task* "side"
  "The name the ledger gives the side calls made on this thread, 'side' by default.")

(defvar *side-call* nil
  "The CALL-RECORD of the auxiliary call this thread is inside, or NIL outside one.")

(defvar *write-failure-said* nil
  "Whether a write that failed was already said, so a broken folder is not a flood.")

(defmacro with-task (label &body body)
  "Name the side calls BODY makes, so /aux groups them under LABEL rather than 'side'."
  `(let ((*task* ,label)) ,@body))

(defun side-target (session-id)
  "The provider and model the auxiliary target resolves to for SESSION-ID."
  (handler-case (nle:auxiliary-model session-id)
    (error () (values "" ""))))

(defun usage-count (usage slot)
  "One token count of USAGE (a provider's usage record), NIL when it did not say."
  (let ((value (and usage (ignore-errors (slot-value usage slot)))))
    (and (integerp value) (plusp value) value)))

(defun usage-tokens (usage)
  "USAGE as the token plist a row keeps, or NIL when the provider reported none."
  (when usage
    (list :input-tokens (usage-count usage 'nodecode.evolved::input-tokens)
          :output-tokens (usage-count usage 'nodecode.evolved::output-tokens)
          :cached-input-tokens (usage-count usage 'nodecode.evolved::cached-input-tokens)
          :cache-write-tokens (usage-count usage 'nodecode.evolved::cache-write-tokens)
          :reasoning-tokens (usage-count usage 'nodecode.evolved::reasoning-tokens))))

(defun condition-type (condition)
  "CONDITION's class name, lowercased: the ledger keeps the kind, never the message."
  (and condition (string-downcase (string (class-name (class-of condition))))))

(defun elapsed-seconds (started)
  (/ (float (- (get-internal-real-time) started) 1.0)
     (float internal-time-units-per-second 1.0)))

(defun call-record-row (record condition)
  "RECORD as the row it leaves: identifiers, timings, counts, and the kind of failure."
  (let ((usage (call-record-usage record)))
    (make-row :ts (get-universal-time)
              :session-id (call-record-session-id record)
              :task (call-record-task record)
              :provider (call-record-provider record)
              :model (call-record-model record)
              :retry-count (max 0 (1- (call-record-attempts record)))
              :duration-s (elapsed-seconds (call-record-started record))
              :error-type (condition-type condition)
              :usage-p (not (null usage))
              :tokens (usage-tokens usage))))

(defun note-call (record condition)
  "Append RECORD's row, saying a write that failed rather than taking the turn with it."
  (handler-case
      (append-row (ledger-path) (call-record-row record condition) (setting :max-rows))
    (error (failure)
      (unless *write-failure-said*
        (setf *write-failure-said* t)
        (nle:notice (format nil "aux ledger: a call could not be recorded in ~a: ~a"
                            (ledger-path) failure)
                    :level :warning)))))

(defun observe-side-call (next &rest args)
  "Record one auxiliary call: NLE:COMPLETE's advice, and the whole point of the cell."
  (if (not (setting :record))
      (apply next args)
      (let ((record (make-call-record :task *task*
                                      :session-id (or (getf (cddr args) :session-id) "")
                                      :started (get-internal-real-time))))
        (multiple-value-bind (provider model) (side-target (call-record-session-id record))
          (setf (call-record-provider record) (or provider "")
                (call-record-model record) (or model "")))
        (let ((*side-call* record))
          (handler-case (prog1 (apply next args)
                          (note-call record nil))
            (error (condition)
              (note-call record condition)
              (error condition)))))))

(defun observe-provider-call (next &rest args)
  "Capture the attempt count and the usage of the auxiliary call this thread is inside."
  (let ((record *side-call*))
    (if (null record)
        (apply next args)
        (let ((answer (multiple-value-list (apply next args))))
          (setf (call-record-attempts record) (1+ (call-record-attempts record)))
          (when (second answer) (setf (call-record-usage record) (second answer)))
          (values-list answer)))))

;;; --- /aux -------------------------------------------------------------------

(defparameter +windows+ '("session" "all" "day"))

(defparameter +group-words+ '(("models" . "model") ("providers" . "provider") ("days" . "day")))

(defparameter +known-words+
  (append '("session" "all" "clear" "day" "recent") (mapcar #'car +group-words+)))

(defun words-of (text)
  (remove "" (uiop:split-string (string-downcase (or text ""))
                                  :separator '(#\Space #\Tab #\Newline))
          :test #'equal))

(defun unknown-word (words recent-p)
  (find-if (lambda (word)
             (not (or (member word +known-words+ :test #'equal)
                      (parse-span word)
                      (and recent-p (parse-count word)))))
           words))

(defun by-word (words)
  (or (loop for (word . group) in +group-words+
            when (member word words :test #'equal)
              return group)
      "task"))

(define-verb report (text)
  "The /aux report for TEXT's option words: a window, a grouping, recent N, or clear."
  (let* ((words (words-of text))
         (recent-p (member "recent" words :test #'equal))
         (unknown (unknown-word words recent-p)))
    (when unknown
      (return-from report
        (if (member unknown '("help" "-h" "--help") :test #'equal)
            +help+
            (format nil "Unknown option: ~a~%~%~a" unknown +help+))))
    (when (member "clear" words :test #'equal)
      (return-from report
        (format nil "aux-ledger: deleted ~d recorded call(s)." (clear-file (ledger-path)))))
    (let* ((window (cond ((member "session" words :test #'equal) "session")
                         ((member "all" words :test #'equal) "all")
                         (t "day")))
           (span-word (and (equal window "day")
                           (find-if (lambda (word) (and (parse-span word) t)) words)))
           (span (or (and span-word (parse-span span-word)) +day-seconds+))
           (now (get-universal-time))
           (rows (select-rows (read-rows (ledger-path)) window now span))
           (title (title-of window span-word)))
      (if recent-p
          (recent-text rows now title
                       (or (some #'parse-count words) +recent-rows+))
          (summarize-text rows title (by-word words))))))

(define-verb clear ()
  "Empty the ledger, answering how many calls it held."
  (format nil "aux-ledger: deleted ~d recorded call(s)." (clear-file (ledger-path))))

(defun run-slash (args session-id)
  "/aux [session|all|7d] [models|providers|days|recent [N]] | clear"
  (declare (ignore session-id))
  (report args))

;;; --- the tool ----------------------------------------------------------------

(nlk:define-contract aux-ledger-query
  (:doc "What Nodecode's auxiliary (side) LLM calls cost: the titles, notes and classifications a session did not ask for in so many words. Answers one row per task, model, provider or day with its call count, failures, tokens and wall time, or the newest calls one line each. The ledger holds identifiers and counts only, never what was asked or answered.")
  ("window" :string :default "24h"
           :doc "24h, all, session, or hours or days back such as 6h or 7d")
  ("group_by" :string :options '("task" "model" "provider" "day") :default "task"
             :doc "what one table row is: a task name, a model, a provider, or a calendar day")
  ("recent" :integer :default 0
           :doc "list the newest N calls instead of a summary; 0 for the summary"))

(defun query-tool (call)
  "The aux_ledger tool: the report CALL's members ask for."
  (let* ((window (nlk:trimmed (aux-ledger-query-window call)))
         (group (aux-ledger-query-group-by call))
         (recent (aux-ledger-query-recent call))
         (words (list (if (plusp (length window)) window "24h")
                      (car (rassoc group +group-words+ :test #'equal))
                      (and (plusp recent) "recent")
                      (and (plusp recent) (princ-to-string recent)))))
    (report (format nil "~{~a~^ ~}" (remove nil words)))))

;;; --- the declaration ---------------------------------------------------------

(nle:define-cell aux-ledger
  (:section ("aux-ledger")
    (:guide "file is the JSONL ledger: one line per auxiliary (side) call; nothing else needs setting -- record turns the writing off without deleting what is there")
    ("file" :path :required t
            :doc "the JSONL file the ledger is written to, one line per auxiliary call")
    ("record" :boolean :default t
              :doc "record each auxiliary (side) call as it is made")
    ("max_rows" :integer :default 5000 :min 10
                :doc "rows kept when the file is trimmed, its newest"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((file (getf values :file)))
                 (unless (and file (plusp (length (nlk:trimmed (princ-to-string file)))))
                   (nlk:config-error "aux-ledger.file names the JSONL ledger the auxiliary calls are written to; set it to a file this machine can write")))
               values))
  (:hook 'nle:complete #'observe-side-call)
  (:hook 'nle::call-provider #'observe-provider-call)
  (:tool "aux_ledger" (nlk:contract 'aux-ledger-query) 'query-tool)
  (:command "aux" 'run-slash
            :catches 'aux-ledger-error
            :description "What the auxiliary (side) LLM calls cost"
            :argument-hint "[session|all|7d] [models|providers|days|recent [N]] | clear")
  (:help :aux "aux: what the auxiliary (side) LLM calls cost - (aux-ledger:report \"7d providers\")" +primer+))
