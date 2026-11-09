;;;; cell.lisp --- tripwire: one line for every refused call, and a digest.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a plugin for another agent harness: it listens for a
;;;; blocked tool and for a denied approval, writes one line about each to a
;;;; shared file, and a scheduled script reads the new lines and sends the
;;;; owner one short alert. This cell is that record on the seam Nodecode
;;;; already has:
;;;;
;;;;   :TOOL  one tool dispatch, in the around shape. A call another cell or
;;;;          plugin stopped answers a failure string ("ERROR: refused by ...",
;;;;          "the operator was asked and chose Deny"), and a call that raised
;;;;          is "ERROR: <type>: <message>"; either is one row -- who, which
;;;;          tool, how, why in one line, and when. Never the arguments, the
;;;;          result or the message text, which is the original's own rule.
;;;;          The one time this cell stops a call itself is the drill
;;;;          (drill_tool), which is how the alarm is proved end to end.
;;;;
;;;;   /tripwire and the tripwire_digest tool are the notifier: the rows
;;;;   since the last check, spelled out, so a cron job can deliver them and
;;;;   answer nothing when nothing was refused.
;;;;
;;;; Nothing here can fail a turn: the hook watches inside HANDLER-CASE and
;;;; answers what the call answered, and a log that cannot be written is one
;;;; warning, which is the original's fail-open posture.

(in-package #:nodecode-tripwire)

(nlk:define-peripheral tripwire :not-running t)

(defparameter +log-file+ "tripwire/events.jsonl"
  "The log under the Nodecode home, until events_file names another.")
(defparameter +mark-file+ "digest-mark.txt"
  "Beside the log: how many of its lines the last digest already read.")
(defparameter +usage+ "/tripwire [peek | path]"
  "What the command answers when a word is unknown.")

(defvar *lock* (bt2:make-lock :name "tripwire")
  "One lock over the log's appends and its digest mark: the hook runs on the
turn worker and a command runs on the shell's thread.")

(defvar *session* (lambda () (getf (nle:turn) :session-id))
  "How the cell reads the session a call runs in. A test binds its own.")

(defun option (key fallback)
  "KEY's setting, FALLBACK while the cell is stopped."
  (let ((value (ignore-errors (setting key))))
    (if (null value) fallback value)))

;;; --- where the log lives ---------------------------------------------------

(defun events-path ()
  "The log this cell writes: events_file, or tripwire/events.jsonl under the
Nodecode home."
  (pathname (or (ignore-errors (setting :events-file)) (nlk:home +log-file+))))

(defun digest-mark-path (path)
  "The mark that says how much of the log the last digest read: a sibling of
the log, so one folder holds both."
  (merge-pathnames +mark-file+ (uiop:pathname-directory-pathname (pathname path))))

(defun digest-rows (&optional (path (events-path)))
  "The log's rows, oldest first. A torn or unreadable line is skipped."
  (loop for row in (or (ignore-errors (nlk:read-json-lines path)) '())
        collect (nlk:json-plist row)))

(defun read-mark (path)
  "How many lines of PATH the last digest read; 0 when there is no mark, and
a mark past the end of a shortened log reads as its end."
  (let ((text (ignore-errors (nlk:read-text (digest-mark-path path)))))
    (or (and text (ignore-errors (parse-integer (nlk:trimmed text) :junk-allowed t)))
        0)))

(defun write-mark (path count)
  (ignore-errors
    (let ((file (digest-mark-path path)))
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :if-exists :supersede
                                 :if-does-not-exist :create :external-format :utf-8)
        (format out "~d~%" count)))))

;;; --- one row ---------------------------------------------------------------

(defun ignored-p (tool)
  "Whether TOOL's refusals are housekeeping, by the section's ignore list."
  (let ((name (string-downcase (or tool ""))))
    (dolist (entry (option :ignore '()) nil)
      (when (and (stringp entry) (string-equal name (string-downcase entry)))
        (return t)))))

(defun record! (tool text session how)
  "Append one row, unless TOOL is ignored. A log that cannot be written is
one warning: the call it watched is never the loser."
  (handler-case
      (unless (ignored-p tool)
        (let ((event (refusal-event tool text session how nil)))
          (bt2:with-lock-held (*lock*)
            (nlk:append-json-line (events-path) (nlk:plist-json event)))
          event))
    (error (condition)
      (warn "tripwire: ~a" condition)
      nil)))

(defun note-result (tool text session)
  "Row TEXT, a finished call's result, when it is one a tripwire records:
a call another cell stopped, or, with record_errors, any failed call."
  (handler-case
      (let ((how (refusal-how text)))
        (cond (how (record! tool text session how))
              ((and (option :record-errors nil) (error-result-p text))
               (record! tool text session "error"))
              (t nil)))
    (error (condition)
      (warn "tripwire: ~a" condition)
      nil)))

;;; --- the drill, and the one hook -------------------------------------------

(defun drill-p (tool)
  "Whether TOOL is the one the drill refuses, to prove the alarm end to end."
  (let ((named (nlk:trimmed (or (option :drill-tool "") ""))))
    (and (plusp (length named)) (string-equal named (or tool "")))))

(defun drill-text (tool)
  "What a call the drill refuses answers: a failure the model reads plainly."
  (format nil "ERROR: refused by tripwire (drill): ~a is stopped to prove the alarm; clear drill_tool in the tripwire section to turn it off" tool))

(defun watch (op next)
  "The :TOOL point: the drill stops its one tool; every other call runs, and
what it answered is read for a refusal."
  (let* ((tool (getf op :name))
         (session (ignore-errors (funcall *session*))))
    (if (ignore-errors (drill-p tool))
        (let ((text (drill-text tool)))
          (record! tool text session "drill")
          (nle:failure text))
        (let ((text (funcall next op)))
          (note-result tool text session)
          text))))

;;; --- the digest ------------------------------------------------------------

(defun digest (&key peek)
  "The alert text: the rows since the last check, or, with PEEK, the newest
ones without marking them read."
  (let* ((path (events-path))
         (rows (digest-rows path))
         (total (length rows))
         (from (if peek (max 0 (- total +digest-lines+))
                   (min (read-mark path) total)))
         (fresh (nthcdr from rows)))
    (when (and (not peek) fresh)
      (bt2:with-lock-held (*lock*) (write-mark path total)))
    (digest-body fresh total :peek peek)))

(defun run-slash (args session-id)
  "One /tripwire invocation: the digest, the newest rows, or where the log is."
  (declare (ignore session-id))
  (let ((head (string-downcase (or (first (nlk:split-words (or args ""))) ""))))
    (cond ((member head '("" "digest" "now") :test #'string=) (digest))
          ((string= head "peek") (digest :peek t))
          ((string= head "path")
           (format nil "tripwire: refusals are written to ~a; the digest's mark is ~a"
                   (events-path) (digest-mark-path (events-path))))
          (t (format nil "tripwire: unknown subcommand ~a; usage ~a" head +usage+)))))

(nlk:define-contract digest-request
  (:doc "Read the tripwire log: which tool calls an agent was refused since
the last check, so the operator can be told. Reads a local file; no network.")
  ("verb" :text :required t :options '("digest" "peek")
   :doc "digest answers what is new and marks it read; peek answers the newest rows without marking them"))

(defun digest-tool (request)
  "The tripwire_digest tool: the alert text, what a cron job delivers."
  (if (equal "peek" (digest-request-verb request))
      (digest :peek t)
      (digest)))

(defparameter +manual+
  "The tripwire cell writes one line for every call an agent was refused: a tool stopped by a guard or another cell, an approval the operator denied or let time out, and, with record_errors on, any call that failed. A line holds the time, the session, the tool, how it was stopped and the first line of the reason -- never the arguments, the result or the message text. /tripwire answers what is new since the last check (and marks it read, so a cron job can deliver it once); /tripwire peek answers the newest rows without marking them; /tripwire path says where the log is. The tripwire_digest tool is the same digest for a model to call. drill_tool refuses that one tool, to prove the alarm end to end. The log is tripwire/events.jsonl under the Nodecode home until events_file names another."
  "What (help :tripwire) answers while the cell runs.")

;;; --- the one declaration ---------------------------------------------------

(nle:define-cell tripwire
  (:section ("tripwire")
    (:guide "nothing here is required: the log is tripwire/events.jsonl under the home, and drill_tool empty means the cell only watches; drill_tool refuses one tool to prove the alarm")
    ("events_file" :path
     :doc "where refusals are written; empty means tripwire/events.jsonl under the Nodecode home")
    ("drill_tool" :string :default ""
     :doc "refuse this one tool, to prove the alarm end to end; empty means the cell only watches")
    ("ignore" :list
     :doc "tools whose failures are housekeeping and never a tripwire")
    ("record_errors" :boolean :default nil
     :doc "record a call that failed on its own too, not only one a cell stopped"))
  (:hook :tool #'watch)
  (:command "tripwire" 'run-slash
            :description "What was refused since the last check"
            :argument-hint "peek | path")
  (:help :tripwire "tripwire: one line for every refused tool call, and a digest of what is new" +manual+)
  (:tool "tripwire_digest" (nlk:contract 'digest-request) 'digest-tool)
  (:start
   (lambda ()
     (ignore-errors (ensure-directories-exist (events-path)))
     (let ((drill (nlk:trimmed (or (option :drill-tool "") ""))))
       (when (plusp (length drill))
         (nle:notice (format nil "tripwire: the drill is on, ~a is refused until drill_tool is cleared" drill)
                     :level :warning :key "nodecode-tripwire")
         (nle:on-stop (lambda () (nle:notice nil :key "nodecode-tripwire"))))))))
