;;;; cell.lisp --- every (sh ...) this agent ran, with its verdict, in one tray.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; WHAT IT IS. A rewrite of aydnOktay's hermes-command-tray: the list an
;;;; operator reads when the question is "what did the agent actually run, and
;;;; did it work". In Hermes the seam was a post_tool_call hook on the
;;;; `terminal' tool; Nodecode's shell is EVAL's SH, so the seam here is one
;;;; piece of advice around NODECODE.EVOLVED:SH. The cell sees the command,
;;;; the exit status SH answers and the directory the call named, and never
;;;; runs anything itself: no process, no network, no key.
;;;;
;;;; WHAT A ROW IS. One unique command, newest last: the command text, its
;;;; status and exit code, the directory, the last non-empty line of its
;;;; output (preview_chars, 160 by default), when it last ran and how many
;;;; times it ran. Token-shaped strings are redacted before anything is
;;;; written. At most max_items rows; the same command again updates its row
;;;; in place and counts a run. The rows are a JSON file under the home
;;;; (command-tray/state.json), so a restart does not lose them.
;;;;
;;;; WHAT IT ANSWERS. command_tray_list and command_tray_clear, the two
;;;; tools the plugin registered; /commands in every shell; (help
;;;; :command-tray); and the verbs (command-tray:runs) and
;;;; (command-tray:clear) through eval.

(in-package #:nodecode-command-tray)

;;; --- the words ------------------------------------------------------------------
;;; Data, so a layer can reword them.

(defparameter +primer+
  "Every shell command this agent ran, with the verdict it ended on, is in the
command tray as it runs (the eval verb (sh ...)), through the nodecode-command-tray
cell.
  command_tray_list    the rows: the command, ok or fail with its exit code, the
      directory, the last line of its output and how many times it ran. Ask when
      the operator asks what you ran, or wants to reuse a command; it runs nothing.
  command_tray_clear   empty it, when the operator wants a fresh list for a new
      topic.
  /commands            the same list in a shell; /commands clear empties it.
Newest is last, at most command-tray.max_items rows; the same command again
updates its row instead of adding one. Token-shaped strings in a command or in
the kept output line are redacted. The rows outlive the session: they are a file
under the home, so a restart keeps them."
  "What (help :command-tray) answers while the cell runs.")

(defparameter *empty* "(the command tray is empty)"
  "The list's answer with no rows.")

(defparameter *usage*
  "Usage:
  /commands          — the commands this session ran, with their verdicts
  /commands list     — same
  /commands clear    — empty the tray"
  "/commands with no argument, or one it does not know.")

(defparameter *redacted* "[redacted]"
  "What a token-shaped string is replaced with before anything is written.")

;;; --- what is redacted, and how much is kept -------------------------------------

(defparameter *secret-patterns*
  (list "ghp_[A-Za-z0-9]{20,}"
        "github_pat_[A-Za-z0-9_]{20,}"
        "sk-[A-Za-z0-9]{16,}"
        "AKIA[0-9A-Z]{16}"
        ;; the bearer header before the assignment: `Authorization: Bearer x'
        ;; read the other way round leaves x standing.
        "(?i)bearer\\s+[A-Za-z0-9._-]+"
        "(?i)(?:api[_-]?key|secret|token|password|passwd|authorization)\\s*[:=]\\s*\\S+")
  "The shapes of a credential the plugin redacted, in order: GitHub tokens, an
OpenAI-shaped key, an AWS key id, a `key = value' assignment, and a bearer
header. A row keeps a command and one line of output, and either can carry one.")

(defun redact (text)
  "TEXT with every token-shaped string replaced by *REDACTED*."
  (reduce (lambda (text pattern) (ppcre:regex-replace-all pattern text *redacted*))
          *secret-patterns* :initial-value (or text "")))

(defun clamp (text limit)
  "TEXT cut to LIMIT characters, an ellipsis standing for what was dropped."
  (cond ((null text) "")
        ((<= (length text) limit) text)
        ((zerop limit) "")
        ((= limit 1) "…")
        (t (concatenate 'string (subseq text 0 (1- limit)) "…"))))

(defun one-line (text)
  "TEXT with every run of whitespace collapsed to one space, trimmed."
  (nlk:trimmed (ppcre:regex-replace-all "\\s+" (or text "") " ")))

(defun last-line (text)
  "The last non-empty line of TEXT, whitespace collapsed: what a row keeps of
an output, where the reason a command failed usually is."
  (or (loop for line in (uiop:split-string (or text "") :separator '(#\Newline))
            for text = (one-line line)
            when (plusp (length text)) collect text into kept
            finally (return (car (last kept))))
      ""))

;;; --- where a row lives ------------------------------------------------------------

(defun state-path ()
  "The tray's one file, under the home: (home \"command-tray/state.json\")."
  (nlk:home "command-tray/state.json"))

(defvar *items* nil
  "The rows, oldest first, each a JSON object as the file holds it. Written
under *LOCK* and read the same way, so the advice, a tool and a stop never
see a half-built list.")

(defun read-state ()
  "The rows the file holds, or none: a file that is not there, or is not the
shape this cell writes, is an empty tray and never a failed start."
  (let ((path (state-path)))
    (when (probe-file path)
      (handler-case
          (let* ((raw (nlk:decode-json (uiop:read-file-string path) :whole t))
                 (rows (nlk:json-value raw :array "items")))
            (coerce (remove-if-not #'hash-table-p
                                   (coerce rows 'list))
                    'list))
        (error () nil)))))

(defun state-json ()
  "The rows as the file's one value."
  (nlk:make-json-object "items" (coerce *items* 'vector)))

(defun write-state ()
  "Write the rows where READ-STATE finds them: a temporary file beside it,
then one rename, so a reader sees the old list or the new one."
  (let* ((path (state-path))
         (tmp (merge-pathnames "state.json.tmp" (uiop:pathname-directory-pathname path))))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string (nlk:encode-json-object (state-json)) out))
    (uiop:rename-file-overwriting-target tmp path)))

;;; --- one row out of one run -----------------------------------------------------

(defun row-command (row) (nlk:json-value row :string "command"))
(defun row-runs (row) (or (nlk:json-value row :integer "runs") 1))

(defun command-text (command)
  "COMMAND as a row keeps it: an argv list joined, whitespace collapsed,
redacted, at most 500 characters."
  (let ((text (cond ((stringp command) command)
                     ((listp command)
                      (format nil "~{~a~^ ~}" (mapcar (lambda (word) (princ-to-string word))
                                                       command)))
                     ((null command) "")
                     (t (princ-to-string command)))))
    (clamp (redact (one-line text)) 500)))

(defun status-of (exit-code)
  (cond ((null exit-code) "unknown")
        ((eql exit-code 0) "ok")
        (t "fail")))

(defun record-run (command output exit-code directory)
  "Put one finished run in the tray under the running settings, and write the
file. The same command again moves to the end with runs counted, so the list
is the newest 40 commands this session ran."
  (let ((text (command-text command)))
    (when (plusp (length text))
      (let* ((max-items (setting :max-items))
             (preview-chars (setting :preview-chars))
             (workdir (clamp (one-line (if directory (princ-to-string directory) "")) 300))
             (preview (clamp (last-line (if (setting :redact) (redact output) output))
                             preview-chars))
             (row nil))
        (with-command-tray-lock
          (let* ((old (find text *items* :key #'row-command :test #'equal))
                 (rest (remove text *items* :key #'row-command :test #'equal))
                 (all nil))
            (setf row (nlk:make-json-object
                       "tool" "sh"
                       "command" text
                       "exit_code" (or exit-code -1)
                       "status" (status-of exit-code)
                       "workdir" workdir
                       "preview" preview
                       "runs" (if old (1+ (row-runs old)) 1)
                       "ts" (nlk:iso-now)))
            (setf all (append rest (list row)))
            (when (> (length all) max-items)
              (setf all (last all max-items)))
            (setf *items* all)
            (write-state)))
        row))))

;;; --- the verbs -------------------------------------------------------------------

(define-verb runs ()
  "The tray's rows, oldest first, each a JSON object holding the command, its
status and exit code, the directory, the kept output line, when it last ran
and how many times."
  (with-command-tray-lock (copy-list *items*)))

(define-verb clear ()
  "Empty the tray, and the file with it. Answers how many rows went."
  (let ((gone (with-command-tray-lock
                (prog1 (length *items*)
                  (setf *items* nil)
                  (write-state)))))
    (format nil "~d command~:p cleared from the tray" gone)))

;;; --- the answers a model reads ---------------------------------------------------

(defun rows-text (rows)
  "ROWS as the numbered list a model or an operator reads: the command, its
verdict with the exit code, the directory when the call named one, and the
kept output line under it."
  (if (null rows)
      *empty*
      (format nil "~{~a~^~%~}"
              (loop for row in rows
                    for i from 1
                    for preview = (nlk:json-value row :string "preview")
                    for workdir = (nlk:json-value row :string "workdir")
                    for runs = (row-runs row)
                    collect (format nil "~d. ~a — ~a (~a)~@[ ×~a~]~@[~%   ~a~]~@[~%   in ~a~]"
                                    i (row-command row)
                                    (nlk:json-value row :string "status")
                                    (nlk:json-value row :integer "exit_code")
                                    (and (> runs 1) runs)
                                    (and preview (plusp (length preview)) preview)
                                    (and workdir (plusp (length workdir)) workdir))))))

(nlk:define-contract tray-list-arguments
  (:doc "List the shell commands this agent ran in this session (success and failure), newest last, with the exit code, the directory and the last line of output each kept. Use it when the operator asks what commands were run, or wants to reuse one. It runs nothing.")
  ("markdown" :boolean :default t
              :doc "true (the default) answers the numbered list; false answers the same rows as JSON, for a caller that wants to read a field"))

(defparameter +clear-schema+
  ;; A contract with no members types nothing, so this one is the bare object
  ;; schema a call with nothing to say arrives as.
  (list :description "Empty the command tray, when the operator wants a fresh command list for a new topic; it runs nothing and touches no file but the tray's own."
        :parameters (nlk:make-json-object
                     "type" "object"
                     "properties" (nlk:make-json-object)
                     "required" (vector)))
  "COMMAND_TRAY_CLEAR's schema: an object with no members.")

(defun tray-list (arguments)
  "COMMAND_TRAY_LIST: the tray, as text, or as JSON when the call says
markdown is false."
  (if (tray-list-arguments-markdown arguments)
      (rows-text (runs))
      (nlk:encode-json-object
       (nlk:make-json-object "count" (length (runs))
                              "items" (coerce (runs) 'vector)))))

(defun tray-clear (arguments)
  "COMMAND_TRAY_CLEAR: the tray emptied."
  (declare (ignore arguments))
  (clear))

;;; --- one /commands ---------------------------------------------------------------

(defun run-command (args session-id)
  "The /commands entry point. With no argument, or `list', the tray; with
`clear', emptied; anything else, the usage."
  (declare (ignore session-id))
  (let* ((words (uiop:split-string (or args "") :separator '(#\Space #\Tab)))
         (words (remove "" words :test #'equal))
         (verb (string-downcase (or (first words) "list"))))
    (cond ((member verb '("list" "show" "md" "markdown") :test #'equal) (rows-text (runs)))
          ((member verb '("clear" "reset") :test #'equal) (clear))
          (t *usage*))))

;;; --- the seam: every (sh ...) eval runs lands here --------------------------------

(defvar *last-record-error* nil
  "The condition the last row that could not be written raised, for (cells) to read.")

(defun note-shell-run (next &rest args)
  "The advice around NODECODE.EVOLVED:SH: run the command through NEXT, then
put the run in the tray with the exit status SH answers. A tray that cannot
record a row never fails the command it watched, and never changes its answer."
  (let ((answer (multiple-value-list (apply next args))))
    (handler-case
        (record-run (first args) (first answer) (second answer)
                    (getf (cdr args) :directory))
      (error (condition) (setf *last-record-error* condition)))
    (values-list answer)))

(defun shell-watchable-p ()
  "Whether the shell verb this cell advises is there to advise: a build
without EVAL has no tray to fill."
  (and (find-package '#:nodecode.evolved)
       (fboundp 'nodecode.evolved:sh)))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell command-tray
  (:section ("command-tray")
    (:guide "nothing here is required: the tray fills itself from every shell command the agent runs; max_items bounds the list, preview_chars how much of an output line a row keeps")
    ("max_items" :integer :default 40 :min 1
                 :doc "rows kept; the same command again updates its row instead of adding one")
    ("preview_chars" :integer :default 160 :min 0
                     :doc "characters of the last output line a row keeps, 0 for none")
    ("redact" :boolean :default t
              :doc "mask token-shaped strings in a command and in the kept output line"))
  (:tool "command_tray_list" (nlk:contract 'tray-list-arguments) 'tray-list)
  (:tool "command_tray_clear" +clear-schema+ 'tray-clear)
  (:help :command-tray "Command tray: the shell commands this agent ran, with exit codes" +primer+)
  (:command "commands" 'run-command
            :description "The shell commands this agent ran, with their verdicts")
  (:hook 'nodecode.evolved:sh #'note-shell-run)
  (:start (lambda ()
            (unless (shell-watchable-p)
              (fail "no (sh ...) in this image to watch: the tray needs EVAL's shell verb"))
            (with-command-tray-lock (setf *items* (read-state))))))
