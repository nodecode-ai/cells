;;;; cell.lisp --- the manual, the two observers, /command-log, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two function hooks and one command. A slash line run anywhere in the
;;;; image passes through NODECODE.EVOLVED:SLASH (eval, a room, the web
;;;; page's POST /api/gateway/slash) or through
;;;; NODECODE.EVOLVED:EXECUTE-SLASH-COMMAND (a command typed in the
;;;; terminal), and each run becomes one row in the ledger. The observers
;;;; only watch: an error of theirs is swallowed, and the command runs as it
;;;; would without the cell.

(in-package #:nodecode-command-ledger)

(defparameter +manual+
  "Command Ledger keeps one row per slash command you run and answers /command-log with which
commands you use, how often, in how many sessions, and when last. A Nodecode rewrite of the
Hermes plugin command-ledger.
  /command-log             last 24 hours, by command
  /command-log session     the most recent session that ran one
  /command-log all         everything in the ledger
  /command-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
  /command-log surfaces    add to any of the above to group by surface (cli, gateway)
  /command-log days        add to any of the above to group by day, newest first
  /command-log recent      add to any of the above to list the newest 10 commands instead
  /command-log recent 25   the newest 25 instead (1 to 100)
  /command-log clear       delete the ledger
The rows are also readable through eval, which is what a report to paste elsewhere uses:
  (cmdlog:summary :window \"all\" :by \"surface\")   the table as a string
  (cmdlog:recent :window \"7d\" :limit 25)             the newest commands
  (cmdlog:clear)                                       delete the ledger
A row holds the command name, the surface it was typed on, when it ran, and a 12-digit hash of
the session; never the arguments typed after the command and never the session id itself.
/command-log itself is not counted, and neither the ledger nor the summary leaves this machine."
  "What (help :command-ledger) answers while the cell runs.")

;;; --- watching --------------------------------------------------------------------------

(defun model-session (model)
  "The session MODEL, a shell's chat model, is showing, else the session this
evaluation runs for, else \"\"."
  (or (and model (ignore-errors (funcall 'nodecode.evolved::chat-model-session-id model)))
      nlk:*scribe-session-id*
      ""))

(defun record-command (command session surface)
  "Record COMMAND run in SESSION on SURFACE, unless there is nothing to name."
  (when (plusp (length command))
    (append-row command :session session :surface surface)))

(defun observe-slash (next line &rest arguments)
  "The hook on SLASH: one row for every line run where no shell is typing it
-- eval, a room, the web page's POST /api/gateway/slash -- and the line run
as it would be without the cell."
  (handler-case
      (unless (string-equal "command-log" (command-name line))
        (record-command (command-name line) (getf arguments :session-id) "gateway"))
    (error () nil))
  (apply next line arguments))

(defun observe-typed (next model name arguments)
  "The hook on EXECUTE-SLASH-COMMAND: one row for every command typed in the
terminal, and the command run as it would be without the cell."
  (handler-case
      (let ((command (command-name (princ-to-string name))))
        (unless (string-equal "command-log" command)
          (record-command (princ-to-string name) (model-session model) "cli")))
    (error () nil))
  (funcall next model name arguments))

;;; --- /command-log ------------------------------------------------------------------------

(defparameter +help-text+
  "/command-log: which slash commands you run, and how often

  /command-log             last 24 hours, by command
  /command-log session     the most recent session that ran one
  /command-log all         everything in the ledger
  /command-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
  /command-log surfaces    add to any of the above to group by surface (cli, gateway)
  /command-log days        add to any of the above to group by day, newest first
  /command-log recent      add to any of the above to list the newest 10 commands instead
  /command-log recent 25   the newest 25 instead (1 to 100)
  /command-log clear       delete the ledger"
  "What /command-log answers when it is typed alone or with an option it does not know.")

(defparameter +group-words+ '(("commands" . "command") ("surfaces" . "surface") ("days" . "day"))
  "The words that choose a grouping, and the grouping each one is.")

(defparameter +windows+ '(("day" . "last 24 hours") ("session" . "latest session")
                          ("all" . "all recorded"))
  "The windows a line may choose, and the title each one prints.")

(defun words-of (arguments)
  "ARGUMENTS, a command's argument string, as its lowercased words."
  (let ((words '()) (start 0))
    (loop for at = (position-if (lambda (char) (member char '(#\Space #\Tab))) arguments
                                :start start)
          do (when (> (or at (length arguments)) start)
               (push (string-downcase (subseq arguments start (or at (length arguments)))) words))
          while at
          do (setf start (1+ at)))
    (nreverse words)))

(defun run-command-log (arguments session-id)
  "/command-log's RUN: the summary ARGUMENTS asks for, or one line saying
which option was not understood."
  (declare (ignore session-id))
  (let* ((words (words-of arguments))
         (spans (remove-if-not (lambda (word) (and (parse-span word) t)) words))
         (counts (when (member "recent" words :test #'string=)
                   (remove-if-not (lambda (word) (and (parse-count word) t)) words)))
         (known (append '("session" "all" "day" "clear" "recent")
                        (mapcar #'car +group-words+)))
         (unknown (remove-if (lambda (word)
                               (or (member word known :test #'string=)
                                   (member word spans :test #'string=)
                                   (member word counts :test #'string=)))
                             words)))
    (cond
      (unknown (if (member (first unknown) '("help" "-h" "--help") :test #'string=)
                   +help-text+
                   (format nil "Unknown option: ~a~%~%~a" (first unknown) +help-text+)))
      ((member "clear" words :test #'string=) (clear))
      (t (let* ((window (or (car (find-if (lambda (pair) (member (car pair) words :test #'string=))
                                          +windows+))
                            "day"))
                (title (if (and (string= window "day") spans)
                           (span-title (first spans))
                           (cdr (assoc window +windows+ :test #'string=))))
                (span (if (and (string= window "day") spans)
                          (parse-span (first spans))
                          +day-seconds+))
                (by (or (cdr (assoc (find-if (lambda (word)
                                               (assoc word +group-words+ :test #'string=))
                                             words)
                                     +group-words+ :test #'string=))
                        "command")))
           (if (member "recent" words :test #'string=)
               (recent :window window :span span :title title
                       :limit (if counts (parse-count (first counts)) +recent-rows+))
               (summary :window window :span span :by by :title title)))))))

;;; --- the declaration ---------------------------------------------------------------------

(nle:define-cell command-ledger
  (:section ("command-ledger")
    (:guide "nothing to set up: rows is how many commands the ledger keeps (default 5000); /command-log reads it")
    ("rows" :integer :default 5000 :min 50
     :doc "how many commands the ledger keeps, newest first; the oldest rows are dropped"))
  (:help :command-ledger
         "Command Ledger: which slash commands you run, how often and when last - /command-log"
         +manual+)
  (:command "command-log" 'run-command-log
   :description "Which slash commands you run, how often and when last")
  (:hook 'nodecode.evolved::slash #'observe-slash)
  (:hook 'nodecode.evolved::execute-slash-command #'observe-typed))
