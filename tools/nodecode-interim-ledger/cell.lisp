;;;; cell.lisp --- the manual, the one observer, /interim-log, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One function hook and one command. Every completed provider round of
;;;; every turn is recorded through NLK:RECORD-ASSISTANT-MESSAGE, and a round
;;;; whose message carried text beside at least one tool call is a mid-turn
;;;; message: the "let me check the config first" a model says on its way to a
;;;; tool. The observer only watches -- an error of its own is swallowed, and
;;;; the round is recorded as it would be without the cell.

(in-package #:nodecode-interim-ledger)

(defparameter +manual+
  "Interim Ledger keeps one row per message a model writes in the middle of a turn -- the text
that comes with a tool call, before its final answer -- and answers /interim-log with which
models write them, how many per turn and how long they are. A Nodecode rewrite of the Hermes
plugin interim-ledger.
  /interim-log             last 24 hours, by model
  /interim-log session     the most recent session that wrote one
  /interim-log all         everything in the ledger
  /interim-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
  /interim-log providers   add to any of the above to group by provider (or days)
  /interim-log recent      add to any of the above to list the newest 10 messages instead
  /interim-log recent 25   the newest 25 instead (1 to 100)
  /interim-log clear       delete the ledger
A summary counts messages, the turns they came in, how many a turn wrote, their median length in
characters and when the last one was; the newest list adds the step of the turn and how many
tool calls rode the message.
The rows are also readable through eval, which is what a report to paste elsewhere uses:
  (interim-ledger:summary :window \"all\" :by \"provider\")   the table as a string
  (interim-ledger:recent :window \"7d\" :limit 25)              the newest messages
  (interim-ledger:clear)                                        delete the ledger
A row holds the model and provider, the step of the turn, the length of the message and how
many tool calls rode it, and 12-digit hashes of the session and the turn; never the text and
never the ids themselves. A model that answers without writing anything first leaves no row, and
neither does a final answer. Nothing leaves this machine."
  "What (help :interim-ledger) answers while the cell runs.")

;;; --- watching --------------------------------------------------------------------------

(defun observe-message (next turn message &rest keys)
  "The advice on RECORD-ASSISTANT-MESSAGE: one row for a message that carried
text beside a tool call, then the message recorded exactly as it would be
without the cell."
  (handler-case (record-message message)
    (error () nil))
  (apply next turn message keys))

;;; --- /interim-log ------------------------------------------------------------------------

(defparameter +help-text+
  "/interim-log: which models write messages in the middle of a turn, and how many

  /interim-log             last 24 hours, by model
  /interim-log session     the most recent session that wrote one
  /interim-log all         everything in the ledger
  /interim-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
  /interim-log providers   add to any of the above to group by provider (or days)
  /interim-log recent      add to any of the above to list the newest 10 messages instead
  /interim-log recent 25   the newest 25 instead (1 to 100)
  /interim-log clear       delete the ledger"
  "What /interim-log answers when it is typed alone or with an option it does not know.")

(defparameter +group-words+ '(("models" . "model") ("providers" . "provider") ("days" . "day"))
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

(defun run-interim-log (arguments session-id)
  "/interim-log's RUN: the summary or the newest list ARGUMENTS asks for, one
line saying which option was not understood, or the help alone."
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
                        "model")))
           (if (member "recent" words :test #'string=)
               (recent :window window :span span :title title
                       :limit (if counts (parse-count (first counts)) +recent-rows+))
               (summary :window window :span span :by by :title title)))))))

;;; --- the declaration ---------------------------------------------------------------------

(nle:define-cell interim-ledger
  (:section ("interim-ledger")
    (:guide "nothing to set up: rows is how many messages the ledger keeps (default 5000); /interim-log reads it")
    ("rows" :integer :default 5000 :min 50
     :doc "how many messages the ledger keeps, newest first; the oldest rows are dropped"))
  (:help :interim-ledger
         "Interim Ledger: which models write messages mid-turn, how many per turn - /interim-log"
         +manual+)
  (:command "interim-log" 'run-interim-log
   :description "Which models write messages in the middle of a turn, and how many")
  (:hook 'nlk:record-assistant-message #'observe-message))
