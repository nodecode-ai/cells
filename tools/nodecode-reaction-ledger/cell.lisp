;;;; cell.lisp --- the manual, the reaction seam, /reaction-log, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One hook and one command. Every room adapter of the channel kit hands a
;;;; reaction to NCK:REACTION-NOTICED -- the emoji as the candidate's text,
;;;; and the message, chat and room it happened in -- so one advice on that
;;;; function records the reactions of every room the kit serves. The kit
;;;; loads as a cell of its own, so the advice is taken when the function
;;;; exists and a worker keeps looking while it does not. The observer only
;;;; watches: an error of its own is swallowed, and the kit's answer stands.

(in-package #:nodecode-reaction-ledger)

(defparameter +manual+
  "Reaction Ledger keeps one row per reaction someone leaves on a message of yours in a chat
room and answers /reaction-log with which emoji are used, on how many messages, in how many
chats and when last. A Nodecode rewrite of the Hermes plugin reaction-ledger.
  /reaction-log             last 24 hours, by emoji
  /reaction-log all         everything in the ledger
  /reaction-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
  /reaction-log days        add to any of the above to group by day, newest first
  /reaction-log recent      add to any of the above to list the newest 10 reaction changes instead
  /reaction-log recent 25   the newest 25 instead (1 to 100)
  /reaction-log clear       delete the ledger
The rows are also readable through eval, which is what a report to paste elsewhere uses:
  (reaction-ledger:summary :window \"all\" :by \"day\")   the table as a string
  (reaction-ledger:recent :window \"7d\" :limit 25)        the newest reaction changes
  (reaction-ledger:clear)                                   delete the ledger
A row holds the room, the emoji, whether it was set or taken back, when it happened, and a
12-digit hash of the chat and of the message; never the chat or message id themselves, never
who reacted, and never anything said in the chat. Only rooms the channel kit serves are read:
discord, telegram, slack. Neither the ledger nor the summary leaves this machine."
  "What (help :reaction-ledger) answers while the cell runs.")

(defparameter +help-text+
  "/reaction-log: the emoji reactions left on your messages in a chat room

  /reaction-log             last 24 hours, by emoji
  /reaction-log all         everything in the ledger
  /reaction-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
  /reaction-log days        add to any of the above to group by day, newest first
  /reaction-log recent      add to any of the above to list the newest 10 reaction changes instead
  /reaction-log recent 25   the newest 25 instead (1 to 100)
  /reaction-log clear       delete the ledger"
  "What /reaction-log answers when it is typed alone or with an option it does not know.")

(defparameter +group-words+ '(("days" . "day") ("emojis" . "emoji"))
  "The words that choose a grouping, and the grouping each one is.")

(defparameter +windows+ '(("day" . "last 24 hours") ("all" . "all recorded"))
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

(defun run-reaction-log (arguments session-id)
  "/reaction-log's RUN: the summary ARGUMENTS asks for, or one line saying
which option was not understood."
  (declare (ignore session-id))
  (let* ((words (words-of arguments))
         (spans (remove-if-not (lambda (word) (and (parse-span word) t)) words))
         (counts (when (member "recent" words :test #'string=)
                   (remove-if-not (lambda (word) (and (parse-count word) t)) words)))
         (known (append '("all" "day" "clear" "recent")
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
                        "emoji")))
           (if (member "recent" words :test #'string=)
               (recent :window window :span span :title title
                       :limit (if counts (parse-count (first counts)) +recent-rows+))
               (summary :window window :span span :by by :title title)))))))

;;; --- watching the rooms ------------------------------------------------------------

(defparameter +hook-key+ "nodecode-reaction-ledger"
  "The key this cell's advice on the kit's reaction seam is filed under.")

(defparameter +seam+ '("NCK" "REACTION-NOTICED")
  "The package and symbol of the channel kit's reaction seam: a room adapter
hands it every reaction, and the kit loads as a cell of its own, so it is
looked up by name rather than read here.")

(defun seam ()
  "The channel kit's reaction seam as a symbol, or NIL while the kit is absent."
  (let ((package (find-package (first +seam+))))
    (and package (find-symbol (second +seam+) package))))

(defun text-field (object &rest path)
  "PATH read off OBJECT as text: a string as it is, a number as its digits,
and the empty string for anything else."
  (let ((value (apply #'nlk:json-value object :any path)))
    (cond ((stringp value) (nlk:trimmed value))
          ((numberp value) (princ-to-string value))
          (t ""))))

(defun record-candidate (candidate)
  "One row for CANDIDATE, as a room adapter hands it to the kit: the emoji is
its text, and whether it was set or taken back is the source's own word.
=> the row, or NIL when it names no chat or no message."
  (when (hash-table-p candidate)
    (let* ((source (nlk:json-value candidate :object "source"))
           (room (text-field source "platform"))
           (action (text-field source "reaction")))
      (append-change (if (plusp (length room)) room "unknown")
                     (text-field source "channel_id")
                     (text-field source "reply_to_message_id")
                     (text-field candidate "text")
                     (if (string= action "remove") "remove" "add")))))

(defun observe-reaction (next host candidate)
  "The hook on the channel kit's reaction seam: one row per reaction change,
and the kit's own answer, unchanged."
  (handler-case (record-candidate candidate) (error () nil))
  (funcall next host candidate))

(defun hook-seam ()
  "Advise the kit's reaction seam, once the kit is loaded; => T when it is."
  (let ((symbol (seam)))
    (when (and symbol (fboundp symbol))
      (nle:hook symbol +hook-key+ #'observe-reaction)
      t)))

(defun unhook-seam ()
  "Take this cell's advice off the kit's seam, when it is there."
  (let ((symbol (seam)))
    (when symbol (ignore-errors (nle:unhook symbol +hook-key+)))))

(defun hook-step ()
  "The worker's step: stop looking once the seam is advised."
  (when (hook-seam) :stop))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell reaction-ledger
  (:section ("reaction-ledger")
    (:guide "nothing to set up: rows is how many reaction changes the ledger keeps (default 5000); /reaction-log reads it")
    ("rows" :integer :default 5000 :min 50
     :doc "how many reaction changes the ledger keeps, newest first; the oldest rows are dropped"))
  (:help :reaction-ledger
         "Reaction Ledger: the emoji reactions left in your chats, by emoji, chat and day - /reaction-log"
         +manual+)
  (:command "reaction-log" 'run-reaction-log
   :description "The emoji reactions left in your chats: which, on how many messages, when last"
   :argument-hint "[all|7d] [days|recent [N]] | clear")
  (:start (lambda ()
            ;; The kit is a cell of its own and loads in name order, before this
            ;; one; a kit that arrives later is caught by the worker, which
            ;; gives up the moment the seam is advised.
            (nle:on-stop (lambda () (unhook-seam)))
            (let ((worker (nlk:worker-start "reaction-ledger-seam" #'hook-step
                                             :wake 30 :prime t)))
              (nle:on-stop (lambda () (nlk:worker-stop worker)))))))
