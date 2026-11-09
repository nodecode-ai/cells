;;;; cell.lisp --- the manual, the settings, the hook, /approval-log.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Hermes raised an approval prompt and recorded nothing; Nodecode asks its
;;;; operator through one seam, ASK-OPERATOR, and the gates that use it (the
;;;; guard, the block patterns) forget the answer the moment the call decides.
;;;; This cell advises that one seam: every question asked through it becomes
;;;; one row -- the rule that asked, the answer, how it ended, how long a
;;;; person took -- and /approval-log reads the rows back per rule, per
;;;; surface or per day. Observer only: it cannot answer or change a question,
;;;; and the question's detail is left out, because it carries the command.

(in-package #:nodecode-approval-ledger)

;;; --- the manual --------------------------------------------------------------
;;; (help :approval-ledger) answers it while the cell runs; every request
;;; carries the one line the :HELP clause gives, never this text.

(defparameter +manual+
  "Approval-ledger keeps a row for every question the organism asked its operator before it ran
something: which rule asked, where, what the answer was, how long the answer took. One JSON line
per decision, in this home's approval-ledger.jsonl; the question's detail (the command) is not
recorded. Observer only: it cannot answer or change a question, and it has no network.
  /approval-log             the last default_hours (24) by the rule that asked
  /approval-log session     the most recent session that asked
  /approval-log all         everything in the ledger
  /approval-log 7d          the last 7 days (any number of hours or days: 6h, 30d)
  /approval-log surfaces    add to any of the above to group by where it asked
  /approval-log days        add to any of the above to group by day, newest first
  /approval-log recent      add to any of the above to list the newest 10 decisions instead
  /approval-log recent 25   the newest 25 instead (1 to 100)
  /approval-log clear       delete the ledger
From eval, the same over the verbs in the approval-ledger: package:
  (approval-ledger:report &key window by limit)
      the table; WINDOW is \"all\", \"session\", \"6h\", \"7d\" (default \"24h\"), BY is \"rule\",
      \"surface\" or \"day\" (default \"rule\"), and LIMIT turns it into the newest LIMIT rows.
  (approval-ledger:recent &key window limit)   the newest decisions, newest first.
  (approval-ledger:record \"rule\" \"answer\" &key surface wait session-id ts)
      one row, for a decision taken somewhere this cell cannot see.
  (approval-ledger:rows)      every row as a plist, oldest first.
  (approval-ledger:clear)     delete the ledger; answers how many rows it held.
A row's outcome is approved (Allow once, Approve, Yes), denied (Deny, Reject, No), unanswered
(the question timed out or nobody was there: the call did not run) or other. Median wait counts
people's answers only."
  "What (help :approval-ledger) answers while the cell runs.")

;;; --- one question -------------------------------------------------------------

(defun note (rule answer why wait session-id)
  "One row for one question, written to the ledger; never raises."
  (handler-case (append-row (build-row rule answer why wait
                                      :session-id session-id
                                      :surface (setting :surface)))
    (error () nil)))

(defun observe-question (next &rest args)
  "The ASK-OPERATOR advice: ask as usual, and keep one row for the question.
Answers what the question answered, and never changes it."
  (let* ((start (get-internal-real-time))
         (rule (or (and args (stringp (first args)) (first args)) "question"))
         (session-id (and (listp (rest args)) (getf (rest args) :session-id))))
    (multiple-value-bind (answer why)
        (apply next args)
      (when (setting :record)
        (note rule answer why
              (/ (float (- (get-internal-real-time) start) 1d0)
                 internal-time-units-per-second)
              session-id))
      (values answer why))))

;;; --- the windows ---------------------------------------------------------------

(defun window-name (window)
  "All, session, or a span read as the recent-window word the report knows."
  (cond ((string= window "all") "all")
        ((string= window "session") "session")
        (t "day")))

(defun window-title (window)
  "The title and the seconds a WINDOW word stands for."
  (let ((span (parse-span window)))
    (cond ((string= window "all") (values "all recorded" (* 86400 (setting :default-hours))))
          ((string= window "session") (values "latest session" (* 86400 (setting :default-hours))))
          (span (values (span-title window) span))
          (t (let ((hours (setting :default-hours)))
               (values (format nil "last ~d hour~p" hours hours) (* 3600 hours)))))))

(defun words-of (args)
  (let ((text (string-downcase (or args ""))))
    (loop with start = 0
          for space = (position #\space text :start start)
          for word = (subseq text start space)
          when (plusp (length word)) collect word
          while space do (setf start (1+ space)))))

(defun group-word (words)
  (cond ((member "surfaces" words :test #'string=) "surface")
        ((member "days" words :test #'string=) "day")
        (t "rule")))

(defun delete-ledger ()
  "Delete the ledger, and answer how many rows it held."
  (let ((path (home-file))
        (count (length (read-rows))))
    (when (probe-file path) (delete-file path))
    count))

;;; --- /approval-log ---------------------------------------------------------------

(defun option-refusal (word)
  (if (member word '("help" "-h" "--help") :test #'string=)
      +manual+
      (format nil "Unknown option: ~a~%~%~a" word +manual+)))

(defun find-window-words (words)
  "The window words that are not options, and the count after `recent'."
  (let* ((spans (remove-if-not #'parse-span words))
         (counts (and (member "recent" words :test #'string=)
                      (remove-if-not #'parse-count words)))
         (known '("session" "all" "day" "clear" "recent" "surfaces" "days" "patterns"
                  "help" "-h" "--help")))
    (values spans counts known)))

(defun run-approval-log (args session-id)
  "/approval-log: the ledger, read the way the words after it say."
  (declare (ignore session-id))
  (let ((words (words-of args)))
    (multiple-value-bind (spans counts known) (find-window-words words)
      (let ((unknown (remove-if (lambda (word)
                                  (or (member word known :test #'string=)
                                      (parse-span word)
                                      (and counts (parse-count word))))
                                words)))
        (when unknown (return-from run-approval-log (option-refusal (first unknown))))
        (when (member "help" words :test #'string=)
          (return-from run-approval-log +manual+))
        (when (member "help" words :test #'string=)
          (return-from run-approval-log +manual+))
        (when (member "clear" words :test #'string=)
          (return-from run-approval-log
            (format nil "approval-ledger: deleted ~d recorded decision(s)." (delete-ledger))))
        (let ((window (cond ((member "session" words :test #'string=) "session")
                            ((member "all" words :test #'string=) "all")
                            (t "day"))))
          (when (and spans (string= window "day")) (setf window (first spans)))
          (multiple-value-bind (title span) (window-title window)
            (let* ((now (get-universal-time))
                   (chosen (select (read-rows) (window-name window) span now)))
              (if (member "recent" words :test #'string=)
                  (recent-lines chosen :title title :now now
                                :limit (or (and counts (parse-count (first counts))) 10))
                  (summarize chosen :title title :by (group-word words))))))))))

;;; --- the verbs --------------------------------------------------------------------

(define-verb report (&key (window "24h") (by "rule") limit)
  "The ledger as a table: which rules asked, how the answers ended and how long people took.
WINDOW is \"all\", \"session\", \"6h\", \"7d\" (default \"24h\"); BY is \"rule\", \"surface\"
or \"day\"; LIMIT lists the newest LIMIT decisions instead of the table."
  (multiple-value-bind (title span) (window-title window)
    (let* ((now (get-universal-time))
           (chosen (select (read-rows) (window-name window) span now)))
      (if limit
          (recent-lines chosen :title title :limit limit :now now)
          (summarize chosen :title title :by by)))))

(define-verb recent (&key (window "24h") (limit 10))
  "The newest LIMIT decisions of WINDOW, newest first, one line each (LIMIT is 1 to 100)."
  (multiple-value-bind (title span) (window-title window)
    (let ((now (get-universal-time)))
      (recent-lines (select (read-rows) (window-name window) span now)
                    :title title :limit (or limit 10) :now now))))

(define-verb record (rule answer &key surface wait session-id (ts (get-universal-time)))
  "One row for a decision taken where this cell cannot see the question: RULE is the rule that
asked, ANSWER what it was answered, SURFACE where, WAIT how long the answer took."
  (let ((row (append-row (build-row rule answer nil wait
                                    :session-id session-id
                                    :surface (or surface (setting :surface))
                                    :ts ts))))
    (format nil "recorded: ~a ~a (~a)"
            (nlk:json-value row :string "rule")
            (nlk:json-value row :string "answer")
            (nlk:json-value row :string "outcome"))))

(define-verb rows ()
  "Every row the ledger holds, oldest first, as plists: :TS :RULE :SURFACE :SESSION-ID :ANSWER
:OUTCOME :WAIT-S."
  (loop for row in (read-rows)
        collect (list :ts (nlk:json-value row :number "ts")
                      :rule (nlk:json-value row :string "rule")
                      :surface (nlk:json-value row :string "surface")
                      :session-id (nlk:json-value row :string "session_id")
                      :answer (nlk:json-value row :string "answer")
                      :outcome (nlk:json-value row :string "outcome")
                      :wait-s (nlk:json-value row :number "wait_s"))))

(define-verb clear ()
  "Delete the ledger, and answer how many rows it held."
  (format nil "approval-ledger: deleted ~d recorded decision(s)." (delete-ledger)))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell approval-ledger
  (:section ("approval-ledger")
    (:guide "nothing to set: every question the organism asks its operator is recorded in this home's approval-ledger.jsonl; keep_rows, max_bytes and default_hours only tune the trim and the report; file names another path and surface is what the report calls these questions")
    ("file" :string :default ""
     :doc "the ledger file; empty is approval-ledger.jsonl in this home")
    ("keep_rows" :integer :default 5000 :min 1
     :doc "how many of the newest rows a trim keeps")
    ("max_bytes" :integer :default 2000000 :min 64
     :doc "the size past which the ledger is trimmed to keep_rows")
    ("default_hours" :integer :default 24 :min 1
     :doc "the window /approval-log reads with no argument")
    ("record" :boolean :default t
     :doc "record every question the organism asks its operator")
    ("surface" :string :default "operator"
     :doc "what the report calls the surface these questions came from"))
  (:help :approval-ledger "approval-ledger: /approval-log shows which approval questions were asked, how they ended and how long people took; (approval-ledger:report) is the same table from eval" +manual+)
  (:hook 'nle:ask-operator #'observe-question)
  (:command "approval-log" 'run-approval-log
            :description "Which approval questions were asked, how they ended and how long they took"
            :argument-hint "[session|all|7d] [surfaces|days|recent [N]] | clear"))
