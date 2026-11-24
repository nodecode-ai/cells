;;;; cell.lisp --- the provider calls that failed: which model, why, when.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What it does. A provider call that fails and is retried, or that makes
;;;; the round move to another model, scrolls past in a status line; a turn
;;;; that finished anyway leaves no trace a person can go back to. Nodecode
;;;; already journals each of them: a `turn.provider_retry' fact per failed
;;;; attempt that will be retried (provider, model, HTTP status, the
;;;; provider's own line, which budget the failure spent), and a
;;;; `turn.provider_fallback' fact when the round moved off a model (the
;;;; classification of the failure, and where it went). This cell reads
;;;; those facts -- it records nothing of its own and starts no thread --
;;;; and folds them into the report the original plugin's /errors printed:
;;;; per model, provider, cause or day; or the newest failures, one line
;;;; each, with how long ago.
;;;;
;;;; What is read. The store's turn.provider_retry and turn.provider_fallback
;;;; facts, and one small file of its own under the home holding the log
;;;; position its last clear hid. No network, no keys, no other file.
;;;;
;;;; Config, the "error-ledger" section:
;;;;   {"window_hours": 24, "recent_rows": 10}
;;;; the bare report's window, and how many of the newest failures the list
;;;; shows. Both are optional; nothing here is required to run.

(defpackage #:nodecode-error-ledger
  (:use #:cl)
  (:nicknames #:error-ledger)
  (:export #:summary #:recent #:failures #:clear #:report))

(in-package #:nodecode-error-ledger)

(nlk:define-peripheral error-ledger :not-running t)

;;; --- what the log already recorded --------------------------------------------

(defparameter +retry-kind+ "turn.provider_retry"
  "One failed provider attempt that will be retried.")

(defparameter +fallback-kind+ "turn.provider_fallback"
  "One failure that moved the round from one model to another.")

(defparameter +day-seconds+ 86400)

(defun column-text (key)
  "The SQL column reading KEY, a payload member, as text (\"\" when absent)."
  (format nil "coalesce(json_extract(payload, '$.\"~a\"'), '')" key))

(defun column-number (key)
  "The SQL column reading KEY, a payload member, as a number (0 when absent)."
  (format nil "coalesce(json_extract(payload, '$.\"~a\"'), 0)" key))

(defparameter +row-columns+
  (list "log_position" "occurred_at" "session_id" "turn_id"
        (column-number "attempt") (column-text "provider") (column-text "model")
        (column-number "status") (column-text "reason") (column-text "detail"))
  "One failed attempt as the log holds it:
(POSITION AT SESSION TURN ATTEMPT PROVIDER MODEL STATUS REASON DETAIL).")

(defvar *failure-reader* nil
  "NIL reads the store; a test binds a function of the same keywords
answering rows in the shape of *ROW-COLUMNS* followed by a kind string,
so nothing here needs a database.")

(defun store-rows (&key session-id)
  "The store's failed-attempt facts, oldest first, optionally one session's,
each as a *ROW-COLUMNS* row followed by \"retry\" or \"fallback\". An
unreadable store reads as none: the ledger is never a turn's precondition."
  (when (nlk:store-open-p)
    (handler-case
        (append
         (loop for row in (nlk:events :kind +retry-kind+ :session-id session-id
                                       :order :oldest :as :rows
                                       :columns (list* (column-text "provider")
                                                       (column-text "model")
                                                       (column-text "scope")
                                                       "log_position" "occurred_at" "session_id"
                                                       "turn_id" (column-number "attempt")
                                                       (column-number "status")
                                                       (column-text "detail")))
               collect (list (nth 3 row) (nth 4 row) (nth 5 row) (nth 6 row) (nth 7 row)
                             (nth 0 row) (nth 1 row) (nth 8 row) (nth 2 row) (nth 9 row)
                             "retry"))
         (loop for row in (nlk:events :kind +fallback-kind+ :session-id session-id
                                       :order :oldest :as :rows
                                       :columns (list* (column-text "from-provider")
                                                       (column-text "from-model")
                                                       (column-text "reason")
                                                       "log_position" "occurred_at" "session_id"
                                                       "turn_id" (column-number "attempt")
                                                       (column-number "status")
                                                       (column-text "detail")))
               collect (list (nth 3 row) (nth 4 row) (nth 5 row) (nth 6 row) (nth 7 row)
                             (nth 0 row) (nth 1 row) (nth 8 row) (nth 2 row) (nth 9 row)
                             "fallback"))
         '())
      (error () '()))))

(defun failure-rows (&key session-id)
  "Every failed attempt this cell can see, SESSION-ID's or the home's."
  (if *failure-reader*
      (funcall *failure-reader* :session-id session-id)
      (store-rows :session-id session-id)))

(defun merge-failures (raw)
  "RAW rows (oldest first) folded into one failure the attempt, not the
fact: a retry fact and the fallback fact of the same attempt are one row,
its REASON the classification the fallback carried and its MOVED true."
  (let ((rows (make-hash-table :test #'equal))
        (order '()))
    (dolist (row raw)
      (destructuring-bind (position at session turn attempt provider model status reason detail kind
                           &aux (fallback (equal kind "fallback")))
          row
        (let* ((key (list session turn attempt))
               (known (gethash key rows)))
          (unless known
            (setf known (list :position position :at at :session session :turn turn
                              :attempt attempt
                              :provider (or provider "") :model (or model "")
                              :status (and status (plusp status) status)
                              :reason (or reason "") :detail (or detail "")
                              :moved fallback)
                  (gethash key rows) known)
            (push key order))
          (when fallback
            (setf (getf known :moved) t)
            (when (plusp (length (or reason "")))
              (setf (getf known :reason) reason))
            (when (> (length (or detail "")) (length (or (getf known :detail) "")))
              (setf (getf known :detail) (or detail "")))
            (when (and (null (getf known :status)) status (plusp status))
              (setf (getf known :status) status))
            (when (and (zerop (length (or (getf known :provider) ""))) provider)
              (setf (getf known :provider) provider))
            (when (and (zerop (length (or (getf known :model) ""))) model)
              (setf (getf known :model) model))))))
    (loop for key in (reverse order) collect (gethash key rows))))

;;; --- its one file: where the last clear cut the ledger -----------------------------

(defun cleared-file ()
  "The file holding the log position this cell's last clear hid."
  (nlk:home "error-ledger/cleared.txt"))

(defun cleared-position ()
  "The log position after which the ledger is shown, or NIL."
  (let ((path (cleared-file)))
    (when (probe-file path)
      (handler-case
          (with-open-file (in path :external-format :utf-8)
            (let ((text (or (read-line in nil "") "")))
              (ignore-errors (parse-integer text))))
        (error () nil)))))

(defun (setf cleared-position) (position)
  (handler-case
      (if (null position)
          (when (probe-file (cleared-file)) (delete-file (cleared-file)))
          (nlk:write-file-atomically (cleared-file) (format nil "~d~%" position)
                                     :mode #o600 :directory-mode #o700))
    (error () nil))
  position)

(defun utc-stamp (universal)
  "UNIVERSAL as the UTC ISO-8601 stamp the event log writes."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.000Z"
            year month date hour minute second)))

(defun local-day (at)
  "AT, a log stamp, as the local calendar day YYYY-MM-DD (\"unknown\" when it
has no usable time)."
  (let ((universal (ignore-errors (nlk:iso-universal at))))
    (if universal
        (multiple-value-bind (second minute hour date month year)
            (decode-universal-time universal)
          (declare (ignore second minute hour))
          (format nil "~4,'0d-~2,'0d-~2,'0d" year month date))
        "unknown")))

;;; --- the window ---------------------------------------------------------------

(defun span-seconds (word)
  "Seconds for a window word such as \"12h\" or \"7d\"; NIL when WORD is not
one. At most four digits, a positive count, h or d."
  (let* ((text (string-downcase (or word "")))
         (last (and (plusp (length text)) (char text (1- (length text)))))
         (number (and last (subseq text 0 (1- (length text))))))
    (when (and last (find last "hd") (plusp (length number)) (<= (length number) 4)
               (every #'digit-char-p number))
      (let ((count (parse-integer number)))
        (when (plusp count)
          (* count (if (char= last #\h) 3600 +day-seconds+)))))))

(defun span-title (word)
  "WORD (a window such as \"7d\") in words: \"last 7 days\"."
  (let* ((text (string-downcase word))
         (count (parse-integer (subseq text 0 (1- (length text)))))
         (unit (if (char= (char text (1- (length text))) #\h) "hour" "day")))
    (format nil "last ~d ~a~:[~;s~]" count unit (/= count 1))))

(defun window-titles ()
  '((:day . "last 24 hours") (:session . "latest session") (:all . "all recorded")))

(defun window-title (window span)
  "What a report over WINDOW (a span, when one was named) is titled."
  (cond ((and span (eq window :day)) (span-title span))
        ((eq window :day) "last 24 hours")
        ((eq window :session) "latest session")
        (t "all recorded")))

(defun window-span (window span)
  "The seconds a report over WINDOW covers."
  (case window
    (:all nil)
    (:session nil)
    (t (or (and span (span-seconds span)) (* 3600 (window-hours))))))

(defun window-hours ()
  "How many hours the bare report covers."
  (let ((hours (ignore-errors (setting :window-hours))))
    (if (and (integerp hours) (plusp hours)) hours 24)))

(defun latest-session (rows)
  "The session of the newest row that carries one (\"\" when none does)."
  (or (loop for row in rows
            for session = (getf row :session)
            when (and session (plusp (length session)))
              do (return session))
      ""))

(defun failures (&key (window :day) span session-id (now (get-universal-time)))
  "The failed attempts WINDOW covers, oldest first, each a plist of
:POSITION :AT :SESSION :TURN :ATTEMPT :PROVIDER :MODEL :STATUS :REASON
:DETAIL :MOVED."
  (let* ((cleared (cleared-position))
         (rows (remove-if (lambda (row)
                            (and cleared (<= (or (getf row :position) 0) cleared)))
                          (if session-id
                              (merge-failures (failure-rows :session-id session-id))
                              (merge-failures (failure-rows)))))
         (spanned (window-span window span)))
    (cond ((eq window :all) rows)
          ((eq window :session)
           (let ((session (latest-session (reverse rows))))
             (if (zerop (length session))
                 '()
                 (remove-if-not (lambda (row) (equal session (getf row :session))) rows))))
          (t (let ((since (utc-stamp (- now spanned))))
               (remove-if-not (lambda (row) (string>= (or (getf row :at) "") since)) rows))))))

;;; --- lines ---------------------------------------------------------------------

(defun age (seconds)
  "SECONDS as the report writes it: 3d ago, 5h ago, 12m ago, 40s ago."
  (let ((seconds (max 0 (round seconds))))
    (cond ((>= seconds +day-seconds+) (format nil "~dd ago" (floor seconds +day-seconds+)))
          ((>= seconds 3600) (format nil "~dh ago" (floor seconds 3600)))
          ((>= seconds 60) (format nil "~dm ago" (floor seconds 60)))
          (t (format nil "~ds ago" seconds)))))

(defun table (header body)
  "HEADER and BODY as aligned lines: the first column left, the rest right."
  (let ((widths (loop for i below (length header)
                      collect (reduce #'max
                                      (mapcar (lambda (row) (length (or (nth i row) "")))
                                              (cons header body))))))
    (loop for row in (cons header body)
          collect (string-right-trim
                   '(#\Space)
                   (format nil "~{~a~^  ~}"
                           (loop for cell in row
                                 for i from 0
                                 for width in widths
                                 collect (let ((text (or cell "")))
                                           (if (zerop i)
                                               (format nil "~va" width text)
                                               (format nil "~v@a" width text)))))))))

(defun top (values)
  "The most common of VALUES with its count, ties to the name that sorts
first; \"-\" when there are none."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (value values) (incf (gethash value counts 0)))
    (let ((best nil) (best-count 0))
      (maphash (lambda (name count)
                 (when (or (> count best-count)
                           (and (= count best-count) best (string< name best)))
                   (setf best name best-count count)))
               counts)
      (if best (format nil "~a (~d)" best best-count) "-"))))

(defun by-label (by)
  "The grouping BY names in the table's first column."
  (case by (:provider "provider") (:reason "reason") (:day "day") (t "model")))

(defun row-value (row by)
  "ROW's value under the grouping BY."
  (case by
    (:day (local-day (getf row :at)))
    (:provider (getf row :provider))
    (:reason (getf row :reason))
    (t (getf row :model))))

(defun ordered-groups (rows by)
  "ROWS grouped by BY, as (NAME . ROWS); the day newest first with the
unknown day last, every other grouping largest first, ties by name."
  (let ((groups (make-hash-table :test #'equal))
        (names '()))
    (dolist (row rows)
      (let ((name (row-value row by)))
        (let ((name (if (and name (plusp (length name))) name "unknown")))
          (unless (gethash name groups)
            (push name names)
            (setf (gethash name groups) '()))
          (push row (gethash name groups)))))
    (let ((pairs (loop for name in names
                       collect (cons name (nreverse (gethash name groups))))))
      (if (eq by :day)
          (sort pairs (lambda (a b)
                        (cond ((equal (car a) "unknown") nil)
                              ((equal (car b) "unknown") t)
                              (t (string> (car a) (car b))))))
          (sort pairs (lambda (a b)
                        (if (= (length (cdr a)) (length (cdr b)))
                            (string< (car a) (car b))
                            (> (length (cdr a)) (length (cdr b))))))))))

(defun summarize (rows &key title (by :model))
  "Per-BY failed attempts: how many, how many moved the round, the commonest
of the other two and the commonest HTTP status."
  (if (null rows)
      (format nil "Provider errors, ~a: none recorded." title)
      (let* ((other (if (eq by :reason) :model :reason))
             (groups (ordered-groups rows by))
             (body (loop for (name . items) in groups
                         collect (list name (princ-to-string (length items))
                                       (princ-to-string (count-if (lambda (row) (getf row :moved)) items))
                                       (top (mapcar (lambda (row)
                                                      (let ((value (row-value row other)))
                                                        (if (and value (plusp (length value))) value "unknown")))
                                                    items))
                                       (top (loop for row in items
                                                  when (getf row :status)
                                                    collect (princ-to-string (getf row :status))))))))
        (when (> (length groups) 1)
          (setf body (append body
                             (list (list "total" (princ-to-string (length rows))
                                         (princ-to-string (count-if (lambda (row) (getf row :moved)) rows))
                                         (top (mapcar (lambda (row)
                                                        (let ((value (row-value row other)))
                                                          (if (and value (plusp (length value))) value "unknown")))
                                                      rows))
                                         (top (loop for row in rows
                                                    when (getf row :status)
                                                      collect (princ-to-string (getf row :status)))))))))
        (format nil "~a~%~%~{~a~%~}~%~a"
                (format nil "Provider errors, ~a:" title)
                (table (list (by-label by) "errors" "moved"
                             (format nil "top ~a" (by-label other)) "top status")
                       body)
                "One row per failed attempt the log recorded; the retry after it may have succeeded."))))

(defun recent (rows &key title (limit 10) (now (get-universal-time)))
  "The newest LIMIT failed attempts of ROWS, newest first, one line each."
  (if (null rows)
      (format nil "Provider errors, ~a: none recorded." title)
      (let* ((newest (last rows (min limit (length rows))))
             (body (loop for row in (reverse newest)
                         for at = (ignore-errors (nlk:iso-universal (getf row :at)))
                         collect (list (if at (age (- now at)) (getf row :at))
                                       (let ((value (getf row :model)))
                                         (if (and value (plusp (length value))) value "unknown"))
                                       (let ((value (getf row :provider)))
                                         (if (and value (plusp (length value))) value "unknown"))
                                       (let ((value (getf row :reason)))
                                         (if (and value (plusp (length value))) value "unknown"))
                                       (if (getf row :status)
                                           (princ-to-string (getf row :status))
                                           "-")
                                       (if (getf row :moved) "yes" "no")))))
        (format nil "~a~%~%~{~a~%~}"
                (format nil "Provider errors, ~a, newest ~d of ~d:" title (length newest) (length rows))
                (table '("when" "model" "provider" "reason" "status" "moved") body)))))

;;; --- the surfaces -----------------------------------------------------------------

(defun report (&key (window :day) span (by :model) (limit nil) session-id (now (get-universal-time)))
  "The report WINDOW covers: grouped BY, or its newest LIMIT failures."
  (let* ((title (window-title window span))
         (rows (failures :window window :span span :session-id session-id :now now)))
    (if limit
        (recent rows :title title :limit limit :now now)
        (summarize rows :title title :by by))))

(defun words-of (text)
  "TEXT as its lowercased words."
  (loop for start = 0 then (1+ end)
        for end = (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline))) text :start start)
        for word = (subseq text start (or end (length text)))
        unless (zerop (length word)) collect (string-downcase word)
        while end))

(defparameter +help-text+
  (format nil "/error-ledger: which provider calls failed, and why~2%~
  /error-ledger             ~a, by model~%~
  /error-ledger session     the most recent session that had a failed call~%~
  /error-ledger all         everything in the log~%~
  /error-ledger 7d          the last 7 days (any number of hours or days: 6h, 30d)~%~
  /error-ledger providers   add to any of the above to group by provider~%~
  /error-ledger reasons     add to any of the above to group by cause~%~
  /error-ledger days        add to any of the above to group by day, newest first~%~
  /error-ledger recent      add to any of the above to list the newest 10 failures instead~%~
  /error-ledger recent 25   the newest 25 instead (1 to 100)~%~
  /error-ledger clear       hide what the log holds so far from the report"
          "last 24 hours")
  "What /error-ledger types alone, and an unknown option, prints.")

(defun recent-limit ()
  "How many of the newest failures a bare `recent' lists."
  (let ((count (ignore-errors (setting :recent-rows))))
    (if (and (integerp count) (plusp count)) (min count 100) 10)))

(defun parse-count (word)
  "A row count for `recent', 1 to 100; NIL when WORD is not one."
  (when (and (plusp (length word)) (<= (length word) 3) (every #'digit-char-p word))
    (let ((count (parse-integer word)))
      (when (<= 1 count 100) count))))

(defparameter +group-words+
  '(("model" . :model) ("models" . :model)
    ("provider" . :provider) ("providers" . :provider)
    ("reason" . :reason) ("reasons" . :reason)
    ("cause" . :reason) ("causes" . :reason)
    ("day" . :day) ("days" . :day))
  "The words that pick a grouping, the original's plural ones and the
singular a tool argument more naturally carries.")

(defun grouping-of (word)
  "The grouping WORD picks, or NIL."
  (cdr (assoc (string-downcase (or word "")) +group-words+ :test #'string=)))

(defun run-slash (args session-id)
  "The /error-ledger command: the same words the original's /errors took."
  (let* ((words (words-of (or args "")))
         (spans (remove-if-not #'span-seconds (mapcar (lambda (word) word) words)))
         (counts (and (member "recent" words :test #'string=)
                      (remove-if-not #'parse-count words)))
         (known '("session" "all" "day" "clear" "recent" "models" "providers" "reasons" "days"))
         (unknown (remove-if (lambda (word)
                               (or (member word known :test #'string=)
                                   (member word spans :test #'string=)
                                   (member word counts :test #'string=)))
                             words)))
    (cond
      ((and unknown (member (first unknown) '("help" "-h" "--help") :test #'string=))
       +help-text+)
      (unknown (format nil "Unknown option: ~a~%~%~a" (first unknown) +help-text+))
      ((member "clear" words :test #'string=)
       (format nil "error-ledger: hid ~d recorded error(s) from the report."
               (clear)))
      (t
       (let* ((window (cond ((member "session" words :test #'string=) :session)
                            ((member "all" words :test #'string=) :all)
                            (t :day)))
              (span (first spans))
              (by (or (loop for word in words
                             for group = (grouping-of word)
                             when group return group)
                      :model))
              (limit (if (member "recent" words :test #'string=)
                         (or (and counts (parse-count (first counts))) (recent-limit))
                         nil)))
         (report :window window :span span :by by :limit limit
                 :session-id (and (eq window :session) session-id)))))))

(defun read-tool (args)
  "error_ledger: the ledger for the model -- grouped, or the newest few."
  (handler-case
      (let* ((window-word (string-downcase (or (nlk:json-value args :string "window") "")))
             (group-word (string-downcase (or (nlk:json-value args :string "by") "")))
             (count (nlk:json-value args :integer "recent"))
             (window (cond ((string= window-word "session") :session)
                           ((string= window-word "all") :all)
                           ((span-seconds window-word) :day)
                           (t :day)))
             (by (or (grouping-of group-word) :model)))
        (report :window window
                :span (and (span-seconds window-word) window-word)
                :by by
                :limit (or (and count (<= 1 count 100) count) nil)
                :session-id (getf (nle:turn) :session-id)))
    (error (condition)
      (format nil "ERROR: the ledger could not be read: ~a" (nlk:one-line (princ-to-string condition))))))

(defun clear ()
  "Hide what the log holds so far from every report. => how many failures
that hid."
  (let* ((rows (merge-failures (failure-rows)))
         (newest (reduce #'max rows :key (lambda (row) (or (getf row :position) 0))
                         :initial-value 0)))
    (setf (cleared-position) newest)
    (count-if (lambda (row) (<= (or (getf row :position) 0) newest)) rows)))

(defparameter +manual+
  "The error ledger lists the provider calls that failed and made Nodecode retry or move to another model.
The log already holds each one; this cell folds them.
  /error-ledger   the last 24 hours by model; add session, all or a window such as 7d, 6h, 30d, and
      providers, reasons or days to group differently, recent [N] for the newest few, clear to hide
      what is recorded so far.
  (error-ledger:report :window :day :by :provider :limit 20)   the same from eval; window is :day,
      :session or :all, span a string such as \"7d\", by :model, :provider, :reason or :day.
  (error-ledger:failures :window :all)   the rows as data, oldest first: :AT :SESSION :PROVIDER
      :MODEL :STATUS :REASON :DETAIL :MOVED.
  (error-ledger:clear)   hide everything recorded so far and answer how many rows that hid.
Reasons are the failure's own classification when the round moved off the model, and the budget the
failure spent (provider_request, provider_stream, provider_contract) when it was retried in place.
Moved says the failure cost the turn its model. Nothing here writes to the log, and nothing starts."
  "What (help :error-ledger) answers while the cell runs.")

(nle:define-cell error-ledger
  (:section ("error-ledger")
    (:guide "nothing here is required; window_hours sets the bare report's window and recent_rows how many of the newest failures the list shows")
    ("window_hours" :integer :default 24 :min 1
     :doc "how many hours the bare report and the tool cover")
    ("recent_rows" :integer :default 10 :min 1
     :doc "how many of the newest failures `recent' lists, 1 to 100"))
  (:tool "error_ledger"
         (list :description "The provider calls that failed and were retried or made the round move: per model, provider, cause or day, or the newest few, with the HTTP status and the failure's own line. Reads the log only."
               :parameters (nlk:json-object
                            "type" "object"
                            "properties" (nlk:json-object
                                          "window" (nlk:json-object
                                                    "type" "string"
                                                    "description" "a window: 24h, 7d, 30d; or session for the latest session that had a failure, or all")
                                          "by" (nlk:json-object
                                                "type" "string"
                                                "description" "grouping: model (default), provider, reason or day")
                                          "recent" (nlk:json-object
                                                    "type" "integer"
                                                    "description" "instead of a table, the newest N failures, 1 to 100"))))
         #'read-tool)
  (:command "error-ledger" 'run-slash
            :description "Which provider calls failed (rate limits, overloads, ...) and why"
            :argument-hint "[session|all|7d] [providers|reasons|days|recent [N]] | clear")
  (:help :error-ledger
         "error-ledger: the provider calls that failed, by model, provider, cause and day - /error-ledger"
         +manual+))
