;;;; ledger.lisp --- the file of mid-turn messages, and the summaries /interim-log prints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A row holds when a model wrote a message beside a tool call, which model
;;;; and lane, how many tool calls rode it, at which step of the turn and how
;;;; long it was; never the text. Sessions and turns are kept as short
;;;; hashes. The file is JSON lines under the home, one row a line, trimmed
;;;; to the section's own bound at the newest end.

(in-package #:nodecode-interim-ledger)

(defparameter +filename+ "interim-ledger/interim.jsonl"
  "Where the ledger is written, under the home.")

(defparameter +unix-epoch+ 2208988800
  "Common Lisp's universal time at 1970-01-01T00:00:00Z: what a unix second
and a universal time differ by.")

(defparameter +day-seconds+ 86400
  "One day in unix seconds.")

(defparameter +recent-rows+ 10
  "How many rows /interim-log recent lists when no count is given.")

(defparameter +max-recent-rows+ 100
  "The most rows /interim-log recent N may ask for.")

(defparameter +model-limit+ 80
  "The longest model name a row keeps.")

(defparameter +provider-limit+ 40
  "The longest provider name a row keeps.")

(defparameter +note+
  "Counted for every turn, whether or not the text was drawn on a surface."
  "The last line of a summary, saying what a row was taken from.")

;;; --- the file ---------------------------------------------------------------------

(defun ledger-path ()
  "The ledger's file, read off the home every time: a profile moves it."
  (nlk:home +filename+))

(defun now ()
  "The current unix second."
  (- (get-universal-time) +unix-epoch+))

(defun universal (seconds)
  "SECONDS, a unix second count, as a universal time."
  (+ (round (or seconds 0)) +unix-epoch+))

(defun tag (id)
  "A short, stable stand-in for a session or turn ID, empty when there is
none: the ledger only needs to tell sessions and turns apart."
  (let ((id (and id (princ-to-string id))))
    (if (and id (plusp (length id))) (nlk:short-digest id 12) "")))

(defun clipped (value limit)
  "VALUE as a string of at most LIMIT characters, empty when it is nothing."
  (let ((text (or value "")))
    (subseq text 0 (min limit (length text)))))

(defun message-text (message)
  "The text MESSAGE carries: a wire content string, or the text parts of a
typed part vector joined. Everything else is empty."
  (let ((content (and (hash-table-p message) (gethash "content" message))))
    (cond ((stringp content) content)
          ((vectorp content)
           (with-output-to-string (out)
             (loop for part across content
                   when (and (hash-table-p part) (equal "text" (gethash "type" part))
                             (stringp (gethash "text" part)))
                     do (write-string (gethash "text" part) out))))
          (t ""))))

(defun message-calls (message)
  "How many tool calls rode MESSAGE: the calls that make its text mid-turn."
  (let ((calls (and (hash-table-p message) (gethash "tool_calls" message))))
    (if (vectorp calls) (length calls) 0)))

(defun turn-facts ()
  "=> (values SESSION-ID TURN-ID STEP) of the turn this thread is working
for, read off (nle:turn); NILs outside one."
  (let ((turn (nle:turn)))
    (values (getf turn :session-id) (getf turn :turn-id) (getf turn :rounds))))

(defun lane-of (session-id)
  "=> (values PROVIDER MODEL): the lane SESSION-ID's newest selection pins,
both NIL when it follows the gateway's."
  (if (and session-id (plusp (length (princ-to-string session-id))))
      (handler-case (nlk:session-model-selection (princ-to-string session-id))
        (error () (values nil nil)))
      (values nil nil)))

(defun build-row (&key session turn provider model step chars calls (ts (now)))
  "One ledger row: when, which session and turn (as tags), which lane, at
which step of the turn, and how long the message was. Never its text."
  (nlk:json-object "ts" (round (or ts 0))
                   "session" (tag session)
                   "turn" (tag turn)
                   "model" (let ((name (clipped model +model-limit+)))
                             (if (plusp (length name)) name "unknown"))
                   "provider" (let ((name (clipped provider +provider-limit+)))
                                (if (plusp (length name)) name "unknown"))
                   "step" (max 0 (or step 0))
                   "chars" (max 0 (or chars 0))
                   "calls" (max 0 (or calls 0))))

(defun rows ()
  "Every parseable row of the ledger, oldest first; a torn line is skipped."
  (nlk:read-json-lines (ledger-path)))

(defun row-ts (row) (or (nlk:json-value row :number "ts") 0))
(defun row-session (row) (or (nlk:json-value row :string "session") ""))
(defun row-turn (row) (or (nlk:json-value row :string "turn") ""))
(defun row-model (row) (or (nlk:json-value row :string "model") "unknown"))
(defun row-provider (row) (or (nlk:json-value row :string "provider") "unknown"))
(defun row-step (row) (or (nlk:json-value row :number "step") 0))
(defun row-chars (row) (or (nlk:json-value row :number "chars") 0))
(defun row-calls (row) (or (nlk:json-value row :number "calls") 0))

(defun trim (path keep)
  "Keep the newest KEEP lines of PATH, rewritten through a temporary file
beside it, so a rewrite is never a half-written ledger."
  (let ((lines (remove "" (nlk:lines (nlk:read-text path)) :test #'string=)))
    (when (> (length lines) keep)
      (let ((temporary (merge-pathnames ".interim.tmp" path)))
        (with-open-file (out temporary :direction :output :if-exists :supersede
                                       :if-does-not-exist :create :external-format :utf-8)
          (dolist (line (last lines keep)) (write-line line out)))
        (rename-file temporary path)))))

(defun append-row (row)
  "Append ROW and keep the file within the section's bound. => ROW."
  (let ((path (ledger-path)))
    (nlk:append-json-line path row)
    (trim path (setting :rows))
    row))

(defun record-message (message)
  "One row for MESSAGE when it is a mid-turn message: text a model wrote
beside at least one tool call. => the row, or NIL when there was nothing to
count."
  (let* ((text (message-text message))
         (calls (message-calls message))
         (chars (length text)))
    (when (and (plusp calls) (plusp chars))
      (multiple-value-bind (session turn step) (turn-facts)
        (multiple-value-bind (provider model) (lane-of session)
          (append-row (build-row :session session :turn turn :provider provider :model model
                                 :step step :chars chars :calls calls)))))))

(defun forget-rows ()
  "Delete the ledger file; => how many rows it held."
  (let* ((path (ledger-path))
         (count (length (rows))))
    (when (probe-file path) (delete-file path))
    count))

;;; --- windows ------------------------------------------------------------------------

(defun parse-span (word)
  "Seconds for a window word such as 12h or 7d; NIL when WORD is not one."
  (let* ((word (string-downcase (princ-to-string (or word ""))))
         (unit (and (plusp (length word)) (char word (1- (length word)))))
         (number (subseq word 0 (max 0 (1- (length word))))))
    (when (and (member unit '(#\h #\d)) (plusp (length number))
               (every #'digit-char-p number) (<= (length number) 4))
      (let ((seconds (* (parse-integer number) (if (char= unit #\h) 3600 +day-seconds+))))
        (and (plusp seconds) seconds)))))

(defun parse-count (word)
  "A row count for recent, from 1 to +MAX-RECENT-ROWS+; NIL when WORD is not one."
  (let ((word (princ-to-string (or word ""))))
    (when (and (plusp (length word)) (<= (length word) 3) (every #'digit-char-p word))
      (let ((count (parse-integer word)))
        (and (<= 1 count +max-recent-rows+) count)))))

(defun span-title (word)
  "The window WORD as the words a report's first line uses."
  (format nil "last ~d ~a~:p" (parse-integer word :end (1- (length word)))
          (if (char-equal #\h (char word (1- (length word)))) "hour" "day")))

(defun last-session (rows)
  "The session tag of the newest row that carries one, or the empty string."
  (let ((found ""))
    (dolist (row rows found)
      (when (plusp (row-session row))
        (setf found (row-session row))))))

(defun select-rows (all window span)
  "The rows of ALL in WINDOW: everything, the latest session's, or the last
SPAN seconds."
  (cond ((string= window "all") all)
        ((string= window "session")
         (let ((session (last-session all)))
           (if (plusp (length session))
               (remove-if-not (lambda (row) (string= session (row-session row))) all)
               '())))
        (t (let ((floor (- (now) span)))
             (remove-if (lambda (row) (< (row-ts row) floor)) all)))))

;;; --- how it reads ------------------------------------------------------------------

(defun age (seconds)
  "SECONDS as one short word: 3d ago, 5h ago, 12m ago, 40s ago."
  (let ((seconds (max 0 (round (or seconds 0)))))
    (cond ((>= seconds +day-seconds+) (format nil "~dd ago" (floor seconds +day-seconds+)))
          ((>= seconds 3600) (format nil "~dh ago" (floor seconds 3600)))
          ((>= seconds 60) (format nil "~dm ago" (floor seconds 60)))
          (t (format nil "~ds ago" seconds)))))

(defun local-day (ts)
  "The local calendar day of a unix TS, as YYYY-MM-DD, or unknown."
  (handler-case
      (multiple-value-bind (second minute hour date month year)
          (decode-universal-time (universal ts))
        (declare (ignore second minute hour))
        (format nil "~4,'0d-~2,'0d-~2,'0d" year month date))
    (error () "unknown")))

(defun median (numbers)
  "The middle of NUMBERS, or the mean of the middle two."
  (let* ((sorted (sort (copy-list numbers) #'<))
         (count (length sorted))
         (middle (floor count 2)))
    (if (oddp count)
        (nth middle sorted)
        (round (/ (+ (nth (1- middle) sorted) (nth middle sorted)) 2)))))

(defun pad (text width &key right)
  "TEXT in WIDTH characters, padded on the right (or the left, with RIGHT)."
  (let* ((text (or text ""))
         (fill (make-string (max 0 (- width (length text))) :initial-element #\Space)))
    (if right (concatenate 'string fill text) (concatenate 'string text fill))))

(defun table (header rows)
  "HEADER and ROWS (lists of strings) as aligned lines: the first column to
the left, every other to the right."
  (let* ((all (cons header rows))
         (widths (loop for column below (length header)
                      collect (loop for row in all
                                    maximize (length (or (nth column row) ""))))))
    (loop for row in all
          collect (string-right-trim
                   '(#\Space)
                   (format nil "~{~a~^  ~}"
                           (loop for cell in row
                                 for width in widths
                                 for first = t then nil
                                 collect (pad cell width :right (not first))))))))

(defun numbered (rows)
  "ROWS as (INDEX . ROW), counted from zero, the way a turn without an id
is told apart."
  (loop for row in rows for index from 0 collect (cons index row)))

(defun turn-key (pair)
  "Which turn the (INDEX . ROW) PAIR belongs to. A row without a turn tag
counts as a turn of its own."
  (let ((turn (row-turn (cdr pair))))
    (if (plusp (length turn))
        (format nil "~a/~a" (row-session (cdr pair)) turn)
        (format nil "#~d" (car pair)))))

(defun group-key (row by)
  "ROW's group under BY: its model, its provider, or its local day."
  (cond ((string= by "day") (local-day (row-ts row)))
        ((string= by "provider") (row-provider row))
        (t (row-model row))))

(defun groups-of (all by)
  "ALL's rows grouped under BY as (NAME . PAIRS), largest group first, or
the newest day first for days; a row whose day is unreadable goes last."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (pair (numbered all)) (push pair (gethash (group-key (cdr pair) by) groups)))
    (let ((named (loop for name being the hash-keys of groups using (hash-value items)
                       collect (cons name (nreverse items)))))
      (if (string= by "day")
          (sort named (lambda (left right)
                        (cond ((string= (car left) "unknown") nil)
                              ((string= (car right) "unknown") t)
                              (t (string> (car left) (car right))))))
          (sort named (lambda (left right)
                        (if (= (length (cdr left)) (length (cdr right)))
                            (string< (car left) (car right))
                            (> (length (cdr left)) (length (cdr right))))))))))

(defun group-row (name pairs now)
  "One summary line: NAME, its messages, the turns they came in, how many a
turn wrote, the median length and when the last one was."
  (let* ((turns (remove-duplicates (mapcar #'turn-key pairs) :test #'string=))
         (last (reduce #'max pairs :key (lambda (pair) (row-ts (cdr pair))))))
    (list name
          (princ-to-string (length pairs))
          (princ-to-string (length turns))
          (format nil "~,1f" (/ (length pairs) (length turns)))
          (princ-to-string (median (mapcar (lambda (pair) (row-chars (cdr pair))) pairs)))
          (age (- now last)))))

(defun summary-table (all by title)
  "ALL's rows as a summary under TITLE grouped by BY: a table, a total line
when there is more than one group, and the note under it."
  (let* ((now (now))
         (groups (groups-of all by))
         (lines (loop for (name . pairs) in groups collect (group-row name pairs now))))
    (when (> (length groups) 1)
      (push (group-row "total" (numbered all) now) lines))
    (format nil "Mid-turn messages, ~a:~%~%~a~%~%~a"
            title
            (format nil "~{~a~^~%~}"
                    (table (list by "messages" "turns" "per turn" "median chars" "last")
                           lines))
            +note+)))

(defun recent-table (all title limit)
  "The newest LIMIT rows of ALL, newest first, one line each: when, which
model, at which step, how long and with how many tool calls."
  (let* ((now (now))
         (newest (reverse (last all limit))))
    (format nil "Mid-turn messages, ~a, newest ~d of ~d:~%~%~{~a~^~%~}"
            title (length newest) (length all)
            (table '("when" "model" "step" "chars" "calls")
                   (loop for row in newest
                         collect (list (age (- now (row-ts row)))
                                       (row-model row)
                                       (princ-to-string (row-step row))
                                       (princ-to-string (row-chars row))
                                       (princ-to-string (row-calls row))))))))

;;; --- the verbs ---------------------------------------------------------------------

(defun summary (&key (window "day") (span +day-seconds+) (by "model")
                   (title "last 24 hours"))
  "The mid-turn messages the ledger holds, per BY (model, provider or day):
how many, in how many turns, how many a turn wrote, their median length and
when the last one was. => the whole table as one string; a window with
nothing in it answers one line."
  (running-settings)
  (let ((selected (select-rows (rows) window span)))
    (if (null selected)
        (format nil "Mid-turn messages, ~a: none recorded." title)
        (summary-table selected by title))))

(defun recent (&key (window "day") (span +day-seconds+) (title "last 24 hours")
                  (limit +recent-rows+))
  "The newest LIMIT mid-turn messages of WINDOW, newest first."
  (running-settings)
  (let ((selected (select-rows (rows) window span)))
    (if (null selected)
        (format nil "Mid-turn messages, ~a: none recorded." title)
        (recent-table selected title limit))))

(defun clear ()
  "Delete the ledger and answer what it held."
  (running-settings)
  (format nil "interim-ledger: deleted ~d recorded message(s)." (forget-rows)))
