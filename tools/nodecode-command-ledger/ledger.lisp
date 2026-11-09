;;;; ledger.lisp --- the file of commands run, and the summaries /command-log prints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A row holds which command ran, where it was typed, and a short hash of
;;;; the session it ran in; never the command's arguments and never the
;;;; session id itself. The file is JSON lines under the home, one row a
;;;; line, trimmed to the section's own bound at the newest end.

(in-package #:nodecode-command-ledger)

(defparameter +filename+ "command-ledger/commands.jsonl"
  "Where the ledger is written, under the home.")

(defparameter +unix-epoch+ 2208988800
  "Common Lisp's universal time at 1970-01-01T00:00:00Z: what a unix second
and a universal time differ by.")

(defparameter +day-seconds+ 86400
  "One day in unix seconds.")

(defparameter +recent-rows+ 10
  "How many rows /command-log recent lists when no count is given.")

(defparameter +max-recent-rows+ 100
  "The most rows /command-log recent N may ask for.")

(defparameter +text-limit+ 60
  "The longest command name a row keeps.")

(defparameter +surface-note+
  "The cli surface is a command typed in the terminal; gateway is one run by eval, a room or the web page."
  "The last line of a summary, saying what the surface column tells apart.")

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

(defun session-tag (session-id)
  "A short, stable stand-in for SESSION-ID, empty when there is none: a
session id names a person's chat on a channel, and the ledger only needs to
tell sessions apart."
  (let ((id (and session-id (princ-to-string session-id))))
    (if (and id (plusp (length id))) (nlk:short-digest id 12) "")))

(defun command-name (line)
  "The command LINE names: its first word, without a leading slash."
  (let* ((text (string-left-trim '(#\Space #\Tab) (princ-to-string (or line ""))))
         (text (if (and (plusp (length text)) (char= #\/ (char text 0))) (subseq text 1) text))
         (space (position-if (lambda (char) (member char '(#\Space #\Tab))) text)))
    (subseq text 0 (min +text-limit+ (or space (length text))))))

(defun rows ()
  "Every parseable row of the ledger, oldest first; a torn line is skipped."
  (nlk:read-json-lines (ledger-path)))

(defun row-command (row) (nlk:json-value row :string "command"))
(defun row-session (row) (nlk:json-value row :string "session"))
(defun row-surface (row) (nlk:json-value row :string "surface"))
(defun row-ts (row) (or (nlk:json-value row :number "ts") 0))

(defun trim (path keep)
  "Keep the newest KEEP lines of PATH, rewritten through a temporary file
beside it, so a rewrite is never a half-written ledger."
  (let ((lines (remove "" (nlk:lines (nlk:read-text path)) :test #'string=)))
    (when (> (length lines) keep)
      (let ((temporary (merge-pathnames ".commands.tmp" path)))
        (with-open-file (out temporary :direction :output :if-exists :supersede
                                       :if-does-not-exist :create :external-format :utf-8)
          (dolist (line (last lines keep)) (write-line line out)))
        (rename-file temporary path)))))

(defun append-row (command &key session surface)
  "Append one row for COMMAND, run in SESSION on SURFACE, and keep the file
within the section's bound. => the row."
  (let ((row (nlk:json-object "ts" (now)
                               "command" (if (plusp (length command)) command "unknown")
                               "session" (session-tag session)
                               "surface" (or surface "unknown")))
        (path (ledger-path)))
    (nlk:append-json-line path row)
    (trim path (setting :rows))
    row))

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
      (when (plusp (length (or (row-session row) "")))
        (setf found (row-session row))))))

(defun select-rows (all window span)
  "The rows of ALL in WINDOW: everything, the latest session's, or the last
SPAN seconds."
  (cond ((string= window "all") all)
        ((string= window "session")
         (let ((session (last-session all)))
           (if (plusp (length session))
               (remove-if-not (lambda (row) (string= session (or (row-session row) ""))) all)
               '())))
        (t (let ((floor (- (now) span)))
             (remove-if (lambda (row) (< (row-ts row) floor)) all)))))

(defun session-count (rows)
  "How many distinct sessions ROWS were run in; an unknown one is no session."
  (length (remove-duplicates (remove "" rows :key #'row-session :test #'string=)
                              :key #'row-session :test #'string=)))

;;; --- how it reads ------------------------------------------------------------------

(defun age (seconds)
  "SECONDS as one short word: 3d ago, 5h ago, 12m ago, 40s ago."
  (let ((seconds (max 0 (round (or seconds 0)))))
    (cond ((>= seconds +day-seconds+) (format nil "~dd ago" (floor seconds +day-seconds+)))
          ((>= seconds 3600) (format nil "~dh ago" (floor seconds 3600)))
          ((>= seconds 60) (format nil "~dm ago" (floor seconds 60)))
          (t (format nil "~ds ago" seconds)))))

(defun local-day (ts)
  "The local calendar day of a unix TS, as YYYY-MM-DD."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (universal ts))
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month date)))

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

(defun group-key (row by)
  "ROW's group under BY: the command with its slash, the surface, or the day."
  (cond ((string= by "day") (local-day (row-ts row)))
        ((string= by "surface")
         (let ((surface (row-surface row)))
           (if (and surface (plusp (length surface))) surface "unknown")))
        (t (format nil "/~a" (let ((command (row-command row)))
                                 (if (and command (plusp (length command))) command "unknown"))))))

(defun group-row (name items total now)
  "One summary line: NAME, its uses, its share of TOTAL, its sessions, when last."
  (list name
        (princ-to-string (length items))
        (format nil "~d%" (round (* 100 (/ (length items) total))))
        (princ-to-string (session-count items))
        (age (- now (reduce #'max items :key #'row-ts)))))

(defun groups-of (all by)
  "ALL's rows grouped under BY as (NAME . ROWS), largest group first, or the
newest day first for days; a row whose day is unreadable goes last."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (row all) (push row (gethash (group-key row by) groups)))
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

(defun summary-table (all by title)
  "ALL's rows as a summary under TITLE grouped by BY: a table, a total line
when there is more than one group, and the note under it."
  (let* ((now (now))
         (groups (groups-of all by))
         (lines (loop for (name . items) in groups
                      collect (group-row name items (length all) now))))
    (when (> (length groups) 1)
      (push (group-row "total" all (length all) now) lines))
    (format nil "~a~%~%~a~%~%~a"
            (format nil "Slash commands, ~a:" title)
            (format nil "~{~a~^~%~}"
                    (table (list (if (string= by "command") "command" by)
                                 "uses" "share" "sessions" "last used")
                           lines))
            +surface-note+)))

(defun recent-table (all title limit)
  "The newest LIMIT rows of ALL, newest first, one line each: when, which
command, and the surface it was typed on."
  (let ((now (now))
        (newest (reverse (last all limit))))
    (format nil "Slash commands, ~a, newest ~d of ~d:~%~%~{~a~^~%~}"
            title (length newest) (length all)
            (table '("when" "command" "surface")
                   (loop for row in newest
                         collect (list (age (- now (row-ts row)))
                                       (format nil "/~a" (or (row-command row) "unknown"))
                                       (or (row-surface row) "unknown")))))))

;;; --- the verbs ---------------------------------------------------------------------

(defun summary (&key (window "day") (span +day-seconds+) (by "command")
                   (title "last 24 hours"))
  "The slash commands the ledger holds, per BY: uses, their share, in how
many sessions, and when last. => the whole table as one string; a window
with nothing in it answers one line."
  (running-settings)
  (let ((selected (select-rows (rows) window span)))
    (if (null selected)
        (format nil "Slash commands, ~a: none recorded." title)
        (summary-table selected by title))))

(defun recent (&key (window "day") (span +day-seconds+) (title "last 24 hours")
                  (limit +recent-rows+))
  "The newest LIMIT commands of WINDOW, newest first, with the surface each
was typed on."
  (running-settings)
  (let ((selected (select-rows (rows) window span)))
    (if (null selected)
        (format nil "Slash commands, ~a: none recorded." title)
        (recent-table selected title limit))))

(defun clear ()
  "Delete the ledger and answer what it held."
  (running-settings)
  (format nil "command-ledger: deleted ~d recorded command(s)." (forget-rows)))
