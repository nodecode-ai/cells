;;;; ledger.lisp --- the file of reactions left in your chats, and the summaries /reaction-log prints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A row holds one reaction change: which emoji, whether it was set or
;;;; taken back, the room it happened in, and a short hash of the chat and of
;;;; the message; never the chat or message id themselves, never who reacted,
;;;; and never anything said in the chat. The file is JSON lines under the
;;;; home, one row a line, trimmed to the section's own bound at the newest
;;;; end. A message's reactions now are its rows folded in file order.

(in-package #:nodecode-reaction-ledger)

(defparameter +filename+ "reaction-ledger/reactions.jsonl"
  "Where the ledger is written, under the home.")

(defparameter +unix-epoch+ 2208988800
  "Common Lisp's universal time at 1970-01-01T00:00:00Z: what a unix second
and a universal time differ by.")

(defparameter +day-seconds+ 86400
  "One day in unix seconds.")

(defparameter +recent-rows+ 10
  "How many rows /reaction-log recent lists when no count is given.")

(defparameter +max-recent-rows+ 100
  "The most rows /reaction-log recent N may ask for.")

(defparameter +text-limit+ 16
  "The longest emoji a row keeps: a reaction names one emoji, never a sentence.")

(defparameter +state-note+
  "Each message counts once, with the reactions it has now."
  "The line under every emoji summary, saying how a message is counted.")

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

(defun tag (&rest parts)
  "A short, stable stand-in for the ids PARTS name, empty when one is
missing: a chat id names a person or a group, and the ledger only needs to
tell chats and messages apart."
  (if (some (lambda (part) (or (null part) (zerop (length (princ-to-string part))))) parts)
      ""
      (nlk:short-digest
       (format nil "~{~a~^:~}" (mapcar (lambda (part) (princ-to-string part)) parts)) 12)))

(defun rows ()
  "Every parseable row of the ledger, oldest first; a torn line is skipped."
  (nlk:read-json-lines (ledger-path)))

(defun row-ts (row) (or (nlk:json-value row :number "ts") 0))
(defun row-room (row) (or (nlk:json-value row :string "room") "unknown"))
(defun row-chat (row) (or (nlk:json-value row :string "chat") ""))
(defun row-message (row) (or (nlk:json-value row :string "message") ""))
(defun row-emoji (row) (or (nlk:json-value row :string "emoji") ""))
(defun row-action (row) (or (nlk:json-value row :string "action") "add"))

(defun trim (path keep)
  "Keep the newest KEEP lines of PATH, rewritten through a temporary file
beside it, so a rewrite is never a half-written ledger."
  (let ((lines (remove "" (nlk:lines (nlk:read-text path)) :test #'string=)))
    (when (> (length lines) keep)
      (let ((temporary (merge-pathnames ".reactions.tmp" path)))
        (with-open-file (out temporary :direction :output :if-exists :supersede
                                       :if-does-not-exist :create :external-format :utf-8)
          (dolist (line (last lines keep)) (write-line line out)))
        (rename-file temporary path)))))

(defun append-change (room chat message emoji action)
  "Append one row for EMOJI set (:add) or taken back (:remove) on MESSAGE in
CHAT of ROOM, and keep the file within the section's bound.  => the row, or
NIL when the change names no message."
  (let ((chat-tag (tag room chat))
        (message-tag (tag room chat message)))
    (when (and (plusp (length chat-tag)) (plusp (length message-tag))
               (plusp (length emoji)))
      (let ((row (nlk:json-object "ts" (now)
                                   "room" (if (plusp (length room)) room "unknown")
                                   "chat" chat-tag
                                   "message" message-tag
                                   "emoji" (subseq emoji 0 (min +text-limit+ (length emoji)))
                                   "action" (if (string= action "remove") "remove" "add")))
            (path (ledger-path)))
        (nlk:append-json-line path row)
        (trim path (setting :rows))
        row))))

(defun forget-rows ()
  "Delete the ledger file; => how many rows it held."
  (let* ((path (ledger-path))
         (count (length (rows))))
    (when (probe-file path) (delete-file path))
    count))

;;; --- windows and words --------------------------------------------------------------

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

(defun select-rows (all window span)
  "The rows of ALL in WINDOW: everything, or the last SPAN seconds."
  (if (string= window "all")
      all
      (let ((floor (- (now) span)))
        (remove-if (lambda (row) (< (row-ts row) floor)) all))))

;;; --- a message's reactions now ------------------------------------------------------

(defun fold-messages (rows)
  "ROWS folded in file order: one entry per message, in the order the
ledger first names it -- (:message :chat :room :ts :emojis), EMOJIS a hash
of the emoji the message carries now to T, one it no longer carries to NIL."
  (let ((messages (make-hash-table :test #'equal))
        (order '()))
    (dolist (row rows)
      (let ((key (row-message row)))
        (when (plusp (length key))
          (let ((entry (gethash key messages)))
            (unless entry
              (setf entry (list :message key :chat (row-chat row) :room (row-room row)
                                :ts 0 :emojis (make-hash-table :test #'equal)))
              (setf (gethash key messages) entry)
              (push key order))
            (when (> (row-ts row) (getf entry :ts))
              (setf (getf entry :ts) (row-ts row)))
            (setf (gethash (row-emoji row) (getf entry :emojis))
                  (string= "add" (row-action row)))))))
    (loop for key in (nreverse order) collect (gethash key messages))))

(defun emojis-of (entry)
  "The emoji ENTRY's message carries now, in one order."
  (let ((found '()))
    (maphash (lambda (emoji present) (when present (push emoji found)))
             (getf entry :emojis))
    (sort found #'string<)))

(defun carrying (folded)
  "The entries of FOLDED whose message carries at least one reaction now."
  (remove-if (lambda (entry) (null (emojis-of entry))) folded))

(defun chats-text (entries)
  "How many distinct chats ENTRIES name."
  (princ-to-string
   (length (remove-duplicates (remove "" (mapcar (lambda (entry) (getf entry :chat)) entries)
                                      :test #'string=)
                              :test #'string=))))

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

(defun pad (text width &key right)
  "TEXT in WIDTH characters, padded on the right (or the left, with RIGHT)."
  (let* ((text (or text ""))
         (fill (make-string (max 0 (- width (length text))) :initial-element #\Space)))
    (if right (concatenate 'string fill text) (concatenate 'string text fill))))

(defun table (header rows &key (left 1))
  "HEADER and ROWS (lists of strings) as aligned lines: the first LEFT
columns to the left, every other to the right."
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
                                 for index from 0
                                 collect (pad cell width :right (>= index left))))))))

(defun top-emoji (entries)
  "The emoji of ENTRIES' messages that the most of them carry, or \"-\"."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (dolist (emoji (emojis-of entry))
        (incf (gethash emoji counts 0))))
    (let (best best-count)
      (maphash (lambda (emoji count)
                 (when (or (null best-count) (> count best-count)
                           (and (= count best-count) (string< emoji best)))
                   (setf best emoji best-count count)))
               counts)
      (or best "-"))))

(defun last-of (entries)
  "How long ago the newest entry of ENTRIES changed."
  (age (- (now) (reduce #'max entries :key (lambda (entry) (getf entry :ts))))))

(defun summary-by-emoji (entries all title)
  "ENTRIES (each message once) per emoji: how many messages carry it, in
how many chats, and how long ago it was last set."
  (let ((groups (make-hash-table :test #'equal))
        (last-seen (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (dolist (emoji (emojis-of entry)) (push entry (gethash emoji groups))))
    (dolist (row all)
      (let ((emoji (row-emoji row)))
        (when (> (row-ts row) (gethash emoji last-seen 0))
          (setf (gethash emoji last-seen) (row-ts row)))))
    (let* ((named (loop for emoji being the hash-keys of groups using (hash-value items)
                        collect (cons emoji items)))
           (ordered (sort named (lambda (left right)
                                  (if (= (length (cdr left)) (length (cdr right)))
                                      (string< (car left) (car right))
                                      (> (length (cdr left)) (length (cdr right)))))))
           (body (loop for (emoji . items) in ordered
                       collect (list emoji
                                     (princ-to-string (length items))
                                     (chats-text items)
                                     (age (- (now) (gethash emoji last-seen (now))))))))
      (setf body (append body
                         (list (list "all"
                                     (princ-to-string (length entries))
                                     (chats-text entries)
                                     (last-of entries)))))
      (format nil "~a~%~%~a~%~%~a"
              (format nil "Reactions, ~a:" title)
              (format nil "~{~a~^~%~}"
                      (table (list "emoji" "messages" "chats" "last") body))
              +state-note+))))

(defun summary-by-day (entries all title)
  "ENTRIES (each message once) per local day of their newest change,
newest day first, with that day's most carried emoji."
  (declare (ignore all))
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (entry entries)
      (push entry (gethash (local-day (getf entry :ts)) groups)))
    (let* ((named (loop for day being the hash-keys of groups using (hash-value items)
                        collect (cons day (nreverse items))))
           (ordered (sort named (lambda (left right)
                                  (cond ((string= (car left) "unknown") nil)
                                        ((string= (car right) "unknown") t)
                                        (t (string> (car left) (car right)))))))
           (body (loop for (day . items) in ordered
                       collect (list day
                                     (princ-to-string (length items))
                                     (chats-text items)
                                     (top-emoji items)
                                     (last-of items)))))
      (format nil "~a~%~%~a~%~%~a"
              (format nil "Reactions, ~a:" title)
              (format nil "~{~a~^~%~}"
                      (table (list "day" "messages" "chats" "top" "last") body))
              +state-note+))))

(defun report (&key window span by title)
  "The report /reaction-log and SUMMARY both answer: the rows in WINDOW,
folded, as a table grouped BY emoji or day."
  (let* ((all (select-rows (rows) window span))
         (folded (fold-messages all))
         (entries (carrying folded))
         (removed (- (length folded) (length entries))))
    (cond
      ((null all) (format nil "Reactions, ~a: none recorded." title))
      ((null entries)
       (format nil "Reactions, ~a: none recorded~@[ (reactions removed from ~d message(s))~]."
               title (and (plusp removed) removed)))
      (t (let ((text (if (string= by "day")
                         (summary-by-day entries all title)
                         (summary-by-emoji entries all title))))
           (if (plusp removed)
               (format nil "~a~%~a" text
                       (format nil "Reactions were removed again from ~d message(s)." removed))
               text))))))

(defun recent-report (all title limit)
  "The newest LIMIT changes of ALL, newest first, one line each."
  (if (null all)
      (format nil "Reactions, ~a: none recorded." title)
      (let ((newest (reverse (last all (min limit (length all))))))
        (format nil "Reaction changes, ~a, newest ~d of ~d:~%~%~{~a~^~%~}"
                title (length newest) (length all)
                (table '("when" "change" "message" "chat")
                       (loop for row in newest
                             collect (list (age (- (now) (row-ts row)))
                                           (format nil "~a ~a" (row-emoji row)
                                                   (if (string= "remove" (row-action row))
                                                       "removed" "added"))
                                           (row-message row)
                                           (row-chat row)))
                       :left 2)))))

;;; --- the verbs ---------------------------------------------------------------------

(defun summary (&key (window "day") (span +day-seconds+) (by "emoji")
                   title)
  "The reactions the ledger holds, per emoji (or per day with BY \"day\"):
how many messages carry each, in how many chats, and when last.  => the
whole table as one string; a window with nothing in it answers one line."
  (running-settings)
  (report :window window :span span :by by
          :title (or title (if (string= window "all") "all recorded" "last 24 hours"))))

(defun recent (&key (window "day") (span +day-seconds+) title
                  (limit +recent-rows+))
  "The newest LIMIT reaction changes of WINDOW, newest first."
  (running-settings)
  (recent-report (select-rows (rows) window span)
                 (or title (if (string= window "all") "all recorded" "last 24 hours"))
                 limit))

(defun clear ()
  "Delete the ledger and answer what it held."
  (running-settings)
  (format nil "reaction-ledger: deleted ~d recorded reaction change(s)." (forget-rows)))
