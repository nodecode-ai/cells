;;;; ledger.lisp --- one row, the JSONL file, the selection and the two reports.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A row is a plist of identifiers, timings and token counts -- never the
;;;; system instruction, the user message, or the answer. It is written as one
;;;; JSON object per line, so the file is append-only and a torn line is
;;;; skipped rather than fatal.
;;;;
;;;; Nothing here reaches the engine: the report functions take rows and
;;;; answer text, which is what makes them testable without a provider.

(in-package #:nodecode-aux-ledger)

(defparameter +recent-rows+ 10
  "How many calls /aux lists when no count is typed.")

(defparameter +max-recent-rows+ 100
  "The largest count /aux accepts after RECENT.")

(defparameter +day-seconds+ 86400
  "The window /aux reads with no span typed: the last 24 hours.")

(defparameter +bytes-per-row+ 400
  "What one row is assumed to weigh, so a 5000-row ledger trims near 2 MB.")

(defparameter +token-fields+
  '((:input-tokens . "input_tokens")
    (:output-tokens . "output_tokens")
    (:cached-input-tokens . "cached_input_tokens")
    (:cache-write-tokens . "cache_write_tokens")
    (:reasoning-tokens . "reasoning_tokens"))
  "The token counts a row keeps, and the key each has in the file.")

(defparameter +prompt-fields+ '(:input-tokens :cached-input-tokens :cache-write-tokens)
  "What the provider read: fresh input plus cache reads and cache writes.")

;;; --- one row ----------------------------------------------------------------

(defun make-row (&key ts session-id task provider model retry-count duration-s
                        error-type usage-p tokens)
  "One ledger row. TOKENS is a plist of the counts the provider reported."
  (list :ts (or ts 0)
        :session-id (or session-id "")
        :task (or task "unknown")
        :provider (or provider "")
        :model (or model "")
        :retry-count (max 0 (or retry-count 0))
        :duration-s (max 0.0 (coerce (or duration-s 0) 'single-float))
        :error-type error-type
        :has-usage (and usage-p t)
        :input-tokens (max 0 (or (getf tokens :input-tokens) 0))
        :output-tokens (max 0 (or (getf tokens :output-tokens) 0))
        :cached-input-tokens (max 0 (or (getf tokens :cached-input-tokens) 0))
        :cache-write-tokens (max 0 (or (getf tokens :cache-write-tokens) 0))
        :reasoning-tokens (max 0 (or (getf tokens :reasoning-tokens) 0))))

(defun row-prompt-tokens (row)
  "Everything the provider read for ROW."
  (loop for field in +prompt-fields+ sum (or (getf row field) 0)))

;;; --- the file ---------------------------------------------------------------

(defun quote-json (text)
  "TEXT inside JSON string quotes, escapes and all."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across text
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (if (< (char-code character) 32)
                      (format out "\\u~4,'0x" (char-code character))
                      (write-char character out)))))
    (write-char #\" out)))

(defun json-text (value)
  "VALUE as the JSON a row holds: a number, a string, a boolean or null."
  (typecase value
    (null "null")
    ((eql t) "true")
    (integer (princ-to-string value))
    (real (format nil "~,3f" (coerce value 'single-float)))
    (string (quote-json value))
    (t "null")))

(defun row-pairs (row)
  "ROW as (JSON-KEY . VALUE) pairs, in the file's member order."
  (list* (cons "ts" (getf row :ts))
         (cons "session_id" (getf row :session-id))
         (cons "task" (getf row :task))
         (cons "provider" (getf row :provider))
         (cons "model" (getf row :model))
         (cons "retry_count" (getf row :retry-count))
         (cons "duration_s" (getf row :duration-s))
         (cons "error_type" (getf row :error-type))
         (cons "has_usage" (getf row :has-usage))
         (loop for (field . name) in +token-fields+
               collect (cons name (getf row field)))))

(defun row-line (row)
  "ROW as the one JSON line the ledger appends."
  (format nil "{~{~a~^,~}}"
          (loop for (name . value) in (row-pairs row)
                collect (format nil "~a:~a" (quote-json name) (json-text value)))))

(defun path-suffix (path)
  "The temporary name a trim writes before it replaces PATH."
  (make-pathname :name (format nil ".~a-trim" (or (pathname-name path) "calls"))
                 :type "tmp" :defaults path))

(defun file-size (path)
  (with-open-file (in path :direction :input :if-does-not-exist nil)
    (or (and in (file-length in)) 0)))

(defun trim-file (path max-rows)
  "Keep PATH's newest MAX-ROWS lines, written whole or not at all."
  (let* ((rows (read-rows path))
         (kept (if (> (length rows) max-rows) (last rows max-rows) rows))
         (tmp (path-suffix path)))
    (unwind-protect
         (progn
           (with-open-file (out tmp :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
             (dolist (row kept) (write-line (row-line row) out)))
           (rename-file tmp path))
      (when (probe-file tmp) (ignore-errors (delete-file tmp))))
    (length kept)))

(defun append-row (path row max-rows)
  "Append ROW to PATH, trimming the file to MAX-ROWS once it grows past its weight."
  (ensure-directories-exist path)
  (with-aux-ledger-lock
    (with-open-file (out path :direction :output :if-exists :append
                              :if-does-not-exist :create)
      (write-line (row-line row) out))
    (when (> (file-size path) (* +bytes-per-row+ (max 1 max-rows)))
      (trim-file path max-rows)))
  path)

(defun json-field (object name type)
  (nlk:json-value object type name))

(defun row-from-json (object)
  "OBJECT, one decoded line, as a row; NIL when it is not an object."
  (when (hash-table-p object)
    (make-row :ts (or (json-field object "ts" :integer) 0)
              :session-id (json-field object "session_id" :string)
              :task (or (json-field object "task" :string) "unknown")
              :provider (json-field object "provider" :string)
              :model (json-field object "model" :string)
              :retry-count (or (json-field object "retry_count" :integer) 0)
              :duration-s (or (json-field object "duration_s" :number) 0)
              :error-type (json-field object "error_type" :string)
              :usage-p (json-field object "has_usage" :boolean)
              :tokens (loop for (field . name) in +token-fields+
                             collect field
                             collect (or (json-field object name :integer) 0)))))

(defun read-rows (path)
  "Every parseable row of PATH, oldest first. A torn or foreign line is skipped."
  (let ((rows '()) (text nil))
    (when (probe-file path)
      (setf text (uiop:read-file-string path)))
    (dolist (line (and text (uiop:split-string text :separator '(#\Newline))))
      (let ((trimmed (nlk:trimmed line)))
        (unless (zerop (length trimmed))
          (let ((row (handler-case (row-from-json (nlk:decode-json trimmed))
                       (error () nil))))
            (when row (push row rows))))))
    (nreverse rows)))

(defun clear-file (path)
  "Delete PATH; how many rows it held."
  (with-aux-ledger-lock
    (let ((count (length (read-rows path))))
      (when (probe-file path) (delete-file path))
      count)))

;;; --- windows and counts ------------------------------------------------------

(defun parse-span (word)
  "Seconds for a span such as 12h or 7d; NIL when WORD is not one."
  (let* ((length (length word))
         (unit (and (> length 1) (char word (1- length))))
         (number (and (> length 1) (subseq word 0 (1- length))))
         (scale (case unit (#\h 3600) (#\d +day-seconds+) (t nil))))
    (when (and scale number (plusp (length number)) (<= (length number) 4)
               (every #'digit-char-p number))
      (let ((count (parse-integer number)))
        (and (plusp count) (* count scale))))))

(defun parse-count (word)
  "A row count from 1 to +MAX-RECENT-ROWS+; NIL when WORD is not one."
  (when (and (plusp (length word)) (<= (length word) 3) (every #'digit-char-p word))
    (let ((count (parse-integer word)))
      (and (<= 1 count +max-recent-rows+) count))))

(defun span-title (word)
  (let ((count (parse-integer word :end (1- (length word))))
        (unit (char word (1- (length word)))))
    (format nil "last ~d ~a~p" count (if (char= unit #\d) "day" "hour") count)))

(defun select-rows (rows window now span)
  "ROWS inside WINDOW: all of them, the newest row's session, or the span back from NOW."
  (cond ((equal window "all") rows)
        ((equal window "session")
         (let ((session (last-session-id rows)))
           (if (zerop (length session))
               '()
               (remove-if-not (lambda (row) (equal session (getf row :session-id))) rows))))
        (t (remove-if (lambda (row) (< (getf row :ts) (- now span))) rows))))

(defun last-session-id (rows)
  "The session of the newest row that ran inside one, or the empty string."
  (or (loop for row in rows
            when (plusp (length (or (getf row :session-id) "")))
              do (return-from last-session-id (getf row :session-id)))
      ""))

(defun local-day (ts)
  "TS as a local calendar day, YYYY-MM-DD, or unknown when it carries none."
  (handler-case
      (multiple-value-bind (second minute hour date month year)
          (decode-universal-time (truncate ts))
        (declare (ignore second minute hour))
        (format nil "~4,'0d-~2,'0d-~2,'0d" year month date))
    (error () "unknown")))

;;; --- text -------------------------------------------------------------------

(defun padded (text width right-p)
  (let ((fill (max 0 (- width (length text)))))
    (if right-p
        (concatenate 'string (make-string fill :initial-element #\Space) text)
        (concatenate 'string text (make-string fill :initial-element #\Space)))))

(defun table (header body)
  "HEADER and BODY as aligned lines: the first column left, the rest right."
  (let ((widths (loop for column from 0 below (length header)
                      collect (reduce #'max (cons (length (nth column header))
                                                  (mapcar (lambda (row)
                                                            (length (nth column row)))
                                                          body))))))
    (loop for row in (cons header body)
          collect (string-right-trim
                   " "
                   (format nil "~{~a~^  ~}"
                           (loop for cell in row
                                 for width in widths
                                 for column from 0
                                 collect (padded cell width (plusp column))))))))

(defun count-text (number)
  "NUMBER with thousands separators, the way the report reads a bill."
  (let ((digits (princ-to-string (max 0 (truncate number))))
        (out '()))
    (loop for index downfrom (1- (length digits)) to 0
          do (push (char digits index) out)
             (when (and (plusp index) (zerop (mod (- (length digits) index) 3)))
               (push #\, out)))
    (coerce out 'string)))

(defun duration-text (seconds)
  (if (< seconds 60)
      (format nil "~,1fs" (coerce seconds 'single-float))
      (multiple-value-bind (minutes rest) (truncate (round seconds) 60)
        (format nil "~dm~2,'0ds" minutes rest))))

(defun age-text (seconds)
  (let ((seconds (max 0 (round seconds))))
    (cond ((>= seconds +day-seconds+) (format nil "~dd ago" (truncate seconds +day-seconds+)))
          ((>= seconds 3600) (format nil "~dh ago" (truncate seconds 3600)))
          ((>= seconds 60) (format nil "~dm ago" (truncate seconds 60)))
          (t (format nil "~ds ago" seconds)))))

(defun title-of (window span-word)
  (cond ((and (equal window "day") span-word) (span-title span-word))
        ((equal window "session") "latest session")
        ((equal window "all") "all recorded")
        (t "last 24 hours")))

(defun recent-text (rows now title limit)
  "The newest LIMIT attempts of ROWS, newest first, one line each."
  (if (null rows)
      (format nil "Auxiliary LLM calls, ~a: none recorded." title)
      (let ((newest (reverse (last rows (min limit (length rows))))))
        (format nil "~{~a~^~%~}"
                (append
                 (list (format nil "Auxiliary LLM calls, ~a, newest ~d of ~d:"
                               title (length newest) (length rows))
                       "")
                 (table '("when" "task" "model" "input" "output" "time" "result")
                        (mapcar (lambda (row)
                                  (list (age-text (- now (getf row :ts)))
                                        (getf row :task)
                                        (or-named (getf row :model))
                                        (count-text (row-prompt-tokens row))
                                        (count-text (getf row :output-tokens))
                                        (duration-text (getf row :duration-s))
                                        (or (getf row :error-type) "ok")))
                                newest)))))))

(defun or-named (text)
  "TEXT, or unknown when the row carried none."
  (if (and (stringp text) (plusp (length text))) text "unknown"))

(defun group-key (row by)
  (cond ((equal by "day") (local-day (getf row :ts)))
        ((equal by "model") (or-named (getf row :model)))
        ((equal by "provider") (or-named (getf row :provider)))
        (t (or (getf row :task) "unknown"))))

(defun group-rows (rows by)
  "ROWS tallied per GROUP-KEY: (KEY . #(CALLS FAILED INPUT OUTPUT SECONDS))."
  (let ((groups (make-hash-table :test #'equal))
        (order '()))
    (dolist (row rows)
      (let* ((key (group-key row by))
             (tally (or (gethash key groups)
                        (progn (push key order)
                               (setf (gethash key groups) (vector 0 0 0 0 0.0))))))
        (incf (aref tally 0))
        (when (getf row :error-type) (incf (aref tally 1)))
        (incf (aref tally 2) (row-prompt-tokens row))
        (incf (aref tally 3) (getf row :output-tokens))
        (incf (aref tally 4) (coerce (getf row :duration-s) 'single-float))))
    (loop for key in order collect (cons key (gethash key groups)))))

(defun group-order (groups by)
  "GROUPS, the one that cost most first; by day, the newest day first and unknown last."
  (if (equal by "day")
      (append (sort (remove "unknown" (copy-list groups) :key #'car :test #'equal)
                    #'string> :key #'car)
              (remove-if-not (lambda (group) (equal "unknown" (car group))) groups))
      (sort (copy-list groups)
            (lambda (a b)
              (let ((left (+ (aref (cdr a) 2) (aref (cdr a) 3)))
                    (right (+ (aref (cdr b) 2) (aref (cdr b) 3))))
                (if (= left right)
                    (string< (car a) (car b))
                    (> left right)))))))

(defun tally-line (group)
  "GROUP as one table row: its name, calls, failures, tokens and time."
  (let ((tally (cdr group)))
    (list (car group)
          (princ-to-string (aref tally 0))
          (princ-to-string (aref tally 1))
          (count-text (aref tally 2))
          (count-text (aref tally 3))
          (duration-text (aref tally 4)))))

(defun summarize-text (rows title by)
  "Per-BY totals of ROWS (calls, failures, input and output tokens, wall time)."
  (if (null rows)
      (format nil "Auxiliary LLM calls, ~a: none recorded." title)
      (let* ((groups (group-order (group-rows rows by) by))
             (totals (vector 0 0 0 0 0.0)))
        (dolist (group groups)
          (loop for index from 0 below 5
                do (incf (aref totals index) (aref (cdr group) index))))
        (let* ((body (append (mapcar #'tally-line groups)
                             (list (tally-line (cons "total" totals)))))
               (silent (count-if (lambda (row) (not (getf row :has-usage))) rows))
               (lines (append (list (format nil "Auxiliary LLM calls, ~a:" title) "")
                              (table (list (if (equal by "task") "task" by)
                                           "calls" "failed" "input" "output" "time")
                                     body))))
          (when (plusp silent)
            (setf lines (append lines
                                (list ""
                                      (format nil "~d call(s) reported no token usage (unstreamed or failed)."
                                              silent)))))
          (format nil "~{~a~^~%~}" lines)))))
