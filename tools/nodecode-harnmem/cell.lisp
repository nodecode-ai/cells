;;;; cell.lisp --- HarnMem: cross-session memory in plain Markdown.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ys118/harnmem's Hermes memory provider (see NOTICE). The
;;;; store is the original's, on disk: one Markdown file per memory under
;;;; memories/user/ (or memories/projects/<project>/), a YAML frontmatter in
;;;; the subset the original writes, the body below it. A memory written here
;;;; is read by the other HarnMem harnesses and theirs by this cell, which is
;;;; the point of the format.
;;;;
;;;; The plugin's two halves are carried over: the injection (a small,
;;;; byte-stable snapshot at the head of a turn's standing context) and the
;;;; five tools -- harnmem_search, harnmem_write, harnmem_read,
;;;; harnmem_forget, harnmem_status -- plus /harnmem for the operator. The
;;;; engine the plugin drove through a Node child process is rewritten here:
;;;; reads, the lexical ranker, the hot-index layout and the writes.

(defpackage #:nodecode-harnmem
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:harnmem-error #:harnmem-error-detail))

(in-package #:nodecode-harnmem)

(nlk:define-peripheral harnmem :not-running t :lock "harnmem")

;;; --- the vocabulary of the store ------------------------------------------------

(defparameter +context-header+ "## HarnMem memory (cross-harness; stable snapshot)"
  "The line the injected block opens with, as the original renders it.")

(defparameter +schema-version+ 1
  "The record schema version every file carries.")

(defparameter +memory-types+ '("user" "feedback" "project" "reference")
  "The four canonical memory types, aligned with the original's.")

(defparameter +unix-epoch+ 2208988800
  "Universal time minus this is seconds since 1970, which is what a record stores.")

(defparameter +hour-seconds+ 3600)

(defparameter +crookford+
  "0123456789ABCDEFGHJKMNPQRSTVWXYZ"
  "Crockford Base32, the alphabet a memory id is written in.")

(defparameter +directory-tree+
  '("memories/user" "memories/projects" "state" "index" "trash"
    "digests" "sessions" "prompts" "data")
  "What the store's home holds, as the original's init() creates it.")

(defvar *clock* nil
  "NIL reads the machine's clock; a universal time pins it, which is how the
tests read a fixed instant.")

(defun clock-now ()
  "The universal time this cell reads as now."
  (or *clock* (get-universal-time)))

(defun now-ms ()
  "Now as the store writes it: milliseconds since 1970."
  (* 1000 (- (clock-now) +unix-epoch+)))

(defun ms->universal (ms)
  (+ +unix-epoch+ (floor (or ms 0) 1000)))

(defun new-memory-id (&optional (ms (now-ms)))
  "A time-sortable id of the original's shape: mem_ and 26 Crockford Base32 chars."
  (with-output-to-string (out)
    (write-string "mem_" out)
    (loop for shift from 45 downto 0 by 5
          do (write-char (char +crookford+ (ldb (byte 5 shift) ms)) out))
    (dotimes (index 16)
      (declare (ignore index))
      (write-char (char +crookford+ (random 32)) out))))

;;; --- the frontmatter subset -----------------------------------------------------
;;; The original writes `key: value`, an inline `key: [a, b]`, an empty
;;; `key: []`, a block list, or a nested `key:` with two-space `sub: value`.
;;; Scalars are plain when they can be, JSON-quoted when they cannot. The
;;; reader here takes all of that, and anything else as text.

(defun split-lines (text)
  (let ((lines '())
        (start 0))
    (loop for index from 0 below (length text)
          do (when (char= (char text index) #\Newline)
               (push (string-right-trim '(#\Return) (subseq text start index)) lines)
               (setf start (1+ index))))
    (when (< start (length text)) (push (subseq text start) lines))
    (nreverse lines)))

(defun json-quote (string)
  "STRING as the JSON string literal the original's parser reads back."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across string
          do (case character
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (#\Return (write-string "\\r" out))
               (t (write-char character out))))
    (write-char #\" out)))

(defun json-unquote (string)
  "STRING, a quoted literal including its quotes, as its text."
  (with-output-to-string (out)
    (let ((index 1)
          (end (1- (length string))))
      (loop while (< index end)
            do (let ((character (char string index)))
                 (if (char= character #\\)
                     (let ((next (char string (incf index))))
                       (write-char (case next
                                     (#\n #\Newline)
                                     (#\t #\Tab)
                                     (#\r #\Return)
                                     (t next))
                                   out))
                     (write-char character out))
                 (incf index))))))

(defun split-inline (text)
  "TEXT, the inside of an inline list, as its items -- commas outside quotes split."
  (let ((items '())
        (start 0)
        (quote nil))
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (cond (quote (when (char= character quote) (setf quote nil)))
                   ((member character '(#\" #\')) (setf quote character))
                   ((char= character #\,)
                    (push (subseq text start index) items)
                    (setf start (1+ index)))))
    (push (subseq text start) items)
    (nreverse items)))

(defun parse-scalar (text)
  "TEXT as a value: a quoted string, a list, a number, a boolean or plain text."
  (let ((value (string-trim '(#\Space #\Tab) text)))
    (cond ((string= value "") "")
          ((string= value "null") nil)
          ((string= value "true") t)
          ((string= value "false") nil)
          ((and (> (length value) 1) (char= (char value 0) #\") (char= (char value (1- (length value))) #\"))
           (json-unquote value))
          ((and (> (length value) 1) (char= (char value 0) #\[) (char= (char value (1- (length value))) #\]))
           (mapcar #'parse-scalar (split-inline (subseq value 1 (1- (length value))))))
          (t (multiple-value-bind (number end)
                 (ignore-errors (read-from-string value nil nil))
               (if (and number (or (integerp number) (floatp number))
                        (= end (length value)))
                   number
                   value))))))

(defun split-key (line)
  "LINE's `key: value` as two values, or NIL when it is not one."
  (let ((colon (position #\: line)))
    (when colon
      (values (string-trim '(#\Space #\Tab) (subseq line 0 colon))
              (subseq line (1+ colon))))))

(defun parse-frontmatter (text)
  "TEXT as (values DATA BODY): DATA a hash of the frontmatter, BODY the rest."
  (let ((lines (split-lines text)))
    (if (not (and lines (string= (string-trim '(#\Space #\Tab) (first lines)) "---")))
        (values (make-hash-table :test #'equal) text)
        (let ((data (make-hash-table :test #'equal))
              (index 1)
              (current nil)
              (body-start nil))
          (loop while (and (< index (length lines)) (null body-start))
                do (let ((line (nth index lines)))
                     (cond ((string= (string-trim '(#\Space #\Tab) line) "---")
                            (setf body-start (1+ index)))
                           ((string= (string-trim '(#\Space #\Tab) line) ""))
                           ((member (char line 0) '(#\Space #\Tab))
                            (when current
                              (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                                (if (and (> (length trimmed) 1) (string= "- " (subseq trimmed 0 2)))
                                    (setf (gethash current data)
                                          (append (gethash current data) (list (parse-scalar (subseq trimmed 2)))))
                                    (multiple-value-bind (key value) (split-key trimmed)
                                      (when key
                                        (setf (gethash (format nil "~a/~a" current (string-downcase key)) data)
                                              (parse-scalar value))))))))
                           (t (multiple-value-bind (key value) (split-key line)
                                (when key
                                  (setf current (string-downcase key))
                                  (setf (gethash current data)
                                        (if (string= "" (string-trim '(#\Space #\Tab) value))
                                            nil
                                            (parse-scalar value)))))))
                     (incf index)))
          (values data
                  (if body-start
                      (string-left-trim '(#\Newline #\Space #\Tab)
                                         (format nil "~{~a~^~%~}" (subseq lines body-start)))
                      ""))))))

(defun yaml-scalar (value)
  "VALUE as the original's scalar writer would put it on the line."
  (cond ((null value) "null")
        ((eq value t) "true")
        ((integerp value) (princ-to-string value))
        ((floatp value) (format nil "~,4f" value))
        ((stringp value)
         (if (or (zerop (length value))
                 (some (lambda (character)
                         (member character '(#\: #\# #\[ #\] #\{ #\} #\, #\" #\' #\Newline #\Tab)))
                       value)
                 (member (char value 0) '(#\Space #\- #\? #\& #\* #\! #\| #\> #\% #\@ #\`)))
             (json-quote value)
             value))
        (t (json-quote (princ-to-string value)))))

(defun yaml-lines (key value)
  "KEY and VALUE as the lines of a frontmatter entry."
  (cond ((and (listp value) (null value)) (list (format nil "~a: []" key)))
        ((listp value) (list (format nil "~a: [~{~a~^, ~}]" key (mapcar #'yaml-scalar value))))
        (t (list (format nil "~a: ~a" key (yaml-scalar value))))))

;;; --- paths and shards ------------------------------------------------------------

(defun home ()
  "The store's home: the section's, else ~/.harnmem as the original defaults."
  (let ((given (setting :home)))
    (cond ((null given) (merge-pathnames ".harnmem/" (user-homedir-pathname)))
          ((pathnamep given) (uiop:ensure-directory-pathname given))
          ((plusp (length (string given))) (uiop:ensure-directory-pathname (string given)))
          (t (merge-pathnames ".harnmem/" (user-homedir-pathname))))))

(defun project-name ()
  "The project shard this cell writes to, as the section names it."
  (let ((given (setting :project)))
    (if (and given (plusp (length given)))
        (string-downcase
         (with-output-to-string (out)
           (loop for character across given
                 do (write-char (if (or (alphanumericp character)
                                        (member character '(#\- #\_ #\.)))
                                    character
                                    #\-)
                                out))))
        "nodecode")))

(defun shard-directory (scope)
  (if (equal scope "project")
      (merge-pathnames (format nil "memories/projects/~a/" (project-name)) (home))
      (merge-pathnames "memories/user/" (home))))

(defun record-file-name (id)
  "ID as the file the store keeps it in."
  (format nil "~a.md"
          (subseq (with-output-to-string (out)
                    (loop for character across id
                          do (write-char (if (or (alphanumericp character)
                                                 (member character '(#\- #\_)))
                                             character
                                             #\_)
                                       out)))
                  0 (min 80 (length id)))))

(defun record-path (scope id)
  (merge-pathnames (record-file-name id) (shard-directory scope)))

(defun ensure-store ()
  "The directory tree the original's init() creates, and its VERSION file."
  (dolist (directory +directory-tree+)
    (ensure-directories-exist (merge-pathnames (format nil "~a/" directory) (home))))
  (let ((version (merge-pathnames "VERSION" (home))))
    (unless (probe-file version)
      (with-open-file (out version :direction :output :if-exists :supersede
                                   :if-does-not-exist :create :external-format :utf-8)
        (write-string "1" out)
        (terpri out)))))

(defun shard-files (scope)
  (let ((directory (shard-directory scope)))
    (if (uiop:directory-exists-p directory)
        (remove-if (lambda (file)
                      (equal (pathname-name file) "MEMORY"))
                   (directory (merge-pathnames (make-pathname :name :wild :type "md") directory)))
        '())))

(defun shards-with-records ()
  (remove nil (list (if (shard-files "user") "user" nil)
                    (if (shard-files "project") "project" nil))))

;;; --- a record --------------------------------------------------------------------

(defstruct (record (:constructor make-record))
  id type scope project title description content tags status importance confidence
  pinned created updated last-used used-count source)

(defun data-string (data key &optional (default ""))
  (let ((value (gethash key data)))
    (if (stringp value) value (if value (princ-to-string value) default))))

(defun data-number (data key &optional (default 0))
  (let ((value (gethash key data)))
    (if (numberp value) value default)))

(defun record-from-data (data body fallback-id)
  "DATA, one file's frontmatter, and BODY its markdown, as a record."
  (make-record
   :id (let ((id (data-string data "id")))
         (if (plusp (length id)) id fallback-id))
   :type (let ((type (data-string data "type" "user")))
           (if (member type +memory-types+ :test #'equal) type "user"))
   :scope (data-string data "scope" "user")
   :project (let ((project (gethash "project" data))) (and project (princ-to-string project)))
   :title (data-string data "title")
   :description (data-string data "description")
   :content (or (gethash "__body" data) body)
   :tags (let ((tags (gethash "tags" data)))
           (if (listp tags) (remove-if-not #'stringp tags) '()))
   :status (data-string data "status" "active")
   :importance (data-number data "importance" 5)
   :confidence (data-number data "confidence" 0.7)
   :pinned (eq t (gethash "pinned" data))
   :created (data-number data "created" 0)
   :updated (data-number data "updated" 0)
   :last-used (data-number data "last_used" 0)
   :used-count (data-number data "used_count" 0)
   :source (list (cons :harness (data-string data "source/harness" "nodecode"))
                 (cons :session (data-string data "source/session" "")))))

(defun read-record-file (path)
  "PATH as a record, or NIL when no frontmatter is there to read."
  (handler-case
      (multiple-value-bind (data body) (parse-frontmatter (uiop:read-file-string path))
        (when (plusp (hash-table-count data))
          (record-from-data data body (or (pathname-name path) ""))))
    (error () nil)))

(defun shard-records (scope)
  (remove nil (mapcar #'read-record-file (shard-files scope))))

(defun all-records (&key scope)
  "The store's records: the user shard, and the project shard beside it."
  (if scope (shard-records scope) (append (shard-records "user") (shard-records "project"))))

(defun find-record (id)
  (dolist (scope '("user" "project") nil)
    (let ((record (read-record-file (record-path scope id))))
      (when (and record (equal (record-id record) id)) (return record)))))

(defun write-record-file (record)
  "RECORD as the original's file -- frontmatter, blank line, body -- or nothing."
  (let* ((path (record-path (record-scope record) (record-id record)))
         (temporary (merge-pathnames (format nil "~a.tmp" (pathname-name path))
                                     (shard-directory (record-scope record))))
         (source (record-source record)))
    (ensure-directories-exist path)
    (with-open-file (out temporary :direction :output :if-exists :supersede
                                    :if-does-not-exist :create :external-format :utf-8)
      (flet ((line (key value)
               (dolist (text (yaml-lines key value)) (format out "~a~%" text))))
        (write-string "---" out)
        (terpri out)
        (line "schema" +schema-version+)
        (line "id" (record-id record))
        (line "type" (record-type record))
        (line "scope" (record-scope record))
        (when (equal (record-scope record) "project")
          (line "project" (record-project record)))
        (line "title" (record-title record))
        (line "description" (record-description record))
        (line "tags" (or (record-tags record) '()))
        (line "entities" '())
        (line "importance" (or (record-importance record) 5))
        (line "confidence" (or (record-confidence record) 0.7))
        (line "pinned" (if (record-pinned record) t "false"))
        (line "status" (or (record-status record) "active"))
        (line "created" (or (record-created record) (now-ms)))
        (line "updated" (or (record-updated record) (now-ms)))
        (line "last_used" (or (record-last-used record) 0))
        (line "used_count" (or (record-used-count record) 0))
        (format out "source:~%")
        (format out "  harness: ~a~%" (yaml-scalar (or (cdr (assoc :harness source)) "nodecode")))
        (format out "  session: ~a~%" (yaml-scalar (or (cdr (assoc :session source)) "")))
        (write-string "---" out)
        (terpri out)
        (terpri out)
        (write-string (string-right-trim '(#\Newline #\Space #\Tab) (or (record-content record) "")) out)
        (terpri out)))
    (uiop:rename-file-overwriting-target temporary path)
    path))

;;; --- the ranker ------------------------------------------------------------------

(defun term-character-p (character)
  "A character that belongs to a searchable word: letters, digits, and CJK."
  (or (alphanumericp character) (> (char-code character) #x2E7F)))

(defun tokenize (text)
  "TEXT as the lowercase words the ranker counts, one-letter words dropped."
  (let ((terms '())
        (start nil)
        (text (or text "")))
    (flet ((cut (end)
             (when start
               (push (string-downcase (subseq text start end)) terms)
               (setf start nil))))
      (loop for index from 0 below (length text)
            do (if (term-character-p (char text index))
                   (unless start (setf start index))
                   (cut index)))
      (cut (length text)))
    (remove-if (lambda (term) (< (length term) 2)) (nreverse terms))))

(defun record-terms (record)
  "RECORD as its weighted bag: token -> weight, title and tags heavier than the body."
  (let ((bag (make-hash-table :test #'equal)))
    (flet ((add (text weight)
             (dolist (term (tokenize text)) (incf (gethash term bag 0) weight))))
      (add (record-title record) 3)
      (add (record-description record) 2)
      (dolist (tag (record-tags record)) (add tag 3))
      (add (record-content record) 1))
    bag))

(defun context-score (record now)
  "The original's hot-index score: importance, confidence, recency, use, pinned."
  (let* ((anchor (cond ((plusp (record-last-used record)) (record-last-used record))
                       ((plusp (record-updated record)) (record-updated record))
                       (t (record-created record))))
         (hours (max 0 (/ (- now anchor) 3600000.0)))
         (recency (expt 0.99 hours))
         (use (min 1.0 (/ (log (1+ (or (record-used-count record) 0))) (log 21))))
         (confidence (or (record-confidence record) 0))
         (importance (or (record-importance record) 0)))
    (+ (* 0.55 (/ importance 10))
       (* 0.2 confidence)
       (* 0.1 recency)
       (* 0.1 use)
       (if (record-pinned record) 0.05 0.0))))

(defun active-records (&key scope)
  (remove-if-not (lambda (record) (equal (record-status record) "active"))
                 (all-records :scope scope)))

(defun search-records (query &key limit scope)
  "QUERY against the store: (values HITS ABSTAINED TOTAL), HITS being
((RECORD . SCORE) ...) best first."
  (let* ((now (now-ms))
         (candidates (active-records :scope scope))
         (terms (remove-duplicates (tokenize query) :test #'equal))
         (documents (mapcar (lambda (record) (cons record (record-terms record))) candidates))
         (total (length documents))
         (document-frequency (make-hash-table :test #'equal))
         (hits '()))
    (dolist (entry documents)
      (maphash (lambda (term weight)
                 (declare (ignore weight))
                 (incf (gethash term document-frequency 0)))
               (cdr entry)))
    (dolist (entry documents)
      (let ((record (car entry))
            (bag (cdr entry))
            (score 0.0))
        (dolist (term terms)
          (let ((weight (gethash term bag 0))
                (frequency (gethash term document-frequency 0)))
            (when (plusp weight)
              (incf score (* (log (1+ (/ total (1+ frequency))))
                             (/ (* weight 2.2) (+ weight 1.2)))))))
        (when (plusp score)
          (push (cons record (+ score (* 0.3 (context-score record now)))) hits))))
    (let ((ranked (sort hits (lambda (a b) (> (cdr a) (cdr b))))))
      (values (if limit (subseq ranked 0 (min limit (length ranked))) ranked)
              (null ranked)
              total))))

(defun snippet-of (record)
  "The one-line opening the original puts beside a hit."
  (let ((text (substitute #\Space #\Newline (or (record-content record) ""))))
    (string-trim '(#\Space)
                 (if (> (length text) 160) (concatenate 'string (subseq text 0 160) "...") text))))

;;; --- the injected snapshot ---------------------------------------------------------

(defun line-tokens (line)
  (max 1 (ceiling (length line) 4)))

(defun context-footer ()
  (format nil "<!-- SSOT: ~a. Use harnmem_search for details; never invent memory ids. Snapshot refreshes on session start, after memory writes and at day rollover. -->"
          (string-right-trim "/" (namestring (home)))))

(defun snapshot-text ()
  "The block the original injects at a turn's head, byte-identical while nothing moves."
  (let* ((now (now-ms))
         (budget (setting :max-inject-tokens))
         (hot (setting :hot-entries))
         (records (active-records))
         (ordered '())
         (tokens 0))
    (flet ((entry (record pinned-p)
             (format nil "- ~a[~a] ~a - ~a"
                     (if pinned-p "* " "")
                     (record-type record) (record-title record) (record-description record))))
      (let ((pinned (stable-sort (remove-if-not #'record-pinned records)
                                 (lambda (a b)
                                   (or (> (or (record-importance a) 0) (or (record-importance b) 0))
                                       (> (or (record-updated a) 0) (or (record-updated b) 0)))))))
        (dolist (record pinned)
          (let ((line (entry record t)))
            (push line ordered)
            (incf tokens (line-tokens line))))
        (let ((added 0)
              (pool (stable-sort (remove-if #'record-pinned records)
                                 (lambda (a b) (> (context-score a now) (context-score b now))))))
          (dolist (record pool)
            (when (>= added (max 0 (- hot (length ordered)))) (return))
            (let* ((line (entry record nil))
                   (cost (line-tokens line)))
              (when (> (+ tokens cost) budget) (return))
              (push line ordered)
              (incf tokens cost)
              (incf added))))))
    (let* ((lines (nreverse ordered))
           (body (format nil "~{~a~^~%~}" lines))
           (cap (setting :max-inject-chars)))
      (when (> (length body) cap)
        (let ((cut (position #\Newline body :from-end t :end cap)))
          (setf body (subseq body 0 (or cut (min cap (length body)))))))
      (if (plusp (length body))
          (format nil "~a~%~%~a~%~%~a" +context-header+ body (context-footer))
          ""))))

(defun snapshot-section (next session-id)
  "Every turn's standing sections, with the memory snapshot first."
  (let ((sections (funcall next session-id)))
    (if (and *harnmem* (handler-case (setting :inject) (error () nil)))
        (let ((text (handler-case (snapshot-text) (error () nil))))
          (if (and text (plusp (length text)))
              (acons "harnmem" text (remove "harnmem" sections :key #'car :test #'equal))
              sections))
        sections)))
;;; --- the tools ----------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: harnmem: ~a" condition)))))

(defun argument-string (args name &key (required t))
  (let ((value (and (hash-table-p args) (gethash name args))))
    (cond ((and (stringp value) (plusp (length (string-trim '(#\Space) value))))
           (string-trim '(#\Space) value))
          (required (fail "~a is required" (substitute #\_ #\- name)))
          (t nil))))

(defun argument-integer (args name default &key (floor 0) (ceiling 10))
  (let ((value (and (hash-table-p args) (gethash name args))))
    (if (integerp value) (max floor (min ceiling value)) default)))

(defun argument-boolean (args name default)
  (if (and (hash-table-p args) (nth-value 1 (gethash name args)))
      (and (gethash name args) t)
      default))

(defun string-vector (strings)
  (coerce strings 'vector))

(defun scope-of (name)
  (cond ((equal name "user") "user")
        ((member name '("project" "auto") :test #'equal) "project")
        (t "user")))

(defun check-writable ()
  (when (setting :read-only)
    (fail "harnmem is read-only for this session (read_only=true)")))

(defun store-memory (args session-id)
  "One memory written: MERGE into the active record of the same title, else CREATE.
Answers (values ID ACTION)."
  (let* ((type (argument-string args "type"))
         (title (argument-string args "title"))
         (description (argument-string args "description"))
         (content (argument-string args "content"))
         (scope (scope-of (argument-string args "scope" :required nil)))
         (tags (let ((tags (gethash "tags" args)))
                 (if (listp tags) (remove-if-not #'stringp tags) '())))
         (importance (argument-integer args "importance" 5))
         (now (now-ms))
         (existing (find-if (lambda (record)
                              (and (equal (record-status record) "active")
                                   (string-equal (record-title record) title)))
                            (shard-records scope))))
    (unless (member type +memory-types+ :test #'equal)
      (fail "type must be one of: ~{~a~^, ~}" +memory-types+))
    (if existing
        (progn
          (setf (record-description existing) description
                (record-content existing) content
                (record-tags existing) tags
                (record-importance existing) importance
                (record-updated existing) now)
          (write-record-file existing)
          (values (record-id existing) "MERGE"))
        (let ((record (make-record :id (new-memory-id now)
                                   :type type
                                   :scope scope
                                   :project (and (equal scope "project") (project-name))
                                   :title title
                                   :description description
                                   :content content
                                   :tags tags
                                   :status "active"
                                   :importance importance
                                   :confidence 0.7
                                   :pinned nil
                                   :created now
                                   :updated now
                                   :last-used 0
                                   :used-count 0
                                   :source (list (cons :harness "nodecode")
                                                 (cons :session (or session-id ""))))))
          (write-record-file record)
          (values (record-id record) "CREATE")))))

(defun forget-memory (id &key hard)
  "ID out of the store: to trash/ unless HARD, which deletes it."
  (let ((record (find-record id)))
    (unless record (fail "no memory ~a in the store" id))
    (let ((path (record-path (record-scope record) (record-id record))))
      (cond (hard (ignore-errors (delete-file path)) "hard")
            (t (let ((trash (uiop:ensure-directory-pathname (merge-pathnames "trash/" (home)))))
                 (ensure-directories-exist trash)
                 (uiop:rename-file-overwriting-target
                  path (merge-pathnames (format nil "~a-~a" (now-ms) (record-file-name id)) trash))
                 "trash"))))))

(defun json-record (record)
  (nlk:json-object "id" (record-id record)
                    "type" (record-type record)
                    "scope" (record-scope record)
                    "title" (record-title record)
                    "description" (record-description record)
                    "content" (record-content record)
                    "tags" (string-vector (or (record-tags record) '()))
                    "importance" (or (record-importance record) 5)
                    "confidence" (or (record-confidence record) 0.7)
                    "pinned" (and (record-pinned record) t)
                    "status" (record-status record)
                    "created" (or (record-created record) 0)
                    "updated" (or (record-updated record) 0)))

(defun tool-search (args)
  (answering
    (let* ((query (argument-string args "query"))
           (limit (argument-integer args "limit" 8 :floor 1 :ceiling 50))
           (scope (let ((scope (argument-string args "scope" :required nil)))
                    (and scope (member scope '("user" "project") :test #'equal) scope))))
      (multiple-value-bind (hits abstained total)
          (search-records query :limit limit :scope scope)
        (nlk:encode-json-object
         (nlk:json-object
          "ok" t
          "query" query
          "total" total
          "abstained" abstained
          "results"
          (string-vector
           (mapcar (lambda (hit)
                     (let ((record (car hit)))
                       (nlk:json-object
                        "id" (record-id record)
                        "type" (record-type record)
                        "scope" (record-scope record)
                        "title" (record-title record)
                        "description" (record-description record)
                        "snippet" (snippet-of record)
                        "score" (coerce (/ (round (* (cdr hit) 1000)) 1000.0) 'single-float)
                        "status" (record-status record))))
                   hits))))))))

(defun tool-write (args)
  (answering
    (check-writable)
    (multiple-value-bind (id action) (store-memory args nil)
      (nlk:encode-json-object
       (nlk:json-object "ok" t "id" id "action" action
                        "superseded" #() "warnings" #())))))

(defun tool-read (args)
  (answering
    (let ((id (argument-string args "id")))
      (let ((record (find-record id)))
        (unless record (fail "no memory ~a in the store" id))
        (nlk:encode-json-object
         (nlk:json-object "ok" t "record" (json-record record) "backlinks" #()))))))

(defun tool-forget (args)
  (answering
    (check-writable)
    (let* ((id (argument-string args "id"))
           (hard (argument-boolean args "hard" nil))
           (mode (forget-memory id :hard hard)))
      (nlk:encode-json-object (nlk:json-object "ok" t "id" id "mode" mode)))))

(defun shard-status (scope)
  (let ((records (shard-records scope)))
    (nlk:json-object "total" (length records)
                      "active" (count-if (lambda (record)
                                           (equal (record-status record) "active"))
                                         records)
                      "pinned" (count-if #'record-pinned records))))

(defun status-text ()
  "The store in words: what /harnmem answers with no argument."
  (format nil "HarnMem store at ~a: ~d user memories (~d pinned), ~d in project ~a; injection ~a, ~a.~%The tools harnmem_search, harnmem_write, harnmem_read, harnmem_forget and harnmem_status do the rest."
          (string-right-trim "/" (namestring (home)))
          (length (shard-records "user"))
          (count-if #'record-pinned (shard-records "user"))
          (length (shard-records "project"))
          (project-name)
          (if (setting :inject) "on" "off")
          (if (setting :read-only) "read-only" "writable")))

(defun tool-status (args)
  (declare (ignore args))
  (answering
    (nlk:encode-json-object
     (nlk:json-object "ok" t
                      "home" (string-right-trim "/" (namestring (home)))
                      "project" (project-name)
                      "shards" (nlk:json-object "user" (shard-status "user")
                                                 "project" (shard-status "project"))
                      "index" (nlk:json-object "records" (length (active-records)))
                      "health" (nlk:json-object "read_only" (and (setting :read-only) t)
                                                 "inject" (and (setting :inject) t))
                      "extraction" (nlk:json-object "enabled" nil)))))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type
                                                                     "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *search-schema*
  (tool-schema
   "Search the user's long-term cross-session memory (HarnMem). Use it before answering questions about past sessions, previous decisions, preferences or project history."
   (list (list "query" "string" "What to look for (keywords or a question)." t)
          (list "limit" "integer" "Maximum results (default 8)." nil)
          (list "scope" "string" "Restrict to one memory scope: user or project." nil))))

(defparameter *write-schema*
  (tool-schema
   "Store one durable fact in long-term memory: a stable preference, a decision with its rationale, a project convention, or a recurring pitfall. Do not store transient task state or secrets."
   (list (list "type" "string" "Memory kind: user (who they are / preferences), feedback (corrections), project (ongoing work facts), reference (external pointers)." t)
          (list "title" "string" "Short unique title (at most 80 chars)." t)
          (list "description" "string" "One-line summary (at most 200 chars)." t)
          (list "content" "string" "Full memory body, Markdown allowed." t)
          (list "tags" "array" "Optional tags." nil)
          (list "scope" "string" "Store globally (user) or per project (project)." nil)
          (list "importance" "integer" "0-10, higher means recall more often." nil))))

(defparameter *read-schema*
  (tool-schema
   "Read one memory record by id."
   (list (list "id" "string" "Memory id such as mem_01M66272CCFFSKR7X52B." t)
          (list "include_history" "boolean" "Also return superseded versions." nil))))

(defparameter *forget-schema*
  (tool-schema
   "Remove a memory that is wrong or obsolete (moved to trash by default)."
   (list (list "id" "string" "Memory id to forget." t)
          (list "hard" "boolean" "Delete permanently instead of moving to trash." nil))))

(defparameter *status-schema*
  (tool-schema "Show HarnMem store health: memory counts and index state." '()))

;;; --- /harnmem and the manual ---------------------------------------------------------

(defparameter +primer+
  "HarnMem keeps durable memories as plain Markdown files in one store, shared with every
other HarnMem harness: what the user is like, corrections they gave, how this project works,
and pointers elsewhere. A small snapshot of the store rides the standing context at a turn's
first request; harnmem_search reads the rest by keyword.
  harnmem_search {query, limit?, scope?}   the memories that share words with a question
  harnmem_write {type, title, description, content, tags?, scope?, importance?}
  harnmem_read {id}                        one record whole
  harnmem_forget {id, hard?}               move it to the store's trash, or delete it
  harnmem_status {}                        where the store is and what it holds
Write a memory the moment the user states a stable preference, a correction or a decision worth
recalling in a later session; do not store transient task state or secrets. A write with the
same title as an active memory merges into it. The harnmem section of config.jsonc names the
store (home), the project shard project-scoped memories go to, whether the snapshot is
injected, and its budget."
  "What (help :harnmem) answers while the cell runs.")

(defun search-lines (query)
  "QUERY as the lines the operator reads."
  (multiple-value-bind (hits abstained total) (search-records query :limit 8)
    (if (null hits)
        (format nil "No memory of ~d matches ~s." total query)
        (with-output-to-string (out)
          (format out "~d of ~d memories match ~s:" (length hits) total query)
          (dolist (hit hits)
            (format out "~% - [~a] ~a - ~a" (record-id (car hit))
                    (record-title (car hit)) (record-description (car hit))))
          (when abstained (format out "~% (weak matches)"))))))

(defun record-lines (record)
  "One record as the operator reads it."
  (format nil "[~a] ~a~%~a~%~%~a" (record-type record) (record-title record)
          (record-description record) (record-content record)))

(defun run-command (args session-id)
  "One /harnmem invocation: no argument shows the store, `read ID` and
`forget ID` work on one record, anything else searches."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (cond ((zerop (length text)) (status-text))
          ((and (> (length text) 5) (string-equal "read " (subseq text 0 5)))
           (let* ((id (nlk:trimmed (subseq text 5)))
                  (record (find-record id)))
             (if record (record-lines record) (format nil "No memory ~a in the store." id))))
          ((and (> (length text) 7) (string-equal "forget " (subseq text 7)))
           (let ((id (nlk:trimmed (subseq text 7))))
             (handler-case (format nil "Memory ~a moved to ~a." id (forget-memory id))
               (error (condition) (format nil "~a" condition)))))
          (t (search-lines text)))))

;;; --- the cell --------------------------------------------------------------------------

(nle:define-cell harnmem
  (:section ("harnmem")
    (:guide "home is the store every HarnMem harness shares, ~/.harnmem when unset; inject puts a small snapshot at the head of each turn; the tools and /harnmem work without it; next step: /harnmem shows the store")
    ("home" :path
     :doc "the store's home directory (the original's HARNMEM_HOME); ~/.harnmem unless answered")
    ("project" :string :default "nodecode"
     :doc "the project shard project-scoped memories are written to, under memories/projects/")
    ("inject" :boolean :default t
     :doc "put a small snapshot of the store at the head of every turn")
    ("max_inject_tokens" :integer :default 1200 :min 50
     :doc "the token budget the snapshot's lines are counted against (four characters to a token)")
    ("max_inject_chars" :integer :default 10000 :min 200
     :doc "a hard character cap on the snapshot")
    ("hot_entries" :integer :default 20 :min 1
     :doc "how many memories the snapshot may carry")
    ("read_only" :boolean :default nil
     :doc "never write from this profile: harnmem_write and harnmem_forget refuse"))
  (:help :harnmem "harnmem: memories kept as Markdown in one store shared with the other HarnMem harnesses; harnmem_search, harnmem_write, harnmem_read, harnmem_forget, harnmem_status" +primer+)
  (:tool "harnmem_search" *search-schema* 'tool-search)
  (:tool "harnmem_write" *write-schema* 'tool-write)
  (:tool "harnmem_read" *read-schema* 'tool-read)
  (:tool "harnmem_forget" *forget-schema* 'tool-forget)
  (:tool "harnmem_status" *status-schema* 'tool-status)
  (:command "harnmem" 'run-command
            :description "Show the HarnMem store, search it, or read and forget one memory"
            :argument-hint "QUERY | read ID | forget ID")
  (:hook 'nle::read-harness-sections #'snapshot-section)
  (:start (lambda ()
            (let ((home (home)))
              (when (and (probe-file home) (not (uiop:directory-exists-p home)))
                (fail "harnmem.home is ~a: that is a file, not the store directory" (namestring home)))
              (ensure-store)))))
