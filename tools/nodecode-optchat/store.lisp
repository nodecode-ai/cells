;;;; store.lisp --- the append-only message log (the original's store.py).
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Layout: <home>/optchat/main/YYYY-MM-DD.jsonl, one JSON object a line,
;;;; {"i": 0, "kind": "user", "text": "...", "date": "...", "session": "..."}.
;;;; The index in *ENTRIES* is the message id, as the file's "i" is: the log
;;;; is append-only, so position is identity. Nothing is ever edited or
;;;; deleted; a torn line (a crash mid-write) is skipped at load, as the
;;;; original skips it.
;;;;
;;;; A file this cell creates is owner-only (0600): one shared home never
;;;; exposes the transcript to another account.

(in-package #:nodecode-optchat)

;;; --- where it lives ---------------------------------------------------------------

(defun data-root ()
  "The folder this memory lives in: optchat.store_dir under the home."
  (uiop:ensure-directory-pathname (nlk:home (or *directory* "optchat"))))

(defun day-folder (kind)
  "The folder for one sort of line -- \"main\" for the log, \"tree\" for the
nodes -- created when absent."
  (ensure-directories-exist (merge-pathnames (format nil "~a/" kind) (data-root))))

(defun today-name ()
  "Today, as a file is named: 2026-11-08, in the operator's own time."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun iso-now ()
  "Now, written the way a log line carries it."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun day-files (folder)
  "The .jsonl files in FOLDER, oldest first by name (a name is its date)."
  (sort (directory (merge-pathnames "*.jsonl" (uiop:ensure-directory-pathname folder)))
        #'string< :key #'namestring))

(defun append-line (path line)
  "Append LINE and a newline to PATH, created owner-only; answers PATH."
  (ensure-directories-exist path)
  (let ((new (not (probe-file path))))
    (with-open-file (out path :direction :output :if-exists :append
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string line out)
      (terpri out)
      (finish-output out))
    (when new
      #+sbcl (ignore-errors (sb-posix:chmod (uiop:native-namestring path) #o600)))
    path))

;;; --- the log's warm copy ------------------------------------------------------------

(defun line-entry (line)
  "The ENTRY one decoded line carries, or NIL for a torn line."
  (let ((object (ignore-errors (nlk:decode-json line))))
    (when (hash-table-p object)
      (make-entry (or (nlk:json-value object :string "kind") "note")
                  (or (nlk:json-value object :string "text") "")
                  (or (nlk:json-value object :string "date") "")
                  (or (nlk:json-value object :string "session") "")))))

(defun load-log ()
  "Read every day file into *ENTRIES*, oldest first; answers the count."
  (with-optchat-lock
    (setf *entries* (make-array 0 :adjustable t :fill-pointer 0))
    (dolist (path (day-files (day-folder "main")))
      (with-open-file (in path :external-format :utf-8)
        (loop for line = (read-line in nil nil)
              while line
              do (let ((entry (line-entry line)))
                   (when entry (vector-push-extend entry *entries*))))))
    (length *entries*)))

(defun remember (kind text session)
  "Append one message of KIND with TEXT to the log and to *ENTRIES*; answers
its permanent id."
  (unless (member kind +kinds+ :test #'equal)
    (fail "~s is no memory kind: ~{~a~^, ~}" kind +kinds+))
  (with-optchat-lock
    (let* ((id (length *entries*))
           (date (iso-now))
           (path (merge-pathnames (format nil "~a.jsonl" (today-name)) (day-folder "main")))
           (line (nlk:encode-json-object
                  (nlk:make-json-object "i" id "kind" kind "text" (or text "")
                                         "date" date "session" (or session "")))))
      (append-line path line)
      (vector-push-extend (make-entry kind (or text "") date (or session "")) *entries*)
      (setf *dirty* t)
      id)))

(defun remembered ()
  "How many messages this home's log holds."
  (length *entries*))

(defun entry-at (id)
  "Message ID, or NIL when the log has none."
  (when (and (integerp id) (<= 0 id) (< id (length *entries*)))
    (aref *entries* id)))

;;; --- what a message reads as ----------------------------------------------------------

(defun flatten (text)
  "TEXT on one line: newlines shown as spaces, as a view line renders it."
  (substitute #\Space #\Newline (or text "")))

(defun source-text (entry)
  "ENTRY as a level-0 node reads: KIND: TEXT, the message itself."
  (format nil "~a: ~a" (entry-kind entry) (entry-text entry)))

(defun cap-echo (text)
  "TEXT cut to the head and tail that fit CAP, with the count of what went
(the original's _cap_echo): a tool result is kept, never quoted whole."
  (let ((text (or text "")))
    (if (<= (length text) +cap-chars+)
        text
        (let ((head (floor (* 3 +cap-chars+) 4))
              (tail (- +cap-chars+ (floor (* 3 +cap-chars+) 4))))
          (format nil "~a~%...[optchat: cut ~d chars]...~%~a"
                  (subseq text 0 head)
                  (- (length text) +cap-chars+)
                  (subseq text (- (length text) tail)))))))
