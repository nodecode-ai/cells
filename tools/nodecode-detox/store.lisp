;;;; store.lisp --- evidence kept as files under the home, one record each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original kept its evidence in a local SQLite database. This rewrite
;;;; keeps one text file and one metadata file per record, in a folder under
;;;; the organism's own home, so a profile's evidence stays with the profile:
;;;; no database, no native library, and the operator can read, grep or delete
;;;; a record with the tools they already have.
;;;;
;;;; A record is two files named by its evidence id:
;;;;   ev_<12 hex>.txt    the stored text, redacted before it is written
;;;;   ev_<12 hex>.meta   eight tab-separated fields, one line: the tool,
;;;;                      the content type, the status, characters, lines,
;;;;                      the session, when it was kept (universal time), and
;;;;                      how many characters the report that replaced it took
;;;; The id is random and unguessable, and is what a report prints; knowing
;;;; one is what lets a later call read that record back.

(in-package #:nodecode-detox)

(defparameter +store-folder+ "detox/"
  "The folder under the home this cell keeps evidence in.")

(defparameter +evidence-prefix+ "ev_"
  "What every evidence id starts with, as the original's did.")

(defun store-directory ()
  "The evidence folder, under the home this organism runs in."
  (nlk:home +store-folder+))

(defun record-path (id &optional (type "txt"))
  "The path of ID's text file, or of its metadata file when TYPE is \"meta\"."
  (merge-pathnames (format nil "~a.~a" id type) (store-directory)))

(defun directory-files (wildcard)
  "Every file WILDCARD names in the store, or NIL when the store is not there."
  (handler-case (directory wildcard) (error () nil)))

(defun evidence-id-p (id)
  "Whether ID names a record and nothing else: the prefix and hex digits alone.
The check is what keeps a name read from a model's call or a report out of a path."
  (and (stringp id)
       (> (length id) (length +evidence-prefix+))
       (uiop:string-prefix-p +evidence-prefix+ id)
       (every (lambda (digit) (digit-char-p digit 16)) (subseq id (length +evidence-prefix+)))))

(defun evidence-ids ()
  "Every stored evidence id, the newest record first."
  (let ((paths (remove-if-not (lambda (path) (evidence-id-p (pathname-name path)))
                               (directory-files
                                (merge-pathnames (concatenate 'string +evidence-prefix+ "*.txt")
                                                 (store-directory))))))
    (mapcar #'pathname-name
            (sort paths #'> :key (lambda (path)
                                   (or (ignore-errors (file-write-date path)) 0))))))

(defun fresh-evidence-id ()
  "An id no record holds yet: twelve random hex digits after the prefix."
  (loop for id = (string-downcase
                  (format nil "~a~12,'0x" +evidence-prefix+ (random (expt 16 12))))
        unless (probe-file (record-path id))
          return id))

;;; --- the text, as lines ----------------------------------------------------

(defun text-lines (text)
  "TEXT as lines, a trailing newline ending the last line rather than starting an empty one."
  (if (zerop (length text))
      '()
      (let ((lines '())
            (start 0))
        (loop
          (let ((at (position #\Newline text :start start)))
            (cond (at (push (string-right-trim '(#\Return) (subseq text start at)) lines)
                      (setf start (1+ at)))
                  (t (push (string-right-trim '(#\Return) (subseq text start)) lines)
                     (return)))))
        (let ((lines (nreverse lines)))
          (if (and (cdr lines) (zerop (length (car (last lines)))))
              (butlast lines)
              lines)))))

(defun text-line-count (text)
  "How many lines TEXT holds."
  (length (text-lines text)))

;;; --- one metadata line -----------------------------------------------------

(defun meta-field (value)
  "VALUE as one metadata field: no tab and no newline, so the line stays one line."
  (substitute #\Space #\Tab
              (substitute #\Space #\Newline (princ-to-string (or value "")))))

(defun split-tabs (line)
  "LINE cut at its tabs, an empty field kept as one."
  (let ((fields '())
        (start 0))
    (loop
      (let ((at (position #\Tab line :start start)))
        (cond (at (push (subseq line start at) fields)
                  (setf start (1+ at)))
              (t (push (subseq line start) fields)
                 (return)))))
    (nreverse fields)))

(defun save-evidence (text &key tool content-type session status)
  "TEXT kept under a fresh evidence id; the metadata line follows once the report is built.
=> the id, or NIL when nothing could be written, which the caller reads as fail open."
  (handler-case
      (with-detox-lock
        (ensure-directories-exist (store-directory))
        (let ((id (fresh-evidence-id)))
          (with-open-file (out (record-path id)
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create
                                :external-format :utf-8)
            (write-string text out))
          (write-meta id :tool tool :type content-type :session session :status status
                         :chars (length text) :lines (text-line-count text) :report 0)
          id))
    (error () nil)))

(defun write-meta (id &key tool type session status chars lines report)
  "Write ID's metadata line; an error leaves the record without one, never a broken line."
  (handler-case
      (with-open-file (out (record-path id "meta")
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create
                            :external-format :utf-8)
        (format out "~a	~a	~a	~d	~d	~a	~d	~d~%"
                (meta-field tool) (meta-field type) (meta-field status)
                (or chars 0) (or lines 0) (meta-field session)
                (get-universal-time) (or report 0)))
    (error () nil)))

(defun record-meta (id)
  "ID's metadata as a plist (:tool :type :status :chars :lines :session :at :report), or NIL."
  (let ((path (record-path id "meta")))
    (when (probe-file path)
      (handler-case
          (with-open-file (in path :external-format :utf-8)
            (let ((fields (split-tabs (read-line in nil ""))))
              (when (>= (length fields) 8)
                (destructuring-bind (tool type status chars lines session at report) fields
                  (list :tool tool :type type :status status
                        :chars (parse-integer chars :junk-allowed t)
                        :lines (parse-integer lines :junk-allowed t)
                        :session session
                        :at (parse-integer at :junk-allowed t)
                        :report (parse-integer report :junk-allowed t))))))
        (error () nil)))))

(defun record-text (id)
  "ID's stored text, or NIL when no record holds it."
  (let ((path (record-path id)))
    (when (probe-file path)
      (handler-case (uiop:read-file-string path :external-format :utf-8) (error () nil)))))

(defun record-bytes (id)
  "ID's text file in bytes."
  (or (ignore-errors
        (with-open-file (in (record-path id) :element-type '(unsigned-byte 8))
          (file-length in)))
      0))

(defun delete-record (id)
  "Remove ID's text and metadata. => T when neither is there any more."
  (if (evidence-id-p id)
      (progn (ignore-errors (delete-file (record-path id)))
             (ignore-errors (delete-file (record-path id "meta")))
             (not (probe-file (record-path id))))
      nil))

;;; --- how much is kept, and for how long -------------------------------------

(defun store-bytes ()
  "How many bytes the evidence folder holds."
  (reduce #'+ (mapcar #'record-bytes (evidence-ids)) :initial-value 0))

(defun purge-older-than (days)
  "Remove every record older than DAYS days. => how many went."
  (let ((cutoff (- (get-universal-time) (* 86400 (max 1 days)))))
    (count-if #'delete-record
              (loop for id in (evidence-ids)
                    for at = (ignore-errors (file-write-date (record-path id)))
                    when (and at (< at cutoff)) collect id))))

(defun enforce-size-cap (megabytes)
  "Delete the oldest records until the store holds at most MEGABYTES. => how many went."
  (let ((cap (* (max 5 megabytes) 1024 1024))
        (gone 0))
    (loop for id in (reverse (evidence-ids))
          while (> (store-bytes) cap)
          do (when (delete-record id) (incf gone)))
    gone))
