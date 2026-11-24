;;;; common.lisp --- paths, JSON, redaction and the marker every build carries.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything the cell writes lives under one data directory
;;;; (~/.nodecode/mcp-creator by default): snapshots/ what introspection saw,
;;;; builds/ what scaffold wrote, calls.jsonl one line per MCP call the model
;;;; made through eval. A build folder is recognised by its marker file, so a
;;;; scaffold never empties a folder it did not make.

(in-package #:nodecode-mcp-creator)

;; MCP-CREATOR-ERROR and FAIL are NLK:DEFINE-PERIPHERAL's, in package.lisp:
;; one refusal type for the whole cell, rendered as ERROR: <detail>.

(defparameter *config* nil
  "The whole parsed shared config this cell was started on; NIL before start.")

(defparameter *servers* nil
  "The mcp.servers map as read at start: name -> entry hash. Tools read it.")

(defparameter *data-dir* nil
  "Where this cell writes: snapshots, builds, calls.jsonl. Set at start.")

(defparameter *now* nil
  "Universal time a build stamps with; NIL means now. Bound in tests.")

(defparameter *marker* ".mcp-creator"
  "The file that makes a folder this cell's own to replace.")

(defparameter +snapshot-chars+ 400000
  "The most of one snapshot kept on disk; past it the tools list is cut.")

(defun now ()
  "Universal time, or the test's fixed clock."
  (or *now* (get-universal-time)))

(defun now-iso ()
  (nlk:iso-universal (now)))

;;; --- paths -----------------------------------------------------------------

(defun data-path (&rest parts)
  "PARTS under the data directory, as a pathname; parents are made by the writer."
  (unless *data-dir* (fail "the cell is not running"))
  (merge-pathnames (format nil "~{~a~^/~}" (mapcar #'string parts))
                   (uiop:ensure-directory-pathname *data-dir*)))

(defun builds-root ()
  (merge-pathnames "builds/" (uiop:ensure-directory-pathname *data-dir*)))

(defun build-path (name)
  "The build folder for NAME: builds/<name>, one per plugin name."
  (merge-pathnames (format nil "~a/" name) (builds-root)))

(defun safe-name-p (name)
  "A plugin name as a folder: lowercase letters, digits, dash, dot, underscore."
  (and (stringp name)
       (<= 1 (length name) 60)
       (every (lambda (char) (or (alphanumericp char) (member char '(#\- #\. #\_))))
              name)
       (not (member name '("." "..") :test #'string=))
       (alphanumericp (char name 0))))

(defun ensure-name (name &key (what "name"))
  (let ((name (nlk:trimmed (princ-to-string (or name "")))))
    (unless (safe-name-p name)
      (fail "~a ~s is not lowercase letters, digits, dash, dot or underscore" what name))
    name))

;;; --- files -----------------------------------------------------------------

(defun write-text (path text)
  "Write TEXT to PATH as UTF-8, making its parents; answers the namestring."
  (let ((path (pathname path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create :external-format :utf-8)
      (write-string text out)))
  (namestring path))

(defun read-text (path)
  (with-open-file (in path :external-format :utf-8)
    (let ((text (make-string (file-length in))))
      (subseq text 0 (read-sequence text in)))))

(defun file-exists-p (path)
  (and (probe-file path) t))

(defun marker-path (folder)
  (merge-pathnames *marker* (uiop:ensure-directory-pathname folder)))

(defun markers-own-p (folder)
  "Whether FOLDER carries this cell's marker: only then may it be replaced."
  (file-exists-p (marker-path folder)))

;;; --- JSON -------------------------------------------------------------------

(defun decode (text)
  "TEXT as a JSON value; a table for an object. A refusal for anything else."
  (let ((value (handler-case (nlk:decode-json text)
                 (error () (fail "not JSON: ~a" (nlk:one-line text :cap 120))))))
    (if (hash-table-p value)
        value
        (fail "not a JSON object: ~a" (nlk:one-line text :cap 120)))))

(defun field (object key)
  "KEY of OBJECT when OBJECT is a table."
  (and (hash-table-p object) (gethash key object)))

(defun member-text (object key)
  (let ((value (field object key)))
    (typecase value
      (string value)
      (null "")
      (t (princ-to-string value)))))

(defun members (object)
  "The keys of OBJECT, sorted."
  (when (hash-table-p object)
    (sort (loop for key being the hash-keys of object collect key) #'string<)))

(defun json-string (object)
  "OBJECT as one JSON line."
  (if (stringp object)
      object
      (with-output-to-string (out) (shasht:write-json object out))))

(defun object (&rest pairs)
  "A JSON object from PAIRS of key and value, nil values left out."
  (let ((object (nlk:make-json-object)))
    (loop for (key value) on pairs by #'cddr
          when value do (setf (gethash key object) value))
    object))

(defun json-of (value)
  "VALUE as the JSON a tool answers: a string from the original's vocabulary."
  (json-string value))

;;; --- redaction ----------------------------------------------------------------

(defparameter +secret-prefixes+ '("sk-" "sk_live_" "ghp_" "gho_" "github_pat_" "xoxb-"
                                  "xoxp-" "AKIA" "ASIA" "eyJ" "Bearer " "bot")
  "How a token in the clear announces itself. A value under one is masked.")

(defun token-looking-p (word)
  "Whether WORD looks like a credential: a known prefix, or a long run of
base64-ish characters with digits in it."
  (cond
    ((some (lambda (prefix) (and (<= (length prefix) (length word))
                                 (string-equal prefix word :end2 (length prefix))))
           +secret-prefixes+)
     (values t (if (and (plusp (length word)) (char= (char word 0) #\B)) 7 0)))
    ((and (>= (length word) 24)
          (every (lambda (char) (or (alphanumericp char)
                                     (member char '(#\- #\_ #\+ #\/ #\= #\.))))
                 word)
          (find-if #'digit-char-p word)
          (find-if #'alpha-char-p word))
     (values t 0))
    (t nil)))

(defun mask (word)
  "WORD with the secret part of it replaced: the scheme and the last four kept."
  (multiple-value-bind (secret-p skip) (token-looking-p word)
    (if (not secret-p)
        word
        (let ((tail (if (>= (length word) 4) (subseq word (- (length word) 4)) "")))
          (format nil "~a[redacted]~a" (subseq word 0 skip) tail)))))

(defun email-looking-p (word)
  (and (find #\@ word)
       (let ((at (position #\@ word)))
         (and (plusp at)
              (< at (1- (length word)))
              (find #\. word :start at)))))

(defun path-looking-p (word)
  (and (plusp (length word))
       (let ((stripped (string-trim "(\"',;)" word)))
         (and (plusp (length stripped))
              (char= (char stripped 0) #\/)
              (find #\/ stripped :start 1)))))

(defun redact (text &key paths)
  "TEXT with credentials, emails and (when PATHS) absolute file paths masked.
The original redacts on the way into its log and its mined output; so does
this, so a publish build carries no personal detail."
  (let ((out (make-string-output-stream)))
    (loop with start = 0
          for space = (position #\Space text :start start)
          do (let* ((end (or space (length text)))
                    (word (subseq text start end))
                    (clean (cond ((or (email-looking-p word)) "[redacted]")
                                 ((or (null paths)) (mask word))
                                 ((path-looking-p word) "[path]")
                                 (t (mask word)))))
               (write-string clean out)
               (when space (write-char #\Space out))
               (setf start (if space (1+ space) (length text))))
          while space)
    (get-output-stream-string out)))

(defun private-findings (text &key paths)
  "What in TEXT a publish build must not carry: (:what :sample) pairs, sample redacted."
  (let ((findings '()))
    (loop with start = 0
          for space = (position #\Space text :start start)
          for end = (or space (length text))
          for word = (subseq text start end)
          do (cond ((email-looking-p word)
                    (push (list :what "an email address" :sample "[redacted]") findings))
                   ((token-looking-p word)
                    (push (list :what "a credential-shaped value" :sample (mask word)) findings))
                   ((and paths (path-looking-p word))
                    (push (list :what "an absolute path" :sample "[path]") findings)))
             (setf start (if space (1+ space) (length text)))
          while space)
    findings))

;;; --- the call log -------------------------------------------------------------

(defun call-log-path ()
  (data-path "calls.jsonl"))

(defun note-call (name &key ok detail)
  "Append one call to the log: a tool NAME, whether it failed, DETAIL redacted."
  (ignore-errors
    (nlk:append-json-line (call-log-path)
                           (object "at" (now) "tool" name "ok" (and ok t)
                                   "detail" (and detail (nlk:one-line (redact detail) :cap 200))))))

(defun call-log ()
  "Every logged call, oldest first: plists of :at :tool :ok :detail."
  (let ((path (call-log-path)))
    (when (file-exists-p path)
      (mapcar (lambda (line)
                (list :at (nlk:json-value line :integer "at")
                      :tool (nlk:json-value line :string "tool")
                      :ok (nlk:json-value line :boolean "ok")
                      :detail (nlk:json-value line :string "detail")))
              (nlk:read-json-lines path)))))
