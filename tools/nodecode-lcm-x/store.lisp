;;;; store.lisp --- the store: one JSON line per block, one file per session.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; LCM-X keeps every turn in a recoverable store, and nothing a turn said
;;;; is lost to compression. Here that store is the simplest shape that keeps
;;;; the promise without the original's DAG: a session's blocks are an
;;;; append-only file, each block one JSON line carrying its seq, its time,
;;;; its turn, its role and its text verbatim. A block's reference is
;;;; lcm:<session>:<seq> -- the original's lcm:<store>:<start>-<end>, spelled
;;;; for a store that holds one block a line.

(in-package #:nodecode-lcm-x)

;; The cell's own condition, FAIL, settings variable and RUNNING-SETTINGS:
;; declared here because this is the first file whose functions refuse.
(nlk:define-peripheral lcm-x :not-running t)

(defparameter +suffix+ ".jsonl"
  "A session's block file ends this way: one line per block.")

;;; --- where a session's blocks live -----------------------------------------------

(defun store-directory (dir)
  "The store's directory: DIR, under the Nodecode home, as a directory pathname."
  (uiop:ensure-directory-pathname (nlk:home dir)))

(defun session-label (session)
  "SESSION as a file name: letters, digits, dash and underscore kept, anything
else a dash. NIL is the sessionless store."
  (let* ((text (or session ""))
         (label (map 'string
                     (lambda (character)
                       (if (or (alphanumericp character)
                               (member character '(#\- #\_)))
                           character
                           #\-))
                     text)))
    (if (zerop (length label)) "local" label)))

(defun session-file (dir session)
  "The file SESSION's blocks are kept in, under DIR's store directory."
  (merge-pathnames (format nil "~a~a" (session-label session) +suffix+)
                   (store-directory dir)))

(defun store-sessions (dir)
  "Every session the store under DIR holds a file for, by label, sorted."
  (let ((directory (store-directory dir)))
    (sort (if (probe-file directory)
              (loop for file in (uiop:directory-files directory (format nil "*~a" +suffix+))
                    collect (pathname-name file))
              '())
          #'string<)))

;;; --- one block, as a line ---------------------------------------------------------

(defparameter +fields+
  '((:seq . "seq") (:at . "at") (:turn . "turn") (:role . "role")
    (:name . "name") (:text . "text"))
  "A block's members and their wire names: the line a block is kept as.")

(defun encode-block (block)
  "BLOCK, a plist of +FIELDS+, as the one JSON line it is kept as."
  (nlk:encode-json-object
   (apply #'nlk:make-json-object
          (loop for (key . wire) in +fields+
                append (list wire (or (getf block key) ""))))))

(defun decode-block (line)
  "LINE as a block plist, or NIL for a line that is not one."
  (let ((object (ignore-errors (nlk:decode-json line))))
    (when (hash-table-p object)
      (list :seq (or (nlk:json-value object :integer "seq") 0)
            :at (or (nlk:json-value object :string "at") "")
            :turn (or (nlk:json-value object :string "turn") "")
            :role (or (nlk:json-value object :string "role") "")
            :name (or (nlk:json-value object :string "name") "")
            :text (or (nlk:json-value object :string "text") "")))))

(defun read-blocks (dir session)
  "Every block SESSION holds under DIR's store, oldest first."
  (let ((file (session-file dir session)))
    (if (probe-file file)
        (with-open-file (in file :direction :input :external-format :utf-8
                                 :if-does-not-exist nil)
          (loop for line = (read-line in nil nil)
                while line
                for block = (decode-block line)
                when block collect block))
        '())))

(defun count-lines (file)
  "How many lines FILE holds, or 0 when it is not there."
  (if (probe-file file)
      (with-open-file (in file :direction :input :external-format :utf-8
                               :if-does-not-exist nil)
        (loop for line = (read-line in nil nil) while line count line))
      0))

(defun append-block (dir session block)
  "Write BLOCK as SESSION's newest line; answer BLOCK."
  (let ((file (session-file dir session)))
    (ensure-directories-exist file)
    (with-open-file (out file :direction :output :if-exists :append
                              :if-does-not-exist :create :external-format :utf-8)
      (write-line (encode-block block) out))
    block))

(defvar *counts* (make-hash-table :test #'equal)
  "How many blocks each store file held when this process last appended, by
\"DIR|SESSION\": the file is counted once, and every later block takes the
next number without reading the file back.")

(defun next-seq (dir session)
  "The seq SESSION's next block under DIR takes: one past the lines the file
already holds."
  (let ((key (format nil "~a|~a" (store-directory dir) (session-label session))))
    (setf (gethash key *counts*)
          (1+ (or (gethash key *counts*)
                  (count-lines (session-file dir session)))))))

;;; --- finding a block again ---------------------------------------------------

(defun block-ref (session block)
  "BLOCK of SESSION as the reference lcm_expand reads it back by."
  (format nil "lcm:~a:~d" session (getf block :seq)))

(defun block-matches-p (block words)
  "Whether BLOCK's text carries every one of WORDS, case-insensitively."
  (let ((text (string-downcase (getf block :text))))
    (every (lambda (word) (search (string-downcase word) text)) words)))

(defun hit-newer-p (first second)
  "Whether hit FIRST (SESSION . BLOCK) is newer than SECOND: by time, then by seq."
  (let ((at-first (getf (cdr first) :at))
        (at-second (getf (cdr second) :at)))
    (if (string= at-first at-second)
        (> (getf (cdr first) :seq) (getf (cdr second) :seq))
        (string> at-first at-second))))

(defun search-store (dir sessions words limit)
  "The newest LIMIT blocks of SESSIONS under DIR carrying every one of WORDS,
as ((SESSION . BLOCK) ...) newest first."
  (let ((hits '()))
    (dolist (session sessions)
      (dolist (block (read-blocks dir session))
        (when (block-matches-p block words)
          (push (cons session block) hits))))
    (let ((hits (sort hits #'hit-newer-p)))
      (if (> (length hits) limit) (subseq hits 0 limit) hits))))
