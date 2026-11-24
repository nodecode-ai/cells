;;;; store.lisp --- what the cell keeps between boots: the grades, and the ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Two files under the cell's own directory (<dir>/records.sexp and
;;;; <dir>/ledger.jsonl), never beside the definitions they judge. The
;;;; records are a readable list -- this cell's own data, written and read
;;;; back by one pair of functions. The ledger is append-only JSON lines,
;;;; each entry carrying its fields as one canonical line, the sha256 of the
;;;; one before it and of its own line, so a line edited or removed in the
;;;; middle is visible (/witness verify).

(in-package #:nodecode-witness)

(defparameter +genesis+ (make-string 64 :initial-element #\0)
  "The head a ledger with no entries starts from: 64 zeroes.")

(defvar *records* nil
  "skill name -> record plist, loaded at start: (:grade :score :findings
:positive :status :rel :qpath :uses :ok :fail :created).")

(defvar *trace* '()
  "Newest first, at most 200: (:tool NAME :keys (...)). Tool names and
argument KEYS only, never a value -- the debug trace /witness trace prints.")

;;; --- where the cell keeps its own --------------------------------------------------------

(defun witness-directory ()
  "The folder the cell writes: the section's dir, else witness/ under this home."
  (let ((dir (setting :dir)))
    (uiop:ensure-directory-pathname
     (if (and (stringp dir) (plusp (length (string-trim " " dir))))
         (pathname dir)
         (nlk:home "witness/")))))

(defun records-path ()
  (merge-pathnames "records.sexp" (witness-directory)))

(defun ledger-path ()
  (merge-pathnames "ledger.jsonl" (witness-directory)))

;;; --- the records ------------------------------------------------------------------------

(defun records ()
  (or *records* (setf *records* (load-records))))

(defun load-records ()
  "The records file as a hash of name -> record; an unreadable file reads as empty."
  (let ((table (make-hash-table :test 'equal))
        (path (records-path)))
    (when (probe-file path)
      (handler-case
          (let ((*read-eval* nil))
            (with-open-file (in path :direction :input)
              (loop for (name . record) in (with-standard-io-syntax (read in nil nil))
                    do (setf (gethash name table) record))))
        (error () nil)))
    table))

(defun save-records ()
  (ensure-directories-exist (records-path))
  (let ((path (records-path))
        (data (loop for name being the hash-keys of (records) using (hash-value record)
                    collect (cons name record))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (with-standard-io-syntax
        (let ((*print-readably* t) (*print-pretty* t) (*package* (find-package :cl-user)))
          (prin1 data out)
          (terpri out))))
    path))

(defun record-for (name)
  (gethash name (records)))

(defun (setf record-for) (record name)
  (setf (gethash name (records)) record)
  (save-records)
  record)

;;; --- the ledger -------------------------------------------------------------------------

(defun ledger-entries ()
  "The ledger's entries, oldest first, each a JSON object read back; a line
that does not read answers NIL in its place."
  (let ((path (ledger-path)))
    (when (probe-file path)
      (with-open-file (in path :direction :input)
        (loop for line = (read-line in nil nil)
              while line
              unless (zerop (length (string-trim " " line)))
                collect (handler-case (shasht:read-json line) (error () nil)))))))

(defun ledger-line (fields)
  "FIELDS, a plist of scalars, as one canonical line: key=value; in the
order given, so the hash never depends on a reader's key order."
  (format nil "~{~a~^;~}"
            (loop for (key value) on fields by #'cddr
                  collect (format nil "~a=~a" (string-downcase (symbol-name key)) value))))

(defun ledger-append (fields)
  "Append FIELDS, a plist of scalars, to the chained ledger. => the entry."
  (let* ((entries (ledger-entries))
         (last-entry (car (last entries)))
         (prev (or (and (hash-table-p last-entry) (gethash "hash" last-entry)) +genesis+))
         (line (ledger-line (append fields (list :seq (1+ (length entries))))))
         (entry (make-hash-table :test #'equal)))
    (setf (gethash "body" entry) line
          (gethash "prev" entry) prev
          (gethash "hash" entry) (nlk:sha256-text (concatenate 'string prev line)))
    (ensure-directories-exist (ledger-path))
    (with-open-file (out (ledger-path) :direction :output :if-exists :append
                                          :if-does-not-exist :create)
      (format out "~a~%"
              (with-output-to-string (s)
                (let ((*print-pretty* nil)) (shasht:write-json entry s)))))
    entry))

(defun ledger-verify (&optional expect-head)
  "Check the chain whole. => (values OK MESSAGE); EXPECT-HEAD, when given,
must be the prefix of the head the chain ends on."
  (let ((prev +genesis+)
        (count 0)
        (broken nil)
        (broken-why nil))
    (dolist (entry (ledger-entries))
      (unless broken
        (let ((body (and (hash-table-p entry) (gethash "body" entry))))
          (if (or (not (stringp body))
                  (not (string= (or (gethash "prev" entry) "") prev))
                  (not (string= (nlk:sha256-text (concatenate 'string prev body))
                                (or (gethash "hash" entry) ""))))
              (setf broken (1+ count)
                    broken-why (format nil "seq ~a: ~s" (1+ count) entry))
              (progn (setf prev (gethash "hash" entry))
                     (incf count))))))
    (cond (broken (values nil (format nil "chain broken at entry #~a: ~a" (1+ broken) broken-why)))
          ((and expect-head (not (uiop:string-prefix-p expect-head prev)))
           (values nil "head hash does not match your anchor (entries removed or rewritten?)"))
          (t (values t (format nil "~a entries, head ~a" count (subseq prev 0 (min 23 (length prev)))))))))

(defun trace-text ()
  "The last hook calls, newest first: tool names and argument keys only."
  (if *trace*
      (format nil "~{~a~^~%~}"
              (loop for row in (subseq *trace* 0 (min 25 (length *trace*)))
                    collect (format nil "~a  keys=~{~a~^,~}"
                                    (getf row :tool) (getf row :keys))))
      "no hook calls recorded yet"))
