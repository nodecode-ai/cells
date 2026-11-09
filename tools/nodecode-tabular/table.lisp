;;;; table.lisp --- delimited text read and written in Lisp.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The profile and the split both start from one parsed table, and neither
;;;; needs a Python runtime to get it: RFC 4180 quoting (a quoted field may
;;;; hold the separator, a doubled quote, a line break), and a writer that
;;;; quotes back exactly what needs it. Nothing here knows what a column means.

(in-package #:nodecode-tabular)

(defparameter *missing-tokens* '("" "na" "n/a" "nan" "null" "none" "missing" "n.a." "-")
  "A cell whose trimmed, downcased text is one of these reads as missing.
The list is the profile skill's, with `-` read as a blank.")

(defun missing-text-p (text)
  "Whether TEXT, a trimmed cell, is a missing value."
  (member (string-downcase text) *missing-tokens* :test #'string=))

(defstruct (table (:constructor %make-table (columns rows separator))
                  (:conc-name "TABLE."))
  "One parsed file: COLUMNS a vector of header strings, ROWS a list of
vectors of cell strings in file order, SEPARATOR the character they were
split on."
  (columns (vector) :type vector :read-only t)
  (rows '() :type list :read-only t)
  (separator #\, :type character :read-only t))

(defun parse-delimited (text separator)
  "TEXT split into rows of fields on SEPARATOR, RFC 4180 quoting honoured: a
quote opens a field only at its start, a doubled quote inside quotes is one
quote, and a line break inside quotes does not end the row. A trailing
newline ends no extra row. => (values COLUMNS ROWS)."
  (let ((rows '()) (fields '()) (field (make-string-output-stream))
        (quoted nil) (fresh t) (started nil) (n (length text)) (i 0))
    (labels ((end-field ()
             (push (get-output-stream-string field) fields)
             (setf started t fresh t))
           (end-row ()
             (end-field)
             (push (coerce (nreverse fields) 'vector) rows)
             (setf fields '() fresh t started nil)))
      (loop while (< i n)
            for ch = (char text i)
            do (cond
                 (quoted
                  (cond ((char= ch #\")
                         (if (and (< (1+ i) n) (char= (char text (1+ i)) #\"))
                             (progn (write-char #\" field) (incf i))
                             (setf quoted nil)))
                        (t (write-char ch field))))
                 ((and (char= ch #\") fresh) (setf quoted t fresh nil started t))
                 ((char= ch separator) (end-field))
                 ((char= ch #\Newline) (end-row))
                 ((char= ch #\Return) nil)
                 (t (write-char ch field) (setf fresh nil started t)))
            do (incf i))
      (when started (end-row)))
    (let ((rows (nreverse rows)))
      (if rows
          (values (first rows) (rest rows))
          (values (vector) '())))))

(defun read-table (path &key (separator #\,))
  "PATH parsed as delimited text on SEPARATOR."
  (let ((text (uiop:read-file-string path :external-format :utf-8)))
    ;; A byte-order mark is not a column name.
    (when (and (plusp (length text)) (char= (char text 0) #\uFEFF))
      (setf text (subseq text 1)))
    (multiple-value-bind (columns rows) (parse-delimited text separator)
      (%make-table columns rows separator))))

(defun quote-field (text separator)
  "TEXT as one field on SEPARATOR: quoted only when it must be."
  (if (or (find separator text) (find #\" text) (find #\Newline text) (find #\Return text))
      (with-output-to-string (out)
        (write-char #\" out)
        (loop for ch across text
              do (when (char= ch #\") (write-char #\" out)) (write-char ch out))
        (write-char #\" out))
      text))

(defun write-table (path table rows)
  "Write TABLE's header and ROWS to PATH, in file order. => PATH."
  (ensure-directories-exist path)
  (uiop:with-output-file (out path :if-exists :supersede :external-format :utf-8)
    (flet ((line (cells)
             (loop for cell across cells
                   for first = t then nil
                   do (unless first (write-char (table.separator table) out))
                      (write-string (quote-field cell (table.separator table)) out))
             (terpri out)))
      (line (table.columns table))
      (dolist (row rows) (line row))))
  path)
