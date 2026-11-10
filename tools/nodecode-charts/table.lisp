;;;; table.lisp --- the table a chart is drawn from: rows, columns, labels.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's data_io.py. The same five ways of handing a
;;;; table over are read: row objects, columns plus rows, pasted CSV/TSV or a
;;;; Markdown table, JSON text, and a local .csv/.tsv/.txt/.json by absolute
;;;; path. The path reader keeps the original's own fence: only those four
;;;; extensions, no hidden file, nothing inside the agent's own home folder,
;;;; 10 MB at most - a chart tool has no business reading a key store.

(in-package #:nodecode-charts)

(defparameter *file-extensions* '(".csv" ".tsv" ".txt" ".json")
  "The only files a path may name.")

(defparameter *max-file-bytes* 10000000
  "The original's own ceiling on a file it reads: 10 MB.")

(defstruct table
  (columns #() :type vector)
  (rows #() :type vector)
  (labels #() :type vector)
  (hints nil))

(defun cell-text (value)
  "VALUE as the text a table cell holds; NIL and JSON null as the empty string."
  (cond ((null value) "")
        ((stringp value) value)
        ((eq value t) "true")
        ((integerp value) (format nil "~d" value))
        ((realp value) (let ((*read-default-float-format* 'double-float))
                         (format nil "~f" (coerce value 'double-float))))
        (t (princ-to-string value))))

(defun spaces-to-dashes (key)
  (substitute #\- #\_ (string-downcase (string key))))

(defun spec-value (object key &optional default)
  "KEY's value in OBJECT, spelling top_n and top-n alike."
  (if (hash-table-p object)
      (or (gethash key object)
          (gethash (substitute #\_ #\- key) object)
          default)
      default))

(defun table-from-cells (columns rows &optional hints)
  "A TABLE from COLUMNS and ROWS, with the duplicate and empty checks the
original reports rather than drawing a chart from a broken table."
  (let ((columns (coerce (map 'list (lambda (c) (string-trim " " (cell-text c))) columns) 'vector)))
    (when (zerop (length columns))
      (refuse-chart "empty_data" "The table has no columns." :where "data"
            :fix "Give columns, or paste the table as data.text."))
    (let ((seen nil))
      (loop for column across columns
            do (when (member column seen :test #'string=)
                 (refuse-chart "duplicate_columns" (format nil "The column ~s appears more than once." column)
                       :where "data" :fix "Give every column a distinct name."))
               (push column seen)))
    (make-table :columns columns :rows rows :hints hints
                :labels (coerce (loop for i from 1 to (length rows) collect (format nil "row ~d" (1+ i)))
                                'vector))))

(defun table-from-values (values &optional hints)
  "An array of row objects as a TABLE: the columns are the keys, in the order
they first appear."
  (let ((columns nil) (rows nil))
    (loop for object across values
          do (unless (hash-table-p object)
               (refuse-chart "bad_data" "data.values must be an array of row objects."
                     :where "data.values" :fix "Send rows like [{\"month\": \"2026-01\", \"sales\": 1200}]."))
             (let ((row nil))
               (maphash (lambda (key value)
                          (unless (member key columns :test #'string=) (setf columns (append columns (list key))))
                          (push (cons key (cell-text value)) row))
                        object)
               (push row rows)))
    (setf columns (or columns nil)
          rows (nreverse rows))
    (when (null columns)
      (refuse-chart "empty_data" "data.values holds no rows." :where "data.values"
            :fix "Send at least one row object."))
    (table-from-cells columns
                       (coerce (loop for row in rows
                                     collect (coerce (loop for column in columns
                                                           collect (cdr (assoc column row :test #'string=)))
                                                     'vector))
                               'vector)
                       hints)))

(defun split-on (text delimiter)
  "TEXT split on DELIMITER, quotes honoured for a comma-separated line."
  (let ((cells nil) (out (make-string-output-stream)) (quoted nil) (index 0))
    (loop while (< index (length text))
          for character = (char text index)
          do (cond ((and (char= character #\") (char= delimiter #\,))
                    (if (and quoted (< (1+ index) (length text)) (char= (char text (1+ index)) #\"))
                        (progn (write-char #\" out) (incf index))
                        (setf quoted (not quoted))))
                   ((and (char= character delimiter) (not quoted))
                    (push (string-trim " " (get-output-stream-string out)) cells))
                   (t (write-char character out)))
             (incf index))
    (push (string-trim " " (get-output-stream-string out)) cells)
    (nreverse cells)))

(defun delimiter-of (line)
  (cond ((find #\Tab line) #\Tab)
        ((find #\, line) #\,)
        ((find #\| line) #\|)
        (t nil)))

(defun markdown-separator-p (line)
  "A Markdown table's |---|:---| line, which is not a row."
  (let ((cells (split-on (string-trim " |" line) #\|)))
    (and (plusp (length cells))
         (every (lambda (cell)
                  (let ((cell (string-trim " " cell)))
                    (and (plusp (length cell))
                         (every (lambda (c) (find c "-: " :test #'char=)) cell))))
                cells))))

(defun table-from-text (text)
  "CSV, TSV copied from Excel, or a Markdown table as a TABLE: the longest run
of delimited lines is the table, so a title or a note around it is skipped."
  (let ((lines (loop for line in (uiop:split-string (subst #\Newline #\Return text)
                                                     :separator '(#\Newline))
                     for trimmed = (string-trim " " line)
                     when (plusp (length trimmed))
                       collect trimmed)))
    (let* ((delimiter (find-if #'delimiter-of lines))
           (run nil) (best nil))
      (when lines
        (let ((delimiter (or (and delimiter (delimiter-of (find-if (lambda (line) (delimiter-of line)) lines)))
                             #\Null)))
          (if (char= delimiter #\Null)
              (setf best (reverse lines))
              (progn
                (dolist (line lines)
                  (if (delimiter-of line)
                      (push line run)
                      (progn (when (> (length run) (length best)) (setf best run)) (setf run nil))))
                (when (> (length run) (length best)) (setf best run))))
          (setf best (reverse best))
          (let ((rows (loop for line in best
                            for cells = (cond ((char= delimiter #\|) (split-on (string-trim " |" line) #\|))
                                              ((char= delimiter #\Null) (list line))
                                              (t (split-on line delimiter)))
                            unless (and (char= delimiter #\|) (markdown-separator-p line))
                              collect cells)))
            (when (< (length rows) 2)
              (refuse-chart "bad_text" "The pasted text needs a header row and at least one row of values."
                    :where "data.text"
                    :fix "Paste the table with its header row first, or send data.values instead."))
            (let* ((width (loop for row in rows maximize (length row)))
                   (header (first rows)))
              (table-from-cells
               (coerce (loop for i below width
                             collect (let ((cell (nth i header)))
                                       (if (and cell (plusp (length cell))) cell (format nil "col ~d" (1+ i)))))
                       'vector)
               (coerce (loop for row in (rest rows)
                             collect (coerce (loop for i below width
                                                   collect (or (nth i row) ""))
                                             'vector))
                       'vector)))))))))

(defun nodecode-home ()
  "Where the agent keeps its own files - a chart's path reader refuses inside it."
  (let ((home (uiop:getenv "NODECODE_HOME")))
    (if (and home (plusp (length home)))
        (uiop:ensure-directory-pathname home)
        (merge-pathnames (make-pathname :directory '(:relative ".nodecode"))
                         (user-homedir-pathname)))))

(defun table-from-path (path)
  "The table a local .csv/.tsv/.txt/.json file holds, under the original's fence."
  (let* ((named (pathname (string path))))
    (unless (uiop:absolute-pathname-p named)
      (refuse-chart "bad_path" (format nil "~s is not an absolute path." path) :where "data.path"
            :fix "Pass the whole path, not a name relative to the folder you are in."))
    (let* ((real (or (ignore-errors (truename named)) named))
           (name (string-downcase (file-namestring real))))
      (unless (some (lambda (extension) (string-suffix-p extension name)) *file-extensions*)
        (refuse-chart "bad_path" (format nil "Only ~{~a~^, ~} files can be read." *file-extensions*)
              :where "data.path" :fix "Export the sheet as CSV first, or paste it as data.text."))
      (unless (probe-file real)
        (refuse-chart "file_not_found" (format nil "No file at ~a." path) :where "data.path"
              :fix "Check the path; list the folder first if unsure."))
      (let* ((home (nodecode-home))
             (hidden (some (lambda (component) (and (plusp (length component)) (char= (char component 0) #\.)))
                           (rest (pathname-directory real))))
             (inside (and home (search (namestring home) (namestring real) :test #'char-equal))))
        (when (or hidden (and inside (zerop inside)))
          (refuse-chart "path_not_allowed" "Hidden files and files inside the agent's own home folder are not read."
                :where "data.path"
                :fix "Copy the table to a normal folder, or paste it as data.text.")))
      (let ((size (with-open-file (in real :element-type '(unsigned-byte 8) :if-does-not-exist nil)
                    (and in (file-length in)))))
        (unless size
          (refuse-chart "file_not_found" (format nil "No file at ~a." path) :where "data.path"))
        (when (> size *max-file-bytes*)
          (refuse-chart "file_too_large" (format nil "The file is ~,1f MB (limit 10 MB)." (/ size 1d6))
                :where "data.path" :fix "Aggregate the data first and chart the summary.")))
      (let ((text (or (ignore-errors (uiop:read-file-string real :external-format :utf-8))
                      (ignore-errors (uiop:read-file-string real :external-format :cp932))
                      (refuse-chart "bad_encoding" "The file is neither UTF-8 nor Shift_JIS text."
                            :where "data.path" :fix "Save it as UTF-8 CSV."))))
        (if (string-suffix-p ".json" name)
            (table-from-json text)
            (table-from-text text))))))

(defun table-from-json (text)
  "JSON text as a TABLE: an object with values or columns and rows, or a bare
array of row objects."
  (let ((decoded (handler-case (nlk:decode-json text)
                   (error (e) (declare (ignore e))
                          (refuse-chart "bad_data" "data.json is not valid JSON." :where "data.json"
                                :fix "Send a JSON object with values, or columns and rows.")))))
    (cond ((hash-table-p decoded) (load-table decoded))
          ((and (vectorp decoded) (not (stringp decoded))) (table-from-values decoded))
          (t (refuse-chart "bad_data" "data.json holds neither a table object nor an array of rows."
                   :where "data.json" :fix "Send {\"columns\": [...], \"rows\": [[...]]} or an array of objects.")))))

(defun load-table (spec)
  "The table SPEC names: values, columns and rows, text, json, or a path."
  (unless (hash-table-p spec)
    (refuse-chart "bad_data" "data must be an object." :where "data"
          :fix "Send {\"values\": [...]}, {\"text\": \"...\"} or {\"path\": \"/abs/file.csv\"}."))
  (let ((hints (list :unit (spec-value spec "unit") :source (spec-value spec "source")))
        (values-array (nlk:json-value spec :array "values"))
        (columns (nlk:json-value spec :array "columns"))
        (rows (nlk:json-value spec :array "rows"))
        (text (nlk:json-value spec :string "text"))
        (json (nlk:json-value spec :string "json"))
        (path (nlk:json-value spec :string "path")))
    (let ((given (count-if #'identity (list values-array (and columns rows) text json path))))
      (cond ((zerop given)
             (refuse-chart "empty_data" "data holds nothing to draw." :where "data"
                   :fix "Give exactly one of values, columns + rows, text, json or path."))
            ((> given 1)
             (refuse-chart "bad_data" (format nil "data gives ~d of values, columns+rows, text, json and path; exactly one is drawn." given)
                   :where "data" :fix "Keep one and remove the rest."))))
    (let ((hints (append (let ((unit (getf hints :unit)))
                            (and unit (plusp (length (princ-to-string unit))) (list :unit unit)))
                          (let ((source (getf hints :source)))
                            (and source (plusp (length (princ-to-string source))) (list :source source))))))
      (cond (values-array (table-from-values values-array hints))
            ((and columns rows)
             (table-from-cells columns
                               (coerce (loop for row across rows
                                             collect (if (vectorp row) row (vector (cell-text row))))
                                       'vector)
                               hints))
            (text (let ((table (table-from-text text))) (setf (table-hints table) hints) table))
            (json (let ((table (table-from-json json))) (setf (table-hints table) hints) table))
            (path (let ((table (table-from-path path))) (setf (table-hints table) hints) table))
            (t (refuse-chart "empty_data" "data holds nothing to draw." :where "data"))))))
