;;;; text.lisp --- where a position is, in the units a server counts, and edits.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A server counts a line's columns in the encoding the initialize exchange
;;;; settled: UTF-16 code units unless it chose UTF-8 (we offer both). SBCL
;;;; counts characters, so every position crosses here: an emoji is one
;;;; character, two UTF-16 units and four UTF-8 bytes. Lines end at LF, a CR
;;;; before it belonging to the line ending.
;;;;
;;;; Pure functions over strings, but for APPLY-WORKSPACE-EDIT at the end,
;;;; which writes through the core's one atomic writer so the write is a
;;;; write like any other: atomic, and seen by the cell's own write hook.

(in-package #:nodecode-lsp)

;;; --- paths and URIs ------------------------------------------------------------

(defun native (path)
  "PATH as a native namestring."
  (if (pathnamep path) (uiop:native-namestring path) path))

(defun path-uri (path)
  "The file URI of the absolute PATH, every byte but the unreserved and / escaped."
  (with-output-to-string (out)
    (write-string "file://" out)
    (loop for byte across (sb-ext:string-to-octets (native path) :external-format :utf-8)
          for char = (code-char byte)
          do (if (and (< byte 128)
                      (or (alphanumericp char) (find char "-._~/")))
                 (write-char char out)
                 (format out "%~2,'0X" byte)))))

(defun uri-path (uri)
  "The native path a file URI names, or NIL for another scheme."
  (when (and (stringp uri) (> (length uri) 7) (string-equal "file://" uri :end2 7))
    (let ((slash (position #\/ uri :start 7)))
      (when slash
        (let ((bytes (make-array (length uri) :element-type '(unsigned-byte 8)
                                              :fill-pointer 0 :adjustable t)))
          (loop with i = slash
                while (< i (length uri))
                do (let ((char (char uri i)))
                     (if (and (char= char #\%) (< (+ i 2) (length uri))
                              (digit-char-p (char uri (+ i 1)) 16)
                              (digit-char-p (char uri (+ i 2)) 16))
                         (progn (vector-push-extend (parse-integer uri :start (1+ i) :end (+ i 3) :radix 16) bytes)
                                (incf i 3))
                         (progn (loop for byte across (sb-ext:string-to-octets (string char) :external-format :utf-8)
                                      do (vector-push-extend byte bytes))
                                (incf i)))))
          (sb-ext:octets-to-string (coerce bytes '(simple-array (unsigned-byte 8) (*)))
                                   :external-format (list :utf-8 :replacement +replacement+)))))))

(defun read-file (path)
  "The file at PATH as text, a byte that is not UTF-8 read as U+FFFD; NIL when
it cannot be read."
  (ignore-errors
   (uiop:read-file-string path :external-format (list :utf-8 :replacement +replacement+))))

;;; --- positions -----------------------------------------------------------------

(defun line-starts (text)
  "The index each of TEXT's lines starts at."
  (let ((starts (make-array 16 :adjustable t :fill-pointer 0)))
    (vector-push-extend 0 starts)
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (vector-push-extend (1+ i) starts))
    starts))

(defun line-end (text starts line)
  "Where LINE of TEXT ends, its line ending excluded."
  (let ((start (aref starts line))
        (end (if (< (1+ line) (length starts))
                 (1- (aref starts (1+ line)))
                 (length text))))
    (if (and (> end start) (char= (char text (1- end)) #\Return)) (1- end) end)))

(defun line-text (text starts line)
  "LINE of TEXT, without its line ending; empty past the last line."
  (if (< -1 line (length starts))
      (subseq text (aref starts line) (line-end text starts line))
      ""))

(defun char-units (char encoding)
  "How many units of ENCODING CHAR takes."
  (let ((code (char-code char)))
    (ecase encoding
      (:utf-16 (if (> code #xffff) 2 1))
      (:utf-8 (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4)))
      (:utf-32 1))))

(defun position-index (text starts line character encoding)
  "The index in TEXT of the position LINE, CHARACTER (ENCODING units), clamped
to the text and to the line's end."
  (cond ((minusp line) 0)
        ((>= line (length starts)) (length text))
        (t (let ((end (line-end text starts line)))
             (loop with units = 0
                   for i from (aref starts line) below end
                   when (>= units character) do (return i)
                     do (incf units (char-units (char text i) encoding))
                   finally (return end))))))

(defun index-line (starts index)
  "The line INDEX falls on."
  (let ((low 0) (high (1- (length starts))))
    (loop while (< low high)
          do (let ((mid (ceiling (+ low high) 2)))
               (if (<= (aref starts mid) index) (setf low mid) (setf high (1- mid)))))
    low))

(defun lsp-position (text starts index encoding)
  "INDEX in TEXT as an LSP position object in ENCODING units."
  (let ((line (index-line starts index)))
    (nlk:json-object "line" line
                     "character" (loop for i from (aref starts line) below index
                                       sum (char-units (char text i) encoding)))))

(defun position-column (text starts line character encoding)
  "The 1-based character column of the position LINE, CHARACTER."
  (if (< -1 line (length starts))
      (1+ (- (position-index text starts line character encoding) (aref starts line)))
      (1+ character)))

;;; --- a symbol, found by name ------------------------------------------------------

(defun identifier-p (symbol)
  "Whether SYMBOL is a bare identifier, which matches only on word boundaries."
  (ppcre:scan "^[$A-Za-z_][A-Za-z0-9_$]*$" symbol))

(defun identifier-char-p (char)
  (and (< (char-code char) 128)
       (or (alphanumericp char) (char= char #\_) (char= char #\$))))

(defun symbol-hits (line symbol &optional fold)
  "Where SYMBOL starts in the string LINE, left to right, case folded under FOLD."
  (let ((test (if fold #'char-equal #'char=))
        (bounded (identifier-p symbol))
        (hits '()))
    (when (plusp (length symbol))
      (loop with from = 0
            for at = (search symbol line :start2 from :test test)
            while at
            do (let ((after (+ at (length symbol))))
                 (if (and bounded
                          (or (and (plusp at) (identifier-char-p (char line (1- at))))
                              (and (< after (length line)) (identifier-char-p (char line after)))))
                     (setf from (1+ at))
                     (progn (push at hits) (setf from after))))))
    (nreverse hits)))

(defun parse-symbol-spec (spec)
  "SPEC as (values NAME OCCURRENCE): `parse#2' is the second `parse', a bare
name the first. A name that itself ends in #digits needs an occurrence after it."
  (let ((parsed (ppcre:register-groups-bind (name (#'parse-integer n)) ("^(.+)#(\\d+)$" spec)
                  (list name (max 1 n)))))
    (values-list (or parsed (list spec 1)))))

(defun locate-symbol (text spec &key line)
  "Where SPEC names a symbol in TEXT: (values LINE INDEX), LINE 0-based, INDEX
into TEXT. LINE, 1-based, looks on that line alone; else the whole file, in
order. An exact match wins over one that differs only in case."
  (multiple-value-bind (name occurrence) (parse-symbol-spec spec)
    (let* ((starts (line-starts text))
           (lines (if line
                      (progn (unless (and (integerp line) (<= 1 line (length starts)))
                               (fail ":line ~a is outside the file's ~d lines" line (length starts)))
                             (list (1- line)))
                      (loop for i below (length starts) collect i))))
      (flet ((hits (fold)
               (loop for i in lines
                     nconc (loop for at in (symbol-hits (line-text text starts i) name fold)
                                 collect (cons i (+ (aref starts i) at))))))
        (let ((hits (or (hits nil) (hits t))))
          (cond ((null hits)
                 (fail "~s is not in the file~@[ on line ~d~]" name line))
                ((> occurrence (length hits))
                 (fail "~s occurs ~d time~:p~@[ on line ~d~]; there is no #~d"
                       name (length hits) line occurrence))
                (t (let ((hit (nth (1- occurrence) hits)))
                     (values (car hit) (cdr hit))))))))))

;;; --- edits ------------------------------------------------------------------------

(defun range-span (text starts range encoding)
  "RANGE, an LSP range, as (values START END) indices into TEXT."
  (flet ((index (point)
           (position-index text starts
                           (or (nlk:json-value point :integer "line") 0)
                           (or (nlk:json-value point :integer "character") 0)
                           encoding)))
    (values (index (gethash "start" range)) (index (gethash "end" range)))))

(defun apply-text-edits (text edits encoding)
  "TEXT with the LSP text EDITS applied, as if all at once. Refuses edits that
overlap and snippet edits; inserts at one point keep the order they came in."
  (let* ((starts (line-starts text))
         (spans (loop for edit across edits
                      for i from 0
                      collect (let ((new (gethash "newText" edit)))
                                (unless (stringp new)
                                  (fail "the server sent a snippet edit, which this cell does not apply"))
                                (multiple-value-bind (start end)
                                    (range-span text starts (gethash "range" edit) encoding)
                                  (list start (max start end) new i))))))
    (setf spans (sort spans (lambda (a b) (or (< (first a) (first b))
                                              (and (= (first a) (first b)) (< (fourth a) (fourth b)))))))
    (loop for (a b) on spans
          when (and b (< (first b) (second a)))
            do (fail "the server sent edits that overlap (line ~d); nothing was changed"
                     (1+ (index-line starts (first b)))))
    (with-output-to-string (out)
      (let ((at 0))
        (loop for (start end new) in spans
              do (write-string text out :start at :end start)
                 (write-string new out)
                 (setf at end))
        (write-string text out :start at)))))

(defun workspace-edit-files (edit)
  "EDIT, an LSP WorkspaceEdit, as ((PATH . TEXT-EDITS)...) in the order the
files first appear. Refuses a resource operation: this cell only edits text."
  (let ((files '()))
    (flet ((add (uri edits)
             (let ((path (uri-path uri)))
               (unless path
                 (fail "the server's edit names ~a, which is not a file" uri))
               (let ((entry (assoc path files :test #'string=)))
                 (if entry
                     (setf (cdr entry) (concatenate 'vector (cdr entry) edits))
                     (push (cons path (coerce edits 'vector)) files))))))
      (let ((changes (nlk:json-value edit :object "changes")))
        (when changes
          (maphash (lambda (uri edits) (add uri (or (nlk:json-value edits :array) #()))) changes)))
      (loop for change across (nlk:json-array edit "documentChanges")
            do (let ((kind (nlk:json-value change :string "kind")))
                 (if kind
                     (fail "the server's edit would also ~a ~a; this cell edits text only, ~
                            so nothing was changed"
                           kind (or (nlk:json-value change :string "uri")
                                    (nlk:json-value change :string "oldUri") "a file"))
                     (add (nlk:json-value change :string "textDocument" "uri")
                          (nlk:json-array change "edits"))))))
    (nreverse files)))

(defun apply-workspace-edit (edit encoding)
  "Apply the WorkspaceEdit EDIT, its positions in ENCODING units: every file's
new text made first, then each written. => ((PATH . EDIT-COUNT)...)."
  ;; A file that cannot be read as UTF-8 refuses the whole edit before
  ;; anything is written: written back, its undecodable bytes would be lost.
  (let ((planned (loop for (path . edits) in (workspace-edit-files edit)
                       collect (let ((text (nlk:read-text path)))
                                 (unless text
                                   (fail "cannot read ~a as UTF-8 text; nothing was changed" path))
                                 (list path (apply-text-edits text edits encoding) (length edits))))))
    (loop for (path text count) in planned
          do (nle::write-file-text (uiop:parse-native-namestring path) text)
          collect (cons path count))))
