;;;; ledger.lisp --- the ledger file, and the reports /edit-log prints.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A row is one turn that changed at least one file: when it was, which
;;;; model ran it, how many files it changed and of which types (extensions).
;;;; Never a path and never a word the agent wrote, so the file is safe to
;;;; read and small to keep. Everything here is pure but the two file verbs,
;;;; which take the path they work on, so the tests drive them directly.

(in-package #:nodecode-edit-ledger)

(defconstant +day-seconds+ 86400.0)
(defconstant +max-types+ 20)
(defconstant +recent-rows+ 10)
(defconstant +max-recent-rows+ 100)

(defparameter +no-type+ "(none)"
  "A file with no extension: Makefile, .env.")

(defparameter +other-type+ "(other)"
  "An extension that is not a short plain one.")

;;; --- file types -----------------------------------------------------------------

(defun last-component (path)
  "PATH's last component, split on either separator so a path from any
system reads the same here."
  (let ((text (if (stringp path) path (princ-to-string (or path ""))))
        (cut nil))
    (loop for i from 0 below (length text)
          when (member (char text i) '(#\/ #\\) :test #'char=) do (setf cut i))
    (if cut (subseq text (1+ cut)) text)))

(defun plain-extension-p (extension)
  "Whether EXTENSION — a dot and its letters, lower case — is a short plain
one: .py, .tsx, .md."
  (and (plusp (length extension))
       (<= 2 (length extension) 11)
       (loop for i from 1 below (length extension)
             always (let ((c (char extension i)))
                      (or (char<= #\a c #\z) (char<= #\0 c #\9)
                          (member c '(#\_ #\+ #\-) :test #'char=))))))

(defun file-type (path)
  "The extension of PATH's last component, lower case (.py), (none) when it
has none, (other) when it is not a short plain one."
  (let* ((name (last-component path))
         (dot (position #\. name :from-end t)))
    (cond ((null dot) +no-type+)
          ((zerop dot) +no-type+)
          (t (let ((extension (string-downcase (subseq name dot))))
               (if (plain-extension-p extension) extension +other-type+))))))

(defun count-types (paths)
  "Files per type, as an alist, the +MAX-TYPES+ most common kept and the rest
folded into (other). Ties read alphabetically, so a report is stable."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (path paths) (incf (gethash (file-type path) counts 0)))
    (let* ((ranked (sort (loop for kind being the hash-keys of counts using (hash-value n)
                                collect (cons kind n))
                          (lambda (a b)
                            (or (> (cdr a) (cdr b))
                                (and (= (cdr a) (cdr b)) (string< (car a) (car b)))))))
           (kept (subseq ranked 0 (min +max-types+ (length ranked))))
           (rest (loop for (kind . n) in (nthcdr +max-types+ ranked) sum n)))
      (if (plusp rest)
          (let ((other (assoc +other-type+ kept :test #'string=)))
            (if other
                (progn (incf (cdr other) rest) kept)
                (append kept (list (cons +other-type+ rest)))))
          kept))))

(defun types-object (alist)
  "ALIST of (TYPE . COUNT) as the JSON object a row carries."
  (apply #'nlk:make-json-object
         (loop for (kind . n) in alist append (list kind n))))

(defun top-type (types)
  "The type changed most, alphabetically first on a tie; \"-\" when none."
  (let ((best nil))
    (loop for (kind . n) in types
          when (or (null best) (> n (cdr best))
                   (and (= n (cdr best)) (string< kind (car best))))
            do (setf best (cons kind n)))
    (if best (car best) "-")))

;;; --- rows -----------------------------------------------------------------------

(defun session-tag (session)
  "A short, stable stand-in for a session id, \"\" when there is none: the
row names the session without keeping it."
  (let ((text (and session (princ-to-string session))))
    (if (and text (plusp (length text))) (nlk:short-digest text 12) "")))

(defun trimmed-model (model)
  (let ((text (and model (princ-to-string model))))
    (if (and text (plusp (length text))) text "unknown")))

(defun make-row (&key at session model paths)
  "One ledger row from a finished turn's AT, SESSION, MODEL and changed PATHS.
Only how many files changed and their types are kept."
  (nlk:json-object "at" (round at)
                    "session" (session-tag session)
                    "model" (trimmed-model model)
                    "files" (length paths)
                    "types" (types-object (count-types paths))))

(defun row-at (row)
  (let ((at (nlk:json-value row :number "at")))
    (if (numberp at) at 0.0)))

(defun row-files (row)
  (let ((n (nlk:json-value row :integer "files")))
    (if (and n (plusp n)) n 0)))

(defun row-model (row) (or (nlk:json-value row :string "model") "unknown"))
(defun row-session (row) (or (nlk:json-value row :string "session") ""))

(defun row-types (row)
  "ROW's types as an alist of (TYPE . COUNT), the count as a number."
  (let ((types (nlk:json-value row :object "types"))
        (out '()))
    (when types
      (maphash (lambda (kind n)
                 (let ((count (if (numberp n) n 0)))
                   (when (plusp count) (push (cons kind count) out))))
               types))
    (sort out (lambda (a b)
                (or (> (cdr a) (cdr b))
                    (and (= (cdr a) (cdr b)) (string< (car a) (car b))))))))

(defun read-ledger (path)
  "Every parseable row, oldest first; a torn or foreign line is skipped."
  (remove-if-not #'hash-table-p (nlk:read-json-lines path)))

(defun ledger-bytes (path)
  (let ((probe (probe-file path)))
    (if probe
        (with-open-file (in probe :element-type '(unsigned-byte 8) :if-does-not-exist nil)
          (if in (file-length in) 0))
        0)))

(defun trim-ledger (path keep)
  "Leave PATH its newest KEEP rows, written whole and in place of it."
  (let* ((rows (read-ledger path))
         (kept (if (> (length rows) keep) (nthcdr (- (length rows) keep) rows) rows)))
    (nlk:write-file-atomically
     path (format nil "~{~a~%~}" (mapcar #'nlk:encode-json-object kept))
     :parents t)
    (length rows)))

(defun append-row (path row &key (keep 5000) (max-bytes 2000000))
  "Append ROW to the ledger at PATH; once the file passes MAX-BYTES its
newest KEEP rows are written back in its place."
  (nlk:append-json-line path row)
  (when (> (ledger-bytes path) max-bytes)
    (trim-ledger path keep))
  row)

(defun clear-ledger (path)
  "Delete the ledger at PATH and answer how many rows it held."
  (let ((count (length (read-ledger path))))
    (when (probe-file path) (ignore-errors (delete-file path)))
    count))

;;; --- windows --------------------------------------------------------------------

(defun parse-span (word)
  "Seconds for a window word such as 12h or 7d; NIL when WORD is not one."
  (let* ((text (string-downcase (or word "")))
         (end (length text)))
    (when (>= end 2)
      (let ((unit (char text (1- end)))
            (digits (subseq text 0 (1- end))))
        (when (and (or (char= unit #\h) (char= unit #\d))
                   (<= (length digits) 4)
                   (every #'digit-char-p digits))
          (let ((seconds (* (parse-integer digits)
                            (if (char= unit #\h) 3600.0 +day-seconds+))))
            (and (plusp seconds) seconds)))))))

(defun span-title (word)
  (let* ((text (string-downcase word))
         (count (parse-integer text :end (1- (length text)))))
    (format nil "last ~d ~a~:[s~;~]" count
            (if (char= (char text (1- (length text))) #\h) "hour" "day")
            (= count 1))))

(defun parse-count (word)
  "A row count for recent, 1 to +MAX-RECENT-ROWS+; NIL when WORD is not one."
  (let ((text (or word "")))
    (when (and (plusp (length text)) (<= (length text) 3) (every #'digit-char-p text))
      (let ((n (parse-integer text)))
        (and (<= 1 n +max-recent-rows+) n)))))

(defun last-session (rows)
  "The session of the newest row that carries one, \"\" when none does."
  (let ((found ""))
    (dolist (row rows found)
      (when (plusp (length (row-session row))) (setf found (row-session row))))))

(defun select-rows (rows window now &optional (span +day-seconds+))
  "ROWS in WINDOW — all, session or the SPAN seconds ending NOW."
  (cond ((string= window "all") rows)
        ((string= window "session")
         (let ((session (last-session rows)))
           (if (plusp (length session))
               (remove-if-not (lambda (row) (string= session (row-session row))) rows)
               '())))
        (t (remove-if (lambda (row) (< (row-at row) (- now span))) rows))))

;;; --- reading time ----------------------------------------------------------------

(defun age-text (seconds)
  "SECONDS as the report says it: 3s ago, 12m ago, 5h ago, 2d ago."
  (let ((seconds (max 0.0 (float seconds 1.0))))
    (cond ((>= seconds +day-seconds+) (format nil "~dd ago" (floor seconds +day-seconds+)))
          ((>= seconds 3600.0) (format nil "~dh ago" (floor seconds 3600.0)))
          ((>= seconds 60.0) (format nil "~dm ago" (floor seconds 60.0)))
          (t (format nil "~ds ago" (floor seconds))))))

(defun local-day (at)
  "AT — seconds since the epoch — as this machine's calendar day, or
\"unknown\" when it carries no usable time."
  (if (and (numberp at) (plusp at) (< at 100000000000))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (round at))
        (declare (ignore second minute hour))
        (format nil "~4,'0d-~2,'0d-~2,'0d" year month day))
      "unknown"))

(defun median (numbers)
  "The middle value, averaged over the two middle ones when the count is
even."
  (let* ((sorted (sort (copy-list numbers) #'<))
         (n (length sorted)))
    (cond ((zerop n) 0.0)
          ((oddp n) (float (nth (floor n 2) sorted) 1.0))
          (t (/ (+ (nth (1- (floor n 2)) sorted) (nth (floor n 2) sorted)) 2.0)))))

(defun number-text (value)
  "VALUE as the report says it: 2 for 2.0, 2.5 for a median that falls
between two files."
  (if (and (numberp value) (= value (fround value)))
      (format nil "~d" (round value))
      (let ((text (format nil "~,1f" (float value 1.0))))
        (string-right-trim "." (string-right-trim "0" text)))))

;;; --- the tables -----------------------------------------------------------------

(defun table-lines (header body)
  "HEADER and BODY as columns: the first left, the rest right."
  (let* ((rows (cons header body))
         (widths (loop for i from 0 below (length header)
                        collect (loop for row in rows maximize (length (nth i row))))))
    (loop for row in rows
          collect (string-right-trim
                   " "
                   (format nil "~{~a~^  ~}"
                           (loop for cell in row
                                 for i from 0
                                 for width in widths
                                 collect (if (zerop i)
                                             (format nil "~va" width cell)
                                             (format nil "~v@a" width cell))))))))

(defun turns-heading (title &optional (kind ""))
  (format nil "Turns that changed files, ~a~@[ by ~a~]:" title kind))

(defparameter +caveat+
  "Counts files changed by the agent's write-file and edit verbs, not by shell commands.")

(defun group-rows (rows by)
  "ROWS grouped by BY (:model or :day), as an alist (KEY . ROWS) in report
order."
  (let ((groups (make-hash-table :test #'equal))
        (order '()))
    (dolist (row rows)
      (let ((key (if (eq by :day) (local-day (row-at row)) (row-model row))))
        (unless (gethash key groups) (push key order))
        (push row (gethash key groups))))
    (let ((alist (loop for key in (nreverse order) collect (cons key (nreverse (gethash key groups))))))
      (if (eq by :day)
          (stable-sort alist
                       (lambda (a b)
                         (cond ((string= (car a) "unknown") nil)
                               ((string= (car b) "unknown") t)
                               (t (string> (car a) (car b))))))
          (sort alist (lambda (a b)
                        (or (> (length (cdr a)) (length (cdr b)))
                            (and (= (length (cdr a)) (length (cdr b)))
                                 (string< (car a) (car b))))))))))

(defun group-cells (name rows now)
  "One report row for ROWS: NAME, turns, files, median, top type, last."
  (let ((files (mapcar #'row-files rows)))
    (list name
          (format nil "~d" (length rows))
          (format nil "~d" (reduce #'+ files :initial-value 0))
          (number-text (median files))
          (top-type (count-types-merged rows))
          (age-text (- now (reduce #'max (mapcar #'row-at rows) :initial-value 0))))))

(defun count-types-merged (rows)
  "Every row's types added up."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (row rows)
      (dolist (pair (row-types row))
        (incf (gethash (car pair) counts 0) (cdr pair))))
    (sort (loop for kind being the hash-keys of counts using (hash-value n)
                collect (cons kind n))
          (lambda (a b)
            (or (> (cdr a) (cdr b))
                (and (= (cdr a) (cdr b)) (string< (car a) (car b))))))))

(defun summarize (rows now &key title (by :model))
  "Turns that changed files in ROWS, grouped by BY (:model, :day or :type)."
  (cond
    ((null rows) (format nil "Turns that changed files, ~a: none recorded." title))
    ((eq by :type) (summarize-types rows now title))
    (t
     (let* ((groups (group-rows rows by))
            (body (loop for (name . items) in groups
                        collect (group-cells name items now))))
       (when (> (length groups) 1)
         (push (group-cells "total" rows now) body))
       (format nil "~a~%~%~{~a~%~}~%~a"
               (turns-heading title (if (eq by :day) "day" "model"))
               (table-lines (list (if (eq by :day) "day" "model")
                                  "turns" "files" "median" "top type" "last")
                            body)
               +caveat+)))))

(defun summarize-types (rows now title)
  "File types instead of groups: files of that type and the turns that
changed one."
  (let ((stats (make-hash-table :test #'equal)))
    (dolist (row rows)
      (dolist (pair (row-types row))
        (let ((entry (gethash (car pair) stats)))
          (unless entry (setf entry (list 0 0 0.0) (gethash (car pair) stats) entry))
          (incf (first entry) (cdr pair))
          (incf (second entry) 1)
          (setf (third entry) (max (third entry) (row-at row))))))
    (let* ((ranked (sort (loop for kind being the hash-keys of stats using (hash-value entry)
                               collect (cons kind entry))
                         (lambda (a b)
                           (or (> (first (cdr a)) (first (cdr b)))
                               (and (= (first (cdr a)) (first (cdr b)))
                                    (string< (car a) (car b)))))))
           (body (loop for (kind . entry) in ranked
                       collect (list kind
                                     (format nil "~d" (first entry))
                                     (format nil "~d" (second entry))
                                     (age-text (- now (third entry)))))))
      (format nil "~a~%~%~{~a~%~}~%~a"
              (format nil "Turns that changed files, ~a, by file type:" title)
              (table-lines (list "type" "files" "turns" "last") body)
              +caveat+))))

(defun recent-text (rows now &key title (limit +recent-rows+))
  "The newest LIMIT turns, newest first, one line each."
  (if (null rows)
      (format nil "Turns that changed files, ~a: none recorded." title)
      (let* ((newest (reverse (last rows (min limit (length rows)))))
             (body (loop for row in newest
                         collect (list (age-text (- now (row-at row)))
                                       (row-model row)
                                       (format nil "~d" (row-files row))
                                       (let ((types (row-types row)))
                                         (if types
                                             (format nil "~{~a~^, ~}~a"
                                                     (loop for (kind . n) in (subseq types 0 (min 3 (length types)))
                                                           collect (format nil "~a ~d" kind n))
                                                     (if (> (length types) 3) " ..." ""))
                                             "-"))))))
        (format nil "~a~%~%~{~a~%~}"
                (format nil "Turns that changed files, ~a, newest ~d of ~d:"
                        title (length newest) (length rows))
                (table-lines (list "when" "model" "files" "types") body)))))
