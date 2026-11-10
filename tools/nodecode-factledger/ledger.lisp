;;;; ledger.lisp --- the ledger: one TSV record a fact, a closed row kept.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's store, whole. A TSV file whose first line names the ten
;;;; columns, one record a fact; a new value closes the row it replaces
;;;; (valid_to and superseded_by) instead of deleting it, so for every
;;;; (scope, subject, predicate) exactly one row holds today and the rest are
;;;; its history. The file stays human-readable, diffable and greppable: the
;;;; original's reader takes a quoted field (a double quote quoting, "" its
;;;; escape) and its writer writes one only when the field needs it.
;;;;
;;;; A key is normalized on the way in and on the way out (trimmed,
;;;; lowercased, runs of whitespace collapsed); a value, a source and a
;;;; subject keep the bytes they were written with.

(in-package #:nodecode-factledger)

;;; --- the shape of a row ---------------------------------------------------------

(defparameter +fields+
  '("id" "scope" "subject" "predicate" "value"
    "valid_from" "valid_to" "superseded_by" "source" "confidence")
  "The ledger's columns, in the order the file's header writes them.")

(defparameter +confidences+ '("stated" "derived" "assumed")
  "How firm a value is: the original's three, firmest first.")

(defparameter +confidence-rank+ '(("stated" . 0) ("derived" . 1) ("assumed" . 2))
  "The order a compiled view puts them in, firmest first.")

(defun row-field (row name)
  "ROW's value under NAME, the empty string when it has none."
  (or (cdr (assoc name row :test #'string=)) ""))

(defun (setf row-field) (value row name)
  (let ((cell (assoc name row :test #'string=)))
    (unless cell
      (error "no column named ~s in the ledger's row" name))
    (setf (cdr cell) value)))

(defun make-row (&key (id "") (scope "") (subject "") (predicate "") (value "")
                     (valid-from "") (valid-to "") (superseded-by "")
                     (source "") (confidence "stated"))
  "One ledger row, its columns in the file's own order."
  (list (cons "id" id) (cons "scope" scope) (cons "subject" subject)
        (cons "predicate" predicate) (cons "value" value)
        (cons "valid_from" valid-from) (cons "valid_to" valid-to)
        (cons "superseded_by" superseded-by) (cons "source" source)
        (cons "confidence" confidence)))

(defun norm (text)
  "TEXT as a ledger key: trimmed, lowercased, every run of whitespace one space."
  (if (null text)
      ""
      (string-downcase (collapse-whitespace (princ-to-string text)))))

(defun collapse-whitespace (text)
  "TEXT with every run of whitespace read as one space."
  (with-output-to-string (out)
    (loop with space = nil
          for character across text
          do (if (member character '(#\Space #\Tab #\Newline #\Return #\Page))
                 (setf space t)
                 (progn (when space (write-char #\Space out))
                        (setf space nil)
                        (write-char character out))))))

(defun key-of (row)
  "ROW's identity: its scope, subject and predicate, each normalized."
  (list (norm (row-field row "scope"))
        (norm (row-field row "subject"))
        (norm (row-field row "predicate"))))

(defun row-sort-key (row)
  "ROW as one string that orders by scope, subject and predicate."
  (format nil "~a~c~a~c~a"
          (row-field row "scope") #\Null (row-field row "subject") #\Null
          (row-field row "predicate")))

(defun row-string< (a b)
  (string< (row-sort-key a) (row-sort-key b)))

(defun today (&optional (universal (get-universal-time)))
  "UNIVERSAL as the ledger's date, YYYY-MM-DD."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun iso-date (text)
  "TEXT as (VALUES YEAR MONTH DAY) when it is YYYY-MM-DD, else NIL."
  (when (and (stringp text) (= 10 (length text))
             (char= (char text 4) #\-) (char= (char text 7) #\-))
    (let ((year (ignore-errors (parse-integer text :start 0 :end 4)))
          (month (ignore-errors (parse-integer text :start 5 :end 7)))
          (day (ignore-errors (parse-integer text :start 8 :end 10))))
      (when (and year month day (<= 1 month 12) (<= 1 day 31))
        (values year month day)))))

(defun day-number (text)
  "TEXT's day, as a universal time at midnight, or NIL when it is no ISO date."
  (multiple-value-bind (year month day) (iso-date text)
    (and year (ignore-errors (encode-universal-time 0 0 0 day month year)))))

(defun days-between (from to)
  "How many whole days FROM is before TO, or NIL when either is no ISO date."
  (let ((start (day-number from)) (end (day-number to)))
    (and start end (truncate (- end start) 86400))))

(defun covers-p (row reference)
  "Whether ROW holds on the ISO date REFERENCE: it began by then and has not closed."
  (let ((from (row-field row "valid_from"))
        (to (row-field row "valid_to")))
    (and (or (zerop (length from)) (not (string< reference from)))
         (not (and (plusp (length to)) (not (string< reference to)))))))

;;; --- the file ------------------------------------------------------------------

(defun expanded (path)
  "PATH with a leading ~ read as the home."
  (if (and (plusp (length path)) (char= (char path 0) #\~))
      (namestring (merge-pathnames (subseq path (if (and (> (length path) 1)
                                                         (char= (char path 1) #\/))
                                                    2 1))
                                    (user-homedir-pathname)))
      path))

(defun ledger-file ()
  "The ledger this cell writes: the section's path, or the one under the home."
  (let ((path (setting :path)))
    (if (and path (plusp (length (string-trim '(#\Space #\Tab) path))))
        (expanded path)
        (namestring (nlk:home "factledger/facts.tsv")))))

(defun aliases-file ()
  "The file mapping other predicate spellings onto one, or the section's NIL."
  (let ((path (setting :aliases)))
    (and path (plusp (length (string-trim '(#\Space #\Tab) path))) (expanded path))))

(defstruct (ledger (:constructor make-ledger (path)))
  "PATH's rows, read whole, and the predicate aliases they are read with."
  path rows aliases)

(defun parse-tsv (text)
  "TEXT as records of fields: tab-delimited, a double quote quoting a field
and \"\" its escape, a newline ending a record even inside a quote."
  (let ((records '()) (record '()) (field (make-string-output-stream))
        (quoted nil) (pending nil) (index 0) (end (length text)))
    (labels ((end-field () (push (get-output-stream-string field) record) (setf pending t))
             (end-record ()
               (end-field)
               (push (nreverse record) records)
               (setf record '() pending nil)))
      (loop while (< index end)
            for character = (char text index)
            do (cond (quoted
                      (cond ((char= character #\")
                             (if (and (< (1+ index) end) (char= (char text (1+ index)) #\"))
                                 (progn (write-char #\" field) (incf index) (setf pending t))
                                 (setf quoted nil)))
                            (t (write-char character field) (setf pending t))))
                     ((char= character #\") (setf quoted t pending t))
                     ((char= character #\Tab) (end-field))
                     ((char= character #\Newline) (end-record))
                     ((char= character #\Return))
                     (t (write-char character field) (setf pending t)))
               (incf index))
      (when (or pending quoted record) (end-record)))
    (nreverse records)))

(defun field-text (value)
  "VALUE as one TSV field: quoted when it carries a tab, a quote or a newline."
  (let ((text (or value "")))
    (if (some (lambda (character) (member character '(#\Tab #\" #\Newline #\Return)))
              text)
        (with-output-to-string (out)
          (write-char #\" out)
          (loop for character across text
                do (when (char= character #\") (write-char #\" out))
                   (write-char character out))
          (write-char #\" out))
        text)))

(defun tsv-line (values)
  "VALUES as one TSV line, a tab between them and none after the last."
  (with-output-to-string (out)
    (loop for value in values
          for first = t then nil
          do (unless first (write-char #\Tab out))
             (write-string (field-text value) out))))

(defun render-rows (rows)
  "ROWS as the file's whole text: the header, then one line a row."
  (with-output-to-string (out)
    (write-line (tsv-line +fields+) out)
    (dolist (row rows)
      (write-line (tsv-line (mapcar (lambda (name) (row-field row name)) +fields+)) out))))

(defun load-rows (path)
  "PATH's rows: the header checked, an empty file as no rows at all."
  (if (not (probe-file path))
      '()
      (let ((records (parse-tsv (uiop:read-file-string path :external-format :utf-8))))
        (when (and records (null (rest (first records))))
          (setf records (rest records))) ; a file of one empty line carries nothing
        (let ((header (first records)))
          (unless (and header (every (lambda (name) (member name header :test #'string=)) +fields+))
            (fail "the ledger at ~a does not begin with its columns (~{~a~^, ~}): the file is no TSV ledger"
                  path +fields+))
          (loop for record in (rest records)
                unless (and (= 1 (length record)) (zerop (length (first record))))
                  collect (loop for name in +fields+
                                for value = (nth (position name header :test #'string=) record)
                                collect (cons name (or value ""))))))))

(defun load-aliases (path)
  "PATH's predicate aliases: a canonical spelling, a tab, then its other spellings."
  (let ((aliases '()))
    (when (and path (probe-file path))
      (dolist (line (nlk:lines (uiop:read-file-string path :external-format :utf-8)))
        (let ((line (string-trim '(#\Space #\Tab) line)))
          (when (and (plusp (length line))
                     (not (char= (char line 0) #\#))
                     (find #\Tab line))
            (let* ((tab (position #\Tab line))
                   (canonical (string-trim '(#\Space #\Tab) (subseq line 0 tab))))
              (dolist (alternative (nlk:split-words (substitute #\Space #\,
                                                                 (subseq line (1+ tab)))))
                (push (cons (norm alternative) canonical) aliases)))))))
    aliases))

(defun open-ledger (&key path)
  "PATH's ledger, or the one the section names, read whole."
  (let ((ledger (make-ledger (namestring (or path (ledger-file))))))
    (setf (ledger-rows ledger) (load-rows (ledger-path ledger))
          (ledger-aliases ledger) (load-aliases (aliases-file)))
    ledger))

(defun save-ledger (ledger)
  "Write LEDGER whole through the home's atomic file seam: a temp, then one rename."
  (nlk:write-file-atomically (ledger-path ledger) (render-rows (ledger-rows ledger))
                              :mode #o600 :directory-mode #o700))

(defun canonical-predicate (ledger predicate)
  "PREDICATE as the ledger spells it: its alias's canonical form, or its own."
  (let ((key (norm predicate)))
    (or (cdr (assoc key (ledger-aliases ledger) :test #'string=)) key)))

(defun next-id (rows)
  "The id after the newest one ROWS carry, f00001 upward."
  (let ((highest 0))
    (dolist (row rows)
      (let ((id (row-field row "id")))
        (when (and (> (length id) 1)
                   (char= (char id 0) #\f)
                   (every #'digit-char-p (subseq id 1)))
          (setf highest (max highest (parse-integer id :start 1))))))
    (format nil "f~5,'0d" (1+ highest))))

;;; --- writing a fact ------------------------------------------------------------

(defun add-fact (scope subject predicate value source
                 &key (confidence "stated") valid-from)
  "Record VALUE for SCOPE/SUBJECT/PREDICATE, closing the row it supersedes.
=> a plist (:STATUS :ID :CLOSED :ROW), STATUS one of \"added\", \"closed\"
or \"unchanged\"."
  (unless (and (stringp source) (plusp (length (string-trim '(#\Space #\Tab #\Newline) source))))
    (fail "a fact is not written without a source: say where it comes from"))
  (let ((confidence (norm (or confidence "stated"))))
    (unless (member confidence +confidences+ :test #'string=)
      (fail "confidence is ~s, not stated, derived or assumed" confidence))
    (when (or (zerop (length (norm scope)))
              (zerop (length (norm subject)))
              (zerop (length (norm predicate))))
      (fail "scope, subject and predicate are all needed"))
    (with-factledger-lock
      (let* ((ledger (open-ledger))
             (predicate (canonical-predicate ledger predicate))
             (start (or valid-from (today)))
             (key (list (norm scope) (norm subject) predicate))
             (rows (ledger-rows ledger))
             (holding (remove-if-not (lambda (row)
                                       (and (zerop (length (row-field row "valid_to")))
                                            (equal key (key-of row))))
                                     rows)))
        (dolist (row holding)
          (when (string= (norm (row-field row "value")) (norm value))
            (return-from add-fact (list :status "unchanged" :id (row-field row "id")
                                         :closed '() :row row))))
        (let* ((id (next-id rows))
               (new (make-row :id id :scope (norm scope) :subject (norm subject)
                               :predicate predicate :value (string-trim '(#\Space #\Tab #\Newline)
                                                                         (princ-to-string value))
                               :valid-from start :source (string-trim '(#\Space #\Tab #\Newline)
                                                                       source)
                               :confidence confidence))
               (closed '()))
          (dolist (row holding)
            (setf (row-field row "valid_to") start
                  (row-field row "superseded_by") id)
            (push (row-field row "id") closed))
          (setf rows (append rows (list new))
                (ledger-rows ledger) rows)
          (save-ledger ledger)
          (list :status (if holding "closed" "added") :id id
                :closed (nreverse closed) :row new))))))

;;; --- reading the ledger --------------------------------------------------------

(defun scope-filter (rows scopes)
  "ROWS in one of SCOPES, or all of them when SCOPES is empty."
  (if (null scopes)
      rows
      (let ((want (mapcar #'norm scopes)))
        (remove-if-not (lambda (row) (member (norm (row-field row "scope")) want
                                             :test #'string=))
                       rows))))

(defun active-facts (&key scope subject as-of)
  "The rows holding on AS-OF (today by default) for SCOPE and SUBJECT, if given."
  (let ((reference (or as-of (today)))
        (rows '()))
    (dolist (row (ledger-rows (open-ledger)))
      (when (and (covers-p row reference)
                 (or (null scope) (zerop (length (norm scope)))
                     (string= (norm scope) (norm (row-field row "scope"))))
                 (or (null subject) (zerop (length (norm subject)))
                     (string= (norm subject) (norm (row-field row "subject")))))
        (push row rows)))
    (sort rows (lambda (a b) (or (row-string< a b)
                                 (and (string= (row-sort-key a) (row-sort-key b))
                                      (string< (row-field a "id") (row-field b "id"))))))))

(defun fact-history (subject &key scope predicate)
  "Every row recorded for SUBJECT, oldest first: what changed and when."
  (let ((ledger (open-ledger))
        (rows '()))
    (dolist (row (ledger-rows ledger))
      (when (and (string= (norm subject) (norm (row-field row "subject")))
                 (or (null scope) (zerop (length (norm scope)))
                     (string= (norm scope) (norm (row-field row "scope"))))
                 (or (null predicate) (zerop (length (norm predicate)))
                     (string= (canonical-predicate ledger predicate)
                              (norm (row-field row "predicate")))))
        (push row rows)))
    (sort rows (lambda (a b)
                 (let ((x (row-field a "valid_from")) (y (row-field b "valid_from")))
                   (cond ((string/= x y) (string< x y))
                         (t (string< (row-field a "id") (row-field b "id")))))))))

(defun search-facts (term &key scope)
  "The rows whose subject, value, predicate or source carries TERM."
  (let ((needle (norm term)))
    (if (zerop (length needle))
        '()
        (scope-filter
         (remove-if-not
          (lambda (row)
            (some (lambda (name) (search needle (norm (row-field row name))))
                  '("subject" "value" "predicate" "source")))
          (ledger-rows (open-ledger)))
         (and scope (plusp (length scope)) (list scope))))))

;;; --- a view for a prompt --------------------------------------------------------

(defun confidence-rank (row)
  "ROW's confidence as a number: the firmest first, an unknown one last."
  (or (cdr (assoc (row-field row "confidence") +confidence-rank+ :test #'string=)) 9))

(defun compile-view (&key scopes budget header order)
  "The truth holding today as compact lines, never longer than BUDGET characters.
=> the view's text."
  (let* ((ledger (open-ledger))
         (budget (or budget (setting :budget)))
         (order (or order (setting :order)))
         (reference (today))
         (rows (remove-if-not (lambda (row) (covers-p row reference)) (ledger-rows ledger))))
    (setf rows (scope-filter rows scopes))
    (setf rows (if (string= order "recency")
                   (sort rows (lambda (a b)
                                (let ((x (row-field a "valid_from"))
                                      (y (row-field b "valid_from")))
                                  (cond ((string/= x y) (string> x y))
                                        (t (row-string< a b))))))
                   (sort rows (lambda (a b)
                                (let ((x (confidence-rank a)) (y (confidence-rank b)))
                                  (cond ((/= x y) (< x y))
                                        (t (row-string< a b))))))))
    (let ((lines '())
          (used (length (or header ""))))
      (loop for row in rows
            for index from 0
            for line = (format nil "- ~a . ~a = ~a" (row-field row "subject")
                               (row-field row "predicate") (row-field row "value"))
            do (if (> (+ used (length line) 1) budget)
                   (progn (push (format nil "... (+~d more, see factledger show)"
                                        (- (length rows) index))
                                lines)
                          (loop-finish))
                   (progn (push line lines) (incf used (1+ (length line))))))
      (format nil "~@[~a~%~]~{~a~^~%~}" header (nreverse lines)))))

;;; --- what needs re-checking, and what is there to hand over -------------------

(defun stale-facts (&key days scopes)
  "The rows holding today whose valid_from is DAYS old or older.
=> ((ROW . AGE-DAYS) ...), the oldest first."
  (let ((days (or days (setting :stale-days)))
        (reference (today))
        (out '()))
    (dolist (row (scope-filter (ledger-rows (open-ledger)) scopes))
      (when (covers-p row reference)
        (let ((age (days-between (row-field row "valid_from") reference)))
          (when (and age (>= age days))
            (push (cons row age) out)))))
    (sort out (lambda (a b)
                (cond ((/= (cdr a) (cdr b)) (> (cdr a) (cdr b)))
                      (t (row-string< (car a) (car b))))))))

(defun export-facts (&key scopes include-history)
  "The rows as data for another tool: the closed ones too with INCLUDE-HISTORY."
  (let ((rows (scope-filter (ledger-rows (open-ledger)) scopes)))
    (if include-history
        rows
        (remove-if-not (lambda (row) (zerop (length (row-field row "valid_to")))) rows))))

(defun ledger-stats ()
  "The ledger's totals: how many rows, active, closed, and which scopes."
  (let* ((rows (ledger-rows (open-ledger)))
         (active (count-if (lambda (row) (zerop (length (row-field row "valid_to")))) rows)))
    (list :total (length rows)
          :active active
          :closed (- (length rows) active)
          :scopes (sort (remove-duplicates (mapcar (lambda (row) (row-field row "scope")) rows)
                                           :test #'string=)
                        #'string<))))

;;; --- the integrity check ---------------------------------------------------------

(defun key-text (key)
  "A key as the check says it: scope/subject . predicate."
  (format nil "~a/~a . ~a" (first key) (second key) (third key)))

(defun check-ledger ()
  "Every integrity problem the ledger holds, in words. An empty list is healthy."
  (let* ((ledger (open-ledger))
         (rows (ledger-rows ledger))
         (reference (today))
         (problems '())
         (seen (make-hash-table :test #'equal))
         (by-key (make-hash-table :test #'equal))
         (keys '()))
    (flet ((problem (control &rest arguments)
             (push (apply #'format nil control arguments) problems)))
      (dolist (row rows)
        (let ((id (row-field row "id")))
          (when (gethash id seen) (problem "duplicate id: ~a" id))
          (setf (gethash id seen) row))
        (dolist (name '("scope" "subject" "predicate" "value"))
          (when (zerop (length (row-field row name)))
            (problem "~a: empty column ~s" (row-field row "id") name)))
        (unless (member (row-field row "confidence") +confidences+ :test #'string=)
          (problem "~a: unknown confidence ~s" (row-field row "id")
                   (row-field row "confidence")))
        (dolist (name '("valid_from" "valid_to"))
          (let ((value (row-field row name)))
            (when (and (plusp (length value)) (null (iso-date value)))
              (problem "~a: ~a is no ISO date: ~a" (row-field row "id") name value))))
        (let ((key (key-of row)))
          (unless (gethash key by-key) (push key keys))
          (push row (gethash key by-key))))
      (dolist (key (nreverse keys))
        (let* ((group (nreverse (gethash key by-key)))
               (holding (remove-if-not (lambda (row) (covers-p row reference)) group))
               (ordered (sort (copy-list group)
                              (lambda (a b)
                                (let ((x (row-field a "valid_from"))
                                      (y (row-field b "valid_from")))
                                  (cond ((string/= x y) (string< x y))
                                        (t (string< (row-field a "id")
                                                     (row-field b "id")))))))))
          (when (> (length holding) 1)
            (problem "conflicting truth: ~d rows cover ~a for ~a -> ~{~a~^, ~}"
                     (length holding) reference (key-text key)
                     (mapcar (lambda (row) (row-field row "id")) holding)))
          (loop for (previous next) on ordered
                while next
                do (if (zerop (length (row-field previous "valid_to")))
                       (problem "overlapping spans for ~a: ~a stays open while ~a follows"
                                (key-text key) (row-field previous "id")
                                (row-field next "id"))
                       (when (string< (row-field next "valid_from")
                                      (row-field previous "valid_to"))
                         (problem "overlapping spans for ~a: ~a closes ~a, after ~a begins"
                                  (key-text key) (row-field previous "id")
                                  (row-field previous "valid_to")
                                  (row-field next "id")))))
          (dolist (row group)
            (let ((successor (row-field row "superseded_by")))
              (cond ((zerop (length successor))
                     (when (plusp (length (row-field row "valid_to")))
                       (problem "~a: a closed row names no successor" (row-field row "id"))))
                    ((null (gethash successor seen))
                     (problem "~a: superseded_by names ~a, which is no row here"
                              (row-field row "id") successor))
                    (t
                     (let ((start (row-field (gethash successor seen) "valid_from"))
                           (end (row-field row "valid_to")))
                       (when (and (plusp (length start)) (plusp (length end))
                                  (string< start end))
                         (problem "~a: its successor ~a begins before it closes"
                                  (row-field row "id") successor))))))))))
    (nreverse problems)))

