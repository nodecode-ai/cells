;;;; fleetkeep.lisp --- the CSV, the six columns, and what needs review.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original package was one SKILL.md: prose telling a model to read the
;;;; spreadsheet, map six columns, keep unknown dates blank, flag duplicate and
;;;; missing registrations, and leave a cell that might be a formula out of the
;;;; import until the user approves it. Its rules are kept here and made
;;;; arithmetic, so the same answer comes back whatever the model is: REVIEW
;;;; is where every rule lives and what the tests read; the tool and the
;;;; command are a few lines around it.
;;;;
;;;; Nothing here reads a network, a key or a file: REVIEW takes the CSV text
;;;; it is given, and only WRITE-RESULTS writes, into the output_dir the
;;;; operator set. The source file is never touched.

(in-package #:nodecode-fleetkeep)

;;; --- what a Fleetkeep import takes -------------------------------------------

(defparameter +columns+
  '("registration" "label" "insurance_due" "last_service_date"
    "last_service_mileage" "service_interval_months")
  "The six core import columns, in the one order the import takes them.")

(defparameter +aliases+
  '(("registration" "reg" "registration_number" "registration_no" "vrm" "plate" "number_plate")
    ("label" "name" "vehicle" "vehicle_name" "fleet_name" "vehicle_label")
    ("insurance_due" "insurance" "insurance_date" "insurance_expiry" "insurance_expires"
     "insurance_due_date" "insurance_renewal")
    ("last_service_date" "last_service" "last_service_on" "service_date" "serviced_on")
    ("last_service_mileage" "mileage" "last_mileage" "current_mileage" "odometer"
     "mileage_at_last_service" "last_service_miles")
    ("service_interval_months" "service_interval" "interval_months" "service_months"
     "service_every_months" "interval"))
  "The spellings a column may arrive as, its canonical name first. A header
the table does not name is unmapped: it is named in the review and kept out
of the import, which takes six columns and no more.")

(defparameter *date-orders* '("day-first" "month-first")
  "What date_order accepts: which reading of 03/04/2026 the review names
first. Neither is chosen for the user - that is the original's rule.")

(defparameter *months*
  #("January" "February" "March" "April" "May" "June" "July" "August"
    "September" "October" "November" "December")
  "The month names a review spells a numeric date out with.")

(defparameter *issue-limit* 40
  "How many issues the report spells out; the counts above it are always whole.")

;;; --- the CSV itself -----------------------------------------------------------

(defun parse-csv (text)
  "TEXT's records, each a list of fields. RFC 4180 quoting: a field may be
wrapped in double quotes, a doubled quote inside it is one quote, and a
comma or a newline inside it is data. A CRLF ends a record as an LF does,
and a final newline adds no record."
  (let ((rows '()) (row '()) (field (make-string-output-stream)) (quoted nil) (started nil)
        (length (length text)) (index 0))
    (labels ((end-field () (push (get-output-stream-string field) row) (setf started t))
           (end-row () (end-field) (push (nreverse row) rows) (setf row '() started nil)))
      (loop while (< index length)
            for character = (char text index)
            do (cond
                 (quoted
                  (cond ((char= character #\")
                         (if (and (< (1+ index) length) (char= (char text (1+ index)) #\"))
                             (progn (write-char #\" field) (incf index))
                             (setf quoted nil)))
                        (t (write-char character field))))
                 ((char= character #\") (setf quoted t started t))
                 ((char= character #\,) (end-field))
                 ((char= character #\Newline) (end-row))
                 ((char= character #\Return) nil)
                 (t (write-char character field) (setf started t)))
               (incf index))
      (when (or started row) (end-row))
      (nreverse rows))))

(defun field-text (value)
  "VALUE as one CSV field: quoted when it carries a comma, a quote, a newline
or an edge of whitespace, and its quotes doubled as the rules say."
  (let ((text (or value "")))
    (if (or (find-if (lambda (character) (member character '(#\, #\" #\Newline #\Return))) text)
            (and (plusp (length text))
                 (or (member (char text 0) '(#\Space #\Tab))
                     (member (char text (1- (length text))) '(#\Space #\Tab)))))
        (format nil "\"~a\""
                (with-output-to-string (out)
                  (loop for character across text
                        do (if (char= character #\") (write-string "\"\"" out) (write-char character out)))))
        text)))

(defun write-csv (rows)
  "ROWS as CSV text, one record a line, the last without a newline."
  (format nil "~{~a~^~%~}"
          (mapcar (lambda (row) (format nil "~{~a~^,~}" (mapcar #'field-text row))) rows)))

;;; --- reading a value -----------------------------------------------------------

(defun digits-p (text)
  "Whether TEXT is one or more digits and nothing else."
  (and (plusp (length text)) (every #'digit-char-p text)))

(defun normalize-header (text)
  "TEXT as a header key: trimmed, downcased, a run of spaces, dashes or
underscores read as one underscore."
  (let ((out (make-string-output-stream)) (separator nil))
    (loop for character across (string-downcase (nlk:trimmed (or text "")))
          do (if (member character '(#\Space #\Tab #\- #\/ #\. #\_))
                 (unless separator (write-char #\_ out) (setf separator t))
                 (progn (write-char character out) (setf separator nil))))
    (get-output-stream-string out)))

(defun digits-only-of (text)
  "TEXT without the commas, spaces and underscores a hand-typed number carries."
  (remove-if (lambda (character) (member character '(#\, #\Space #\Tab #\_)))
             (nlk:trimmed text)))

(defun number-of (text)
  "TEXT as a number, commas and spaces dropped; NIL when it is not one. Only
digits, a sign and a point are read, so no reader syntax can arrive."
  (let ((clean (digits-only-of text)))
    (and (plusp (length clean))
         (every (lambda (character)
                  (or (digit-char-p character) (member character '(#\+ #\- #\.))))
                clean)
         (let ((value (ignore-errors (read-from-string clean))))
           (and (realp value) value)))))

(defun formula-p (value)
  "Whether a spreadsheet would read VALUE as a formula: =, +, - or @ as its
first character, after any leading spaces or control characters. A plain
signed number (-500, +12) is a number, not a formula."
  (let* ((trimmed (string-left-trim '(#\Space #\Tab #\Return #\Newline #\Page) (or value "")))
         (first (and (plusp (length trimmed)) (char trimmed 0))))
    (and first (member first '(#\= #\+ #\- #\@)) (null (number-of trimmed)))))

(defun iso-date-p (text)
  "Whether TEXT is YYYY-MM-DD with a month 1-12 and a day 1-31."
  (and (= (length text) 10)
       (char= (char text 4) #\-) (char= (char text 7) #\-)
       (digits-p (subseq text 0 4)) (digits-p (subseq text 5 7)) (digits-p (subseq text 8 10))
       (<= 1 (parse-integer text :start 5 :end 7) 12)
       (<= 1 (parse-integer text :start 8 :end 10) 31)))

(defun split-date (text)
  "TEXT's numeric date as (values FIRST SECOND YEAR) when it is one written
with slashes, dots or dashes; NIL otherwise."
  (let ((parts (uiop:split-string text :separator '(#\/ #\. #\-))))
    (when (and (= 3 (length parts))
               (every #'digits-p parts)
               (<= 1 (length (first parts)) 2)
               (<= 1 (length (second parts)) 2)
               (<= 2 (length (third parts)) 4)
               (<= 1000 (parse-integer (third parts)) 9999))
      (values (parse-integer (first parts)) (parse-integer (second parts))
              (parse-integer (third parts))))))

(defun month-name (number)
  "NUMBER as a month's name, or NIL when it is no month."
  (and (integerp number) (<= 1 number 12) (aref *months* (1- number))))

(defun date-issue (value order)
  "VALUE's problem as a date: (values KIND TEXT), KIND :OK when it has none."
  (cond
    ((iso-date-p value) (values :ok nil))
    (t (multiple-value-bind (first second year) (split-date value)
         (cond
           ((null first) (values :bad-date (format nil "~s is not a date" value)))
           ((= first second)
            (values :not-iso (format nil "~s is not YYYY-MM-DD (it reads ~d ~a ~d either way)"
                                      value first (month-name second) year)))
           ((or (> first 12) (> second 12))
            (let ((day (if (> first 12) first second))
                  (month (if (> first 12) second first)))
              (if (null (month-name month))
                  (values :bad-date (format nil "~s has no month in it" value))
                  (values :not-iso (format nil "~s is not YYYY-MM-DD (it reads ~d ~a ~d)"
                                            value day (month-name month) year)))))
           (t (let ((day-first (format nil "~d ~a ~d" first (month-name second) year))
                    (month-first (format nil "~d ~a ~d" second (month-name first) year)))
                (values :ambiguous-date
                        (if (string= order "month-first")
                            (format nil "~s reads ~a month-first or ~a day-first - neither chosen"
                                    value month-first day-first)
                            (format nil "~s reads ~a day-first or ~a month-first - neither chosen"
                                    value day-first month-first))))))))))

;;; --- the headers --------------------------------------------------------------

(defun map-headers (header)
  "HEADER's columns as (values MAPPED UNMAPPED DUPLICATE): MAPPED a list of
(CANONICAL . INDEX) in the table's order, UNMAPPED a list of (NAME . INDEX)
the table does not name, DUPLICATE a list of (CANONICAL NAME . INDEX) for a
second column taking a name an earlier one took."
  (let ((mapped '()) (unmapped '()) (duplicate '()))
    (loop for name in header
          for index from 0
          for key = (normalize-header name)
          do (let ((names (find-if (lambda (spellings) (member key spellings :test #'string=)) +aliases+)))
               (cond ((null names) (push (cons (or name "") index) unmapped))
                     ((assoc (first names) mapped :test #'string=)
                      (push (list (first names) (or name "") index) duplicate))
                     (t (push (cons (first names) index) mapped)))))
    (values (nreverse mapped) (nreverse unmapped) (nreverse duplicate))))

(defun column-index (mapped name)
  "The index MAPPED gives NAME, or NIL when the table has no such column."
  (cdr (assoc name mapped :test #'string=)))

(defun field (row index)
  "ROW's field at INDEX, or NIL."
  (and index (nth index row)))

(defun blank-p (text)
  "Whether TEXT is NIL or nothing but whitespace."
  (or (null text) (zerop (length (nlk:trimmed text)))))

;;; --- what may go in the import-ready file ---------------------------------------

(defun usable-date-p (value)
  "Whether VALUE may go in a date column as it stands: empty, or ISO already."
  (or (blank-p value) (iso-date-p (nlk:trimmed value))))

(defun usable-mileage-p (value)
  "Whether VALUE may go in the mileage column as it stands."
  (let ((number (and (not (blank-p value)) (number-of value))))
    (if (blank-p value) t (and number (not (minusp number))))))

(defun usable-interval-p (value)
  "Whether VALUE may go in the interval column as it stands: a positive whole number of months."
  (let ((number (and (not (blank-p value)) (number-of value))))
    (if (blank-p value) t (and number (integerp number) (plusp number)))))

;;; --- the review ---------------------------------------------------------------

(defun review (csv &key (date-order "day-first") (formula-guard t) (max-rows 5000))
  "CSV text reviewed against the six columns a Fleetkeep import takes. => a
plist: :rows how many were read, :truncated how many were passed over,
:mapped, :unmapped and :duplicate from MAP-HEADERS, :problems at the header,
:issues one plist each (:row :column :kind :value :text), :ready the
import-ready rows in the six columns, :held how many rows a formula kept out,
:file the import-ready CSV, :report the review in words."
  (let* ((table (parse-csv (or csv "")))
         (header (first table))
         (body (rest table)))
    (if (or (null header) (every #'blank-p header))
        (list :rows 0 :truncated 0 :mapped '() :unmapped '() :duplicate '()
              :problems '("the CSV has no header row") :issues '() :ready '() :held 0
              :empty t :file "" :report (report-text (list :empty t)))
        (multiple-value-bind (mapped unmapped duplicate) (map-headers header)
          (let ((truncated (max 0 (- (length body) max-rows)))
                (body (if (> (length body) max-rows) (subseq body 0 max-rows) body))
                (issues '()) (tail nil) (ready '()) (ready-tail nil) (held 0)
                (seen (make-hash-table :test #'equal))
                (problems '()))
            (unless (or (column-index mapped "registration") unmapped)
              (push "no registration column, so no row can be told from another" problems))
            (flet ((add (number column kind value text)
                     (let ((cell (list (list :row number :column (or column "") :kind kind
                                             :value (or value "") :text text))))
                       (if tail (setf (cdr tail) cell) (setf issues cell))
                       (setf tail cell)))
                   (keep (candidate)
                     (let ((cell (list candidate)))
                       (if ready-tail (setf (cdr ready-tail) cell) (setf ready cell))
                       (setf ready-tail cell))))
              (loop for row in body
                    for number from 2
                    for start = (length issues)
                    do (progn
                         (when (/= (length row) (length header))
                           (add number "" :ragged (format nil "~d" (length row))
                                (format nil "the row has ~d cell~:p where the header has ~d"
                                        (length row) (length header))))
                         ;; the registration: kept, and flagged, never dropped
                         (let* ((registration (nlk:trimmed (or (field row (column-index mapped "registration")) "")))
                                (key (string-upcase (remove-if (lambda (c) (member c '(#\Space #\Tab #\-)))
                                                               registration))))
                           (cond ((zerop (length registration))
                                  (add number "registration" :missing-registration ""
                                       "no registration: the row is kept and flagged"))
                                 ((gethash key seen)
                                  (add number "registration" :duplicate-registration registration
                                       (format nil "the same registration as row ~d" (gethash key seen))))
                                 (t (setf (gethash key seen) number))))
                         ;; a cell a spreadsheet might run
                         (loop for (column . index) in mapped
                               for value = (field row index)
                               when (and value (formula-p value))
                                 do (add number column :formula value
                                         (format nil "~s may be read as a spreadsheet formula" value)))
                         ;; the two dates
                         (loop for column in '("insurance_due" "last_service_date")
                               for index = (column-index mapped column)
                               for value = (field row index)
                               when (and value (not (blank-p value)))
                                 do (multiple-value-bind (kind text)
                                        (date-issue (nlk:trimmed value) date-order)
                                      (unless (eq kind :ok) (add number column kind value text))))
                         ;; the mileage
                         (let ((value (field row (column-index mapped "last_service_mileage"))))
                           (when (and value (not (blank-p value)))
                             (let ((number-value (number-of value)))
                               (cond ((null number-value)
                                      (add number "last_service_mileage" :bad-mileage value
                                           (format nil "~s is not a number of miles" (nlk:trimmed value))))
                                     ((minusp number-value)
                                      (add number "last_service_mileage" :negative-mileage value
                                           (format nil "~s is below zero" (nlk:trimmed value))))))))
                         ;; the interval
                         (let ((value (field row (column-index mapped "service_interval_months"))))
                           (when (and value (not (blank-p value)))
                             (let ((number-value (number-of value)))
                               (cond ((null number-value)
                                      (add number "service_interval_months" :bad-interval value
                                           (format nil "~s is not a number of months" (nlk:trimmed value))))
                                     ((not (integerp number-value))
                                      (add number "service_interval_months" :bad-interval value
                                           (format nil "~s is not a whole number of months" (nlk:trimmed value))))
                                     ((<= number-value 0)
                                      (add number "service_interval_months" :bad-interval value
                                           (format nil "~s is not a positive interval" (nlk:trimmed value))))))))
                         (let ((kinds (loop for entry in (nthcdr start issues) collect (getf entry :kind))))
                           (if (and formula-guard (member :formula kinds))
                               (incf held)
                               (keep (list (nlk:trimmed (or (field row (column-index mapped "registration")) ""))
                                           (or (field row (column-index mapped "label")) "")
                                           (or (when (usable-date-p (field row (column-index mapped "insurance_due")))
                                                 (nlk:trimmed (field row (column-index mapped "insurance_due"))))
                                               "")
                                           (or (when (usable-date-p (field row (column-index mapped "last_service_date")))
                                                 (nlk:trimmed (field row (column-index mapped "last_service_date"))))
                                               "")
                                           (when (usable-mileage-p (field row (column-index mapped "last_service_mileage")))
                                             (digits-only-of (field row (column-index mapped "last_service_mileage"))))
                                           (when (usable-interval-p (field row (column-index mapped "service_interval_months")))
                                             (digits-only-of (field row (column-index mapped "service_interval_months"))))))))))
            (let ((result (list :rows (length body) :truncated truncated
                                :mapped mapped :unmapped unmapped :duplicate duplicate
                                :problems (nreverse problems) :issues issues
                                :ready ready :held held)))
              (append result
                      (list :file (write-csv (cons +columns+ ready))
                            :report (report-text result))))))))))

;;; --- the review in words -------------------------------------------------------

(defun report-text (result &key written)
  "RESULT as the review a person reads: what was read, how many rows are
import-ready, and every row that needs review. WRITTEN names the files, when
any were written."
  (if (getf result :empty)
      "fleetkeep: nothing to read - the CSV has no header row."
      (let* ((issues (getf result :issues))
             (rows (remove-duplicates (mapcar (lambda (issue) (getf issue :row)) issues))))
        (with-output-to-string (out)
          (format out "fleetkeep: ~d row~:p read" (getf result :rows))
          (when (plusp (getf result :truncated))
            (format out ", the first ~d of them" (getf result :truncated)))
          (format out ". ~d issue~:p in ~d row~:p.~%" (length issues) (length rows))
          (format out "  mapped: ~{~a~^, ~}~%" (mapcar #'car (getf result :mapped)))
          (let ((unmapped (getf result :unmapped)))
            (when unmapped
              (format out "  unmapped, kept out of the import: ~{~a~^, ~}~%" (mapcar #'car unmapped))))
          (let ((duplicate (getf result :duplicate)))
            (when duplicate
              (format out "  a second column takes: ~{~a~^; ~}~%"
                      (mapcar (lambda (entry) (format nil "~a (column ~d)" (first entry) (third entry)))
                              duplicate))))
          (let ((problems (getf result :problems)))
            (when problems (format out "  the header: ~{~a~^; ~}~%" problems)))
          (format out "  ~d row~:p import-ready, in the import's six columns" (length (getf result :ready)))
          (when (plusp (getf result :held))
            (format out ", ~d held out for review - a cell that may be a formula" (getf result :held)))
          (format out ".~%")
          (let ((shown (if (> (length issues) *issue-limit*) (subseq issues 0 *issue-limit*) issues)))
            (dolist (issue shown)
              (format out "  row ~d · ~a: ~a~%"
                      (getf issue :row)
                      (let ((column (getf issue :column)))
                        (if (plusp (length column)) column "the row"))
                      (getf issue :text))))
          (when (> (length issues) *issue-limit*)
            (format out "  ... and ~d more.~%" (- (length issues) *issue-limit*)))
          (when written
            (format out "  wrote ~{~a~^ and ~}.~%" written))))))

(defun file-name (name)
  "NAME as a file's base name: downcased, without what a path cannot carry."
  (let ((clean (string-downcase
                (remove-if (lambda (character)
                             (member character '(#\/ #\\ #\: #\* #\? #\" #\< #\> #\| #\Space)))
                           (nlk:trimmed (or name ""))))))
    (if (plusp (length clean)) clean "vehicles")))

(defun write-results (result directory name)
  "RESULT's import-ready CSV and its review written under DIRECTORY as NAME:
=> the two paths, as namestrings. Only these two files are written, and only
in the folder the operator set."
  (when (blank-p directory)
    (fail "no output_dir set: name the folder a review writes its two files into"))
  (let* ((folder (uiop:ensure-directory-pathname (nlk:trimmed directory)))
         (csv (merge-pathnames (format nil "~a.import.csv" (file-name name)) folder))
         (review (merge-pathnames (format nil "~a.review.md" (file-name name)) folder)))
    (ensure-directories-exist csv)
    (with-open-file (out csv :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (getf result :file) out))
    (with-open-file (out review :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string (report-text result) out))
    (list (namestring csv) (namestring review))))

;;; --- the verbs ----------------------------------------------------------------

(defun review-text (csv &key (name "vehicles") (write t))
  "CSV reviewed as the settings say, and written under output_dir unless
WRITE is false: the review in words."
  (let* ((result (review csv
                         :date-order (setting :date-order)
                         :formula-guard (setting :formula-guard)
                         :max-rows (setting :max-rows)))
         (written (and write (write-results result (setting :output-dir) name))))
    (report-text result :written written)))

(defun run-review (arguments)
  "The fleetkeep_review tool: ARGUMENTS' csv reviewed and, unless it says
otherwise, written under output_dir."
  (handler-case
      (review-text (review-arguments-csv arguments)
                   :name (review-arguments-name arguments)
                   :write (review-arguments-save arguments))
    (fleetkeep-error (condition) (format nil "fleetkeep: ~a" condition))))

;;; --- /fleetkeep ----------------------------------------------------------------

(defun settings-text ()
  "What /fleetkeep says bare: the settings in force, and how to hand it a file."
  (format nil "fleetkeep: the proposed CSV and the review go to ~a; a date that reads both ways is named ~a first and never chosen; a formula-like cell ~:[does not hold a row back~;holds its row out of the import~]; at most ~d rows a review. /fleetkeep FILE.csv reviews one."
          (setting :output-dir) (setting :date-order) (setting :formula-guard) (setting :max-rows)))

(defun resolve-path (text)
  "TEXT as a pathname; a relative one against the process's own directory."
  (let ((path (pathname text)))
    (if (uiop:absolute-pathname-p path) path (merge-pathnames path (uiop:getcwd)))))

(defun run-slash (args session-id)
  "The /fleetkeep command: bare, the settings in force; a path, that file
reviewed and written. => the answer's text."
  (declare (ignore session-id))
  (running-settings)
  (let ((word (nlk:trimmed (or args ""))))
    (if (zerop (length word))
        (settings-text)
        (let ((path (resolve-path word)))
          (if (null (probe-file path))
              (format nil "fleetkeep: no file at ~a" (namestring path))
              (handler-case
                  (review-text (uiop:read-file-string path)
                               :name (or (pathname-name path) "vehicles"))
                (fleetkeep-error (condition) (format nil "fleetkeep: ~a" condition))))))))
