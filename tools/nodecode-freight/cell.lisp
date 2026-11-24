;;;; cell.lisp --- carrier bills against trips and rates, one deterministic pass.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A case is a billing period of one carrier: the carrier's bill lines, the
;;;; trip ledger they should answer to, the contracted rate rules, and the
;;;; evidence for the two charges that are not a flat rate (waiting blocks and
;;;; toll receipts). The model imports the files, runs the reconciliation and
;;;; reads the difference list; it never approves a payment and never writes
;;;; back to an ERP: a difference list is what this cell produces.
;;;;
;;;; The arithmetic is integer minor units end to end (no float reaches a
;;;; total), a charge with no comparable rate keeps `unknown' rather than zero,
;;;; and the same inputs produce the same summary: nothing here reads the
;;;; clock into a result except the run's own stamp.
;;;;
;;;; Storage is one JSON file per case under the section's data_dir; nothing
;;;; here dials anything.

(in-package #:nodecode-freight)

;;; --- the words ---------------------------------------------------------------

(defparameter +charges+ '("BASE" "WAIT" "TOLL")
  "The three charge codes a bill line may carry: the per-trip base freight,
waiting blocks, and a toll at its actual receipt.")

(defparameter +kinds+ '("bill" "trips" "rates" "waiting" "receipts")
  "The five files a case is made of, by the tool's `kind' member.")

(defparameter +formulas+
  '(("BASE" . "PER_TRIP") ("WAIT" . "WAIT_BLOCKS") ("TOLL" . "ACTUAL_RECEIPT"))
  "The one formula each charge code may be rated by; a rule that says
otherwise is UNSUPPORTED_FORMULA.")

(defparameter *framing*
  "[freight ~a: ~d differences of ~d groups; net difference ~a ~a. A difference is evidence for a person to read, not a payment decision.]"
  "The first line of a difference page: the case, how many differences of how
many groups, and the net difference. A layer may reword it.")

(defparameter +primer+
  "A freight bill is paid line by line, and every line has an arithmetic answer:
the rate the contract sets for that route and vehicle, the waiting blocks the
gateway log proves, or the toll receipt itself. The nodecode-freight cell keeps
the case library and works that arithmetic; its verbs answer a string, and a
refusal is ERROR: FREIGHT-ERROR with the reason.

  (freight:open-case \"november\" :carrier \"C-01\"
                     :period-from \"2026-11-01T00:00:00+08:00\"
                     :period-to \"2026-12-01T00:00:00+08:00\")
      one case: one carrier, one currency, one billing period.
  (freight:load-source \"november\" \"bill\" \"bills.csv\")
      import one CSV: kind is bill | trips | rates | waiting | receipts.
      Importing a kind again replaces that kind in the case.
  (freight:reconcile \"november\")
      match every bill line to a trip, select the one rate rule in force at
      the trip's departure, compute the expected amount in integer minor
      units and keep the difference; answer the summary.
  (freight:summary \"november\")         the last run's totals, in words
  (freight:differences \"november\")      the difference list, a page at a time
  (freight:export-review \"november\")    the difference list as a CSV for review
  (freight:cases)                         what the library holds

The model tools freight_case, freight_import, freight_run, freight_differences
and freight_export do the same five things; /freight shows the library and
runs one case. Never a payment, never a message: a person confirms.
Every amount is an integer in minor units. A line with no comparable rate is
NOT_COMPARABLE with expected unknown, never 0, and the summary's three
partitions -- comparable, unresolved, out of scope -- add up to the bill total.
Input files are frozen by digest when imported, so a re-run on the same case
answers the same numbers."
  "What (help :freight) answers while the cell runs.")

;;; --- money --------------------------------------------------------------------
;;; Integer minor units, parsed from a decimal string and never from a float:
;;; the same boundary the engine keeps, because 0.1 * 3 is not a rate.

(defun parse-minor (text &key (what "an amount"))
  "TEXT as integer minor units. A decimal with at most two places, no
exponent, no separator. Refuses anything else."
  (let* ((text (nlk:trimmed (if (stringp text) text (princ-to-string (or text "")))))
         (negative (and (plusp (length text)) (char= (char text 0) #\-)))
         (body (if negative (subseq text 1) text))
         (dot (position #\. body)))
    (flet ((digits-p (part) (and (plusp (length part)) (every #'digit-char-p part))))
      (unless (and (digits-p (if dot (subseq body 0 dot) body))
                   (or (null dot) (<= 1 (- (length body) dot 1) 2))
                   (or (null dot) (digits-p (subseq body (1+ dot)))))
        (fail "~s is not ~a: use a decimal number with at most two places, like 1200.50"
              text what))
      (let* ((whole (parse-integer (if dot (subseq body 0 dot) body)))
             (fraction (if dot
                           (let ((part (subseq body (1+ dot))))
                             (parse-integer (concatenate 'string part (if (= 1 (length part)) "0" ""))))
                           0))
             (minor (+ (* whole 100) fraction)))
        (when (> minor 1000000000000)
          (fail "~s is larger than a case can carry" text))
        (if negative (- minor) minor)))))

(defun minor-text (minor)
  "MINOR as the decimal an operator reads, or \"unknown\" when there is none."
  (if (null minor)
      "unknown"
      (let* ((abs (abs minor)))
        (format nil "~:[~;-~]~d.~2,'0d" (minusp minor) (floor abs 100) (mod abs 100)))))

(defun delta-text (minor)
  "MINOR with its sign always said, for a difference."
  (format nil "~:[+~;-~]~a" (minusp minor) (minor-text (abs minor))))

;;; --- time ---------------------------------------------------------------------
;;; An instant is read once, from an ISO string that says its timezone, and
;;; compared as integer seconds; the text is kept for the reader.

(defun parse-instant (text &key (what "a time"))
  "TEXT as integer epoch seconds: an ISO time with an explicit timezone.
Refuses a time without one, because a case can span two."
  (let* ((text (nlk:trimmed (if (stringp text) text (princ-to-string (or text "")))))
         (length (length text)))
    (flet ((digits (start end)
             (let ((part (subseq text start (min end length))))
               (unless (and (= (length part) (- end start)) (every #'digit-char-p part))
                 (fail "~s is not ~a: an ISO time like 2026-11-01T00:00:00+08:00" text what))
               (parse-integer part))))
      (unless (and (>= length 20) (char= (char text 4) #\-) (char= (char text 7) #\-)
                   (member (char text 10) '(#\T #\t #\Space)))
        (fail "~s is not ~a: an ISO time like 2026-11-01T00:00:00+08:00" text what))
      (let* ((year (digits 0 4)) (month (digits 5 7)) (day (digits 8 10))
             (hour (digits 11 13)) (minute (digits 14 16)) (second (digits 17 19))
             (rest (subseq text 19))
             (rest (if (and (plusp (length rest)) (char= (char rest 0) #\.))
                       (let ((stop (position-if-not #'digit-char-p rest :start 1)))
                         (if stop (subseq rest stop) ""))
                       rest))
             (offset (cond ((zerop (length rest))
                            (fail "~s is not ~a: it names no timezone, so two cases could read it differently"
                                  text what))
                           ((member (char rest 0) '(#\Z #\z)) 0)
                           ((member (char rest 0) '(#\+ #\-))
                            (let* ((sign (if (char= (char rest 0) #\+) 1 -1))
                                   (body (subseq rest 1))
                                   (digits (remove-if-not #'digit-char-p body)))
                              (unless (member (length digits) '(2 4))
                                (fail "~s is not ~a: its timezone is not +HH:MM, +HHMM or Z" text what))
                              (* sign (if (= 4 (length digits))
                                          (+ (* 60 (parse-integer (subseq digits 0 2)))
                                             (parse-integer (subseq digits 2 4)))
                                          (* 3600 (parse-integer digits))))))
                           (t (fail "~s is not ~a: its timezone is not +HH:MM, +HHMM or Z" text what)))))
        (handler-case
            (- (encode-universal-time second minute hour day month year 0) 2208988800 offset)
          (error () (fail "~s is not a day that exists, so it is not ~a" text what)))))))

(defun instant-text (epoch)
  "EPOCH as the short UTC stamp a reader checks: 2026-11-01T00:00Z."
  (and epoch (subseq (nlk:iso-time epoch) 0 16)))

;;; --- CSV ----------------------------------------------------------------------
;;; One reader for the five files: RFC 4180 quotes, a header row with the
;;; column names, a blank line skipped, every other line data.

(defun csv-rows (text)
  "TEXT as rows of fields: RFC 4180 quotes, a doubled quote read as one, a
row of empty fields dropped."
  (let ((rows '()) (row '()) (field (make-string-output-stream)) (quoted nil)
        (length (length text)) (position 0))
    (labels ((end-field () (push (get-output-stream-string field) row)
                           (setf field (make-string-output-stream)))
             (end-row () (end-field)
                     (unless (every (lambda (cell) (zerop (length (nlk:trimmed cell)))) row)
                       (push (nreverse row) rows))
                     (setf row '())))
      (loop while (< position length)
            for char = (char text position)
            do (cond
                 (quoted
                  (cond ((char= char #\")
                         (if (and (< (1+ position) length) (char= (char text (1+ position)) #\"))
                             (progn (write-char #\" field) (incf position))
                             (setf quoted nil)))
                        (t (write-char char field))))
                 ((char= char #\") (setf quoted t))
                 ((char= char #\,) (end-field))
                 ((char= char #\Newline) (end-row))
                 ((char= char #\Return) nil)
                 (t (write-char char field)))
               (incf position))
      (when (or row (plusp (file-position field)))
        ;; a file whose last line carries no newline is data too
        (end-row))
      (nreverse rows))))

;;; --- the case library ---------------------------------------------------------
;;; One JSON file a case, under the section's data_dir: the imported rows with
;;; the digest of the file they came from, and the last run's result. Nothing
;;; else is written, and nothing outside that folder is touched.

(defun data-directory ()
  "The folder the case library lives in, from the section."
  (uiop:ensure-directory-pathname (setting :data-dir)))

(defun case-path (name)
  "Where the case NAME is kept."
  (merge-pathnames (format nil "cases/~a.json" name) (data-directory)))

(defun case-name (text)
  "TEXT as a case's name, or a refusal."
  (let ((name (string-downcase (nlk:trimmed (if (stringp text) text (princ-to-string (or text "")))))))
    (unless (and (<= 1 (length name) 40)
                 (every (lambda (char)
                          (or (and (< (char-code char) 128) (alphanumericp char))
                              (find char "._-")))
                        name)
                 (alphanumericp (char name 0)))
      (fail "~s is not a case name: lowercase letters, digits, . _ and -, at most 40, a letter or digit first"
            text))
    name))

(defun read-case (name)
  "The case NAME as its file holds it, or a refusal naming the library."
  (let ((text (nlk:read-text (case-path name))))
    (unless text
      (fail "no case named ~s in the library: (freight:open-case ~s ...) opens one" name name))
    (or (handler-case (nlk:decode-json text) (error () nil))
        (fail "the file of case ~s is not readable JSON: import it again" name))))

(defun write-case (case)
  "CASE as its file: atomic, 0600, its folders 0700."
  (nlk:write-file-atomically (case-path (jstr case "name"))
                             (nlk:encode-json-object case)
                             :mode #o600 :directory-mode #o700)
  case)

(defun sources-of (case)
  "CASE's source object, made when a case has none yet."
  (or (nlk:json-value case :object "sources")
      (setf (gethash "sources" case) (nlk:make-json-object))))

(defun source-rows (case kind)
  "The rows imported for KIND in CASE, as a vector of objects."
  (let ((source (nlk:json-value (sources-of case) :object kind)))
    (or (and source (nlk:json-value source :array "rows")) #())))

(defun jstr (object key)
  "OBJECT's KEY as a string, \"\" when absent."
  (or (nlk:json-value object :string key) ""))

(defun jint (object key &optional default)
  "OBJECT's KEY as an integer, DEFAULT when absent."
  (or (nlk:json-value object :integer key) default))

(defun jbool (object key)
  "OBJECT's KEY as a boolean: JSON true only."
  (and (nlk:json-value object :boolean key) t))

;;; --- importing a file ------------------------------------------------------------

(defun header-cell (text)
  "A header cell as the column name this cell reads: lowercase, spaces and
dashes as underscores."
  (string-downcase
   (substitute #\_ #\Space
               (substitute #\_ #\- (nlk:trimmed text)))))

(defun column-index (header names &key (what "file"))
  "An alist of each name of NAMES to its column in HEADER, or a refusal
naming the column the file lacks."
  (let ((index '()))
    (dolist (name names (nreverse index))
      (let ((position (position name header :test #'string=)))
        (unless position
          (fail "the ~a needs a ~a column; this one has ~{~a~^, ~}" what name header))
        (push (cons name position) index)))))

(defun full-index (header required what)
  "Every column of HEADER by name, refusing when one of REQUIRED is missing,
so a carrier that left a needed column out is told which one."
  (let ((index (loop for name in header for position from 0 collect (cons name position))))
    (dolist (name required)
      (unless (assoc name index :test #'string=)
        (fail "the ~a needs a ~a column; this one has ~{~a~^, ~}" what name header)))
    index))

(defun cell (row name index)
  "ROW's field for the column NAME, \"\" when the file has no such column
or the row is short: an optional column the carrier did not send."
  (let ((position (cdr (assoc name index :test #'string=))))
    (if position (or (nth position row) "") "")))

(defun cell-value (row name index)
  "ROW's field for NAME, NIL when empty or absent."
  (let ((value (nlk:trimmed (cell row name index))))
    (unless (zerop (length value)) value)))

(defmacro cell-or (row name index default)
  "ROW's field for NAME, else DEFAULT -- a form, evaluated only when the
field is empty, so a refusal costs nothing on a good row."
  `(or (cell-value ,row ,name ,index) ,default))

(defun cell-flag (row name index &optional (default t))
  "ROW's NAME as a boolean: true/yes/1, false/no/0, DEFAULT when empty."
  (let ((value (string-downcase (nlk:trimmed (cell row name index)))))
    (cond ((member value '("true" "yes" "1" "y") :test #'string=) t)
          ((member value '("false" "no" "0" "n") :test #'string=) nil)
          ((zerop (length value)) default)
          (t (fail "~s is not a yes or no" value)))))

(defun parse-bill-row (row index line default-currency)
  (nlk:make-json-object
   "line_id" (cell-or row "line_id" index (format nil "L~d" line))
   "carrier" (cell-or row "carrier_id" index (fail "a bill row names no carrier_id"))
   "trip_ref" (cell-or row "trip_ref" index "")
   "leg" (cell-or row "service_leg_id" index "")
   "charge" (cell-or row "charge_code" index (fail "a bill row names no charge_code"))
   "occurrence" (cell-or row "occurrence_id" index "")
   "currency" (cell-or row "currency" index default-currency)
   "basis" (cell-or row "amount_basis" index "")
   "amount" (parse-minor (cell row "amount" index) :what "a bill amount")
   "reversal_of" (cell-or row "reversal_of" index "")
   "source" (cell-or row "source" index "")))

(defun parse-trip-row (row index line)
  (declare (ignore line))
  (nlk:make-json-object
   "trip_id" (cell-or row "trip_id" index (fail "a trip row names no trip_id"))
   "carrier" (cell-or row "carrier_id" index (fail "a trip row names no carrier_id"))
   "route" (cell-or row "route_id" index "")
   "vehicle" (cell-or row "vehicle_class" index "")
   "status" (string-upcase (cell-or row "execution_status" index "EXECUTED"))
   "departed" (parse-instant (cell row "departed_at" index) :what "a departure time")
   "departed_at" (nlk:trimmed (cell row "departed_at" index))))

(defun parse-rate-row (row index line default-currency)
  (declare (ignore line))
  (let* ((from (parse-instant (cell row "effective_from" index) :what "an effective_from time"))
         (to (parse-instant (cell row "effective_to" index) :what "an effective_to time"))
         (block (parse-integer-or (cell-or row "block_seconds" index "1") "block_seconds")))
    (unless (< from to)
      (fail "a rate rule's effective_to is not after its effective_from"))
    (unless (plusp block)
      (fail "a rate rule's block_seconds is not a positive count of seconds"))
    (nlk:make-json-object
     "rule_id" (cell-or row "rule_id" index (fail "a rate row names no rule_id"))
     "version" (parse-integer-or (cell-or row "version" index "1") "version")
     "carrier" (cell-or row "carrier_id" index (fail "a rate row names no carrier_id"))
     "route" (cell-or row "route_id" index "")
     "vehicle" (cell-or row "vehicle_class" index "")
     "charge" (cell-or row "charge_code" index (fail "a rate row names no charge_code"))
     "currency" (cell-or row "currency" index default-currency)
     "basis" (cell-or row "amount_basis" index "")
     "from" from "to" to
     "formula" (string-upcase (cell-or row "formula" index (or (cdr (assoc (cell-or row "charge_code" index "BASE") +formulas+ :test #'string=)) "")))
     "rate" (parse-minor (cell-or row "rate" index "0") :what "a rate")
     "free" (parse-integer-or (cell-or row "free_seconds" index "0") "free_seconds")
     "block" block
     "cap" (let ((cap (nlk:trimmed (cell row "cap" index))))
             (unless (zerop (length cap)) (parse-minor cap :what "a cap")))
     "confirmed" (cell-flag row "confirmed" index))))

(defun parse-integer-or (text what)
  "TEXT as a non-negative integer, or a refusal naming WHAT."
  (let ((text (nlk:trimmed text)))
    (unless (and (plusp (length text)) (every #'digit-char-p text))
      (fail "~s is not a whole number of ~a" text what))
    (parse-integer text)))

(defun parse-wait-row (row index line)
  (declare (ignore line))
  (nlk:make-json-object
   "trip_id" (cell-or row "trip_id" index (fail "a waiting row names no trip_id"))
   "occurrence" (cell-or row "occurrence_id" index "")
   "elapsed" (parse-integer-or (cell-or row "elapsed_seconds" index (fail "a waiting row names no elapsed_seconds"))
                                "seconds")
   "verified" (cell-flag row "verified" index)))

(defun parse-receipt-row (row index line)
  (declare (ignore line))
  (nlk:make-json-object
   "trip_id" (cell-or row "trip_id" index (fail "a receipt row names no trip_id"))
   "occurrence" (cell-or row "occurrence_id" index "")
   "amount" (parse-minor (cell-or row "amount" index (fail "a receipt row names no amount"))
                          :what "a receipt amount")
   "verified" (cell-flag row "verified" index)))

(defun parse-row (kind header row line default-currency)
  "ROW, line LINE of a KIND file, as the object the case keeps. The columns
the kind needs are checked once, here; every other column of the file is
optional and reads empty when the carrier did not send it."
  (let ((index (full-index header
                            (ecase (intern (string-upcase kind) :keyword)
                              (:bill '("carrier_id" "charge_code" "amount"))
                              (:trips '("trip_id" "carrier_id" "departed_at"))
                              (:rates '("rule_id" "carrier_id" "charge_code"
                                        "effective_from" "effective_to"))
                              (:waiting '("trip_id" "elapsed_seconds"))
                              (:receipts '("trip_id" "amount")))
                            kind)))
    (handler-case
        (ecase (intern (string-upcase kind) :keyword)
          (:bill (parse-bill-row row index line default-currency))
          (:trips (parse-trip-row row index line))
          (:rates (parse-rate-row row index line default-currency))
          (:waiting (parse-wait-row row index line))
          (:receipts (parse-receipt-row row index line)))
      (freight-error (condition)
        (fail "line ~d: ~a" line (freight-error-detail condition))))))

(defun resolve-file (text)
  "TEXT as an existing file: absolute, or from the home folder with ~."
  (let* ((text (nlk:trimmed (if (stringp text) text (princ-to-string (or text "")))))
         (path (cond ((zerop (length text)) (fail "a source needs a file path"))
                     ((and (> (length text) 1) (char= (char text 0) #\~))
                      (merge-pathnames (subseq text 2) (user-homedir-pathname)))
                     (t (pathname text)))))
    (or (probe-file path)
        (fail "no file at ~a" text))))

;;; --- the pass ------------------------------------------------------------------

(defun group-key (line trip-id)
  "The seven parts a bill line is grouped by: one amount total a group."
  (list (jstr line "carrier") trip-id (jstr line "leg") (jstr line "charge")
        (jstr line "occurrence") (jstr line "currency") (jstr line "basis")))

(defun group-id (key)
  "KEY as the group's id, a reader's handle on one bill total."
  (format nil "GRP-~{~a~^-~}"
          (mapcar (lambda (part) (if (zerop (length part)) "NONE" part)) key)))

(defun formula-for (charge)
  "The one formula a CHARGE code may be rated by."
  (or (cdr (assoc charge +formulas+ :test #'string=)) ""))

(defun wait-minor (elapsed free block rate)
  "The waiting charge: the free seconds taken off first, the rest rounded up
to whole blocks, never a negative count."
  (* (ceiling (max 0 (- elapsed free)) block) rate))

(defun reversal-issue (lines)
  "REVERSAL_INVALID when a credit names no positive original in the same
group, or credits more than the original."
  (let ((by-id (make-hash-table :test #'equal))
        (credited (make-hash-table :test #'equal)))
    (dolist (line lines) (setf (gethash (jstr line "line_id") by-id) line))
    (dolist (line lines)
      (let ((amount (jint line "amount" 0))
            (target (jstr line "reversal_of")))
        (cond
          ((minusp amount)
           (let ((original (gethash target by-id)))
             (unless (and original
                          (plusp (jint original "amount" 0))
                          (string= (jstr original "reversal_of") ""))
               (return-from reversal-issue "REVERSAL_INVALID"))
             (incf (gethash target credited 0) (- amount))))
          ((plusp (length target)) (return-from reversal-issue "REVERSAL_INVALID")))))
    (maphash (lambda (target amount)
               (when (> amount (jint (gethash target by-id) "amount" 0))
                 (return-from reversal-issue "REVERSAL_INVALID")))
             credited)
    nil))

(defun select-rule (group route vehicle when rules)
  "The one confirmed rule in force at WHEN for GROUP's route, vehicle and
charge. => (values RULE ISSUE)."
  (let ((scoped '()))
    (loop for rule across rules
          when (and (string= (jstr rule "carrier") (getf group :carrier))
                    (string= (jstr rule "route") route)
                    (string= (jstr rule "vehicle") vehicle)
                    (string= (jstr rule "charge") (getf group :charge))
                    (<= (jint rule "from" 0) when)
                    (< when (jint rule "to" 0)))
            do (push rule scoped))
    (cond ((null scoped) (values nil "RATE_NOT_FOUND"))
          ((cdr scoped) (values nil "RATE_AMBIGUOUS"))
          (t (values (first scoped) nil)))))

(defun event-for (rows trip occurrence)
  "The one row of ROWS for TRIP and OCCURRENCE, or NIL."
  (loop for row across rows
        when (and (string= (jstr row "trip_id") trip)
                  (string= (jstr row "occurrence") occurrence))
          return row))

(defun expected-for (rule group waits receipts)
  "The expected amount in minor units for GROUP's charge.
=> (values EXPECTED ISSUE REFS)."
  (let ((charge (getf group :charge))
        (trip (getf group :trip))
        (occurrence (getf group :occurrence)))
    (cond
      ((string= charge "BASE") (values (jint rule "rate" 0) nil '()))
      ((string= charge "WAIT")
       (let ((event (event-for waits trip occurrence)))
         (if (or (null event) (not (jbool event "verified")) (null (jint event "elapsed")))
             (values nil "WAIT_EVIDENCE_INVALID" '())
             (values (wait-minor (jint event "elapsed") (jint rule "free" 0)
                                  (jint rule "block" 1) (jint rule "rate" 0))
                     nil
                     (list (format nil "waiting#~a/~a" trip (if (zerop (length occurrence)) "-" occurrence)))))))
      ((string= charge "TOLL")
       (let ((verified (loop for receipt across receipts
                             when (and (string= (jstr receipt "trip_id") trip)
                                       (string= (jstr receipt "occurrence") occurrence)
                                       (jbool receipt "verified")
                                       (jint receipt "amount"))
                               collect receipt)))
         (if (null verified)
             (values nil "MISSING_RECEIPT" '())
             (let* ((total (reduce #'+ verified :key (lambda (receipt) (jint receipt "amount" 0))))
                    (cap (jint rule "cap"))
                    (expected (if cap (min total cap) total)))
               (values expected nil
                       (list (format nil "receipts#~a/~a" trip
                                     (if (zerop (length occurrence)) "-" occurrence))))))))
      (t (values nil "UNSUPPORTED_FORMULA" '())))))

(defun reconcile-group (group trips rules waits receipts)
  "One group's arithmetic: matching, evidence, the rule in force, the
expected amount and the difference. => the result plist."
  (let* ((lines (getf group :lines))
         (trip-id (getf group :trip))
         (trip (and trip-id (gethash trip-id trips)))
         (billed (reduce #'+ lines :key (lambda (line) (jint line "amount" 0))))
         (issues '())
         (refs '())
         (result (list :id (getf group :id) :trip trip-id :charge (getf group :charge)
                       :leg (getf group :leg) :occurrence (getf group :occurrence)
                       :currency (getf group :currency) :billed billed
                       :expected nil :delta nil :comparison "NOT_COMPARABLE"
                       :evidence "COMPLETE" :issues nil :refs nil :rule nil
                       :line-count (length lines))))
    (block done
      (flet ((finish (code)
               (when code (push code issues))
               (setf (getf result :issues) (sort (remove-duplicates issues :test #'string=) #'string<)
                     (getf result :refs) (sort (remove-duplicates refs :test #'string=) #'string<))
               (return-from done result)))
        (dolist (line lines)
          (let ((source (jstr line "source")))
            (push (if (zerop (length source))
                      (format nil "bill#line=~a" (jstr line "line_id"))
                      source)
                  refs)))
        (when trip (push (format nil "trips#trip=~a" trip-id) refs))
        ;; A trip whose delivery evidence nobody indexed is not complete
        ;; evidence: it is a gap the reviewer fills, never a silent pass.
        (when trip
          (push "MISSING_POD" issues)
          (setf (getf result :evidence) "MISSING"))
        (unless (member (getf group :match) '("EXACT" "CONFIRMED") :test #'string=)
          (finish (if (string= (getf group :match) "CONFLICT")
                      "CARRIER_CONFLICT"
                      "UNMATCHED")))
        (unless (string= (jstr trip "status") "EXECUTED")
          (finish "TRIP_NOT_EXECUTED"))
        (when (or (reversal-issue lines) (minusp billed))
          (finish "REVERSAL_INVALID"))
        (multiple-value-bind (rule issue)
            (select-rule group (jstr trip "route") (jstr trip "vehicle")
                         (jint trip "departed") rules)
          (when issue (finish issue))
          (setf (getf result :rule) (format nil "~a@~a" (jstr rule "rule_id") (jint rule "version" 1)))
          (push (getf result :rule) refs)
          (unless (jbool rule "confirmed") (finish "RULE_UNCONFIRMED"))
          (unless (and (string= (jstr rule "currency") (getf group :currency))
                       (string= (jstr rule "basis") (getf group :basis)))
            (finish "BASIS_MISMATCH"))
          (unless (string= (jstr rule "formula") (formula-for (getf group :charge)))
            (finish "UNSUPPORTED_FORMULA"))
          (multiple-value-bind (expected compute-issue where) (expected-for rule group waits receipts)
            (when compute-issue
              (when (member compute-issue '("WAIT_EVIDENCE_INVALID" "MISSING_RECEIPT") :test #'string=)
                (setf (getf result :evidence) "MISSING"))
              (finish compute-issue))
            (dolist (ref where) (push ref refs))
            (let ((delta (- billed expected)))
              (setf (getf result :comparison) "COMPARABLE"
                    (getf result :expected) expected
                    (getf result :delta) delta)
              (cond ((plusp delta) (push "AMOUNT_HIGH" issues))
                    ((minusp delta) (push "AMOUNT_LOW" issues)))
              (when (> (count-if (lambda (line)
                                   (and (plusp (jint line "amount" 0))
                                        (string= (jstr line "reversal_of") "")))
                                 lines)
                       1)
                (push "DUPLICATE_SUSPECT" issues))
              (finish nil))))))))

(defun not-billed-trips (case)
  "Executed trips departed inside the billing period that no bill line names."
  (let* ((from (jint case "period_from"))
         (to (jint case "period_to"))
         (billed (make-hash-table :test #'equal))
         (missing '()))
    (loop for line across (source-rows case "bill")
          do (let ((ref (jstr line "trip_ref")))
               (unless (zerop (length ref)) (setf (gethash ref billed) t))))
    (loop for trip across (source-rows case "trips")
          for id = (jstr trip "trip_id")
          for at = (jint trip "departed")
          do (when (and (string= (jstr trip "status") "EXECUTED")
                        at (<= from at) (< at to)
                        (not (gethash id billed)))
               (push id missing)))
    (sort missing #'string<)))

;;; --- the whole pass ------------------------------------------------------------

(defun reconcile-all (case)
  "Match every bill line, group them and compute each group.
=> (values RESULTS OUT-OF-SCOPE-LINES)."
  (let* ((carrier (jstr case "carrier"))
         (trips (make-hash-table :test #'equal))
         (groups (make-hash-table :test #'equal))
         (ordered '())
         (out '())
         (rules (source-rows case "rates"))
         (waits (source-rows case "waiting"))
         (receipts (source-rows case "receipts")))
    (loop for trip across (source-rows case "trips")
          do (setf (gethash (jstr trip "trip_id") trips) trip))
    (loop for line across (source-rows case "bill")
          do (let ((row-carrier (jstr line "carrier"))
                   (charge (jstr line "charge")))
               (cond
                 ((not (string= row-carrier carrier))
                  (push (list :line line :issue "CARRIER_CONFLICT") out))
                 ((not (member charge +charges+ :test #'string=))
                  (push (list :line line :issue "UNSUPPORTED") out))
                 ((not (string= (jstr line "currency") (jstr case "currency")))
                  (push (list :line line :issue "CURRENCY_UNSUPPORTED") out))
                 (t
                  ;; P1 only: the carrier's own trip_ref names a trip of the
                  ;; ledger. An unknown ref is unmatched; a ref whose trip
                  ;; belongs to another carrier is a conflict, never a match.
                  (let* ((ref (jstr line "trip_ref"))
                         (trip (unless (zerop (length ref)) (gethash ref trips)))
                         (trip-id (and trip (string= (jstr trip "carrier") row-carrier) ref))
                         (state (cond (trip-id "EXACT")
                                      (trip "CONFLICT")
                                      (t "UNMATCHED")))
                         (key (group-key line (or trip-id "")))
                         (group (gethash key groups)))
                    (unless group
                      (setf group (list :id (group-id key) :carrier row-carrier
                                        :trip trip-id :leg (jstr line "leg")
                                        :charge charge :occurrence (jstr line "occurrence")
                                        :currency (jstr line "currency")
                                        :basis (jstr line "basis")
                                        :match state :lines '())
                            (gethash key groups) group)
                      (push group ordered))
                    (push line (getf group :lines)))))))
    (let ((ordered (sort ordered #'string< :key (lambda (group) (getf group :id)))))
      (values (loop for group in ordered
                    collect (reconcile-group group trips rules waits receipts))
              (nreverse out)))))

(defun summarize (results out)
  "The run's totals, with the three partitions kept apart: comparable,
unresolved and out of scope add up to the bill total, and the differences
add up to the net."
  (let ((summary (list :groups 0 :bill_lines 0 :comparable_groups 0 :comparable_lines 0
                       :source_total 0 :comparable_billed 0 :expected 0 :unresolved 0
                       :out_of_scope 0 :out_of_scope_lines 0 :positive 0 :negative 0
                       :net 0 :group_issues 0)))
    (loop for result in results
          do (incf (getf summary :groups))
             (incf (getf summary :bill_lines) (getf result :line-count))
             (incf (getf summary :source_total) (getf result :billed))
             (if (string= (getf result :comparison) "COMPARABLE")
                 (progn (incf (getf summary :comparable_groups))
                        (incf (getf summary :comparable_lines) (getf result :line-count))
                        (incf (getf summary :comparable_billed) (getf result :billed))
                        (incf (getf summary :expected) (getf result :expected))
                        (incf (getf summary :positive) (max 0 (getf result :delta)))
                        (incf (getf summary :negative) (min 0 (getf result :delta))))
                 (incf (getf summary :unresolved) (getf result :billed)))
             (when (getf result :issues) (incf (getf summary :group_issues))))
    (loop for item in out
          do (let ((amount (jint (getf item :line) "amount" 0)))
               (incf (getf summary :out_of_scope_lines))
               (incf (getf summary :bill_lines))
               (incf (getf summary :source_total) amount)
               (incf (getf summary :out_of_scope) amount)))
    (setf (getf summary :net) (+ (getf summary :positive) (getf summary :negative)))
    (unless (= (getf summary :source_total)
               (+ (getf summary :comparable_billed) (getf summary :unresolved)
                  (getf summary :out_of_scope)))
      (fail "the three partitions do not add up to the bill total: this is a bug in the pass"))
    (unless (= (- (getf summary :comparable_billed) (getf summary :expected))
               (getf summary :net))
      (fail "the differences do not add up to the net: this is a bug in the pass"))
    summary))

(defun out-result (item)
  "An out-of-scope bill line as a difference row."
  (let ((line (getf item :line)))
    (list :id (format nil "BIL-~a" (jstr line "line_id"))
          :trip (jstr line "trip_ref") :charge (jstr line "charge")
          :leg (jstr line "leg") :occurrence (jstr line "occurrence")
          :currency (jstr line "currency") :billed (jint line "amount" 0)
          :expected nil :delta nil :comparison "NOT_COMPARABLE" :evidence "COMPLETE"
          :issues (list (getf item :issue))
          :refs (list (format nil "bill#line=~a" (jstr line "line_id")))
          :rule nil :line-count 1)))

(defun result->json (result)
  "RESULT as the case file keeps it."
  (nlk:make-json-object
   "id" (getf result :id) "trip" (getf result :trip) "charge" (getf result :charge)
   "leg" (getf result :leg) "occurrence" (getf result :occurrence)
   "currency" (getf result :currency) "billed" (getf result :billed)
   "expected" (getf result :expected) "delta" (getf result :delta)
   "comparison" (getf result :comparison) "evidence" (getf result :evidence)
   "issues" (coerce (getf result :issues) 'vector)
   "refs" (coerce (getf result :refs) 'vector)
   "rule" (getf result :rule) "lines" (getf result :line-count)))

(defun summary->json (summary)
  (apply #'nlk:make-json-object
         (loop for (key value) on summary by #'cddr
               append (list (substitute #\_ #\- (string-downcase (symbol-name key))) value))))

(defun json->summary (object)
  "The summary OBJECT read back as the plist SUMMARY-* builds."
  (loop for key in (list :groups :bill_lines :comparable_groups :comparable_lines
                          :source_total :comparable_billed :expected :unresolved
                          :out_of_scope :out_of_scope_lines :positive :negative
                          :net :group_issues)
        append (list key (or (nlk:json-value object :integer
                                               (substitute #\_ #\- (string-downcase (symbol-name key))))
                             0))))

(defun run-case (case)
  "Run the pass over CASE, store it and answer its summary plist."
  (when (zerop (length (source-rows case "bill")))
    (fail "case ~a has no bill imported: (freight:load-source ~s \"bill\" PATH) first"
          (jstr case "name") (jstr case "name")))
  (multiple-value-bind (results out) (reconcile-all case)
    (let* ((summary (summarize results out))
           (issues (append (remove-if-not (lambda (result) (getf result :issues)) results)
                           (mapcar #'out-result out)))
           (unbilled (not-billed-trips case)))
      (setf (gethash "run" case)
            (nlk:make-json-object
             "at" (nlk:unix-now)
             "summary" (summary->json summary)
             "issues" (coerce (mapcar #'result->json issues) 'vector)
             "not_billed" (coerce unbilled 'vector)
             "groups" (coerce (mapcar #'result->json results) 'vector)))
      (write-case case)
      summary)))

;;; --- what the operator and the model read ------------------------------------------

(defun summary-text (case summary)
  "CASE's run as the few lines a reader checks first."
  (format nil "[freight ~a: carrier ~a, ~a, ~a to ~a~% ~d bill lines in ~d groups: ~d comparable, ~d unresolved, ~d out of scope~% billed ~a, expected ~a, net difference ~a ~a; ~d group~:p with issues, ~d trip~:p not billed]"
          (jstr case "name") (jstr case "carrier") (jstr case "currency")
          (instant-text (jint case "period_from")) (instant-text (jint case "period_to"))
          (getf summary :bill_lines) (getf summary :groups)
          (getf summary :comparable_groups)
          (- (getf summary :groups) (getf summary :comparable_groups))
          (getf summary :out_of_scope_lines)
          (minor-text (getf summary :source_total))
          (minor-text (getf summary :expected))
          (delta-text (getf summary :net)) (jstr case "currency")
          (getf summary :group_issues)
          (length (or (nlk:json-value (nlk:json-value case :object "run") :array "not_billed") #()))))

(defun run-summary (case)
  "CASE's stored run summary as a plist, or a refusal."
  (let ((run (nlk:json-value case :object "run")))
    (unless run
      (fail "case ~a has no reconciliation yet: (freight:reconcile ~s) runs one"
            (jstr case "name") (jstr case "name")))
    (json->summary (nlk:json-value run :object "summary"))))

(defun difference-rows (case)
  "Every difference the last run left, as objects: the groups with issues,
then the executed trips nobody billed."
  (let* ((run (nlk:json-value case :object "run"))
         (issues (and run (nlk:json-value run :array "issues")))
         (unbilled (and run (nlk:json-value run :array "not_billed"))))
    (append (loop for row across (or issues #()) collect row)
            (loop for trip across (or unbilled #())
                  collect (nlk:make-json-object
                           "id" (format nil "TRIP-~a" trip) "trip" trip "charge" ""
                           "billed" nil "expected" nil "delta" nil
                           "comparison" "NOT_BILLED" "evidence" "COMPLETE"
                           "issues" (vector "NOT_BILLED") "refs" (vector))))))

(defun row-issues (row)
  "ROW's issue codes as a list."
  (loop for code across (or (nlk:json-value row :array "issues") #()) collect code))

(defun filter-rows (rows filter)
  "ROWS narrowed by FILTER: all, high, low, unresolved, not-billed."
  (cond
    ((string= filter "all") rows)
    ((string= filter "high") (remove-if-not (lambda (row) (member "AMOUNT_HIGH" (row-issues row) :test #'string=)) rows))
    ((string= filter "low") (remove-if-not (lambda (row) (member "AMOUNT_LOW" (row-issues row) :test #'string=)) rows))
    ((string= filter "unresolved") (remove-if (lambda (row) (string= (jstr row "comparison") "COMPARABLE")) rows))
    ((string= filter "not-billed") (remove-if-not (lambda (row) (member "NOT_BILLED" (row-issues row) :test #'string=)) rows))
    (t rows)))

(defun difference-line (row)
  "One difference as a line: what was billed, what the contract makes it, by
how much it is off, and the evidence behind both numbers."
  (let* ((issues (row-issues row))
         (refs (loop for ref across (or (nlk:json-value row :array "refs") #()) collect ref)))
    (format nil "~a ~a~@[ trip ~a~]~@[ occurrence ~a~]: billed ~a~@[, expected ~a~]~@[, delta ~a~]~@[ — ~{~a~^, ~}~]~@[ [~{~a~^; ~}]~]"
            (jstr row "id") (jstr row "charge")
            (let ((trip (jstr row "trip"))) (and (plusp (length trip)) trip))
            (let ((occurrence (jstr row "occurrence"))) (and (plusp (length occurrence)) occurrence))
            (minor-text (nlk:json-value row :integer "billed"))
            (and (plusp (length (jstr row "charge")))
                 (minor-text (nlk:json-value row :integer "expected")))
            (and (string= (jstr row "comparison") "COMPARABLE")
                 (delta-text (or (nlk:json-value row :integer "delta") 0)))
            issues refs)))

(defun differences-text (case page filter)
  "One page of CASE's difference list, with how many there are in all."
  (let* ((rows (filter-rows (difference-rows case) filter))
         (size (setting :page-size))
         (pages (max 1 (ceiling (length rows) size)))
         (page (max 1 (min page pages)))
         (start (* size (1- page)))
         (shown (loop for index from start below (min (length rows) (+ start size))
                      collect (difference-line (nth index rows))))
         (summary (run-summary case)))
    (if (null rows)
        (format nil "[freight ~a: no differences~@[ under the ~a filter~]]"
                (jstr case "name") (and (not (string= filter "all")) filter))
        (format nil "~a~%~{~a~^~%~}~%[page ~d of ~d, differences ~d-~d of ~d]"
                (format nil *framing* (jstr case "name") (length rows)
                        (getf summary :groups) (delta-text (getf summary :net))
                        (jstr case "currency"))
                shown page pages (1+ start) (+ start (length shown)) (length rows)))))

(defun csv-field (text)
  "TEXT as one CSV field, quoted when it has to be."
  (let ((text (or text "")))
    (if (find-if (lambda (char) (member char '(#\, #\" #\Newline #\Return))) text)
        (with-output-to-string (out)
          (write-char #\" out)
          (loop for char across text
                do (when (char= char #\") (write-char #\" out))
                   (write-char char out))
          (write-char #\" out))
        text)))

(defun export-csv (case)
  "CASE's difference list as CSV, header first."
  (with-output-to-string (out)
    (format out "group_id,trip_id,charge_code,occurrence,billed,expected,delta,comparison,evidence,issues,rule,evidence_refs~%")
    (dolist (row (difference-rows case))
      (format out "~{~a~^,~}~%"
              (mapcar #'csv-field
                      (list (jstr row "id") (jstr row "trip") (jstr row "charge")
                            (jstr row "occurrence")
                            (minor-text (jint row "billed" 0))
                            (if (string= (jstr row "comparison") "COMPARABLE")
                                (minor-text (jint row "expected" 0)) "")
                            (if (string= (jstr row "comparison") "COMPARABLE")
                                (delta-text (jint row "delta" 0)) "")
                            (jstr row "comparison") (jstr row "evidence")
                            (format nil "~{~a~^ ~}" (row-issues row))
                            (or (nlk:json-value row :string "rule") "")
                            (format nil "~{~a~^ ~}"
                                    (loop for ref across (or (nlk:json-value row :array "refs") #())
                                          collect ref))))))))

(defun case-line (file)
  "One line of the library: the case FILE, its period and its last run."
  (handler-case
      (let* ((case (read-case (pathname-name file)))
             (run (nlk:json-value case :object "run"))
             (summary (and run (json->summary (nlk:json-value run :object "summary")))))
        (if summary
            (format nil "~a [~a, ~a, ~a to ~a] - ~d groups, net ~a ~a, ~d difference~:p"
                    (jstr case "name") (jstr case "carrier") (jstr case "currency")
                    (instant-text (jint case "period_from")) (instant-text (jint case "period_to"))
                    (getf summary :groups) (delta-text (getf summary :net))
                    (jstr case "currency") (length (difference-rows case)))
            (format nil "~a [~a, ~a, ~a to ~a] - imported only"
                    (jstr case "name") (jstr case "carrier") (jstr case "currency")
                    (instant-text (jint case "period_from")) (instant-text (jint case "period_to")))))
    (freight-error (condition)
      (format nil "~a (unreadable: ~a)" (file-namestring file)
              (freight-error-detail condition)))))

;;; --- the verbs the package exports --------------------------------------------

(define-verb cases ()
  "The case library under the section's data_dir: one line a case."
  (let* ((folder (merge-pathnames "cases/" (data-directory)))
         (files (and (probe-file folder) (uiop:directory-files folder "*.json"))))
    (if (null files)
        "no cases yet: (freight:open-case \"NAME\" :carrier \"C-01\" :period-from \"2026-11-01T00:00:00+08:00\" :period-to \"2026-12-01T00:00:00+08:00\") opens one"
        (format nil "~{~a~^~%~}" (sort (mapcar #'case-line files) #'string<)))))

(define-verb open-case (name &key carrier period-from period-to currency workspace)
  "Open a case: one carrier, one currency, one billing period. A name that
already exists starts that case over."
  (let* ((name (case-name name))
         (carrier-id (nlk:trimmed (if (stringp carrier) carrier (princ-to-string (or carrier "")))))
         (from (parse-instant period-from :what "the billing period's opening"))
         (to (parse-instant period-to :what "the billing period's closing"))
         (existing (probe-file (case-path name))))
    (unless (plusp (length carrier-id))
      (fail "a case needs a carrier id: the one its bill lines and trips carry"))
    (unless (< from to)
      (fail "the billing period closes before it opens"))
    (let* ((currency (string-upcase
                       (let ((text (nlk:trimmed (if (stringp currency) currency (princ-to-string (or currency ""))))))
                         (if (zerop (length text)) (setting :currency) text))))
           (workspace (let ((text (nlk:trimmed (if (stringp workspace) workspace (princ-to-string (or workspace ""))))))
                        (if (zerop (length text)) "default" text)))
           (case (nlk:make-json-object
                  "version" 1
                  "name" name
                  "carrier" carrier-id
                  "workspace" workspace
                  "currency" currency
                  "period_from" from "period_to" to
                  "period_from_text" (nlk:trimmed (if (stringp period-from) period-from ""))
                  "period_to_text" (nlk:trimmed (if (stringp period-to) period-to ""))
                  "sources" (nlk:make-json-object)
                  "run" nil)))
      (write-case case)
      (format nil "case ~a opened~@[ (the earlier case of that name is replaced)~]: carrier ~a, ~a, ~a to ~a~%next: (freight:load-source ~s \"bill\" PATH) for bill, trips and rates, then (freight:reconcile ~s)"
              name existing carrier-id currency (instant-text from) (instant-text to)
              name name))))

(define-verb load-source (name kind path)
  "Import one CSV into a case: bill, trips, rates, waiting or receipts.
Importing a kind again replaces that kind; the file's digest is kept with
its rows, so a re-run on the same case answers the same numbers."
  (let* ((name (case-name name))
         (kind (string-downcase (nlk:trimmed (if (stringp kind) kind (princ-to-string (or kind ""))))))
         (case (read-case name))
         (file (resolve-file path))
         (text (or (nlk:read-text file) (fail "~a carries nothing" file))))
    (unless (member kind +kinds+ :test #'string=)
      (fail "~s is not a source kind: bill, trips, rates, waiting or receipts" kind))
    (let* ((rows (csv-rows text))
           (header (and rows (mapcar #'header-cell (first rows))))
           (data (rest rows)))
      (when (null rows) (fail "~a carries no rows" file))
      (when (null data) (fail "~a carries a header and nothing else" file))
      (when (> (length data) (setting :max-rows))
        (fail "~a carries ~d rows; freight.max_rows is ~d in the section"
              file (length data) (setting :max-rows)))
      (let ((parsed (loop for row in data
                          for line from 2
                          collect (parse-row kind header row line (jstr case "currency")))))
        (setf (gethash kind (sources-of case))
              (nlk:make-json-object
               "file" (file-namestring file)
               "digest" (nlk:sha256-text text)
               "count" (length parsed)
               "rows" (coerce parsed 'vector)))
        (write-case case)
        (format nil "imported ~d ~a rows from ~a into case ~a (sha256 ~a); the previous ~a source, if any, is replaced"
                (length parsed) kind (file-namestring file) name
                (subseq (nlk:sha256-text text) 7 19) kind)))))

(define-verb reconcile (name)
  "Run the pass over a case's imported files and answer its summary. The
same inputs answer the same numbers; nothing is paid and nothing is sent."
  (let ((case (read-case (case-name name))))
    (summary-text case (run-case case))))

(define-verb summary (name)
  "The totals of a case's last reconciliation, in words."
  (let ((case (read-case (case-name name))))
    (summary-text case (run-summary case))))

(define-verb differences (name &key (page 1) (filter "all"))
  "One page of a case's difference list: billed, expected, delta, issues and
the evidence refs behind both numbers."
  (let* ((case (read-case (case-name name)))
         (filter (string-downcase (nlk:trimmed (if (stringp filter) filter "all")))))
    (unless (member filter '("all" "high" "low" "unresolved" "not-billed") :test #'string=)
      (fail "~s is not a filter: all, high, low, unresolved or not-billed" filter))
    (cond ((not (integerp page)) (fail "a page is a whole number"))
          ((< page 1) (fail "pages start at 1")))
    (run-summary case)
    (differences-text case page filter)))

(define-verb export-review (name)
  "Write a case's difference list as a CSV under the library's exports
folder, for a person to review and confirm."
  (let* ((name (case-name name))
         (case (read-case name)))
    (run-summary case)
    (let* ((rows (difference-rows case))
           (path (merge-pathnames (format nil "exports/~a-review.csv" name) (data-directory))))
      (nlk:write-file-atomically path (export-csv case) :mode #o600 :directory-mode #o700)
      (format nil "exported ~d difference row~:p to ~a: review it and decide; nothing was sent anywhere"
              (length rows) (namestring path)))))

;;; --- the tools the model calls ------------------------------------------------

(nlk:define-contract freight-case
  (:doc "Open a freight reconciliation case: one carrier, one currency, one billing period. A case holds the imported bill lines, the trip ledger, the contracted rate rules and the evidence for waiting and tolls.")
  ("case" :text :required t :slot case-name :doc "the case name: lowercase letters, digits, . _ and -")
  ("carrier" :text :required t :doc "the carrier id that the bill lines and the trips carry")
  ("period_from" :text :required t :doc "the ISO time the billing period opens, with its timezone")
  ("period_to" :text :required t :doc "the ISO time it closes; a departure at that instant is outside")
  ("currency" :text :default "" :doc "ISO 4217 code of the case; the section's currency when empty")
  ("workspace" :text :default "" :doc "the workspace the rate rules are scoped to"))

(nlk:define-contract freight-source
  (:doc "Import one CSV into a case: the carrier's bill lines, the trip ledger, the contracted rate rules, or the evidence a waiting or toll charge needs. Importing a kind again replaces that kind in the case, atomically.")
  ("case" :text :required t :slot case-name :doc "the case name")
  ("kind" :text :required t :options +kinds+
   :doc "bill | trips | rates | waiting | receipts")
  ("path" :text :required t
   :doc "the CSV file: an absolute path, or one from the home folder when it starts with ~"))

(nlk:define-contract freight-run
  (:doc "Reconcile a case: match every bill line to a trip, select the one contracted rate rule in force at that trip's departure, compute the expected amount in integer minor units and keep the difference. Nothing is paid, approved or sent.")
  ("case" :text :required t :slot case-name :doc "the case name"))

(nlk:define-contract freight-differences
  (:doc "Read a case's difference list a page at a time: what was billed, what the contract makes it, the delta, the issue codes and the evidence refs behind both numbers.")
  ("case" :text :required t :slot case-name :doc "the case name")
  ("page" :integer :default 1 :doc "which page of the list")
  ("filter" :text :default "all" :options '("all" "high" "low" "unresolved" "not-billed")
   :doc "all, or only over-billed, under-billed, unresolved, or trips nobody billed"))

(nlk:define-contract freight-export
  (:doc "Write a case's difference list as a CSV under the case library's exports folder, for a person to review and confirm.")
  ("case" :text :required t :slot case-name :doc "the case name"))

(defun tool-case (entry)
  "freight_case: open a case."
  (open-case (freight-case-case-name entry)
             :carrier (freight-case-carrier entry)
             :period-from (freight-case-period-from entry)
             :period-to (freight-case-period-to entry)
             :currency (freight-case-currency entry)
             :workspace (freight-case-workspace entry)))

(defun tool-import (entry)
  "freight_import: import one CSV into a case."
  (load-source (freight-source-case-name entry)
               (freight-source-kind entry)
               (freight-source-path entry)))

(defun tool-run (entry)
  "freight_run: reconcile a case."
  (reconcile (freight-run-case-name entry)))

(defun tool-differences (entry)
  "freight_differences: one page of a case's difference list."
  (differences (freight-differences-case-name entry)
               :page (freight-differences-page entry)
               :filter (freight-differences-filter entry)))

(defun tool-export (entry)
  "freight_export: a case's difference list as a CSV."
  (export-review (freight-export-case-name entry)))

;;; --- /freight -----------------------------------------------------------------

(defun first-word (text)
  "TEXT up to its first blank."
  (let ((space (position-if (lambda (char) (member char '(#\Space #\Tab))) text)))
    (if space (subseq text 0 space) text)))

(defun panel ()
  "The library as /freight's panel: one case a line."
  (let* ((folder (merge-pathnames "cases/" (data-directory)))
         (files (and (probe-file folder) (uiop:directory-files folder "*.json"))))
    (or (and files (format nil "~{~a~^~%~}" (sort (mapcar #'case-line files) #'string<)))
        "no cases yet")))

(defun run-slash (args session-id)
  "/freight: the case library, run CASE, diff CASE, export CASE."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (head (string-downcase (first-word text)))
         (rest (nlk:trimmed (subseq text (length (first-word text))))))
    (cond ((member head '("" "cases") :test #'string=) (values (cases) (panel)))
          ((string= head "run") (reconcile (if (zerop (length rest)) (fail "run needs a case name") rest)))
          ((string= head "diff")
           (multiple-value-bind (case page) (values (first-word rest)
                                                     (let ((tail (nlk:trimmed (subseq rest (length (first-word rest))))))
                                                       (if (zerop (length tail)) 1 (parse-integer tail :junk-allowed t))))
             (if (zerop (length case))
                 (fail "diff needs a case name")
                 (differences case :page (or page 1)))))
          ((string= head "export") (export-review (if (zerop (length rest)) (fail "export needs a case name") rest)))
          (t (reconcile head)))))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell freight
  (:section ("freight")
    (:guide "data_dir is the folder the case library lives in, on a local disk; the agent opens a case, imports the carrier's CSVs, and runs the reconciliation")
    ("data_dir" :directory :required t
     :doc "the folder the case library lives in: one JSON file a case, one CSV an export")
    ("currency" :string :default "CNY"
     :doc "the currency a case uses when the tool names none")
    ("page_size" :integer :default 20 :min 1
     :doc "how many differences one page of the list carries")
    ("max_rows" :integer :default 20000 :min 1
     :doc "how many data rows one imported file may carry"))
  ;; A required member that is merely absent does not refuse by itself: the
  ;; start does, because every verb writes under data_dir and none may guess.
  (:start (lambda ()
            (unless (setting :data-dir)
              (error 'nlk:config-refusal
                     :detail "freight.data_dir is required: the folder the case library lives in, on a local disk"))))
  (:tool "freight_case" (nlk:contract 'freight-case) 'tool-case)
  (:tool "freight_import" (nlk:contract 'freight-source) 'tool-import)
  (:tool "freight_run" (nlk:contract 'freight-run) 'tool-run)
  (:tool "freight_differences" (nlk:contract 'freight-differences) 'tool-differences)
  (:tool "freight_export" (nlk:contract 'freight-export) 'tool-export)
  (:help :freight "freight: reconcile carrier bills against trip ledgers and contracted rates, locally and deterministically; /freight" +primer+)
  (:command "freight" 'run-slash
            :catches 'freight-error
            :description "The freight case library: run a case, read its differences"
            :argument-hint "[cases | run CASE | diff CASE | export CASE]"))
