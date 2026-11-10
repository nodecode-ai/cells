;;;; filings.lisp --- the three verbs: search a file date, read a filing's
;;;; headline figures, save a document.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin's tools.py and save.py. EDINET's list API is by FILE DATE --
;;;; the day a document was submitted, not its fiscal period -- so a search
;;;; fetches one day's list and filters it here: the company name, the
;;;; securities code, the EDINET code and the corporate number never leave
;;;; this machine. Every answer is text the model reads and cites.

(in-package #:nodecode-edinet)

(defparameter +doc-types+
  '(("yuho" . ("120"))
    ("annual" . ("120"))
    ("quarterly" . ("140"))
    ("semiannual" . ("160"))
    ("extraordinary" . ("180" "190"))
    ("amendment" . ("130" "150" "170" "190")))
  "The plugin's document-kind aliases, each as its EDINET docTypeCode list.")

(defparameter +max-calendar-days+ 31
  "EDINET's own ceiling on a list range: the section may ask for less, never more.")

;;; --- file dates ----------------------------------------------------------------

(defun day-token (text field)
  "TEXT as a YYYY-MM-DD file date, an integer day number, or a refusal naming FIELD."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (unless (and text
                 (= 10 (length text))
                 (char= (char text 4) #\-)
                 (char= (char text 7) #\-)
                 (every #'digit-char-p (remove #\- text)))
      (fail "~a must be a file date as YYYY-MM-DD, for example 2024-06-25; the call said ~s."
            field (or text "nothing")))
    (handler-case
        (values (encode-universal-time 0 0 0
                                       (parse-integer text :start 8 :end 10)
                                       (parse-integer text :start 5 :end 7)
                                       (parse-integer text :start 0 :end 4)
                                       0)
                text)
      (error () (fail "~a is not a day on the calendar: ~s." field text)))))

(defun token-day (token)
  "TOKEN — a day number — as the YYYY-MM-DD EDINET spells a file date with."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time token 0)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun today-jst ()
  "Today in Japan as a day number: an EDINET file date is a Japan date."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 9)
    (declare (ignore second minute hour))
    (encode-universal-time 0 0 0 day month year 0)))

(defun day-span (from to)
  "The inclusive count of calendar days FROM..TO, both day numbers."
  (1+ (round (- to from) 86400)))

;;; --- the filters ---------------------------------------------------------------

(defun digits-only (text)
  "TEXT's digit characters, as a string."
  (remove-if-not #'digit-char-p (or text "")))

(defun sec-matches-p (row-code call-code)
  "Whether ROW-CODE answers CALL-CODE: EDINET keeps five digits where people type four."
  (let ((row (digits-only row-code))
        (call (digits-only call-code)))
    (cond ((or (zerop (length row)) (zerop (length call))) nil)
          ((string= row call) t)
          ((and (= (length call) 4) (uiop:string-prefix-p call row)) t)
          ((and (= (length row) 4) (uiop:string-prefix-p row call)) t)
          (t nil))))

(defun row-matches-p (row call)
  "Whether ROW, a document list entry, answers CALL's company filters."
  (let ((name (search-call-company-name call))
        (code (search-call-sec-code call))
        (edinet (search-call-edinet-code call))
        (jcn (search-call-corporate-number call)))
    (and (or (not (nonempty name))
             (let ((filer (or (json-member row "filerName") "")))
               (search (string-downcase (nlk:trimmed name)) (string-downcase filer))))
         (or (not (nonempty code)) (sec-matches-p (json-member row "secCode") code))
         (or (not (nonempty edinet))
             (string-equal (nlk:trimmed edinet) (or (json-member row "edinetCode") "")))
         (or (not (nonempty jcn))
             (string= (digits-only jcn) (digits-only (json-member row "JCN")))))))

(defun type-codes (call)
  "The docTypeCodes CALL's doc_types names, or NIL for every kind."
  (let ((aliases (search-call-doc-types call)))
    (when (and aliases (plusp (length aliases)))
      (let ((codes '())
            (known (loop for pair in +doc-types+ append (rest pair))))
        (loop for alias across aliases
              for key = (string-downcase (nlk:trimmed alias))
              do (cond ((rest (assoc key +doc-types+ :test #'string=))
                        (setf codes (append (rest (assoc key +doc-types+ :test #'string=)) codes)))
                       ((and (plusp (length key)) (every #'digit-char-p key))
                        (push key codes))
                       (t (fail "~s is not a document kind: use ~{~a~^, ~}, or a docTypeCode such as ~a."
                                alias (mapcar #'first +doc-types+) (first known)))))
        codes))))

(defun row-type-matches-p (row codes)
  "Whether ROW's docTypeCode is in CODES, or CODES is NIL."
  (or (null codes) (member (json-member row "docTypeCode") codes :test #'string=)))

;;; --- one day's list ------------------------------------------------------------

(defun rows-of (payload)
  "PAYLOAD's results as a list of document objects, dropping anything else."
  (let ((results (json-member payload "results")))
    (cond ((vectorp results) (remove-if-not #'hash-table-p (coerce results 'list)))
          ((hash-table-p results) (remove-if-not #'hash-table-p (coerce (or (json-member results "docs") #()) 'list)))
          (t '()))))

(defun row-warnings (row)
  "What ROW says about itself that a reader should know: an amendment, an edited entry."
  (let ((warnings '()))
    (when (json-member row "parentDocID") (push "an amendment or child of another filing" warnings))
    (when (search "訂正" (or (json-member row "docDescription") ""))
      (push "a correction filing (訂正)" warnings))
    (when (string= "2" (or (json-member row "withdrawalStatus") "")) (push "withdrawn" warnings))
    (nreverse warnings)))

(defun row-line (row)
  "ROW as one line of an answer."
  (let* ((doc-id (json-member row "docID"))
         (warnings (row-warnings row))
         (files (remove nil (list (and (string= "1" (or (json-member row "pdfFlag") "")) "PDF")
                                  (and (string= "1" (or (json-member row "csvFlag") "")) "CSV")))))
    (format nil "  ~a  ~a  ~a  ~a ~a  filed ~a~@[  [~{~a~^, ~}]~]~@[  (~{~a~^; ~})~]"
            doc-id
            (or (nonempty (json-member row "secCode")) "-")
            (or (nonempty (json-member row "filerName")) "-")
            (or (nonempty (json-member row "docTypeCode")) "-")
            (or (nonempty (json-member row "docDescription")) "")
            (or (nonempty (json-member row "submitDateTime")) "-")
            files
            warnings)))

;;; --- the search ---------------------------------------------------------------

(defun search-days (call)
  "CALL — an EDINET-SEARCH-CALL record — searched file date by file date, as text."
  (multiple-value-bind (start-day from-text) (day-token (search-call-date-from call) "date_from")
    (multiple-value-bind (end-day to-text) (day-token (search-call-date-to call) "date_to")
      (when (> start-day end-day) (fail "date_to (~a) is before date_from (~a)." to-text from-text))
      (let ((today (today-jst)))
        (when (or (> start-day today) (> end-day today))
          (fail "The range ends after today (~a, Japan time): EDINET files nothing ahead of a day."
                (token-day today))))
      (unless (some (lambda (name) (nonempty (funcall name call)))
                    (list #'search-call-company-name #'search-call-sec-code
                          #'search-call-edinet-code #'search-call-corporate-number))
        (fail "Give at least one of company_name, sec_code, edinet_code or corporate_number."))
      (when (nonempty (search-call-corporate-number call))
        (let ((digits (digits-only (search-call-corporate-number call))))
          (unless (= 13 (length digits))
            (fail "corporate_number is a 13-digit corporate number (法人番号); the call said ~s."
                  (search-call-corporate-number call)))))
      (let* ((span (day-span start-day end-day))
             (cap (min (or (option :max-calendar-days +max-calendar-days+) +max-calendar-days+)
                       +max-calendar-days+))
             (codes (type-codes call))
             (limit (or (search-call-limit call) 50))
             (pace (or (option :pace-seconds 1) 0)))
        (when (> limit 200) (fail "limit tops out at 200 filings per call; the call said ~d." limit))
        (when (> span cap)
          (fail "The range is ~d calendar days; this cell searches at most ~d per call. The range was ~
not shortened: call again with date_from=~a and date_to=~a, then continue from ~a."
                span cap from-text (token-day (+ start-day (* (1- cap) 86400)))
                (token-day (+ start-day (* cap 86400)))))
        (let ((lines '())
              (only-doc-id nil)
              (matches 0)
              (days 0)
              (missing '())
              (fetched 0))
          (loop for token from start-day to end-day by 86400
                while (< matches limit)
                do (progn
                     (when (plusp fetched) (sleep pace))
                     (incf fetched)
                     (let ((payload (fetch-day (token-day token))))
                       (incf days)
                       (if (eq payload :not-found)
                           (push (token-day token) missing)
                           (dolist (row (rows-of payload))
                             (when (and (row-matches-p row call) (row-type-matches-p row codes))
                               (incf matches)
                               (when (= matches 1) (setf only-doc-id (json-member row "docID")))
                               (when (<= matches limit) (push (row-line row) lines))))))))
          (with-output-to-string (out)
            (format out "EDINET filings ~a to ~a (~d file date~:p scanned, limit ~d): ~d found~%"
                    from-text to-text days limit matches)
            (if lines
                (dolist (line (nreverse lines)) (format out "~a~%" line))
                (format out "  none matched. The list API is by file date, not by fiscal period: ~
widen the range around the day the document was filed.~%"))
            (when missing
              (format out "  no list for ~{~a~^, ~} (a non-business day, or outside EDINET's retention)~%"
                      (nreverse missing)))
            (when (>= matches limit)
              (format out "  stopped at the ~d-filing limit; the later file dates were not scanned: ~
raise limit (up to 200) or narrow the range~%" limit))
            (format out "~a~%" (citation (and (= matches 1) only-doc-id)))))))))
