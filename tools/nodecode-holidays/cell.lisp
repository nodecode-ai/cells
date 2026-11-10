;;;; cell.lisp --- a country's public holidays, and the workday math on them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Akloenx123/hermes-public-holidays (see NOTICE). Holiday
;;;; data comes from the public Nager.Date API (https://date.nager.at, no
;;;; key); weekends, regional filtering, counting and adding are computed
;;;; here, so the model never does calendar math in its head. The original's
;;;; four tools are the four here, /holidays is the operator's way in, and
;;;; (help :holidays) is the model's page.

(defpackage #:nodecode-holidays
  (:use #:cl)
  (:nicknames #:holidays)
  (:export #:holidays-error #:holidays-error-detail))

(in-package #:nodecode-holidays)

(nlk:define-peripheral holidays :not-running t)

;;; --- the words ------------------------------------------------------------------

(defparameter +primer+
  "Public holidays and workday arithmetic, through the nodecode-holidays cell: four
tools, and one verb of the same name each in the holidays: package for eval.
Holiday data is Nager.Date (date.nager.at, no key); weekends, regions and the
counting are computed locally, so a workday is never guessed.
  holidays_list {country, year?, subdivision?, types?}   the year's holidays, with weekdays
  workday_check {date, country, subdivision?, weekend?, types?}   is that date a workday?
  workdays_count {start, end, country, ...}              workdays in a range, and its holidays
  workdays_add {start, workdays, country, ...}           the date N workdays away, for a deadline
country is an ISO 3166-1 alpha-2 code (DE, AT, CH, US, GB). Without a subdivision
only nationwide holidays count as days off: name one (DE-NI, US-CA) to count the
region's too, or set it in the section so every call gets it. types defaults to
Public; Bank, School, Authorities, Optional and Observance are the others. weekend
defaults to Saturday and Sunday. A country Nager.Date does not cover is a refusal
naming it, never an empty calendar."
  "What (help :holidays) answers while the cell runs.")

(defparameter +types+ '("Public" "Bank" "School" "Authorities" "Optional" "Observance")
  "The holiday types Nager.Date publishes; the declaration's enum reads this list.")

(defparameter +default-types+ '("Public")
  "The types that count when a call names none: the original's default.")

(defparameter +weekday-names+ #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
  "Monday first, as the weekday index counts: 0 is Monday.")

(defparameter +default-weekend+ '(5 6)
  "Saturday and Sunday, as weekday indexes.")

(defparameter +max-add-workdays+ 2000
  "The furthest one workdays_add call walks, either way: the original's cap.")

(defparameter +max-body-chars+ 1000000
  "The largest Nager.Date answer parsed: the original's 1 MB cap.")

;;; --- dates, as serial days -------------------------------------------------------
;;; Every walk here is over day numbers, so no timezone or DST is in the count.

(defun leap-year-p (year)
  "T when YEAR has a 29th of February."
  (and (zerop (mod year 4))
       (or (plusp (mod year 100)) (zerop (mod year 400)))))

(defun month-length (month year)
  "The days in MONTH of YEAR, 0 for a month that is not 1..12."
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))
    (t 0)))

(defun day-number (year month date)
  "The day's serial number: days since 1970-01-01, anchored at noon so no
daylight shift moves a count."
  (floor (- (encode-universal-time 0 0 12 date month year 0)
            (encode-universal-time 0 0 12 1 1 1970 0))
         86400))

(defun date-from-day-number (number)
  "NUMBER's date => (values YEAR MONTH DATE)."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (+ (encode-universal-time 0 0 12 1 1 1970 0) (* number 86400)) 0)
    (declare (ignore second minute hour))
    (values year month date)))

(defun weekday-of (year month date)
  "The date's weekday index: Monday 0 ... Sunday 6. 1970-01-01 was a Thursday."
  (mod (+ (day-number year month date) 3) 7))

(defun weekday-name (index)
  "Weekday INDEX as the word a person reads."
  (aref +weekday-names+ index))

(defun iso-date (year month date)
  "The date as YYYY-MM-DD."
  (format nil "~4,'0d-~2,'0d-~2,'0d" year month date))

(defun iso-date-text-p (text)
  "T when TEXT is shaped YYYY-MM-DD and nothing else."
  (and (= 10 (length text))
       (char= (char text 4) #\-)
       (char= (char text 7) #\-)
       (loop for index from 0 below 10
             always (or (= index 4) (= index 7) (digit-char-p (char text index))))))

(defun parse-iso-date (value field)
  "VALUE as a calendar date => (values YEAR MONTH DATE), or a refusal naming FIELD."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (unless (and text (iso-date-text-p text))
      (fail "~a must be an ISO date YYYY-MM-DD, got ~s" field value))
    (let ((year (parse-integer text :end 4))
          (month (parse-integer text :start 5 :end 7))
          (date (parse-integer text :start 8 :end 10)))
      (unless (<= 1 month 12)
        (fail "~a ~s has no month ~d: months run 01 to 12" field text month))
      (unless (<= 1 date (month-length month year))
        (fail "~a ~s is not a day of that month" field text))
      (values year month date))))

(defun weekday-of-iso (text)
  "TEXT's weekday index, as a holiday row's date reads it."
  (multiple-value-bind (year month date) (parse-iso-date text "holiday date")
    (weekday-of year month date)))

;;; --- the arguments, read the original's way ---------------------------------------

(defun nonblank (value)
  "VALUE trimmed when it is a non-blank string; else NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun parse-country (value)
  "VALUE as an ISO 3166-1 alpha-2 country code, or a refusal."
  (let ((code (string-upcase (or (nonblank value) ""))))
    (unless (and (= 2 (length code))
                 (every (lambda (character)
                          (and (< (char-code character) 128) (alpha-char-p character)))
                        code))
      (fail "country must be an ISO 3166-1 alpha-2 code like DE, AT, CH, US or GB, got ~s" value))
    code))

(defun parse-subdivision (value country)
  "VALUE as an ISO 3166-2 code of COUNTRY -- a bare NI completes to DE-NI --
or NIL when none is named, or a refusal."
  (let ((code (string-upcase (or (nonblank value) ""))))
    (when (plusp (length code))
      (let ((code (substitute #\- #\_ code)))
        (when (and (not (find #\- code)) (<= (length code) 3))
          (setf code (concatenate 'string country "-" code)))
        (unless (and (<= 4 (length code) 9)
                     (char= (char code 2) #\-)
                     (every (lambda (character)
                              (or (char= character #\-)
                                  (and (< (char-code character) 128) (alphanumericp character))))
                            code)
                     (uiop:string-prefix-p (concatenate 'string country "-") code))
          (fail "subdivision must be an ISO 3166-2 code of ~a like ~a-NI or ~a-BY, got ~s"
                country country country value))
        code))))

(defun section-subdivision (country)
  "The region the section names as the default, when it belongs to COUNTRY."
  (let ((region (or (nonblank (setting :subdivision)) "")))
    (when (uiop:string-prefix-p (concatenate 'string country "-") (string-upcase region))
      (parse-subdivision region country))))

(defun parse-year (value)
  "VALUE as a year, the current one when none is named, or a refusal."
  (let ((year (cond ((null value) (nth-value 5 (get-decoded-time)))
                     ((integerp value) value)
                     ((and (stringp value)
                           (plusp (length (nlk:trimmed value)))
                           (every #'digit-char-p (nlk:trimmed value)))
                      (parse-integer (nlk:trimmed value)))
                     (t (fail "year must be a number like 2026, got ~s" value)))))
    (unless (<= 1900 year 2200)
      (fail "year must be between 1900 and 2200, got ~d" year))
    year))

(defun weekend-words (value)
  "VALUE as the weekday words it names: a string split on commas and blanks,
or an array of them."
  (cond ((stringp value)
         (remove "" (uiop:split-string value :separator '(#\Space #\Tab #\, #\;))
                 :test #'string=))
        ((vectorp value)
         (loop for item across value
               when (and (stringp item) (plusp (length (nlk:trimmed item))))
                 collect (nlk:trimmed item)))
        (t nil)))

(defun weekday-index (word)
  "WORD as a weekday index: a name, its first three letters, or 0-6."
  (let* ((word (string-downcase (nlk:trimmed word)))
         (named (position-if (lambda (name)
                               (or (string= word (string-downcase name))
                                   (string= word (subseq (string-downcase name) 0 3))))
                             +weekday-names+))
         (numbered (and (plusp (length word))
                        (every #'digit-char-p word)
                        (let ((number (parse-integer word)))
                          (and (<= 0 number 6) number)))))
    (or named numbered
        (fail "weekend entries must be weekday names (e.g. Saturday) or 0-6, got ~s" word))))

(defun parse-weekend (value)
  "VALUE as the sorted weekend indexes -- the section's weekend when the call
names none, Saturday and Sunday when the section names none -- or a refusal."
  (let ((words (or (weekend-words value) (weekend-words (setting :weekend)))))
    (if (null words)
        +default-weekend+
        (let ((days (remove-duplicates (mapcar #'weekday-index words))))
          (when (= 7 (length days))
            (fail "weekend cannot be all seven days: every date would be a day off"))
          (sort days #'<)))))

(defun parse-types (value)
  "VALUE as the holiday types that count -- Public alone when none is named --
or a refusal naming the ones Nager.Date publishes."
  (let ((named (weekend-words value)))
    (if (null named)
        +default-types+
        (mapcar (lambda (item)
                  (or (find (string-downcase item) +types+
                            :test (lambda (word type) (string= word (string-downcase type))))
                      (fail "unknown holiday type ~s: use any of ~{~a~^, ~}" item +types+)))
                named))))

(defun types-word (types)
  "TYPES as the words an answer opens with: public, or public/bank."
  (string-downcase (format nil "~{~a~^/~}" types)))

;;; --- one year of holidays, fetched once -------------------------------------------

(defvar *calendar* (make-hash-table :test #'equal)
  "One year's rows per (root, country, year), for the life of the process: the
original's in-process cache, with the root in the key so a section that moves
the root never reads another's rows.")

(defun calendar-key (country year)
  "The cache key of one country's year at the section's root."
  (format nil "~a|~a|~a" (setting :base-url) country year))

(defun display-name (row)
  "ROW's name as an answer reads it: the English one, with the local one
beside it when they differ, as the original's two fields did."
  (let ((name (getf row :name)) (local (getf row :local)))
    (cond ((and (plusp (length name)) (plusp (length local)) (not (string-equal name local)))
           (format nil "~a (~a)" name local))
          ((plusp (length name)) name)
          (t local))))

(defun regions-of (row)
  "ROW's regions, or NIL when it is observed everywhere."
  (let ((regions '()))
    (loop for region across (nlk:json-array row "counties")
          when (and (stringp region) (plusp (length region)))
            do (push region regions))
    (nreverse regions)))

(defun row-of (row)
  "ROW, one holiday as Nager.Date sends it, as the plist this cell runs on."
  (let* ((local (nlk:json-value row :string "localName"))
         (name (nlk:json-value row :string "name"))
         (regions (regions-of row))
         (types '()))
    (multiple-value-bind (global present) (nlk:json-value row :boolean "global")
      (loop for type across (nlk:json-array row "types")
            when (and (stringp type) (plusp (length type)))
              do (push type types))
      (list :date (nlk:json-value row :string "date")
            :name (or name local "")
            :local (or local name "")
            :nationwide (if present (and global t) (null regions))
            :regions regions
            :types (nreverse (or types +default-types+))))))

(defun rows-of (body)
  "BODY's holiday rows as plists, in the order the API sent them."
  (let ((rows '()))
    (loop for row across (nlk:json-array body)
          when (and (hash-table-p row) (nlk:json-value row :string "date"))
            do (push (row-of row) rows))
    (nreverse rows)))

(defun fetch-year (country year)
  "One country's holidays for one year, from Nager.Date => its rows, or a
refusal saying what the API said. An answer with no rows is a refusal, not an
empty calendar: a country it does not cover would otherwise turn every
holiday into a workday."
  (let* ((url (format nil "~a/PublicHolidays/~d/~a" (setting :base-url) year country))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :get url
                    :headers '(("accept" . "application/json"))
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 10 (setting :timeout-seconds))))
      (error (condition)
        (fail "could not reach ~a (~a); check the network and try again"
              (setting :base-url) (nlk:one-line (princ-to-string condition)))))
    (cond ((eql status 404)
           (fail "Nager.Date has no data for country ~a: use an ISO 3166-1 alpha-2 code such as DE, AT, CH, US or GB (https://date.nager.at/Country)"
                 country))
          ((eql status 400)
           (fail "Nager.Date does not cover year ~d: it serves roughly the last and the next few decades" year))
          ((not (and (integerp status) (<= 200 status 299)))
           (fail "Nager.Date answered HTTP ~a for ~a ~d; try again later" status country year)))
    (when (and (stringp answer) (> (length answer) +max-body-chars+))
      (fail "Nager.Date's answer for ~a ~d was unexpectedly large; refusing to parse it" country year))
    (let* ((text (and (stringp answer) (nlk:trimmed answer)))
           (body (and (plusp (length text)) (ignore-errors (nlk:decode-json text))))
           (rows (and body (rows-of body))))
      (cond ((and body (not (vectorp body)))
             (fail "Nager.Date answered ~a ~d with something that is not a list of holidays" country year))
            ((null rows)
             (fail "Nager.Date publishes no holiday data for ~a in ~d (country not covered: https://date.nager.at/Country), so holidays and workdays cannot be computed for it"
                   country year)))
      rows)))

(defun raw-year (country year)
  "COUNTRY's rows for YEAR: the cache's, or a fresh fetch's."
  (let ((key (calendar-key country year)))
    (or (gethash key *calendar*)
        (setf (gethash key *calendar*) (fetch-year country year)))))

(defun holidays (country year subdivision types)
  "YEAR's holidays of TYPES, narrowed to SUBDIVISION when one is named, by
date. Without a subdivision the region's own holidays are still listed."
  (let ((rows '()))
    (dolist (row (raw-year country year))
      (when (and (some (lambda (type) (member type (getf row :types) :test #'string-equal)) types)
                 (or (null subdivision)
                     (getf row :nationwide)
                     (member subdivision (getf row :regions) :test #'string-equal)))
        (push row rows)))
    (sort rows #'string< :key (lambda (row) (getf row :date)))))

(defun merge-off (off country years subdivision types)
  "OFF -- a date -> holiday names table -- with YEARS' days off merged in.
Without a subdivision only nationwide holidays count, since a regional one
does not apply everywhere. => OFF."
  (dolist (year years)
    (dolist (row (holidays country year subdivision types))
      (when (or subdivision (getf row :nationwide))
        (push (display-name row) (gethash (getf row :date) off)))))
  off)

;;; --- the answers, as the model reads them -----------------------------------------

(defun format-holidays (country subdivision year types rows)
  "What holidays_list answers: one line a holiday, and what a subdivision
would add when none was named."
  (with-output-to-string (out)
    ;; The count twice: ~:p reads the argument before it, which here is the type word.
    (format out "~d ~a holiday~p for ~a~@[ (~a)~] in ~d (Nager.Date):"
            (length rows) (types-word types) (length rows) country subdivision year)
    (if (null rows)
        (format out "~%  none: Nager.Date lists no ~a holiday there in ~d"
                (types-word types) year)
        (dolist (row rows)
          (format out "~% ~a ~10a  ~a~@[ [~{~a~^, ~}]~]"
                  (getf row :date)
                  (weekday-name (weekday-of-iso (getf row :date)))
                  (display-name row)
                  (and (not (getf row :nationwide)) (getf row :regions)))))
    (when (and (null subdivision)
               (some (lambda (row) (not (getf row :nationwide))) rows))
      (format out "~%Regional holidays carry the regions that observe them; without a subdivision only nationwide ones count as days off."))))

(defun regional-on (country year iso types)
  "The holidays on ISO that are regional, as lines naming their regions: what a
subdivision would add."
  (loop for row in (holidays country year nil types)
        when (and (string= (getf row :date) iso) (not (getf row :nationwide)))
          collect (format nil "~a~@[ (~{~a~^, ~})~]" (display-name row) (getf row :regions))))

(defun format-day (country subdivision date weekend types)
  "What workday_check answers: the weekday, whether the date is a workday and
why, and what a subdivision would change."
  (multiple-value-bind (year month day) (parse-iso-date date "date")
    (let* ((weekday (weekday-of year month day))
           (iso (iso-date year month day))
           (names (reverse (gethash iso (merge-off (make-hash-table :test #'equal)
                                                  country (list year) subdivision types))))
           (weekend-p (and (member weekday weekend) t))
           (regional (and (null subdivision) (regional-on country year iso types))))
      (with-output-to-string (out)
        (format out "~a is a ~a in ~a~@[ (~a)~]: ~a."
                iso (weekday-name weekday) country subdivision
                (cond ((and weekend-p names)
                       (format nil "not a workday -- the weekend, and ~{~a~^; ~}" names))
                      (weekend-p "not a workday -- the weekend")
                      (names (format nil "not a workday -- ~{~a~^; ~}" names))
                      (t "a workday")))
        (when regional
          (format out "~%Not counted without a subdivision: ~{~a~^; ~}." regional))
        (when (null subdivision)
          (format out "~%No subdivision given: only nationwide holidays were treated as days off."))))))

(defun format-count (country subdivision start end weekend types)
  "What workdays_count answers: the span, the workdays in it, and the holidays
that fell on a weekday."
  (multiple-value-bind (start-year start-month start-date) (parse-iso-date start "start")
    (multiple-value-bind (end-year end-month end-date) (parse-iso-date end "end")
      (let ((first (day-number start-year start-month start-date))
            (last (day-number end-year end-month end-date)))
        (unless (<= first last)
          (fail "end must be on or after start: ~a is after ~a"
                (iso-date end-year end-month end-date)
                (iso-date start-year start-month start-date)))
        (when (> (- last first) (setting :max-span-days))
          (fail "the range is limited to ~d days per call; split it up" (setting :max-span-days)))
        (let ((off (merge-off (make-hash-table :test #'equal) country
                              (loop for year from start-year to end-year collect year)
                              subdivision types))
              (workdays 0)
              (weekend-days 0)
              (skipped '()))
          (loop for number from first to last
                do (multiple-value-bind (year month date) (date-from-day-number number)
                     (let ((iso (iso-date year month date))
                           (weekday (mod (+ number 3) 7)))
                       (cond ((member weekday weekend) (incf weekend-days))
                             ((gethash iso off)
                              (push (format nil "~a (~a) ~{~a~^; ~}" iso (weekday-name weekday)
                                            (reverse (gethash iso off)))
                                    skipped))
                             (t (incf workdays))))))
          (with-output-to-string (out)
            (format out "~a to ~a inclusive: ~d calendar days, ~d workday~:p, ~d weekend day~:p, ~d holiday~:p on a workday."
                    (iso-date start-year start-month start-date)
                    (iso-date end-year end-month end-date)
                    (1+ (- last first)) workdays weekend-days (length skipped))
            (when skipped
              (format out "~%Holidays on workdays:")
              (dolist (line (reverse skipped)) (format out "~% ~a" line)))
            (when (null subdivision)
              (format out "~%No subdivision given: only nationwide holidays were excluded."))))))))

(defun format-add (country subdivision start count weekend types)
  "What workdays_add answers: the date COUNT workdays away, and the holidays
the walk stepped over."
  (multiple-value-bind (start-year start-month start-date) (parse-iso-date start "start")
    (unless (integerp count)
      (fail "workdays must be an integer (negative counts backwards), got ~s" count))
    (when (> (abs count) +max-add-workdays+)
      (fail "workdays is limited to ~d either way" +max-add-workdays+))
    (let ((number (day-number start-year start-month start-date))
          (step (if (minusp count) -1 1))
          (remaining (abs count))
          (off (merge-off (make-hash-table :test #'equal) country (list start-year) subdivision types))
          (loaded (list start-year))
          (skipped '()))
      (loop while (plusp remaining)
            do (incf number step)
               (multiple-value-bind (year month date) (date-from-day-number number)
                 (unless (member year loaded)
                   (merge-off off country (list year) subdivision types)
                   (push year loaded))
                 (let ((iso (iso-date year month date))
                       (weekday (mod (+ number 3) 7)))
                   (cond ((member weekday weekend))
                         ((gethash iso off)
                          (push (format nil "~a (~a) ~{~a~^; ~}" iso (weekday-name weekday)
                                        (reverse (gethash iso off)))
                                skipped))
                         (t (decf remaining))))))
      (multiple-value-bind (result-year result-month result-date) (date-from-day-number number)
        (with-output-to-string (out)
          (format out "~d workday~:p from ~a: ~a (~a). The start day is not counted."
                  count (iso-date start-year start-month start-date)
                  (iso-date result-year result-month result-date)
                  (weekday-name (weekday-of result-year result-month result-date)))
          (when skipped
            (format out "~%Skipped as holidays:")
            (dolist (line (reverse skipped)) (format out "~% ~a" line)))
          (when (null subdivision)
            (format out "~%No subdivision given: only nationwide holidays were skipped.")))))))

;;; --- the verbs the tools and /holidays call ---------------------------------------

(define-verb list-holidays (country &key year subdivision types)
  "COUNTRY's holidays of TYPES for YEAR, narrowed to SUBDIVISION when one is
named, as the lines the model reads."
  (let* ((code (parse-country country))
         (region (or (parse-subdivision subdivision code) (section-subdivision code)))
         (types (parse-types types))
         (year (parse-year year)))
    (format-holidays code region year types (holidays code year region types))))

(define-verb check-day (date country &key subdivision weekend types)
  "Whether DATE is a workday in COUNTRY, and which holiday makes it one."
  (let* ((code (parse-country country))
         (region (or (parse-subdivision subdivision code) (section-subdivision code))))
    (format-day code region date (parse-weekend weekend) (parse-types types))))

(define-verb count-workdays (start end country &key subdivision weekend types)
  "The workdays between START and END inclusive, and the holidays among them."
  (let* ((code (parse-country country))
         (region (or (parse-subdivision subdivision code) (section-subdivision code))))
    (format-count code region start end (parse-weekend weekend) (parse-types types))))

(define-verb add-workdays (start count country &key subdivision weekend types)
  "The date COUNT workdays from START, forward or back, skipping weekends and
the COUNTRY's holidays."
  (let* ((code (parse-country country))
         (region (or (parse-subdivision subdivision code) (section-subdivision code))))
    (format-add code region start count (parse-weekend weekend) (parse-types types))))

;;; --- the two boundaries ------------------------------------------------------------

(nlk:define-contract holidays-list
  (:doc "List a country's public holidays for one year: dates, weekdays, names and the regions that observe them. Data: Nager.Date, which covers most countries and no key is needed.")
  ("country" :text :required t
             :doc "ISO 3166-1 alpha-2 country code, e.g. DE, AT, CH, US, GB")
  ("year" :integer
          :doc "the year, e.g. 2026; unset, the current one")
  ("subdivision" :text
                 :doc "optional ISO 3166-2 region, e.g. DE-NI or US-CA; regional holidays are marked either way, and counted as days off only when one is named")
  ("types" :array :of :string
           :doc "the holiday types that count: Public, Bank, School, Authorities, Optional, Observance; unset, Public alone"))

(nlk:define-contract workday-check
  (:doc "Check whether a date is a working day in a country or region: its weekday, whether it is a weekend, and which public holiday falls on it. Use it instead of guessing from memory.")
  ("date" :text :required t
          :doc "the date, ISO YYYY-MM-DD")
  ("country" :text :required t
             :doc "ISO 3166-1 alpha-2 country code, e.g. DE, AT, CH, US, GB")
  ("subdivision" :text
                 :doc "optional ISO 3166-2 region, e.g. DE-NI or US-CA; without one only nationwide holidays are days off")
  ("weekend" :array :of :string
             :doc "optional weekend days as weekday names or 0-6 (Monday 0); unset, Saturday and Sunday")
  ("types" :array :of :string
           :doc "the holiday types that count: Public, Bank, School, Authorities, Optional, Observance; unset, Public alone"))

(nlk:define-contract workdays-count
  (:doc "Count the working days between two dates, both inclusive, excluding weekends and public holidays, and list the holidays that fell on a weekday. About ten years per call at most.")
  ("start" :text :required t
           :doc "the first day, ISO YYYY-MM-DD, counted")
  ("end" :text :required t
         :doc "the last day, ISO YYYY-MM-DD, counted")
  ("country" :text :required t
             :doc "ISO 3166-1 alpha-2 country code, e.g. DE, AT, CH, US, GB")
  ("subdivision" :text
                 :doc "optional ISO 3166-2 region, e.g. DE-NI or US-CA; without one only nationwide holidays are days off")
  ("weekend" :array :of :string
             :doc "optional weekend days as weekday names or 0-6 (Monday 0); unset, Saturday and Sunday")
  ("types" :array :of :string
           :doc "the holiday types that count: Public, Bank, School, Authorities, Optional, Observance; unset, Public alone"))

(nlk:define-contract workdays-add
  (:doc "The date that is N working days after (or before, if negative) a start date, skipping weekends and public holidays: deadline math. The start day itself is not counted.")
  ("start" :text :required t
           :doc "the start date, ISO YYYY-MM-DD, not counted")
  ("workdays" :integer :required t
              :doc "how many working days to add; negative goes backwards, at most 2000 either way")
  ("country" :text :required t
             :doc "ISO 3166-1 alpha-2 country code, e.g. DE, AT, CH, US, GB")
  ("subdivision" :text
                 :doc "optional ISO 3166-2 region, e.g. DE-NI or US-CA; without one only nationwide holidays are skipped")
  ("weekend" :array :of :string
             :doc "optional weekend days as weekday names or 0-6 (Monday 0); unset, Saturday and Sunday")
  ("types" :array :of :string
           :doc "the holiday types that count: Public, Bank, School, Authorities, Optional, Observance; unset, Public alone"))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (holidays-error (condition)
       (nle:failure (format nil "ERROR: ~a" (holidays-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: holidays: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun holidays-list-tool (call)
  "The holidays_list tool: CALL's country and year through Nager.Date."
  (answering
    (list-holidays (holidays-list-country call)
                   :year (holidays-list-year call)
                   :subdivision (holidays-list-subdivision call)
                   :types (holidays-list-types call))))

(defun workday-check-tool (call)
  "The workday_check tool: whether CALL's date is a workday."
  (answering
    (check-day (workday-check-date call) (workday-check-country call)
               :subdivision (workday-check-subdivision call)
               :weekend (workday-check-weekend call)
               :types (workday-check-types call))))

(defun workdays-count-tool (call)
  "The workdays_count tool: the workdays between CALL's two dates."
  (answering
    (count-workdays (workdays-count-start call) (workdays-count-end call)
                    (workdays-count-country call)
                    :subdivision (workdays-count-subdivision call)
                    :weekend (workdays-count-weekend call)
                    :types (workdays-count-types call))))

(defun workdays-add-tool (call)
  "The workdays_add tool: the date CALL's workdays land on."
  (answering
    (add-workdays (workdays-add-start call) (workdays-add-workdays call)
                  (workdays-add-country call)
                  :subdivision (workdays-add-subdivision call)
                  :weekend (workdays-add-weekend call)
                  :types (workdays-add-types call))))

;;; --- /holidays --------------------------------------------------------------------

(defun status-line ()
  "What /holidays answers with no argument: where the data comes from, which
days are the weekend, and the region every call takes when it names none."
  (format nil "Public holidays from ~a. Weekend: ~{~a~^, ~}~@[; region ~a unless a call names one~]. Type a country code, e.g. /holidays DE, or a country and a year."
          (setting :base-url)
          (mapcar #'weekday-name (parse-weekend (setting :weekend)))
          (nonblank (setting :subdivision))))

(defun run-command (args session-id)
  "/holidays [COUNTRY [YEAR]]: a country's holidays for this year, or the year
named. No argument, what the cell points at and which days it treats as the
weekend."
  (declare (ignore session-id))
  (let* ((country (first (nlk:split-words (or args ""))))
         (year-word (second (nlk:split-words (or args "")))))
    (if (null country)
        (status-line)
        (answering
          (list-holidays country :year (and year-word (parse-year year-word)))))))

;;; --- the section, read back -------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES with the root as one URL fragment: the scheme checked,
the trailing slash trimmed."
  (declare (ignore table))
  (let* ((root (string-right-trim "/" (or (nonblank (getf values :base-url)) "")))
         (plist (copy-list values)))
    (unless (or (uiop:string-prefix-p "https://" root) (uiop:string-prefix-p "http://" root))
      (nlk:config-error "base_url ~s has no http:// or https:// scheme: set it to https://date.nager.at/api/v3, or to the proxy that answers for it" root))
    (setf (getf plist :base-url) root)
    plist))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell holidays
  (:section ("holidays")
    (:guide "nothing is required: Nager.Date answers without a key; subdivision sets the region every call counts (e.g. DE-NI) so regional holidays are days off; weekend names the days off; next step: ask for a country's holidays, e.g. /holidays DE")
    ("base_url" :string :default "https://date.nager.at/api/v3"
                :doc "the Nager.Date API root, for a proxy of your own")
    ("subdivision" :string :default ""
                   :doc "optional ISO 3166-2 region used when a call names none, e.g. DE-NI")
    ("weekend" :string :default "Saturday, Sunday"
               :doc "the weekend days, as names or 0-6 (Monday 0), comma separated")
    ("timeout_seconds" :integer :default 10 :min 1
                       :doc "one API call's ceiling, in seconds")
    ("max_span_days" :integer :default 3660 :min 1
                     :doc "the longest range one workdays_count walks"))
  (:settings #'read-settings)
  (:help :holidays
         "holidays: public holidays and workday math (holidays_list, workday_check, workdays_count, workdays_add)"
         +primer+)
  (:tool "holidays_list" (nlk:contract 'holidays-list) #'holidays-list-tool)
  (:tool "workday_check" (nlk:contract 'workday-check) #'workday-check-tool)
  (:tool "workdays_count" (nlk:contract 'workdays-count) #'workdays-count-tool)
  (:tool "workdays_add" (nlk:contract 'workdays-add) #'workdays-add-tool)
  (:command "holidays" 'run-command
            :description "Public holidays for a country, or a workday's math"
            :argument-hint "COUNTRY [YEAR]"))
