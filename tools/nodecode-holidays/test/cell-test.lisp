;;;; cell-test.lisp --- the four tools, /holidays, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a member under its floor or a root without a scheme refused), the
;;;; four tools through the seam the model calls them by -- their URL, their
;;;; filters and the answer they build -- and /holidays with a country and
;;;; with none. Every request is a stubbed NLK:HTTP, so nothing reaches
;;;; Nager.Date or the network, and every call names its year, so no answer
;;;; depends on the day the suite runs.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *holidays-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *holidays-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-holidays-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *HOLIDAYS-REQUESTS* and
answered by *HOLIDAYS-ANSWER-FN*, so no test dials Nager.Date."
  `(progn
     ;; The calendar is the cell's own, for the life of the process: one test's
     ;; year must not answer the next one's request.
     (clrhash nodecode-holidays::*calendar*)
     (let ((*holidays-requests* '())
           (*holidays-answer-fn* (lambda (method url args)
                                    (declare (ignore method url args))
                                    (error "unexpected Nager.Date request"))))
       (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                  (push (list* method url args) *holidays-requests*)
                                  (funcall *holidays-answer-fn* method url args))
         ,@body))))

(defun holidays-asked ()
  "The stubbed requests, oldest first."
  (reverse *holidays-requests*))

(defun holidays-answer (text &optional (status 200))
  "The one answer every stubbed request gets."
  (setf *holidays-answer-fn* (lambda (method url args)
                               (declare (ignore method url args))
                               (values text status))))

(defparameter +de-2026+
  (concatenate 'string
               "[{\"date\":\"2026-01-01\",\"localName\":\"Neujahr\",\"name\":\"New Year's Day\","
               "\"global\":true,\"counties\":null,\"types\":[\"Public\"]},"
               "{\"date\":\"2026-01-06\",\"localName\":\"Heilige Drei Koenige\",\"name\":\"Epiphany\","
               "\"global\":false,\"counties\":[\"DE-BY\",\"DE-BW\"],\"types\":[\"Public\"]},"
               "{\"date\":\"2026-05-01\",\"localName\":\"Tag der Arbeit\",\"name\":\"Labour Day\","
               "\"global\":true,\"counties\":null,\"types\":[\"Public\"]},"
               "{\"date\":\"2026-12-25\",\"localName\":\"Weihnachten\",\"name\":\"Christmas Day\","
               "\"global\":true,\"counties\":null,\"types\":[\"Bank\"]}]")
  "Four of Germany's 2026 holidays as Nager.Date sends them: two nationwide,
one of Bavaria and Baden-Wuerttemberg alone, and one that is a bank holiday.")

(defun holidays-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

(defmacro is-answering (form &body checks)
  "Assert that FORM's answer -- a tool call or a command's own text -- carries
IS-CARRYING's CHECKS. The call itself cannot be IS-CARRYING's binding: a cons
reads as (VAR FORM), so it would bind the function's name."
  `(let ((text ,form))
     (is-carrying text ,@checks)
     text))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "holidays" "HOLIDAYS-CELL-")

(defun holidays-config (&rest pairs)
  "A whole shared config with one `holidays' section built from PAIRS."
  (nlk:json-object "holidays" (apply #'nlk:make-json-object pairs)))

(defun holidays-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-holidays:start-cell (apply #'holidays-config pairs)))

(define-cell-lifecycle-tests "holidays"
  (:fixture with-holidays-wire)
  (:help :holidays)
  (:command "holidays")
  (:running (is (nle::find-tool "holidays_list") "holidays_list is registered")
            (is (nle::find-tool "workday_check") "workday_check is registered")
            (is (nle::find-tool "workdays_count") "workdays_count is registered")
            (is (nle::find-tool "workdays_add") "workdays_add is registered")
            (is (equal "https://date.nager.at/api/v3" (nodecode-holidays::setting :base-url)) "the section's default root")
            (is (equal '(5 6) (nodecode-holidays::parse-weekend (nodecode-holidays::setting :weekend))) "Saturday and Sunday by default"))
  (:stopped (is (null (nle::find-tool "holidays_list")) "stop unregisters the tools"))
  (:refused ("timeout_seconds" 0) ("max_span_days" 0) ("base_url" "date.nager.at/api/v3"))
  (:idle nodecode-holidays:holidays-error (nodecode-holidays::list-holidays "DE")))

;;; --- holidays_list ------------------------------------------------------------

(deftest holidays-cell-lists-a-year-with-weekdays-and-regions ()
  (with-holidays-wire
    (with-cell-stop ((holidays-start))
      (holidays-answer +de-2026+)
      (let ((text (holidays-tool "holidays_list" (nlk:json-object "country" "DE" "year" 2026))))
        (is-carrying text
                     ("3 public holidays for DE in 2026 (Nager.Date):")
                     ("2026-01-01 Thursday")
                     ("New Year's Day (Neujahr)")
                     ("2026-01-06 Tuesday")
                     ("Epiphany (Heilige Drei Koenige)")
                     ("[DE-BY, DE-BW]")
                     ("only nationwide ones count as days off")
                     (:absent "Christmas Day")))
      (let ((request (first (holidays-asked))))
        (is (= 1 (length (holidays-asked))) "one request for one year")
        (is (eq :get (first request)) "a GET")
        (is (equal "https://date.nager.at/api/v3/PublicHolidays/2026/DE" (second request)) "the year and the country in the path")
        (is (equal "application/json"
                   (cdr (assoc "accept" (getf (cddr request) :headers) :test #'string-equal))) "it asks for JSON")))))

(deftest holidays-cell-narrows-to-a-region-and-to-the-types-asked-for ()
  (with-holidays-wire
    (with-cell-stop ((holidays-start "subdivision" "DE-BY"))
      (holidays-answer +de-2026+)
      (let ((text (holidays-tool "holidays_list" (nlk:json-object "country" "DE" "year" 2026))))
        (is-carrying text
                     ("3 public holidays for DE (DE-BY) in 2026")
                     ("Epiphany")
                     (:absent "only nationwide ones count as days off")))
      (let ((text (holidays-tool "holidays_list"
                                  (nlk:json-object "country" "DE" "year" 2026
                                                   "types" (vector "Bank")))))
        (is-carrying text
                     ("1 bank holiday for DE (DE-BY) in 2026 (Nager.Date):")
                     ("Christmas Day (Weihnachten)")
                     (:absent "New Year's Day")))
      (is (= 1 (length (holidays-asked))) "the year is fetched once and cached"))))

;;; --- workday_check ------------------------------------------------------------

(deftest holidays-cell-a-regional-holiday-needs-its-subdivision ()
  (with-holidays-wire
    (with-cell-stop ((holidays-start))
      (holidays-answer +de-2026+)
      (is-answering (holidays-tool "workday_check"
                                   (nlk:json-object "date" "2026-01-06" "country" "DE"))
                   ("2026-01-06 is a Tuesday in DE: a workday.")
                   ("Not counted without a subdivision: Epiphany (Heilige Drei Koenige) (DE-BY, DE-BW)")
                   ("No subdivision given: only nationwide holidays were treated as days off."))
      (is-answering (holidays-tool "workday_check"
                                   (nlk:json-object "date" "2026-01-06" "country" "DE"
                                                    "subdivision" "BY"))
                   ("2026-01-06 is a Tuesday in DE (DE-BY): not a workday -- Epiphany (Heilige Drei Koenige)")
                   (:absent "No subdivision given"))
      (is-answering (holidays-tool "workday_check"
                                   (nlk:json-object "date" "2026-01-01" "country" "DE"))
                   ("not a workday -- New Year's Day (Neujahr)"))
      (is-answering (holidays-tool "workday_check"
                                   (nlk:json-object "date" "2026-01-03" "country" "DE"))
                   ("2026-01-03 is a Saturday in DE: not a workday -- the weekend")))))

(deftest holidays-cell-the-weekend-is-yours-to-name ()
  (with-holidays-wire
    (with-cell-stop ((holidays-start "weekend" "Friday, Saturday"))
      (holidays-answer +de-2026+)
      (is-answering (holidays-tool "workday_check"
                                   (nlk:json-object "date" "2026-01-02" "country" "DE"))
                   ("2026-01-02 is a Friday in DE: not a workday -- the weekend"))
      (is-answering (holidays-tool "workday_check"
                                   (nlk:json-object "date" "2026-01-04" "country" "DE"))
                   ("2026-01-04 is a Sunday in DE: a workday.")))))

;;; --- workdays_count ------------------------------------------------------------

(deftest holidays-cell-counts-workdays-and-names-the-holidays-in-the-range ()
  (with-holidays-wire
    (with-cell-stop ((holidays-start))
      (holidays-answer +de-2026+)
      (is-answering (holidays-tool "workdays_count"
                                   (nlk:json-object "start" "2026-01-01" "end" "2026-01-31"
                                                    "country" "DE"))
                   ("2026-01-01 to 2026-01-31 inclusive: 31 calendar days, 21 workdays, 9 weekend days, 1 holiday on a workday.")
                   ("Holidays on workdays:")
                   ("2026-01-01 (Thursday) New Year's Day (Neujahr)")
                   ("No subdivision given: only nationwide holidays were excluded."))
      (is-answering (holidays-tool "workdays_count"
                                   (nlk:json-object "start" "2026-01-31" "end" "2026-01-01"
                                                    "country" "DE"))
                   ("ERROR: end must be on or after start")))))

;;; --- workdays_add -------------------------------------------------------------

(deftest holidays-cell-adds-workdays-and-skips-a-regions-holiday ()
  (with-holidays-wire
    (with-cell-stop ((holidays-start))
      (holidays-answer +de-2026+)
      (is-answering (holidays-tool "workdays_add"
                                   (nlk:json-object "start" "2026-01-01" "workdays" 5
                                                    "country" "DE"))
                   ("5 workdays from 2026-01-01: 2026-01-08 (Thursday).")
                   (:absent "Skipped as holidays"))
      (is-answering (holidays-tool "workdays_add"
                                   (nlk:json-object "start" "2026-01-09" "workdays" -3
                                                    "country" "DE"))
                   ("-3 workdays from 2026-01-09: 2026-01-06 (Tuesday)")))
    (with-holidays-wire
      (with-cell-stop ((holidays-start "subdivision" "DE-BY"))
        (holidays-answer +de-2026+)
        (is-answering (holidays-tool "workdays_add"
                                     (nlk:json-object "start" "2026-01-01" "workdays" 5
                                                      "country" "DE"))
                     ("5 workdays from 2026-01-01: 2026-01-09 (Friday)")
                     ("Skipped as holidays:")
                     ("2026-01-06 (Tuesday) Epiphany"))))))

;;; --- refusals -----------------------------------------------------------------

(deftest holidays-cell-an-uncovered-country-is-a-refusal-not-an-empty-calendar ()
  (with-holidays-wire
    (with-cell-stop ((holidays-start))
      (holidays-answer "" 204)
      (is-answering (holidays-tool "holidays_list" (nlk:json-object "country" "IN" "year" 2026))
                   ("ERROR: Nager.Date publishes no holiday data for IN in 2026"))
      (holidays-answer "{\"message\":\"nope\"}" 404)
      (is-answering (holidays-tool "holidays_list" (nlk:json-object "country" "XX" "year" 2026))
                   ("ERROR: Nager.Date has no data for country XX"))
      (holidays-answer "{\"message\":\"boom\"}" 500)
      (is-answering (holidays-tool "holidays_list" (nlk:json-object "country" "DE" "year" 2026))
                   ("ERROR: Nager.Date answered HTTP 500 for DE 2026"))
      (is-answering (holidays-tool "holidays_list" (nlk:json-object "country" "Germany"))
                   ("ERROR: country must be an ISO 3166-1 alpha-2 code"))
      (is-answering (holidays-tool "workdays_add"
                                   (nlk:json-object "start" "2026-01-01" "workdays" 5000
                                                    "country" "DE"))
                   ("ERROR: workdays is limited to 2000")))))

;;; --- /holidays -----------------------------------------------------------------

(deftest holidays-cell-the-command-answers-with-a-country-or-with-itself ()
  (with-holidays-wire
    (with-cell-stop ((holidays-start))
      (is-answering (cell-entry "nodecode-holidays" "holidays" "")
                   ("Public holidays from https://date.nager.at/api/v3.")
                   ("Weekend: Saturday, Sunday")
                   (:absent "region"))
      (is (null (holidays-asked)) "a status read asks Nager.Date nothing")
      (holidays-answer +de-2026+)
      (is-answering (cell-entry "nodecode-holidays" "holidays" "DE 2026")
                   ("3 public holidays for DE in 2026 (Nager.Date):"))
      (is (equal "https://date.nager.at/api/v3/PublicHolidays/2026/DE"
                 (second (first (holidays-asked)))) "the year the argument names"))))
