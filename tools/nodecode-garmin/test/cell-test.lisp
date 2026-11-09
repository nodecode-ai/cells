;;;; cell-test.lisp --- the garmin cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The key is a stubbed ENVIRONMENT-VALUE and Garmin a stubbed HTTP that
;;;; answers by URL fragment, so a test proves the URL, the headers and the
;;;; summary a verb builds. Nothing here reaches the network, the environment
;;;; or the operator's home.

(in-package #:nodecode.test)

(define-test-slice "garmin" "GARMIN-CELL-" :start nodecode-garmin:start-cell)

;;; --- fixtures ----------------------------------------------------------------------

(defmacro with-garmin-key (&body body)
  "BODY with the variable the section names answering a test key, and no token
carried in from another test."
  `(with-stubbed-fdefinition (nodecode-garmin::environment-value (name)
                              (if (equal name "GARMIN_PERSONAL_API_KEY") "test-key" nil))
     (let ((nodecode-garmin::*token* nil))
       ,@body)))

(defvar *asked* '()
  "The stubbed requests of one test, newest first.")

(defmacro with-garmin-wire ((&rest routes) &body body)
  "BODY with the cell's HTTP seam stubbed and the cell started. ROUTES are
FRAGMENT FORM pairs, flat: the first whose text the URL contains answers
FORM's (values BODY STATUS); a token exchange answers a token unless a route
says otherwise."
  `(let ((*asked* '()))
     (with-garmin-key
      (with-stubbed-fdefinition (nodecode-garmin::http (method url &rest args)
        (push (list method url (getf args :headers)) *asked*)
        (cond ,@(loop for (fragment form) on routes by #'cddr
                      collect `((search ,fragment url) ,form))
              ((search "personalapitoken-service" url) (values "token-1" 200))
              (t (error "unexpected request to ~a" url))))
        (with-cell-stop ((garmin-start))
          ,@body)))))

(defun garmin-request-header (request header)
  "HEADER's value in REQUEST -- (METHOD URL HEADERS) -- or NIL."
  (cdr (assoc header (third request) :test #'string-equal)))

(defun garmin-days-json (days)
  "The steps endpoint's own body: DAYS days, newest first."
  (nlk:encode-json-object
   (coerce (loop for index from 0 below days
                 collect (nlk:json-object
                          "calendarDate" (nodecode-garmin::iso-add (nodecode-garmin::today-iso) (- index))
                          "totalSteps" (* 1000 (1+ index))
                          "stepGoal" 8000
                          "totalDistance" 5000))
           'vector)))

(defun garmin-hrv-json (&key (last 20))
  "Twenty-eight nights of HRV at 50 ms, LAST as the newest night's value."
  (nlk:encode-json-object
   (nlk:json-object
    "hrvSummaries"
    (coerce (loop for index from 27 downto 0
                  collect (nlk:json-object
                           "calendarDate" (nodecode-garmin::iso-add (nodecode-garmin::today-iso) (- index))
                           "lastNightAvg" (if (zerop index) last 50)
                           "weeklyAvg" 50
                           "status" "BALANCED"
                           "baseline" (nlk:json-object "balancedLow" 40 "balancedUpper" 60)))
            'vector))))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "garmin"
  (:fixture with-garmin-key)
  (:help :garmin)
  (:command "garmin")
  (:running (is (search "garmin_today" (nle:help :garmin)) "the manual names the tools"))
  (:refused ("region" 5) ("timeout_seconds" 1)))

;;; --- the key -----------------------------------------------------------------------

(deftest garmin-cell-without-a-key-the-cell-refuses-and-says-which-setting ()
  (with-stubbed-fdefinition (nodecode-garmin::environment-value (name)
                              (and (equal name "GARMIN_PERSONAL_API_KEY") nil))
    (let ((condition (handler-case (progn (garmin-start) nil)
                       (nlk:config-refusal (c) (princ-to-string c)))))
      (is (search "GARMIN_PERSONAL_API_KEY" condition) "the refusal names the variable")
      (is (search "personalApiToken" condition) "and the page the key comes from"))))

(deftest garmin-cell-a-server-error-is-the-verb-s-own-line ()
  (with-garmin-wire ("usersummary-service/stats/steps" (values "{}" 500))
    (let ((answer (garmin:steps (nodecode-garmin::decode-garmin-window (nlk:json-object "days" 2)))))
      (is (search "HTTP 500" answer) "a server error is the verb's own line"))))

(deftest garmin-cell-one-token-exchange-then-the-read ()
  (with-garmin-wire ("usersummary-service/stats/steps" (values "[]" 200))
    (garmin:steps (nodecode-garmin::decode-garmin-window (nlk:json-object "date" "2026-01-02")))
    (is (= 2 (length *asked*)) "one token exchange and one read")
    (let ((exchange (garmin-request-header (second *asked*) "authorization"))
          (read (garmin-request-header (first *asked*) "authorization")))
      (is (equal "ApiKey test-key" exchange) "the exchange carries the key as an ApiKey header")
      (is (equal "Bearer token-1" read) "the read carries the token as a bearer"))))

;;; --- the verbs ---------------------------------------------------------------------

(deftest garmin-cell-steps-asks-its-own-window-and-reads-it-back ()
  (with-garmin-wire ("stats/steps" (values (garmin-days-json 3) 200))
    (let ((answer (garmin:steps (nodecode-garmin::decode-garmin-window (nlk:json-object "days" 3)))))
      (is (search "garmin steps" answer))
      (is (search "steps 3000" answer) "the newest day's steps are in the answer")
      (is (search "goal 8000" answer))
      (is (search "5.00" answer) "and the distance in kilometres")
      (let ((url (second (first *asked*))))
        (is (search "stats/steps/daily" url) "the range endpoint, not the single-day one")
        (let ((first-day (nodecode-garmin::iso-add (nodecode-garmin::today-iso) -2)))
          (is (search first-day url) "the path carries the first day of the three-day window"))
        (is (search (nodecode-garmin::today-iso) url) "and ends today")))))

(deftest garmin-cell-trends-flags-a-drop-against-the-own-baseline ()
  (with-garmin-wire ("hrv/daily" (values (garmin-hrv-json :last 20) 200)
                     "dailysummaries" (values "{\"data\":[]}" 200)
                     "sleep/dailySleepSummariesByDate" (values "[]" 200))
    (let ((answer (garmin:trends (nodecode-garmin::decode-garmin-trends (nlk:json-object "days" 28)))))
      (is (search "FLAGS" answer))
      (is (search "overnight HRV" answer))
      (is (search "below baseline" answer) "20 ms against a 50 ms baseline is a drop")
      (is (search "unfavourable" answer) "and unfavourable, HRV being better high"))))

(deftest garmin-cell-a-window-longer-than-a-quarter-refuses ()
  (with-garmin-wire ()
    (let ((answer (garmin:steps (nodecode-garmin::decode-garmin-window (nlk:json-object "days" 120)))))
      (is (search "at most 93 days" answer)))))

(deftest garmin-cell-the-command-says-where-the-key-comes-from ()
  (with-garmin-wire ()
    (is (search "GARMIN_PERSONAL_API_KEY" (garmin:run-command "status" "s1")))
    (is (search "personal.connectapi.garmin.com" (garmin:run-command "" "s1")))
    (is (search "garmin_today" (garmin:run-command "help" "s1")))
    (is (search "not a verb" (garmin:run-command "dance" "s1")))))
