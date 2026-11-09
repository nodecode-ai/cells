;;;; cell-test.lisp --- the health-data cell against a stubbed Oura.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Oura endpoint is a stubbed NLK:HTTP and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches the API, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it —
;;;; through the real dispatch, contract and all — so the schema the provider
;;;; is shown and the body that runs are one shape. The store lives in a
;;;; scratch directory per test, under the run's own temporary directory.

(in-package #:nodecode.test)

(define-test-slice "health-data" "HEALTH-DATA-CELL-")

;;; --- the section the cell reads ------------------------------------------------

(defun health-data-config (&rest pairs)
  "A whole shared config holding the `healthdata' section PAIRS make, with a
fresh token file defaulted: the section's key, never the folder's name."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "oura_token_file" section (health-token-file))
    (nlk:json-object "healthdata" section)))

(defun health-data-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-health-data:start-cell (apply #'health-data-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defun health-scratch-dir ()
  "A fresh store directory for one test."
  (namestring (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "health-data-~a-~a/"
                                         (get-universal-time) (random 1000000))
                                (uiop:temporary-directory)))))

(defun health-token-file ()
  "A file holding a token, written fresh for one test."
  (let ((path (merge-pathnames "token" (health-scratch-dir))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string "test-token" out))
    (namestring path)))

(defun oura-endpoint (url)
  "The fixture body URL answers with: the collection its path names."
  (cond ((search "/v2/usercollection/daily_sleep" url) +daily-sleep+)
        ((search "/v2/usercollection/daily_readiness" url) +daily-readiness+)
        ((search "/v2/usercollection/daily_activity" url) +daily-activity+)
        ((search "/v2/usercollection/daily_stress" url) +daily-stress+)
        ((search "/v2/usercollection/daily_spo2" url) +daily-spo2+)
        ((search "/v2/usercollection/sleep" url) +long-sleep+)
        (t (error "unexpected Oura request to ~a" url))))

(defmacro with-oura-wire ((asked) &body body)
  "BODY with NLK:HTTP answering the fixture by URL. ASKED collects
(METHOD URL HEADERS), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :headers)) ,log)
                                   (values (oura-endpoint url) 200))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +day-one+ "2026-01-05")
(defparameter +day-two+ "2026-01-06")

(defparameter +daily-sleep+
  (format nil "{\"data\":[{\"day\":\"~a\",\"score\":81},{\"day\":\"~a\",\"score\":74}]}"
          +day-one+ +day-two+))

(defparameter +daily-readiness+
  (format nil "{\"data\":[{\"day\":\"~a\",\"score\":70,\"temperature_deviation\":0.1,\"contributors\":{\"resting_heart_rate\":52}},{\"day\":\"~a\",\"score\":66,\"temperature_deviation\":0.3,\"contributors\":{\"resting_heart_rate\":55}}]}"
          +day-one+ +day-two+))

(defparameter +daily-activity+
  (format nil "{\"data\":[{\"day\":\"~a\",\"score\":88,\"steps\":7200,\"active_calories\":340},{\"day\":\"~a\",\"score\":61,\"steps\":3100,\"active_calories\":210}]}"
          +day-one+ +day-two+))

(defparameter +daily-stress+
  (format nil "{\"data\":[{\"day\":\"~a\",\"stress_high\":5400,\"recovery_high\":7200,\"day_summary\":\"normal\"},{\"day\":\"~a\",\"stress_high\":12600,\"recovery_high\":3600,\"day_summary\":\"stressful\"}]}"
          +day-one+ +day-two+))

(defparameter +daily-spo2+
  (format nil "{\"data\":[{\"day\":\"~a\",\"spo2_percentage\":{\"average\":96.5}},{\"day\":\"~a\",\"spo2_percentage\":{\"average\":95.2}}]}"
          +day-one+ +day-two+))

(defparameter +long-sleep+
  (format nil "{\"data\":[{\"day\":\"~a\",\"type\":\"long_sleep\",\"total_sleep_duration\":25200,\"efficiency\":88,\"average_hrv\":42,\"lowest_heart_rate\":49,\"bedtime_start\":\"2026-01-04T23:10:00-08:00\"},{\"day\":\"~a\",\"type\":\"long_sleep\",\"total_sleep_duration\":21600,\"efficiency\":81,\"average_hrv\":38,\"lowest_heart_rate\":52,\"bedtime_start\":\"2026-01-05T23:40:00-08:00\"}]}"
          +day-one+ +day-two+))

(defun standard-oura (&optional (tail '()))
  "The routes a healthy sync asks for, in the order the cell asks."
  (append (list (list "/v2/usercollection/daily_sleep" `(values ,+daily-sleep+ 200))
                (list "/v2/usercollection/daily_readiness" `(values ,+daily-readiness+ 200))
                (list "/v2/usercollection/daily_activity" `(values ,+daily-activity+ 200))
                (list "/v2/usercollection/daily_stress" `(values ,+daily-stress+ 200))
                (list "/v2/usercollection/daily_spo2" `(values ,+daily-spo2+ 200))
                (list "/v2/usercollection/sleep" `(values ,+long-sleep+ 200)))
          tail))

(defun hd-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun hd-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (hd-answer id tool arguments)))

(defmacro with-store ((&rest pairs) &body body)
  "BODY with the cell started on a scratch store, and PAIRS over the section."
  `(with-cell-stop ((health-data-start "data_dir" (health-scratch-dir) ,@pairs))
     ,@body))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "health-data"
  (:config "oura_token_file" (health-token-file))
  (:help :health-data)
  (:command "health")
  (:refused ("oura_token_env" "" "oura_token_file" "")
            ("oura_token_env" "OURA_UNSET" "oura_token_file" "")))

(deftest health-data-cell-without-a-token-refuses ()
  ;; The one setting no verb can work without: the section names a token, and a
  ;; section that names none is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal
                                  (health-data-start "oura_token_env" "" "oura_token_file" ""))))
    (is (search "no Oura token" (princ-to-string condition)) "the refusal says what is missing")
    (is (search "oura_token_file" (princ-to-string condition)) "and names both ways to give it")
    (is (null (nle::find-tool "health_query")) "and installs nothing")))

;;; --- health_sync ---------------------------------------------------------------------

(deftest health-data-cell-syncs-oura ()
  (with-store ()
    (with-oura-wire (asked)
      (progn
        (let ((object (hd-object "hd-sync-1" "health_sync"
                                 (format nil "{\"start_date\":\"~a\",\"end_date\":\"~a\"}" +day-one+ +day-two+))))
          (is (= 6 (length asked)) "one request per collection")
          (is (equal :get (first (first asked))))
          (is (search "start_date=2026-01-05" (second (first asked))) "the window rides the query string")
          (is (equal "Bearer test-token" (cdr (assoc "authorization" (third (first asked)) :test #'equal)))
              "the token is the section's, as a bearer header")
          (is (eq t (nlk:json-value object :boolean "ok")))
          (is (= 2 (nlk:json-value object :integer "days_synced")))
          (is (= 2 (nlk:json-value object :integer "collections" "daily_sleep")) "each collection reports what it answered"))))))

(deftest health-data-cell-explains-a-failed-sync ()
  (with-store ()
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method options))
                                (if (search "daily_sleep" url)
                                    (values "{\"detail\":\"token expired\"}" 401)
                                    (values "{\"data\":[]}" 200)))
      (let ((answer (hd-answer "hd-sync-2" "health_sync" "{\"days\":2}")))
        (is (search "ERROR" answer))
        (is (search "401" answer) "the refusal carries Oura's status")
        (is (search "token expired" answer) "and Oura's own words")))))

;;; --- health_query ----------------------------------------------------------------------

(defmacro with-synced-store ((&rest pairs) &body body)
  "BODY with the cell started on a scratch store and one sync already in it."
  `(with-store ,pairs
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                 (declare (ignore method options))
                                 (cond ((search "daily_sleep" url) (values +daily-sleep+ 200))
                                       ((search "daily_readiness" url) (values +daily-readiness+ 200))
                                       ((search "daily_activity" url) (values +daily-activity+ 200))
                                       ((search "daily_stress" url) (values +daily-stress+ 200))
                                       ((search "daily_spo2" url) (values +daily-spo2+ 200))
                                       ((search "collection/sleep" url) (values +long-sleep+ 200))
                                       (t (values "{\"data\":[]}" 200))))
       (hd-answer "hd-seed" "health_sync" "{\"days\":2}")
       ,@body)))

(deftest health-data-cell-answers-recent ()
  (with-synced-store ()
    (let* ((object (hd-object "hd-q-1" "health_query" "{\"query_type\":\"recent\",\"limit\":7}"))
           (days (nlk:json-array object "days")))
      (is (= 2 (nlk:json-value object :integer "count")))
      (is (equal +day-one+ (nlk:json-value (aref days 0) :string "day")) "oldest first")
      (is (= 81 (nlk:json-value (aref days 0) :integer "sleep_score")))
      (is (= 55 (nlk:json-value (aref days 1) :integer "resting_heart_rate"))))
  (with-synced-store ()
    (let ((row (aref (nlk:json-array (hd-object "hd-q-2" "health_query"
                                                 "{\"query_type\":\"date_range\",\"start\":\"2026-01-06\",\"end\":\"2026-01-06\"}")
                                      "days") 0)))
      (is (= 21600 (nlk:json-value row :integer "sleep_duration_seconds")) "the night's own seconds")
      (is (equal "stressful" (nlk:json-value row :string "stress_summary")))))))

(deftest health-data-cell-orders-stress-days ()
  (with-synced-store ()
    (let* ((object (hd-object "hd-q-3" "health_query" "{\"query_type\":\"stress_days\",\"limit\":1}"))
           (days (nlk:json-array object "days")))
      (is (= 1 (length days)))
      (is (equal +day-two+ (nlk:json-value (aref days 0) :string "day")) "the highest stress_high first")
      (is (= 12600 (nlk:json-value (aref days 0) :integer "stress_high"))))))

(deftest health-data-cell-reports-coverage ()
  (with-synced-store ()
    (let ((object (hd-object "hd-q-4" "health_query" "{\"query_type\":\"coverage\"}")))
      (is (= 2 (nlk:json-value object :integer "days")))
      (is (equal +day-two+ (nlk:json-value object :string "last_day")))
      (is (= 2 (nlk:json-value object :integer "fields" "sleep_score" "days")) "coverage counts each metric")
      (is (eq nil (nlk:json-value object :boolean "stale")) "a store written just now is fresh"))))

(deftest health-data-cell-correlates-with-a-shift ()
  (with-synced-store ()
    (let ((object (hd-object "hd-q-5" "health_query"
                              "{\"query_type\":\"correlate\",\"left\":\"sleep_score\",\"right\":\"readiness_score\",\"shift_days\":1}")))
      (is (= 1 (nlk:json-value object :integer "count")) "one pair, shifted by a day")
      (is (= 81 (nlk:json-value (aref (nlk:json-array object "pairs") 0) :integer "left_value"))
          "last night's sleep against today's readiness"))
    (let ((object (hd-object "hd-q-6" "health_query"
                              "{\"query_type\":\"correlate\",\"left\":\"sleep_hours\",\"right\":\"average_hrv\",\"shift_days\":0}")))
      (is (= 2 (nlk:json-value object :integer "count")) "sleep_hours is derived from the night's seconds"))))

(deftest health-data-cell-refuses-an-unknown-metric ()
  (with-synced-store ()
    (let ((answer (hd-answer "hd-q-7" "health_query"
                              "{\"query_type\":\"correlate\",\"left\":\"mood\",\"right\":\"sleep_score\"}")))
      (is (search "unknown metric" answer))
      (is (search "sleep_score" answer) "the refusal lists the metrics that exist"))))

;;; --- health_log_food -----------------------------------------------------------------

(deftest health-data-cell-logs-food ()
  (with-store ()
    (let ((first (hd-object "hd-f-1" "health_log_food"
                             (format nil "{\"day\":\"~a\",\"description\":\"two eggs on toast\",\"calories\":420}" +day-one+)))
          (second (hd-object "hd-f-2" "health_log_food"
                              (format nil "{\"day\":\"~a\",\"description\":\"coffee\"}" +day-one+))))
      (is (eq t (nlk:json-value first :boolean "ok")))
      (is (= 1 (nlk:json-value first :integer "meals_today")))
      (is (= 2 (nlk:json-value second :integer "meals_today")) "a second meal joins the first")
      (is (= 1 (nlk:json-value second :integer "meals_with_calories_today"))
          "a meal without calories is kept, and counted apart")
      (is (= 420 (nlk:json-value second :integer "total_calories_today"))))
    (let ((object (hd-object "hd-f-3" "health_query"
                              (format nil "{\"query_type\":\"food\",\"start\":\"~a\",\"end\":\"~a\"}" +day-one+ +day-one+))))
      (is (= 2 (nlk:json-value object :integer "count")))
      (is (= 420 (nlk:json-value object :integer "total_calories")))
      (is (equal "coffee" (nlk:json-value (aref (nlk:json-array object "entries") 1) :string "description"))))))

(deftest health-data-cell-refuses-a-bad-day ()
  (with-store ()
    (let ((answer (hd-answer "hd-f-4" "health_log_food"
                              "{\"day\":\"yesterday\",\"description\":\"soup\"}")))
      (is (search "not a day" answer))
      (is (search "YYYY-MM-DD" answer) "the refusal says the shape it wants"))))

;;; --- the /health command ----------------------------------------------------------------

(deftest health-data-cell-command-reports-the-store ()
  (with-synced-store ()
    (let ((text (nodecode-health-data::health-command "status" nil)))
      (is (search "2 days stored" text))
      (is (search "sleep 74" text) "the last night's scores, in a line a person reads"))))

(deftest health-data-cell-command-on-an-empty-store ()
  (with-store ()
    (let ((text (nodecode-health-data::health-command "" nil)))
      (is (search "No health data yet" text) "an empty store says how to fill it")
      (is (search "/health sync" text) "and names the command that does"))))
