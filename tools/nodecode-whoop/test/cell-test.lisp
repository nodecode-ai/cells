;;;; cell-test.lisp --- the whoop cell against a stubbed WHOOP.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every WHOOP endpoint is a stubbed NLK:HTTP — the collection pages, the
;;;; token exchange, the profile — and every secret a stubbed NLE::CREDENTIAL-ENV:
;;;; nothing reaches WHOOP, the environment or the operator's files. Each tool is
;;;; called the way a model's call reaches it, through the real dispatch,
;;;; contract and all, so the schema the provider is shown and the body that runs
;;;; are one shape.

(in-package #:nodecode.test)

(define-test-slice "whoop" "WHOOP-CELL-")

(defun whoop-config (&rest pairs)
  "A whole shared config holding the `whoop' section PAIRS make, with the
client, the two secrets and WHOOP's own URLs defaulted."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "client_id" section "test-client")
    (alexandria:ensure-gethash "client_secret_env" section "WHOOP_CLIENT_SECRET")
    (alexandria:ensure-gethash "refresh_token_env" section "WHOOP_REFRESH_TOKEN")
    (alexandria:ensure-gethash "api_base" section "https://api.whoop.test/developer")
    (alexandria:ensure-gethash "token_url" section "https://whoop.test/oauth/oauth2/token")
    (nlk:json-object "whoop" section)))

(defun whoop-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-whoop:start-cell (apply #'whoop-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-whoop-credentials (&body body)
  "BODY with the two environment names the section uses answering fixed values."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (cond ((equal name "WHOOP_CLIENT_SECRET") "test-secret")
                                    ((equal name "WHOOP_REFRESH_TOKEN") "test-refresh")))
     ,@body))

(defmacro with-whoop-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT HEADERS), oldest
first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)
                                               (getf options :headers))
                                         ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected WHOOP request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +token+
  "{\"access_token\":\"access-1\",\"refresh_token\":\"refresh-2\",\"expires_in\":3600,\"token_type\":\"bearer\"}"
  "What WHOOP's token endpoint answers to a refresh-token grant.")

(defparameter +recoveries+
  (concatenate
   'string
   "{\"records\":["
   "{\"cycle_id\":3,\"sleep_id\":\"s3\",\"created_at\":\"2026-03-03T07:00:00.000Z\",\"score_state\":\"SCORED\","
   "\"score\":{\"recovery_score\":68,\"hrv_rmssd_milli\":74.5,\"resting_heart_rate\":48,\"spo2_percentage\":96.5,\"skin_temp_celsius\":34.2}},"
   "{\"cycle_id\":2,\"sleep_id\":\"s2\",\"created_at\":\"2026-03-02T07:00:00.000Z\",\"score_state\":\"SCORED\","
   "\"score\":{\"recovery_score\":55,\"hrv_rmssd_milli\":62,\"resting_heart_rate\":52}},"
   "{\"cycle_id\":1,\"sleep_id\":\"s1\",\"created_at\":\"2026-03-01T07:00:00.000Z\",\"score_state\":\"SCORED\","
   "\"score\":{\"recovery_score\":41,\"hrv_rmssd_milli\":51,\"resting_heart_rate\":56}},"
   "{\"cycle_id\":0,\"sleep_id\":\"s0\",\"created_at\":\"2026-02-28T07:00:00.000Z\",\"score_state\":\"PENDING_SCORE\"}"
   "],\"next_token\":null}")
  "Three scored recoveries, newest first, and one still pending.")

(defparameter +sleeps+
  (concatenate
   'string
   "{\"records\":["
   "{\"id\":\"s3\",\"start\":\"2026-03-02T23:00:00.000Z\",\"end\":\"2026-03-03T07:00:00.000Z\",\"nap\":false,\"score_state\":\"SCORED\","
   "\"score\":{\"sleep_performance_percentage\":88,\"sleep_efficiency_percentage\":92,\"sleep_consistency_percentage\":80,\"respiratory_rate\":14.1,"
   "\"stage_summary\":{\"total_in_bed_time_milli\":28800000,\"total_awake_time_milli\":1800000,\"disturbance_count\":4}}},"
   "{\"id\":\"s2\",\"start\":\"2026-03-01T22:30:00.000Z\",\"end\":\"2026-03-02T06:30:00.000Z\",\"nap\":false,\"score_state\":\"SCORED\","
   "\"score\":{\"sleep_performance_percentage\":75,\"sleep_efficiency_percentage\":86,"
   "\"stage_summary\":{\"total_in_bed_time_milli\":27000000,\"total_awake_time_milli\":2700000,\"disturbance_count\":6}}},"
   "{\"id\":\"s1\",\"start\":\"2026-03-01T13:00:00.000Z\",\"end\":\"2026-03-01T14:00:00.000Z\",\"nap\":true,\"score_state\":\"SCORED\","
   "\"score\":{\"sleep_performance_percentage\":40}},"
   "{\"id\":\"s0\",\"start\":\"2026-02-28T22:00:00.000Z\",\"end\":\"2026-03-01T05:00:00.000Z\",\"nap\":false,\"score_state\":\"SCORED\","
   "\"score\":{\"sleep_performance_percentage\":62,\"sleep_efficiency_percentage\":78,"
   "\"stage_summary\":{\"total_in_bed_time_milli\":25200000,\"total_awake_time_milli\":3600000,\"disturbance_count\":9}}}"
   "],\"next_token\":null}")
  "Two scored nights, a nap, and an older scored night, newest first.")

(defparameter +cycles+
  (concatenate
   'string
   "{\"records\":["
   "{\"id\":3,\"start\":\"2026-03-03T07:00:00.000Z\",\"score_state\":\"SCORED\",\"score\":{\"strain\":12.4,\"kilojoule\":9800,\"average_heart_rate\":62,\"max_heart_rate\":148}},"
   "{\"id\":2,\"start\":\"2026-03-02T07:00:00.000Z\",\"score_state\":\"SCORED\",\"score\":{\"strain\":9.1,\"kilojoule\":8100,\"average_heart_rate\":58,\"max_heart_rate\":139}}"
   "],\"next_token\":null}")
  "Two scored cycles, newest first.")

(defparameter +workouts+
  (concatenate
   'string
   "{\"records\":["
   "{\"id\":\"w2\",\"start\":\"2026-03-02T17:00:00.000Z\",\"end\":\"2026-03-02T18:00:00.000Z\",\"sport_id\":1,\"score_state\":\"SCORED\","
   "\"score\":{\"strain\":11.2,\"kilojoule\":2600,\"average_heart_rate\":141,\"max_heart_rate\":176,\"distance_meter\":9200.5,\"percent_recorded\":99.1}},"
   "{\"id\":\"w1\",\"start\":\"2026-03-01T17:00:00.000Z\",\"end\":\"2026-03-01T17:40:00.000Z\",\"sport_id\":44,\"score_state\":\"SCORED\","
   "\"score\":{\"strain\":7.4,\"kilojoule\":1400,\"average_heart_rate\":122,\"max_heart_rate\":151}}"
   "],\"next_token\":null}")
  "Two scored workouts, newest first.")

(defparameter +profile+
  "{\"user_id\":424242,\"first_name\":\"Test\",\"last_name\":\"Rider\"}"
  "What GET /v2/user/profile/basic answers.")

(defparameter +body+
  "{\"height_meter\":1.82,\"weight_kilogram\":76.5,\"max_heart_rate\":191}"
  "What GET /v2/user/measurement/body answers.")

(defun approx (expected actual &optional (tolerance 0.01))
  "Whether ACTUAL is EXPECTED within TOLERANCE, whatever float type the wire
read it as: a JSON decimal is a different precision coming back than going out."
  (and (numberp actual) (< (abs (- expected actual)) tolerance)))

(defun whoop-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun whoop-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (whoop-answer id tool arguments)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "whoop"
  (:config "client_id" "test-client")
  (:help :whoop)
  (:fixture with-whoop-credentials)
  (:refused ("client_id" 5) ("timeout" "twenty")))

(deftest whoop-cell-without-a-client-id-refuses (with-whoop-credentials)
  ;; The one setting no verb can work without: a section that names no client
  ;; is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (whoop-start "client_id" ""))))
    (is (search "client_id" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "whoop_daily_summary")) "and installs nothing")))

(deftest whoop-cell-refuses-an-unset-refresh-token-variable (with-whoop-credentials)
  (let ((condition (signals-error nlk:config-refusal
                                  (whoop-start "refresh_token_env" "WHOOP_UNSET_TOKEN"))))
    (is (search "WHOOP_UNSET_TOKEN" (princ-to-string condition))
        "the refusal names the variable that is unset")
    (is (null (nle::find-tool "whoop_daily_summary")))))

;;; --- whoop_daily_summary -------------------------------------------------------------- 

(deftest whoop-cell-answers-a-daily-summary (with-whoop-credentials)
  (with-cell-stop ((whoop-start))
    (with-whoop-wire (asked ("oauth2/token" (values +token+ 200))
                             ("/v2/recovery" (values +recoveries+ 200))
                             ("/v2/activity/sleep" (values +sleeps+ 200))
                             ("/v2/cycle" (values +cycles+ 200))
                             ("/v2/activity/workout" (values +workouts+ 200)))
      (let* ((text (whoop-answer "whoop-1" "whoop_daily_summary" "{\"days\":7}"))
             (object (nlk:decode-json text)))
        (is (= 2 (nlk:json-value object :integer "window" "cycles")) "two scored cycles")
        (is (= 68 (nlk:json-value object :number "latest_recovery" "recovery_score"))
            "the newest scored recovery is the headline")
        (is (= 48 (nlk:json-value object :number "latest_recovery" "resting_heart_rate")))
        (is (= 7.5 (nlk:json-value object :number "latest_sleep" "hours_asleep"))
            "hours asleep is in bed minus awake, from the millisecond stages")
        (is (= 88 (nlk:json-value object :number "latest_sleep" "sleep_performance_percentage"))
            "and the nap in the window is not mistaken for the night")
        (is (approx 12.4 (nlk:json-value object :number "latest_cycle" "strain"))
            "the newest cycle's strain, read at wire precision")
        (is (= 2 (length (nlk:json-array object "workouts"))) "both workouts ride along")
        (is (= 3 (nlk:json-value object :integer "window" "recoveries"))
            "the pending score is not counted as a recovery")
        (is (= 3 (nlk:json-value object :integer "window" "sleeps"))
            "and the nap is left out of the sleep count"))
      (is (= 5 (length asked)) "one token exchange and four collection reads")
      (is (equal :post (first (first asked))) "the first exchange is the token grant")
      (is (search "grant_type=refresh_token" (third (first asked)))
          "and it carries the refresh grant"))))

(deftest whoop-cell-asleep-window-is-clamped (with-whoop-credentials)
  (with-cell-stop ((whoop-start))
    (with-whoop-wire (asked ("oauth2/token" (values +token+ 200))
                             ("/v2/recovery" (values +recoveries+ 200))
                             ("/v2/activity/sleep" (values +sleeps+ 200))
                             ("/v2/cycle" (values +cycles+ 200))
                             ("/v2/activity/workout" (values +workouts+ 200)))
      (let ((object (whoop-object "whoop-2" "whoop_daily_summary" "{\"days\":365}")))
        (is (= 30 (nlk:json-value object :integer "window_days"))
            "a window longer than WHOOP will page is 30 days, not an error"))
      (is (search "start=" (second (second asked))) "the window rides the query string"))))

(deftest whoop-cell-asks-the-token-endpoint-once (with-whoop-credentials)
  (with-cell-stop ((whoop-start))
    (with-whoop-wire (asked ("oauth2/token" (values +token+ 200))
                             ("/v2/recovery" (values +recoveries+ 200))
                             ("/v2/user/profile/basic" (values +profile+ 200))
                             ("/v2/user/measurement/body" (values +body+ 200)))
      (whoop-object "whoop-3" "whoop_recovery_trend" "{\"days\":30}")
      (whoop-object "whoop-3b" "whoop_get_profile" "{}")
      (is (= 1 (count-if (lambda (entry) (search "oauth2/token" (second entry))) asked))
          "the access token is held for the process, not fetched per call")
      (let ((data-call (find-if (lambda (entry) (search "/v2/recovery" (second entry))) asked)))
        (is (equal "Bearer access-1"
                   (cdr (assoc "authorization" (fourth data-call) :test #'equal)))
            "and every data call carries it")))))

(deftest whoop-cell-answers-a-profile (with-whoop-credentials)
  (with-cell-stop ((whoop-start))
    (with-whoop-wire (asked ("oauth2/token" (values +token+ 200))
                             ("/v2/user/profile/basic" (values +profile+ 200))
                             ("/v2/user/measurement/body" (values +body+ 200)))
      (let ((object (whoop-object "whoop-4" "whoop_get_profile" "{}")))
        (is (equal "Test" (nlk:json-value object :string "first_name")))
        (is (= 76.5 (nlk:json-value object :number "weight_kilogram")))
        (is (= 191 (nlk:json-value object :integer "max_heart_rate")))))))

;;; --- whoop_recovery_trend -------------------------------------------------------------

(deftest whoop-cell-fits-a-recovery-trend (with-whoop-credentials)
  (with-cell-stop ((whoop-start))
    (with-whoop-wire (asked ("oauth2/token" (values +token+ 200))
                             ("/v2/recovery" (values +recoveries+ 200)))
      (let ((object (whoop-object "whoop-5" "whoop_recovery_trend" "{\"days\":30}")))
        (is (equal "rising" (nlk:json-value object :string "metrics" "recovery_score" "direction"))
            "41, 55 then 68 is a rising fit")
        (is (= 3 (nlk:json-value object :integer "metrics" "recovery_score" "n_valid")))
        (is (= 54.7 (nlk:json-value object :number "metrics" "recovery_score" "avg"))
            "the average is one decimal")
        (is (equal "falling" (nlk:json-value object :string "metrics" "resting_heart_rate" "direction"))
            "resting heart rate falling is the good direction")
        (is (search "Recovery score averaged"
                    (aref (nlk:json-array object "summary") 0))
            "the summary line reads as a sentence")))))

(deftest whoop-cell-refuses-a-trend-of-one-day (with-whoop-credentials)
  (with-cell-stop ((whoop-start))
    (with-whoop-wire (asked ("oauth2/token" (values +token+ 200))
                             ("/v2/recovery" (values +recoveries+ 200)))
      (let ((object (whoop-object "whoop-6" "whoop_recovery_trend" "{\"days\":1}")))
        (is (= 2 (nlk:json-value object :integer "lookback_days"))
            "two points are the least a slope can be fitted to")))))

;;; --- whoop_sleep_trend ----------------------------------------------------------------

(deftest whoop-cell-fits-a-sleep-trend (with-whoop-credentials)
  (with-cell-stop ((whoop-start))
    (with-whoop-wire (asked ("oauth2/token" (values +token+ 200))
                             ("/v2/activity/sleep" (values +sleeps+ 200)))
      (let ((object (whoop-object "whoop-7" "whoop_sleep_trend" "{\"days\":30}")))
        (is (equal "rising" (nlk:json-value object :string "metrics" "sleep_performance_percentage" "direction")))
        (is (= 3 (nlk:json-value object :integer "count"))
            "the nap is left out of the trend")
        (is (= 6.8 (nlk:json-value object :number "metrics" "sleep_duration_hours" "avg"))
            "6, 6.75 and 7.5 hours asleep average 6.75, read to one decimal")))))

;;; --- whoop_list_workouts ----------------------------------------------------------------

(deftest whoop-cell-lists-workouts-and-caps-the-answer (with-whoop-credentials)
  (with-cell-stop ((whoop-start))
    (with-whoop-wire (asked ("oauth2/token" (values +token+ 200))
                             ("/v2/activity/workout" (values +workouts+ 200)))
      (let ((object (whoop-object "whoop-8" "whoop_list_workouts" "{\"days\":14,\"limit\":1}")))
        (is (= 1 (nlk:json-value object :integer "count")))
        (is (= 2 (nlk:json-value object :integer "total_in_window")))
        (is (eq t (nlk:json-value object :boolean "truncated")))
        (is (search "Showing the newest 1 of 2"
                    (nlk:json-value object :string "truncated_note")))
        (is (= 11.2 (nlk:json-value (aref (nlk:json-array object "workouts") 0)
                                     :number "strain"))
            "the newest workout comes first")))))

(deftest whoop-cell-refuses-a-workout-call-it-cannot-read (with-whoop-credentials)
  ;; A field of the wrong type is refused by the contract, before the body runs
  ;; and before any request leaves.
  (with-cell-stop ((whoop-start))
    (with-whoop-wire (asked)
      (let ((answer (whoop-answer "whoop-9" "whoop_list_workouts" "{\"days\":\"seven\"}")))
        (is (search "preflight" answer) "the contract refuses it")
        (is (search "days" answer))
        (is (null asked) "nothing was asked of WHOOP")))))
