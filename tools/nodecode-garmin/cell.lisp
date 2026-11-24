;;;; cell.lisp --- the section, the fourteen tools, /garmin, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One NLE:DEFINE-CELL declaration: the garmin section, the fourteen
;;;; read-only tools, the /garmin command, the (help :garmin) topic, and a
;;;; start that refuses before anything is registered when no key is reachable.

(in-package #:nodecode-garmin)

(defparameter *primer*
  "Garmin Connect's own data, read-only, through Garmin's Personal API: today's overview, sleep, overnight
HRV, Body Battery, stress, steps, heart rate, activities, weight and hydration, training load, fitness age, and
deterministic baselines over them (garmin_trends). Fourteen tools, all prefixed garmin_.

Which tool: garmin_today for \"how am I\", \"did I sleep well\", \"should I train\"; garmin_trends for anything
about recovery, fatigue or progress over weeks; garmin_sleep or garmin_hrv with detail true for one night;
garmin_activities then garmin_activity (or garmin_activity_fit for the original file) for a workout;
garmin_training_load for how hard the week was; garmin_body for weight and water; garmin_fitness_age for
Garmin's fitness age.

Read it the way a coach would. A night of sleep is filed under the morning the person woke up, so last night
is today. One night says little: before saying someone is fatigued, recovered or improving, call
garmin_trends and quote its flags, which compare the latest value with that person's own baseline and the
7-day average with the 28-day one. HRV is individual (30 ms can be normal for one person and low for
another), so compare it with the balanced range Garmin reports, never with population figures. A single low
night is usually noise; several days of lower HRV with a higher resting heart rate is a common sign of
accumulated fatigue, illness coming on, alcohol or poor sleep. Body Battery at wake shows how well sleep
recharged. Garmin's stress score is physiological, from heart-rate variability while awake, not emotional
stress, and exercise is excluded. Heart-rate zones come from the person's own Garmin settings: quote the bpm
the tools give. Data appears after the watch syncs, so early in the morning today's values may be missing or
running totals; say \"as of\" the last sync rather than calling someone inactive.

This is not medical advice and Garmin's metrics are estimates. If the data shows something that could be a
health issue (a very high or very low resting heart rate, repeated low SpO2 in sleep, a large change that
persists), say so plainly and suggest a doctor, without alarming. Do not suggest training through fever,
chest pain or unusual shortness of breath.

A rejected key (HTTP 401): make a new Personal API key at the key page, paste it in this cell's api_key
setting, then (restart-cells \"nodecode-garmin\"). /garmin status says where the key comes from."
  "What (help :garmin) answers while the cell runs.")

;;; --- the boundaries ----------------------------------------------------------------

(nlk:define-contract garmin-day
  (:doc "One day of Garmin data: today's overview, or the latest fitness age as of that day.")
  ("date" :text :default "" :doc "A day, YYYY-MM-DD, or today or yesterday. Default today."))

(nlk:define-contract garmin-window
  (:doc "A range of days, or one day: either a date, or a start and an end, or the last N days.")
  ("date"         :text :default "" :doc "One day, YYYY-MM-DD, or today or yesterday.")
  ("start_date"   :text :default "" :doc "First day of a range (YYYY-MM-DD).")
  ("end_date"     :text :default "" :doc "Last day of a range (YYYY-MM-DD), inclusive. Default today.")
  ("days"         :integer :doc "Instead of dates: the last N days ending today.")
  ("detail"       :boolean :default nil :doc "Sleep and HRV only: read one night in detail, the last day of the range."))

(nlk:define-contract garmin-hr
  (:doc "One day's heart rate: resting, minimum and maximum, and the 7-day resting average.")
  ("date"   :text :default "" :doc "The day, YYYY-MM-DD, or today or yesterday. Default today.")
  ("hourly" :boolean :default nil :doc "Also the mean beats per minute of the day's readings."))

(nlk:define-contract garmin-activities
  (:doc "Recorded activities, newest first, filtered by dates, type or a word in the name.")
  ("start_date"    :text :default "" :doc "First day of the range.")
  ("end_date"      :text :default "" :doc "Last day of the range, inclusive. Default today.")
  ("days"          :integer :doc "Instead of dates: the last N days. Default 30.")
  ("activity_type" :text :default "" :doc "A Garmin type key: running, cycling, swimming, walking, hiking, fitness_equipment (gym), strength_training, indoor_cycling, treadmill_running.")
  ("search"        :text :default "" :slot search-text :doc "Text in the activity's name.")
  ("limit"         :integer :doc "How many to list, at most 100. Default 10."))

(nlk:define-contract garmin-activity
  (:doc "One activity in full: pace, heart rate, cadence, power, training effect and load, time in each heart-rate zone.")
  ("activity_id" :integer :required t :doc "The Garmin activity id, from garmin_activities."))

(nlk:define-contract garmin-activity-fit
  (:doc "One activity's original file (a ZIP, usually a .fit), saved under the home's nodecode folder, and the path to it.")
  ("activity_id" :integer :required t :doc "The Garmin activity id, from garmin_activities."))

(nlk:define-contract garmin-trends
  (:doc "Deterministic baselines over the last N days: mean, 7-day against 28-day, the z-score of the latest value against the person's own baseline, and plain-language flags. Call this before drawing conclusions about recovery, fatigue or progress.")
  ("days"          :integer :default 28 :doc "The window, 14 to 120 days.")
  ("threshold_pct" :number :default 10.0 :doc "The smallest change worth flagging, in percent.")
  ("as_of"         :text :default "" :doc "Analyse as of this day instead of today."))

(nlk:define-contract garmin-load
  (:doc "Training intensity by heart-rate zone over the last N days: minutes per zone, the share of easy, moderate and hard time, the same per activity type, and intensity minutes against the weekly goal.")
  ("days"  :integer :default 7 :doc "The window, 1 to 28 days.")
  ("as_of" :text :default "" :doc "Last day of the window instead of today."))

;;; --- the cell ----------------------------------------------------------------------

(nle:define-cell garmin
  (:section ("garmin")
    (:guide "make a Garmin Personal API key in Garmin Connect on the web, Settings > Personal API Key, and paste it in api_key (or set the variable api_key_env names); region picks Garmin's global or China server; the timeouts are already right")
    ("api_key" :secret :doc "Garmin Personal API key, pasted here. Garmin shows it once and it expires after the 30, 90, 180 or 365 days you pick.")
    ("api_key_env" :env :default "GARMIN_PERSONAL_API_KEY" :doc "The variable holding the key, for a key already in the environment. api_key wins when both are set.")
    ("region" :choice :options '("global" "china") :default "global" :doc "global = personal.connectapi.garmin.com; china = personal.connectapi.garmin.cn (Garmin China accounts)")
    ("timeout_seconds" :integer :default 30 :min 5 :doc "One request's ceiling, in seconds"))
  (:tool "garmin_today" (nlk:contract 'garmin-day) #'today)
  (:tool "garmin_sleep" (nlk:contract 'garmin-window) #'sleep)
  (:tool "garmin_hrv" (nlk:contract 'garmin-window) #'hrv)
  (:tool "garmin_body_battery" (nlk:contract 'garmin-window) #'body-battery)
  (:tool "garmin_stress" (nlk:contract 'garmin-window) #'stress)
  (:tool "garmin_steps" (nlk:contract 'garmin-window) #'steps)
  (:tool "garmin_heart_rate" (nlk:contract 'garmin-hr) #'heart-rate)
  (:tool "garmin_activities" (nlk:contract 'garmin-activities) #'activities)
  (:tool "garmin_activity" (nlk:contract 'garmin-activity) #'activity)
  (:tool "garmin_activity_fit" (nlk:contract 'garmin-activity-fit) #'activity-fit)
  (:tool "garmin_body" (nlk:contract 'garmin-window) #'body)
  (:tool "garmin_trends" (nlk:contract 'garmin-trends) #'trends)
  (:tool "garmin_training_load" (nlk:contract 'garmin-load) #'training-load)
  (:tool "garmin_fitness_age" (nlk:contract 'garmin-day) #'fitness-age)
  (:command "garmin" #'run-command
            :description "Garmin Connect: key status, today's overview, or the tool list"
            :argument-hint "status|today|help")
  (:help :garmin "garmin: the person's own Garmin Connect data - sleep, HRV, Body Battery, stress, steps, heart rate, activities, training load, fitness age, and trend flags over them" *primer*)
  (:start (lambda ()
            (check-config))))
