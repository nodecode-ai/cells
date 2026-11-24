;;;; contracts.lisp --- the three tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-health-data)

(nlk:define-contract sync-request
  (:doc "Refresh the local health store from Oura. Answers the days stored and how many records each collection answered. Sync only when the user asks for fresh data or health_query reports the store is stale: a model that syncs on every question spends the ring's rate limit for nothing.")
  ("start_date" :string :default ""
   :doc "first day to sync, YYYY-MM-DD. Omit for the section's own window")
  ("end_date" :string :default ""
   :doc "last day to sync, YYYY-MM-DD, inclusive. Omit for today")
  ("days" :integer :default 0
   :doc "how many days back from end_date to sync when start_date is omitted, e.g. 30; 0 uses the section's days"))

(nlk:define-contract query-request
  (:doc "Read the local health store: daily sleep, readiness, activity, stress and SpO2 rows already synced from Oura, plus logged meals. This never dials a provider, so it answers instantly and works offline. Cite the dates and values it returns; when a row is missing say so rather than guessing.")
  ("query_type" :string :default "recent"
   :options '("recent" "date_range" "stress_days" "correlate" "coverage" "food")
   :doc "recent (the last limit days, in date order), date_range (start through end), stress_days (the limit most stressful days, highest physiological stress first), correlate (two metrics paired across the window, optionally shifted by shift_days), coverage (how much data the store holds and how fresh it is), food (logged meals in the window)")
  ("start" :string :default ""
   :doc "first day of the window, YYYY-MM-DD")
  ("end" :string :default ""
   :doc "last day of the window, YYYY-MM-DD, inclusive")
  ("limit" :integer :default 7
   :doc "how many days recent and stress_days answer")
  ("left" :string :default ""
   :doc "correlate: the first metric, e.g. sleep_score, readiness_score, activity_score, steps, stress_high, recovery_high, resting_heart_rate, average_hrv, sleep_hours, sleep_efficiency, spo2_average, temperature_deviation")
  ("right" :string :default ""
   :doc "correlate: the second metric, from the same list")
  ("shift_days" :integer :default 0
   :doc "correlate: pair each left row with the right row this many days later, e.g. 1 for last night's sleep against today's readiness"))

(nlk:define-contract food-entry
  (:doc "Log one meal or snack in the local store, on the day it was eaten. Log only what the user said or a photo analysis produced: never invent calories. A logged meal without calories is still worth keeping, and later answers should say how many meals had calorie data.")
  ("day" :text :required t
   :doc "the day the food was eaten, YYYY-MM-DD")
  ("description" :text :required t
   :doc "what was eaten, in the user's own words, e.g. two eggs on toast with coffee")
  ("calories" :integer
   :doc "the estimated calories for the meal; omit when there is no estimate")
  ("items" :array :of :string
   :doc "the meal broken into items, when a photo analysis produced them"))
