;;;; contracts.lisp --- the four tools that take arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.
;;;;
;;;; The three tools that take no argument - connection status, data
;;;; inventory, privacy audit - register against a hand-written empty object,
;;;; because a declaration with no members types nothing.

(in-package #:nodecode-apple-health)

(nlk:define-contract daily-request
  (:doc "One day of Apple Health numbers from the local export: steps, distance, active energy, heart rate, resting heart rate, heart-rate variability, sleep hours and the day's workouts. Leave date out for the most recent day the export covers.")
  ("date" :string :default ""
   :doc "the day to summarize, YYYY-MM-DD; omit it for the latest day the export covers"))

(nlk:define-contract weekly-request
  (:doc "A run of consecutive days of Apple Health numbers from the local export, one row per day plus the totals over the whole window. Leave the arguments out for the last seven days the export covers.")
  ("end_date" :string :default ""
   :doc "the last day of the window, YYYY-MM-DD; omit it for the latest day the export covers")
  ("days" :integer :default 7
   :doc "how many days the window holds, ending at end_date (1 to 90, 7 by default)"))

(nlk:define-contract record-query
  (:doc "List individual Apple Health records from the local export, bounded. Filter by record type (a substring such as StepCount, HeartRate or SleepAnalysis) and by first and last day. The answer says how many records matched in all, and whether the list was cut.")
  ("type" :string :default "" :slot record-type
   :doc "record type, or a part of one: StepCount, HeartRate, SleepAnalysis, ActiveEnergyBurned. Omit for every type")
  ("start_date" :string :default ""
   :doc "first day to include, YYYY-MM-DD")
  ("end_date" :string :default ""
   :doc "last day to include, YYYY-MM-DD")
  ("limit" :integer :default 0
   :doc "how many records to answer for this one call; 0 or absent uses the operator's max_records"))

(nlk:define-contract workout-query
  (:doc "List the workouts in the local export, bounded: activity, minutes, distance and energy burned, newest first, optionally between two days. The answer says how many workouts matched in all, and whether the list was cut.")
  ("start_date" :string :default ""
   :doc "first day to include, YYYY-MM-DD")
  ("end_date" :string :default ""
   :doc "last day to include, YYYY-MM-DD")
  ("limit" :integer :default 0
   :doc "how many workouts to answer for this one call; 0 or absent uses the operator's max_records"))
