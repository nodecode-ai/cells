;;;; contracts.lisp --- the two tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-hass-statistics)

(nlk:define-contract statistics-list
  (:doc "List the Home Assistant long-term statistics the Recorder holds: one entry per statistic id, with its unit, source and type. Use this to discover the exact statistic id before asking ha_get_statistics for history; a bare call on a large install answers thousands, so narrow it with search (energy, temperature, sensor.office).")
  ("search" :string :default "" :slot search-text
   :doc "case-insensitive text a statistic id must contain (energy, temperature, office)"))

(nlk:define-contract statistics-get
  (:doc "Get Home Assistant long-term historical statistics from the Recorder for one or more statistic ids over a time range, aggregated by period. Discover ids with ha_list_statistics first. Every timestamp needs a timezone offset, and end_time is optional (it defaults to now). For energy use change rather than the cumulative sum: change is the energy used in each interval. A range wide enough to exceed the row budget is refused outright rather than truncated, so ask for fewer ids, a shorter range or a coarser period.")
  ("statistic_ids" :array :of :string :required t
   :doc "the statistic ids to query ([\"sensor.office_temperature\"]), at most 100 distinct ids per call")
  ("start_time" :text :required t
   :doc "ISO-8601 start of the range, timezone offset included: 2026-10-01T00:00:00+02:00")
  ("end_time" :string :default ""
   :doc "optional ISO-8601 end of the range, timezone offset included; later than start_time")
  ("period" :string :default "day" :options '("5minute" "hour" "day" "week" "month" "year")
   :doc "the aggregation period: 5minute, hour, day (the default), week, month or year")
  ("types" :array :of :string
   :doc "the values to return: mean, min, max, state, sum or change. Omit for all six; for interval energy consumption ask for change"))
