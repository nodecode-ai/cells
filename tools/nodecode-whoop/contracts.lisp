;;;; contracts.lisp --- what each tool's call carries, declared once.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The schema the model is shown and the record the body reads come off one
;;;; declaration, so a tool cannot advertise one shape and dispatch another.

(in-package #:nodecode-whoop)

(nlk:define-contract summary-request
  (:doc "The window the daily summary reads: WHOOP records from the last DAYS
 days, newest first, as the recovery, sleep, cycle and workout summaries the
 tool answers with.")
  ("days" :integer :default 7
   :doc "How many days of WHOOP records to read, 1 to 30. The newest scored recovery, sleep and cycle in that window are the summary's headline; default 7."))

(nlk:define-contract trend-request
  (:doc "The window a trend aggregates: scored WHOOP records from the last
 DAYS days, oldest to newest.")
  ("days" :integer :default 30
   :doc "How many days to aggregate into the trend, 2 to 30. Two points are the least a slope can be fitted to; default 30."))

(nlk:define-contract workout-request
  (:doc "Which workouts to list: scored workouts from the last DAYS days,
 newest first, at most LIMIT of them.")
  ("days" :integer :default 14
   :doc "How many days back to look for workouts, 1 to 30; default 14.")
  ("limit" :integer :default 10
   :doc "The most workouts to answer with, 1 to 25; default 10."))
