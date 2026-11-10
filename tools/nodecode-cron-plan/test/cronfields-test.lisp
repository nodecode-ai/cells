;;;; cronfields-test.lisp --- what a schedule fires on, and when two of them collide.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The collision check rests on this file: Monday 09:00 and Tuesday 09:00
;;;; do not collide, a daily job collides with both, ranges, steps and names
;;;; read as cron reads them, and a schedule it cannot parse refuses rather
;;;; than guessing.

(in-package #:nodecode.test)

(defun cron (expression)
  "The parsed sets EXPRESSION fires on."
  (nodecode-cron-plan::parse-cron expression))

(deftest cron-plan-cell-reads-a-schedule-as-cron-does ()
  (is (equal '((0) (9) (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
                24 25 26 27 28 29 30 31)
               (1 2 3 4 5 6 7 8 9 10 11 12) (1))
             (cron "0 9 * * mon")) "a name reads as its number")
  (is (equal '(0 15 30 45 50 51 52 53 54 55) (first (cron "*/15,50-55 9 * * sun"))) "a step and a range")
  (is (equal '(0 1 2 3 4 5 6) (fifth (cron "0 9 * * 0-7"))) "Sunday may be written 7")
  (is (equal '(5 10 15 20 25 30 35 40 45 50 55) (first (cron "5/5 * * * *"))) "a stepped value runs to the field's end")
  (is (equal '(0 30) (first (cron "0,30 9 * * sat"))) "a list is expanded"))

(deftest cron-plan-cell-says-when-two-schedules-collide ()
  (is (not (nodecode-cron-plan::cron-overlap-p (cron "0 9 * * mon")
                                                        (cron "0 9 * * tue"))) "Monday and Tuesday are different minutes")
  (is (nodecode-cron-plan::cron-overlap-p (cron "0 9 * * *")
                                                   (cron "0 9 * * 2")) "a daily job shares Tuesday's minute")
  (is (not (nodecode-cron-plan::cron-overlap-p (cron "0 9 * * 1-5")
                                                        (cron "0 9 * * sat"))) "weekdays do not collide with Saturday")
  (is (= 1.0 (nodecode-cron-plan::cron-fires-per-day (cron "0 9 * * *"))) "a daily job fires once a day")
  (is (= 48.0 (nodecode-cron-plan::cron-fires-per-day (cron "*/30 * * * *"))) "a half-hourly one fires forty-eight times"))

(deftest cron-plan-cell-refuses-a-schedule-it-cannot-read ()
  (is (signals-error nodecode-cron-plan::cron-plan-error (cron "0 9 * *")) "four fields are not a schedule")
  (is (signals-error nodecode-cron-plan::cron-plan-error
                      (cron "0 25 * * *")) "an hour past the day refuses")
  (is (signals-error nodecode-cron-plan::cron-plan-error
                      (cron "0 9 * * someday")) "an unknown name refuses"))
