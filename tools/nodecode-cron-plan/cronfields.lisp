;;;; cronfields.lisp --- a five-field cron expression, as the sets it fires on.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Only what "do these two schedules ever fire in the same minute" needs:
;;;; *, lists, ranges, steps and the month and weekday names. Anything else
;;;; refuses, and the caller treats the job's timing as unknown rather than
;;;; guessing.

(in-package #:nodecode-cron-plan)

(defparameter +field-ranges+ '((0 . 59) (0 . 23) (1 . 31) (1 . 12) (0 . 6))
  "The first and last value each of the five fields may name.")

(defparameter +field-names+
  '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12)
    ("sun" . 0) ("mon" . 1) ("tue" . 2) ("wed" . 3) ("thu" . 4) ("fri" . 5) ("sat" . 6))
  "The three-letter month and weekday names a field may spell a value with.")

(defun separate (text separator)
  "TEXT cut at every SEPARATOR, the pieces in order and empty ones kept."
  (loop with start = 0
        for at = (position separator text :start start)
        collect (subseq text start at)
        while at
        do (setf start (1+ at))))

(defun cron-value (text)
  "The value TEXT spells: a decimal number, or a three-letter day or month name."
  (let ((lower (string-downcase text)))
    (cond ((and (plusp (length lower)) (every #'digit-char-p lower))
           (parse-integer lower))
          ((<= 3 (length lower) 9)
           (let ((named (assoc (subseq lower 0 3) +field-names+ :test #'string=)))
             (if named (cdr named) (fail "unknown value ~s" text))))
          (t (fail "unknown value ~s" text)))))

(defun cron-field (text index)
  "The values field TEXT fires on, ascending; INDEX counts from the minutes field.

`*', a list, a range, a step and a name are all it reads. A step is
`BASE/STEP'; a lone value stepped (`5/15') runs from it to the field's end,
as cron does; Sunday may be written 7."
  (let* ((range (nth index +field-ranges+))
         (lo (car range))
         (hi (cdr range))
         (top (if (= index 4) 7 hi))
         (values '()))
    (dolist (part (separate text #\,))
      (let* ((slash (position #\/ part))
             (base (if slash (subseq part 0 slash) part))
             (stepped (and slash (subseq part (1+ slash))))
             (step (if stepped (ignore-errors (parse-integer stepped)) 1)))
        (unless (and (integerp step) (plusp step))
          (fail "bad step in ~s" text))
        (multiple-value-bind (start end)
            (cond ((member base '("*" "?") :test #'string=) (values lo hi))
                  ((position #\- base)
                   (let ((dash (position #\- base)))
                     (values (cron-value (subseq base 0 dash))
                             (cron-value (subseq base (1+ dash))))))
                  (t (let ((value (cron-value base)))
                       (values value (if stepped hi value)))))
          (unless (and (<= lo start top) (<= lo end top) (<= start end))
            (fail "out of range in ~s" text))
          (loop for value from start to end by step
                do (push (if (= index 4) (mod value 7) value) values)))))
    (sort (remove-duplicates values) #'<)))

(defun parse-cron (expression)
  "The five value sets EXPRESSION fires on, minutes first; anything else refuses.

A leading seconds field is not modelled and is dropped, as the six-field
spelling puts it first."
  (let ((fields (remove "" (separate (string-trim '(#\Space #\Tab) expression)
                                      #\Space)
                        :test #'string=)))
    (when (= (length fields) 6) (setf fields (rest fields)))
    (unless (= (length fields) 5)
      (fail "expected 5 fields, got ~d" (length fields)))
    (loop for field in fields
          for index from 0
          collect (cron-field field index))))

(defun cron-overlap-p (first second)
  "True when both parsed schedules can fire in the same minute of some day."
  (every (lambda (a b) (and (intersection a b) t)) first second))

(defun cron-fires-per-day (parsed)
  "Average firings a day over a year; months and days of month weigh in as shares.

When both day-of-month and day-of-week are restricted cron fires on either,
so the larger of the two shares is the day's."
  (destructuring-bind (minutes hours days months weekdays) parsed
    (let ((per-day (* (length minutes) (length hours)))
          (months-share (/ (length months) 12.0))
          (days-share (/ (length days) 31.0))
          (week-share (/ (length weekdays) 7.0)))
      (cond ((and (< (length days) 31) (< (length weekdays) 7))
             (* per-day months-share (max days-share week-share)))
            ((< (length days) 31)
             (* per-day months-share days-share week-share))
            (t (* per-day months-share week-share))))))
