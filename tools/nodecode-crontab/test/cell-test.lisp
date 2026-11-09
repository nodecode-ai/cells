;;;; cell-test.lisp --- the reading, the tool, the command and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Offline: the only files these tests touch are zone files of their own,
;;;; written byte by byte in a scratch folder of the home -- a tz database with
;;;; one fixed offset, and one that changes offset mid-week -- and the clock is
;;;; pinned with *NOW* wherever an instant's reading is asserted. What is
;;;; pinned is what a user gets: the sentence, the next runs, the notes, the
;;;; tool through its declared contract, /crontab, and the refusals.

(in-package #:nodecode.test)

(define-test-slice "crontab" "CRONTAB-" :start nodecode-crontab:start-cell)

(define-cell-lifecycle-tests "crontab"
  (:config)
  (:command "crontab")
  (:help :crontab)
  (:running
   (let ((text (nodecode-crontab::reading-for "0 9 * * 1-5" :source "the test's own" :now +afternoon+)))
     (is (search "At 09:00 on Monday through Friday." text) "the sentence says what it fires")
     (is (search "Next 5 runs" text) "and the runs follow it")))
  (:refused ("timezone" "No/Such_Zone")
            ("timezone" "../etc/passwd")
            ("timezone" "Europe/Berlin/")
            ("timezone" "Europe/Berlin and more")
            ("runs" 0)
            ("runs" 99))
  (:idle nodecode-crontab:crontab-error
         (nodecode-crontab::reading-for "0 9 * * 1-5"  :source "the test's own")))

;;; --- a tz database of this test's own ----------------------------------------------

(defun crontab-tzif (&key (transitions '()) (types '((0 . nil))) (abbrs "UTC\0"))
  "A version 2 TZif file: TRANSITIONS (universal times), and ((OFFSET . DST) ...)
per type, each transition switching to the next type in turn. Version 2 is
what the tz database ships -- the 4-byte block for old readers, then the
8-byte one that holds times past 2038."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((put (value size)
             (dotimes (index size)
               (vector-push-extend (ldb (byte 8 (* 8 (- size 1 index))) value) out)))
           (text (string)
             (loop for char across string do (vector-push-extend (char-code char) out))))
      (labels ((header ()
                 (text "TZif")
                 (vector-push-extend 50 out)           ; "2"
                 (dotimes (index 15) (vector-push-extend 0 out))
                 (put 0 4)                             ; UT/local indicators
                 (put 0 4)                             ; standard/wall indicators
                 (put 0 4)                             ; leap seconds
                 (put (length transitions) 4)
                 (put (length types) 4)
                 (put (length abbrs) 4))
               (segment (time-size)
                 (dolist (transition transitions) (put transition time-size))
                 (loop for index from 0 below (length transitions)
                       do (vector-push-extend (mod (1+ index) (length types)) out))
                 (loop for (offset . daylight) in types
                       for index from 0
                       do (put offset 4)
                          (vector-push-extend (if daylight 1 0) out)
                          (vector-push-extend (* 4 index) out))
                 (text abbrs)))
        (header) (segment 4)
        (header) (segment 8)
        (text (format nil "~%~a~%" abbrs)))
      (coerce out '(vector (unsigned-byte 8))))))

(defun crontab-zone-dir (&rest keys)
  "A tz database of this test's own, holding Test/Zone, holding the zone KEYS describes."
  (let* ((dir (merge-pathnames (format nil "crontab-zones-~12,'0x/" (random (expt 16 10)))
                                (nlk:home)))
         (path (merge-pathnames "Test/Zone" dir)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence (apply #'crontab-tzif keys) out))
    (namestring dir)))

(defmacro with-zone ((dir &rest keys) &body body)
  "BODY with DIR a tz database of the test's own, removed after."
  `(let ((,dir (crontab-zone-dir ,@keys)))
     (nlk:with-cleanup ((uiop:delete-directory-tree (pathname ,dir) :validate t
                                                                :if-does-not-exist :ignore))
       ,@body)))

(defparameter +half-past-five+ '((19800 . nil))
  "One fixed type thirty minutes east of the hour: a zone with no transition.")

(defparameter +afternoon+ (encode-universal-time 0 30 8 8 11 2026 0)
  "Sunday 2026-11-08 08:30 UTC, which is 14:00 where the five-hour zone stands.")

(defun crontab-call (&rest pairs)
  "The explain_cron tool as the engine calls it: the declared contract
decoded, then the body."
  (let ((entry (nle::find-tool "explain_cron")))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

;;; --- what the reading says ------------------------------------------------------------

(deftest crontab-reads-a-weekday-schedule ()
  (with-zone (dir :types +half-past-five+)
    (with-cell-stop ((crontab-start "timezone" "Test/Zone" "zoneinfo_dir" dir))
      (let* ((nodecode-crontab::*now* +afternoon+)
             (text (nodecode-crontab::reading-for "0 9 * * 1-5"  :source "the test's own")))
        (is (search "At 09:00 on Monday through Friday." text) "the sentence reads the schedule")
        (is (search "Next 5 runs, in Test/Zone (the test's own)" text) "the zone a reading is made in is named")
        (is (search "Mon 2026-11-09 09:00 UTC+05:30" text) "the first run is Monday, in the zone's own clock")
        (is (search "Fri 2026-11-13 09:00 UTC+05:30" text) "the fifth is that Friday")
        (is (not (search "Sat 2026" text)) "and a weekday schedule never lists a Saturday")
        (is (search "day of week   1-5" text) "every field is on a row of its own")))))

(deftest crontab-notes-what-people-get-wrong ()
  (with-zone (dir :types '((0 . nil)))
    (with-cell-stop ((crontab-start "timezone" "Test/Zone" "zoneinfo_dir" dir))
      (let* ((nodecode-crontab::*now* +afternoon+)
             (either (nodecode-crontab::reading-for "0 9 1 * 1"  :source "the test's own"))
             (step (nodecode-crontab::reading-for "*/7 * * * *"  :source "the test's own"))
             (day31 (nodecode-crontab::reading-for "0 9 31 2 *"  :source "the test's own")))
        (is (search "on day 1 of the month or on Monday" either) "both day fields are said as either")
        (is (search "EITHER one runs" either) "and the note explains why")
        (is (search "Every 7 minutes" step) "a step is said as a step")
        (is (search "*/7 in the minute field starts again every hour" step) "with the turn of the hour noted")
        (is (search "Next runs: none." day31) "a date that never comes says so")
        (is (search "skipped" day31) "and day 31 is explained")))))

(deftest crontab-a-shortcut-is-read-as-the-fields-it-stands-for ()
  (with-zone (dir :types '((0 . nil)))
    (with-cell-stop ((crontab-start "timezone" "Test/Zone" "zoneinfo_dir" dir))
      (let* ((nodecode-crontab::*now* +afternoon+)
             (daily (nodecode-crontab::reading-for "@daily"  :source "the test's own")))
        (is (search "At 00:00 every day." daily) "@daily is read in words")
        (is (search "same as 0 0 * * *" daily) "and the fields it stands for are shown")
        (is (search "Mon 2026-11-09 00:00" daily) "with the run that comes next")))))

(deftest crontab-reads-runs-across-a-change-of-offset ()
  (with-zone (dir :transitions (list (encode-universal-time 0 30 0 10 11 2026 0))
                   :types '((3600 . nil) (7200 . t)))
    (with-cell-stop ((crontab-start "timezone" "Test/Zone" "zoneinfo_dir" dir))
      (let* ((nodecode-crontab::*now* (encode-universal-time 0 0 12 9 11 2026 0))
             (text (nodecode-crontab::reading-for "0 0 * * *"  :source "the test's own")))
        (is (search "Tue 2026-11-10 00:00 UTC+01:00" text) "the run before the change keeps the old offset")
        (is (search "Wed 2026-11-11 00:00 UTC+02:00" text) "and the one after it carries the new one")))))

;;; --- the tool and the command ---------------------------------------------------------

(deftest crontab-the-tool-answers-through-its-contract ()
  (with-zone (dir :types +half-past-five+)
    (with-cell-stop ((crontab-start "timezone" "Test/Zone" "zoneinfo_dir" dir "runs" 2))
      (let ((nodecode-crontab::*now* +afternoon+))
        (let ((answer (crontab-call "expression" "0 9 * * 1-5")))
          (is (search "At 09:00 on Monday through Friday." answer) "the tool reads the schedule")
          (is (search "Next 2 runs" answer) "the runs setting bounds the list"))
        (is (search "UTC+05:30" (crontab-call "expression" "0 9 * * 1-5" "timezone" "Test/Zone")) "and a zone the call names is read")
        (let ((refused (crontab-call "expression" "0 9 * * 9")))
          (is (search "ERROR:" refused) "a reading that cannot be made is answered, not signalled")
          (is (search "day of week" refused) "naming the field to repair"))))))

(deftest crontab-a-call-without-an-expression-is-refused ()
  (with-cell-stop ((crontab-start))
    (let ((entry (nle::find-tool "explain_cron")))
      (is (signals-error nlk:contract-refusal
                         (nlk:contract-decode (second entry) (nlk:make-json-object))) "the contract refuses a call that names no expression"))))

(deftest crontab-the-command-answers-and-shows-the-way-in ()
  (with-zone (dir :types +half-past-five+)
    (with-cell-stop ((crontab-start "timezone" "Test/Zone" "zoneinfo_dir" dir))
      (let ((nodecode-crontab::*now* +afternoon+))
        (is (search "Monday through Friday"
                    (nodecode-crontab::run-crontab "\"0 9 * * 1-5\"" "s1")) "/crontab reads an expression, quoted as it is typed")
        (let ((examples (nodecode-crontab::run-crontab "" "s1")))
          (is (search "=>" examples) "typed alone it shows examples")
          (is (search "At 00:00 every day." examples) "each example with its reading"))
        (is (search "this has 4"
                    (nodecode-crontab::run-crontab "0 9 * *" "s1")) "and a broken expression is a sentence, not a signal")))))
