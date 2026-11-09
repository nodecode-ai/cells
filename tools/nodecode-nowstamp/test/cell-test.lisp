;;;; cell-test.lisp --- the clock the model reads, and the gap behind it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Offline: the clock is pinned with the cell's own *CLOCK* wherever an
;;;; instant is asserted, the zones named are the machine's own tz database
;;;; (UTC, and Asia/Tokyo for a +09:00 reading), and the state file the gap
;;;; survives a restart in is one this home keeps, as it does in use. What is
;;;; asserted is what an operator gets: the standing section at the head of a
;;;; turn, the gap from the turn before, the language and the format, the tool
;;;; and /now, the tool-result stamp behind its switch, and a refusal -- not a
;;;; wrong hour -- for a zone that is not there.

(in-package #:nodecode.test)

(define-test-slice "nowstamp" "NOWSTAMP-" :start nodecode-nowstamp:start-cell)

;;; --- the cell's own lifecycle -------------------------------------------------

(define-cell-lifecycle-tests "nowstamp"
  (:config)
  (:hooks 'nle::read-harness-sections)
  (:command "now")
  (:help :nowstamp)
  (:running
   (let ((section (nowstamp-section "n-lifecycle")))
     (is-present section "the standing section rides the harness"
                 (is (search "[NOWSTAMP] Now:" section) "with a reading the model can use")
                 (is (search "Read at this turn's first request" section) "and says what it is for"))
     (is (= 1 (count "nowstamp" (nle::read-harness-sections "n-lifecycle") :key #'car :test #'string=)) "one of it, read any number of times")))
  (:refused ("timezone" "No/Such_Zone")
            ("timezone" "../etc/passwd")
            ("timezone" "Europe/Berlin/")
            ("timezone" "Europe/Berlin and more")
            ("language" "fr"))
  (:idle nodecode-nowstamp:nowstamp-error (nodecode-nowstamp::setting :timezone)))

;;; --- helpers ------------------------------------------------------------------

(defparameter +moment+ 1793000000 "1956-10-26 07:33 UTC, a Friday")
(defparameter +five-minutes-later+ 1793000300 "the same Friday, five minutes on")
(defparameter +two-days-later+ 1793200000 "Sunday 1956-10-28 15:06 UTC")

(defun nowstamp-section (session) "The cell's standing section for SESSION, or NIL."
  (cdr (assoc "nowstamp" (nle::read-harness-sections session) :test #'string=)))

(defun nowstamp-utc (time) "TIME as the cell reads it in UTC under the default format: the test's own
reading of the same instant, written the way the cell writes it."
  (multiple-value-bind (sec min hour date month year) (decode-universal-time time 0)
    (declare (ignore sec))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d UTC+00:00"
            year month date hour min)))

(defun nowstamp-call (&rest pairs) "The now tool as the engine calls it: the declared contract decoded, then the
body."
  (let ((entry (nle::find-tool "now")))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defun nowstamp-tool-text (text) "TEXT through the cell's :TOOL hook, as a tool result leaves it -- TEXT
itself when nothing is hooked."
  (let ((hook (cell-hook "nodecode-nowstamp" :tool)))
    (if hook
        (funcall hook
                 (list :name "sh" :arguments (nlk:make-json-object "script" "date"))
                 (lambda (op) (declare (ignore op)) text))
        text)))

;;; --- the clock, and the gap behind it ------------------------------------------

(deftest nowstamp-the-clock-and-the-gap ()
  (with-cell-stop ((nowstamp-start "timezone" "UTC"))
    (let* ((nodecode-nowstamp::*clock* +moment+)
           (first (nowstamp-section "n-clock")))
      (is (search (nowstamp-utc +moment+) first) "the clock is there, in the zone the section names")
      (is (search "Fri " first) "with the weekday, in English by default")
      (is (not (search "previous message" first)) "a session this cell has never seen gets no gap: there is nothing to measure from"))
    (let* ((nodecode-nowstamp::*clock* +five-minutes-later+)
           (second (nowstamp-section "n-clock")))
      (is (search "previous message: 5 min ago (today 07:33)" second) "the next turn is told the real gap, and the clock time it counted from"))
    (let* ((nodecode-nowstamp::*clock* +two-days-later+)
           (third (nowstamp-section "n-clock")))
      (is (search "previous message: 2 d 7 h ago (1956-10-26 07:38)" third) "a gap past a day is said in days and names the date, not `today'"))
    (let ((nodecode-nowstamp::*clock* (+ +two-days-later+ 3600)))
      (is (search "previous message: 1 h ago (today 15:06)" (nowstamp-section "n-clock")) "and an hour of the same day reads as one"))))

(deftest nowstamp-the-gap-survives-a-restart ()
  (let ((nodecode-nowstamp::*clock* +moment+))
    (with-cell-stop ((nowstamp-start "timezone" "UTC"))
      (nowstamp-section "n-restart")))
  ;; A restart: the table in memory goes, the state file under this home does
  ;; not, so the next process reads the same gap.
  (clrhash nodecode-nowstamp::*last-seen*)
  (let ((nodecode-nowstamp::*clock* +five-minutes-later+))
    (with-cell-stop ((nowstamp-start "timezone" "UTC"))
      (is (search "previous message: 5 min ago" (nowstamp-section "n-restart")) "the instant the last process wrote is the one this one measures from"))))

(deftest nowstamp-the-language-and-the-format-are-the-operators ()
  (with-cell-stop ((nowstamp-start "timezone" "UTC" "language" "id" "format" "%A %d %m %Y %H:%M %z"))
    (let* ((nodecode-nowstamp::*clock* +moment+)
           (section (nowstamp-section "n-format")))
      (is (search "Jumat 26 10 1956 07:33 +0000" section) "the weekday in the language asked for, the date in the order asked for")
      (is (not (search "Friday" section)) "and not in English"))))

(deftest nowstamp-an-unknown-zone-is-named-not-guessed ()
  (with-cell-stop ((nowstamp-start "timezone" "UTC"))
    (let ((answer (nowstamp-call "timezone" "No/Such_Zone"))
          (said (nodecode-nowstamp::run-now "No/Such_Zone" "n-zone")))
      (is (search "ERROR:" answer) "the tool answers a failure")
      (is (search "No/Such_Zone" answer) "naming the zone it could not find")
      (is (search "tz database" answer) "and the database it looked in")
      (is (search "No/Such_Zone" said) "/now says the same in words")
      (is (not (search "Now:" said)) "and no reading is invented"))))

;;; --- the reading on demand: the tool and /now -----------------------------------

(deftest nowstamp-the-tool-and-now-read-the-same-clock ()
  (with-cell-stop ((nowstamp-start "timezone" "UTC"))
    (let ((nodecode-nowstamp::*clock* +moment+))
      (is (search (nowstamp-utc +moment+) (nowstamp-call)) "now answers the clock")
      (is (search "UTC+00:00" (nowstamp-call)) "with the offset the zone has")
      (is (search "UTC+09:00" (nowstamp-call "timezone" "Asia/Tokyo")) "and any zone the database holds, when a call names one")
      (is (search (nowstamp-utc +moment+) (nodecode-nowstamp::run-now "" "n-now")) "/now answers the same reading to the operator")
      (nowstamp-section "n-now"))
    (let ((nodecode-nowstamp::*clock* +five-minutes-later+))
      (let ((said (nodecode-nowstamp::run-now "" "n-now")))
        (is (search "previous message: 5 min ago" said) "/now carries the gap of the session it was typed in")))))

;;; --- the tool-result stamp, behind its switch -----------------------------------

(deftest nowstamp-tool-results-carry-the-time-only-when-asked ()
  (with-cell-stop ((nowstamp-start "timezone" "UTC"))
    (is (null (cell-hook "nodecode-nowstamp" :tool)) "off by default: the :TOOL point is not hooked at all")
    (is (equal "ok" (nowstamp-tool-text "ok")) "so a tool result is untouched"))
  (with-cell-stop ((nowstamp-start "timezone" "UTC" "format" "%Y-%m-%d %H:%M %Z" "stamp_tool_results" t "stamp_interval_seconds" 0))
    (let ((nodecode-nowstamp::*clock* +moment+))
      (let ((stamped (nowstamp-tool-text "ok")))
        (is (search "ok" stamped) "the result itself is kept")
        (is (search (format nil "~%[Current time: ~a]" (nowstamp-utc +moment+)) stamped) "and a line with the current time follows it")))))

(deftest nowstamp-a-long-turn-is-spaced-not-stamped-every-time ()
  (with-cell-stop ((nowstamp-start "timezone" "UTC" "stamp_tool_results" t "stamp_interval_seconds" 60))
    (let ((nodecode-nowstamp::*clock* +moment+))
      (nowstamp-section "n-spacing")
      (is (equal "ok" (nowstamp-tool-text "ok")) "the turn's own reading counts: no stamp inside the interval"))
    (let ((nodecode-nowstamp::*clock* (+ +moment+ 90)))
      (is (search "[Current time:" (nowstamp-tool-text "ok")) "past the interval the model is told the time again")
      (is (equal "ok" (nowstamp-tool-text "ok")) "and once is enough until the next interval passes"))))
