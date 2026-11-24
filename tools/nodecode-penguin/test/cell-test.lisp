;;;; cell-test.lisp --- the conversion, the windows, the saved team and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Offline: the only tz database these tests read is one of their own, written
;;;; byte by byte in a scratch folder of the home -- fixed-offset zones, and a
;;;; zone that springs forward and falls back -- so a reading is asserted
;;;; against a file this test wrote, not against the machine's own data. What is
;;;; pinned is what a user gets: one instant read in several zones, a repeated
;;;; hour refused until the call says which one, a gap refused, the windows
;;;; where a whole meeting fits, a team saved and read back, /penguin, and the
;;;; refusals a wrong call answers instead of throwing.

(in-package #:nodecode.test)

(define-test-slice "penguin" "PENGUIN-" :start nodecode-penguin:start-cell)

(define-cell-lifecycle-tests "penguin"
  (:config)
  (:command "penguin")
  (:help :penguin)
  (:running
   (dolist (name '("penguin_convert" "penguin_overlap" "penguin_save_team" "penguin_get_team"))
     (is (nle::find-tool name) (format nil "~a is registered" name))))
  (:refused ("zoneinfo_dir" "/no/such/tz-database")
            ("max_slots" 0)
            ("max_slots" 25))
  (:idle nodecode-penguin:penguin-error (nodecode-penguin::named-zone "UTC")))

;;; --- a tz database of this test's own ---------------------------------------------

(defun penguin-tzif (&key (transitions '()) (types '((0 . nil))) (abbrs "UTC\0"))
  "A version 2 TZif file: TRANSITIONS (universal times), ((OFFSET . DST) ...) per
TYPE, and the abbreviation bytes ABBRS, four per type. Version 2 is what the tz
database ships: the 4-byte block for old readers, then the 8-byte one."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (flet ((put (value size)
             (dotimes (index size)
               (vector-push-extend (ldb (byte 8 (* 8 (- size 1 index))) value) out)))
           (text (string)
             (loop for char across string do (vector-push-extend (char-code char) out))))
      (labels ((header ()
                 (text "TZif")
                 (vector-push-extend 50 out)
                 (dotimes (index 15) (vector-push-extend 0 out))
                 (put 0 4)
                 (put 0 4)
                 (put 0 4)
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
        (text (format nil "~%~a~%" abbrs))
        (coerce out '(vector (unsigned-byte 8)))))))

(defun penguin-write-bytes (dir name octets)
  "NAME under the tz database DIR, holding OCTETS."
  (let ((path (merge-pathnames name (uiop:ensure-directory-pathname dir))))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    (namestring path)))

(defun penguin-write-zone (dir name &rest keys)
  "NAME under the tz database DIR, holding the zone KEYS describes."
  (penguin-write-bytes dir name (apply #'penguin-tzif keys)))

(defun penguin-zone-dir ()
  "A tz database of this test's own: fixed offsets, and one zone that moves."
  (let ((dir (merge-pathnames (format nil "penguin-zones-~12,'0x/" (random (expt 16 10)))
                               (nlk:home))))
    (penguin-write-zone dir "UTC" :types '((0 . nil)) :abbrs "UTC\0")
    (penguin-write-zone dir "Test/East" :types '((32400 . nil)) :abbrs "EAS\0")
    (penguin-write-zone dir "Test/West" :types '((-18000 . nil)) :abbrs "WES\0")
    (penguin-write-zone dir "Test/Mid" :types '((0 . nil)) :abbrs "MID\0")
    (penguin-write-zone dir "Test/FallBack" :transitions '(1774746000 1792890000)
                         :types '((3600 . nil) (7200 . t)) :abbrs "CET\0CEST\0")
    (penguin-write-bytes dir "Test/NotAZone" #(110 111 116 32 97 32 122 111 110 101))
    (namestring dir)))

(defmacro with-zone-dir ((dir) &body body)
  "BODY with DIR a tz database of the test's own, removed after."
  `(let ((,dir (penguin-zone-dir)))
     (nlk:with-cleanup ((uiop:delete-directory-tree (pathname ,dir) :validate t
                                                                :if-does-not-exist :ignore))
       ,@body)))

;;; --- what a call builds ---------------------------------------------------------------

(defun penguin-call (name &rest pairs)
  "The tool NAME as the engine calls it: the declared contract decoded, then the body."
  (let ((entry (nle::find-tool name)))
    (funcall (third entry)
             (nlk:contract-decode (second entry) (apply #'nlk:make-json-object pairs)))))

(defparameter +penguin-weekdays+ (vector 0 1 2 3 4 5 6))

(defun penguin-member-json (name zone start end &optional (weekdays +penguin-weekdays+))
  "One member, as the wire carries it."
  (nlk:make-json-object
   "name" name "zone" zone
   "windows" (vector (nlk:make-json-object "start" start "end" end "weekdays" weekdays))))

(defun penguin-team-json (name &rest members)
  "A team, as the wire carries it."
  (nlk:make-json-object "name" name "members" (coerce members 'vector)))

;;; --- one instant, read in every zone ---------------------------------------------------

(deftest penguin-converts-one-instant-across-zones ()
  (with-zone-dir (dir)
    (with-cell-stop ((penguin-start "zoneinfo_dir" dir))
      (let ((text (penguin-call "penguin_convert"
                                "local_time" "2026-11-08T14:30"
                                "source_zone" "Test/East"
                                "target_zones" (vector "Test/West" "UTC"))))
        (is (search "\"ok\":true" text) "the call answers")
        (is (search "2026-11-08T05:30Z" text) "14:30 at UTC+09:00 is 05:30 UTC")
        (is (search "2026-11-08T00:30" text) "which is 00:30 in the -05:00 zone")
        (is (search "UTC-05:00" text) "named by its offset")
        (is (search "\"resolution\":\"unique\"" text) "and says the reading was the only one"))
      (let ((text (penguin-call "penguin_convert"
                                "local_time" "2026-11-08T14:30"
                                "source_zone" "Test/East"
                                "target_zones" (vector "Test/NotAZone"))))
        (is (search "\"ok\":false" text) "a file of the database that is not a zone is answered")
        (is (search "is not a zone file" text) "in the words of the refusal")))))

(deftest penguin-refuses-a-repeated-hour-until-the-call-chooses ()
  (with-zone-dir (dir)
    (with-cell-stop ((penguin-start "zoneinfo_dir" dir))
      (let ((ask (penguin-call "penguin_convert"
                               "local_time" "2026-10-25T02:30"
                               "source_zone" "Test/FallBack"
                               "target_zones" (vector "UTC"))))
        (is (search "\"ok\":false" ask) "the default refuses to guess")
        (is (search "occurs twice" ask) "and says why"))
      (let ((earlier (penguin-call "penguin_convert"
                                   "local_time" "2026-10-25T02:30"
                                   "source_zone" "Test/FallBack"
                                   "target_zones" (vector "UTC")
                                   "ambiguity" "earlier")))
        (is (search "2026-10-25T00:30Z" earlier) "earlier is the reading before the clocks went back")
        (is (search "\"ambiguous\":true" earlier) "and is marked as one of two"))
      (let ((later (penguin-call "penguin_convert"
                                 "local_time" "2026-10-25T02:30"
                                 "source_zone" "Test/FallBack"
                                 "target_zones" (vector "UTC")
                                 "ambiguity" "later")))
        (is (search "2026-10-25T01:30Z" later) "later is the reading an hour on"))
      (let ((gap (penguin-call "penguin_convert"
                               "local_time" "2026-03-29T02:30"
                               "source_zone" "Test/FallBack"
                               "target_zones" (vector "UTC"))))
        (is (search "does not exist" gap) "a time the clocks skipped is refused")))))

;;; --- the windows a whole team shares ---------------------------------------------------

(deftest penguin-finds-the-windows-where-the-whole-meeting-fits ()
  (with-zone-dir (dir)
    (with-cell-stop ((penguin-start "zoneinfo_dir" dir))
      (let ((team (penguin-team-json
                   "Evening standup"
                   (penguin-member-json "Ana" "Test/East" "18:00" "22:00")
                   (penguin-member-json "Bo" "Test/Mid" "09:00" "13:00"))))
        (let ((text (penguin-call "penguin_overlap"
                                  "team" team
                                  "reference_zone" "Test/Mid"
                                  "start_date" "2026-11-09"
                                  "duration_minutes" 60
                                  "step_minutes" 60
                                  "max_slots" 2)))
          (is (search "\"total_slots\":4" text) "four whole hours fit, so four hourly starts")
          (is (search "2026-11-09T09:00Z" text) "the first opens at 09:00 UTC")
          (is (search "\"slots_truncated\":true" text) "and the offer stops at the two asked for")
          (is (search "Evening standup" text) "the answer names the team"))
        (let ((text (penguin-call "penguin_overlap"
                                  "team" (penguin-team-json
                                           "Split nights"
                                           (penguin-member-json "Ana" "Test/East" "09:00" "18:00")
                                           (penguin-member-json "Bo" "Test/West" "09:00" "17:00"))
                                  "reference_zone" "Test/Mid"
                                  "start_date" "2026-11-09"
                                  "days" 3
                                  "duration_minutes" 30)))
          (is (search "\"total_slots\":0" text) "a team whose hours never meet has no window")
          (is (search "\"common_windows\":[]" text) "with none listed"))))))

(deftest penguin-checks-what-a-team-is-made-of ()
  (with-zone-dir (dir)
    (with-cell-stop ((penguin-start "zoneinfo_dir" dir))
      (let ((text (penguin-call "penguin_overlap"
                                "team" (penguin-team-json
                                         "Twins"
                                         (penguin-member-json "Ana" "Test/East" "09:00" "18:00")
                                         (penguin-member-json "Ana" "Test/West" "09:00" "17:00"))
                                "reference_zone" "Test/Mid"
                                "start_date" "2026-11-09"
                                "duration_minutes" 30)))
        (is (search "\"ok\":false" text) "two members of one name is refused")
        (is (search "unique" text) "in a sentence that says what to fix"))
      (let ((text (penguin-call "penguin_overlap"
                                "team" (penguin-team-json
                                         "Unknown hours"
                                         (penguin-member-json "Ana" "Test/East" "09:00" "18:00"))
                                "team_id" "other"
                                "reference_zone" "Test/Mid"
                                "start_date" "2026-11-09"
                                "duration_minutes" 30)))
        (is (search "not both" text) "a team and a team id together is refused")))))

;;; --- a team kept, and read back --------------------------------------------------------

(deftest penguin-keeps-a-team-under-an-id ()
  (with-zone-dir (dir)
    (with-cell-stop ((penguin-start "zoneinfo_dir" dir))
      (let ((saved (penguin-call "penguin_save_team"
                                 "team_id" "evening-standup"
                                 "team" (penguin-team-json
                                          "Evening standup"
                                          (penguin-member-json "Ana" "Test/East" "18:00" "22:00")
                                          (penguin-member-json "Bo" "Test/Mid" "09:00" "13:00")))))
        (is (search "\"saved\":true" saved) "the team is written"))
      (let ((read (penguin-call "penguin_get_team" "team_id" "evening-standup")))
        (is (search "Evening standup" read) "and read back under its id")
        (is (search "Test/East" read) "with its members' zones"))
      (let ((missing (penguin-call "penguin_get_team" "team_id" "no-such-team")))
        (is (search "\"ok\":false" missing) "an id nothing was saved under is a refusal")
        (is (search "no team is saved" missing) "in plain words"))
      (let ((text (penguin-call "penguin_overlap"
                                "team_id" "evening-standup"
                                "reference_zone" "Test/Mid"
                                "start_date" "2026-11-09"
                                "duration_minutes" 60)))
        (is (search "\"ok\":true" text) "and a search names the saved team instead of repeating it")
        (is (search "Evening standup" text) "by its name")))))

;;; --- /penguin --------------------------------------------------------------------------

(deftest penguin-command-reads-one-time-in-every-zone ()
  (with-zone-dir (dir)
    (with-cell-stop ((penguin-start "zoneinfo_dir" dir))
      (let ((text (nodecode-penguin::run-penguin "2026-11-08T14:30 Test/East Test/West UTC" "s1")))
        (is (search "2026-11-08T05:30Z" text) "the instant is stated")
        (is (search "Test/West" text) "and every zone named after the source is read")
        (is (search "Sun 2026-11-08 00:30" text) "at that zone's own wall time"))
      (let ((text (nodecode-penguin::run-penguin "" "s1")))
        (is (search "use /penguin" text) "and typing nothing says how to use it")))))
