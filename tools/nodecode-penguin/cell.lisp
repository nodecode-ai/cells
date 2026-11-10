;;;; cell.lisp --- deterministic IANA time conversion and whole-meeting overlap.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A meeting across zones is one wall-clock problem in several places at
;;;; once. A model that adds offsets in its head is wrong on the two days a
;;;; year the clocks move, wrong again on the half-hour zones, and wrong on
;;;; an overnight shift. This cell does the arithmetic on the machine's own
;;;; tz database: one explicit local time resolved to an instant and read in
;;;; every zone asked for, and a search for the windows where the whole of a
;;;; meeting fits inside every member's working hours.
;;;;
;;;; Nothing leaves the machine: no request, no subprocess. The files read
;;;; are a zone of the tz database under zoneinfo_dir, and the teams this
;;;; cell saved under the home; the one file written is that teams file.

(defpackage #:nodecode-penguin
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:penguin-error #:penguin-error-detail #:*penguin*))

(in-package #:nodecode-penguin)

(nlk:define-peripheral penguin :not-running t)

;;; --- the calendar ---------------------------------------------------------------
;;; Days from 1970-01-01, proleptic Gregorian, after Howard Hinnant's
;;; days_from_civil: the whole conversion is one integer day count, so a
;;; wall time, a date and a weekday are arithmetic and not a date library.

(defparameter +seconds-per-day+ 86400)

(defparameter +weekday-names+
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
  "English labels, Monday first, so a reading does not depend on the locale.")

(defparameter +zoneinfo+ "/usr/share/zoneinfo"
  "Where a zone name is looked up unless the section says otherwise.")

(defun days-from-civil (year month day)
  "The day number of YEAR-MONTH-DAY, days from 1970-01-01."
  (let* ((year (if (<= month 2) (1- year) year))
         (era (floor year 400))
         (yoe (- year (* era 400)))
         (mp (+ month (if (> month 2) -3 9)))
         (doy (+ (floor (+ (* 153 mp) 2) 5) (1- day)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (- (+ (* era 146097) doe) 719468)))

(defun civil-from-days (days)
  "YEAR, MONTH and DAY of the day number DAYS."
  (let* ((days (+ days 719468))
         (era (floor days 146097))
         (doe (- days (* era 146097)))
         (yoe (floor (- doe (floor doe 1460) (floor doe 36524) (floor doe 146096)) 365))
         (year (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (day (+ (- doy (floor (+ (* 153 mp) 2) 5)) 1))
         (month (+ mp (if (< mp 10) 3 -9))))
    (values (if (<= month 2) (1+ year) year) month day)))

(defun day-of-week (days)
  "DAYS as a weekday, Monday 0 through Sunday 6."
  (mod (+ 3 days) 7))

(defun date-text (days)
  "DAYS as YYYY-MM-DD."
  (multiple-value-bind (year month day) (civil-from-days days)
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun stamp-text (wall)
  "WALL seconds, read as if they were UTC, as YYYY-MM-DDTHH:MM."
  (multiple-value-bind (days rest) (floor wall +seconds-per-day+)
    (format nil "~aT~2,'0d:~2,'0d" (date-text days)
            (floor rest 3600) (mod (floor rest 60) 60))))

(defun readable-text (wall)
  "WALL seconds as Sat 2026-11-08 07:30: a person's reading, not the wire's."
  (multiple-value-bind (days rest) (floor wall +seconds-per-day+)
    (format nil "~a ~a ~2,'0d:~2,'0d"
            (subseq (aref +weekday-names+ (day-of-week days)) 0 3)
            (date-text days) (floor rest 3600) (mod (floor rest 60) 60))))

(defun offset-label (seconds)
  "SECONDS east of UTC as UTC+05:30."
  (multiple-value-bind (hours rest) (floor (abs seconds) 3600)
    (format nil "UTC~a~2,'0d:~2,'0d" (if (minusp seconds) "-" "+")
            hours (floor rest 60))))

(defun digits-p (text start count)
  "Whether the COUNT characters of TEXT from START are all digits."
  (and (<= (+ start count) (length text))
       (loop for index from start below (+ start count)
             always (digit-char-p (char text index)))))

(defun parse-date (text)
  "TEXT as a day number, YYYY-MM-DD between 2000 and 2100, or a refusal."
  (unless (and (stringp text) (= 10 (length text))
               (string= "-" (subseq text 4 5)) (string= "-" (subseq text 7 8))
               (digits-p text 0 4) (digits-p text 5 2) (digits-p text 8 2))
    (fail "~s is not a date; use YYYY-MM-DD, between 2000 and 2100" text))
  (let* ((year (parse-integer text :start 0 :end 4))
         (month (parse-integer text :start 5 :end 7))
         (day (parse-integer text :start 8 :end 10)))
    (unless (<= 2000 year 2100)
      (fail "~s is outside the dates this cell reads; use 2000-01-01 through 2100-12-31" text))
    (let ((days (days-from-civil year month day)))
      (multiple-value-bind (known-year known-month known-day) (civil-from-days days)
        (unless (and (= known-year year) (= known-month month) (= known-day day))
          (fail "~s is not a calendar date that exists" text)))
      days)))

(defun parse-minutes (text label)
  "TEXT as minutes after midnight, HH:MM, or a refusal naming LABEL."
  (unless (and (stringp text) (= 5 (length text)) (string= ":" (subseq text 2 3))
               (digits-p text 0 2) (digits-p text 3 2))
    (fail "~a ~s is not a time; use HH:MM in 24-hour form" label text))
  (let ((hours (parse-integer text :start 0 :end 2))
        (minutes (parse-integer text :start 3 :end 5)))
    (when (or (> hours 23) (> minutes 59))
      (fail "~a ~s is not a time of day; use HH:MM from 00:00 to 23:59" label text))
    (+ (* hours 60) minutes)))

(defun parse-local (text)
  "TEXT as a wall-clock time, YYYY-MM-DDTHH:MM, in seconds as if UTC."
  (unless (and (stringp text) (= 16 (length text)) (string= "T" (subseq text 10 11)))
    (fail "~s is not a local time; use YYYY-MM-DDTHH:MM, without a UTC offset" text))
  (+ (* (parse-date (subseq text 0 10)) +seconds-per-day+)
     (* (parse-minutes (subseq text 11 16) "local time") 60)))

;;; --- a zone of the tz database ---------------------------------------------------
;;; RFC 8536 in the little a clock needs: the transition times, which type
;;; each switches to, and each type's offset, daylight flag and abbreviation.
;;; A version 1 file has one block of 4-byte times; version 2 and later keep
;;; that block for old readers and follow it with a header and the block of
;;; 8-byte times, which is the one read here.

(defstruct (zone (:constructor make-zone (name transitions indices offsets daylight abbreviations)))
  "A zone of the tz database: NAME, the TRANSITIONS (universal, ascending),
INDICES the type each switches to, and per type its seconds east of UTC, its
daylight flag and its abbreviation."
  (name "" :read-only t)
  (transitions #() :read-only t)
  (indices #() :read-only t)
  (offsets #() :read-only t)
  (daylight #() :read-only t)
  (abbreviations #() :read-only t))

(defun read-octets (path)
  "Every byte of PATH."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence data in)
      data)))

(defun be-unsigned (data start size)
  "The big-endian unsigned integer of SIZE bytes at START in DATA."
  (let ((value 0))
    (dotimes (index size)
      (setf value (+ (ash value 8) (aref data (+ start index)))))
    value))

(defun be-signed (data start size)
  "The big-endian two's-complement integer of SIZE bytes at START in DATA."
  (let ((value (be-unsigned data start size)))
    (if (logbitp (1- (* 8 size)) value)
        (- value (ash 1 (* 8 size)))
        value)))

(defun tzif-counts (data at)
  "The six counts of the TZif header at AT: UT/local indicators, standard/wall
indicators, leap seconds, transitions, types and abbreviation bytes."
  (values (be-unsigned data (+ at 20) 4)
          (be-unsigned data (+ at 24) 4)
          (be-unsigned data (+ at 28) 4)
          (be-unsigned data (+ at 32) 4)
          (be-unsigned data (+ at 36) 4)
          (be-unsigned data (+ at 40) 4)))

(defun tzif-block-length (data at time-size)
  "How many bytes the block under the header at AT takes, in TIME-SIZE-byte times."
  (multiple-value-bind (utcnt stdcnt leapcnt timecnt typecnt charcnt) (tzif-counts data at)
    (+ (* timecnt time-size) timecnt (* typecnt 6) charcnt
       (* leapcnt (+ time-size 4)) utcnt stdcnt)))

(defun abbreviation-of (chars start)
  "The abbreviation CHARS spells from START up to its NUL."
  (let ((end (or (position 0 chars :start start) (length chars))))
    (map 'string #'code-char (subseq chars start end))))

(defun parse-tzif-block (data at time-size)
  "The transitions, the type each switches to, and each type's offset, daylight
flag and abbreviation, from the block under the header at AT."
  (multiple-value-bind (utcnt stdcnt leapcnt timecnt typecnt charcnt) (tzif-counts data at)
    (declare (ignore utcnt stdcnt leapcnt))
    (let ((cursor (+ at 44))
          (transitions (make-array timecnt))
          (indices (make-array timecnt))
          (offsets (make-array typecnt))
          (daylight (make-array typecnt))
          (starts (make-array typecnt)))
      (dotimes (index timecnt)
        (setf (aref transitions index) (be-signed data cursor time-size))
        (incf cursor time-size))
      (dotimes (index timecnt)
        (setf (aref indices index) (aref data cursor))
        (incf cursor))
      (dotimes (index typecnt)
        (setf (aref offsets index) (be-signed data cursor 4)
              (aref daylight index) (plusp (aref data (+ cursor 4)))
              (aref starts index) (aref data (+ cursor 5)))
        (incf cursor 6))
      (let ((chars (if (plusp charcnt)
                       (subseq data cursor (min (length data) (+ cursor charcnt)))
                       #())))
        (values transitions indices offsets daylight
                (map 'vector (lambda (start) (abbreviation-of chars start)) starts))))))

(defun read-zone (path name)
  "PATH as the ZONE called NAME, or a refusal."
  (let ((data (read-octets path)))
    (unless (and (>= (length data) 44) (equalp (subseq data 0 4) #(84 90 105 102)))
      (fail "~a is not a zone file of the tz database" name))
    (multiple-value-bind (transitions indices offsets daylight abbreviations)
        (if (member (aref data 4) '(0 48))
            (parse-tzif-block data 0 4)
            (let ((second (+ 44 (tzif-block-length data 0 4))))
              (when (> (+ second 44) (length data))
                (fail "~a ends before the 64-bit block it announces" name))
              (parse-tzif-block data second 8)))
      (make-zone name transitions indices offsets daylight abbreviations))))

(defun zone-type-index (zone universal)
  "The index of ZONE's type in force at universal time UNIVERSAL."
  (let ((transitions (zone-transitions zone)))
    (if (zerop (length transitions))
        0
        (let ((low 0) (high (1- (length transitions))) (found -1))
          (loop while (<= low high)
                do (let ((middle (floor (+ low high) 2)))
                     (if (<= (aref transitions middle) universal)
                         (setf found middle low (1+ middle))
                         (setf high (1- middle)))))
          (if (minusp found) 0 (aref (zone-indices zone) found))))))

(defun zone-offset (zone universal)
  "ZONE's seconds east of UTC at universal time UNIVERSAL."
  (if (zerop (length (zone-offsets zone)))
      0
      (aref (zone-offsets zone) (zone-type-index zone universal))))

(defun zone-daylight-p (zone universal)
  "Whether ZONE reads summer time at UNIVERSAL."
  (and (plusp (length (zone-daylight zone)))
       (aref (zone-daylight zone) (zone-type-index zone universal))))

(defun zone-abbreviation (zone universal)
  "The abbreviation ZONE writes at UNIVERSAL, such as CET."
  (if (zerop (length (zone-abbreviations zone)))
      ""
      (aref (zone-abbreviations zone) (zone-type-index zone universal))))

(defun zone-name-p (name)
  "Whether NAME is shaped like an IANA zone name, and cannot walk out of the database."
  (and (stringp name)
       (plusp (length name))
       (<= (length name) 64)
       (not (search ".." name))
       (not (string= "/" (subseq name 0 1)))
       (not (string= "/" (subseq name (1- (length name)))))
       (every (lambda (char) (or (alphanumericp char) (find char "/_-+" :test #'char=)))
              name)))

(defvar *zones* (make-hash-table :test #'equal)
  "Every zone read so far, by name and tz database folder.")

(defun zone-directory ()
  "The tz database folder the section names."
  (let ((dir (nlk:trimmed (or (setting :zoneinfo-dir) ""))))
    (if (plusp (length dir)) dir +zoneinfo+)))

(defun named-zone (name)
  "The ZONE called NAME -- an IANA name such as Asia/Taipei -- or a refusal."
  (unless (zone-name-p name)
    (fail "~s is not an IANA zone name; they look like Asia/Taipei or America/New_York" name))
  (let* ((dir (uiop:ensure-directory-pathname (zone-directory)))
         (key (cons name (namestring dir))))
    (or (gethash key *zones*)
        (let ((path (merge-pathnames name dir)))
          (unless (probe-file path)
            (fail "there is no zone ~s in the tz database at ~a; name an IANA zone such as Asia/Taipei"
                  name (namestring dir)))
          (when (> (hash-table-count *zones*) 256) (clrhash *zones*))
          (setf (gethash key *zones*) (read-zone path name))))))

(defun local-wall (zone universal)
  "UNIVERSAL as the wall-clock seconds ZONE reads, as if they were UTC."
  (+ universal (zone-offset zone universal)))

(defun resolve-local (zone wall)
  "Every universal time ZONE reads as WALL seconds: none in a spring-forward
  gap, two in an autumn repeated hour, one otherwise."
  (let ((candidates '()))
    (dotimes (index (length (zone-offsets zone)) (sort (remove-duplicates candidates) #'<))
      (let* ((offset (aref (zone-offsets zone) index))
             (universal (- wall offset)))
        (when (= (zone-offset zone universal) offset)
          (push universal candidates))))))

(defun local-view (zone name universal)
  "UNIVERSAL read in ZONE, as the JSON object the model reads."
  (let* ((offset (zone-offset zone universal))
         (days (floor (local-wall zone universal) +seconds-per-day+)))
    (nlk:make-json-object
     "zone" name
     "local" (stamp-text (local-wall zone universal))
     "date" (date-text days)
     "weekday" (aref +weekday-names+ (day-of-week days))
     "offset" (offset-label offset)
     "offset_minutes" (floor offset 60)
     "abbreviation" (zone-abbreviation zone universal)
     "is_dst" (zone-daylight-p zone universal))))

(defun json-array (items)
  "ITEMS as a JSON array: an empty list is [], never null and never false."
  (coerce (or items '()) 'vector))

(defun database-note ()
  "What an answer was computed from, for the model to state honestly."
  (nlk:make-json-object
   "source" "this machine's tz database under zoneinfo_dir"
   "precision" "minute"
   "algorithm_version" "1"))

;;; --- the boundary ----------------------------------------------------------------

(defparameter +ambiguity-choices+ '("reject" "earlier" "later")
  "What a conversion does with a wall time that occurs twice.")

(nlk:define-contract penguin-window
  (:doc "One stretch of a member's working day, in that member's own zone.")
  ("start" :text :required t :doc "the window opens, HH:MM in the member's zone")
  ("end" :text :required t :doc "the window closes, HH:MM; an end before the start is a shift that runs overnight on the start day")
  ("weekdays" :array :of :integer :doc "the days the window applies to, Monday 0 through Sunday 6; empty means none"))

(nlk:define-contract penguin-member
  (:doc "One person on the team: their name, their zone and the hours they work.")
  ("name" :text :required t :doc "how a meeting proposal names them")
  ("zone" :text :required t :doc "their confirmed IANA zone, such as Asia/Taipei; never guessed from a name or a country")
  ("windows" :array :of penguin-window :doc "their working windows, up to 8; none means they keep no hours this search can use"))

(nlk:define-contract penguin-team
  (:doc "A team: a name, and one entry per person with their zone and working windows.")
  ("name" :text :required t :doc "the team's name, as a meeting proposal says it")
  ("members" :array :of penguin-member :required t :doc "one to thirty people"))

(nlk:define-contract penguin-convert
  (:doc "Convert one explicit local date and time from one IANA zone into others, using this machine's tz database. A time that does not exist is refused (the clocks jumped forward over it); a time that occurs twice is refused by default, so ask the user which one they mean. It never assumes the current time.")
  ("local_time" :text :required t :doc "YYYY-MM-DDTHH:MM, between 2000 and 2100, with no UTC offset")
  ("source_zone" :text :required t :doc "the IANA zone that local time is in, such as Asia/Taipei")
  ("target_zones" :array :of :string :required t :doc "the IANA zones to read the same instant in, 1 to 30")
  ("ambiguity" :text :options +ambiguity-choices+ :default "reject"
   :doc "what to do when the local time occurs twice: reject (ask the user), earlier or later"))

(nlk:define-contract penguin-overlap
  (:doc "Find the windows where a meeting of a given length fits inside the whole of every member's own working hours, in their own zones. Pass a team object, or the team_id of one saved with penguin_save_team. Dates and the windows are read in reference_zone. Weekdays are Monday 0 through Sunday 6; a window whose end is before its start runs overnight on the start day. Answers the earliest windows first. It books nothing and reads no calendar.")
  ("team" :object :of penguin-team :doc "the team, inline")
  ("team_id" :text :doc "the id of a team saved with penguin_save_team, instead of team")
  ("reference_zone" :text :required t :doc "the IANA zone that start_date and the reported windows are read in")
  ("start_date" :text :required t :doc "the first date to search, YYYY-MM-DD in reference_zone")
  ("days" :integer :default 1 :doc "how many dates to search, 1 to 14")
  ("duration_minutes" :integer :required t :doc "how long the meeting is, 1 to 480 minutes")
  ("step_minutes" :integer :default 15 :doc "how far apart the offered starts are, 1 to 120 minutes")
  ("max_slots" :integer :doc "how many starts to offer, 1 to 20; the cell's max_slots setting answers when omitted"))

(nlk:define-contract penguin-save-team
  (:doc "Save or replace a named team, so the next search names it by id instead of repeating it. Save only when the user asks to remember the team; the same id replaces the whole team.")
  ("team_id" :text :required t :doc "the id to save under, 1 to 40 lowercase letters, digits, underscores or hyphens")
  ("team" :object :of penguin-team :required t :doc "the team to keep"))

(nlk:define-contract penguin-get-team
  (:doc "Read a team saved earlier by its exact id.")
  ("team_id" :text :required t :doc "the id it was saved under"))

;;; --- a member's hours, and a team of them ------------------------------------------

(defstruct (work-window (:constructor make-work-window (start end weekdays)))
  "One declared window, in minutes after the member's own midnight."
  (start 0 :read-only t)
  (end 0 :read-only t)
  (weekdays '() :read-only t))

(defstruct (person (:constructor make-person (name zone-label zone windows)))
  "One member: their NAME, the ZONE-LABEL they gave, the ZONE read, and WINDOWS."
  (name "" :read-only t)
  (zone-label "" :read-only t)
  (zone nil :read-only t)
  (windows '() :read-only t))

(defstruct (crew (:constructor make-crew (name members)))
  "A team: its NAME and its people."
  (name "" :read-only t)
  (members '() :read-only t))

(defun checked-window (window label)
  "One declared WINDOW of LABEL's, as a WORK-WINDOW, or a refusal."
  (let ((start (parse-minutes (penguin-window-start window)
                               (format nil "~a's window start" label)))
        (end (parse-minutes (penguin-window-end window)
                             (format nil "~a's window end" label)))
        (days (coerce (or (penguin-window-weekdays window) '()) 'list)))
    (dolist (day days)
      (unless (and (integerp day) (<= 0 day 6))
        (fail "~a's weekdays are Monday 0 through Sunday 6, and ~s is not" label day)))
    (make-work-window start end (sort (remove-duplicates days) #'<))))

(defun checked-team (record)
  "RECORD, a decoded penguin-team, as a CREW, or a refusal."
  (let ((name (nlk:trimmed (penguin-team-name record)))
        (members (coerce (or (penguin-team-members record) '()) 'list)))
    (when (or (zerop (length name)) (> (length name) 80))
      (fail "a team name is 1 to 80 characters, and ~s is not" name))
    (unless (<= 1 (length members) 30)
      (fail "a team needs 1 to 30 members, and this one has ~d" (length members)))
    (let ((seen '()) (clean '()))
      (dolist (member members)
        (let ((label (nlk:trimmed (penguin-member-name member)))
              (zone-label (nlk:trimmed (penguin-member-zone member))))
          (when (member label seen :test #'string-equal)
            (fail "two members are called ~s; member names are unique within a team" label))
          (push label seen)
          (let ((windows (coerce (or (penguin-member-windows member) '()) 'list)))
            (when (> (length windows) 8)
              (fail "~a has ~d working windows; a member may have 0 to 8" label (length windows)))
            (push (make-person label zone-label (named-zone zone-label)
                               (mapcar (lambda (window) (checked-window window label)) windows))
                  clean))))
      (make-crew name (nreverse clean)))))

(defun working-p (wall windows)
  "Whether WALL -- a member's own local seconds -- falls in one of WINDOWS."
  (let* ((days (floor wall +seconds-per-day+))
         (minute (floor (- wall (* days +seconds-per-day+)) 60)))
    (dolist (window windows nil)
      (let ((start (work-window-start window))
            (end (work-window-end window)))
        (unless (= start end)
          (let* ((overnight-p (> start end))
                 (shift-days (if (and overnight-p (< minute end)) (1- days) days))
                 (inside-p (if overnight-p
                               (or (>= minute start) (< minute end))
                               (and (>= minute start) (< minute end)))))
            (when (and inside-p (member (day-of-week shift-days)
                                        (work-window-weekdays window)))
              (return t))))))))

(defun interval-view (start end crew)
  "START through END as one JSON object, read in UTC and in every member's zone."
  (nlk:make-json-object
   "start_utc" (format nil "~aZ" (stamp-text start))
   "end_utc" (format nil "~aZ" (stamp-text end))
   "elapsed_minutes" (floor (- end start) 60)
   "members" (json-array (mapcar (lambda (member)
                        (nlk:make-json-object
                         "name" (person-name member)
                         "start" (local-view (person-zone member) (person-zone-label member) start)
                         "end" (local-view (person-zone member) (person-zone-label member) end)))
                      (crew-members crew)))))

;;; --- the teams this cell keeps -----------------------------------------------------

(defun teams-file ()
  "The file saved teams are kept in, under this home."
  (nlk:home "penguin/teams.json"))

(defun read-teams ()
  "Every saved team, as the id -> its JSON object table the file holds."
  (let ((table (make-hash-table :test #'equal))
        (path (teams-file)))
    (when (probe-file path)
      (let* ((data (handler-case (nlk:decode-json (uiop:read-file-string path))
                     (error () nil)))
             (teams (nlk:json-value data :object "teams")))
        (when (hash-table-p teams)
          (maphash (lambda (id value) (setf (gethash id table) value)) teams))))
    table))

(defun write-teams (table)
  "TABLE as this cell's teams file, its folder made first."
  (let ((path (teams-file)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:encode-json-object
                     (nlk:make-json-object "schema_version" 1 "teams" table))
                    out))
    path))

(defun team-id-p (id)
  "Whether ID is shaped like a team id, and cannot name anything else."
  (and (stringp id)
       (<= 1 (length id) 40)
       (let ((first (char id 0)))
         (or (digit-char-p first) (and (lower-case-p first) (alpha-char-p first))))
       (every (lambda (char) (or (alphanumericp char)
                                 (find char "-_" :test #'char=)))
              id)))

(defun checked-team-id (id)
  "ID, or a refusal naming what a team id looks like."
  (unless (team-id-p id)
    (fail "~s is not a team id; ids are 1 to 40 lowercase letters, digits, underscores or hyphens" id))
  id)

(defun saved-team (id)
  "The crew saved under ID, or a refusal."
  (checked-team-id id)
  (let ((encoded (gethash id (read-teams))))
    (unless encoded
      (fail "no team is saved under ~s; save one with penguin_save_team first" id))
    (checked-team (nlk:contract-decode (nlk:contract 'penguin-team) encoded))))

;;; --- the answers ------------------------------------------------------------------

(defun checked-instant (local source targets ambiguity)
  "The universal time LOCAL names in SOURCE, with every target checked:
  (values INSTANT SOURCE-ZONE CANDIDATE-COUNT)."
  (let* ((wall (parse-local local))
         (name (nlk:trimmed source))
         (zone (named-zone name))
         (count (length targets))
         (choice (or (and (stringp ambiguity) (nlk:trimmed ambiguity)) "reject")))
    (unless (<= 1 count 30)
      (fail "name 1 to 30 target zones, and this call names ~d" count))
    (dolist (target targets) (named-zone target))
    (unless (member choice +ambiguity-choices+ :test #'string=)
      (fail "ambiguity is reject, earlier or later, and ~s is not" ambiguity))
    (let ((candidates (resolve-local zone wall)))
      (when (null candidates)
        (fail "~s does not exist in ~a: the clocks jump forward over it, so ask the user for another local time"
              local name))
      (when (and (= 2 (length candidates)) (string= choice "reject"))
        (fail "~s occurs twice in ~a, when the clocks go back; ask which one is meant, then retry with ambiguity earlier or later"
              local name))
      (values (if (string= choice "later") (car (last candidates)) (car candidates))
              zone (length candidates) choice))))

(defun convert-action (request)
  "The penguin_convert answer: one instant, read in every zone asked for."
  (let* ((local (penguin-convert-local-time request))
         (source (nlk:trimmed (penguin-convert-source-zone request)))
         (targets (mapcar #'nlk:trimmed
                          (coerce (or (penguin-convert-target-zones request) '()) 'list)))
         (ambiguity (penguin-convert-ambiguity request)))
    (multiple-value-bind (instant zone count choice)
        (checked-instant local source targets ambiguity)
      (nlk:make-json-object
       "utc" (format nil "~aZ" (stamp-text instant))
       "source" (local-view zone source instant)
       "targets" (json-array (mapcar (lambda (target) (local-view (named-zone target) target instant)) targets))
       "ambiguous" (= count 2)
       "resolution" (if (= count 2) choice "unique")
       "database" (database-note)))))

(defun conversion-text (local source targets ambiguity)
  "The same conversion as one person's reading, for /penguin."
  (multiple-value-bind (instant zone count choice)
      (checked-instant local source targets ambiguity)
    (let* ((source-offset (zone-offset zone instant))
           (width (reduce #'max targets :key #'length :initial-value 0)))
      (with-output-to-string (out)
        (format out "~a ~a (~a) is ~aZ~%" local (nlk:trimmed source)
                (offset-label source-offset) (stamp-text instant))
        (dolist (target targets)
          (let* ((one (named-zone target))
                 (offset (zone-offset one instant)))
            (format out "  ~va  ~a (~a, ~a)~%" (+ 2 width) target
                    (readable-text (local-wall one instant))
                    (offset-label offset) (zone-abbreviation one instant))))
        (when (= count 2)
          (format out "this local time occurs twice; ~a read.~%" choice))))))

(defun overlap-action (request)
  "The penguin_overlap answer: every window where the whole meeting fits."
  (let* ((inline (penguin-overlap-team request))
         (id (penguin-overlap-team-id request))
         (crew (let ((has-team (not (null inline)))
                     (has-id (and (stringp id) (plusp (length (nlk:trimmed id))))))
                 (cond ((and has-team has-id)
                        (fail "pass either team or team_id, not both"))
                       (has-team (checked-team inline))
                       (has-id (saved-team (nlk:trimmed id)))
                       (t (fail "pass a team, or the team_id of one saved with penguin_save_team")))))
         (reference (nlk:trimmed (penguin-overlap-reference-zone request)))
         (reference-zone (named-zone reference))
         (first (parse-date (penguin-overlap-start-date request)))
         (days (or (penguin-overlap-days request) 1))
         (duration-minutes (penguin-overlap-duration-minutes request))
         (step (or (penguin-overlap-step-minutes request) 15))
         (limit (or (penguin-overlap-max-slots request) (setting :max-slots))))
    (unless (<= 1 days 14)
      (fail "days is 1 to 14, and ~d is not" days))
    (unless (<= 1 duration-minutes 480)
      (fail "duration_minutes is 1 to 480, and ~d is not" duration-minutes))
    (unless (<= 1 step 120)
      (fail "step_minutes is 1 to 120, and ~d is not" step))
    (unless (<= 1 limit 20)
      (fail "max_slots is 1 to 20, and ~d is not" limit))
    (let* ((duration (* 60 duration-minutes))
           (step-seconds (* 60 step))
           (members (crew-members crew))
           (threshold (* first +seconds-per-day+))
           (limit-day (+ first days))
           (stop (* limit-day +seconds-per-day+))
           (cursor (- threshold +seconds-per-day+))
           (runs '())
           (run-start nil))
      (loop while (< cursor stop)
            do (let* ((day (floor (local-wall reference-zone cursor) +seconds-per-day+))
                      (active (and (<= first day) (< day limit-day)
                                   (every (lambda (member)
                                            (working-p (local-wall (person-zone member) cursor)
                                                       (person-windows member)))
                                          members))))
                 (cond (active (unless run-start (setf run-start cursor)))
                       (run-start (push (cons run-start cursor) runs)
                                  (setf run-start nil)))
                 (incf cursor 60)))
      (when run-start (push (cons run-start cursor) runs))
      (setf runs (nreverse runs))
      (let ((slots '()) (total 0))
        (dolist (run runs)
          (let* ((length-seconds (- (cdr run) (car run)))
                 (count (max 0 (1+ (floor (- length-seconds duration) step-seconds)))))
            (incf total count)
            (loop for index from 0 below count
                  while (< (length slots) limit)
                  do (let ((candidate (+ (car run) (* index step-seconds))))
                       (push (nlk:copy-json-object
                              (interval-view candidate (+ candidate duration) crew)
                              "reference_start" (local-view reference-zone reference candidate))
                             slots)))))
        (setf slots (nreverse slots))
        (nlk:make-json-object
         "team_name" (crew-name crew)
         "reference_zone" reference
         "start_date" (date-text first)
         "end_date_exclusive" (date-text limit-day)
         "duration_minutes" duration-minutes
         "step_minutes" step
         "common_windows"
         (json-array
          (mapcar (lambda (run)
                    (nlk:copy-json-object
                     (interval-view (car run) (cdr run) crew)
                     "fits_duration" (>= (- (cdr run) (car run)) duration)))
                  (if (> (length runs) 128) (subseq runs 0 128) runs)))
         "windows_truncated" (> (length runs) 128)
         "slots" (json-array slots)
         "total_slots" total
         "slots_truncated" (> total (length slots))
         "ordering" "earliest UTC first; each offered start opens at a common window"
         "availability_policy"
         "wall-clock windows; both readings of a repeated hour count; a spring-forward gap holds no instant; an overnight window uses its start day; the end is exclusive"
         "database" (database-note))))))

(defun save-team-action (request)
  "The penguin_save_team answer: the team kept under its id."
  (let* ((id (checked-team-id (nlk:trimmed (penguin-save-team-team-id request))))
         (record (penguin-save-team-team request)))
    (checked-team record)
    (let ((table (read-teams))
          (encoded (nlk:contract-encode (nlk:contract 'penguin-team) record)))
      (setf (gethash id table) encoded)
      (write-teams table)
      (nlk:make-json-object
       "team_id" id
       "team" encoded
       "saved" t
       "database" (database-note)))))

(defun get-team-action (request)
  "The penguin_get_team answer: the team kept under its id."
  (let* ((id (checked-team-id (nlk:trimmed (penguin-get-team-team-id request))))
         (encoded (gethash id (read-teams))))
    (unless encoded
      (fail "no team is saved under ~s; save one with penguin_save_team first" id))
    (checked-team (nlk:contract-decode (nlk:contract 'penguin-team) encoded))
    (nlk:make-json-object "team_id" id "team" encoded "database" (database-note))))

;;; --- the tools --------------------------------------------------------------------

(defmacro guarded (&body body)
  "BODY's JSON text, or the JSON text of the refusal it raised: a tool answers
what to fix, and never throws at the model."
  `(handler-case (progn ,@body)
     (penguin-error (condition)
       (nlk:encode-json-object
        (nlk:make-json-object "ok" nil "error" (penguin-error-detail condition))))
     (error (condition)
       (nlk:encode-json-object
        (nlk:make-json-object "ok" nil
                               "error" (format nil "unexpected: ~a" condition))))))

(defun answer (result)
  "RESULT as the JSON text a tool answers."
  (nlk:encode-json-object (nlk:make-json-object "ok" t "result" result)))

(defun convert-tool (request)
  "penguin_convert."
  (guarded (answer (convert-action request))))

(defun overlap-tool (request)
  "penguin_overlap."
  (guarded (answer (overlap-action request))))

(defun save-team-tool (request)
  "penguin_save_team."
  (guarded (answer (save-team-action request))))

(defun get-team-tool (request)
  "penguin_get_team."
  (guarded (answer (get-team-action request))))

;;; --- /penguin ---------------------------------------------------------------------

(defun run-penguin (args session-id)
  "The /penguin command: /penguin 2026-11-08T14:30 Asia/Taipei Europe/Berlin."
  (declare (ignore session-id))
  (handler-case
      (let ((words (remove "" (uiop:split-string (or args "") :separator " ")
                           :test #'string=)))
        (unless (>= (length words) 3)
          (fail "use /penguin YYYY-MM-DDTHH:MM SOURCE_ZONE TARGET_ZONE ..."))
        (conversion-text (first words) (second words) (cddr words) "earlier"))
    (penguin-error (condition) (penguin-error-detail condition))))

;;; --- the manual -------------------------------------------------------------------

(defparameter +manual+
  (format nil "~{~a~^~%~}"
          '("penguin: a time across zones, and the windows where a whole team's meeting fits."
            ""
            "The four tools take an explicit local time or a whole team, and never assume now:"
            "  penguin_convert     one local date and time in one IANA zone, read in up to 30 others."
            "                      A time the clocks skipped is refused; a time that occurs twice is"
            "                      refused unless the call says earlier or later, so ask the user."
            "  penguin_overlap     the windows where a meeting of a given length fits inside every"
            "                      member's working hours, from a team object or a saved team_id."
            "  penguin_save_team   keep a team under a short id, when the user asks to remember it."
            "  penguin_get_team    read a saved team back by its id."
            ""
            "A member is (name, zone, windows); a window is start and end as HH:MM in that"
            "member's own zone and the weekdays it covers, Monday 0 through Sunday 6. An end"
            "before its start is a shift that runs overnight on the start day."
            "Typed at the prompt, /penguin 2026-11-08T14:30 Asia/Taipei Europe/Berlin reads one"
            "time in every zone named after the source."
            "Zones come from this machine's tz database, under the zoneinfo_dir setting."
            "Nothing leaves the machine, and no calendar event is created."))
  "What (help :penguin) answers.")

;;; --- the cell ---------------------------------------------------------------------

(defun checked-settings (values table)
  "VALUES with the tz database folder checked, so a missing one is a refusal at
start rather than a wrong hour later."
  (declare (ignore table))
  (let ((dir (nlk:trimmed (or (getf values :zoneinfo-dir) ""))))
    (when (zerop (length dir))
      (nlk:config-error "penguin.zoneinfo_dir is empty; this cell reads zone names out of a tz database, and Linux and macOS keep one at /usr/share/zoneinfo"))
    (unless (uiop:directory-exists-p (uiop:ensure-directory-pathname dir))
      (nlk:config-error "penguin.zoneinfo_dir is ~s and there is no such folder; point it at a tz database such as /usr/share/zoneinfo" dir))
    (setf (getf values :zoneinfo-dir) dir)
    (let ((slots (getf values :max-slots)))
      (unless (and (integerp slots) (<= 1 slots 20))
        (nlk:config-error "penguin.max_slots is ~s; a search offers 1 to 20 windows" slots)))
    values))

(nle:define-cell penguin
  (:section ("penguin")
    (:guide "zoneinfo_dir is the folder this machine keeps its tz database in (Linux and macOS: /usr/share/zoneinfo, which the default already names); max_slots is how many meeting windows a search offers when the call does not say")
    ("zoneinfo_dir" :string :default "/usr/share/zoneinfo"
     :doc "the tz database folder zone names are read from")
    ("max_slots" :integer :default 5 :min 1
     :doc "how many meeting windows a search offers, 1 to 20"))
  (:settings #'checked-settings)
  (:tool "penguin_convert" (nlk:contract 'penguin-convert) 'convert-tool)
  (:tool "penguin_overlap" (nlk:contract 'penguin-overlap) 'overlap-tool)
  (:tool "penguin_save_team" (nlk:contract 'penguin-save-team) 'save-team-tool)
  (:tool "penguin_get_team" (nlk:contract 'penguin-get-team) 'get-team-tool)
  (:command "penguin" 'run-penguin
            :description "Convert one time across zones"
            :argument-hint "LOCAL_TIME ZONE ZONE ...")
  (:help :penguin
         "penguin: a local time converted across IANA zones, and the windows where a whole team's meeting fits"
         +manual+))
