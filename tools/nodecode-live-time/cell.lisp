;;;; cell.lisp --- the real current time in every turn's standing context.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A session stamped its start once and never again: leave one open across a
;;;; day and the model still answers as though it were that same afternoon.
;;;; This cell puts the clock in front of it at every turn, in the standing
;;;; harness section the engine reads at the first request of a turn
;;;; (nle::read-harness-sections) -- context the model reads and the transcript
;;;; never keeps, so no tool call and no guessing is needed. A turn that runs
;;;; long reads the clock again through the current_time tool.
;;;;
;;;; Nothing leaves the machine: no request, no subprocess, no file of the
;;;; operator's. The only file read is a zone of the machine's own tz database
;;;; (<zoneinfo_dir>/<name>), the same bytes TZ=Europe/Berlin reads -- a
;;;; version 2 TZif file, its transitions decoded here rather than shelled out
;;;; to date(1) or resolved through a library the cell would have to carry.

(defpackage #:nodecode-live-time
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:live-time-error #:live-time-error-detail #:*live-time*))

(in-package #:nodecode-live-time)

(nlk:define-peripheral live-time :not-running t)

(defparameter +section-name+ "live-time"
  "The standing section's name: what a request shows above the block.")

(defparameter +zoneinfo+ "/usr/share/zoneinfo"
  "Where a zone name is looked up unless the section says otherwise.")

(defparameter +weekdays+ #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "English labels, so the block reads the same whatever language the rest of
the conversation is in.")

(defvar *clock* nil
  "NIL reads the machine's clock; a universal time pins it, which is how the
tests read a fixed instant.")

(defun clock-now ()
  "The universal time this cell reads as now."
  (or *clock* (get-universal-time)))

;;; --- a zone of the tz database ---------------------------------------------------
;;; RFC 8536 in the little of it a clock needs: the transition times, which type
;;; each one switches to, and each type's offset east of UTC. A version 1 file
;;; has one block with 4-byte times; version 2 and later keep that block for old
;;; readers and follow it with a header and a block of 8-byte times, which is
;;; the one read here.

(defstruct (zone-table (:constructor make-zone-table (transitions indices types)))
  "TRANSITIONS: universal times, ascending. INDICES: each transition's type.
TYPES: (offset-seconds-east . daylight-p) per type."
  (transitions #() :read-only t)
  (indices #() :read-only t)
  (types #() :read-only t))

(defun read-octets (path)
  "Every byte of PATH."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((data (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence data in)
      data)))

(defun be-integer (data start size)
  "The big-endian unsigned integer of SIZE bytes at START in DATA."
  (let ((value 0))
    (dotimes (index size)
      (setf value (+ (ash value 8) (aref data (+ start index)))))
    value))

(defun be-signed (data start size)
  "The big-endian two's-complement integer of SIZE bytes at START in DATA."
  (let ((value (be-integer data start size)))
    (if (logbitp (1- (* 8 size)) value)
        (- value (ash 1 (* 8 size)))
        value)))

(defun tzif-counts (data header)
  "The six counts of the TZif header at HEADER: how many UT/local indicators,
standard/wall indicators, leap seconds, transitions, types and abbreviation
bytes."
  (values (be-integer data (+ header 20) 4)
          (be-integer data (+ header 24) 4)
          (be-integer data (+ header 28) 4)
          (be-integer data (+ header 32) 4)
          (be-integer data (+ header 36) 4)
          (be-integer data (+ header 40) 4)))

(defun tzif-block-length (data header time-size)
  "How many bytes the block under the header at HEADER takes, in TIME-SIZE-byte
times."
  (multiple-value-bind (utcnt stdcnt leapcnt timecnt typecnt charcnt)
      (tzif-counts data header)
    (+ (* timecnt time-size) timecnt (* typecnt 6) charcnt
       (* leapcnt (+ time-size 4)) utcnt stdcnt)))

(defun parse-zone-block (data header time-size)
  "The zone table under the header at HEADER, its transitions in TIME-SIZE bytes."
  (multiple-value-bind (utcnt stdcnt leapcnt timecnt typecnt charcnt)
      (tzif-counts data header)
    (declare (ignore utcnt stdcnt leapcnt charcnt))
    (let ((at (+ header 44))
          (transitions (make-array timecnt))
          (indices (make-array timecnt))
          (types (make-array typecnt)))
      (dotimes (index timecnt)
        (setf (aref transitions index) (be-signed data at time-size))
        (incf at time-size))
      (dotimes (index timecnt)
        (setf (aref indices index) (aref data at))
        (incf at))
      (dotimes (index typecnt)
        (setf (aref types index)
              (cons (be-signed data at 4) (plusp (aref data (+ at 4)))))
        (incf at 6))
      (make-zone-table transitions indices types))))

(defun read-zone-table (path)
  "PATH as a ZONE-TABLE. Signals the cell's condition on anything else."
  (let ((data (read-octets path)))
    (unless (and (>= (length data) 44)
                 (equalp (subseq data 0 4) #(84 90 105 102))) ; "TZif"
      (fail "~a is not a zone file of the tz database" (namestring path)))
    (if (member (aref data 4) '(0 48))                    ; NUL or "0": version 1
        (parse-zone-block data 0 4)
        (let ((second (+ 44 (tzif-block-length data 0 4))))
          (when (> (+ second 44) (length data))
            (fail "~a ends before the 64-bit block it announces" (namestring path)))
          (parse-zone-block data second 8)))))

(defun type-offset (type)
  "TYPE's seconds east of UTC."
  (if type (car type) 0))

(defun zone-offset (table time)
  "TABLE's seconds east of UTC at universal TIME: the type of the last
transition at or before TIME, the standard type before the first one."
  (let ((transitions (zone-table-transitions table))
        (indices (zone-table-indices table))
        (types (zone-table-types table)))
    (cond ((zerop (length types)) 0)
          ((zerop (length transitions)) (type-offset (aref types 0)))
          (t (let ((index (1- (length transitions))))
               (loop while (and (>= index 0) (> (aref transitions index) time))
                     do (decf index))
               (if (minusp index)
                   (let ((standard (position-if-not #'cdr types)))
                     (type-offset (aref types (or standard 0))))
                   (type-offset (aref types (aref indices index)))))))))

;;; --- naming a zone ---------------------------------------------------------------

(defun zone-directory (dir)
  "DIR as the tz database's root, or the machine's own when it is empty."
  (if (and dir (plusp (length dir))) dir +zoneinfo+))

(defun zone-file (name dir)
  "The file NAME names under the tz database at DIR."
  (merge-pathnames name (uiop:ensure-directory-pathname (zone-directory dir))))

(defun zone-name-p (name)
  "Whether NAME is shaped like an IANA zone name: letters, digits and / _ - +
between, no empty part, no `..' to walk out of the database."
  (and (stringp name)
       (plusp (length name))
       (<= (length name) 64)
       (not (search ".." name))
       (not (char= #\/ (char name 0)))
       (not (char= #\/ (char name (1- (length name)))))
       (every (lambda (char) (or (alphanumericp char) (find char "/_-+"))) name)))

(defun checked-zone-file (name dir)
  "The zone file NAME names under DIR, or the cell's condition saying which
part of it is wrong."
  (unless (zone-name-p name)
    (fail "~s is not a zone name; they look like Europe/Berlin or Asia/Tokyo, and an empty timezone reads this machine's own"
          name))
  (let ((path (zone-file name dir)))
    (unless (probe-file path)
      (fail "there is no zone ~s in the tz database at ~a; names look like Europe/Berlin or Asia/Tokyo, and an empty timezone reads this machine's own"
            name (zone-directory dir)))
    path))

;;; --- the reading, in words the model can use --------------------------------------

(defun offset-label (seconds)
  "SECONDS east of UTC as `UTC+01:00'."
  (multiple-value-bind (hours rest) (floor (abs seconds) 3600)
    (format nil "UTC~a~2,'0d:~2,'0d" (if (minusp seconds) "-" "+") hours (floor rest 60))))

(defun stamp (time offset)
  "TIME shifted by OFFSET as `2026-11-08 14:32:05', and its weekday as
(values STAMP NUMBER NAME)."
  (multiple-value-bind (sec min hour date month year day)
      (decode-universal-time (+ time offset) 0)
    (values (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                    year month date hour min sec)
            (1+ day)
            (aref +weekdays+ day))))

(defun local-offset (time)
  "TIME's seconds east of UTC on this machine, daylight saving included."
  (multiple-value-bind (sec min hour date month year day daylight-p zone)
      (decode-universal-time time)
    (declare (ignore sec min hour date month year day))
    (* 3600 (- (if daylight-p 1 0) zone))))

(defun clock-of (time zone dir)
  "TIME as this cell reads it: in ZONE (an IANA name looked up under the tz
database at DIR) when one is named, else on this machine.
=> (values STAMP WEEKDAY NUMBER NAME LABEL): `2026-11-08 14:32:05', `Sat', 6,
`Europe/Berlin' or NIL for the machine's own, `UTC+01:00'."
  (let* ((named (and zone (plusp (length zone)) zone))
         (offset (if named
                     (zone-offset (read-zone-table (checked-zone-file named dir)) time)
                     (local-offset time))))
    (multiple-value-bind (stamp number name) (stamp time offset)
      (values stamp name number named (offset-label offset)))))

(defun reading-text (time zone dir)
  "TIME as the block the model reads, claiming what it is for and saying which
instant it was read at."
  (multiple-value-bind (stamp weekday number named label) (clock-of time zone dir)
    (format nil "[LIVE-TIME] Now: ~a (Weekday ~d/7, ~a), TZ ~a, ~a. ~
Read at this turn's first request by the live-time cell: this is the authoritative ~
current time for every today/now/elapsed/date judgment, and an earlier ~
session-start stamp is history. current_time reads the clock again mid-turn."
            stamp number weekday (or named "local") label)))

(defun standing-section ()
  "The (\"live-time\" . TEXT) standing section for the turn being read: the
machine's own zone when the configured one cannot be read, so a moved tz
database costs a fallback rather than the turn."
  (cons +section-name+
        (handler-case (reading-text (clock-now) (setting :timezone) (setting :zoneinfo-dir))
          (error () (reading-text (clock-now) nil nil)))))

(defun inject (next session-id)
  "NLE::READ-HARNESS-SECTIONS advice: the live time first, the rest as they were,
and never two of this section."
  (cons (standing-section)
        (remove +section-name+ (funcall next session-id) :key #'car :test #'string=)))

;;; --- the tool: one call, one reading ---------------------------------------------

(nlk:define-contract time-request
  (:doc "The current date and time, read at this moment: the wall clock with its UTC offset and weekday. Use it when a turn runs long enough for the time to have moved since the turn started, or for a zone other than the configured one, instead of trusting any timestamp already in the conversation.")
  ("timezone" :text
              :doc "an IANA zone name such as Asia/Tokyo or America/New_York; omitted, the configured zone or this machine's own"))

(defun answer-time (request)
  "The current_time tool. A refusal is answered, never signalled."
  (handler-case
      (let* ((asked (nlk:trimmed (or (time-request-timezone request) "")))
             (zone (if (plusp (length asked)) asked (setting :timezone))))
        (reading-text (clock-now) zone (setting :zoneinfo-dir)))
    (live-time-error (condition)
      (nle:failure (format nil "ERROR: ~a" (live-time-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

;;; --- /time -------------------------------------------------------------------------

(defun run-time (args session-id)
  "/time => now, here or in the zone ARGS names."
  (declare (ignore session-id))
  (let ((asked (nlk:trimmed (or args ""))))
    (handler-case
        (let ((zone (if (plusp (length asked)) asked (setting :timezone))))
          (multiple-value-bind (stamp weekday number named label)
              (clock-of (clock-now) zone (setting :zoneinfo-dir))
            (declare (ignore number))
            (format nil "Now: ~a, ~a~@[, ~a~], ~a" stamp weekday named label)))
      (live-time-error (condition) (live-time-error-detail condition))
      (error (condition) (format nil "~a" condition)))))

;;; --- what the manual says ----------------------------------------------------------

(defparameter +manual+
  "The cell puts the real current time at the head of every turn's context:
a [LIVE-TIME] line with the date, the clock, the weekday, the zone and its
UTC offset, read at that turn's first request. Treat it as the authoritative
now for anything about today, now, elapsed time or a date -- and treat any
session-start stamp as history, which is the whole point: a session left open
overnight must not answer as though it were still the same afternoon.

The clock is read again while a turn runs through the current_time tool (an
optional IANA timezone argument), and /time answers the same reading to the
operator, with a zone typed after it when one is wanted. With no timezone
setting the cell reads this machine's own zone, daylight saving included; set
timezone to an IANA name (Europe/Berlin, Asia/Tokyo) to read another, and
zoneinfo_dir only when the machine keeps its tz database somewhere unusual.
An unknown zone name refuses at start rather than answering a wrong hour."
  "What (help :live-time) answers while the cell runs.")

;;; --- the cell -----------------------------------------------------------------------

(defun checked-settings (values table)
  "The section's values with the timezone checked: a zone the database does not
hold is a CONFIG-REFUSAL at start, never a wrong hour later."
  (declare (ignore table))
  (let ((zone (nlk:trimmed (or (getf values :timezone) "")))
        (dir (zone-directory (nlk:trimmed (or (getf values :zoneinfo-dir) "")))))
    (setf (getf values :timezone) zone
          (getf values :zoneinfo-dir) dir)
    (when (plusp (length zone))
      (unless (zone-name-p zone)
        (nlk:config-error "live-time.timezone is ~s; a zone name looks like Europe/Berlin or Asia/Tokyo, and an empty timezone reads this machine's own"
                          zone))
      (let ((path (zone-file zone dir)))
        (unless (probe-file path)
          (nlk:config-error "live-time.timezone is ~s and the tz database at ~a has no such zone; name an IANA zone, or leave timezone empty for this machine's own"
                            zone dir))
        (handler-case (read-zone-table path)
          (error (condition)
            (nlk:config-error "live-time.timezone is ~s and its zone file could not be read: ~a"
                              zone condition)))))
    values))

(nle:define-cell live-time
  (:section ("live-time")
    (:guide "nothing here is required: an empty timezone reads this machine's own zone; name an IANA zone such as Europe/Berlin to read the clock there; zoneinfo_dir is only read when timezone names a zone")
    ("timezone" :string :default ""
                :doc "an IANA zone name such as Europe/Berlin or Asia/Tokyo; empty reads this machine's own zone, daylight saving included")
    ("zoneinfo_dir" :string :default "/usr/share/zoneinfo"
                     :doc "where this machine's tz database lives, the folder zone names are looked up under"))
  (:settings #'checked-settings)
  (:hook 'nle::read-harness-sections #'inject)
  (:tool "current_time" (nlk:contract 'time-request) 'answer-time)
  (:command "time" 'run-time
            :description "What time it is now, here or in a zone you name")
  (:help :live-time
         "live-time: every turn's context carries the real current time, so today and now never follow a stale session stamp"
         +manual+))
