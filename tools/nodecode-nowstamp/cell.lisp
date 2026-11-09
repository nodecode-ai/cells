;;;; cell.lisp --- the real clock, and the real gap since the last message.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A session is stamped with the instant it started and never again: leave one
;;;; open across a day and the model still answers as though it were that
;;;; afternoon, and nothing in the history it was sent says when any message
;;;; arrived, so "yesterday" is a guess. This cell puts two facts in front of
;;;; the model at every turn, in the standing harness section the engine reads at
;;;; a turn's first request (NLE::READ-HARNESS-SECTIONS): the clock, read then,
;;;; and how long ago this conversation last moved. A turn that runs long keeps
;;;; its clock fresh through the :TOOL point, which is off until the operator
;;;; asks for it.
;;;;
;;;; Nothing leaves the machine: no request, no subprocess. The files it reads
;;;; are a zone of the machine's own tz database (<zoneinfo_dir>/<name>) and the
;;;; small table of turn instants it keeps under this home (nowstamp/state.tsv),
;;;; which is how the gap survives a restart.

(defpackage #:nodecode-nowstamp
  (:use #:cl)
  (:export #:start-cell #:stop-cell #:setting
           #:nowstamp-error #:nowstamp-error-detail #:*nowstamp*))

(in-package #:nodecode-nowstamp)

(nlk:define-peripheral nowstamp :not-running t)

(defparameter +section-name+ "nowstamp"
  "The standing section's name: what a request shows above the block.")

(defparameter +zoneinfo+ "/usr/share/zoneinfo"
  "Where a zone name is looked up unless the section says otherwise.")

(defparameter +max-sessions+ 200
  "How many conversations' last instants the table keeps, newest kept, oldest
dropped -- the plugin's own cap, so the file cannot grow with use.")

(defparameter +weekdays+
  '(("en" . #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
    ("id" . #("Senin" "Selasa" "Rabu" "Kamis" "Jumat" "Sabtu" "Minggu")))
  "Weekday names per language, Monday first. The short form is the first three
letters, which is the abbreviation both languages use.")

(defparameter +default-format+ "%a %Y-%m-%d %H:%M %Z"
  "The stamp's shape. The plugin's strftime format, in the tokens this cell
carries: %a %A %Y %m %d %e %H %M %S %Z %z %%.")

(defvar *clock* nil
  "NIL reads the machine's clock; a universal time pins it, which is how the
tests read a fixed instant.")

(defvar *last-seen* (make-hash-table :test #'equal)
  "Session id -> the universal time this cell last saw that conversation move.
Loaded from the state file at start and written back after every turn.")

(defvar *last-shown* nil
  "The universal time the model was last given, for the tool-result stamp's
interval. In memory alone: an interval is about one turn, not one process.")

(defun clock-now ()
  "The universal time this cell reads as now."
  (or *clock* (get-universal-time)))

;;; --- the table of last instants ------------------------------------------------

(defun state-path ()
  "The file the table is kept in, under this home."
  (nlk:home "nowstamp/state.tsv"))

(defun printable-session-p (id)
  "Whether ID can stand in a line-oriented file without tearing it."
  (and (stringp id) (plusp (length id))
       (not (find #\Tab id)) (not (find #\Newline id))
       (not (find #\Return id))))

(defun load-state ()
  "Fill the table from the state file: one `UNIVERSAL-TIME<TAB>SESSION-ID` a
line. A file that is not there, or a line that is not one, is skipped."
  (let ((path (state-path)))
    (when (probe-file path)
      (handler-case
          (with-open-file (in path :direction :input :if-does-not-exist nil)
            (loop for line = (and in (read-line in nil nil))
                  while line
                  do (let ((tab (position #\Tab line)))
                       (when (and tab (plusp tab))
                         (let ((time (ignore-errors (parse-integer line :end tab :junk-allowed t)))
                               (id (subseq line (1+ tab))))
                           (when (and time (printable-session-p id))
                             (setf (gethash id *last-seen*) time)))))))
        (error () nil)))))

(defvar *state-lock* (sb-thread:make-mutex :name "nowstamp state")
  "Serializes writes of the state file across the threads that turn on.")

(defun save-state ()
  "Write the table back through a temp and one rename. A failure is not the
turn's: the gap is a courtesy, so the file going unwritten costs a restart's
memory of it, not the reading."
  (handler-case
      (sb-thread:with-mutex (*state-lock*)
        (nlk:write-file-atomically
         (state-path)
         (with-output-to-string (out)
           (maphash (lambda (id time)
                      (when (and (integerp time) (printable-session-p id))
                        (format out "~d~c~a~%" time #\Tab id)))
                    *last-seen*))))
    (error () nil)))

(defun trim-sessions ()
  "Drop the oldest entries past +MAX-SESSIONS+."
  (loop while (> (hash-table-count *last-seen*) +max-sessions+)
        do (let ((oldest nil) (oldest-time nil))
             (maphash (lambda (id time)
                        (when (or (null oldest-time) (< time oldest-time))
                          (setf oldest id oldest-time time)))
                      *last-seen*)
             (if oldest (remhash oldest *last-seen*) (return)))))

(defun remember (session-id time)
  "Record that SESSION-ID's conversation moved at TIME."
  (when (printable-session-p session-id)
    (setf (gethash session-id *last-seen*) time)
    (trim-sessions)))

(defun previous-instant (session-id)
  "When this cell last saw SESSION-ID's conversation move, or NIL when it never
has -- the first turn of a session, or the first after a restart that wrote
nothing."
  (and (printable-session-p session-id) (gethash session-id *last-seen*)))

;;; --- a zone of the tz database ------------------------------------------------
;;; RFC 8536 in the little of it a clock needs: the transition times, the type
;;; each switches to, and each type's offset east of UTC. A version 1 file has
;;; one block of 4-byte times; version 2 and later follow it with a header and a
;;; block of 8-byte times, which is the one read here.

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
  "The six counts of the TZif header at HEADER: UT/local indicators,
standard/wall indicators, leap seconds, transitions, types, abbreviation
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
  "The zone table under the header at HEADER, its transitions in TIME-SIZE
bytes."
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

;;; --- the stamp, in the words the model reads -----------------------------------

(defun offset-label (seconds)
  "SECONDS east of UTC as `UTC+01:00'."
  (multiple-value-bind (hours rest) (floor (abs seconds) 3600)
    (format nil "UTC~a~2,'0d:~2,'0d" (if (minusp seconds) "-" "+") hours (floor rest 60))))

(defun numeric-offset (seconds)
  "SECONDS east of UTC as `+0100'."
  (multiple-value-bind (hours rest) (floor (abs seconds) 3600)
    (format nil "~a~2,'0d~2,'0d" (if (minusp seconds) "-" "+") hours (floor rest 60))))

(defun local-offset (time)
  "TIME's seconds east of UTC on this machine, daylight saving included."
  (multiple-value-bind (sec min hour date month year day daylight-p zone)
      (decode-universal-time time)
    (declare (ignore sec min hour date month year day))
    (* 3600 (- (if daylight-p 1 0) zone))))

(defun weekday-names (language)
  "The seven weekday names of LANGUAGE, Monday first."
  (or (cdr (assoc language +weekdays+ :test #'string=))
      (cdr (assoc "en" +weekdays+ :test #'string=))))

(defun format-stamp (time offset language format)
  "TIME shifted by OFFSET, written through FORMAT's tokens."
  (multiple-value-bind (sec min hour date month year day)
      (decode-universal-time (+ time offset) 0)
    (let ((name (aref (weekday-names language) day))
          (out (make-string-output-stream))
          (index 0)
          (length (length format)))
      (loop while (< index length)
            do (let ((char (char format index)))
                 (if (and (char= char #\%) (< (1+ index) length))
                     (let ((token (char format (1+ index))))
                       (case token
                         (#\a (write-string (subseq name 0 (min 3 (length name))) out))
                         (#\A (write-string name out))
                         (#\Y (format out "~4,'0d" year))
                         (#\m (format out "~2,'0d" month))
                         (#\d (format out "~2,'0d" date))
                         (#\e (format out "~2d" date))
                         (#\H (format out "~2,'0d" hour))
                         (#\M (format out "~2,'0d" min))
                         (#\S (format out "~2,'0d" sec))
                         (#\Z (write-string (offset-label offset) out))
                         (#\z (write-string (numeric-offset offset) out))
                         (#\% (write-char #\% out))
                         (t (write-char #\% out) (write-char token out)))
                       (incf index 2))
                     (progn (write-char char out) (incf index)))))
      (get-output-stream-string out))))

(defun ago-text (seconds)
  "SECONDS as `5 min ago', `2 h 3 min ago', `3 d 4 h ago'."
  (let ((s (max 0 (round seconds))))
    (cond ((< s 60) "just now")
          ((< s 3600) (format nil "~d min ago" (floor s 60)))
          ((< s 86400) (multiple-value-bind (hours minutes) (floor (floor s 60) 60)
                         (if (plusp minutes)
                             (format nil "~d h ~d min ago" hours minutes)
                             (format nil "~d h ago" hours))))
          (t (multiple-value-bind (days hours) (floor (floor s 3600) 24)
               (if (plusp hours)
                   (format nil "~d d ~d h ago" days hours)
                   (format nil "~d d ago" days)))))))

(defun date-key (time offset)
  "TIME as OFFSET shows it, as (YEAR MONTH DAY HOUR MINUTE), the civil reading
calendar arithmetic and a printed date both start from."
  (multiple-value-bind (sec min hour date month year day)
      (decode-universal-time (+ time offset) 0)
    (declare (ignore sec day))
    (list year month date hour min)))

(defun days-between (earlier later)
  "How many calendar days EARLIER is before LATER, both DATE-KEY readings: the
same day is 0 even when one is 23:50 and the other 00:10, which is the point --
the plugin calls a calendar date, not a count of hours."
  (round (- (encode-universal-time 0 0 12 (third later) (second later) (first later))
            (encode-universal-time 0 0 12 (third earlier) (second earlier) (first earlier)))
         86400))

(defun when-text (now previous offset)
  "PREVIOUS in words: its clock time, or its date when it is not a day this
week's reading can name."
  (destructuring-bind (year month day hour minute) (date-key previous offset)
    (case (days-between (date-key previous offset) (date-key now offset))
      (0 (format nil "today ~2,'0d:~2,'0d" hour minute))
      (1 (format nil "yesterday ~2,'0d:~2,'0d" hour minute))
      (otherwise (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
                 year month day hour minute)))))

(defun offset-of (time)
  "TIME's seconds east of UTC as this cell reads them: the configured zone when
one is set, else this machine's own, daylight saving included."
  (let ((zone (setting :timezone)))
    (if (and zone (plusp (length zone)))
        (zone-offset (read-zone-table (checked-zone-file zone (setting :zoneinfo-dir))) time)
        (local-offset time))))

;;; --- the reading, and the gap --------------------------------------------------

(defun reading-text (time session-id &key fallback-p)
  "TIME as the block the model reads: the clock, and -- on any turn but a
session's first -- how long ago the conversation last moved. FALLBACK-P is the
machine's own zone after a configured one could not be read."
  (let* ((offset (if fallback-p (local-offset time) (offset-of time)))
         (stamp (format-stamp time offset (setting :language) (setting :format)))
         (previous (and (setting :show-elapsed) (previous-instant session-id))))
    (with-output-to-string (out)
      (format out "[NOWSTAMP] Now: ~a" stamp)
      (when previous
        (let ((gap (- time previous)))
          (format out " | previous message: ~a (~a)"
                  (ago-text gap)
                  (when-text time previous offset))))
      (write-string ". Read at this turn's first request: this is the clock for every now, today or how-long-ago judgment, and the gap is the real one -- an older timestamp in the conversation is history" out)
      (when (and (setting :stamp-tool-results) (setting :show-elapsed))
        (write-string "; tool results carry the current time when the model has not seen it for a while" out))
      (write-string "." out))))

(defun standing-section (session-id)
  "The (\"nowstamp\" . TEXT) standing section for the turn being read, and the
record of this turn's instant."
  (let ((time (clock-now)))
    (prog1 (cons +section-name+
                 (handler-case (reading-text time session-id)
                   (error () (reading-text time session-id :fallback-p t))))
      (setf *last-shown* time)
      (remember session-id time)
      (save-state))))

(defun inject (next session-id)
  "NLE::READ-HARNESS-SECTIONS advice: the reading first, the rest as they were,
and never two of this section."
  (cons (standing-section session-id)
        (remove +section-name+ (funcall next session-id) :key #'car :test #'string=)))

;;; --- the :TOOL point: a fresh clock inside a long turn -------------------------

(defun stamp-due-p (time)
  "Whether the model may be told the time again: TIME is more than the interval
past the last reading it was given, or nothing was ever given."
  (let ((interval (max 0 (or (setting :stamp-interval-seconds) 60))))
    (or (null *last-shown*)
        (>= (- time *last-shown*) interval))))

(defun stamp-tool-result (op next)
  "The :TOOL point hook: NEXT's text with the current time on a trailing line,
when the operator asked for stamping and the interval has passed. OP is
(:name TOOL-NAME :arguments HASH :call-id ID)."
  (let ((text (funcall next op)))
    (if (not (stringp text))
        text
        (let ((time (clock-now)))
          (if (not (stamp-due-p time))
              text
              (progn
                (setf *last-shown* time)
                (format nil "~a~%[Current time: ~a]"
                        text
                        (format-stamp time (offset-of time)
                                      (setting :language) (setting :format)))))))))

;;; --- one reading on demand: the tool and /now ----------------------------------

(nlk:define-contract now-request
  (:doc "The current date and time, read at this moment, with its UTC offset and weekday. Use it when a turn runs long enough for the clock to have moved since the turn started, or for a zone other than the configured one, instead of trusting a timestamp already in the conversation.")
  ("timezone" :text
              :doc "an IANA zone name such as Asia/Tokyo or America/New_York; omitted, the configured zone or this machine's own"))

(defun answer-now (request)
  "The now tool. A refusal is answered, never signalled."
  (handler-case
      (let* ((asked (nlk:trimmed (or (now-request-timezone request) "")))
             (zone (if (plusp (length asked)) asked (setting :timezone)))
             (time (clock-now))
             (offset (if (and zone (plusp (length zone)))
                         (zone-offset (read-zone-table (checked-zone-file zone (setting :zoneinfo-dir)))
                                      time)
                         (local-offset time))))
        (format nil "Now: ~a"
                (format-stamp time offset (setting :language) (setting :format))))
    (nowstamp-error (condition)
      (nle:failure (format nil "ERROR: ~a" (nowstamp-error-detail condition))))
    (error (condition)
      (nle:failure (format nil "ERROR: ~a" condition)))))

(defun run-now (args session-id)
  "/now => the clock here, and the gap since this conversation last moved."
  (let ((asked (nlk:trimmed (or args ""))))
    (handler-case
        (let* ((zone (if (plusp (length asked)) asked (setting :timezone)))
               (time (clock-now))
               (offset (if (and zone (plusp (length zone)))
                           (zone-offset (read-zone-table (checked-zone-file zone (setting :zoneinfo-dir)))
                                        time)
                           (local-offset time)))
               (previous (and (setting :show-elapsed) (previous-instant session-id))))
          (format nil "Now: ~a~@[ | previous message: ~a (~a)~]~@[, ~a~]"
                  (format-stamp time offset (setting :language) (setting :format))
                  (and previous (ago-text (- time previous)))
                  (and previous (when-text time previous offset))
                  (and zone (plusp (length zone)) zone)))
      (nowstamp-error (condition) (nowstamp-error-detail condition))
      (error (condition) (format nil "~a" condition)))))

;;; --- what the manual says ----------------------------------------------------------

(defparameter +manual+
  "The cell puts the clock at the head of every turn's standing context: a
[NOWSTAMP] line with the date and time in the configured zone, its UTC offset,
and -- on every turn but a session's first -- how long ago this conversation
last moved, in words (`5 min ago (today 19:37)'). Treat it as the
authoritative now for anything about today, now, elapsed time or a date: an
earlier session-start stamp, and any time word already in the conversation, is
history.

The gap is what a stale stamp gets wrong: five minutes and a day both read as
`as we discussed', so the line says which. It survives a restart because the
instants live in a small table under this home (nowstamp/state.tsv); a session
this cell has never seen gets no gap on its first turn.

now reads the same clock on demand, and /now answers it to the operator, with
an IANA zone typed after it for another zone. stamp_tool_results, off by
default, also puts a fresh [Current time: ...] line on a tool result once
stamp_interval_seconds have passed since the model was last given the time, so
a turn that runs long does not drift. With no timezone setting the cell reads
this machine's own zone, daylight saving included; an unknown zone refuses at
start rather than answering a wrong hour."
  "What (help :nowstamp) answers while the cell runs.")

;;; --- the cell -----------------------------------------------------------------------

(defun checked-settings (values table)
  "The section's values with the zone checked and the format proved: a zone the
database does not hold is a CONFIG-REFUSAL at start, never a wrong hour later."
  (declare (ignore table))
  (let ((zone (nlk:trimmed (or (getf values :timezone) "")))
        (dir (zone-directory (nlk:trimmed (or (getf values :zoneinfo-dir) ""))))
        (format (or (getf values :format) +default-format+))
        (language (or (getf values :language) "en")))
    (setf (getf values :timezone) zone
          (getf values :zoneinfo-dir) dir
          (getf values :format) format
          (getf values :language) language)
    (unless (plusp (length format))
      (nlk:config-error "nowstamp.format is empty; write the stamp's shape, for example ~s"
                        +default-format+))
    (handler-case (format-stamp (clock-now) 0 language format)
      (error (condition)
        (nlk:config-error "nowstamp.format is ~s and cannot be read: ~a" format condition)))
    (when (plusp (length zone))
      (unless (zone-name-p zone)
        (nlk:config-error "nowstamp.timezone is ~s; a zone name looks like Europe/Berlin or Asia/Tokyo, and an empty timezone reads this machine's own"
                          zone))
      (let ((path (zone-file zone dir)))
        (unless (probe-file path)
          (nlk:config-error "nowstamp.timezone is ~s and the tz database at ~a has no such zone; name an IANA zone, or leave timezone empty for this machine's own"
                            zone dir))
        (handler-case (read-zone-table path)
          (error (condition)
            (nlk:config-error "nowstamp.timezone is ~s and its zone file could not be read: ~a"
                              zone condition)))))
    values))

(nle:define-cell nowstamp
  (:section ("nowstamp")
    (:guide "nothing here is required: an empty timezone reads this machine's own zone; name an IANA zone such as Europe/Berlin to read the clock there; stamp_tool_results is off until you ask for it")
    ("timezone" :string :default ""
                :doc "an IANA zone name such as Europe/Berlin or Asia/Tokyo; empty reads this machine's own zone, daylight saving included")
    ("zoneinfo_dir" :string :default "/usr/share/zoneinfo"
                     :doc "where this machine's tz database lives, the folder zone names are looked up under")
    ("format" :string :default "%a %Y-%m-%d %H:%M %Z"
              :doc "the stamp's shape in strftime tokens: %a %A %Y %m %d %e %H %M %S %Z %z %%")
    ("language" :choice :options '("en" "id") :default "en"
                :doc "the language of weekday names: English, or Bahasa Indonesia")
    ("show_elapsed" :boolean :default t
                    :doc "also say how long ago this conversation last moved")
    ("stamp_tool_results" :boolean :default nil
                           :doc "also put the current time on a tool result during a long turn")
    ("stamp_interval_seconds" :integer :default 60 :min 0
                               :doc "the least number of seconds between tool-result stamps; 0 stamps every one"))
  (:settings #'checked-settings)
  (:hook 'nle::read-harness-sections #'inject)
  (:tool "now" (nlk:contract 'now-request) 'answer-now)
  (:command "now" 'run-now
            :description "The time now, and how long ago the conversation last moved")
  (:help :nowstamp
         "nowstamp: every turn carries the real clock and the real gap since the conversation last moved"
         +manual+)
  (:start (lambda ()
            (load-state)
            (when (setting :stamp-tool-results)
              (nle:hook :tool "nodecode-nowstamp" #'stamp-tool-result)
              (nle:on-stop (lambda () (nle:unhook :tool "nodecode-nowstamp")))))))
