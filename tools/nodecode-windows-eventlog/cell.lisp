;;;; cell.lisp --- the Windows Event Log, read for the one question that
;;;; matters when a program vanishes: why did it die.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three model-facing tools over the Event Log, the original plugin's:
;;;; win_crash_events (Application Error / Hang / Windows Error Reporting /
;;;; .NET Runtime records with the faulting module and the exception code
;;;; decoded, plus groups that collapse repeats of one signature),
;;;; win_shutdown_events (Kernel-Power 41 restarts, bugchecks decoded to
;;;; names, EventLog 6008 dirty shutdowns, 1074 operator restarts and the
;;;; start/stop markers) and win_eventlog_query (any channel, filtered,
;;;; structured records instead of raw XML). /windows-eventlog reads one
;;;; channel into the few lines an operator wants first.
;;;;
;;;; Read-only in the same sense the original was: it queries, it never
;;;; writes a log, clears one or configures a channel. The original reached
;;;; wevtapi.dll through ctypes in-process; this asks the same API through
;;;; wevtutil, the tool Windows ships, and parses the XML wevtutil already
;;;; renders -- one bounded child process per call, no library, no
;;;; dependency. A machine without Windows answers a structured error, as
;;;; the original did off-Windows.

(in-package #:nodecode-windows-eventlog)

;;; --- the manual ---------------------------------------------------------------
;;; (help :windows-eventlog) answers this while the cell runs; a request
;;; carries the one line the :HELP clause gives, never this text.

(defparameter +manual+
  "The Windows Event Log is readable through this cell: three read-only tools,
Lisp functions in the windows-eventlog: package called through eval, each
answering one JSON object ({\"ok\": false, ...} when the log could not be read).
  win_crash_events {\"hours\": 24, \"process\": \"hermes\", \"kinds\": [\"application_error\"], \"limit\": 25}
      Application Error / Application Hang / Windows Error Reporting / .NET
      Runtime records in the window, newest first, with app, version, faulting
      module and path, offset and the exception code decoded to its debugger name
      (0xC0000005 (ACCESS_VIOLATION)); plus groups that collapse repeats of one
      app and signature. Use it when a process, a gateway or an app simply
      vanished, hung, or was reported as crashed.
  win_shutdown_events {\"hours\": 168, \"limit\": 30}
      The System log's machine-level story: Kernel-Power 41 unexpected
      restarts, bugchecks decoded to names (0x9F -> DRIVER_POWER_STATE_FAILURE),
      EventLog 6008 dirty shutdowns, 1074 operator restarts with the process and
      the comment, and the 6005/6006/12/13 start and stop markers.
  win_eventlog_query {\"channel\": \"System\", \"event_ids\": [7031, 7034], \"levels\": [1, 2], \"provider\": \"Service Control Manager\", \"contains\": \"gateway\", \"include_details\": true, \"hours\": 24, \"limit\": 20}
      Any channel, with filters applied twice: in the query Windows runs and
      again on what comes back, so an upstream that ignores a filter still
      answers what you asked for. Security needs elevation: expect an ok=false
      with exit 5 rather than an exception.
Read-only: it queries, it never writes a log, clears one or configures a
channel. Windows only -- off Windows every tool answers ok=false.
(help :windows-eventlog) repeats this while the cell runs."
  "The primer the model reads; each tool's description also says its own part.")

;;; --- the section ----------------------------------------------------------------

(defun expand-prefix (word)
  "WORD with a leading ~ read against the home directory, as a shell would."
  (if (and (stringp word) (uiop:string-prefix-p "~" word))
      (uiop:native-namestring (nlk:expand-home word))
      word))

(defun read-settings (values table)
  "The declared VALUES, with the wevtutil argv prefix expanded and checked."
  (declare (ignore table))
  (let* ((command (mapcar #'expand-prefix (coerce (or (getf values :wevtutil) '()) 'list)))
         (plist (copy-list values)))
    (when (or (null command)
              (not (every #'stringp command))
              (every (lambda (word) (or (null word) (zerop (length word)))) command))
      (nlk:config-error "wevtutil names no program: set the windows-eventlog section's wevtutil to [\"wevtutil\"] for the one Windows ships, or to its full path, e.g. [\"C:/Windows/System32/wevtutil.exe\"]"))
    (setf (getf plist :wevtutil) command)
    plist))

;;; --- running wevtutil -------------------------------------------------------------
;;; One seam, so a test stubs the log without a Windows machine: every read
;;; goes through *RUN*.

(defparameter *run* 'run-wevtutil
  "How a read runs wevtutil: (funcall *RUN* ARGV :TIMEOUT SECONDS) =>
(values STDOUT STDERR STATUS), STATUS an exit code or :TIMEOUT.")

(defparameter *windows* (uiop:os-windows-p)
  "Whether this process is on Windows; a test binds it, a section setting can
override it, and off Windows every tool says so instead of running anything.")

(defun windows-p ()
  "Whether the log is here to read: this process's own platform, or the
section saying to try anyway."
  (or (and (setting :assume-windows) t) *windows*))

(defun run-wevtutil (argv &key timeout)
  "Run ARGV to completion or TIMEOUT, => (values STDOUT STDERR STATUS)."
  (handler-case (nlk:run-bounded argv :seconds (or timeout (setting :timeout-seconds)))
    (error (condition)
      ;; A program that is not there is the ordinary state of this cell on a
      ;; machine whose admin moved System32; 127 is what a shell says for it.
      (values nil
              (format nil "~a could not be run: ~a"
                      (first argv) (nlk:one-line (princ-to-string condition)))
              127))))

(defun wevtutil-argv (channel xpath)
  "The one command line a read is: the channel, the XPath filter, XML, newest
first, at most scan_cap records."
  (append (setting :wevtutil)
          (list "qe" channel
                (format nil "/q:~a" xpath)
                "/f:xml"
                (format nil "/c:~d" (setting :scan-cap))
                "/rd:true")))

;;; --- what the model may name ---------------------------------------------------------

(defun plain-name-p (text &key (extra "._/-") (limit 256))
  "TEXT as a channel or provider name: a letter or digit first, then plain
punctuation, no quote to break out of the XPath."
  (and (stringp text)
       (<= 1 (length text) limit)
       (every (lambda (character)
                (or (and (< (char-code character) 128) (alphanumericp character))
                    (find character extra)))
              text)
       (alphanumericp (char text 0))))

(defun whole-number (value)
  "VALUE as the integer it spells, or NIL."
  (cond ((integerp value) value)
        ((and (stringp value)
              (plusp (length (string-trim '(#\Space #\Tab) value))))
         (handler-case (parse-integer (string-trim '(#\Space #\Tab) value)) (error () nil)))
        ((realp value) (round value))
        (t nil)))

(defun event-id-list (values)
  "VALUES, a JSON array of integers, as the event ids it names: 1-65535, in
order, without repeats."
  (let ((ids '()))
    (when (vectorp values)
      (loop for value across values
            for id = (whole-number value)
            when (and id (<= 1 id 65535) (not (member id ids)))
              do (push id ids)))
    (nreverse ids)))

(defun level-list (values)
  "VALUES as event levels 1-5."
  (let ((levels '()))
    (when (vectorp values)
      (loop for value across values
            for level = (whole-number value)
            when (and level (<= 1 level 5) (not (member level levels)))
              do (push level levels)))
    (nreverse levels)))

(defun wanted-kinds (values)
  "VALUES, a JSON array of kind names, as the set of kinds it limits to, or
NIL for all of them. An unknown word is kept, and then matches nothing."
  (when (and (vectorp values) (plusp (length values)))
    (remove-duplicates
     (loop for value across values
           when (and (stringp value) (plusp (length (string-trim '(#\Space #\Tab) value))))
             collect (string-downcase (string-trim '(#\Space #\Tab) value)))
     :test #'equal)))

;;; --- the XPath the read carries --------------------------------------------------------

(defun event-ids-xpath (ids)
  "The XPath filter for IDS: every record whose id is one of them."
  (if ids
      (format nil "*[System[(~{~a~^ or ~})]]"
              (mapcar (lambda (id) (format nil "EventID=~d" id)) ids))
      "*"))

(defun filtered-xpath (ids levels provider)
  "The XPath filter for the general query: event ids, levels, exact provider."
  (let ((clauses '()))
    (when ids
      (push (format nil "(~{~a~^ or ~})"
                    (mapcar (lambda (id) (format nil "EventID=~d" id)) ids))
            clauses))
    (when levels
      (push (format nil "(~{~a~^ or ~})"
                    (mapcar (lambda (level) (format nil "Level=~d" level)) levels))
            clauses))
    (when (and (stringp provider) (plusp (length provider)))
      (push (format nil "Provider[@Name='~a']" provider) clauses))
    (if clauses
        (format nil "*[System[~{~a~^ and ~}]]" (nreverse clauses))
        "*")))

;;; --- times ---------------------------------------------------------------------------
;;; Windows writes SystemTime as ISO 8601 UTC with a 7-digit fraction and a
;;; Z, sometimes with an offset. Everything here is seconds since 1970 and
;;; microseconds, so a window is arithmetic rather than string comparison.

(defconstant +seconds-per-day+ 86400 "Seconds in a day, for the date arithmetic below.")

(defun days-from-civil (year month day)
  "The day number of YEAR-MONTH-DAY, 1970-01-01 being 0."
  ;; Howard Hinnant's civil_from_days, inverted: no table, no leap-year case.
  (let* ((year (if (<= month 2) (1- year) year))
         (era (floor year 400))
         (yoe (- year (* era 400)))
         (mp (if (> month 2) (- month 3) (+ month 9)))
         (doy (+ (floor (+ (* 153 mp) 2) 5) (1- day)))
         (doe (+ (* yoe 365) (floor yoe 4) (- (floor yoe 100)) doy)))
    (- (+ (* era 146097) doe) 719468)))

(defun civil-from-days (days)
  "YEAR, MONTH and DAY of the day number DAYS."
  (let* ((days (+ days 719468))
         (era (floor days 146097))
         (doe (- days (* era 146097)))
         (yoe (floor (+ doe (- (floor doe 1460)) (floor doe 36524) (- (floor doe 146096))) 365))
         (year (+ yoe (* era 400)))
         (doy (- doe (+ (* 365 yoe) (floor yoe 4) (- (floor yoe 100)))))
         (mp (floor (+ (* 5 doy) 2) 153))
         (day (+ (- doy (floor (+ (* 153 mp) 2) 5)) 1))
         (month (if (< mp 10) (+ mp 3) (- mp 9))))
    (values (if (<= month 2) (1+ year) year) month day)))

(defun digits-value (text start end)
  "TEXT between START and END as the integer it spells, or NIL when it is not
all digits."
  (and (<= 0 start) (<= start end) (<= end (length text))
       (let ((value 0))
         (loop for index from start below end
               for digit = (digit-char-p (char text index))
               do (if digit (setf value (+ (* value 10) digit)) (return-from digits-value nil)))
         value)))

(defun parse-time (text)
  "TEXT, an ISO 8601 SystemTime, as (values SECONDS MICROSECONDS) in UTC, or
NIL when it does not parse."
  (when (and (stringp text) (>= (length text) 19))
    (let ((text (string-trim '(#\Space #\Tab) text)))
      (when (and (>= (length text) 19)
                 (find (char text 10) "Tt ")
                 (char= (char text 13) #\:)
                 (char= (char text 16) #\:))
        (let ((year (digits-value text 0 4)) (month (digits-value text 5 7))
              (day (digits-value text 8 10)) (hour (digits-value text 11 13))
              (minute (digits-value text 14 16)) (second (digits-value text 17 19)))
          (when (and year month day hour minute second
                     (<= 1 month 12) (<= 1 day 31) (<= hour 23) (<= minute 59) (<= second 60))
            (let ((index 19) (micros 0) (offset 0))
              (when (and (< index (length text)) (char= (char text index) #\.))
                (let ((start (1+ index)) (stop (1+ index)))
                  (loop while (and (< stop (length text)) (digit-char-p (char text stop)))
                        do (incf stop))
                  (let* ((digits (concatenate 'string (subseq text start stop) "000000"))
                         (micros-value (digits-value digits 0 6)))
                    (setf micros (or micros-value 0)
                          index stop))))
              (when (< index (length text))
                (let ((zone (subseq text index)))
                  (when (and (plusp (length zone)) (find (char zone 0) "+-"))
                    (let* ((sign (if (char= (char zone 0) #\-) -1 1))
                           (hours (digits-value zone 1 3))
                           (minutes (if (and (> (length zone) 4) (char= (char zone 3) #\:))
                                        (digits-value zone 4 6)
                                        (digits-value zone 3 5))))
                      (when (and hours minutes)
                        (setf offset (* sign (+ (* hours 3600) (* minutes 60)))))))))
              (values (+ (* (days-from-civil year month day) +seconds-per-day+)
                         (* hour 3600) (* minute 60) second)
                      micros))))))))

(defun format-time (seconds micros)
  "SECONDS and MICROS as the ISO 8601 UTC string every record is stamped with."
  (multiple-value-bind (days rest) (floor seconds +seconds-per-day+)
    (multiple-value-bind (year month day) (civil-from-days days)
      (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.~6,'0dZ"
              year month day
              (floor rest 3600) (floor (mod rest 3600) 60) (mod rest 60)
              (or micros 0)))))

(defun now-seconds ()
  "This moment, in the seconds-since-1970 this file counts in."
  (- (get-universal-time) 2208988800))

(defun cutoff-seconds (hours)
  "The instant HOURS ago."
  (round (- (now-seconds) (* hours 3600))))

;;; --- a record -------------------------------------------------------------------------

(defstruct (record (:constructor make-record))
  "One event record as the log renders it: the System block, the provider's
named and positional data, and its own UserData schema."
  (time-seconds nil)
  (time-micros 0)
  (event-id nil)
  (level-number nil)
  (provider nil)
  (channel nil)
  (computer nil)
  (process-id nil)
  (thread-id nil)
  (record-id nil)
  (data '())
  (data-list '())
  (user-data ""))

(defparameter +level-names+ '((0 . "log_always") (1 . "critical") (2 . "error")
                             (3 . "warning") (4 . "information") (5 . "verbose"))
  "The Event Log's five levels, and the zero some providers write.")

(defun record-level (record)
  "RECORD's level as a word, or NIL."
  (cdr (assoc (record-level-number record) +level-names+)))

(defun record-iso-time (record)
  "RECORD's time as ISO 8601 UTC, or NIL."
  (let ((seconds (record-time-seconds record)))
    (and seconds (format-time seconds (record-time-micros record)))))

(defun safe-int (text)
  (and (stringp text) (plusp (length text))
       (handler-case (parse-integer text) (error () nil))))

(defun event-record (root)
  "ROOT, one rendered Event element, as a RECORD; every field optional."
  (let* ((system (xchild root "System"))
         (provider (xchild system "Provider"))
         (created (xchild system "TimeCreated"))
         (execution (xchild system "Execution"))
         (event-data (xchild root "EventData"))
         (named '()) (positional '()) (indexed '()))
    (loop for item in (xchildren event-data "Data")
          for position from 0
          for value = (xtext item)
          for name = (xattr item "Name")
          do (if (and name (plusp (length name)))
                 (push (cons name value) named)
                 (progn (push value positional)
                        (push (cons (format nil "#~d" position) value) indexed))))
    (multiple-value-bind (seconds micros)
        (parse-time (and created (xattr created "SystemTime")))
      (make-record
       :time-seconds seconds
       :time-micros micros
       :event-id (safe-int (xtext (xchild system "EventID")))
       :level-number (safe-int (xtext (xchild system "Level")))
       :provider (and provider (xattr provider "Name"))
       :channel (let ((channel (xtext (xchild system "Channel"))))
                  (and (plusp (length channel)) channel))
       :computer (let ((computer (xtext (xchild system "Computer"))))
                   (and (plusp (length computer)) computer))
       :process-id (safe-int (and execution (xattr execution "ProcessID")))
       :thread-id (safe-int (and execution (xattr execution "ThreadID")))
       :record-id (safe-int (xtext (xchild system "EventRecordID")))
       :data (append (nreverse named) (nreverse indexed))
       :data-list (nreverse positional)
       :user-data (xdescribe (xchild root "UserData"))))))

(defun field (record &rest names)
  "RECORD's named data under the first of NAMES it carries, case-blind --
providers differ in casing between Windows builds."
  (let ((data (record-data record)))
    (loop for name in names
          for value = (cdr (assoc name data :test #'name-equal))
          when (and value (plusp (length value)))
            return value)))

(defun payload-text (record)
  "Every payload value RECORD carries, joined: what a substring filter reads."
  (let ((parts (append (record-data-list record)
                       (mapcar #'cdr (record-data record))
                       (list (record-user-data record)))))
    (format nil "~{~a~^ ~}" (remove "" (remove nil parts) :test #'string=))))

;;; --- reading the log ---------------------------------------------------------------------

(defun parse-log (text)
  "Every Event element in TEXT, parsed."
  (parse-xml-elements (or text "") "Event"))

(defun window-events (channel xpath cutoff)
  "The records of CHANNEL that XPATH selects and that are newer than CUTOFF,
newest first: (values RECORDS SCANNED TRUNCATED). TRUNCATED is true when the
scan cap was reached before the window's start."
  (multiple-value-bind (output errors status)
      (funcall *run* (wevtutil-argv channel xpath) :timeout (setting :timeout-seconds))
    (cond
      ((eq status :timeout)
       (fail "~a qe ~a did not answer within ~d s; raise timeout_seconds in the windows-eventlog section"
             (first (setting :wevtutil)) channel (setting :timeout-seconds)))
      ((and (integerp status) (plusp status))
       ;; wevtutil's own words are the reason: exit 5 is access denied, 15007
       ;; a channel that is not there. Both are answers, not exceptions.
       (fail "~a qe ~a exited ~d: ~a"
             (first (setting :wevtutil)) channel status
             (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                       (format nil "~@[~a~] ~@[~a~]" errors output))))
               (if (plusp (length text)) text "nothing was written"))))
      (t
       (let ((records '()) (scanned 0) (truncated nil) (cap (setting :scan-cap)))
         (dolist (root (parse-log output))
           (incf scanned)
           (let* ((record (event-record root))
                  (seconds (record-time-seconds record)))
             (cond
               ;; A record whose time does not parse is counted and dropped, as
               ;; the original did: it can neither be placed nor trusted.
               ((null seconds) nil)
               ((< seconds cutoff) (return))
               (t (push record records))))
           (when (>= scanned cap)
             (setf truncated t)
             (return)))
         (values (nreverse records) scanned truncated))))))

;;; --- what a crash is ---------------------------------------------------------------
;;; The ids both crash tools ask for, and the words each id answers. The same
;;; id is reused by unrelated providers (1000 and 1001 are common
;;; "informational" ids), so a record's kind is read off its provider too,
;;; and anything outside the real family is dropped rather than reported as a
;;; crash with empty fields.

(defparameter +crash-ids+ '(1000 1001 1002 1023 1026)
  "The Application log ids the crash tool asks Windows for.")

(defparameter +crash-kinds+ '((1000 . "application_error") (1002 . "application_hang")
                              (1001 . "wer") (1023 . "dotnet") (1026 . "dotnet"))
  "The id a crash record carries, and the kind it is.")

(defparameter +shutdown-ids+ '(12 13 41 1001 1074 6005 6006 6008)
  "The System log ids the shutdown tool asks Windows for.")

(defparameter +shutdown-kinds+ '((41 . "unexpected_restart") (6008 . "unexpected_shutdown")
                                 (1074 . "initiated_shutdown") (6005 . "event_log_started")
                                 (6006 . "event_log_stopped") (12 . "os_started")
                                 (13 . "os_shutdown"))
  "The id a machine-level record carries, and the kind it is.")

(defparameter +exception-codes+
  '((#xC0000005 . "ACCESS_VIOLATION") (#xC0000006 . "IN_PAGE_ERROR")
    (#xC0000008 . "INVALID_HANDLE") (#xC0000017 . "NO_MEMORY")
    (#xC000001D . "ILLEGAL_INSTRUCTION") (#xC0000022 . "ACCESS_DENIED")
    (#xC0000094 . "INTEGER_DIVIDE_BY_ZERO") (#xC0000096 . "PRIVILEGED_INSTRUCTION")
    (#xC00000FD . "STACK_OVERFLOW") (#xC0000135 . "DLL_NOT_FOUND")
    (#xC0000139 . "ENTRYPOINT_NOT_FOUND") (#xC0000142 . "DLL_INIT_FAILED")
    (#xC000027B . "STOWED_EXCEPTION") (#xC0000374 . "HEAP_CORRUPTION")
    (#xC0000409 . "STACK_BUFFER_OVERRUN") (#xC000041D . "UNHANDLED_EXCEPTION")
    (#xC0000420 . "ASSERTION_FAILURE") (#xC0000602 . "FAIL_FAST_EXCEPTION")
    (#x80000003 . "BREAKPOINT") (#x80000004 . "SINGLE_STEP")
    (#xE0434352 . "CLR_EXCEPTION") (#xE06D7363 . "CPP_EXCEPTION"))
  "The exception codes a crash record carries, with the name every debugger
shows for them.")

(defparameter +bugcheck-codes+
  '((#x0000000A . "IRQL_NOT_LESS_OR_EQUAL") (#x0000001A . "MEMORY_MANAGEMENT")
    (#x0000001E . "KMODE_EXCEPTION_NOT_HANDLED") (#x0000003B . "SYSTEM_SERVICE_EXCEPTION")
    (#x00000050 . "PAGE_FAULT_IN_NONPAGED_AREA") (#x0000007E . "SYSTEM_THREAD_EXCEPTION_NOT_HANDLED")
    (#x0000009F . "DRIVER_POWER_STATE_FAILURE") (#x000000BE . "ATTEMPTED_WRITE_TO_READONLY_MEMORY")
    (#x000000D1 . "DRIVER_IRQL_NOT_LESS_OR_EQUAL") (#x000000EF . "CRITICAL_PROCESS_DIED")
    (#x00000133 . "DPC_WATCHDOG_VIOLATION") (#x00000139 . "KERNEL_SECURITY_CHECK_FAILURE")
    (#x00000154 . "UNEXPECTED_STORE_EXCEPTION") (#x0000021A . "STATUS_SYSTEM_PROCESS_TERMINATED"))
  "The bugcheck codes Kernel-Power 41 and WER-SystemErrorReporting 1001 carry.")

(defparameter +app-keys+
  '("Application Name" "AppName" "Application" "P1" "Process Name"
    "Faulting application name" "Image")
  "The keys that name the crashing application, in the order each kind uses them.")

;;; --- the words a record is read into ---------------------------------------------------

(defmacro answering (&body body)
  "BODY's JSON answer; a refusal is the ok=false answer, never an unhandled error."
  `(handler-case (progn ,@body)
     (windows-eventlog-error (condition)
       (json-fail "~a" (windows-eventlog-error-detail condition)))
     (error (condition)
       (json-fail "~a" (nlk:one-line (princ-to-string condition))))))

(defparameter +windows-only+
  "the Windows Event Log is only on Windows: this cell reads it with wevtutil, and this process is not on Windows (set the windows-eventlog section's assume_windows to true to try anyway)"
  "What every tool answers off Windows.")

(defun json-fail (control &rest args)
  "One structured failure: what the original answered when a read went wrong."
  (nlk:encode-json-object
   (nlk:json-object "ok" nil "error" (apply #'format nil control args))))

(defun plist-object (plist)
  "PLIST as a JSON object, its NIL and empty-string members left out -- the
original dropped absent fields so a record says only what Windows recorded."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on plist by #'cddr
          when (and key value (not (and (stringp value) (zerop (length value)))))
            do (setf (gethash key object) value))
    object))

(defun trimmed-text (value)
  "VALUE as a trimmed string, the empty string for anything else."
  (if (stringp value) (string-trim '(#\Space #\Tab) value) ""))

(defun clamp (value low high)
  (max low (min high value)))

(defun hours-of (value default)
  "VALUE as a window in hours, clamped to the half day an hour is worth and
the thirty days the log is asked for."
  (clamp (if (realp value) (float value 1d0) default) 0.1d0 720d0))

(defun limit-of (value default)
  (clamp (if (integerp value) value default) 1 200))

(defun clip (text limit)
  "TEXT cut at LIMIT characters, saying how much it left out."
  (if (or (not (stringp text)) (<= (length text) limit))
      (or text "")
      (format nil "~a... [~d chars truncated]" (subseq text 0 limit) (- (length text) limit))))

(defun hex-value (text)
  "TEXT as the integer its hex spelling means, with or without 0x, or NIL."
  (when (stringp text)
    (let* ((text (string-trim '(#\Space #\Tab) text))
           (digits (if (and (>= (length text) 2) (string-equal "0x" text :end2 2))
                       (subseq text 2)
                       text)))
      (when (and (<= 1 (length digits) 8)
                 (every (lambda (character) (digit-char-p character 16)) digits))
        (parse-integer digits :radix 16)))))

(defun decode-exception (code)
  "CODE as the debugger name of the exception it means, or NIL."
  (let ((value (hex-value code)))
    (and value (cdr (assoc value +exception-codes+)))))

(defun describe-exception (code)
  "`0xC0000005 (ACCESS_VIOLATION)' when the code is known, else the code as written."
  (let ((text (trimmed-text code)))
    (cond ((zerop (length text)) nil)
          (t (let ((name (decode-exception text)))
               (if name
                   (format nil "0x~8,'0X (~a)" (hex-value text) name)
                   text))))))

(defun bugcheck-name (raw)
  "BUGCHECK from Kernel-Power 41 or WER 1001 decoded to its name, or NIL. The
two providers spell it differently: one decimal (`159'), one hex (`0x9f')."
  (let ((text (trimmed-text raw)))
    (when (plusp (length text))
      (let* ((hex-p (and (>= (length text) 2) (string-equal "0x" text :end2 2)))
             (values (if hex-p
                         (list (hex-value text))
                         (list (hex-value text)
                               (handler-case (parse-integer text :radix 16) (error () nil))
                               (handler-case (parse-integer text) (error () nil))))))
        (loop for value in values
              when (and value (cdr (assoc value +bugcheck-codes+)))
                return (cdr (assoc value +bugcheck-codes+)))))))

(defun app-name (record)
  "The application RECORD names as the one that crashed, or NIL."
  (loop for key in +app-keys+
        for value = (field record key)
        when value return value))

(defun crash-kind (event-id provider)
  "The kind of crash EVENT-ID from PROVIDER is, or NIL for a record outside
the crash family."
  (let ((provider (string-downcase (or provider "")))
        (kind (cdr (assoc event-id +crash-kinds+))))
    (cond
      ((null kind) nil)
      ((and (= event-id 1000) (not (search "application error" provider))) nil)
      ((and (= event-id 1002) (not (search "application hang" provider))) nil)
      ((and (= event-id 1001) (not (search "error reporting" provider))) nil)
      ((and (member event-id '(1023 1026)) (not (search ".net runtime" provider))) nil)
      (t kind))))

;;; --- one crash record ---------------------------------------------------------------

(defun crash-item (record)
  "RECORD as the event a crash answer carries, or NIL outside the crash family."
  (let* ((event-id (or (record-event-id record) 0))
         (kind (crash-kind event-id (record-provider record))))
    (when kind
      (if (member event-id '(1023 1026))
          (plist-object
           (list "kind" "dotnet"
                 "time" (record-iso-time record)
                 "app" (app-name record)
                 "event_id" event-id
                 "provider" (record-provider record)
                 "level" (record-level record)
                 "exception_text" (clip (payload-text record) 600)))
          (plist-object
           (list "kind" kind
                 "time" (record-iso-time record)
                 "app" (app-name record)
                 "app_version" (field record "Application Version" "P2")
                 "app_path" (field record "Application Path" "AppPath")
                 "module" (field record "Faulting Module Name" "ModName")
                 "module_path" (field record "Faulting Module Path" "Module Path")
                 "exception" (describe-exception (field record "Exception Code" "ExceptionCode"))
                 "offset" (field record "Fault Offset")
                 "event_name" (field record "EventName")
                 "bucket" (field record "Bucket")
                 "report_id" (field record "Report Id" "ReportId")
                 "process_id" (record-process-id record)
                 "event_id" event-id
                 "provider" (record-provider record)
                 "level" (record-level record)))))))

(defun matches-process-p (record needle)
  "Whether RECORD is about NEEDLE: the crashing image, its path, the faulting
module, or anything in the payload."
  (let ((needle (string-downcase needle))
        (candidates (list (app-name record)
                          (field record "Application Path" "AppPath")
                          (field record "Faulting Module Name"))))
    (or (loop for candidate in candidates
              when (and candidate (search needle (string-downcase candidate)))
                return t)
        (not (null (search needle (string-downcase (payload-text record))))))))

(defun sort-by-time (events)
  "EVENTS newest first, as every answer is."
  (stable-sort (copy-list events)
               (lambda (left right)
                 (string> (or (gethash "time" left) "") (or (gethash "time" right) "")))))

(defun crash-groups (events)
  "EVENTS collapsed by application and signature, the busiest group first,
five modules each."
  (let ((buckets (make-hash-table :test #'equal))
        (order '()))
    (dolist (event events)
      (let* ((app (or (gethash "app" event) "unknown"))
             (signature (or (gethash "exception" event) (gethash "event_name" event)
                            (gethash "kind" event) "unknown"))
             (key (cons (string-downcase app) (princ-to-string signature))))
        (let ((entry (gethash key buckets)))
          (unless entry
            (setf entry (list :app app :signature signature :count 0
                              :first nil :last nil :modules '()))
            (setf (gethash key buckets) entry)
            (push key order))
          (incf (getf entry :count))
          (let ((time (gethash "time" event))
                (module (gethash "module" event)))
            (when time
              (unless (getf entry :first) (setf (getf entry :first) time))
              (unless (getf entry :last) (setf (getf entry :last) time))
              (when (string< time (getf entry :first)) (setf (getf entry :first) time))
              (when (string< (getf entry :last) time) (setf (getf entry :last) time)))
            (when (and module (not (member module (getf entry :modules) :test #'equal)))
              (push module (getf entry :modules)))))))
    (let ((groups (mapcar (lambda (key) (gethash key buckets)) order)))
      (coerce
       (mapcar (lambda (entry)
                 (plist-object
                  (list "app" (getf entry :app)
                        "signature" (getf entry :signature)
                        "count" (getf entry :count)
                        "first" (getf entry :first)
                        "last" (getf entry :last)
                        "modules" (coerce (subseq (nreverse (getf entry :modules))
                                                   0 (min 5 (length (getf entry :modules))))
                                          'vector))))
               (stable-sort groups (lambda (left right)
                                     (> (getf left :count) (getf right :count)))))
       'vector))))

;;; --- the three tools --------------------------------------------------------------

(defun window-notes (scanned hours channel truncated)
  (let ((notes (list (format nil "scanned ~d ~a events in the last ~a h"
                             scanned channel
                             (if (= hours (fround hours)) (format nil "~d" (round hours))
                                 (format nil "~,1f" hours))))))
    (when truncated
      (push "stopped at the scan cap before reaching the window start; narrow the window or raise scan_cap"
            notes))
    (nreverse notes)))

(defun crash-events (request)
  "The win_crash_events tool: crash, hang, WER and .NET records in the window,
with the faulting module and the exception code decoded, and the groups they
collapse into."
  (answering
    (progn
      (unless (windows-p) (fail "~a" +windows-only+))
      (let* ((hours (hours-of (crash-events-request-hours request) 24))
             (limit (limit-of (crash-events-request-limit request) 25))
             (process (trimmed-text (crash-events-request-process request)))
             (wanted (wanted-kinds (crash-events-request-kinds request))))
        (multiple-value-bind (records scanned truncated)
            (window-events "Application" (event-ids-xpath +crash-ids+)
                           (cutoff-seconds hours))
          (let ((events '()))
            (dolist (record records)
              (let ((item (crash-item record)))
                (when (and item
                           (or (null wanted) (member (gethash "kind" item) wanted :test #'equal))
                           (or (zerop (length process)) (matches-process-p record process)))
                  (push item events))))
            (let* ((events (sort-by-time events))
                   (events (subseq events 0 (min limit (length events))))
                   (notes (window-notes scanned hours "Application" truncated)))
              (unless events
                (push "no crash records in this window; a process killed from outside (Task Manager, a supervisor, a job object) writes no Application log record -- ask win_eventlog_query about the System log's service-control events instead"
                      notes))
              (nlk:encode-json-object
               (nlk:json-object "ok" t
                                "window_hours" (coerce hours 'double-float)
                                "matched" (length events)
                                "scanned" scanned
                                "events" (coerce events 'vector)
                                "groups" (crash-groups events)
                                "notes" (coerce (nreverse notes) 'vector))))))))))

(defun shutdown-item (record)
  "RECORD as the machine-level event a shutdown answer carries, or NIL for a
1001 that is not a bugcheck."
  (let* ((event-id (or (record-event-id record) 0))
         (provider (or (record-provider record) ""))
         (kind (cdr (assoc event-id +shutdown-kinds+))))
    (cond
      ((and (= event-id 1001)
            (or (search "systemerrorreporting" (string-downcase provider))
                (search "bugcheck" (string-downcase provider))))
       (plist-object
        (list "kind" "bugcheck"
              "time" (record-iso-time record)
              "event_id" event-id
              "provider" (record-provider record)
              "level" (record-level record)
              "bugcheck_code" (field record "BugcheckCode")
              "bugcheck" (bugcheck-name (field record "BugcheckCode"))
              "parameters" (coerce (loop for index from 1 to 4
                                          for value = (field record (format nil "BugcheckParameter~d" index))
                                          collect (or value ""))
                                    'vector))))
      ((and (= event-id 1001) (null kind)) nil)
      (kind
       (let ((item (list "kind" kind
                          "time" (record-iso-time record)
                          "event_id" event-id
                          "provider" (record-provider record)
                          "level" (record-level record))))
         (when (= event-id 41)
           (setf item (append item
                              (list "bugcheck_code" (field record "BugcheckCode")
                                    "bugcheck" (bugcheck-name (field record "BugcheckCode"))
                                    "power_button" (field record "PowerButtonTimestamp")
                                    "sleep_in_progress" (field record "SleepInProgress")))))
         (when (= event-id 1074)
           (setf item (append item
                              (list "process" (field record "param1" "Process")
                                    "reason" (field record "param2" "ReasonCode")
                                    "comment" (clip (field record "param5" "Comment") 200)))))
         (plist-object item)))
      (t nil))))

(defun shutdown-events (request)
  "The win_shutdown_events tool: unexpected restarts, bugchecks and the
start/stop markers the System log keeps."
  (answering
    (progn
      (unless (windows-p) (fail "~a" +windows-only+))
      (let* ((hours (hours-of (shutdown-events-request-hours request) 168))
             (limit (limit-of (shutdown-events-request-limit request) 30)))
        (multiple-value-bind (records scanned truncated)
            (window-events "System" (event-ids-xpath +shutdown-ids+)
                           (cutoff-seconds hours))
          (let ((events '()))
            (dolist (record records)
              (let ((item (shutdown-item record)))
                (when item (push item events))))
            (let* ((events (subseq (sort-by-time events) 0 (min limit (length events))))
                   (notes (window-notes scanned hours "System" truncated)))
              (unless events
                (push "no unexpected restart, bugcheck or shutdown record in this window: a clean week of uptime looks exactly like this"
                      notes))
              (nlk:encode-json-object
               (nlk:json-object "ok" t
                                "window_hours" (coerce hours 'double-float)
                                "matched" (length events)
                                "scanned" scanned
                                "events" (coerce events 'vector)
                                "notes" (coerce (nreverse notes) 'vector))))))))))

(defun data-object (record)
  "RECORD's named data as a JSON object: twenty members, clipped."
  (let ((object (make-hash-table :test #'equal))
        (count 0))
    (dolist (pair (record-data record))
      (when (< count 20)
        (setf (gethash (car pair) object) (clip (cdr pair) 300))
        (incf count)))
    object))

(defun query-item (record details)
  "RECORD as the record a general query answers, with its data when DETAILS."
  (let ((item (list "time" (record-iso-time record)
                     "event_id" (record-event-id record)
                     "level" (record-level record)
                     "provider" (record-provider record)
                     "channel" (record-channel record)
                     "process_id" (record-process-id record)
                     "message" (clip (payload-text record) 500))))
    (when details
      (setf item (append item (list "data" (data-object record)))))
    (plist-object item)))

(defun eventlog-query (request)
  "The win_eventlog_query tool: any channel, filtered, as structured records."
  (answering
    (progn
      (unless (windows-p) (fail "~a" +windows-only+))
      (let* ((channel (let ((given (trimmed-text (eventlog-query-request-channel request))))
                        (if (plusp (length given)) given "Application")))
             (hours (hours-of (eventlog-query-request-hours request) 24))
             (limit (limit-of (eventlog-query-request-limit request) 20))
             (ids (event-id-list (eventlog-query-request-event-ids request)))
             (levels (level-list (eventlog-query-request-levels request)))
             (provider (trimmed-text (eventlog-query-request-provider request)))
             (contains (string-downcase (trimmed-text (eventlog-query-request-contains request))))
             (details (eventlog-query-request-include-details request)))
        (unless (plain-name-p channel)
          (fail "channel must be a plain log name such as Application, System or Microsoft-Windows-TaskScheduler/Operational"))
        (unless (or (zerop (length provider)) (plain-name-p provider))
          (fail "provider must be an exact provider name without quotes"))
        (multiple-value-bind (records scanned truncated)
            (window-events channel (filtered-xpath ids levels provider)
                           (cutoff-seconds hours))
          (let ((events '()))
            (dolist (record records)
              (let ((haystack (string-downcase
                               (format nil "~a ~a ~a"
                                       (or (record-provider record) "")
                                       (or (record-channel record) "")
                                       (payload-text record)))))
                (when (and (or (zerop (length provider))
                               (and (record-provider record)
                                    (string= provider (record-provider record))))
                           (or (null ids) (member (record-event-id record) ids))
                           (or (null levels) (member (record-level-number record) levels))
                           (or (zerop (length contains)) (search contains haystack)))
                  (push (query-item record details) events))))
            (let* ((events (subseq (sort-by-time events) 0 (min limit (length events))))
                   (notes (window-notes scanned hours channel truncated)))
              (nlk:encode-json-object
               (nlk:json-object "ok" t
                                "channel" channel
                                "window_hours" (coerce hours 'double-float)
                                "matched" (length events)
                                "scanned" scanned
                                "events" (coerce events 'vector)
                                "notes" (coerce (nreverse notes) 'vector))))))))))

;;; --- /windows-eventlog ---------------------------------------------------------------

(defun event-line (record)
  "One record as the single line an operator reads."
  (format nil "  ~a  ~a  ~@[~a ~]~@[~a ~]~a"
          (or (record-iso-time record) "(no time)")
          (or (record-event-id record) "?")
          (record-level record)
          (record-provider record)
          (clip (payload-text record) 160)))

(defun run-command (args session-id)
  "One /windows-eventlog invocation: the newest records of a channel, or of
Application when none is named."
  (declare (ignore session-id))
  (let ((given (trimmed-text (and args (princ-to-string args)))))
    (handler-case
        (progn
          (unless (windows-p) (fail "~a" +windows-only+))
          (let ((channel (if (plusp (length given)) given "Application")))
            (unless (plain-name-p channel)
              (fail "~s is no channel name: give one such as Application, System or Microsoft-Windows-TaskScheduler/Operational"
                    channel))
            (multiple-value-bind (records scanned truncated)
                (window-events channel "*" (cutoff-seconds 24))
              (with-output-to-string (out)
                (format out "windows-eventlog -- ~a, the last 24 h, newest first~%" channel)
                (if (null records)
                    (format out "  no records in the window~%")
                    (dolist (record (subseq records 0 (min 15 (length records))))
                      (format out "~a~%" (event-line record))))
                (format out "  scanned ~d record~:p~@[, stopped at the scan cap~]~%"
                        scanned truncated)))))
      (windows-eventlog-error (condition)
        (format nil "windows-eventlog: ~a" (windows-eventlog-error-detail condition)))
      (error (condition)
        (format nil "windows-eventlog: ~a" (nlk:one-line (princ-to-string condition)))))))

;;; --- the declaration ---------------------------------------------------------------


(nle:define-cell windows-eventlog
  (:section ("windows-eventlog")
    (:guide "nothing is required on Windows: wevtutil is already there; the tools read the Application and System logs by themselves; next step: ask why a process died, or type /windows-eventlog System")
    ("wevtutil" :list :default '("wevtutil")
     :doc "the argv prefix that runs wevtutil: [\"wevtutil\"] for the one Windows ships on PATH, or its full path, e.g. [\"C:/Windows/System32/wevtutil.exe\"]")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one read's ceiling in seconds; its whole process tree is killed past it")
    ("scan_cap" :integer :default 5000 :min 1
     :doc "how many records one read asks Windows for: the window is walked newest first until it is crossed or this many have been read")
    ("assume_windows" :boolean :default nil
     :doc "read the log from a process that does not think it is on Windows (an emulation layer, a wrapper): the tools try wevtutil anyway"))
  (:settings #'read-settings)
  (:help :windows-eventlog
         "windows-eventlog: Windows crash forensics - win_crash_events, win_shutdown_events, win_eventlog_query read the Application and System logs"
         +manual+)
  (:command "windows-eventlog" 'run-command
            :description "The newest records of a Windows event channel"
            :argument-hint "[CHANNEL]")
  (:tool "win_crash_events" (nlk:contract 'crash-events-request) #'crash-events)
  (:tool "win_shutdown_events" (nlk:contract 'shutdown-events-request) #'shutdown-events)
  (:tool "win_eventlog_query" (nlk:contract 'eventlog-query-request) #'eventlog-query))
