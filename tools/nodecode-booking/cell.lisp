;;;; cell.lisp --- free time and real bookings on the operator's own calendar.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Five tools, the same five the Hermes plugin offered: what is free, what
;;;; is coming, and the three writes. Availability is Google's own FreeBusy
;;;; answer cut into slots, never a guess; every write re-checks it first, so a
;;;; slot that filled since the last look is refused instead of double booked.
;;;;
;;;; A write also needs confirm: true, which the model sets only after the
;;;; operator has agreed to that exact time, and the tool says so in its own
;;;; description. Read-only tools never need it and work everywhere.
;;;;
;;;; Rewritten from a Hermes Agent plugin of the same subject; see NOTICE.

(in-package #:nodecode-booking)

;;; --- the manual ---------------------------------------------------------------

(defparameter *summary*
  "Free slots and real bookings on Google Calendar: availability, upcoming events, book, move, cancel."
  "The one line every request's help section carries.")

(defparameter *manual*
  "This cell reads and writes the operator's own Google Calendar through the Calendar v3 API, under the
one calendar named in its section. Times are RFC3339 (2026-11-17T09:00:00-06:00) or a bare
2026-11-17T09:00, read at the call's utc_offset, else the section's, else this machine's zone; every
answer writes times back at the same offset.
  booking_list_availability {time_min, time_max, duration_minutes, utc_offset, calendar_id}
      genuinely free slots, from Google's FreeBusy answer, in a window of at most 14 days. Read
      availability before offering any hour: never invent one. Slots come back to back, a minute grid
      from time_min, at most 60 of them (then truncated is true).
  booking_list_upcoming {max_results, time_min, calendar_id}   the next events with their ids.
  booking_book_event {title, start, end, utc_offset, description, attendees, calendar_id, confirm}
      one consult: re-checks FreeBusy, refuses on a conflict, and an identical booking is answered
      with duplicate: true instead of a second event.
  booking_reschedule_event {event_id, start, end, utc_offset, calendar_id, confirm}
      moves an event to a free time, keeping its length when end is left out; ignores the event's own
      current interval when it checks.
  booking_cancel_event {event_id, calendar_id, confirm}   deletes one event.
The three writes need confirm: true, set only after the operator agreed to that exact action; without
it the answer is a refusal and nothing changes. Booked events carry the private property
bookingBridgeKey, the same idempotency mark the original plugin wrote, and attendees named in the call
are emailed an invitation (sendUpdates=all).
The /booking command says whether the grant answers and what is coming; /booking help prints this.
Setup, when a call says the grant was refused or a variable is unset: a Google Cloud project with the
Calendar API on, an OAuth client of type Desktop app, and a refresh token for calendar.events and
calendar.freebusy in the section's variables."
  "What (help :booking) answers while the cell runs.")

;;; --- the schemas, once -------------------------------------------------------------

(defparameter *argument-docs*
  '(("time_min" . ("string" "window start: RFC3339, or YYYY-MM-DDTHH:MM at utc_offset"))
    ("time_max" . ("string" "window end, same forms, at most 14 days after time_min"))
    ("duration_minutes" . ("integer" "slot length in minutes; defaults to the configured consult length"))
    ("utc_offset" . ("string" "the UTC offset times are written at, e.g. -06:00; defaults to the section's"))
    ("calendar_id" . ("string" "calendar id; defaults to the configured calendar"))
    ("max_results" . ("integer" "how many events, default 10, at most 25"))
    ("title" . ("string" "event title, e.g. Strategy consult"))
    ("start" . ("string" "when it starts, RFC3339 or YYYY-MM-DDTHH:MM at utc_offset"))
    ("end" . ("string" "when it ends; left out, the consult length or the event's own length"))
    ("description" . ("string" "the event body"))
    ("attendees" . ("array" "email addresses that receive an invitation"))
    ("event_id" . ("string" "an event id from booking_list_upcoming"))
    ("confirm" . ("boolean" "true, and only after the operator agreed to this exact action")))
  "Each argument's JSON type and what it means, in the order its tool lists them.")

(defparameter *tools*
  '(("booking_list_availability"
     "Find genuinely free slots on the operator's Google Calendar (FreeBusy). Never guess availability; call this first. The window is capped at 14 days."
     ("time_min" "time_max" "duration_minutes" "utc_offset" "calendar_id")
     ("time_min" "time_max"))
    ("booking_list_upcoming"
     "List the next calendar events with their ids, titles, times, links and status."
     ("max_results" "time_min" "calendar_id")
     ())
    ("booking_book_event"
     "Book a consult on the calendar. Re-checks FreeBusy and refuses on a conflict. Requires confirm=true, set only after the operator agreed to this exact time."
     ("title" "start" "end" "utc_offset" "description" "attendees" "calendar_id" "confirm")
     ("title" "start" "confirm"))
    ("booking_reschedule_event"
     "Move an existing event to a new free time. Re-checks FreeBusy, ignoring the event itself. Requires confirm=true."
     ("event_id" "start" "end" "utc_offset" "calendar_id" "confirm")
     ("event_id" "start" "confirm"))
    ("booking_cancel_event"
     "Cancel (delete) a calendar event by id. Requires confirm=true, set only after the operator agreed."
     ("event_id" "calendar_id" "confirm")
     ("event_id" "confirm")))
  "Each tool this cell offers: its name, its description, its arguments, its required ones.")

(defun tool-schema (name)
  "NAME's schema: one JSON-schema object over the argument table above."
  (let ((entry (assoc name *tools* :test #'string=)))
    (unless entry
      (fail "~s is not one of this cell's tools" name))
    (list :description (second entry)
          :parameters
          (nlk:json-object
           "type" "object"
           "properties"
           (nlk:make-json-object
            (loop for argument in (third entry)
                  append (list argument
                               (destructuring-bind (type doc) (cdr (assoc argument *argument-docs* :test #'string=))
                                 (nlk:json-object "type" type "description" doc)))))
           "required" (coerce (fourth entry) 'vector)))))

;;; --- reading a call -----------------------------------------------------------------

(defun argument (arguments name)
  "NAME's value in ARGUMENTS when it is a string."
  (text-field arguments name))

(defun required-argument (arguments name)
  "NAME's value, trimmed, or a refusal naming it."
  (or (nonblank (argument arguments name))
      (fail "~a is required" name)))

(defun offset-for (arguments)
  "The offset seconds east of UTC this call reads and writes times at: its
utc_offset, else the section's, else this machine's zone now."
  (let ((text (nonblank (argument arguments "utc_offset"))))
    (cond (text (parse-offset text))
          ((setting :zone-offset))
          (t (machine-offset (now-seconds))))))

(defun calendar-for (arguments)
  "Which calendar this call means: its calendar_id, else the section's."
  (or (nonblank (argument arguments "calendar_id")) (setting :calendar)))

(defun confirmed (arguments)
  "True when the model set confirm: true, which it does after the operator agreed."
  (eq t (and (hash-table-p arguments) (gethash "confirm" arguments))))

(defun guard (arguments)
  "A refusal when this write was not confirmed, else NIL."
  (unless (confirmed arguments)
    (fail "Not done: confirm must be true, and only after the operator agreed to this exact action")))

(defun slim (event)
  "EVENT as an answer: id, title, times, link, status."
  (nlk:json-object
   "event_id" (text-field event "id")
   "title" (or (text-field event "summary") "")
   "start" (or (text-field (gethash "start" event) "dateTime") (text-field (gethash "start" event) "date"))
   "end" (or (text-field (gethash "end" event) "dateTime") (text-field (gethash "end" event) "date"))
   "htmlLink" (text-field event "htmlLink")
   "status" (text-field event "status")))

(defun event-seconds (event side)
  "The instant EVENT's SIDE (\"start\" or \"end\") carries."
  (let ((stamp (or (text-field (gethash side event) "dateTime") (text-field (gethash side event) "date"))))
    (unless stamp
      (fail "Google returned no ~a for that event" side))
    (parse-time stamp)))

(defun key-hash (text)
  "TEXT's FNV-1a hash, eight hex digits: the idempotency mark an event carries."
  (let ((hash 2166136261))
    (loop for char across text
          do (setf hash (logand (* (logxor hash (char-code char)) 16777619) #xFFFFFFFF)))
    (format nil "~8,'0X" hash)))

(defun find-duplicate (calendar start key)
  "The event already carrying KEY, when this exact booking ran before."
  (loop for event in (list-events calendar (1- start) 10)
        for mark = (private-mark event)
        when (equal mark key) return event))

(defun private-mark (event)
  "EVENT's bookingBridgeKey, or NIL when it carries none."
  (text-field (and (hash-table-p (gethash "extendedProperties" event))
                    (gethash "private" (gethash "extendedProperties" event)))
              "bookingBridgeKey"))

(defun answer (object)
  "OBJECT as the text a tool answers with."
  (nlk:encode-json-object object))

(defmacro refusing-on-error (&body body)
  "BODY's object as JSON text; a BOOKING-ERROR is an answer, never a signal."
  `(handler-case (answer (progn ,@body))
     (booking-error (condition) (answer (nlk:json-object "ok" nil "error" (booking-error-detail condition))))
     (error (condition)
       (answer (nlk:json-object "ok" nil "error" (format nil "~a: ~a" (type-of condition) condition))))))

;;; --- the five tools -----------------------------------------------------------------

(defun list-availability (arguments)
  "Really free slots from Google's FreeBusy answer, cut back to back."
  (refusing-on-error
    (let ((offset (offset-for arguments))
          (minutes (or (number-field arguments "duration_minutes") (setting :duration-minutes))))
      (when (or (< minutes 5) (> minutes 480))
        (fail "duration_minutes must be between 5 and 480"))
      (multiple-value-bind (start start-offset)
          (parse-time (required-argument arguments "time_min") (offset-for arguments))
        (declare (ignore start-offset))
        (let* ((end (parse-time (required-argument arguments "time_max") offset))
               (calendar (calendar-for arguments)))
          (when (<= end start)
            (fail "time_max must be after time_min"))
          (when (> (- end start) (* *max-window-days* +seconds-per-day+))
            (fail "Window too large: ask for at most ~d days at a time" *max-window-days*))
          (let* ((busy (free-busy start end calendar))
                 (slots (free-slots start end busy (* minutes 60)))
                 (shown (subseq slots 0 (min *max-slots* (length slots)))))
            (nlk:json-object
             "ok" t
             "calendar_id" calendar
             "utc_offset" (format-offset offset)
             "duration_minutes" minutes
             "slots" (coerce (loop for (slot-start . slot-end) in shown
                                    collect (nlk:json-object "start" (format-time slot-start offset)
                                                             "end" (format-time slot-end offset)))
                             'vector)
             "truncated" (> (length slots) *max-slots*))))))))

(defun list-upcoming (arguments)
  "The next events from now, or from the time_min the call names."
  (refusing-on-error
    (let* ((calendar (calendar-for arguments))
           (count (max 1 (min (or (number-field arguments "max_results") 10) 25)))
           (start (if (nonblank (argument arguments "time_min"))
                      (parse-time (argument arguments "time_min") (offset-for arguments))
                      (now-seconds))))
      (nlk:json-object
       "ok" t
       "calendar_id" calendar
       "events" (coerce (loop for event in (list-events calendar start count)
                              collect (slim event))
                        'vector)))))

(defun book-event (arguments)
  "One consult, re-checked for conflicts and idempotent on a retry."
  (refusing-on-error
    (guard arguments)
    (let* ((offset (offset-for arguments))
           (calendar (calendar-for arguments))
           (title (subseq (required-argument arguments "title") 0
                          (min 200 (length (required-argument arguments "title")))))
           (start (parse-time (required-argument arguments "start") offset))
           (end (if (nonblank (argument arguments "end"))
                    (parse-time (argument arguments "end") offset)
                    (+ start (* 60 (setting :duration-minutes)))))
           (attendees (remove nil (loop for address in (items-field arguments "attendees")
                                        for text = (and (stringp address) (nlk:trimmed address))
                                        when (and text (search "@" text)) collect text)))
           (key (key-hash (format nil "~a|~a|~a|~a|~{~a~^,~}"
                                   calendar title (format-time start 0) (format-time end 0)
                                   (sort (copy-list attendees) #'string<)))))
      (when (<= end start)
        (fail "end must be after start"))
      (when (< start (now-seconds))
        (fail "That start time is in the past"))
      (let ((existing (find-duplicate calendar start key)))
        (if existing
            (nlk:json-object "ok" t "duplicate" t
                             "note" "Already booked; nothing new was created."
                             "event" (slim existing))
            (progn
              (let ((busy (free-busy start end calendar)))
                (when busy
                  (fail "That time is no longer free; nothing was booked~@[: (~{~a~^, ~})~]"
                        (loop for (busy-start . busy-end) in busy
                              collect (format nil "~a to ~a" (format-time busy-start offset)
                                              (format-time busy-end offset))))))
              (let ((body (nlk:json-object
                           "summary" title
                           "start" (nlk:json-object "dateTime" (format-time start offset))
                           "end" (nlk:json-object "dateTime" (format-time end offset))
                           "extendedProperties" (nlk:json-object
                                                 "private" (nlk:json-object "bookingBridgeKey" key)))))
                (when (nonblank (argument arguments "description"))
                  (setf (gethash "description" body)
                        (subseq (argument arguments "description") 0
                                (min 4000 (length (argument arguments "description"))))))
                (when attendees
                  (setf (gethash "attendees" body)
                        (coerce (loop for address in attendees
                                       collect (nlk:json-object "email" address))
                                'vector)))
                (nlk:json-object "ok" t "event" (slim (insert-event calendar body))))))))))

(defun reschedule-event (arguments)
  "One event moved to a time that is free, its length kept when end is left out."
  (refusing-on-error
    (guard arguments)
    (let* ((offset (offset-for arguments))
           (calendar (calendar-for arguments))
           (event-id (required-argument arguments "event_id"))
           (event (get-event calendar event-id))
           (old-start (event-seconds event "start"))
           (old-end (event-seconds event "end"))
           (start (parse-time (required-argument arguments "start") offset))
           (end (if (nonblank (argument arguments "end"))
                    (parse-time (argument arguments "end") offset)
                    (+ start (- old-end old-start)))))
      (when (<= end start)
        (fail "end must be after start"))
      (let ((busy (remove-if (lambda (interval)
                               (and (= (car interval) old-start) (= (cdr interval) old-end)))
                             (free-busy start end calendar))))
        (when busy
          (fail "That time is not free; nothing was moved~@[: (~{~a~^, ~})~]"
                (loop for (busy-start . busy-end) in busy
                      collect (format nil "~a to ~a" (format-time busy-start offset) (format-time busy-end offset))))))
      (nlk:json-object
       "ok" t
       "event" (slim (patch-event calendar event-id
                                   (nlk:json-object
                                    "start" (nlk:json-object "dateTime" (format-time start offset))
                                    "end" (nlk:json-object "dateTime" (format-time end offset)))))))))

(defun cancel-event (arguments)
  "One event deleted, its title read first so the answer can name it."
  (refusing-on-error
    (guard arguments)
    (let* ((calendar (calendar-for arguments))
           (event-id (required-argument arguments "event_id"))
           (event (get-event calendar event-id)))
      (delete-event calendar event-id)
      (nlk:json-object "ok" t "cancelled" t "event_id" event-id
                       "title" (or (text-field event "summary") "")))))

;;; --- the command ---------------------------------------------------------------------

(defun status-line ()
  "What /booking says: whether the grant answers, and what is coming."
  (handler-case
      (let* ((text (list-upcoming (nlk:make-json-object "max_results" 5)))
             (data (json-text text))
             (events (items-field data "events")))
        (if (and (hash-table-p data) (eq t (gethash "ok" data)))
            (with-output-to-string (out)
              (format out "booking: signed in to Google Calendar, calendar ~a. Next events:~%"
                      (or (text-field data "calendar_id") (setting :calendar)))
              (if events
                  (dolist (event events)
                    (format out "- ~a  ~a~%" (or (text-field event "start") "?") (or (text-field event "title") "")))
                  (format out "- none upcoming~%")))
            (format nil "booking: ~a" (or (text-field data "error") "no answer"))))
    (error (condition)
      (format nil "booking: could not read the calendar (~a)" (type-of condition)))))

(defun run-slash (args session-id)
  "The /booking command: its status, its manual, or what it answers."
  (declare (ignore session-id))
  (let ((verb (string-downcase (or (first (nlk:split-words (or args ""))) ""))))
    (cond
      ((member verb '("" "status") :test #'string=)
       (values (status-line)
               (nle:list-dialog
                :title "Booking" :summary (status-line)
                :rows (list (nle:run-row "What is free" "/booking status"
                                         :detail "the grant's answer and the next five events")
                            (nle:run-row "The manual" "/booking help"
                                         :detail "the five tools, the confirm rule, setup")))))
      ((string= verb "help") (values *manual* nil))
      (t (values (format nil "booking: ~a is not one of its words; /booking alone says whether the grant answers, /booking help has the manual"
                         verb)
                 nil)))))

;;; --- reading the section ---------------------------------------------------------------

(defun secret-from (values name)
  "The secret the section's NAME variable holds, read once here, at start."
  (let ((variable (or (nonblank (getf values name)) "")))
    (unless (plusp (length variable))
      (nlk:config-error "~(~a~) names no variable: set it to the name of the one holding the secret" name))
    (or (nonblank (nle::credential-env variable))
        (nlk:config-error "~a is unset or empty: put the value there, or name another variable in ~(~a~)"
                          variable name))))

(defun read-settings (values table)
  "The declared VALUES, plus the two secrets the section names and the zone."
  (declare (ignore table))
  (let* ((client-id (or (nonblank (getf values :client-id)) ""))
         (offset-text (nonblank (getf values :utc-offset)))
         (plist (copy-list values)))
    (unless (plusp (length client-id))
      (nlk:config-error "client_id is unset: put the Google OAuth Desktop app client id there"))
    (setf (getf plist :client-id) client-id
          (getf plist :client-secret) (secret-from values :client-secret-env)
          (getf plist :refresh-token) (secret-from values :refresh-token-env)
          (getf plist :zone-offset) (and offset-text (parse-offset offset-text))
          (getf plist :timeout) (or (getf values :timeout) 20)
          (getf plist :duration-minutes) (or (getf values :duration-minutes) 30)
          (getf plist :calendar) (or (nonblank (getf values :calendar)) "primary"))
    plist))

;;; --- the cell -------------------------------------------------------------------------

(nle:define-cell booking
  (:section ("booking")
    (:guide "where to go: a Google Cloud project with the Calendar API on, an OAuth client of type Desktop app, then a refresh token for calendar.events and calendar.freebusy exported in the shell that starts Nodecode; first step: give client_id and the two variables a value")
    ("calendar" :string :default "primary" :doc "calendar id to read and write")
    ("client_id" :string :doc "Google OAuth Desktop app client id")
    ("client_secret_env" :env :default "GOOGLE_CALENDAR_CLIENT_SECRET"
     :doc "environment variable holding that OAuth client's secret")
    ("refresh_token_env" :env :default "GOOGLE_CALENDAR_REFRESH_TOKEN"
     :doc "environment variable holding the refresh token for calendar.events and calendar.freebusy")
    ("utc_offset" :string :default "" :doc "the offset times are read and written at, e.g. -06:00; empty is this machine's zone")
    ("duration_minutes" :integer :default 30 :min 5 :doc "consult length, when a call names none")
    ("timeout" :integer :default 20 :min 5 :doc "seconds one call to Google may take"))
  (:settings #'read-settings)
  (:help :booking *summary* *manual*)
  (:command "booking" 'run-slash
            :description "Google Calendar: whether the grant answers, the next events, the manual"
            :argument-hint "status | help")
  (:tool "booking_list_availability" (tool-schema "booking_list_availability") 'list-availability)
  (:tool "booking_list_upcoming" (tool-schema "booking_list_upcoming") 'list-upcoming)
  (:tool "booking_book_event" (tool-schema "booking_book_event") 'book-event)
  (:tool "booking_reschedule_event" (tool-schema "booking_reschedule_event") 'reschedule-event)
  (:tool "booking_cancel_event" (tool-schema "booking_cancel_event") 'cancel-event))
