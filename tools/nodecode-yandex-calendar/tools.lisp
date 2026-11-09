;;;; tools.lisp --- the seven verbs, their arguments, and the JSON they answer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every tool answers a string of JSON and none of them signals: a failure
;;;; becomes {"error": "..."} the model can read and repair from, which is what
;;;; the Hermes plugin did with its handlers. The declarations below are the
;;;; one place a call's shape lives: the schema the model is shown and the
;;;; record the body reads are both projected from them.
;;;;
;;;; An absent member is NIL and an empty string is "", so an update can tell
;;;; "leave it alone" from "clear it": a text field set to "" clears it. A
;;;; boolean cannot tell those apart, so the two tri-state switches (all_day,
;;;; busy) are choices of keep/true/false.

(in-package #:nodecode-yandex-calendar)

;;; --- what a call may say ------------------------------------------------------

(nlk:define-contract list-calendars-arguments
  (:doc "The calendars this account holds, the default one first.")
  ("name" :string
   :doc "Optional: only the calendars whose name or href holds this text."))

(nlk:define-contract list-events-arguments
  (:doc "The time range to read, and which calendar to read it from.")
  ("start" :string
   :doc "Range start: '2026-07-25' or '2026-07-25T14:00:00+03:00'. Defaults to now.")
  ("end" :string
   :doc "Range end, same forms. Defaults to seven days after the start.")
  ("calendar" :string
   :doc "The calendar's name, the last segment of its href, or the href itself.
Omit for the default calendar (the first one listed)."))

(nlk:define-contract create-event-arguments
  (:doc "A new event. Times may carry an offset; a time without one is read as UTC.")
  ("summary" :text :required t :doc "The event's title.")
  ("start" :text :required t
   :doc "'2026-07-25' for an all-day event or '2026-07-25T14:00:00+03:00' for a timed one.")
  ("end" :string
   :doc "The end, the same kind of value as start. Defaults to an hour later, or
the next day for an all-day event. A whole-day end is the day after the last one.")
  ("location" :string :doc "Where it happens.")
  ("description" :string :doc "Notes for the event.")
  ("all_day" :boolean :default nil :doc "A whole-day event; implied by a bare date start.")
  ("calendar" :string :doc "Which calendar to put it in; omit for the default one.")
  ("attendees" :array :of :string
   :doc "People to invite, each 'a@x.ru' or 'Name <a@x.ru>'. Inviting makes the
server mail them from this account.")
  ("free" :boolean :default nil
   :doc "Show the time as free rather than busy."))

(nlk:define-contract update-event-arguments
  (:doc "Changes to one event, named by the href a listing returned.
Only the members given are applied; an empty string clears a text field.")
  ("event_href" :text :required t :doc "The event resource, as a listing returned it.")
  ("summary" :string :doc "A new title; an empty string clears it.")
  ("start" :string :doc "A new start, as a date or a date-time with an offset.")
  ("end" :string :doc "A new end, the same kind of value as the start.")
  ("location" :string :doc "A new location; an empty string clears it.")
  ("description" :string :doc "New notes; an empty string clears them.")
  ("all_day" :string :options '("keep" "true" "false") :default "keep"
   :doc "Whether the event is a whole-day one: keep, true or false.")
  ("busy" :string :options '("keep" "true" "false") :default "keep"
   :doc "Whether the time reads busy or free: keep, true or false.")
  ("add_attendees" :array :of :string
   :doc "People to invite, each 'a@x.ru' or 'Name <a@x.ru>'; the server mails them.")
  ("remove_attendees" :array :of :string :doc "Addresses to take off the event."))

(nlk:define-contract respond-event-arguments
  (:doc "An answer to a meeting invitation, as its account owner.")
  ("event_href" :text :required t :doc "The event resource, as a listing returned it.")
  ("response" :text :required t :options '("accept" "decline" "tentative")
   :doc "How to answer the invitation."))

(nlk:define-contract move-event-arguments
  (:doc "Move an event to another calendar, with its attendees and alarms.")
  ("event_href" :text :required t :doc "The event resource, as a listing returned it.")
  ("calendar" :text :required t
   :doc "The destination calendar: its name, a segment of its href, or the href."))

(nlk:define-contract delete-event-arguments
  (:doc "Delete one event; an href that is already gone reads as deleted.")
  ("event_href" :text :required t :doc "The event resource, as a listing returned it."))

;;; --- the answers ----------------------------------------------------------------

(defun json-text (object)
  "OBJECT, a JSON-OBJECT, as the string a tool answers."
  (nlk:encode-json-object object))

(defun json-error (text)
  "The one thing a failed call says."
  (json-text (nlk:json-object "error" text)))

(defmacro answering (what &body body)
  "BODY's JSON answer, or {\"error\": ...} naming WHAT — no tool ever signals."
  `(handler-case (progn ,@body)
     (caldav-error (condition) (json-error (caldav-error-detail condition)))
     (error (condition) (json-error (format nil "~a failed: ~a" ,what condition)))))

(defun attendee-json (attendee)
  "ATTENDEE as the object a listing shows."
  (nlk:json-object "email" (attendee-email attendee)
                   "name" (attendee-name attendee)
                   "role" (attendee-role attendee)
                   "partstat" (attendee-partstat attendee)))

(defun event-json (event)
  "EVENT as the object a listing shows: the times as ISO 8601, the href to reuse."
  (nlk:json-object "uid" (event-uid event)
                   "summary" (event-summary event)
                   :opt "start" (and (event-start event) (moment-iso (event-start event)))
                   :opt "end" (and (event-end event) (moment-iso (event-end event)))
                   "location" (event-location event)
                   "description" (event-description event)
                   "all_day" (not (null (event-all-day event)))
                   :when (plusp (length (event-transp event)))
                   "busy" (not (string= (event-transp event) *transp-free*))
                   :opt "organizer" (and (event-organizer event)
                                          (attendee-json (event-organizer event)))
                   "attendees" (coerce (mapcar #'attendee-json (event-attendees event)) 'vector)
                   "href" (event-href event)))

(defun sorted-events (events)
  "EVENTS oldest first, the ones with no start last."
  (stable-sort (copy-list events)
               (lambda (a b)
                 (let ((one (event-start a)) (two (event-start b)))
                   (cond ((and one two) (moment< one two))
                         (one t)
                         (t nil))))))

(defmacro event-list-json (events &key (key "events"))
  "EVENTS as the object a listing answers, under KEY."
  `(nlk:json-object "count" (length ,events)
                    ,key (coerce (mapcar #'event-json (sorted-events ,events)) 'vector)))

;;; --- times and people ----------------------------------------------------------------

(defun moment-add-seconds (moment seconds)
  "MOMENT moved by SECONDS, in UTC."
  (epoch-moment (+ (moment-epoch moment) seconds)))

(defun iso-or (text moment &key (what "date/time"))
  "MOMENT when TEXT is blank, else TEXT parsed; CALDAV-ERROR names WHAT."
  (if (and text (plusp (length (string-trim '(#\Space #\Tab) text))))
      (parse-iso-moment text :what what)
      moment))

(defun check-span (start end all-day)
  "Refuse a span that does not run forwards, before it costs a round trip."
  (when (and start end)
    (cond
      ((or all-day (moment-all-day start) (moment-all-day end))
       (when (< (moment-epoch end) (+ (moment-epoch start) 86400))
         (caldav-fail "An all-day event must end on a later day than it starts: ~a to ~a. ~
The end is the day after the last one it covers."
                      (moment-iso start) (moment-iso end))))
      (t (when (and (= (if (moment-all-day start) 1 0) (if (moment-all-day end) 1 0))
                    (not (moment< start end)))
           (caldav-fail "An event must end after it starts: ~a to ~a."
                        (moment-iso start) (moment-iso end)))))))

(defun parse-attendee (item)
  "ITEM, 'a@x.ru' or 'Name <a@x.ru>', as an ATTENDEE; CALDAV-ERROR for anything else."
  (let* ((text (string-trim '(#\Space #\Tab) (or item "")))
         (open (position #\< text))
         (close (position #\> text))
         (name (if (and open close (< open close)) (string-trim " " (subseq text 0 open)) ""))
         (email (cond ((and open close (< open close)) (string-trim " " (subseq text (1+ open) close)))
                      (t text))))
    (when (and (>= (length email) 7) (string-equal "mailto:" email :end2 7))
      (setf email (subseq email 7)))
    (when (or (zerop (length email))
              (some (lambda (char) (<= (char-code char) #x20)) email)
              (null (position #\@ email))
              (zerop (position #\@ email))
              (= (1+ (position #\@ email)) (length email)))
      (caldav-fail "Invalid attendee email address: ~s" item))
    (make-attendee :email email :name name)))

;;; --- the verbs -------------------------------------------------------------------------

(defun list-calendars (arguments)
  "The calendars this account holds, the default one first."
  (answering "listing calendars"
    (let* ((filter (string-downcase (or (list-calendars-arguments-name arguments) "")))
           (calendars (remove-if-not
                       (lambda (calendar)
                         (or (zerop (length filter))
                             (search filter (string-downcase (cdr calendar)))
                             (search filter (string-downcase (car calendar)))))
                       (client-calendars (client-for-settings)))))
      (json-text
       (nlk:json-object
        "count" (length calendars)
        "calendars" (coerce (loop for (href . name) in calendars
                                   collect (nlk:json-object "name" name "href" href))
                             'vector))))))

(defun list-events (arguments)
  "The events in a time range, oldest first, each with the href to change it by."
  (answering "listing events"
    (let* ((start (iso-or (list-events-arguments-start arguments) (now-moment)
                          :what "start"))
           (end (iso-or (list-events-arguments-end arguments)
                        (moment-add-seconds start (* 7 86400))
                        :what "end"))
           (client (client-for-settings))
           (calendar (list-events-arguments-calendar arguments)))
      (json-text (event-list-json (query-events client start end :calendar calendar))))))

(defun create-event (arguments)
  "Create one event and answer it as it was stored."
  (answering "creating the event"
    (let* ((start (parse-iso-moment (create-event-arguments-start arguments) :what "start"))
           (end (iso-or (create-event-arguments-end arguments)
                        (moment-add-seconds start (if (moment-all-day start) 86400 3600))
                        :what "end"))
           (all-day (or (create-event-arguments-all-day arguments)
                        (moment-all-day start)
                        (moment-all-day end)))
           (event (make-ical-event)))
      (setf (event-summary event) (string-trim '(#\Space #\Tab)
                                                (create-event-arguments-summary arguments))
            (event-location event) (or (create-event-arguments-location arguments) "")
            (event-description event) (or (create-event-arguments-description arguments) "")
            (event-attendees event)
            (mapcar #'parse-attendee
                    (coerce (or (create-event-arguments-attendees arguments) #()) 'list)))
      (set-event-start event start)
      (set-event-end event end)
      (set-event-all-day event all-day)
      (when (create-event-arguments-free arguments)
        (setf (event-transp event) *transp-free*))
      (check-span start end all-day)
      (let ((created (put-new-event (client-for-settings) event
                                     :calendar (create-event-arguments-calendar arguments))))
        (json-text (nlk:json-object "created" t "event" (event-json created)))))))

(defun apply-attendee-updates (event arguments)
  "Add and remove attendees, matching addresses case-insensitively."
  (dolist (item (coerce (or (update-event-arguments-add-attendees arguments) #()) 'list))
    (let ((new (parse-attendee item)))
      (unless (find (attendee-email new) (event-attendees event)
                    :key #'attendee-email :test #'string-equal)
        (push new (event-attendees event)))))
  (let ((remove (mapcar (lambda (item) (string-downcase (string-trim " " item)))
                        (coerce (or (update-event-arguments-remove-attendees arguments) #())
                                'list))))
    (when remove
      (setf (event-attendees event)
            (remove-if (lambda (attendee)
                         (member (string-downcase (attendee-email attendee)) remove
                                 :test #'string=))
                       (event-attendees event))))))

(defun apply-updates (event arguments)
  "Apply the members the call actually carried to EVENT."
  (let ((summary (update-event-arguments-summary arguments))
        (location (update-event-arguments-location arguments))
        (description (update-event-arguments-description arguments)))
    (when summary (setf (event-summary event) (string-trim '(#\Space #\Tab) summary)))
    (when location (setf (event-location event) (string-trim '(#\Space #\Tab) location)))
    (when description (setf (event-description event) (string-trim '(#\Space #\Tab) description))))
  (let ((all-day (update-event-arguments-all-day arguments))
        (start (update-event-arguments-start arguments))
        (end (update-event-arguments-end arguments))
        (busy (update-event-arguments-busy arguments)))
    (when (member all-day '("true" "false") :test #'string-equal)
      (set-event-all-day event (string-equal all-day "true")))
    (when start (set-event-start event (parse-iso-moment start :what "start")))
    (when end (set-event-end event (parse-iso-moment end :what "end")))
    (when (and (event-all-day event) (event-start event))
      (when (or (null (event-end event))
                (<= (moment-epoch (event-end event))
                    (moment-epoch (event-start event))))
        (set-event-end event (moment-add-seconds (event-start event) 86400))))
    (when (member busy '("true" "false") :test #'string-equal)
      (setf (event-transp event) (if (string-equal busy "true") *transp-busy* *transp-free*)))
    (apply-attendee-updates event arguments)))

(defun update-event (arguments)
  "Change the members this call carried and answer the event as stored."
  (answering "updating the event"
    (let* ((client (client-for-settings))
           (href (update-event-arguments-event-href arguments))
           (event (or (fetch-event client href) (caldav-fail "Event not found: ~a" href))))
      (apply-updates event arguments)
      (check-span (event-start event) (event-end event) (event-all-day event))
      (json-text (nlk:json-object "updated" t
                                   "event" (event-json (put-event client event href)))))))

(defun respond-event (arguments)
  "Answer an invitation as the account owner."
  (answering "responding to the event"
    (let* ((partstat (cond ((string-equal (respond-event-arguments-response arguments) "accept")
                            "ACCEPTED")
                           ((string-equal (respond-event-arguments-response arguments) "decline")
                            "DECLINED")
                           (t "TENTATIVE")))
           (updated (answer-invitation (client-for-settings)
                                       (respond-event-arguments-event-href arguments)
                                       partstat)))
      (json-text (nlk:json-object "responded" t "status" partstat
                                   "event" (event-json updated))))))

(defun move-event (arguments)
  "Move one event into another calendar."
  (answering "moving the event"
    (let ((moved (relocate-event (client-for-settings)
                                 (move-event-arguments-event-href arguments)
                                 (move-event-arguments-calendar arguments))))
      (json-text (nlk:json-object "moved" t "event" (event-json moved))))))

(defun delete-event (arguments)
  "Delete one event by its href."
  (answering "deleting the event"
    (let ((href (delete-event-arguments-event-href arguments)))
      (remove-event (client-for-settings) href)
      (json-text (nlk:json-object "deleted" t "event_href" href)))))
