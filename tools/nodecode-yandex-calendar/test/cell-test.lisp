;;;; cell-test.lisp --- the lifecycle, and the seven tools over a stubbed server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test here touches the network: every exchange is a stubbed NLK:HTTP
;;;; answering the document the verb it was called with would get. The cell is
;;;; started from a literal section, as the gateway starts it.

(in-package #:nodecode.test)

(define-test-slice "yandex-calendar" "YANDEX-CALENDAR-CELL-")

;; The section this cell reads is `yandex_calendar', so the test builds that
;; one: DEFINE-TEST-SLICE's own pair would key it by the slice's name.
(defun yandex-calendar-config (&rest pairs)
  "A whole shared config holding one `yandex_calendar' section built from PAIRS."
  (nlk:json-object "yandex_calendar" (apply #'nlk:make-json-object pairs)))

(defun yandex-calendar-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-yandex-calendar:start-cell (apply #'yandex-calendar-config pairs)))

(defparameter *asked* nil
  "Every stubbed exchange of the test, (METHOD URL . ARGS), newest first.")

(defun octets (text)
  "TEXT as the UTF-8 octets a response body is."
  (sb-ext:string-to-octets text :external-format :utf-8))

(defun ics (lines)
  "LINES as one iCalendar document, each on its own line."
  (format nil "~{~a~%~}" lines))

(defparameter +calendars-xml+
  (concatenate 'string
               "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
               "<d:multistatus xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\">"
               "<d:response><d:href>/calendars/me%40yandex.ru/</d:href>"
               "<d:propstat><d:prop><d:displayname>Home</d:displayname>"
               "<d:resourcetype><d:collection/><c:calendar/></d:resourcetype>"
               "</d:prop></d:propstat></d:response>"
               "<d:response><d:href>/calendars/me%40yandex.ru/holidays/</d:href>"
               "<d:propstat><d:prop><d:displayname>Holidays</d:displayname>"
               "<d:resourcetype><d:collection/><c:calendar/></d:resourcetype>"
               "</d:prop></d:propstat></d:response>"
               "</d:multistatus>"))

(defparameter +event-ics+
  (ics '("BEGIN:VCALENDAR" "VERSION:2.0" "PRODID:-//Yandex//EN"
         "BEGIN:VEVENT" "UID:e1@yandex.ru" "DTSTAMP:20260701T000000Z"
         "DTSTART:20260725T110000Z" "DTEND:20260725T120000Z"
         "SUMMARY:Standup"
         "ATTENDEE;CN=Me;PARTSTAT=NEEDS-ACTION:mailto:me@yandex.ru"
         "ORGANIZER;CN=Ann:mailto:ann@example.org"
         "END:VEVENT" "END:VCALENDAR")))

(defparameter +events-xml+
  (concatenate 'string
               "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
               "<d:multistatus xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\">"
               "<d:response><d:href>/calendars/me%40yandex.ru/e1.ics</d:href>"
               "<d:propstat><d:prop><d:getetag>\"7\"</d:getetag>"
               "<c:calendar-data>"
               (with-output-to-string (out)
                 (dolist (line (uiop:split-string +event-ics+ :separator '(#\Newline)))
                   (write-string line out)
                   (write-char #\Newline out)))
               "</c:calendar-data></d:prop></d:propstat></d:response>"
               "</d:multistatus>"))

(defun stub-answer (method)
  "The (values BODY STATUS HEADERS) a CalDAV server answers METHOD with."
  (case method
    (:propfind (values (octets +calendars-xml+) 207 nil))
    (:report (values (octets +events-xml+) 207 nil))
    (:get (values (octets +event-ics+) 200 '(("ETag" . "\"7\""))))
    (:put (values (octets "") 201 '(("ETag" . "\"8\""))))
    (:delete (values (octets "") 204 nil))
    (t (values (octets "") 500 nil))))

(defmacro with-caldav (&body body)
  "BODY with NLK:HTTP stubbed to a CalDAV server, every exchange logged."
  `(progn
     (setf *asked* '())
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *asked*)
                                (stub-answer method))
       ,@body)))

(defun asked-for (method)
  "The newest logged exchange whose METHOD is METHOD."
  (find method *asked* :key #'first))

(defun asked-body (exchange)
  "The CONTENT of EXCHANGE, a logged call."
  (getf (cddr exchange) :content))

(defun asked-headers (exchange)
  "The HEADERS of EXCHANGE, a logged call."
  (getf (cddr exchange) :headers))

(defun answered (name json)
  "The answer of the registered tool NAME called with the JSON object JSON."
  (let* ((entry (or (nle::find-tool name)
                    (error "~a is not registered" name)))
         (table (nlk:decode-json json)))
    (funcall (third entry) (nle::tool-body-arguments entry table))))

(defun answer-value (text key)
  "TEXT, a tool's JSON answer, read at KEY."
  (nlk:json-value (nlk:decode-json text) :any key))

(define-cell-lifecycle-tests "yandex-calendar"
  (:config "login" "me@yandex.ru" "app_password" "pw")
  (:help :yandex-calendar)
  (:running
   (is (= 7 (loop for (name) in nle::*tools*
                  when (uiop:string-prefix-p "yandex_calendar_" name) count t)) "every verb is registered"))
  (:stopped
   (is (null (nle::find-tool "yandex_calendar_list_events")) "and stop unregisters them"))
  (:refused ("login" "me@yandex.ru" "app_password" "pw" "base_url" "http://insecure.example")))

(deftest yandex-calendar-cell-registers-only-the-actions-allowed ()
  (with-cell-stop ((yandex-calendar-start "login" "me@yandex.ru" "app_password" "pw"
                                           "actions" #("read")))
    (is (nle::find-tool "yandex_calendar_list_events") "a read verb is registered")
    (is (null (nle::find-tool "yandex_calendar_delete_event")) "a delete verb is not, so the model never sees it")))

(deftest yandex-calendar-cell-lists-calendars-and-events ()
  (with-cell-stop ((yandex-calendar-start "login" "me@yandex.ru" "app_password" "pw"))
    (with-caldav
      (let ((answer (answered "yandex_calendar_list_calendars" "{}")))
        (is (equal 2 (answer-value answer "count")) "both calendars are listed")
        (is (search "Home" answer) "by display name")
        (is (string= "PROPFIND" (string-upcase (string (first (first *asked*))))) "one PROPFIND on the account's home"))
      (let ((answer (answered "yandex_calendar_list_events"
                              "{\"start\":\"2026-07-25\",\"end\":\"2026-07-26\"}")))
        (is (equal 1 (answer-value answer "count")) "the one event in range")
        (is (search "Standup" answer) "with its title")
        (is (search "/calendars/me@yandex.ru/e1.ics" answer) "and the href to change it by")
        (is (search "2026-07-25T11:00:00+00:00" answer) "and its time, as ISO 8601")
        (let ((report (asked-for :report)))
          (is report "the listing was a REPORT")
          (is (search "20260725T000000Z" (asked-body report)) "the range is written in UTC"))))))

(deftest yandex-calendar-cell-creates-an-event ()
  (with-cell-stop ((yandex-calendar-start "login" "me@yandex.ru" "app_password" "pw"))
    (with-caldav
      (let ((answer (answered "yandex_calendar_create_event"
                               "{\"summary\":\"Planning\",\"start\":\"2026-07-25T12:00:00+03:00\",
                                 \"attendees\":[\"Ann <ann@example.org>\"],
                                 \"calendar\":\"Home\"}")))
        (is (eq t (answer-value answer "created")) "the call succeeded")
        (let ((put (asked-for :put)))
          (is put "one PUT was sent")
          (is (equal "*" (cdr (assoc "If-None-Match" (asked-headers put) :test #'string-equal))) "a create cannot overwrite an existing resource")
          (is (search "SUMMARY:Planning" (asked-body put)) "the title is in the body")
          (is (search "DTSTART:20260725T090000Z" (asked-body put)) "a +03:00 start is written as its UTC instant")
          (is (search "ORGANIZER" (asked-body put)) "an invite names its organizer"))))))

(deftest yandex-calendar-cell-updates-answers-moves-and-deletes ()
  (with-cell-stop ((yandex-calendar-start "login" "me@yandex.ru" "app_password" "pw"))
    (with-caldav
      (let ((answer (answered "yandex_calendar_update_event"
                               "{\"event_href\":\"/calendars/me%40yandex.ru/e1.ics\",
                                 \"summary\":\"Standup, longer\",\"busy\":\"false\"}")))
        (is (eq t (answer-value answer "updated")) "the update answers")
        (let ((put (asked-for :put)))
          (is (search "SUMMARY:Standup\\, longer" (asked-body put)) "the new title is written")
          (is (search "TRANSP:TRANSPARENT" (asked-body put)) "and the busy change")
          (is (equal "\"7\"" (cdr (assoc "If-Match" (asked-headers put) :test #'string-equal))) "against the version it read")))
      (let ((answer (answered "yandex_calendar_respond_event"
                               "{\"event_href\":\"/calendars/me%40yandex.ru/e1.ics\",
                                 \"response\":\"accept\"}")))
        (is (equal "ACCEPTED" (answer-value answer "status")) "the answer is named")
        (is (search "PARTSTAT=ACCEPTED" (asked-body (asked-for :put))) "and written on the attendee"))
      (let ((answer (answered "yandex_calendar_move_event"
                               "{\"event_href\":\"/calendars/me%40yandex.ru/e1.ics\",
                                 \"calendar\":\"Holidays\"}")))
        (is (eq t (answer-value answer "moved")) "the move answers")
        (is (search "/calendars/me@yandex.ru/holidays/"
                    (nlk:json-value (nlk:decode-json answer) :string "event" "href")) "and names where the event now is"))
      (let ((answer (answered "yandex_calendar_delete_event"
                               "{\"event_href\":\"/calendars/me%40yandex.ru/e1.ics\"}")))
        (is (eq t (answer-value answer "deleted")) "the delete answers")
        (is (asked-for :delete) "and one DELETE was sent")))))

(deftest yandex-calendar-cell-without-a-login-refuses-in-words ()
  (with-cell-stop ((yandex-calendar-start "app_password" "pw"))
    (with-caldav
      (let ((answer (answered "yandex_calendar_list_calendars" "{}")))
        (is (search "yandex_calendar section has no login"
                    (answer-value answer "error")) "the missing setting is named, and the call answers rather than signals")
        (is (null *asked*) "nothing was sent")))))

(deftest yandex-calendar-cell-refuses-a-time-that-does-not-run-forwards ()
  (with-cell-stop ((yandex-calendar-start "login" "me@yandex.ru" "app_password" "pw"))
    (with-caldav
      (let ((answer (answered "yandex_calendar_create_event"
                               "{\"summary\":\"Backwards\",
                                 \"start\":\"2026-07-25T12:00:00Z\",
                                 \"end\":\"2026-07-25T11:00:00Z\"}")))
        (is (search "must end after it starts" (answer-value answer "error")) "the span is refused before a round trip")
        (is (null (asked-for :put)) "and nothing was written")))))

(deftest yandex-calendar-cell-reads-the-ics-it-was-given ()
  (is (equal "Planning, with the team"
             (nodecode-yandex-calendar::event-summary
              (first (nodecode-yandex-calendar:parse-ical
                      (ics '("BEGIN:VCALENDAR" "BEGIN:VEVENT" "UID:a@b"
                             "DTSTART;VALUE=DATE:20260725"
                             "SUMMARY:Planning\\, with the team"
                             "RRULE:FREQ=WEEKLY" "END:VEVENT" "END:VCALENDAR")))))) "an escaped title is read back")
  (is (equal "2026-07-25" (nodecode-yandex-calendar:moment-iso
                            (nodecode-yandex-calendar::parse-iso-moment "2026-07-25"))) "a bare date is an all-day value")
  (is (equal "/calendars/me@yandex.ru/x.ics"
             (nodecode-yandex-calendar::percent-decode
              (nodecode-yandex-calendar::percent-encode "/calendars/me@yandex.ru/x.ics"
                                                         :safe "@/"))) "percent escapes round-trip"))
