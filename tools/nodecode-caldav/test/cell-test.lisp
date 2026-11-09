;;;; cell-test.lisp --- lifecycle, the two tools, the refusals, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not a calendar server: the
;;;; config surface (a section naming no collection, no user or no password
;;;; refuses the start), the two tools through the real dispatch seam against
;;;; a stubbed collection, the REPORT each one sends, the fields its answer
;;;; is cleaned into, and the refusal a failed status gets.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "caldav"
  (:config "url" +url+ "user" +user+ "password" +password+)
  (:help :caldav)
  (:command "caldav")
  (:running
   (is (nle::find-tool "list_events") "list_events is registered")
   (is (nle::find-tool "read_event") "read_event is registered")
   (is (equal +url+ (nodecode-caldav::setting :url)) "the collection the section carries"))
  (:stopped
   (is (null (nle::find-tool "list_events")) "stop unregisters list_events")
   (is (null (nle::find-tool "read_event")) "and read_event"))
  (:refused
   ("user" +user+ "password" +password+)
   ("url" +url+ "password" +password+)
   ("url" +url+ "user" +user+ "password_env" "UNSET_CALDAV_PASSWORD")
   ("url" "ftp://calendar.test/user/calendar/" "user" +user+ "password" +password+)
   ("url" "http://calendar.test/user/calendar/?x=1" "user" +user+ "password" +password+)
   ("url" +url+ "user" +user+ "password" +password+ "days" 0)))

(deftest caldav-cell-a-section-with-no-collection-refuses-and-installs-nothing ()
  ;; The one setting no call can work without: the refusal names it, and the
  ;; tools are not offered on a calendar the cell cannot read.
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (caldav-start "user" +user+ "password" +password+))))
      (is (cl:search "no collection" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "list_events")) "and installs nothing"))))

(deftest caldav-cell-a-section-with-no-password-refuses ()
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (caldav-start "url" +url+ "user" +user+
                                                  "password_env" "UNSET_CALDAV_PASSWORD"))))
      (is (cl:search "no password" (princ-to-string condition)) "the refusal names the password"))))

(defun stamp-seconds (stamp)
  "One YYYYMMDDTHHMMSSZ stamp as a universal time, for arithmetic in a test."
  (encode-universal-time (parse-integer stamp :start 13 :end 15)
                         (parse-integer stamp :start 11 :end 13)
                         (parse-integer stamp :start 9 :end 11)
                         (parse-integer stamp :start 6 :end 8)
                         (parse-integer stamp :start 4 :end 6)
                         (parse-integer stamp :start 0 :end 4)
                         0))

;;; --- list_events ------------------------------------------------------------------

(deftest caldav-cell-a-window-arrives-as-the-events-it-holds ()
  (with-caldav ()
    (setf *answer* (lambda (call)
                      (declare (ignore call))
                      (values (multistatus (ics-event :uid "standup@example.test"
                                                      :summary "Team standup"
                                                      :start "20261109T093000Z"
                                                      :end "20261109T100000Z"))
                              207)))
    (let ((text (caldav-call-tool "list_events" "{}")))
      (is (cl:search "Team standup" text) "the summary")
      (is (cl:search "start 20261109T093000Z" text) "the start as the calendar wrote it")
      (is (cl:search "end 20261109T100000Z" text) "and the end")
      (is (cl:search "standup@example.test" text) "and the uid a read_event call takes"))
    (let ((call (first *calls*)))
      (is (equal "REPORT" (getf call :method)) "one calendar-query REPORT, as the original sent")
      (is (equal +url+ (getf call :url)) "to the section's collection")
      (is (equal "1" (header call "depth")) "with Depth 1")
      (is (cl:search "Basic " (header call "authorization")) "and the Basic credential")
      (is (cl:search "calendar-query" (getf call :content)) "the body is a calendar-query"))))

(deftest caldav-cell-a-call-with-no-arguments-reads-the-section-s-days ()
  (with-caldav ()
    (caldav-call-tool "list_events" "{}")
    (multiple-value-bind (start end) (window-of (getf (first *calls*) :content))
      (is (and start end) "the body carries a window")
      (is (= 16 (length start)) "in the YYYYMMDDTHHMMSSZ form the original sent")
      (is (= (* 7 86400) (- (stamp-seconds end) (stamp-seconds start))) "seven days, the section's days, start included and end excluded"))))

(deftest caldav-cell-a-ranged-call-asks-for-the-window-it-names ()
  (with-caldav ()
    (caldav-call-tool "list_events" "{\"start\":\"20261108T000000Z\",\"end\":\"20261115T000000Z\"}")
    (multiple-value-bind (start end) (window-of (getf (first *calls*) :content))
      (is (equal "20261108T000000Z" start) "the start the call named")
      (is (equal "20261115T000000Z" end) "and its end"))))

(deftest caldav-cell-half-a-window-is-refused-before-the-wire ()
  (with-caldav ()
    (let ((condition (signals-error nodecode-caldav:caldav-error
                                    (nodecode-caldav:list-events
                                     (nodecode-caldav:make-list-events-request
                                      :start "20261108T000000Z")))))
      (is (cl:search "passed together" (princ-to-string condition)) "start alone is refused")
      (is (null *calls*) "and nothing was dialled"))))

(deftest caldav-cell-a-time-zone-and-no-end-are-kept-as-written ()
  (with-caldav ()
    (setf *answer* (lambda (call)
                      (declare (ignore call))
                      (values (multistatus (ics-event :uid "tz@example.test"
                                                      :summary "Sync"
                                                      :start "20261109T093000"
                                                      :tzid "Europe/Berlin"))
                              207)))
    (let ((text (caldav-call-tool "list_events" "{}")))
      (is (cl:search "tzid Europe/Berlin" text) "the TZID is kept")
      (is (null (cl:search "end " text)) "and a missing DTEND is not invented")
      (is (null (cl:search "repeats" text)) "a plain event does not repeat"))))

(deftest caldav-cell-a-list-past-the-cap-is-cut-and-says-so ()
  (with-caldav ()
    (setf *answer* (lambda (call)
                      (declare (ignore call))
                      (values (apply #'multistatus
                                     (loop for index from 1 to 55
                                           collect (ics-event
                                                    :uid (format nil "e~d@example.test" index)
                                                    :summary (format nil "Event ~d" index))))
                              207)))
    (let ((text (caldav-call-tool "list_events" "{}")))
      (is (cl:search "[50] Event 50" text) "the fiftieth row is shown")
      (is (null (cl:search "[51]" text)) "the fifty-first is not")
      (is (cl:search "This is not the whole list" text) "and the answer says it is cut")
      (is (cl:search "55 events" text) "naming how many the window held"))))

(deftest caldav-cell-an-event-with-no-uid-fails-the-whole-call ()
  (with-caldav ()
    (setf *answer* (lambda (call)
                      (declare (ignore call))
                      (values (multistatus (ics-event :summary "Nameless")) 207)))
    (let ((condition (signals-error nodecode-caldav:caldav-error
                                    (nodecode-caldav:list-events
                                     (nodecode-caldav:make-list-events-request)))))
      (is (cl:search "missing_uid" (princ-to-string condition)) "the answer is refused, not half read"))))

;;; --- read_event -------------------------------------------------------------------

(deftest caldav-cell-a-read-takes-the-exact-uid-out-of-a-substring-match ()
  (with-caldav ()
    (setf *answer* (lambda (call)
                      (declare (ignore call))
                      (values (multistatus (ics-event :uid "meet-1@example.test"
                                                      :summary "The right one")
                                           (ics-event :uid "meet-12@example.test"
                                                      :summary "The other one"))
                              207)))
    (let ((text (caldav-call-tool "read_event" "{\"uid\":\"meet-1@example.test\"}")))
      (is (cl:search "The right one" text) "the event whose uid was asked for")
      (is (null (cl:search "meet-12" text)) "and not the uid that merely contains it")
      (is (cl:search "1 event" text) "one event, said plainly"))
    (let ((body (getf (first *calls*) :content)))
      (is (cl:search "<C:prop-filter name=\"UID\">" body) "the uid is a property filter")
      (is (cl:search ">meet-1@example.test<" body) "carrying the uid as asked"))))

(deftest caldav-cell-a-uid-the-collection-does-not-hold-is-not-an-empty-success ()
  (with-caldav ()
    (let ((condition (signals-error nodecode-caldav:caldav-error
                                    (nodecode-caldav:read-event
                                     (nodecode-caldav:make-read-event-request
                                      :uid "nope@example.test")))))
      (is (cl:search "No event with that uid" (princ-to-string condition)) "it says what is missing"))))

;;; --- what a refused or odd answer becomes -------------------------------------------

(deftest caldav-cell-a-refused-credential-names-the-status ()
  (with-caldav ()
    (setf *answer* (lambda (call)
                      (declare (ignore call))
                      (values "<html><body>Forbidden</body></html>" 401)))
    (let ((condition (signals-error nodecode-caldav:caldav-error
                                    (nodecode-caldav:list-events
                                     (nodecode-caldav:make-list-events-request)))))
      (is (cl:search "http_401" (princ-to-string condition)) "the status is named")
      (is (cl:search "Forbidden" (princ-to-string condition)) "with what the server said"))))

(deftest caldav-cell-a-200-with-a-calendar-in-it-is-still-a-failure ()
  ;; The original's rule: success is 207 whose root is a multistatus, and a
  ;; 200 is not one, however much it looks like a calendar.
  (with-caldav ()
    (setf *answer* (lambda (call)
                      (declare (ignore call))
                      (values (multistatus (ics-event :uid "a@example.test" :summary "A")) 200)))
    (let ((condition (signals-error nodecode-caldav:caldav-error
                                    (nodecode-caldav:list-events
                                     (nodecode-caldav:make-list-events-request)))))
      (is (cl:search "http_200" (princ-to-string condition)) "a 200 is refused"))))

(deftest caldav-cell-a-server-that-does-not-speak-caldav-is-named ()
  (with-caldav ()
    (setf *answer* (lambda (call)
                      (declare (ignore call))
                      (values "<D:foo xmlns:D=\"DAV:\"/>" 207)))
    (let ((condition (signals-error nodecode-caldav:caldav-error
                                    (nodecode-caldav:list-events
                                     (nodecode-caldav:make-list-events-request)))))
      (is (cl:search "not_multistatus" (princ-to-string condition)) "the root element is named"))))

(deftest caldav-cell-an-empty-collection-is-not-an-error ()
  (with-caldav ()
    (let ((text (caldav-call-tool "list_events" "{}")))
      (is (cl:search "No events in" text) "an empty answer says so"))))

;;; --- the command -------------------------------------------------------------------

(deftest caldav-cell-the-command-reads-the-calendar-and-explains-itself ()
  (with-caldav ()
    (setf *answer* (lambda (call)
                      (declare (ignore call))
                      (values (multistatus (ics-event :uid "c@example.test"
                                                      :summary "Dentist"
                                                      :start "20261110T140000Z"))
                              207)))
    (is (cl:search "Dentist" (cell-entry "nodecode-caldav" "caldav" "" "caldav-cell-1")) "/caldav reads the section's days")
    (let ((guide (cell-entry "nodecode-caldav" "caldav" "whenever" "caldav-cell-1")))
      (is (cl:search "list_events" guide) "an argument it does not know names the tools")
      (is (cl:search "/caldav today" guide) "and the windows it does"))))
