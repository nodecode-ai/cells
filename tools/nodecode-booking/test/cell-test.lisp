;;;; cell-test.lisp --- the booking cell against a stubbed Google.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP and both secrets a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing leaves the machine. A tool is called the way
;;;; a model's call reaches it -- through the real dispatch and the real tool
;;;; schema -- so the schema the provider is shown and the body that runs are
;;;; one shape.

(in-package #:nodecode.test)
;;; debug scratch

(define-test-slice "booking" "BOOKING-CELL-" :start nodecode-booking:start-cell
  :defaults (("client_id" "test-client") ("utc_offset" "-06:00")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter *requests* nil
  "Every request the stub answered, newest first, as (METHOD URL OPTIONS).")

(defparameter *busy* nil
  "What FreeBusy answers: (START . END) RFC3339 stamps.")

(defparameter *events* nil
  "What the events calls answer: event hash tables.")

(defparameter *start* (+ 86400 (- (nodecode-booking::now-seconds)
                                 (mod (nodecode-booking::now-seconds) 3600)))
  "A whole hour tomorrow: a booking window that is never in the past.")

(defun stamp (offset-seconds)
  "OFFSET-SECONDS after *START*, as the UTC stamp Google answers with."
  (nodecode-booking::format-time (+ *start* offset-seconds) 0))

(defun at (offset-seconds)
  "OFFSET-SECONDS after *START*, at the section's offset: what a call passes in."
  (nodecode-booking::format-time (+ *start* offset-seconds) -21600))

(defun event (&key (id "e1") (title "Strategy consult") (start 0) (length 1800) (key nil))
  "One event as Google returns it."
  (let ((event (nlk:json-object
                "id" id "summary" title "status" "confirmed"
                "htmlLink" (format nil "https://www.google.com/calendar/event?eid=~a" id)
                "start" (nlk:json-object "dateTime" (stamp start))
                "end" (nlk:json-object "dateTime" (stamp (+ start length))))))
    (when key
      (setf (gethash "extendedProperties" event)
            (nlk:json-object "private" (nlk:json-object "bookingBridgeKey" key))))
    event))

(defun stub-json (object &optional (status 200))
  (values (if object (nlk:encode-json-object object) "") status))

(defun find-in-url (url marker)
  "URL's text after MARKER, up to a query string."
  (let* ((from (search marker url))
         (text (and from (subseq url (+ from (length marker)))))
         (query (and text (search "?" text))))
    (and text (if query (subseq text 0 query) text))))

(defun stub-google (method url options)
  "What Google answers METHOD URL with, out of the fixtures above."
  (let ((content (getf options :content)))
    (cond
      ((search "/token" url)
       (stub-json (nlk:json-object "access_token" "test-token" "expires_in" 3600)))
      ((search "/freeBusy" url)
       (stub-json (nlk:json-object
                   "calendars"
                   (nlk:json-object
                    "primary"
                    (nlk:json-object
                     "busy" (coerce (loop for (from . to) in *busy*
                                           collect (nlk:json-object "start" from "end" to))
                                    'vector))))))
      ((and (equal method "GET") (search "/events?" url))
       (stub-json (nlk:json-object "items" (coerce *events* 'vector))))
      ((and (equal method "GET") (search "/events/" url))
       (let ((id (find-in-url url "/events/")))
         (stub-json (or (find id *events* :key (lambda (one) (nodecode-booking::text-field one "id"))
                                     :test #'equal)
                        (nlk:json-object)))))
      ((equal method "POST")
       (let ((body (nlk:decode-json content)))
         (setf (gethash "id" body) "new-1"
               (gethash "status" body) "confirmed")
         (stub-json body)))
      ((equal method "PATCH")
       (let ((body (nlk:decode-json content)))
         (setf (gethash "id" body) (find-in-url url "/events/")
               (gethash "summary" body) "Strategy consult"
               (gethash "status" body) "confirmed")
         (stub-json body)))
      ((equal method "DELETE") (stub-json nil 204))
      (t (stub-json (nlk:json-object "error" (nlk:json-object "message" "unexpected")) 500)))))

(defmacro with-booking (&body body)
  "BODY with both secrets answering, the request log emptied, and no fixtures."
  `(let ((saved *requests*))
     (setf *requests* '() *busy* '() *events* '())
     (nlk:with-cleanup ((setf *requests* saved))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                   (cond ((equal name "GOOGLE_CALENDAR_CLIENT_SECRET") "test-secret")
                                         ((equal name "GOOGLE_CALENDAR_REFRESH_TOKEN") "test-refresh")
                                         (t nil)))
         ,@body))))

(defmacro with-booking-wire (&body body)
  "BODY with NLK:HTTP answering Google's two endpoints."
  `(with-stubbed-fdefinition (nlk:http (method url &rest options)
                              (push (list method url options) *requests*)
                              (stub-google method url options))
     (with-booking ,@body)))

(defun tool-answer (id name arguments)
  "The tool's answer as the model reads it, through the real dispatch."
  (nle::message-content (execute-wire-call id name arguments)))

(defun tool-object (id name arguments)
  "The tool's answer decoded."
  (nlk:decode-json (tool-answer id name arguments)))

(defun answer-field (object name)
  (nodecode-booking::text-field object name))

(defun answer-list (object name)
  (nodecode-booking::items-field object name))

(defun asked (marker)
  "The request whose URL carries MARKER, or NIL."
  (find marker *requests* :key #'second :test (lambda (marker url) (search marker url))))

(defun written (&optional (method "POST"))
  "The event write of METHOD, or NIL: the grant's own POST is not one."
  (find-if (lambda (request) (and (equal method (first request)) (search "/events" (second request))))
           *requests*))

(defun answer-event (object)
  "The event an answer carries, or NIL."
  (and (hash-table-p object) (gethash "event" object)))

;;; --- the lifecycle, from the one declaration --------------------------------------

(define-cell-lifecycle-tests "booking"
  (:config)
  (:help :booking)
  (:command "booking")
  (:fixture with-booking)
  (:refused ("duration_minutes" 1) ("client_id" "")))

(deftest booking-cell-an-unset-variable-refuses ()
  (with-booking
    (let ((condition (signals-error nlk:config-refusal
                        (booking-start "refresh_token_env" "SOME_UNSET_VARIABLE"))))
      (is (search "SOME_UNSET_VARIABLE" (princ-to-string condition)) "the refusal names the variable")
      (is (null (nle::find-tool "booking_list_availability")) "and installs nothing"))))

;;; --- availability -----------------------------------------------------------------

(deftest booking-cell-free-busy-becomes-slots (with-booking-wire)
  (with-cell-stop ((booking-start))
    (setf *busy* (list (cons (stamp 1800) (stamp 3600))))
    (let ((answer (tool-object "c1" "booking_list_availability"
                                (format nil "{\"time_min\":\"~a\",\"time_max\":\"~a\"}"
                                        (at 0) (at 7200))))
          (slots nil))
      (setf slots (answer-list answer "slots"))
      (is (eq t (gethash "ok" answer)) "the answer is a success")
      (is (= 3 (length slots)) "the busy half hour leaves two free ones and one before it")
      (is (equal (at 0) (answer-field (first slots) "start")) "the first slot is the window's own start")
      (is (equal (at 3600) (answer-field (second slots) "start")) "and the busy interval is skipped")
      (let ((request (asked "/freeBusy")))
        (is (equal "POST" (first request)) "freeBusy is a POST")
        (is (equal "Bearer test-token" (cdr (assoc "authorization" (getf (third request) :headers) :test #'equal))) "the token from the grant rides the call")))))

(deftest booking-cell-a-window-over-fourteen-days-is-refused (with-booking-wire)
  (with-cell-stop ((booking-start))
    (let ((answer (tool-object "c2" "booking_list_availability"
                                (format nil "{\"time_min\":\"~a\",\"time_max\":\"~a\"}"
                                        (at 0) (nodecode-booking::format-time (+ *start* (* 20 86400)) -21600)))))
      (is (search "14 days" (answer-field answer "error")) "the window's cap is named")
      (is (null (asked "/freeBusy")) "and nothing was asked"))))

;;; --- the writes ---------------------------------------------------------------------

(deftest booking-cell-a-write-without-confirm-sends-nothing (with-booking-wire)
  (with-cell-stop ((booking-start))
    ;; The schema already names confirm as required, so the refusal may come from
    ;; the preflight or from the tool's own guard: either way it says confirm, and
    ;; either way the calendar is untouched.
    (let ((answer (tool-answer "c3" "booking_book_event"
                                (format nil "{\"title\":\"Strategy consult\",\"start\":\"~a\"}" (at 3600)))))
      (is (search "confirm" answer) "the refusal asks for confirm")
      (is (null (written)) "and no event was created")
      (is (search "confirm" (nodecode-booking::book-event (nlk:decode-json (format nil "{\"title\":\"Only if agreed\",\"start\":\"~a\"}" (at 5400))))) "and the tool's own guard refuses it even when the schema's gate is bypassed"))))

(deftest booking-cell-a-confirmed-booking-inserts-once (with-booking-wire)
  (with-cell-stop ((booking-start))
    (let* ((arguments (format nil "{\"title\":\"Strategy consult\",\"start\":\"~a\",\"confirm\":true}" (at 3600)))
           (first (tool-object "c4" "booking_book_event" arguments))
           (posted (written "POST"))
           (body (nlk:decode-json (getf (third posted) :content))))
      (is (eq t (gethash "ok" first)) "the booking answers success")
      (is (search "sendUpdates=all" (second posted)) "attendees would be told")
      (is (equal "new-1" (answer-field (answer-event first) "event_id")) "the new event's id is the answer")
      (is (search "bookingBridgeKey" (getf (third posted) :content)) "the event carries the idempotency mark")
      (setf *events* (list body))
      (let ((again (tool-object "c5" "booking_book_event" arguments)))
        (is (eq t (gethash "duplicate" again)) "the same booking twice is one event")))))

(deftest booking-cell-a-conflict-refuses-the-booking (with-booking-wire)
  (with-cell-stop ((booking-start))
    (setf *busy* (list (cons (stamp 3600) (stamp 5400))))
    (let ((answer (tool-object "c6" "booking_book_event"
                                (format nil "{\"title\":\"Strategy consult\",\"start\":\"~a\",\"confirm\":true}"
                                        (at 3600)))))
      (is (search "no longer free" (answer-field answer "error")) "the conflict is named")
      (is (null (written)) "and nothing was inserted"))))

(deftest booking-cell-reschedule-ignores-the-events-own-interval (with-booking-wire)
  (with-cell-stop ((booking-start))
    (setf *events* (list (event :id "e9" :start 3600))
          *busy* (list (cons (stamp 3600) (stamp 5400))))
    (let ((answer (tool-object "c7" "booking_reschedule_event"
                                (format nil "{\"event_id\":\"e9\",\"start\":\"~a\",\"confirm\":true}"
                                        (at 7200)))))
      (is (eq t (gethash "ok" answer)) "moving into the event's own old hour is free")
      (is (equal "e9" (answer-field (answer-event answer) "event_id")) "and the id comes back"))))

(deftest booking-cell-a-cancelled-event-is-named (with-booking-wire)
  (with-cell-stop ((booking-start))
    (setf *events* (list (event :id "e9")))
    (let ((answer (tool-object "c8" "booking_cancel_event" "{\"event_id\":\"e9\",\"confirm\":true}")))
      (is (eq t (gethash "cancelled" answer)) "the answer says it was cancelled")
      (is (equal "Strategy consult" (answer-field answer "title")) "and names what it was")
      (is (find "DELETE" *requests* :key #'first :test #'equal) "one request deleted it"))))

;;; --- the command ---------------------------------------------------------------------

(deftest booking-cell-the-command-answers-through-stubbed-io (with-booking-wire)
  (with-cell-stop ((booking-start))
    (setf *events* (list (event :id "e1")))
    (let ((line (cell-entry "nodecode-booking" "booking" "")))
      (is (search "signed in" line) (format nil "/booking says the grant answers: ~a" line))
      (is (search "Strategy consult" line) "and names what is coming"))
    (is (search "booking_book_event" (cell-entry "nodecode-booking" "booking" "help")) "/booking help is the manual")
    (is (search "not one of its words" (cell-entry "nodecode-booking" "booking" "wobble")))))

