;;;; support.lisp --- the test slice: a stubbed CalDAV collection.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; CALDAV-CELL- name prefix; RUN-CALDAV-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-CALDAV::HTTP, the cell's
;;;; only way to the network. WITH-WIRE replaces it with a canned answer and
;;;; records every call, so a test asserts on the request and nothing dials.

(in-package #:nodecode.test)

(define-test-slice "caldav" "CALDAV-CELL-" :start nodecode-caldav:start-cell)

;;; --- the section a test starts on ------------------------------------------------

(defparameter +url+ "http://127.0.0.1:5232/user/calendar/"
  "The collection a test's section names, in the original's own example shape.")

(defparameter +user+ "test-user"
  "The account name a test's section carries.")

(defparameter +password+ "test-password"
  "The password a test's section carries.")

;;; --- the stub --------------------------------------------------------------------

(defvar *calls* '()
  "Every request the stub took, newest first: a plist of :method, :url,
:headers and :content.")

(defvar *answer* nil
  "A function of one call answering (values BODY STATUS), or NIL for an empty multistatus.")

(defun stub-http (method url &key headers content timeout)
  (declare (ignore timeout))
  (let ((call (list :method method :url url :headers headers :content content)))
    (push call *calls*)
    (if *answer* (funcall *answer* call) (values (multistatus) 207))))

(defmacro with-wire (&body body)
  "Run BODY with the cell's one HTTP seam stubbed and its calls recorded."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nodecode-caldav::http
                                (method url &key headers content timeout)
                                (stub-http method url :headers headers
                                           :content content :timeout timeout))
       ,@body)))

;;; --- starting it, calling it ------------------------------------------------------

(defun caldav-run-start (&rest pairs)
  "START-CELL on PAIRS, with the collection, the user and the password unless
PAIRS name them."
  (apply #'caldav-start
         (append (unless (member "url" pairs :test #'string=) (list "url" +url+))
                 (unless (member "user" pairs :test #'string=) (list "user" +user+))
                 (unless (member "password" pairs :test #'string=) (list "password" +password+))
                 (copy-list pairs))))

(defmacro with-caldav ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS, its wire stubbed, stopped on unwind."
  `(with-wire
     (with-cell-stop ((caldav-run-start ,@pairs))
       ,@body)))

(defun caldav-call-tool (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

;;; --- a canned collection ----------------------------------------------------------

(defun ics-event (&key uid summary (start "20261109T093000Z") end tzid repeats)
  "One VEVENT inside a VCALENDAR, as a calendar object: UID and DTSTART
always, and the rest only when the test names it."
  (with-output-to-string (out)
    (format out "BEGIN:VCALENDAR~%VERSION:2.0~%BEGIN:VEVENT~%")
    (when uid (format out "UID:~a~%" uid))
    (format out "DTSTART~a:~a~%" (if tzid (format nil ";TZID=~a" tzid) "") start)
    (when end (format out "DTEND~a:~a~%" (if tzid (format nil ";TZID=~a" tzid) "") end))
    (when summary (format out "SUMMARY:~a~%" summary))
    (when repeats (format out "RRULE:FREQ=WEEKLY~%"))
    (format out "END:VEVENT~%END:VCALENDAR~%")))

(defun multistatus (&rest calendar-objects)
  "A DAV multistatus holding one response with a calendar-data per object, the
way the collection answers one calendar-query."
  (with-output-to-string (out)
    (format out "<?xml version=\"1.0\" encoding=\"utf-8\"?>~%~
<D:multistatus xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:caldav\">~%")
    (dolist (object calendar-objects)
      (format out "<D:response><D:href>/user/calendar/x.ics</D:href>~
<D:propstat><D:prop><C:calendar-data>~a</C:calendar-data></D:prop>~
<D:status>HTTP/1.1 200 OK</D:status></D:propstat></D:response>~%" object))
    (format out "</D:multistatus>~%")))

;;; --- reading the request a call made ----------------------------------------------

(defun header (call name)
  "The value CALL sends in header NAME, or NIL."
  (cdr (assoc name (getf call :headers) :test #'string-equal)))

(defun window-of (body)
  "The start and end a list REPORT's body asks for, as (values START END)."
  (let* ((at (cl:search "start=\"" body))
         (start (and at (subseq body (+ at 7) (+ at 23))))
         (the-end (cl:search "end=\"" body))
         (end (and the-end (subseq body (+ the-end 5) (+ the-end 21)))))
    (values start end)))
