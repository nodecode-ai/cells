;;;; package.lisp --- the YANDEX-CALENDAR package and the one failure it names.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The package is named after the system, so the loader finds START-CELL as
;;;; its export. The files below it are plain Common Lisp as far as they can
;;;; be: xml.lisp and ical.lisp use nothing from the image, so they load and
;;;; are tested on their own; caldav.lisp reaches the network through
;;;; NLK:HTTP, and tools.lisp turns a call into the JSON the model reads.

(defpackage #:nodecode-yandex-calendar
  (:use #:cl)
  (:nicknames #:yandex-calendar)
  (:export
   ;; the cell, from NLK:DEFINE-PERIPHERAL and NLE:DEFINE-CELL
   #:start-cell #:stop-cell #:setting
   #:yandex-calendar-error #:yandex-calendar-error-detail
   ;; the seven tool bodies, each a string of JSON, none of which signals
   #:list-calendars #:list-events #:create-event #:update-event
   #:respond-event #:move-event #:delete-event
   ;; the CalDAV client, for a layer or a test
   #:client #:client-for-settings #:client-calendars
   #:caldav-error #:caldav-error-detail
   ;; iCalendar, so a layer can read what the server sent
   #:parse-ical #:build-ical #:ical-event #:moment #:moment-iso
   #:event-summary #:event-start #:event-end #:event-attendees #:attendee-email))

(in-package #:nodecode-yandex-calendar)

(define-condition caldav-error (error)
  ((detail :initarg :detail :reader caldav-error-detail))
  (:report (lambda (condition stream)
             (write-string (caldav-error-detail condition) stream)))
  (:documentation "One CalDAV or iCalendar failure, as the model reads it."))

(defun caldav-fail (format-control &rest arguments)
  "Signal CALDAV-ERROR with a formatted detail."
  (error 'caldav-error :detail (apply #'format nil format-control arguments)))
