;;;; package.lisp --- the CALDAV package: two verbs, the condition, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One package, nicknamed CALDAV so an eval form reads
;;;; (caldav:list-events) or (caldav:read-event "uid"): the same calls the two
;;;; tools make, answering the same text.

(defpackage #:nodecode-caldav
  (:use #:cl)
  (:nicknames #:caldav)
  (:export
   ;; the two verbs, each answering the text a model reads
   #:list-events
   #:read-event
   ;; the window a command or a layer may ask for
   #:window-stamps
   #:render-events
   ;; data, so a layer can reword it
   #:*primer*
   ;; the one condition
   #:caldav-error))

(in-package #:nodecode-caldav)

(nlk:define-peripheral caldav :not-running t)

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun calendar-failed (kind &optional detail)
  "Signal the read failure the original named KIND, with DETAIL when it has one.
KIND is a short token such as http_401, timeout or not_multistatus, so a
model reading the message learns which part of the exchange failed."
  (fail "The calendar could not be read (~a).~@[ ~a~]" kind detail))
