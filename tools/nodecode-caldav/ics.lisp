;;;; ics.lisp --- the VEVENTs an answer carries, one row per uid.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's iCalendar reading, in Lisp: fold a long line back onto
;;;; the one it continues, count VEVENT at a BEGIN line (never inside a
;;;; summary), read UID, SUMMARY, DTSTART, DTEND, RECURRENCE-ID, RRULE and
;;;; RDATE, and keep a TZID as written. A recurring event that a server
;;;; split into a master plus one component per changed occurrence carries
;;;; one uid, so the components are grouped into the one row of that uid
;;;; (RFC 5545 section 3.8.4.4) and that row repeats. Times are not
;;;; converted and repeats are not expanded: one event is one row.

(in-package #:nodecode-caldav)

(defstruct (component (:constructor make-component))
  (uid nil)
  (summary nil)
  (start nil)
  (end nil)
  (recurrence nil)
  (repeats nil))

(defstruct (event-group (:constructor make-event-group (first)))
  (master nil)
  (first nil)
  (keys '())
  (changed 0))

(defun ics-unfold (text)
  "TEXT's lines, a line that starts with a space or tab joined onto the one
before it, and the empty lines dropped."
  (let ((lines '()))
    (dolist (line (uiop:split-string text :separator '(#\Newline #\Return)))
      (cond ((zerop (length line)))
            ((and lines (member (char line 0) '(#\Space #\Tab)))
             (setf (first lines) (concatenate 'string (first lines) (subseq line 1))))
            (t (push line lines))))
    (nreverse lines)))

(defun ics-unescape (value)
  "VALUE with the backslash escapes of RFC 5545 section 3.3.11 undone."
  (with-output-to-string (out)
    (loop with size = (length value)
          for index = 0 then (1+ index)
          while (< index size)
          do (let ((character (char value index)))
               (if (and (char= character #\\) (< (1+ index) size))
                   (let ((next (char value (1+ index))))
                     (write-char (case next ((#\n #\N) #\Newline) (t next)) out)
                     (incf index))
                   (write-char character out))))))

(defun param-value (params name)
  "The value PARAMS carries for NAME (`TZID=Europe/Berlin' => Europe/Berlin), or NIL."
  (loop for param in params
        for equals = (position #\= param)
        when (and equals (string-equal (subseq param 0 equals) name))
          return (string-trim "\"" (subseq param (1+ equals)))))

(defun components-of (text)
  "One COMPONENT per VEVENT in TEXT, in document order, as the fields the
original lifted out of it."
  (let ((stack '())
        (events '())
        (current nil))
    (dolist (line (ics-unfold text))
      (let* ((colon (position #\: line))
             (left (and colon (subseq line 0 colon)))
             (parts (and left (uiop:split-string left :separator ";")))
             (name (and parts (string-upcase (first parts))))
             (value (and colon (ics-unescape (subseq line (1+ colon))))))
        (when name
          (cond
            ((string= name "BEGIN")
             (let ((kind (string-upcase (nlk:trimmed value))))
               (push kind stack)
               (when (string= kind "VEVENT")
                 (setf current (make-component))
                 (push current events))))
            ((string= name "END")
             (when stack (pop stack))
             (when (string= (string-upcase (nlk:trimmed value)) "VEVENT") (setf current nil)))
            ((and current (equal (first stack) "VEVENT"))
             (let ((params (rest parts)))
               (cond ((string= name "UID") (setf (component-uid current) value))
                     ((string= name "SUMMARY") (setf (component-summary current) value))
                     ((string= name "DTSTART") (setf (component-start current) (cons params value)))
                     ((string= name "DTEND") (setf (component-end current) (cons params value)))
                     ((string= name "RECURRENCE-ID")
                      (setf (component-recurrence current) (cons params value)))
                     ((member name '("RRULE" "RDATE") :test #'string=)
                      (setf (component-repeats current) t)))))))))
    (nreverse events)))

(defun recurrence-key (component)
  "COMPONENT's place in its event: NIL for a master, else its RECURRENCE-ID
with the TZID it was written in."
  (let ((recurrence (component-recurrence component)))
    (and recurrence (list (param-value (car recurrence) "TZID") (cdr recurrence)))))

(defun checked-components (components)
  "COMPONENTS with every event proved to carry a uid and a start, as the
original failed the whole call on one that did not."
  (dolist (component components)
    (unless (nonblank (component-uid component))
      (calendar-failed "missing_uid" "An event in the answer carried no uid."))
    (unless (nonblank (cdr (component-start component)))
      (calendar-failed "missing_start" "An event in the answer carried no start.")))
  components)

(defun group-events (components)
  "One row per uid: a changed occurrence belongs to the master of its uid, and
two masters, or two copies of one occurrence, are ambiguous and fail the call."
  (let ((order '())
        (groups (make-hash-table :test #'equal)))
    (dolist (component components)
      (let* ((uid (component-uid component))
             (group (or (gethash uid groups)
                        (let ((fresh (make-event-group component)))
                          (setf (gethash uid groups) fresh)
                          (push uid order)
                          fresh))))
        (let ((key (recurrence-key component)))
          (when (member key (event-group-keys group) :test #'equal)
            (calendar-failed "ambiguous" "Two events in the answer carried the same uid and occurrence."))
          (push key (event-group-keys group))
          (if key
              (incf (event-group-changed group))
              (setf (event-group-master group) component)))))
    (loop for uid in (nreverse order)
          for group = (gethash uid groups)
          for base = (or (event-group-master group) (event-group-first group))
          collect (let ((start (component-start base)))
                    (list :uid uid
                          :summary (component-summary base)
                          :start (cdr start)
                          :end (and (component-end base) (cdr (component-end base)))
                          :tzid (unless (uiop:string-suffix-p (cdr start) "Z")
                                  (param-value (car start) "TZID"))
                          :repeats (or (component-repeats base)
                                        (plusp (event-group-changed group))
                                        (not (null (component-recurrence base)))))))))

(defun ics-events (text)
  "TEXT's VEVENTs as rows, one per uid, in document order."
  (group-events (checked-components (components-of text))))
