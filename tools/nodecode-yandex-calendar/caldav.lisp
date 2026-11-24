;;;; caldav.lisp --- a small CalDAV client for Yandex Calendar.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Just enough CalDAV (RFC 4791) to discover the calendars, run a
;;;; time-range calendar-query REPORT, GET one event, PUT a new one
;;;; (If-None-Match: *) or a modified one (If-Match when its ETag is known),
;;;; answer an invitation, move an event between calendars (verbatim copy,
;;;; read-back, then a conditional DELETE of the original) and DELETE one.
;;;;
;;;; Credentials are pinned to one HTTPS origin: a base URL is checked before
;;;; anything is sent, and an href that names another origin is refused
;;;; rather than followed, so the account's app password can only ever reach
;;;; the server the operator configured. A failed status is a value here; the
;;;; tool layer turns it into the sentence the model reads.

(in-package #:nodecode-yandex-calendar)

(defparameter *default-base-url* "https://caldav.yandex.ru"
  "The server a Yandex Calendar account is read at.")

(defstruct (client (:constructor %make-client) (:conc-name client-))
  "One account's CalDAV client: where it dials, as whom, and what it may touch."
  (login "" :type string)
  (password "" :type string)
  (base-url "" :type string)
  (origin "" :type string)
  (base-path "" :type string)
  (allowed '() :type list)
  (calendar-list nil))

;;; --- URLs and credentials ------------------------------------------------------

(defun base64 (text)
  "TEXT, its bytes read as a string, base64-encoded."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (octets (sb-ext:string-to-octets text :external-format :utf-8)))
    (with-output-to-string (out)
      (loop for start from 0 below (length octets) by 3
            for a = (aref octets start)
            for b = (if (< (+ start 1) (length octets)) (aref octets (+ start 1)) 0)
            for c = (if (< (+ start 2) (length octets)) (aref octets (+ start 2)) 0)
            for triple = (+ (ash a 16) (ash b 8) c)
            do (write-char (char alphabet (ldb (byte 6 18) triple)) out)
               (write-char (char alphabet (ldb (byte 6 12) triple)) out)
               (write-char (if (< (+ start 1) (length octets))
                               (char alphabet (ldb (byte 6 6) triple))
                               #\=)
                           out)
               (write-char (if (< (+ start 2) (length octets))
                               (char alphabet (ldb (byte 6 0) triple))
                               #\=)
                           out)))))

(defun percent-encode (text &key (safe ""))
  "TEXT percent-encoded as one path segment, the bytes of SAFE left alone."
  (with-output-to-string (out)
    (loop for char across text
          do (if (or (alphanumericp char)
                     (position char "-._~" :test #'char=)
                     (position char safe :test #'char=))
                 (write-char char out)
                 (loop for octet across (sb-ext:string-to-octets (string char)
                                                                 :external-format :utf-8)
                       do (format out "%~2,'0X" octet))))))

(defun percent-decode (text)
  "TEXT with its percent escapes decoded, one UTF-8 byte run at a time."
  (if (null (position #\% text))
      text
      (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
            (i 0))
        (loop while (< i (length text))
              do (let ((char (char text i)))
                   (if (and (char= char #\%)
                            (< (+ i 2) (length text))
                            (digit-char-p (char text (1+ i)) 16)
                            (digit-char-p (char text (+ i 2)) 16))
                       (progn (vector-push-extend (parse-integer text :start (1+ i) :end (+ i 3)
                                                                  :radix 16)
                                                   octets)
                              (incf i 3))
                       (progn (loop for octet across (sb-ext:string-to-octets
                                                       (string char) :external-format :utf-8)
                                     do (vector-push-extend octet octets))
                              (incf i)))))
        (sb-ext:octets-to-string octets :external-format :utf-8))))

(defun url-parts (url)
  "URL as (values SCHEME HOST PORT PATH QUERY), or CALDAV-ERROR for one unreadable."
  (when (some (lambda (char) (< (char-code char) #x21)) url)
    (caldav-fail "CalDAV addresses must not contain control characters or spaces."))
  (let* ((scheme-end (search "://" url))
         (scheme (and scheme-end (string-downcase (subseq url 0 scheme-end))))
         (rest (if scheme-end (subseq url (+ scheme-end 3)) url))
         (path-at (or (position #\/ rest) (length rest)))
         (authority (subseq rest 0 path-at))
         (path-query (subseq rest path-at))
         (query-at (position #\? path-query))
         (at (position #\@ authority)))
    (unless (and scheme (plusp (length authority)) (null at))
      (caldav-fail "~s is not a plain HTTPS address." url))
    (let* ((port-at (position #\: authority))
           (host (string-downcase (if port-at (subseq authority 0 port-at) authority)))
           (port (and port-at (ignore-errors (parse-integer authority :start (1+ port-at)))))
           (path (if query-at (subseq path-query 0 query-at) path-query)))
      (when (and port-at (null port))
        (caldav-fail "~s does not name a port number." url))
      (values scheme host (or port 443)
              (if (zerop (length path)) "/" path)
              (and query-at (subseq path-query (1+ query-at)))))))

(defun https-origin (url &key (what "CalDAV addresses"))
  "URL as the canonical (values ORIGIN HOST PORT PATH) its credentials may reach."
  (multiple-value-bind (scheme host port path) (url-parts url)
    (unless (and (string= scheme "https") (plusp (length host)))
      (caldav-fail "~a must use a valid HTTPS origin." what))
    (values (format nil "https://~a~@[:~a~]" host (unless (eql port 443) port))
            host port path)))

;;; --- the client ------------------------------------------------------------------

(defun client-for-settings ()
  "The client the running cell's settings describe."
  (let* ((login (string-trim '(#\Space #\Tab) (or (setting :login) "")))
         (password (or (setting :app-password) ""))
         (base-url (string-right-trim "/" (or (setting :base-url) *default-base-url*))))
    (when (zerop (length login))
      (caldav-fail "the yandex_calendar section has no login: set `login' in /settings."))
    (when (zerop (length password))
      (caldav-fail "the yandex_calendar section has no app password: set `app_password' in ~
/settings (a Yandex account needs an app password for CalDAV)."))
    (multiple-value-bind (origin host port path) (https-origin base-url :what "base_url")
      (declare (ignore host port))
      (%make-client :login login :password password :base-url base-url
                    :origin origin
                    :base-path (string-right-trim "/" path)
                    :allowed (setting :calendars)))))

(defun client-home (client)
  "The account's CalDAV home collection, a path."
  (format nil "/calendars/~a/" (percent-encode (client-login client) :safe "@")))

(defun client-url (client href)
  "HREF resolved against CLIENT's origin, or CALDAV-ERROR for another one."
  (let* ((href (string-trim '(#\Space #\Tab) href))
         (absolute (or (uiop:string-prefix-p "https://" (string-downcase href))
                       (uiop:string-prefix-p "http://" (string-downcase href)))))
    (cond
      (absolute
       (multiple-value-bind (origin host port path query) (https-origin href)
         (declare (ignore host port))
         (unless (string= origin (client-origin client))
           (caldav-fail "CalDAV addresses must use the configured HTTPS origin (~a)."
                        (client-origin client)))
         (when query
           (caldav-fail "event_href must be a CalDAV resource path without query data."))
         (concatenate 'string origin (percent-encode-path path))))
      ((member (char href 0) '(#\/ #\#))
       (when (position #\# href)
         (caldav-fail "event_href must be a CalDAV resource path without a fragment."))
       (let ((path (if (client-base-path client)
                       (if (or (string= href (client-base-path client))
                               (uiop:string-prefix-p (concatenate 'string (client-base-path client) "/")
                                                     href))
                           href
                           (concatenate 'string (client-base-path client)
                                        (if (char= (char href 0) #\/) "" "/") href))
                       href)))
         (concatenate 'string (client-origin client) (percent-encode-path path))))
      (t (client-url client (concatenate 'string "/" href))))))

(defun percent-encode-path (path)
  "PATH re-encoded one segment at a time, so a spent escape is not double-encoded."
  (format nil "~{/~a~}" (mapcar (lambda (segment) (percent-encode (percent-decode segment)
                                                                   :safe "@"))
                                 (rest (uiop:split-string path :separator "/")))))

(defun client-collection-key (client href)
  "The collection HREF names, decoded and without its last slash, for comparing."
  (let ((url (client-url client href)))
    (multiple-value-bind (scheme host port path) (url-parts url)
      (declare (ignore scheme port))
      (string-right-trim "/" (format nil "https://~a~a" host (percent-decode path))))))

(defun client-request (client method href &key content headers)
  "One exchange with CLIENT's server => (values BODY STATUS ALIST-OF-HEADERS), BODY a string."
  (let ((response-headers nil)
        (body "")
        (status 0))
    (handler-case
        (multiple-value-bind (octets code received)
            (nlk:http method (client-url client href)
                      :headers (append (list (cons "Authorization"
                                                   (concatenate 'string "Basic "
                                                                (base64 (format nil "~a:~a"
                                                                                (client-login client)
                                                                                (client-password client)))))
                                             (cons "User-Agent" "nodecode-yandex-calendar"))
                                       headers)
                      :content content
                      :binary t
                      :timeout 30)
          (setf status code
                response-headers received
                body (if (streamp octets)
                         (with-output-to-string (out)
                           (loop for line = (read-line octets nil nil)
                                 while line do (write-string line out)))
                         (sb-ext:octets-to-string (or octets (make-array 0 :element-type '(unsigned-byte 8)))
                                                       :external-format :utf-8))))
      (error (condition)
        (caldav-fail "CalDAV request failed: ~a" condition)))
    (when (member status '(401 403))
      (caldav-fail "Authentication failed: check the login and app password in the ~
yandex_calendar section (an app password is required)."))
    (values body status response-headers)))

(defun client-header (headers name)
  "The value HEADERS carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

;;; --- discovery, and which calendar a name means ------------------------------

(defun parse-calendars (client body)
  "The calendars in a PROPFIND multistatus BODY, in the order the server lists them."
  (let* ((root (parse-xml body))
         (found '()))
    (dolist (response (xml-children root "response"))
      (let ((href (xml-find response "href"))
            (name (xml-find response "displayname")))
        (when (and href (xml-find response "calendar"))
          (let ((path (nth-value 3 (url-parts (client-url client (xml-text href))))))
            (push (cons path (string-trim '(#\Space #\Tab)
                                           (if name (xml-text name) "")))
                  found)))))
    (nreverse found)))

(defun calendar-keys (calendar)
  "The spellings an allow-list entry may use for CALENDAR: its display name and
the last segment of its href, lower-cased."
  (let ((segments (remove "" (uiop:split-string (car calendar) :separator "/")))
        (name (string-downcase (string-trim '(#\Space #\Tab) (cdr calendar)))))
    (remove "" (list name (string-downcase (or (car (last segments)) "")))
            :test #'string=)))

(defun calendar-position (calendar order)
  "Where CALENDAR sits in ORDER, or MOST-POSITIVE-FIXNUM for one it does not name."
  (or (loop for entry in order
            for index from 0
            when (member entry (calendar-keys calendar) :test #'string=)
              return index)
      most-positive-fixnum))

(defun calendar-allow-order (client)
  "CLIENT's allow-list, lower-cased, trimmed and deduplicated, in its own order."
  (remove-duplicates
   (loop for entry in (client-allowed client)
         for trimmed = (string-downcase (string-trim '(#\Space #\Tab) entry))
         when (plusp (length trimmed)) collect trimmed)
   :test #'string= :from-end t))

(defun client-calendars (client)
  "Every calendar the account holds, as (HREF . DISPLAY-NAME) pairs.
With an allow-list only its calendars are kept, in the order the list names
them, because the first calendar is where a write goes when none is named."
  (unless (client-calendar-list client)
    (multiple-value-bind (body status)
        (client-request client :propfind (client-home client)
                        :content (concatenate 'string
                                              "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
                                              "<d:propfind xmlns:d=\"DAV:\" "
                                              "xmlns:c=\"urn:ietf:params:xml:ns:caldav\">"
                                              "<d:prop><d:resourcetype/><d:displayname/>"
                                              "</d:prop></d:propfind>")
                        :headers '(("Depth" . "1")
                                   ("Content-Type" . "application/xml; charset=utf-8")))
      (when (>= status 400)
        (caldav-fail "Calendar discovery failed: HTTP ~a." status))
      (let ((order (calendar-allow-order client))
            (calendars (parse-calendars client body)))
        (setf (client-calendar-list client)
              (if (null order)
                  calendars
                  (stable-sort (remove-if (lambda (calendar)
                                            (= most-positive-fixnum
                                               (calendar-position calendar order)))
                                          calendars)
                               (lambda (a b)
                                 (< (calendar-position a order)
                                    (calendar-position b order)))))))))
  (client-calendar-list client))

(defun resolve-calendar (client reference)
  "The collection href REFERENCE names — a display name, the last segment of an
href, or the href itself — or the default calendar when it names none."
  (let ((reference (string-trim '(#\Space #\Tab) (or reference ""))))
    (if (and (plusp (length reference))
             (or (char= #\/ (char reference 0))
                 (uiop:string-prefix-p "https://" (string-downcase reference)))
             (null (client-allowed client)))
        (string-right-trim "/" (nth-value 3 (url-parts (client-url client reference))))
        (let ((calendars (client-calendars client)))
          (when (null calendars)
            (caldav-fail "No calendars are available on this account~@[ matching ~{~a~^, ~}~]."
                         (client-allowed client)))
          (if (zerop (length reference))
              (caar calendars)
              (or (let ((needle (string-downcase reference)))
                    (loop for calendar in calendars
                          when (or (member needle (calendar-keys calendar) :test #'string=)
                                   (handler-case
                                       (string-equal
                                        (client-collection-key client (car calendar))
                                        (client-collection-key client reference))
                                     (caldav-error () nil)))
                            return (car calendar)))
                  (caldav-fail "Calendar ~s not found. Available calendars: ~{~a~^, ~}."
                               reference (mapcar #'cdr calendars))))))))

;;; --- reading ----------------------------------------------------------------------

(defun event-path-p (path)
  "Whether PATH names an event resource rather than a collection."
  (let ((segments (uiop:split-string path :separator "/")))
    (and (plusp (length path))
         (not (char= #\/ (char path (1- (length path)))))
         (uiop:string-suffix-p (string-downcase (car (last segments))) ".ics")
         (notany (lambda (segment) (member segment '("." "..") :test #'string=)) segments)
         (null (position #\\ path)))))

(defun event-path (client href)
  "HREF as the path of one event resource, or CALDAV-ERROR."
  (let ((url (client-url client href)))
    (multiple-value-bind (scheme host port path query) (url-parts url)
      (declare (ignore scheme host port))
      (when query
        (caldav-fail "event_href must be a CalDAV resource path without query data."))
      (let ((path (percent-decode path)))
        (unless (event-path-p path)
          (caldav-fail "event_href must identify an event resource (a path ending in .ics)."))
        path))))

(defun href-allowed-p (client href)
  "Whether HREF sits in one of the calendars CLIENT may use."
  (let ((allowed (client-allowed client)))
    (or (null allowed)
        (let ((parent (format nil "~a/"
                               (string-right-trim "/"
                                                   (subseq href 0 (or (position #\/ href :from-end t)
                                                                      (length href)))))))
          (some (lambda (calendar) (string= (client-collection-key client (car calendar))
                                             (client-collection-key client parent)))
                (client-calendar-list client))))))

(defun query-events (client start end &key calendar)
  "The events in CALENDAR (or the default one) overlapping [START, END)."
  (let* ((collection (resolve-calendar client calendar))
         (body nil)
         (status 0))
    (multiple-value-setq (body status)
      (client-request client :report (concatenate 'string collection "/")
                      :content (format nil "<?xml version=\"1.0\" encoding=\"utf-8\"?>~
<c:calendar-query xmlns:d=\"DAV:\" xmlns:c=\"urn:ietf:params:xml:ns:caldav\">~
<d:prop><d:getetag/><c:calendar-data/></d:prop>~
<c:filter><c:comp-filter name=\"VCALENDAR\"><c:comp-filter name=\"VEVENT\">~
<c:time-range start=\"~a\" end=\"~a\"/>~
</c:comp-filter></c:comp-filter></c:filter></c:calendar-query>"
                                (moment-utc-stamp start) (moment-utc-stamp end))
                      :headers '(("Depth" . "1")
                                 ("Content-Type" . "application/xml; charset=utf-8"))))
    (when (>= status 400)
      (caldav-fail "Listing events failed: HTTP ~a." status))
    (let ((events '()))
      (dolist (response (xml-children (parse-xml body) "response"))
        (let ((data (xml-find response "calendar-data"))
              (href (xml-find response "href"))
              (etag (xml-find response "getetag")))
          (when (and data (plusp (length (string-trim " " (xml-text data)))))
            (let ((location (and href (nth-value 3 (url-parts (client-url client (xml-text href))))))
                  (tag (if etag (string-trim '(#\Space #\Tab) (xml-text etag)) "")))
              (dolist (event (parse-ical (xml-text data)))
                (setf (event-href event) location
                      (event-etag event) tag)
                (push event events))))))
      (nreverse events))))

(defun fetch-document (client href)
  "HREF's raw iCalendar text, its events and its ETag, or NIL when it is gone."
  (let ((path (event-path client href)))
    (unless (href-allowed-p client path)
      (caldav-fail "Event ~s is not in an allowed calendar." path))
    (multiple-value-bind (body status headers) (client-request client :get path)
      (cond ((eql status 404) nil)
            ((>= status 400) (caldav-fail "Fetching event failed: HTTP ~a." status))
            (t (values body (parse-ical body) (or (client-header headers "etag") "")))))))

(defun fetch-event (client href)
  "One event resource as an ICAL-EVENT, or NIL when it is gone.
A resource holding several components is refused: this model keeps one, so
writing it back would drop the others."
  (multiple-value-bind (text events etag) (fetch-document client href)
    (declare (ignore text))
    (when (and events (rest events))
      (caldav-fail "This resource holds ~a components (a recurring event and its ~
modified occurrences); editing it here would drop them." (length events)))
    (when events
      (let ((event (first events)))
        (setf (event-href event) (nth-value 3 (url-parts (client-url client href)))
              (event-etag event) etag)
        event))))

;;; --- writing ------------------------------------------------------------------------

(defun account-address (client)
  "The account's own address, as the server spells its login."
  (let ((login (client-login client)))
    (if (position #\@ login) login (concatenate 'string login "@yandex.ru"))))

(defun ensure-organizer (client event)
  "A meeting with attendees names its organizer: the account owner."
  (when (and (event-attendees event) (null (event-organizer event)))
    (setf (event-organizer event) (make-attendee :email (account-address client)))))

(defun put-new-event (client event &key calendar)
  "PUT EVENT into CALENDAR (or the default one) => the event with its href and UID."
  (let ((collection (resolve-calendar client calendar)))
    (unless (plusp (length (event-uid event)))
      (setf (event-uid event)
            (format nil "~a-~a@nodecode-yandex-calendar" (get-universal-time)
                            (random 1000000000))))
    (ensure-organizer client event)
    (let* ((path (format nil "~a/~a.ics" (string-right-trim "/" collection)
                          (percent-encode (event-uid event))))
           (body nil)
           (status 0))
      (declare (ignorable body))
      (multiple-value-setq (body status)
        (client-request client :put path
                        :content (build-ical event)
                        :headers '(("Content-Type" . "text/calendar; charset=utf-8")
                                   ("If-None-Match" . "*"))))
      (unless (member status '(200 201 204))
        (caldav-fail "Creating the event failed: HTTP ~a." status))
      (setf (event-href event) (percent-decode path))
      event)))

(defun put-event (client event href)
  "PUT EVENT back to HREF, unconditionally (a freshly built event).
A caller holding an ETag writes conditionally, which is the conditional form."
  (let ((path (event-path client href)))
    (ensure-organizer client event)
    (multiple-value-bind (body status headers)
        (client-request client :put path
                        :content (build-ical event)
                        :headers (append '(("Content-Type" . "text/calendar; charset=utf-8"))
                                         (when (plusp (length (event-etag event)))
                                           (list (cons "If-Match" (event-etag event))))))
      (declare (ignore body))
      (when (eql status 412)
        (caldav-fail "The event changed on the server since it was read, so it was left ~
untouched. Read it again and reapply the change."))
      (unless (member status '(200 201 204))
        (caldav-fail "Updating the event failed: HTTP ~a." status))
      (setf (event-href event) (percent-decode path)
            (event-etag event) (or (client-header headers "etag") ""))
      event)))

(defun answer-invitation (client href partstat)
  "Set the account owner's PARTSTAT on HREF, which is how CalDAV answers an invitation."
  (let ((event (fetch-event client href)))
    (unless event
      (caldav-fail "Event not found: ~a" href))
    (let* ((me (string-downcase (account-address client)))
           (mine (find-if (lambda (attendee)
                            (string= (fold-mail-domain (attendee-email attendee)) me))
                          (event-attendees event))))
      (unless mine
        (caldav-fail "~a is not listed as an attendee of this event; cannot respond."
                     (client-login client)))
      (setf (attendee-partstat mine) partstat
            (attendee-rsvp mine) :false)
      (put-event client event href))))

(defun fold-mail-domain (address)
  "ADDRESS lower-cased, Yandex's interchangeable mail domains folded to one."
  (let* ((address (string-downcase (string-trim '(#\Space #\Tab) address)))
         (at (position #\@ address :from-end t)))
    (if (and at (member (subseq address (1+ at))
                        '("ya.ru" "yandex.ru" "yandex.com" "yandex.by" "yandex.kz"
                          "yandex.ua" "narod.ru")
                        :test #'string=))
        (concatenate 'string (subseq address 0 (1+ at)) "yandex.ru")
        address)))

(defun copy-into (client text target uid)
  "PUT TEXT into TARGET under UID and prove the copy is readable => its path.
A 2xx is not proof: some Yandex calendars are read-only and accept a PUT
without storing anything, so the copy is read back before the original goes."
  (let* ((path (concatenate 'string (string-right-trim "/" target) "/"
                             (percent-encode uid) ".ics"))
         (body nil)
         (status 0))
    (declare (ignorable body))
    (multiple-value-setq (body status)
      (client-request client :put path
                      :content text
                      :headers '(("Content-Type" . "text/calendar; charset=utf-8")
                                 ("If-None-Match" . "*"))))
    (unless (member status '(200 201 204))
      (caldav-fail "Moving the event failed: HTTP ~a." status))
    (multiple-value-bind (stored events) (fetch-document client path)
      (declare (ignore stored))
      (when (null events)
        (caldav-fail "~s accepted the copy but does not store it, so the event was left ~
where it is. Some Yandex calendars are read-only in this way." target)))
    (percent-decode path)))

(defun relocate-event (client href target)
  "Move an event to TARGET: a verbatim copy, a read-back, then a conditional DELETE."
  (multiple-value-bind (text events etag) (fetch-document client href)
    (unless events
      (caldav-fail "Event not found: ~a" href))
    (let* ((event (first events))
           (uid (event-uid event))
           (path (event-path client href))
           (destination (resolve-calendar client target)))
      (when (zerop (length uid))
        (caldav-fail "Cannot move an event without a UID."))
      (cond
        ((string= (client-collection-key client path) (client-collection-key client destination))
         (setf (event-href event) path)
         event)
        (t
         (let ((copied (copy-into client text destination uid)))
           (handler-case
               (remove-event client href :etag etag)
             (caldav-error (condition)
               (caldav-fail "The event was copied to ~a, but the original at ~a could not be ~
removed (~a); it is now in both calendars." copied href condition)))
           (setf (event-href event) copied)
           event))))))

(defun remove-event (client href &key (etag ""))
  "DELETE an event resource, conditionally when ETAG names the version read."
  (let ((path (event-path client href)))
    (unless (href-allowed-p client path)
      (caldav-fail "Event ~s is not in an allowed calendar." path))
    (multiple-value-bind (body status)
        (client-request client :delete path
                        :headers (when (plusp (length etag))
                                   (list (cons "If-Match" etag))))
      (declare (ignore body))
      (when (eql status 412)
        (caldav-fail "The event changed on the server since it was read, so it was not ~
deleted. Read it again and delete it if that is still what you want."))
      (unless (member status '(200 204 404))
        (caldav-fail "Deleting the event failed: HTTP ~a." status))
      t)))
