;;;; wire.lisp --- the Diet proceedings API, its pages, and the block each call answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Everything the cell knows about kokkai.ndl.go.jp: two paths, one
;;;; parameter set, the page checks the plugin made (a page must hold every
;;;; match, or the call says so), the excerpt rule, and the credit line the
;;;; open-data licence asks for. CELL.LISP and WATCH.LISP are the callers.
;;;;
;;;; The only peer is https://kokkai.ndl.go.jp, over GET, with no key. The
;;;; user's words leave the machine (that is the search); nothing else does.
;;;; Nothing is written here, spawned, or read from the environment.

(in-package #:nodecode-kokkai)

;;; --- what the peer is, and its bounds ------------------------------------------

(defparameter +api-base+ "https://kokkai.ndl.go.jp"
  "The one host. Both paths are built from it, and no caller names another.")

(defparameter +meeting-path+ "/api/meeting_list"
  "The meeting list: meetings, with their speech rows folded inside each.")

(defparameter +speech-path+ "/api/speech"
  "The speech search: the rows themselves, with the words spoken.")

(defparameter +api-page+ "https://kokkai.ndl.go.jp/api.html"
  "The API's own page, cited when a page carries no meeting or speech URL.")

(defparameter +pdl-url+ "https://www.digital.go.jp/resources/open_data/public_data_license_v1.0"
  "PDL 1.0, the licence the credit line names.")

(defparameter +user-agent+ "nodecode-kokkai/0.1.0 (+https://github.com/nodecode-ai/cells)"
  "Who the API sees asking: the cell, and where it comes from.")

(defparameter +max-response-bytes+ (* 2 1000 1000)
  "The largest page one call accepts. A watch keeps it too, so a page that
big is a page to narrow, never one to page through.")

(defparameter +excerpt-chars+ 240
  "How much of one speech an excerpt keeps, as the original kept it.")

(defparameter +speaker-cap+ 30
  "How many distinct speakers one meeting summary names.")

(defparameter +search-max+ 30
  "The API's own maximumRecords ceiling on the meeting list.")

(defparameter +speech-max+ 10
  "The most speech rows one call shows.")

(defparameter +query-limit+ 2000
  "The longest query, in encoded bytes, the API is asked to read.")

(defparameter +houses+ '("衆議院" "参議院" "両院" "両院協議会")
  "The house values the API reads. An unknown house is silently dropped by
the API -- the call comes back with the count a search with no house has --
so it is refused here, before the request.")

(defparameter +date-scanner+ (cl-ppcre:create-scanner "^\\d{4}-\\d{2}-\\d{2}$")
  "A date as the API reads one, in full.")

;;; --- the one floor the API asks for ---------------------------------------------

(defparameter *gap-seconds* 5.0
  "The least time between one answer and the next request. The API's own
note asks for a wait on the order of several seconds; this cell's floor is
five, and it is shared by both paths. Tests bind it to zero.")

(defvar *last-done* nil
  "When the last answer arrived, on *CLOCK*, or NIL before the first one.")

(defvar *clock* (lambda () (/ (get-internal-real-time) (float internal-time-units-per-second)))
  "The cell's clock. A test binds a function of its own.")

(defun gap-remaining ()
  "Seconds still to wait before another request may be sent, or NIL."
  (when *last-done*
    (max 0.0 (- *gap-seconds* (- (funcall *clock*) *last-done*)))))

(defun check-gap ()
  "Refuse before a request the API's own pace would rather not see yet."
  (let ((left (gap-remaining)))
    (when (and left (plusp left))
      (fail "the previous answer was ~,1f seconds ago; wait ~,1f more seconds and send one request, not two in one turn"
            (- *gap-seconds* left) left))))

;;; --- the request ---------------------------------------------------------------

(defun query-string (params)
  "PARAMS, an alist of (name . value), as an encoded query string."
  (format nil "~{~a=~a~^&~}"
          (loop for (name . value) in params
                append (list (quri:url-encode (string name))
                             (quri:url-encode (princ-to-string value) :encoding :utf-8)))))

(defun http (url params timeout)
  "The cell's one network call: NLK:HTTP GET of URL with PARAMS, the body
as text under the byte cap. Answers (values TEXT STATUS). Tests stub this
name and never touch the network."
  (multiple-value-bind (body status)
      (nlk:http :get (format nil "~a?~a" url (query-string params))
                :headers `(("user-agent" . ,+user-agent+)
                           ("accept" . "application/json"))
                :timeout timeout
                :max-bytes +max-response-bytes+)
    (values (if (stringp body) body (nlk:decode-octets body)) status)))

(defun call-path (path params timeout)
  "One GET of PATH with PARAMS => the decoded JSON page as a hash table."
  (check-gap)
  (unwind-protect
       (multiple-value-bind (body status) (http (format nil "~a~a" +api-base+ path) params timeout)
         (let ((json (ignore-errors (and (plusp (length body)) (nlk:decode-json body)))))
           (unless (eql status 200)
             (fail "kokkai.ndl.go.jp answered HTTP ~a~@[: ~a~]"
                   status (json-text json "message")))
           (unless (hash-table-p json)
             (fail "the answer from kokkai.ndl.go.jp was not a JSON page"))
           json))
    (setf *last-done* (funcall *clock*))))

;;; --- json, read leniently -------------------------------------------------------

(defun json-field (object key)
  "OBJECT's member KEY when OBJECT is a JSON object, else NIL."
  (and (hash-table-p object) (gethash key object)))

(defun json-text (object key &optional default)
  "OBJECT's member KEY as text: DEFAULT when it is absent, null or not text."
  (let ((value (json-field object key)))
    (if (stringp value) value default)))

(defun json-count (object key)
  "OBJECT's member KEY as an integer, 0 when it is absent or not a number."
  (let ((value (json-field object key)))
    (cond ((integerp value) value)
          ((floatp value) (round value))
          (t 0))))

(defun json-list (value)
  "VALUE as a list: a JSON array decodes to a vector, and anything else is
no list at all."
  (cond ((vectorp value) (coerce value 'list))
        ((listp value) (copy-list value))
        (t nil)))

;;; --- what a caller may ask ------------------------------------------------------

(defun trimmed* (text)
  "TEXT as a trimmed string, \"\" for NIL."
  (nlk:trimmed (or text "")))

(defun check-house (house)
  "HOUSE, or \"\", refused when the API would silently drop it."
  (let ((house (trimmed* house)))
    (when (and (plusp (length house)) (not (member house +houses+ :test #'string=)))
      (fail "house is ~s; it is 衆議院, 参議院, 両院 or 両院協議会, and the API drops an unknown one" house))
    house))

(defun plausible-date-p (text)
  "T when TEXT is a YYYY-MM-DD date the calendar has."
  (and (cl-ppcre:scan +date-scanner+ text)
       (let ((month (parse-integer text :start 5 :end 7))
             (day (parse-integer text :start 8 :end 10)))
         (and (<= 1 month 12) (<= 1 day 31)))))

(defun check-dates (from until)
  "The (values FROM UNTIL) a call may ask for: together or neither, in
order, each a real date."
  (let ((from (trimmed* from))
        (until (trimmed* until)))
    (when (or (and (plusp (length from)) (zerop (length until)))
              (and (plusp (length until)) (zerop (length from))))
      (fail "from and until are passed together, as YYYY-MM-DD; one without the other was not requested"))
    (dolist (pair (list (cons "from" from) (cons "until" until)))
      (when (and (plusp (length (cdr pair))) (not (plausible-date-p (cdr pair))))
        (fail "~a is ~s; it is a YYYY-MM-DD date" (car pair) (cdr pair))))
    (when (and (plusp (length from)) (plusp (length until)) (string> from until))
      (fail "from ~a is after until ~a" from until))
    (values from until)))

(defun check-window (from until)
  "The (values FROM UNTIL) of one request: the dates checked, and until
defaulting to today in Tokyo so a search with one date still has both."
  (multiple-value-bind (from until) (check-dates from until)
    (if (and (plusp (length from)) (zerop (length until)))
        (values from from)
        (values from until))))

(defun check-query-length (pairs)
  "PAIRS encoded, refused when longer than the API reads."
  (let ((encoded (query-string pairs)))
    (when (> (length (sb-ext:string-to-octets encoded :external-format :utf-8)) +query-limit+)
      (fail "the query is longer than ~d bytes; shorten the words" +query-limit+))
    pairs))

;;; --- the day, and the credit ---------------------------------------------------

(defun tokyo-today ()
  "Today's date in Asia/Tokyo, as YYYY-MM-DD. Fixed at UTC+9: the API's
window is Tokyo's, and a fixed offset is what the original used."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 9)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun credit (urls)
  "The credit line PDL 1.0 and the API's terms ask for, naming the URLs
that were used (the API's own page when a page carries none)."
  (format nil "出典：国立国会図書館「国会会議録検索システム」（~a）、PDL1.0（~a）（~aに利用）。国立国会図書館「国会会議録検索システム」の検索結果を nodecode-kokkai が選択して作成。"
          (if urls (format nil "~{~a~^、~}" urls) +api-page+)
          +pdl-url+
          (tokyo-today)))

;;; --- the meeting list -----------------------------------------------------------

(defun meeting-urls (records)
  "The meetingURL of each record, in order, once each."
  (remove-duplicates (remove nil (mapcar (lambda (record) (json-text record "meetingURL")) records))
                     :test #'string=))

(defun speech-ids (records)
  "The speechID of every speech row in RECORDS, in order."
  (loop for record in records
        append (loop for speech in (json-list (json-field record "speechRecord"))
                     collect (json-text speech "speechID"))))

(defun speakers-of (speeches)
  "The distinct speakers of SPEECHES, in order, at most +SPEAKER-CAP+."
  (let ((names '()))
    (dolist (speech speeches (nreverse names))
      (let ((name (json-text speech "speaker")))
        (when (and name (plusp (length name)) (not (member name names :test #'string=))
                   (< (length names) +speaker-cap+))
          (push name names))))))

(defun render-meeting (record)
  "One meeting as the line block a search answers with."
  (let* ((speeches (json-list (json-field record "speechRecord")))
         (names (speakers-of speeches))
         (url (json-text record "meetingURL" +api-page+))
         (session (json-text record "session" ""))
         (issue (json-text record "issue" ""))
         (date (json-text record "date" "")))
    (format nil "- ~a ~a (~@[~a ~]~a ~a)~%  ~a~%  ~d speeches, ~d speakers~a"
            (json-text record "nameOfHouse" "") (json-text record "nameOfMeeting" "")
            (if (plusp (length session)) session nil) issue date url
            (length speeches) (length names)
            (if names (format nil ": ~{~a~^, ~}" names) ""))))

(defun page-pairs (any speaker house from until count)
  "The parameters of one meeting-list request."
  (check-query-length
   (remove-if (lambda (pair) (string= "" (cdr pair)))
              (list (cons "any" (trimmed* any))
                    (cons "speaker" (trimmed* speaker))
                    (cons "nameOfHouse" (trimmed* house))
                    (cons "from" (trimmed* from))
                    (cons "until" (trimmed* until))
                    (cons "maximumRecords" (princ-to-string count))
                    (cons "recordPacking" "json")
                    (cons "startRecord" "1")))))

(defun meeting-page (any speaker house from until count)
  "One meeting-list page, with the two checks the original made: the page
must hold every match, and a row without a speechID is not a page a watch
may mark seen from. => the page as a hash table."
  (let* ((pairs (page-pairs any speaker house from until count))
         (page (call-path +meeting-path+ pairs (setting :timeout-seconds)))
         (total (json-count page "numberOfRecords"))
         (returned (json-count page "numberOfReturn")))
    (when (< returned total)
      (fail "~d meetings match and one page holds ~d, so this is not the whole result: ~@[set maximum_records to ~d, or ~]shorten from and until and ask again"
            total returned (and (<= total +search-max+) total)))
    (when (some #'null (speech-ids (json-list (json-field page "meetingRecord"))))
      (fail "a speech row on this page has no speechID, so the page was not used"))
    page))

(defun meetings (any speaker house from until maximum-records)
  "The `kokkai_search' answer: which Diet meetings match, each with the
official meeting URL, and the credit line."
  (multiple-value-bind (from until) (check-window from until)
    (let* ((house (check-house house))
           (any (trimmed* any))
           (speaker (trimmed* speaker))
           (count (or maximum-records 10)))
      (when (and (zerop (length any)) (zerop (length speaker)))
        (unless (or (plusp (length house)) (plusp (length from)))
          (fail "a search needs any, speaker, a house, or both dates; nothing was requested")))
      (unless (<= 1 count +search-max+)
        (fail "maximum_records is ~a; it is 1 through ~d" count +search-max+))
      (let* ((page (meeting-page any speaker house from until count))
             (records (json-list (json-field page "meetingRecord")))
             (total (json-count page "numberOfRecords")))
        (format nil "~a~%~%~a~%~%~a"
                (if (zerop total)
                    "Japan Diet meetings: 0 meetings. Names are copied as the API sent them, and speech text is not included."
                    (format nil "Japan Diet meetings: ~d~:[~; (the page holds every match)~]. Names are copied as the API sent them, and speech text is not included."
                            total (plusp (length records))))
                (if records (format nil "~{~a~^~%~}" (mapcar #'render-meeting records)) "(no meeting rows)")
                (credit (meeting-urls records)))))))

;;; --- the speech search ----------------------------------------------------------

(defun excerpt (text)
  "The (values EXCERPT CUT-P) of TEXT: its first +EXCERPT-CHARS+
characters, and a mark when there was more."
  (let ((text (or text "")))
    (if (<= (length text) +excerpt-chars+)
        (values text nil)
        (values (format nil "~a…" (subseq text 0 +excerpt-chars+)) t))))

(defun render-speech (record)
  "One speech as the line block a speech search answers with."
  (multiple-value-bind (excerpt cut) (excerpt (json-text record "speech" ""))
    (format nil "- ~a, ~a, ~a~%  ~a~%  \"~a\"~@[ (excerpt cut)~]"
            (json-text record "speaker" "") (json-text record "nameOfMeeting" "")
            (json-text record "date" "") (json-text record "speechURL" +api-page+)
            excerpt (and cut t))))

(defun speeches (any speaker house from until maximum-records)
  "The `kokkai_speech' answer: the first characters of each matching
speech, who said it, where, and the credit line."
  (multiple-value-bind (from until) (check-window from until)
    (let* ((house (check-house house))
           (any (trimmed* any))
           (speaker (trimmed* speaker))
           (count (or maximum-records 5)))
      (when (and (zerop (length any)) (zerop (length speaker)))
        (fail "speaker or any is required for a speech search; nothing was requested"))
      (unless (<= 1 count +speech-max+)
        (fail "maximum_records is ~a; for speeches it is 1 through ~d" count +speech-max+))
      (let* ((pairs (check-query-length
                     (remove-if (lambda (pair) (string= "" (cdr pair)))
                                (list (cons "any" any)
                                      (cons "speaker" speaker)
                                      (cons "nameOfHouse" house)
                                      (cons "from" from)
                                      (cons "until" until)
                                      (cons "maximumRecords" (princ-to-string count))
                                      (cons "recordPacking" "json")
                                      (cons "startRecord" "1")))))
             (page (call-path +speech-path+ pairs (setting :timeout-seconds)))
             (total (json-count page "numberOfRecords"))
             (records (json-list (json-field page "speechRecord")))
             (cut (some (lambda (record) (nth-value 1 (excerpt (json-text record "speech" ""))))
                        records))
             (missing (count nil (mapcar (lambda (record) (json-text record "speech")) records))))
        (format nil "~a~%~%~a~%~%~a"
                (format nil "Japan Diet speeches: ~d in total. Showing ~d. ~a. Each excerpt keeps the first ~d characters; a longer speech adds …, so its excerpt is ~d.~@[ ~d row~p had no speech text, so do not quote one.~]"
                        total (length records) (if cut "truncated" "not truncated")
                        +excerpt-chars+ (1+ +excerpt-chars+) missing missing)
                (if records (format nil "~{~a~^~%~}" (mapcar #'render-speech records)) "(no speech rows)")
                (credit (remove-duplicates
                         (remove nil (mapcar (lambda (record) (json-text record "speechURL")) records))
                         :test #'string=)))))))
