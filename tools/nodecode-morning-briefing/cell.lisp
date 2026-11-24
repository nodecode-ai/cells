;;;; cell.lisp --- the section, the two feeds, the page, the tool, the command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of drkpxl/morning-briefing's rubric as data and code: the same
;;;; two sources it calls directly (the National Weather Service's forecast API
;;;; and the Atom feed of each subreddit), the same 36-hour freshness rule, the
;;;; same newsprint page, written as one HTML file a browser prints. What the
;;;; original got from Hermes's own connectors -- Home Assistant calendars and
;;;; air quality, Gmail newsletters, X search -- is left to the model reading
;;;; this page's JSON-free text, and the PDF/QR/printer steps are gone.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "morning-briefing": {"latitude": "44.05", "longitude": "-121.31", ...}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-morning-briefing)

(nlk:define-peripheral morning-briefing :not-running t)

(defparameter *user-agent* "nodecode-morning-briefing/0.1 (Nodecode cell; one page a morning)"
  "Who is asking. The weather service refuses a client that will not say, and
Reddit hands a bare one 429s.")

(defparameter *default-subreddits* '("news" "worldnews" "technology" "science")
  "The feeds a briefing reads when the section names none of its own.")

(defparameter +primer+
  "The morning_briefing tool assembles today's one-page briefing: the weather
where the section points and the freshest headlines from the subreddits it
names, laid out as a newsprint page and written as one HTML file (8.5x11,
black on white) that you can open or print.

  morning_briefing topics=[\"science\", \"technology\"] hours=36 max_items=12
      topics replaces the configured subreddits for this one page; hours is how
      far back a headline may be (the original's rule is 36); max_items caps
      how many fit. It answers the path of the page and a plain-text digest of
      what is on it: dateline, weather, then one line a headline.

Rules. Every story must carry a feed timestamp inside the window: a thin news
day is honest, stale news is not. A dead source costs only its own section,
which says so in its own words. Say where the page is, and quote a few
headlines rather than the whole page. Errors read ERROR: MORNING-BRIEFING-ERROR
with the reason: fix that and call again, do not repeat the call."
  "What (help :briefing) answers while the cell runs.")

;;; --- the section, read ----------------------------------------------------------

(defun configured (key default)
  "KEY's value from the running section, or DEFAULT when it is unset or blank."
  (let ((value (setting key)))
    (cond ((null value) default)
          ((and (stringp value) (zerop (length (nlk:trimmed value)))) default)
          ((and (vectorp value) (zerop (length value))) default)
          (t value))))

(defun as-list (value)
  "VALUE as a list: a JSON array reads as a vector."
  (cond ((null value) '())
        ((listp value) value)
        ((vectorp value) (coerce value 'list))
        (t (list value))))

(defun coordinates ()
  "The section's latitude and longitude, or a refusal that names them."
  (let ((latitude (nlk:trimmed (or (setting :latitude) "")))
        (longitude (nlk:trimmed (or (setting :longitude) ""))))
    (when (zerop (length latitude))
      (fail "the briefing needs a place: set morning-briefing.latitude and ~
 morning-briefing.longitude in /settings -- the coordinates of anywhere you ~
care about, the weather service rounds them to its nearest forecast grid -- ~
 then call again"))
    (when (zerop (length longitude))
      (fail "the briefing needs a longitude: set morning-briefing.longitude in ~
/settings beside the latitude, then call again"))
    (values latitude longitude)))

;;; --- one HTTP read ---------------------------------------------------------------

(defun host-of (url)
  (or (ignore-errors (quri:uri-host (quri:uri url))) url))

(defun fetch (url &optional (accept "application/json"))
  "URL's body and status, or (values NIL WHY). A refusal is a value: one dead
source costs its own section, never the page."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get url
                          :headers (list (cons "User-Agent" *user-agent*)
                                         (cons "Accept" accept))
                          :timeout (configured :timeout-seconds 20))
        (cond ((null body) (values nil (format nil "~a answered nothing" (host-of url))))
              ((and (integerp status) (<= 200 status 299)) (values body nil))
              (t (values nil (format nil "~a answered ~a" (host-of url) status)))))
    (error (condition)
      (values nil (format nil "~a could not be reached (~a)" (host-of url) condition)))))

(defun fetch-json (url)
  "URL's body decoded, or (values NIL WHY)."
  (multiple-value-bind (body error) (fetch url)
    (if error
        (values nil error)
        (handler-case (values (shasht:read-json body) nil)
          (error () (values nil (format nil "~a answered something that is not JSON" (host-of url))))))))

;;; --- weather --------------------------------------------------------------------

(defun radar-url ()
  "The color radar GIF of the section's station, or NIL when it names none."
  (let ((station (configured :radar-station nil)))
    (and station
         (format nil "https://radar.weather.gov/ridge/standard/~a_0.gif"
                 (string-upcase (nlk:trimmed station))))))

(defun weather-wind (period)
  (let ((direction (nlk:json-value period :string "windDirection"))
        (speed (nlk:json-value period :string "windSpeed")))
    (and (or direction speed) (format nil "~@[~a ~]~@[~a~]" direction speed))))

(defun period-line (period)
  "One forecast period as a line: its name, its temperature, its sky."
  (and period
       (format nil "~a: ~a~a, ~a"
               (or (nlk:json-value period :string "name") "next")
               (nlk:json-value period :integer "temperature")
               (or (nlk:json-value period :string "temperatureUnit") "")
               (or (nlk:json-value period :string "shortForecast") ""))))

(defun weather ()
  "The day's weather where the section points, as a plist, or (:error TEXT)."
  (multiple-value-bind (latitude longitude) (coordinates)
    (multiple-value-bind (points error)
        (fetch-json (format nil "https://api.weather.gov/points/~a,~a" latitude longitude))
      (if error
          (list :error error)
          (let ((grid (nlk:json-value points :string "properties" "forecast")))
            (if (null grid)
                (list :error "the weather service has no forecast grid for those coordinates")
                (multiple-value-bind (forecast error) (fetch-json grid)
                  (if error
                      (list :error error)
                      (let* ((periods (nlk:json-value forecast :array "properties" "periods"))
                             (today (and periods (plusp (length periods)) (aref periods 0))))
                        (list :error nil
                              :describe (nlk:json-value today :string "name")
                              :temperature (nlk:json-value today :integer "temperature")
                              :unit (or (nlk:json-value today :string "temperatureUnit") "")
                              :short (or (nlk:json-value today :string "shortForecast") "")
                              :wind (weather-wind today)
                              :detailed (nlk:json-value today :string "detailedForecast")
                              :next (and periods (> (length periods) 1)
                                         (period-line (aref periods 1)))
                              :radar (radar-url)))))))))))

;;; --- headlines ------------------------------------------------------------------

(defun unescape-xml (text)
  "TEXT with the five XML entities a feed writes undone, &amp; last."
  (when text
    (dolist (pair '(("&#39;" . "'") ("&quot;" . "\"") ("&lt;" . "<") ("&gt;" . ">") ("&amp;" . "&"))
                    text)
      (setf text (cl-ppcre:regex-replace-all (car pair) text (cdr pair))))))

(defun tag-text (chunk tag)
  "The text inside CHUNK's first <TAG ...>...</TAG>, entities undone."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings (format nil "(?s)<~a[^>]*>(.*?)</~a>" tag tag) chunk)
    (declare (ignore match))
    (and groups (aref groups 0) (unescape-xml (aref groups 0)))))

(defun tag-href (chunk)
  "CHUNK's first href, entities undone: an Atom entry's link."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "href=\"([^\"]*)\"" chunk)
    (declare (ignore match))
    (and groups (aref groups 0) (unescape-xml (aref groups 0)))))

(defun atom-entries (xml)
  "Every <entry> of an Atom feed as (TITLE LINK UPDATED), in feed order."
  (let ((entries '()) (from 0))
    (loop
      (let ((start (search "<entry" xml :start2 from)))
        (unless start (return (nreverse entries)))
        (let* ((open (or (position #\> xml :start start) (length xml)))
               (end (or (search "</entry>" xml :start2 open) (length xml)))
               (chunk (subseq xml open (min end (length xml)))))
          (push (list (tag-text chunk "title") (tag-href chunk) (tag-text chunk "updated"))
                entries)
          (setf from (max (1+ start) (min (length xml) end))))))))

(defun iso->universal (text)
  "TEXT, an ISO-8601 timestamp, as universal time; NIL when it is not one."
  (when (and (stringp text) (>= (length text) 19))
    (handler-case
        (let ((year (parse-integer text :start 0 :end 4))
              (month (parse-integer text :start 5 :end 7))
              (day (parse-integer text :start 8 :end 10))
              (hour (parse-integer text :start 11 :end 13))
              (minute (parse-integer text :start 14 :end 16))
              (second (parse-integer text :start 17 :end 19))
              (offset 0))
          (let ((sign (and (> (length text) 19)
                           (position-if (lambda (character) (member character '(#\+ #\-)))
                                        text :start 19))))
            (when (and sign (>= (length text) (+ sign 3)))
              (let ((hours (parse-integer text :start (1+ sign) :end (+ sign 3)))
                    (minutes (if (>= (length text) (+ sign 6))
                                 (parse-integer text :start (+ sign 4) :end (+ sign 6))
                                 0)))
                (setf offset (* (if (char= (char text sign) #\-) -1 1)
                                (+ (* hours 3600) (* minutes 60)))))))
          (- (encode-universal-time second minute hour day month year 0) offset))
      (error () nil))))

(defun feed-stories (subreddit hours)
  "=> (values STORIES ERROR): SUBREDDIT's entries inside HOURS, newest first."
  (multiple-value-bind (body error)
      (fetch (format nil "https://www.reddit.com/r/~a/new/.rss?limit=25" subreddit)
             "application/atom+xml")
    (if error
        (values nil (format nil "r/~a: ~a" subreddit error))
        (let* ((cutoff (- (get-universal-time) (* hours 3600)))
               (fresh (loop for (title href updated) in (atom-entries body)
                            for at = (iso->universal updated)
                            when (and title href at (>= at cutoff))
                              collect (list :title title :link href
                                            :subreddit subreddit :at at))))
          (values (sort fresh #'> :key (lambda (story) (getf story :at))) nil)))))

(defun collect-stories (subreddits hours limit)
  "=> (values STORIES ERRORS): every fresh story, newest first, LIMIT of them."
  (let ((stories '()) (errors '()))
    (dolist (subreddit subreddits)
      (multiple-value-bind (found error) (feed-stories subreddit hours)
        (if error (push error errors) (setf stories (append stories found)))))
    (let* ((sorted (stable-sort stories #'> :key (lambda (story) (getf story :at))))
           (unique (remove-duplicates sorted :test #'equal
                                              :key (lambda (story) (getf story :title)))))
      (values (subseq unique 0 (min limit (length unique))) (nreverse errors)))))

;;; --- the page -------------------------------------------------------------------

(defparameter +stylesheet+
  "@page { size: letter; margin: 0.4in 0.5in; }
* { box-sizing: border-box; margin: 0; padding: 0; }
body { font-family: 'Iowan Old Style', 'Palatino Linotype', 'Book Antiqua', Palatino, Georgia, serif;
       font-size: 10pt; color: #1a1a1a; line-height: 1.4; }
.masthead { border-bottom: 3px double #000; padding-bottom: 6px; margin-bottom: 10px; text-align: center; }
.masthead h1 { font-size: 30pt; font-weight: 900; letter-spacing: -1px; }
.dateline { font-size: 8pt; margin-top: 3px; text-transform: uppercase; letter-spacing: 2px; color: #555; }
.section-title { font-family: Helvetica, Arial, sans-serif; font-size: 8pt; font-weight: 700;
                 text-transform: uppercase; letter-spacing: 1.5px; color: #555;
                 border-bottom: 1px solid #000; padding-bottom: 2px; margin: 12px 0 6px; }
table.strip { width: 100%; border-collapse: collapse; }
table.strip td { vertical-align: top; padding: 0 8px 0 0; }
td.temp { font-size: 34pt; font-weight: 700; line-height: 1; }
td.where { font-family: Helvetica, Arial, sans-serif; font-size: 9pt; color: #555; }
td.radar { width: 150px; text-align: right; }
td.radar img { width: 140px; }
table.stories { width: 100%; border-collapse: collapse; }
table.stories td { padding: 3px 0; border-bottom: 1px dotted #ccc; vertical-align: top; }
td.src { width: 30%; font-family: Helvetica, Arial, sans-serif; font-size: 8pt; color: #888; text-align: right; }
td.lead a { color: #1a1a1a; text-decoration: none; font-weight: 600; }
.note { font-size: 8pt; color: #888; }
.footer { margin-top: 12px; padding-top: 4px; border-top: 1px solid #ddd;
          font-size: 7pt; color: #999; text-align: center; }"
  "The newsprint stylesheet, from the original's design system: real tables,
serif body, sans-serif section heads, black on white.")

(defun escape-html (text)
  (if (null text)
      ""
      (with-output-to-string (out)
        (loop for character across (princ-to-string text)
              do (case character
                   (#\& (write-string "&amp;" out))
                   (#\< (write-string "&lt;" out))
                   (#\> (write-string "&gt;" out))
                   (#\" (write-string "&quot;" out))
                   (t (write-char character out)))))))

(defparameter +day-names+ #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(defparameter +month-names+ #("January" "February" "March" "April" "May" "June" "July"
                              "August" "September" "October" "November" "December"))

(defun date-line (universal)
  "UNIVERSAL as the dateline reads it: `Saturday 8 November 2026'."
  (multiple-value-bind (second minute hour day month year weekday) (decode-universal-time universal)
    (declare (ignore second minute hour weekday))
    (format nil "~a ~d ~a ~d" (aref +day-names+ weekday)
            day (aref +month-names+ (1- month)) year)))

(defun day-stamp (universal)
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal)
    (declare (ignore second minute hour))
    (format nil "~4,'0d-~2,'0d-~2,'0d" year month day)))

(defun page-html (dateline hours weather stories errors)
  "The whole page, as one HTML document."
  (with-output-to-string (out)
    (format out "<!DOCTYPE html>~%<html lang=\"en\">~%<head>~%<meta charset=\"utf-8\">~%")
    (format out "<title>The Morning Briefing - ~a</title>~%<style>~%~a~%</style>~%</head>~%<body>~%"
            (escape-html dateline) +stylesheet+)
    (format out "<div class=\"masthead\"><h1>The Morning Briefing</h1>~%<div class=\"dateline\">~a</div></div>~%"
            (escape-html dateline))
    (if (getf weather :error)
        (format out "<div class=\"note\">Weather unanswered: ~a</div>~%" (escape-html (getf weather :error)))
        (format out (concatenate 'string
                                  "<table class=\"strip\"><tr>"
                                  "<td><div class=\"temp\">~a~a</div><div class=\"where\">~a</div></td>"
                                  "<td>~a~@[<br><span class=\"note\">~a</span>~]~@[<br><span class=\"note\">~a</span>~]</td>"
                                  "~@[<td class=\"radar\"><img src=\"~a\" alt=\"radar\"></td>~]"
                                  "</tr></table>~%")
                (escape-html (getf weather :temperature)) (escape-html (getf weather :unit))
                (escape-html (getf weather :short))
                (escape-html (getf weather :detailed))
                (escape-html (getf weather :next))
                (and (getf weather :wind) (format nil "Wind ~a" (getf weather :wind)))
                (escape-html (getf weather :radar))))
    (format out "<div class=\"section-title\">The Last ~d Hours</div>~%" hours)
    (if stories
        (progn
          (format out "<table class=\"stories\">~%")
          (dolist (story stories)
            (format out "<tr><td class=\"lead\"><a href=\"~a\">~a</a></td><td class=\"src\">~a</td></tr>~%"
                    (escape-html (getf story :link)) (escape-html (getf story :title))
                    (escape-html (format nil "r/~a" (getf story :subreddit)))))
          (format out "</table>~%"))
        (format out "<div class=\"note\">No story carried a timestamp inside the window: a thin news day is honest.</div>~%"))
    (when errors
      (format out "<div class=\"note\">Unread: ~{<br>~a~}</div>~%" (mapcar #'escape-html errors)))
    (format out "<div class=\"footer\">Weather from the National Weather Service, headlines from Reddit's own feeds.</div>~%")
    (format out "</body>~%</html>~%")))

(defun expand-home (text)
  "TEXT with a leading ~/ resolved, so a section may name a folder in the home."
  (if (and (stringp text) (uiop:string-prefix-p "~/" text))
      (namestring (merge-pathnames (subseq text 2) (user-homedir-pathname)))
      text))

(defun page-path (stamp)
  "Where today's page goes: the section's output_dir, else this home's briefings/."
  (let* ((dir (configured :output-dir nil))
         (directory (if dir
                        (uiop:ensure-directory-pathname (expand-home dir))
                        (nlk:home "briefings/"))))
    (ensure-directories-exist (merge-pathnames "page.html" directory))
    (merge-pathnames (format nil "briefing-~a.html" stamp) directory)))

(defun write-page (path html)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-string html out))
  path)

;;; --- the whole of it ------------------------------------------------------------

(defun digest (dateline hours weather stories errors path)
  "What the model reads back: the dateline, the weather, the headlines, where the page is."
  (with-output-to-string (out)
    (format out "Morning briefing - ~a~%" dateline)
    (format out "Page: ~a~%" (namestring path))
    (if (getf weather :error)
        (format out "Weather: unanswered - ~a~%" (getf weather :error))
        (format out "Weather: ~@[~a - ~]~a~a, ~a~@[ - wind ~a~]~%"
                (getf weather :describe) (getf weather :temperature) (getf weather :unit)
                (getf weather :short) (getf weather :wind)))
    (when (and (not (getf weather :error)) (getf weather :next))
      (format out "Next: ~a~%" (getf weather :next)))
    (if stories
        (progn
          (format out "~d headline~:p in the last ~d hours:~%" (length stories) hours)
          (dolist (story stories)
            (format out "- ~a (r/~a, ~a) ~a~%"
                    (getf story :title) (getf story :subreddit)
                    (multiple-value-bind (second minute hour day month year)
                        (decode-universal-time (getf story :at))
                      (declare (ignore second minute)) year
                      (format nil "~d ~a ~2,'0d:~2,'0d" day
                              (subseq (aref +month-names+ (1- month)) 0 3) hour minute))
                    (getf story :link))))
        (format out "No headline carried a timestamp inside the last ~d hours.~%" hours))
    (when errors
      (format out "Unread: ~{~a~^; ~}~%" errors))))

(defun build-briefing (&key topics hours max-items)
  "Fetch, render, write. => (values DIGEST PAGE-PATH)."
  (multiple-value-bind (latitude longitude) (coordinates)
    (declare (ignore longitude))
    (let* ((topics (or topics (configured :subreddits *default-subreddits*)))
           (feeds (remove-duplicates
                   (remove-if (lambda (name) (zerop (length name)))
                              (mapcar (lambda (name) (string-downcase (nlk:trimmed name)))
                                      (as-list topics)))
                   :test #'equal))
           (window (or hours (configured :hours 36)))
           (limit (or max-items (configured :max-items 12)))
           (forecast (weather))
           (place (or (configured :place nil) latitude))
           (stories nil)
           (errors nil))
      (multiple-value-setq (stories errors) (collect-stories feeds window limit))
      (let* ((stamp (day-stamp (get-universal-time)))
             (dateline (format nil "~a - ~a" (date-line (get-universal-time)) place))
             (page (write-page (page-path stamp)
                               (page-html dateline window forecast stories errors))))
        (values (digest dateline window forecast stories errors page) page)))))

;;; --- what the model and the operator call ------------------------------------------

(nlk:define-contract briefing-arguments
  (:doc "Assemble today's morning briefing: the weather where the section points and the freshest headlines from the subreddits it names, as one printable newsprint page. Answers the page's path and a plain-text digest of what is on it.")
  ("topics" :array :of :string
   :doc "subreddits to read for this page instead of the configured ones, e.g. [\"science\", \"technology\"]")
  ("hours" :integer
   :doc "how far back a headline may be, in hours; the section's window when absent")
  ("max_items" :integer
   :doc "how many headlines the page carries; the section's cap when absent"))

(defun brief (arguments)
  "The morning_briefing tool: today's page, written, and what is on it."
  (build-briefing :topics (as-list (briefing-arguments-topics arguments))
                  :hours (briefing-arguments-hours arguments)
                  :max-items (briefing-arguments-max-items arguments)))

(defun run-command (args session-id)
  "The /briefing command: today's page; the argument names subreddits to read."
  (declare (ignore session-id))
  (let ((topics (and args (uiop:split-string args :separator '(#\Space #\,)))))
    (build-briefing :topics topics)))

(defun start ()
  "Nothing runs in the background: the page is built when it is asked for."
  (nle:on-stop (lambda ())))

(nle:define-cell morning-briefing
  (:section ("morning-briefing")
             (:guide "latitude and longitude are the one required pair (the weather service's forecast grid, any place; - then (restart-cells \"nodecode-morning-briefing\") if it says REFUSED); subreddits picks the feeds, hours how fresh a headline must be; radar_station adds the color radar image; output_dir is where the HTML page is written")
             ("latitude" :string :required t
              :doc "the latitude of the place the briefing is about, e.g. 44.05")
             ("longitude" :string :required t
              :doc "the longitude of the same place, e.g. -121.31")
             ("place" :string
              :doc "the name the dateline carries, e.g. Bend, Oregon; the latitude when unset")
             ("subreddits" :list
              :doc "the subreddits the headlines come from; news, worldnews, technology and science when unset")
             ("hours" :integer :default 36 :min 1
              :doc "how far back a headline may be, in hours; a thin news day is honest, stale news is not")
             ("max_items" :integer :default 12 :min 1
              :doc "how many headlines the page carries")
             ("radar_station" :string
              :doc "an NWS radar station id, e.g. KRTX, whose color image rides the weather strip")
             ("output_dir" :path
              :doc "where the HTML page is written; this home's briefings/ when unset")
             ("timeout_seconds" :integer :default 20 :min 1
              :doc "one request's ceiling"))
  (:tool "morning_briefing" (nlk:contract 'briefing-arguments) 'brief)
  (:help :briefing "morning_briefing writes today's one-page briefing: weather and the last 36 hours of headlines" +primer+)
  (:command "briefing" 'run-command
            :description "Today's one-page briefing: weather and fresh headlines"
            :argument-hint "SUBREDDIT")
  (:start #'start))
