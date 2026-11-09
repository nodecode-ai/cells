;;;; sources.lisp --- the public feeds this cell reads, and the items it makes of them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every source here is public and keyless: USGS earthquakes, NASA EONET, GDACS
;;;; disaster alerts, US National Weather Service alerts, adsb.lol military
;;;; aircraft, NOAA SWPC space weather, world headlines over RSS plus GDELT's
;;;; article search, the World Bank, CoinGecko (and Twelve Data when a key is
;;;; set), and the recon lookups: Google Public DNS, rdap.org, crt.sh, CIRCL,
;;;; the CISA KEV list, ipwho.is, GitHub and MacVendors. Nothing is written to
;;;; disk and nothing about the operator leaves but the query itself.

(in-package #:nodecode-osint-map)

;;; --- the section's own numbers ---------------------------------------------------

(defparameter +default-user-agent+ "nodecode-osint-map/0.1 (+https://nodecode.ai)"
  "What every source sees us as, unless the section names another.")

(defun user-agent () (or (setting :user-agent) +default-user-agent+))
(defun request-timeout () (or (setting :timeout-seconds) 15))
(defun item-limit () (or (setting :max-items) 20))
(defun cache-seconds () (or (setting :cache-seconds) 60))

;;; --- one small cache, the way the original keeps one ------------------------------

(defvar *cache* nil
  "KEY -> (VALUE . UNIVERSAL-TIME): the last good answer of each source, or NIL
while the cell is not started.")

(defun cached (key first &optional second third)
  "KEY's answer, fetched when it is absent or older than its lifetime."
  ;; Both call shapes read the same: (CACHED KEY THUNK [SECONDS]) and
  ;; (CACHED KEY SECONDS THUNK), because the panels each wrote one of them.
  (let* ((thunk (if (functionp first) first second))
         (seconds (cond ((functionp first) (or second third (cache-seconds)))
                        (t first)))
         (entry (and *cache* (gethash key *cache*))))
    (if (and entry (<= (- (get-universal-time) (cdr entry)) seconds))
        (car entry)
        (let ((value (funcall thunk)))
          (when *cache* (setf (gethash key *cache*) (cons value (get-universal-time))))
          value))))

;;; --- text and time ---------------------------------------------------------------

(defun clean (value &optional (limit 200))
  "VALUE as one trimmed line of plain text, or NIL when it says nothing."
  (let ((text (typecase value
                (string value)
                (integer (princ-to-string value))
                (real (format nil "~,2f" value))
                (t nil))))
    (when text
      (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) text)))
        (unless (or (zerop (length text))
                    (member (string-downcase text) '("null" "none" "n/a" "unknown") :test #'equal))
          (if (> (length text) limit)
              (concatenate 'string (string-right-trim '(#\Space) (subseq text 0 (1- limit))) "…")
              text))))))

(defun strip-tags (value &optional (limit 300))
  "VALUE with its markup dropped: the text a feed means, not the HTML it carries."
  (let ((text (and (stringp value) (clean value 4000))))
    (when text
      (if (or (find #\< text) (find #\& text))
          (let ((out (make-string-output-stream)) (inside nil))
            (loop for char across text
                  do (case char
                       (#\< (setf inside t))
                       (#\> (setf inside nil) (write-char #\Space out))
                       (t (unless inside (write-char char out)))))
            (clean (decode-entities (get-output-stream-string out)) limit))
          (clean text limit)))))

(defun decode-entities (text)
  "The five entities a feed's text carries, decoded."
  (let ((out text))
    (loop for (from to) in '(("&amp;" "&") ("&lt;" "<") ("&gt;" ">") ("&quot;" "\"")
                             ("&#39;" "'") ("&#x27;" "'") ("&apos;" "'") ("&nbsp;" " "))
          do (setf out (replace-all out from to)))
    out))

(defun replace-all (text from to)
  (let ((at (search from text)))
    (if at
        (concatenate 'string (subseq text 0 at) to (replace-all (subseq text (+ at (length from))) from to))
        text)))

(defconstant +unix-epoch+ 2208988800 "1900-01-01 in Unix seconds.")

(defun stamp (universal-time)
  "UNIVERSAL-TIME as `2024-11-08 12:00 UTC', or NIL."
  (when universal-time
    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time universal-time 0)
      (declare (ignore second))
      (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d UTC" year month date hour minute))))

(defun epoch-ms-time (milliseconds)
  "Epoch milliseconds as a universal time, or NIL."
  (when (and (realp milliseconds) (plusp milliseconds))
    (+ (floor milliseconds 1000) +unix-epoch+)))

(defun iso-time (text)
  "`2024-11-08T12:00:00Z' (or a date alone, or an offset) as a universal time, or NIL."
  (when (and (stringp text) (>= (length text) 10))
    (let* ((s (subseq text 0 (min 19 (length text))))
           (year (ignore-errors (parse-integer s :start 0 :end 4)))
           (month (ignore-errors (parse-integer s :start 5 :end 7)))
           (day (ignore-errors (parse-integer s :start 8 :end 10)))
           (hour (and (> (length s) 12) (ignore-errors (parse-integer s :start 11 :end 13))))
           (minute (and (> (length s) 15) (ignore-errors (parse-integer s :start 14 :end 16))))
           (second (and (> (length s) 18) (ignore-errors (parse-integer s :start 17 :end 19)))))
      (when (and year month day)
        (ignore-errors (encode-universal-time (or second 0) (or minute 0) (or hour 0) day month year 0))))))

(defun rfc822-time (text)
  "`Fri, 08 Nov 2024 12:00:00 GMT' as a universal time, or NIL."
  (when (and (stringp text) (> (length text) 20))
    (let* ((comma (position #\, text))
           (rest (string-trim '(#\Space) (if comma (subseq text (1+ comma)) text)))
           (date (ignore-errors (parse-integer rest :start 0 :end 2 :junk-allowed t)))
           (month (clean (subseq rest 3 (min 6 (length rest)))))
           (year (ignore-errors (parse-integer rest :start 7 :end 11 :junk-allowed t)))
           (hour (ignore-errors (parse-integer rest :start 12 :end 14)))
           (minute (ignore-errors (parse-integer rest :start 15 :end 17)))
           (second (ignore-errors (parse-integer rest :start 18 :end 20)))
           (m (and month (position (string-upcase month)
                                   '("JAN" "FEB" "MAR" "APR" "MAY" "JUN" "JUL" "AUG" "SEP" "OCT" "NOV" "DEC")
                                   :test #'string=))))
      (when (and date m year hour minute)
        (ignore-errors (encode-universal-time (or second 0) minute hour date (1+ m) year 0))))))

(defun fmt (number &optional (digits 1))
  "NUMBER with DIGITS decimals, trailing zeros dropped."
  (when (realp number)
    (let ((shown (format nil (format nil "~~,~df" digits) (coerce number 'double-float))))
      (if (find #\. shown)
          (string-right-trim "." (string-right-trim "0" shown))
          shown))))

;;; --- the wire --------------------------------------------------------------------

(defun url-host (url)
  "URL's host, for an error a person can read."
  (let* ((from (if (search "://" url) (+ 3 (search "://" url)) 0))
         (end (or (position-if (lambda (c) (member c '(#\/ #\? #\#))) url :start from) (length url))))
    (subseq url from end)))

(defun fetch-text (url &key (accept "application/json"))
  "URL's body as text; a failed status is a refusal naming the host."
  (multiple-value-bind (body status)
      (nlk:http :get url
                :headers (list (cons "user-agent" (user-agent))
                               (cons "accept" accept))
                :timeout (request-timeout))
    (unless (and (integerp status) (<= 200 status 299))
      (fail "~a answered HTTP ~a" (url-host url) status))
    (etypecase body
      (null "")
      (string body)
      (vector (map 'string #'code-char body)))))

(defun fetch-json (url &key (accept "application/json"))
  "URL's body, decoded."
  (let ((text (fetch-text url :accept accept)))
    (if (zerop (length (string-trim '(#\Space #\Newline) text)))
        (nlk:json-object)
        (handler-case (nlk:decode-json text)
          (error (condition) (fail "~a answered something that is not JSON (~a)" (url-host url) condition))))))

(defun arr (object &rest path)
  "The array at PATH, or an empty one, so a LOOP ACROSS needs no guard."
  (or (apply #'nlk:json-value object :array path) #()))

(defun obj (object &rest path)
  (apply #'nlk:json-value object :object path))

(defun str (object &rest path)
  (apply #'nlk:json-value object :text path))

(defun num (object &rest path)
  (apply #'nlk:json-value object :number path))

(defun numeric (value)
  "VALUE as a real number when it is one, or spells one (SWPC sends numbers as strings)."
  (typecase value
    (real value)
    (string (ignore-errors
              (multiple-value-bind (number position)
                  (let ((*read-eval* nil)) (read-from-string value nil nil))
                (and (realp number) (= position (length (string-trim '(#\Space) value))) number))))
    (t nil)))

(defun at (sequence index)
  "SEQUENCE's INDEXth member when it has one, else NIL: a JSON array's step, which
JSON-VALUE's key walk never takes."
  (and (vectorp sequence) (integerp index) (< -1 index (length sequence))
       (let ((value (aref sequence index)))
         (if (eq value :null) nil value))))

(defun n-at (sequence index)
  "AT, when it is a number."
  (let ((value (at sequence index)))
    (and (realp value) value)))

(defun first-of (sequence)
  "SEQUENCE's first member, or NIL."
  (at sequence 0))

;;; --- one item, whatever the layer ------------------------------------------------

(defstruct (entry (:constructor make-entry (&key layer title subtitle (severity 0.3) time
                                                 country lat lon url props)))
  "One thing on the map: where and when, how loud, and the facts to show."
  layer title subtitle severity time country lat lon url props)

(defun entry-line (entry)
  "ENTRY as one line the model can quote."
  (with-output-to-string (out)
    (format out "- ~a" (entry-title entry))
    (when (entry-subtitle entry) (format out " (~a)" (entry-subtitle entry)))
    (when (entry-country entry) (format out " [~a]" (entry-country entry)))
    (when (entry-time entry) (format out " at ~a" (stamp (entry-time entry))))
    (when (and (entry-lat entry) (entry-lon entry))
      (format out " ~,2f,~,2f" (entry-lat entry) (entry-lon entry)))
    (when (entry-url entry) (format out " ~a" (entry-url entry)))))

;;; --- layers ----------------------------------------------------------------------

(defparameter +usgs+ "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_day.geojson")
(defparameter +eonet+ "https://eonet.gsfc.nasa.gov/api/v3/events")
(defparameter +gdacs+ "https://www.gdacs.org/gdacsapi/api/events/geteventlist/SEARCH")
(defparameter +nws+ "https://api.weather.gov/alerts/active")
(defparameter +adsb-mil+ "https://api.adsb.lol/v2/mil")

(defun place-country (place)
  "The country a USGS place string ends in: `10 km NE of X, Japan' is Japan."
  (when (and place (position #\, place :from-end t))
    (clean (subseq place (1+ (position #\, place :from-end t))) 60)))

(defun parse-quakes (raw)
  (loop for feature across (arr raw "features")
        for properties = (obj feature "properties")
        for coordinates = (arr (obj feature "geometry") "coordinates")
        for lon = (and (> (length coordinates) 1) (at coordinates 0))
        for lat = (and (> (length coordinates) 1) (at coordinates 1))
        for place = (clean (str properties "place") 120)
        for magnitude = (num properties "mag")
        when (and lat lon (not (and (zerop lat) (zerop lon))))
          collect (let ((depth (and (> (length coordinates) 2) (at coordinates 2))))
                    (make-entry
                     :layer "quakes"
                     :title (or (clean (str properties "title") 160)
                                (format nil "M ~a - ~a" (or (fmt magnitude) "?") (or place "unknown place")))
                     :subtitle (and depth (format nil "Depth ~a km" (fmt depth)))
                     :severity (max 0.1 (min 1.0 (if magnitude (+ 0.1 (/ (* (- magnitude 2.5) 0.9) 4.5)) 0.1)))
                     :time (epoch-ms-time (num properties "time"))
                     :country (place-country place)
                     :lat lat :lon lon
                     :url (str properties "url")
                     :props (remove nil (list (and magnitude (cons "Magnitude" (fmt magnitude)))
                                              (and depth (cons "Depth" (format nil "~a km" (fmt depth))))
                                              (and place (cons "Place" place))
                                              (and (str properties "alert") (cons "PAGER alert" (str properties "alert")))))))))

(defparameter +eonet-base+ '(("volcanoes" . 0.6) ("severeStorms" . 0.5) ("wildfires" . 0.4)
                             ("floods" . 0.5) ("landslides" . 0.5) ("earthquakes" . 0.5)
                             ("drought" . 0.4) ("dustHaze" . 0.25) ("seaLakeIce" . 0.15)
                             ("snow" . 0.2) ("tempExtremes" . 0.35) ("manmade" . 0.35)))

(defun parse-eonet (raw)
  (loop for event across (arr raw "events")
        for geometries = (arr event "geometry")
        for latest = (and (plusp (length geometries)) (aref geometries (1- (length geometries))))
        for coordinates = (and latest (arr latest "coordinates"))
        for lon = (and (plusp (length coordinates)) (at coordinates 0))
        for lat = (and (> (length coordinates) 1) (at coordinates 1))
        for categories = (arr event "categories")
        for category = (and (plusp (length categories)) (first-of categories))
        for kind = (str category "id")
        when (and lat lon (not (and (zerop lat) (zerop lon))))
          collect (make-entry
                   :layer "natural"
                   :title (or (clean (str event "title") 160) (str category "title") "Natural event")
                   :subtitle (clean (str category "title") 40)
                   :severity (or (cdr (assoc kind +eonet-base+ :test #'equal)) 0.3)
                   :time (iso-time (str latest "date"))
                   :lat lat :lon lon
                   :url (or (str (first-of (arr event "sources")) "url") (str event "link"))
                   :props (remove nil (list (and kind (cons "Category" kind))
                                            (and (num latest "magnitudeValue")
                                                 (cons "Magnitude"
                                                       (format nil "~a ~a"
                                                               (fmt (num latest "magnitudeValue"))
                                                               (or (str latest "magnitudeUnit") "")))))))))

(defparameter +gdacs-kind+ '(("EQ" . "Earthquake") ("TC" . "Tropical cyclone") ("FL" . "Flood")
                             ("VO" . "Volcano") ("DR" . "Drought") ("WF" . "Wildfire") ("TS" . "Tsunami")))

(defun gdacs-severity (level)
  (cond ((equal level "red") 1.0) ((equal level "orange") 0.65) (t 0.3)))

(defun parse-gdacs (raw)
  (loop for feature across (arr raw "features")
        for properties = (obj feature "properties")
        for kind = (clean (str properties "eventtype") 4)
        for coordinates = (arr (obj feature "geometry") "coordinates")
        for lon = (and (> (length coordinates) 1) (at coordinates 0))
        for lat = (and (> (length coordinates) 1) (at coordinates 1))
        for level = (string-downcase (or (str properties "alertlevel") ""))
        when (and lat lon (assoc kind +gdacs-kind+ :test #'equal))
          collect (make-entry
                   :layer "disasters"
                   :title (or (clean (str properties "name") 160)
                              (format nil "~a ~a" (cdr (assoc kind +gdacs-kind+ :test #'equal))
                                      (or (str properties "eventid") "")))
                   :subtitle (clean (str (obj properties "severitydata") "severitytext") 120)
                   :severity (gdacs-severity level)
                   :time (or (iso-time (str properties "datemodified")) (iso-time (str properties "fromdate")))
                   :country (clean (str properties "country") 60)
                   :lat lat :lon lon
                   :url (str (obj properties "url") "report")
                   :props (remove nil (list (cons "Type" (cdr (assoc kind +gdacs-kind+ :test #'equal)))
                                            (and (plusp (length level)) (cons "Alert level" level))
                                            (and (str properties "country") (cons "Country" (str properties "country"))))))))

(defparameter +nws-severity+ '(("Extreme" . 1.0) ("Severe" . 0.7) ("Moderate" . 0.45) ("Minor" . 0.25)))

(defun parse-nws (raw)
  (loop for feature across (arr raw "features")
        for properties = (obj feature "properties")
        for ring = (arr (obj feature "geometry") "coordinates")
        for first-ring = (and (plusp (length ring)) (aref ring 0))
        for point = (and first-ring (plusp (length first-ring)) (aref first-ring 0))
        for lon = (and point (at point 0))
        for lat = (and point (at point 1))
        for severity = (str properties "severity")
        for event = (or (str properties "event") "Weather alert")
        when (and lat lon)
          collect (make-entry
                   :layer "weather"
                   :title (format nil "~a · ~a" event (or (clean (str properties "areaDesc") 80) "United States"))
                   :subtitle (clean (str properties "senderName") 120)
                   :severity (or (cdr (assoc severity +nws-severity+ :test #'equal)) 0.4)
                   :time (or (iso-time (str properties "onset"))
                             (iso-time (str properties "effective"))
                             (iso-time (str properties "sent")))
                   :country "United States"
                   :lat lat :lon lon
                   :url (str properties "@id")
                   :props (remove nil (list (and severity (cons "Severity" severity))
                                            (and (str properties "certainty") (cons "Certainty" (str properties "certainty")))
                                            (and (str properties "areaDesc") (cons "Area" (str properties "areaDesc"))))))))

(defun parse-aircraft (raw)
  (let ((now (num raw "now")))
    (loop for aircraft across (arr raw "ac")
          for hex = (clean (str aircraft "hex") 8)
          for lat = (num aircraft "lat")
          for lon = (num aircraft "lon")
          for altitude = (num aircraft "alt_baro")
          for callsign = (clean (str aircraft "flight") 12)
          for registration = (clean (str aircraft "r") 16)
          when (and hex lat lon altitude (not (and (zerop lat) (zerop lon))))
            collect (let ((emergency (cdr (assoc (str aircraft "squawk") '(("7500" . "unlawful interference")
                                                                          ("7600" . "radio failure")
                                                                          ("7700" . "general emergency"))
                                                :test #'equal))))
                      (make-entry
                       :layer "aircraft"
                       :title (or callsign registration (string-upcase hex))
                       :subtitle (format nil "~@[~a · ~]~a ft" (str aircraft "t") (fmt altitude 0))
                       :severity (if emergency 1.0 0.7)
                       :time (and now (epoch-ms-time (- now (* 1000 (or (num aircraft "seen_pos") 0)))))
                       :lat lat :lon lon
                       :props (remove nil (list (and (str aircraft "t") (cons "Type" (str aircraft "t")))
                                                         (and registration (cons "Registration" registration))
                                                         (cons "Altitude" (format nil "~a ft" (fmt altitude 0)))
                                                         (cons "ICAO hex" (string-upcase hex)))))))))

(defparameter +layers+
  (list (list "quakes" "Earthquakes (USGS)" #'parse-quakes)
        (list "natural" "Natural events (NASA EONET)" #'parse-eonet)
        (list "disasters" "Disaster alerts (GDACS)" #'parse-gdacs)
        (list "weather" "Severe weather, US (NWS)" #'parse-nws)
        (list "aircraft" "Aircraft (adsb.lol)" #'parse-aircraft))
  "Every layer: its id, its label and the parser of its answer.")

(defparameter +layer-urls+
  '(("quakes" . "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_day.geojson")
    ("natural" . "https://eonet.gsfc.nasa.gov/api/v3/events")
    ("disasters" . "https://www.gdacs.org/gdacsapi/api/events/geteventlist/SEARCH")
    ("weather" . "https://api.weather.gov/alerts/active")
    ("aircraft" . "https://api.adsb.lol/v2/mil")))

(defun layer-ids () (mapcar #'first +layers+))

(defun layer-fetch (id)
  "LAYER's items, or a refusal naming the layer and what its source said."
  (let ((url (cdr (assoc id +layer-urls+ :test #'equal)))
        (parse (third (assoc id +layers+ :test #'equal))))
    (unless url (fail "unknown layer ~s; use one of ~{~a~^, ~}" id (layer-ids)))
    (let ((raw (cached (format nil "layer:~a" id) (lambda () (fetch-json url)))))
      (sort (funcall parse raw) #'> :key #'entry-severity))))

(defun layers-fetch (&optional (ids (layer-ids)))
  "Every layer's items, as an alist (ID . ITEMS); a source that fails is a refusal
under its own id, so one broken feed never empties the brief."
  (loop for id in ids
        collect (cons id (handler-case (layer-fetch id)
                           (osint-map-error (condition) condition)))))

(defun layer-count (result)
  (if (listp result) (length result) 0))
