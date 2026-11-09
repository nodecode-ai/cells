;;;; cotrip.lisp --- one COtrip layer fetched, one corridor filtered, one text.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's cotrip_corridor.py, as Lisp: the same base host, the
;;;; same six layer names, the same corridor box, the same reading of CDOT's
;;;; fields, and the same rule for what is happening now -- the schedule,
;;;; never the `active' flag, which CDOT sets true for future night work.
;;;; Everything here is pure but FETCH-FEATURES, the one seam a test stubs:
;;;; the test run has no network.
;;;;
;;;; The plugin's script was the whole program; here it is the data half, and
;;;; cell.lisp is the tool, the command and the section.

(in-package #:nodecode-colorado-traffic)

;;; --- what COtrip answers ------------------------------------------------------

(defparameter +base+ "https://api-511x-co.carsprogram.org"
  "CDOT's public 511 API: no key, no account, no telemetry.")

(defparameter +layers+
  '(("incidents" . "roadReports")
    ("closures" . "roadClosures")
    ("work" . "roadWork")
    ("waze" . "wazeReports")
    ("winter" . "winterDriving")
    ("chains" . "chainLaws"))
  "The layer names a user types, and the classification CDOT knows it by.")

(defparameter +default-layers+ '("incidents" "closures" "work" "waze")
  "What is asked for when the section and the caller name none.")

(defparameter +corridors+
  '(("i70-golden-denver"
     :route "I-70" :min 259.0 :max 279.0
     :label "I-70 Golden to east Denver (MP 259-279, C-470 through Quebec)")
    ("i70-golden-i25"
     :route "I-70" :min 259.0 :max 274.5
     :label "I-70 Golden to I-25 (MP 259-274.5)"))
  "The corridors the plugin ships, unchanged: a route and a mile-point box.")

;; routePositiveBearing is a direction of the route, not of the compass: on
;; I-70 positive is east, on I-25 it is north. DIRECTION maps both.
(defparameter +bearings+
  '(("E" "eastbound" "westbound")
    ("W" "westbound" "eastbound")
    ("N" "northbound" "southbound")
    ("S" "southbound" "northbound"))
  "Bearing letter => (positive direction, negative direction).")

(defparameter +layer-titles+
  '(("incidents" . "Incidents") ("closures" . "Closures") ("work" . "Scheduled work")
    ("waze" . "Waze via COtrip") ("winter" . "Winter driving") ("chains" . "Chain laws"))
  "How a layer is headed in the answer.")

(defparameter +entities+
  '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&#39;" . "'") ("&apos;" . "'")
    ("&nbsp;" . " "))
  "The entities COtrip's prose carries; &amp; is replaced last, as always.")

(defun corridor-spec (name)
  "The corridor NAMEs, or a refusal naming the ones there are."
  (or (cdr (assoc name +corridors+ :test #'string=))
      (fail "~s is no corridor: ~{~a~^, ~}" name (mapcar #'car +corridors+))))

(defun layer-classification (name)
  "The COtrip classification NAME asks for, or a refusal naming the layers."
  (or (cdr (assoc name +layers+ :test #'string=))
      (fail "~s is no COtrip layer: ~{~a~^, ~}" name (mapcar #'car +layers+))))

;;; --- the one fetch -------------------------------------------------------------

(defun fetch-features (classification)
  "The FEATURES vector one COtrip layer answers, or a refusal."
  ;; No key and no account, one GET, one host: the plugin's own disclosure.
  (let* ((timeout (setting :timeout-seconds))
         (url (format nil "~a/events/map-features?eventClassifications=~a" +base+ classification)))
    (multiple-value-bind (body status)
        (nlk:http :get url
                  :headers '(("accept" . "application/json")
                             ("content-language" . "en"))
                  :timeout timeout
                  :connect-timeout (min 10 timeout))
      (unless (and (integerp status) (< status 300))
        (fail "CDOT answered HTTP ~a for ~a" status url))
      (let ((data (handler-case (nlk:decode-json body) (error () nil))))
        (or (nlk:json-value data :array "features")
            ;; A bare /events answers {"healthy":true}, which is not an empty road.
            (fail "COtrip answered no features for ~a: ~a" classification
                  (if (stringp body) (subseq body 0 (min 120 (length body))) body)))))))

;;; --- reading one feature -------------------------------------------------------

(defun feature-props (feature)
  (nlk:json-value feature :object "properties"))

(defun feature-report (feature)
  (nlk:json-value feature :object "properties" "eventReport"))

(defun feature-location (feature)
  (nlk:json-value feature :object "properties" "eventReport" "location"))

(defun milepoints (feature)
  "The mile points FEATURE names, primary first."
  (let ((location (feature-location feature))
        (points '()))
    (dolist (key '("primaryPoint" "secondaryPoint"))
      (let ((reference (nlk:json-value location :number key "linearReference")))
        (when reference (push reference points))))
    (nreverse points)))

(defun direction (feature)
  "The direction FEATURE runs, from linkDirection and routePositiveBearing."
  (let* ((location (feature-location feature))
         (link (nlk:json-value location :string "linkDirection"))
         (bearing (nlk:json-value location :string "routePositiveBearing"))
         (pair (and bearing (cdr (assoc bearing +bearings+ :test #'string=)))))
    (cond ((equal link "BOTH_DIRECTIONS") "both directions")
          ((and pair (equal link "POSITIVE_DIRECTION")) (first pair))
          ((and pair (equal link "NEGATIVE_DIRECTION")) (second pair))
          (t "an unstated direction"))))

(defun schedule-windows (feature)
  "FEATURE's scheduled windows as (START-EPOCH-MILLIS . END-EPOCH-MILLIS)."
  (let ((windows (nlk:json-value (feature-report feature) :array "scheduleOccurrences"))
        (out '()))
    (when windows
      (dotimes (index (length windows))
        (let ((window (aref windows index)))
          (push (cons (nlk:json-value window :number "startTime" "time")
                      (nlk:json-value window :number "endTime" "time"))
                out))))
    (nreverse out)))

(defun timing (feature now-ms)
  "Whether FEATURE is happening \"now\", is \"upcoming\", or has \"ended\"."
  ;; CDOT's `active' flag is true for future night closures: never read it.
  (let ((windows (schedule-windows feature)))
    (if (null windows)
        (let ((begin (nlk:json-value (feature-report feature) :number "beginTime" "time")))
          (if (and begin (> begin now-ms)) "upcoming" "now"))
        (let ((states (mapcar (lambda (window)
                                (cond ((and (car window) (> (car window) now-ms)) "upcoming")
                                      ((and (cdr window) (< (cdr window) now-ms)) "ended")
                                      (t "now")))
                              windows)))
          (cond ((member "now" states :test #'string=) "now")
                ((member "upcoming" states :test #'string=) "upcoming")
                (t "ended"))))))

(defun overlaps-p (points min-mp max-mp)
  "Whether the stretch POINTS covers any of MIN-MP..MAX-MP."
  (and points
       (<= (reduce #'min points) max-mp)
       (>= (reduce #'max points) min-mp)))

(defun id-starts-with-p (prefix string)
  (and (stringp string) (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

;;; --- US Mountain civil time ----------------------------------------------------

(defun weekday-of (year month day)
  "DAY's weekday, 0 Sunday."
  (mod (1+ (nth-value 6 (decode-universal-time (encode-universal-time 0 0 12 day month year 0) 0))) 7))

(defun first-sunday (year month)
  "The day of MONTH's first Sunday."
  (+ 1 (mod (- 7 (weekday-of year month 1)) 7)))

(defun mountain-offset (unix)
  "The seconds added to UTC to read UNIX as US Mountain civil time."
  ;; DST begins 02:00 on March's second Sunday and ends 02:00 on November's
  ;; first: the rule is fixed, so no timezone database is carried.
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time unix 0)
    (declare (ignore second minute hour day month))
    (let* ((starts (+ (encode-universal-time 0 0 2 (+ (first-sunday year 3) 7) 3 year 0)
                      (* 7 3600)))
           (ends (+ (encode-universal-time 0 0 2 (first-sunday year 11) 11 year 0)
                    (* 6 3600))))
      (if (and (>= unix starts) (< unix ends)) -21600 -25200))))

(defun stamp (millis)
  "MILLIS since the epoch as US Mountain civil time, or NIL."
  (when (and (numberp millis) (plusp millis))
    (let* ((unix (floor millis 1000))
           (offset (mountain-offset unix)))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (+ unix offset) 0)
        (declare (ignore second))
        (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d ~a"
                year month day hour minute (if (= offset -21600) "MDT" "MST"))))))

(defun now-ms ()
  (* 1000 (get-universal-time)))

;;; --- one feature as a value ----------------------------------------------------

(defun plain (text)
  "TEXT with its HTML dropped and its entities read, or NIL."
  (when (and (stringp text) (plusp (length text)))
    (let ((clean (cl-ppcre:regex-replace-all "(?i)<br[^>]*>|</p>|</div>" text (string #\Newline))))
      (setf clean (cl-ppcre:regex-replace-all "(?i)<[^>]+>" clean " "))
      (dolist (pair +entities+)
        (setf clean (cl-ppcre:regex-replace-all (cl-ppcre:quote-meta-chars (car pair))
                                                 clean (cdr pair))))
      (setf clean (cl-ppcre:regex-replace-all "&amp;" clean "&"))
      (setf clean (cl-ppcre:regex-replace-all "[ 	]+" clean " "))
      (setf clean (cl-ppcre:regex-replace-all (format nil "~c{2,}" #\Newline)
                                               clean (string #\Newline)))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) clean)))
        (and (plusp (length trimmed)) trimmed)))))

(defun clip (text limit)
  (when text
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit) "...")
        text)))

(defun summary (feature now-ms)
  "One COtrip feature as a plist the answer is written from."
  (let* ((props (feature-props feature))
         (report (feature-report feature))
         (description (nlk:json-value report :object "eventDescription"))
         (id (nlk:json-value props :string "id"))
         (headline (plain (or (nlk:json-value props :string "title")
                               (nlk:json-value description :string "descriptionHeader"))))
         (detail (plain (or (nlk:json-value description :string "descriptionFull")
                            (nlk:json-value props :string "description")))))
    (list :id id
          :when (timing feature now-ms)
          :direction (direction feature)
          :milepoints (milepoints feature)
          :headline (or headline "")
          :detail (if (and detail (not (equal detail headline))) (clip detail 240) nil)
          :windows (mapcar (lambda (window)
                             (format nil "~a to ~a" (stamp (car window)) (stamp (cdr window))))
                           (schedule-windows feature))
          :updated (stamp (nlk:json-value props :number "updated"))
          :source (if (id-starts-with-p "Waze" id) "Waze via COtrip" "CDOT")
          :map (and id (format nil "https://www.cotrip.org/events/~a" id)))))

;;; --- one corridor, one query ---------------------------------------------------

(defun when-rank (when)
  (cond ((equal when "now") 0) ((equal when "upcoming") 1) (t 2)))

(defun collect (spec layers &key include-ended (now (now-ms)))
  "SPEC's corridor, LAYERS, as (LAYER-NAME . ITEMS) in the order asked for."
  ;; One GET a layer, every feature of any other route dropped, then the mile
  ;; point box: the plugin's own filter, kept.
  (loop for name in layers
        collect (cons name
                      (let ((items
                              (loop for feature across (fetch-features (layer-classification name))
                                    when (and (equal (nlk:json-value (feature-props feature) :string "route")
                                                      (getf spec :route))
                                              (overlaps-p (milepoints feature)
                                                          (getf spec :min) (getf spec :max)))
                                      collect (summary feature now))))
                        (setf items (stable-sort items (lambda (a b)
                                                         (< (when-rank (getf a :when))
                                                            (when-rank (getf b :when))))))
                        (unless include-ended
                          (setf items (remove-if (lambda (item) (equal (getf item :when) "ended")) items)))
                        (when (> (length items) (setting :max-items))
                          (setf items (subseq items 0 (setting :max-items))))
                        items))))

(defun render-item (item)
  (let* ((miles (sort (copy-list (getf item :milepoints)) #'<))
         (where (if miles (format nil "MP ~{~,1f~^ to ~}" miles) "mile point unstated")))
    (format nil "  [~a] ~a, ~a: ~a~@[~%~@[    ~a~]~]~@[  (~{~a~^; ~})~]~@[  updated ~a~]~@[~%    ~a~]~%"
            (getf item :when) (getf item :direction) where
            (getf item :headline)
            (and (plusp (length (getf item :detail))) (getf item :detail))
            (getf item :windows)
            (getf item :updated)
            (getf item :map))))

(defun render (spec grouped now)
  "The whole answer as text: a heading, a section a layer, one caveat."
  (with-output-to-string (out)
    (format out "~a~%COtrip via CDOT, queried ~a.~2%"
            (getf spec :label) (stamp now))
    (dolist (entry grouped)
      (let ((title (or (cdr (assoc (car entry) +layer-titles+ :test #'string=)) (car entry)))
            (items (cdr entry)))
        (if (null items)
            (format out "~a: none~%" title)
            (progn (format out "~a (~d)~%" title (length items))
                   (dolist (item items) (princ (render-item item) out))))))
    (format out "~%CDOT's active flag is true for future scheduled work, so the tag comes from the schedule. Waze reports are CDOT's republication of unverified crowd reports, not CDOT-confirmed incidents.~%")))
