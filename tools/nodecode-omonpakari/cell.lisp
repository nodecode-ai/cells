;;;; cell.lisp --- a considerate health journal: seven tools, one quiet picture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of tamamo510's omonpakari Hermes Agent plugin (see NOTICE).
;;;; The journal, the cycle marks, the life notes and the pressure samples
;;;; live in one JSON file under the organism's home; nothing else is written.
;;;; OpenWeather is the one host spoken to, and only for barometric pressure.
;;;;
;;;; The original's pre_llm_call hook becomes one standing harness section:
;;;; before each reply the model reads a short picture of today, so its
;;;; suggestions are weighed against the person's body and habits first.

(in-package #:nodecode-omonpakari)

;;; --- the manual -----------------------------------------------------------------

(defparameter *manual*
  "omonpakari keeps a health journal for the person you talk with, and puts a short
quiet picture of today into your standing context. Use the tools, never nag.
  omonpakari_log      {action, kind, label, severity, dose, note, at, since, days,
                       limit, id, max_count, window_h, confirm}
      action add records one thing in the person's own words: kind symptom,
      medication, supplement, food or note; severity 1-5 and dose only when they
      said them; at as ISO 8601 when it happened earlier. list reads back, delete
      one by id, set_limit a medication count, purge (confirm true) everything.
  omonpakari_pressure {lat, lon, remember}
      Barometric pressure now, the change over the past window_h and over the
      forecast ahead, the lowest point ahead. lat/lon only when the person gave a
      place; remember true keeps it. A missing key or place answers a reason, not
      an error.
  omonpakari_cycle    {action, mark, at, id, note}
      mark a period start or end, status the observed cycle lengths and the next
      window as a range of dates - never one predicted day.
  omonpakari_interactions {action, days, window_h}
      check scans the journal for entries taken close to an iron entry that are
      known to inhibit or promote its absorption, with the fact and its source;
      table returns the reference table itself.
  omonpakari_life     {action, topic, note, relates}
      set a standing fact about the person's life in their own words - the
      morning drink, what warms them up - list, delete.
  omonpakari_waves    {}
      The composed states: pressure, cycle, symptoms, sleep, nutrition, each
      settled, watch or heavy, with the counts behind it.
  omonpakari_correlate {kind, label, days, window_h, drop_hpa, observed_only}
      Entries against the pressure hours they fell in, with the raw counts.
How to answer: weigh the picture before suggesting, prefer the person's own
alternatives, and do not propose an item flagged under interactions today near its
conflicting entry. Do not lecture, diagnose, or recite the numbers; the count is
a count, not a conclusion. Do not turn anything into a reminder."
  "What (help :omonpakari) answers while the cell runs.")

;;; --- answers --------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's JSON answer; a refusal becomes the call's failure text, never an
unhandled error."
  `(handler-case (progn ,@body)
     (error (condition)
       (nle:failure (format nil "ERROR: omonpakari: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun job (&rest pairs)
  "A JSON object from PAIRS, a NIL value left out."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on pairs by #'cddr
          do (unless (null value) (setf (gethash key object) value)))
    object))

(defun json-text (object)
  (nlk:encode-json-object object))

(defun tidy (number)
  "NUMBER as a plain JSON number: an integer when it is whole, so a pressure
reads 1006 and not 1.006e3."
  (cond ((null number) nil)
        ((integerp number) number)
        ((and (realp number) (= number (round number))) (round number))
        (t (coerce (/ (round (* number 10)) 10.0) 'single-float))))

(defun strings (list)
  (coerce (or list '()) 'vector))

;;; --- time -----------------------------------------------------------------------

(defparameter +unix-epoch+ 2208988800 "Universal time at 1970-01-01T00:00Z.")

(defun now-utc () (- (get-universal-time) +unix-epoch+))

(defun iso-utc (seconds)
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (round seconds) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun digits-value (text start end)
  (let ((piece (subseq text start (min end (length text)))))
    (and (plusp (length piece))
         (every #'digit-char-p piece)
         (parse-integer piece))))

(defun offset-seconds (text)
  "TEXT's +HH:MM or -HH:MM tail as seconds east of UTC, or 0."
  (let ((at (position-if (lambda (c) (member c '(#\+ #\-))) text :start (min 10 (length text)))))
    (if (null at)
        0
        (let ((sign (if (char= (char text at) #\-) -1 1))
              (hours (digits-value text (1+ at) (+ at 3)))
              (minutes (and (> (length text) (+ at 4)) (digits-value text (+ at 4) (+ at 6)))))
          (if hours
              (* sign (+ (* 3600 hours) (* 60 (or minutes 0))))
              0)))))

(defun parse-at (text)
  "TEXT as UTC epoch seconds: an integer string, or ISO 8601 (a clock time
without an offset is read as UTC). NIL when it cannot be read."
  (when (integerp text) (return-from parse-at text))
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (when (and text (plusp (length text)))
      (if (every #'digit-char-p text)
          (parse-integer text)
          (handler-case
              (let* ((text (subseq text 0 (or (position-if (lambda (c) (member c '(#\Z #\z))) text)
                                               (length text))))
                     (year (digits-value text 0 4))
                     (month (digits-value text 5 7))
                     (day (digits-value text 8 10))
                     (hour (or (digits-value text 11 13) 0))
                     (minute (or (digits-value text 14 16) 0))
                     (second (or (digits-value text 17 19) 0)))
                (when (and year month day)
                  (- (encode-universal-time second minute hour day month year 0)
                     +unix-epoch+
                     (offset-seconds text))))
            (error () nil))))))

;;; --- the store: one JSON file ---------------------------------------------------

(defparameter *store-file* "journal.json"
  "The journal's file name, under the section's data_dir.")

(defun store-path ()
  (merge-pathnames *store-file* (setting :data-dir)))

(defun blank-store ()
  (job "entries" (strings '()) "cycle" (strings '()) "life" (strings '())
       "samples" (strings '()) "next_id" 1))

(defun load-store ()
  (let ((path (store-path)))
    (if (probe-file path)
        (handler-case
            (let ((value (nlk:decode-json (uiop:read-file-string path))))
              (if (hash-table-p value) value (blank-store)))
          (error () (blank-store)))
        (blank-store))))

(defun save-store (store)
  (let* ((directory (setting :data-dir))
         (path (merge-pathnames *store-file* directory))
         (temp (merge-pathnames (format nil "~a.tmp" *store-file*) directory)))
    (ensure-directories-exist path)
    (with-open-file (out temp :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string (json-text store) out))
    (nlk:replace-file temp path)
    store))

(defun rows (store key)
  "The array under KEY, as a list of hash tables."
  (let ((value (gethash key store)))
    (cond ((vectorp value) (coerce value 'list))
          ((listp value) value)
          (t '()))))

(defun put-rows (store key list)
  (setf (gethash key store) (coerce list 'vector))
  store)

(defun add-row (store key row)
  (put-rows store key (append (rows store key) (list row)))
  row)

(defun row-id (row) (let ((id (gethash "id" row))) (and (integerp id) id)))

(defun take-id (store)
  (let ((id (1+ (or (let ((next (gethash "next_id" store)))
                       (and (integerp next) next))
                   1))))
    (setf (gethash "next_id" store) id)
    id))

(defun number-of (value)
  (cond ((numberp value) value)
        ((stringp value) (ignore-errors
                            (let ((*read-eval* nil)) (read-from-string value))))
        (t nil)))

(defun as-real (value)
  (let ((number (number-of value))) (and (numberp number) number)))

;;; --- pressure -------------------------------------------------------------------

(defparameter +current-url+ "https://api.openweathermap.org/data/2.5/weather")
(defparameter +forecast-url+ "https://api.openweathermap.org/data/2.5/forecast")
(defparameter +sample-retention+ (* 400 86400))

(defun api-key ()
  (let ((name (setting :openweather-api-key-env)))
    (and name (nle::credential-env name))))

(defun saved-place ()
  (let* ((store (load-store))
         (place (gethash "place" store))
         (lat (and (hash-table-p place) (as-real (gethash "lat" place))))
         (lon (and (hash-table-p place) (as-real (gethash "lon" place)))))
    (and lat lon (list (coerce lat 'single-float) (coerce lon 'single-float)))))

(defun settings-place ()
  (let ((lat (setting :latitude)) (lon (setting :longitude)))
    (and lat lon (list lat lon))))

(defun place-key (lat lon)
  (format nil "~,2f,~,2f" lat lon))

(defun http-object (url)
  (multiple-value-bind (body status) (nlk:http :get url :timeout (setting :timeout-seconds))
    (unless (and (integerp status) (<= 200 status 299))
      (error "OpenWeather answered ~a" (or status "nothing")))
    (let ((payload (nlk:decode-json body)))
      (unless (hash-table-p payload) (error "OpenWeather answered data that could not be read"))
      payload)))

(defun fetch-points (lat lon key)
  "New samples from OpenWeather: the measured point and the forecast points."
  (let* ((now (now-utc))
         (place (place-key lat lon))
         (suffix (format nil "?lat=~,2f&lon=~,2f&units=metric&appid=~a" lat lon key))
         (current (http-object (concatenate 'string +current-url+ suffix)))
         (forecast (http-object (concatenate 'string +forecast-url+ suffix)))
         (main (nlk:json-value current :object "main"))
         (hpa (as-real (nlk:json-value main :number "pressure")))
         (at (nlk:json-value current :integer "dt"))
         (points '()))
    (unless (and hpa at) (error "OpenWeather answered data that could not be read"))
    (push (job "kind" "observed" "at" at "hpa" (coerce hpa 'single-float)
               "fetched" now "place" place)
          points)
    (let ((items (nlk:json-value forecast :array "list")))
      (loop for item across (or items #())
            for row = (nlk:json-value item :object "main")
            for point-at = (nlk:json-value item :integer "dt")
            for point-hpa = (as-real (nlk:json-value row :number "pressure"))
            when (and point-at point-hpa)
              do (push (job "kind" "forecast" "at" point-at "hpa" (coerce point-hpa 'single-float)
                            "fetched" now "place" place)
                       points)))
    (nreverse points)))

(defun keep-samples (store new-points)
  "STORE's samples, NEW-POINTS merged in (same place and time wins), old ones dropped."
  (let* ((cutoff (- (now-utc) +sample-retention+))
         (kept (remove-if (lambda (row)
                            (let ((at (as-real (gethash "at" row))))
                              (and at (< at cutoff))))
                          (rows store "samples")))
         (merged kept))
    (dolist (point new-points)
      (let ((same (lambda (row)
                    (and (equal (gethash "kind" row) (gethash "kind" point))
                         (equal (gethash "place" row) (gethash "place" point))
                         (equal (gethash "at" row) (gethash "at" point))))))
        (setf merged (cons point (remove-if same merged)))))
    (put-rows store "samples" merged)
    merged))

(defun points-of (samples kind &key place)
  "SAMPLES's points of KIND, oldest first, as (AT . HPA)."
  (sort (loop for row in samples
              for at = (as-real (gethash "at" row))
              for hpa = (as-real (gethash "hpa" row))
              when (and at hpa (equal (gethash "kind" row) kind)
                        (or (null place) (equal (gethash "place" row) place)))
                collect (cons at hpa))
        #'< :key #'car))

(defun change-between (points from to)
  "The change over POINTS inside [FROM, TO], and the hours covered, or NIL."
  (let ((inside (remove-if-not (lambda (point) (<= from (car point) to)) points)))
    (when (>= (length inside) 2)
      (let* ((first (first inside)) (last (car (last inside)))
             (hours (/ (- (car last) (car first)) 3600.0)))
        (values (coerce (- (cdr last) (cdr first)) 'single-float) hours)))))

(defun level-of (hpa)
  (cond ((null hpa) nil)
        ((< hpa (setting :very-low-hpa)) "very_low")
        ((< hpa (setting :low-hpa)) "low")
        ((> hpa (setting :high-hpa)) "high")
        (t "normal")))

(defun trend-of (change)
  (let ((drop (setting :drop-hpa)))
    (cond ((null change) nil)
          ((<= change (- drop)) "falling")
          ((>= change drop) "rising")
          (t "steady"))))

(defun pressure-report (&optional (place nil))
  "Today's pressure from what is already stored: no network, ever."
  (let* ((store (load-store))
         (samples (rows store "samples"))
         (key (and place (place-key (first place) (second place))))
         (observed (points-of samples "observed" :place key))
         (forecast (points-of samples "forecast" :place key))
         (now (now-utc))
         (window (* 3600 (setting :window-h)))
         (latest (car (last observed)))
         (fresh (and latest (<= (- now (car latest)) (* 3 3600)) latest))
         (ahead (remove-if (lambda (point) (< (car point) (- now 3600))) forecast)))
    (unless (or observed forecast) (return-from pressure-report nil))
    (multiple-value-bind (past past-hours) (and latest (change-between observed (- (car latest) window) (car latest)))
      (multiple-value-bind (next next-hours) (and ahead (change-between forecast (car (first ahead))
                                                                     (+ (car (first ahead)) window)))
        (let* ((now-hpa (if fresh (cdr fresh) (and (first ahead) (cdr (first ahead)))))
               (lowest (and ahead (reduce (lambda (a b) (if (<= (cdr a) (cdr b)) a b)) ahead)))
               (lowest-in (and lowest (/ (- (car lowest) now) 3600.0))))
          (job "ok" t
               "hpa" (tidy now-hpa)
               "level" (level-of now-hpa)
               "trend" (trend-of past)
               "past_change_hpa" past
               "past_hours" (and past-hours (coerce (round past-hours) 'single-float))
               "next_change_hpa" next
               "next_hours" (and next-hours (coerce (round next-hours) 'single-float))
               "lowest_ahead_hpa" (and lowest (tidy (cdr lowest)))
               "lowest_ahead_in_h" (and lowest-in (round lowest-in))
               "measured_at" (and latest (car latest))
               "estimated" (not fresh)))))))

(defun pressure-now (args)
  "The omonpakari_pressure tool: a place, a key, the measured and forecast
pressure, and the trend it makes."
  (answering
    (let* ((lat (as-real (gethash "lat" args)))
           (lon (as-real (gethash "lon" args)))
           (given (and lat lon (list (coerce lat 'single-float) (coerce lon 'single-float))))
           (place (or given (saved-place) (settings-place)))
           (key (api-key)))
      (when (and given (gethash "remember" args))
        (let ((store (load-store)))
          (setf (gethash "place" store) (job "lat" (first given) "lon" (second given)))
          (save-store store)))
      (cond
        ((null place)
         (json-text (job "ok" nil "reason" "location_not_set"
                          "hint" "Give latitude/longitude, or set them in the omonpakari section.")))
        ((null key)
         (json-text (job "ok" nil "reason" "missing_key"
                          "hint" "Set OPENWEATHER_API_KEY, and name it in openweather_api_key_env.")))
        (t
         (let* ((store (load-store))
                (points (fetch-points (first place) (second place) key)))
           (keep-samples store points)
           (save-store store)
           (json-text (or (pressure-report place)
                          (job "ok" nil "reason" "bad_response")))))))))

;;; --- the journal ----------------------------------------------------------------

(defparameter +kinds+ '("symptom" "medication" "supplement" "food" "note"))

(defun kind-of (args)
  (let ((kind (gethash "kind" args)))
    (cond ((null kind) "note")
          ((member kind +kinds+ :test #'equal) kind)
          (t (error "kind must be one of ~{~a~^, ~}" +kinds+)))))

(defun labelled (args)
  (let ((label (and (stringp (gethash "label" args)) (nlk:trimmed (gethash "label" args)))))
    (if (and label (plusp (length label)))
        (nlk:clip label 60)
        (error "label is required: the person's own short words for it"))))

(defun near-pressure (store at)
  "The stored measured pressure nearest AT within 90 minutes, or NIL."
  (let ((nearest nil) (best (* 90 60)))
    (dolist (row (rows store "samples"))
      (when (equal (gethash "kind" row) "observed")
        (let ((distance (abs (- (or (as-real (gethash "at" row)) 0) at))))
          (when (< distance best)
            (setf best distance nearest (as-real (gethash "hpa" row)))))))
    nearest))

(defun log-add (args)
  (let ((store (load-store))
        (at (or (parse-at (gethash "at" args)) (now-utc))))
    (when (> at (+ (now-utc) 300)) (error "at may not be in the future"))
    (let ((row (job "id" (take-id store)
                    "kind" (kind-of args)
                    "label" (labelled args)
                    "severity" (let ((severity (as-real (gethash "severity" args))))
                                 (and severity (max 1 (min 5 (round severity)))))
                    "dose" (let ((dose (gethash "dose" args)))
                             (and (stringp dose) (nlk:clip (nlk:trimmed dose) 40)))
                    "note" (let ((note (gethash "note" args)))
                             (and (stringp note) (nlk:clip (nlk:trimmed note) 280)))
                    "at" at
                    "created" (now-utc)
                    "pressure_hpa" (near-pressure store at))))
      (add-row store "entries" row)
      (save-store store)
      (json-text (job "ok" t "entry" row)))))

(defun log-list (args)
  (let* ((store (load-store))
         (kind (gethash "kind" args))
         (since (or (parse-at (gethash "since" args))
                    (- (now-utc) (* 86400 (max 1 (min 3650 (round (or (as-real (gethash "days" args)) 14))))))))
         (limit (max 1 (min 200 (round (or (as-real (gethash "limit" args)) 20)))))
         (found (remove-if-not (lambda (row)
                                  (and (or (null kind) (equal kind (gethash "kind" row)))
                                       (>= (or (as-real (gethash "at" row)) 0) since)))
                                (rows store "entries")))
         (newest (last (sort found #'< :key (lambda (row) (or (as-real (gethash "at" row)) 0)))
                       limit)))
    (json-text (job "ok" t "count" (length newest) "entries" (strings newest)))))

(defun log-delete (args)
  (let* ((store (load-store))
         (id (as-real (gethash "id" args)))
         (before (rows store "entries"))
         (after (remove-if (lambda (row) (equal (row-id row) (and id (round id)))) before)))
    (when (null id) (error "id is required"))
    (put-rows store "entries" after)
    (save-store store)
    (json-text (job "ok" t "deleted" (- (length before) (length after))))))

(defun log-limit (args)
  (let* ((store (load-store))
         (topic (string-downcase (labelled args)))
         (max-count (round (or (as-real (gethash "max_count" args)) 1)))
         (window (round (or (as-real (gethash "window_h" args)) 24)))
         (limits (remove-if (lambda (row) (equal (gethash "topic" row) topic))
                            (rows store "limits"))))
    (when (< max-count 1) (error "max_count must be at least 1"))
    (put-rows store "limits"
               (append limits (list (job "topic" topic "label" (labelled args)
                                         "max_count" max-count "window_h" (max 1 window)))))
    (save-store store)
    (json-text (job "ok" t "limit" (car (last (rows store "limits")))))))

(defun log-purge (args)
  (unless (gethash "confirm" args) (error "purge erases everything: pass confirm true"))
  (let ((store (load-store)))
    (dolist (key '("entries" "cycle" "life" "limits"))
      (put-rows store key '()))
    (save-store store)
    (json-text (job "ok" t "purged" t))))

(defun log-entry (args)
  "The omonpakari_log tool: the person's own words, filed under their kind."
  (answering
    (let ((action (or (gethash "action" args) "list")))
      (cond ((equal action "add") (log-add args))
            ((equal action "list") (log-list args))
            ((equal action "delete") (log-delete args))
            ((equal action "set_limit") (log-limit args))
            ((equal action "purge") (log-purge args))
            (t (error "action must be add, list, delete, set_limit or purge"))))))

;;; --- the cycle ------------------------------------------------------------------

(defun cycle-marks (store)
  (sort (copy-list (rows store "cycle")) #'<
        :key (lambda (row) (or (as-real (gethash "at" row)) 0))))

(defun cycle-starts (store)
  (remove-if-not (lambda (row) (equal (gethash "mark" row) "start")) (cycle-marks store)))

(defun cycle-lengths (store)
  "Observed cycle lengths in days, oldest first."
  (let ((starts (cycle-starts store)))
    (loop for (a b) on starts
          while b
          collect (/ (- (as-real (gethash "at" b)) (as-real (gethash "at" a))) 86400.0))))

(defun cycle-report (&optional (store (load-store)))
  (let* ((lengths (cycle-lengths store))
         (starts (cycle-starts store))
         (last-start (car (last starts)))
         (now (now-utc))
         (average (and lengths (/ (reduce #'+ lengths) (length lengths))))
         (shortest (and lengths (reduce #'min lengths)))
         (longest (and lengths (reduce #'max lengths))))
    (if (null last-start)
        (job "ok" t "marked" nil
             "hint" "Mark a period start with omonpakari_cycle action mark, mark start.")
        (let* ((day (1+ (floor (- now (as-real (gethash "at" last-start))) 86400)))
               (phase (cond ((<= day 5) "menstrual")
                            ((<= day 13) "follicular")
                            ((<= day 16) "ovulation")
                            (t "luteal")))
               (next-start (and average (+ (as-real (gethash "at" last-start)) (* (round average) 86400))))
               (from (and next-start (- next-start (* 86400 (max 1 (round (- average shortest)))))))
               (to (and next-start (+ next-start (* 86400 (max 1 (round (- longest average))))))))
          (job "ok" t
               "marked" (length starts)
               "day" day
               "phase" phase
               "observed_lengths" (and lengths (coerce (mapcar #'tidy lengths) 'vector))
               "average_days" (and average (tidy average))
               "shortest_days" shortest
               "longest_days" longest
               "next_window" (and from (job "from" (iso-utc from) "to" (iso-utc (or to from))))
               "hint" (and (null lengths) "One start is marked; a window needs two."))))))

(defun cycle-status (args)
  "The omonpakari_cycle tool: marks in, observed lengths and a window out."
  (answering
    (let ((action (or (gethash "action" args) "status")))
      (cond
        ((equal action "status") (json-text (cycle-report)))
        ((equal action "mark")
         (let* ((store (load-store))
                (mark (or (gethash "mark" args) "start"))
                (at (or (parse-at (gethash "at" args)) (now-utc))))
           (unless (member mark '("start" "end") :test #'equal)
             (error "mark must be start or end"))
           (let ((row (job "id" (take-id store) "mark" mark "at" at
                           "note" (let ((note (gethash "note" args)))
                                    (and (stringp note) (nlk:clip (nlk:trimmed note) 280))))))
             (add-row store "cycle" row)
             (save-store store)
             (json-text (job "ok" t "mark" row)))))
        ((equal action "delete")
         (let* ((store (load-store))
                (id (as-real (gethash "id" args)))
                (before (rows store "cycle"))
                (after (remove-if (lambda (row) (equal (row-id row) (and id (round id)))) before)))
           (when (null id) (error "id is required"))
           (put-rows store "cycle" after)
           (save-store store)
           (json-text (job "ok" t "deleted" (- (length before) (length after))))))
        (t (error "action must be mark, status or delete"))))))

;;; --- interactions ---------------------------------------------------------------

(defparameter *reference*
  '(("cocoa" ("cocoa" "chocolate" "cacao") "inhibits"
     "Cocoa's polyphenols bind iron in the gut; about 71% of the iron in a test meal was blocked."
     "Hurrell, Reddy & Cook 1999, Am J Clin Nutr 70:97")
    ("black tea" ("black tea" "tea" "tannin" "tannins") "inhibits"
     "Tea tannins complex non-haem iron; 79-94% of absorption blocked in the cited study."
     "Disler et al. 1975, Gut 16:193")
    ("coffee" ("coffee" "espresso" "latte") "inhibits"
     "Chlorogenic acid and tannins in coffee reduce non-haem iron absorption."
     "Morck, Lynch & Cook 1983, Am J Clin Nutr 37:416")
    ("calcium" ("calcium" "milk" "yoghurt" "yogurt" "cheese") "inhibits"
     "Calcium inhibits both haem and non-haem iron absorption within one meal."
     "Hallberg et al. 1991, Am J Clin Nutr 53:112")
    ("soy protein" ("soy" "soya" "tofu" "edamame") "inhibits"
     "Soy protein's phytic acid binds iron; absorption falls in the cited trial."
     "Hurrell et al. 1992, Br J Nutr 68:185")
    ("vitamin C" ("vitamin c" "ascorbic acid" "lemon" "citrus" "orange") "promotes"
     "Ascorbic acid reduces ferric to ferrous iron and holds it soluble, raising absorption."
     "Hallberg, Brune & Rossander 1989, Am J Clin Nutr 49:140")))

(defparameter *pairs*
  '(("nsaid and alcohol" ("ibuprofen" "naproxen" "diclofenac" "aspirin" "nsaid")
     ("alcohol" "beer" "wine" "sake" "spirits")
     "Both irritate the gastric mucosa; together they raise the risk of bleeding."
     "Lanza et al. 2009, Am J Gastroenterol 104:728")
    ("two nsaids" ("ibuprofen" "naproxen" "diclofenac" "aspirin" "nsaid")
     ("ibuprofen" "naproxen" "diclofenac" "aspirin" "nsaid") nil
     "Two NSAIDs add their gastric risk without adding relief."
     "Lanza et al. 2009, Am J Gastroenterol 104:728")))

(defparameter +iron-aliases+ '("iron" "ferrous" "ferritin" "iron supplement"))

(defun matches (label aliases)
  (let ((label (string-downcase (or label ""))))
    (some (lambda (alias) (search (string-downcase alias) label)) aliases)))

(defun classify (label)
  (loop for (name aliases effect fact source) in *reference*
        when (matches label aliases)
          do (return (job "name" name "effect" effect "fact" fact "source" source))))

(defun iron-entry-p (row)
  (and (member (gethash "kind" row) '("supplement" "medication" "food") :test #'equal)
       (matches (gethash "label" row) +iron-aliases+)))

(defun alternative-for (store substance)
  "The person's own alternative to SUBSTANCE, from a life note, or NIL."
  (let ((note (find-if (lambda (row)
                         (and (stringp (gethash "relates" row))
                              (matches (gethash "relates" row) (list substance))))
                       (rows store "life"))))
    (and note (gethash "note" note))))

(defun interactions-scan (store days window)
  (let* ((since (- (now-utc) (* 86400 days)))
         (entries (remove-if-not (lambda (row) (>= (or (as-real (gethash "at" row)) 0) since))
                                 (rows store "entries")))
         (found '()))
    (dolist (iron entries)
      (when (iron-entry-p iron)
        (dolist (other entries)
          (unless (eq other iron)
            (let ((gap (/ (abs (- (as-real (gethash "at" other)) (as-real (gethash "at" iron)))) 3600.0)))
              (when (<= gap window)
                (let ((class (classify (gethash "label" other))))
                  (when class
                    (push (job "substance" (gethash "label" other)
                               "role" (gethash "effect" class)
                               "name" (gethash "name" class)
                               "fact" (gethash "fact" class)
                               "source" (gethash "source" class)
                               "hours_from_iron" (coerce (/ (round (* gap 10)) 10.0) 'single-float)
                               "at" (iso-utc (as-real (gethash "at" other)))
                               "alternative" (alternative-for store (gethash "name" class)))
                           found)))))))))
    found))

(defun interactions-check (args)
  "The omonpakari_interactions tool: what was taken close to iron, and its source."
  (answering
    (let ((action (or (gethash "action" args) "check")))
      (cond
        ((equal action "table")
         (json-text
          (job "interactions"
               (coerce (mapcar (lambda (row) (job "substance" (first row) "effect" (third row)
                                                   "fact" (fourth row) "source" (fifth row)))
                               *reference*) 'vector)
               "pairs"
               (coerce (mapcar (lambda (row) (job "pair" (first row) "fact" (fourth row)
                                                   "source" (fifth row)))
                               *pairs*) 'vector))))
        ((equal action "check")
         (let* ((store (load-store))
                (days (max 1 (min 365 (round (or (as-real (gethash "days" args)) 7)))))
                (window (max 0.5 (min 12.0 (or (as-real (gethash "window_h" args))
                                               (setting :interaction-window-h) 2.0))))
                (found (interactions-scan store days window)))
           (json-text (job "ok" t "days" days "window_h" (coerce window 'single-float)
                           "count" (length found) "interactions" (strings found)))))
        (t (error "action must be check or table"))))))

;;; --- life notes ----------------------------------------------------------------

(defun life-notes (args)
  "The omonpakari_life tool: standing facts about the person, in their own words."
  (answering
    (let ((action (or (gethash "action" args) "list")))
      (cond
        ((equal action "list")
         (json-text (job "ok" t "notes" (strings (rows (load-store) "life")))))
        ((equal action "set")
         (let* ((store (load-store))
                (topic (string-downcase (nlk:trimmed (or (gethash "topic" args)
                                                          (error "topic is required")))))
                (note (nlk:trimmed (or (gethash "note" args) (error "note is required"))))
                (row (job "topic" (nlk:clip topic 40) "note" (nlk:clip note 280)
                          "relates" (let ((relates (gethash "relates" args)))
                                      (and (stringp relates) (nlk:clip (nlk:trimmed relates) 60)))
                          "at" (now-utc))))
           (put-rows store "life"
                     (append (remove-if (lambda (old) (equal (gethash "topic" old) topic))
                                        (rows store "life"))
                             (list row)))
           (save-store store)
           (json-text (job "ok" t "note" row))))
        ((equal action "delete")
         (let* ((store (load-store))
                (topic (string-downcase (nlk:trimmed (or (gethash "topic" args) ""))))
                (before (rows store "life"))
                (after (remove-if (lambda (row) (equal (gethash "topic" row) topic)) before)))
           (put-rows store "life" after)
           (save-store store)
           (json-text (job "ok" t "deleted" (- (length before) (length after))))))
        (t (error "action must be set, list or delete"))))))

;;; --- waves and patterns ----------------------------------------------------------

(defparameter +sleep-words+ '("sleep" "insomnia" "woke" "slept" "night"))

(defun count-between (rows from to &optional (test (lambda (row) t)))
  (count-if (lambda (row)
              (let ((at (as-real (gethash "at" row))))
                (and at (<= from at to) (funcall test row))))
            rows))

(defun signal-of (count)
  (cond ((null count) nil)
        ((zerop count) "settled")
        ((<= count 2) "watch")
        (t "heavy")))

(defun waves ()
  "The omonpakari_waves tool: the composed states, each with the counts behind it."
  (answering
    (let* ((store (load-store))
           (entries (rows store "entries"))
           (now (now-utc))
           (week (- now (* 7 86400)))
           (fortnight (- now (* 14 86400)))
           (report (ignore-errors (pressure-report)))
           (cycle (ignore-errors (cycle-report store)))
           (symptoms (+ (count-between entries week now
                                        (lambda (row) (equal (gethash "kind" row) "symptom")))
                        (count-between entries week now
                                        (lambda (row) (and (equal (gethash "kind" row) "symptom")
                                                           (matches (gethash "label" row) +sleep-words+))))))
           (sleep (count-between entries week now
                                 (lambda (row) (matches (gethash "label" row) +sleep-words+))))
           (sleep-before (count-between entries fortnight week
                                        (lambda (row) (matches (gethash "label" row) +sleep-words+))))
           (food (count-between entries week now
                                (lambda (row) (equal (gethash "kind" row) "food")))))
      (json-text
       (job "ok" t
            "pressure" (and report (job "state" (gethash "level" report)
                                        "trend" (gethash "trend" report)
                                        "hpa" (gethash "hpa" report)))
            "cycle" (and cycle (job "phase" (gethash "phase" cycle) "day" (gethash "day" cycle)
                              "next_window" (gethash "next_window" cycle)))
            "symptoms" (job "state" (signal-of symptoms) "count_7d" symptoms)
            "sleep" (job "state" (signal-of sleep) "count_7d" sleep "count_7d_before" sleep-before)
            "nutrition" (job "state" (signal-of (if (zerop food) nil food)) "food_entries_7d" food)
            "journal_entries" (length entries))))))

(defun correlate (args)
  "The omonpakari_correlate tool: entries against the pressure hours they fell in."
  (answering
    (let* ((store (load-store))
           (kind (or (gethash "kind" args) "symptom"))
           (label (gethash "label" args))
           (days (max 1 (min 365 (round (or (as-real (gethash "days" args)) 60)))))
           (window (max 1 (min 48 (round (or (as-real (gethash "window_h" args)) 12)))))
           (drop (abs (or (as-real (gethash "drop_hpa" args)) (setting :drop-hpa))))
           (since (- (now-utc) (* 86400 days)))
           (observed (points-of (rows store "samples") "observed"))
           (entries (remove-if-not (lambda (row)
                                      (and (equal kind (gethash "kind" row))
                                           (or (null label) (equal label (gethash "label" row)))
                                           (>= (or (as-real (gethash "at" row)) 0) since)))
                                    (rows store "entries")))
           (changes (loop for entry in entries
                          for at = (as-real (gethash "at" entry))
                          when at
                            collect (multiple-value-bind (change hours)
                                        (change-between observed (- at (* 3600 window)) at)
                                      (declare (ignore hours))
                                      change)))
           (known (remove nil changes))
           (falling-entries (count-if (lambda (change) (and change (<= change (- drop)))) known))
           (hours (loop for (a b) on observed
                        while b
                        when (<= (- (car b) (car a)) 7200)
                          collect (- (cdr b) (cdr a))))
           (all-known (length hours))
           (falling-hours (count-if (lambda (change) (<= change (- drop))) hours))
           (share (and (plusp (length known)) (/ (float falling-entries) (length known))))
           (base (and (plusp all-known) (/ (float falling-hours) all-known))))
      (json-text
       (job "ok" t
            "kind" kind "label" label "days" days "window_h" window "drop_hpa" drop
            "entries" (length entries)
            "entries_with_pressure" (length known)
            "entries_on_falling_hours" falling-entries
            "entries_share" (and share (coerce (/ (round (* share 100)) 100.0) 'single-float))
            "hours_covered" all-known
            "baseline_share" (and base (coerce (/ (round (* base 100)) 100.0) 'single-float))
            "lift" (and share base (plusp base) (coerce (/ (round (* 100 (/ share base))) 100.0) 'single-float))
            "insufficient_data" (or (< (length known) 5) (< all-known 72))
            "hint" (and (or (< (length known) 5) (< all-known 72))
                         "Few entries or few measured hours: an early impression, not a pattern."))))))

;;; --- the quiet picture -----------------------------------------------------------

(defun journal-line (store now)
  (let* ((since (- now (* 48 3600)))
         (recent (remove-if-not (lambda (row) (>= (or (as-real (gethash "at" row)) 0) since))
                                 (rows store "entries"))))
    (when recent
      (format nil "journal (last 48h): ~{~a~^, ~}"
              (loop for row in (last (sort (copy-list recent) #'<
                                           :key (lambda (row) (or (as-real (gethash "at" row)) 0)))
                                     6)
                    for label = (gethash "label" row)
                    for hours = (round (/ (- now (or (as-real (gethash "at" row)) now)) 3600.0))
                    collect (format nil "~a (~a) ~ah ago" label (gethash "kind" row) hours))))))

(defun pressure-line (store)
  (let ((report (ignore-errors (pressure-report))))
    (when (and report (gethash "hpa" report))
      (format nil "pressure: ~,0f hPa (~a~@[, ~a~]), past ~ah ~@[~,1f hPa~]; next ~ah ~@[~,1f hPa~]~@[, lowest ~,0f in ~ah~]."
              (gethash "hpa" report) (gethash "level" report)
              (and (gethash "estimated" report) "estimated")
              (setting :window-h) (gethash "past_change_hpa" report)
              (setting :window-h) (gethash "next_change_hpa" report)
              (gethash "lowest_ahead_hpa" report) (gethash "lowest_ahead_in_h" report)))))

(defun cycle-line (store)
  (let ((report (ignore-errors (cycle-report store))))
    (when (and report (gethash "phase" report))
      (format nil "cycle: day ~a, ~a (estimate)~@[; next window ~a to ~a~]."
              (gethash "day" report) (gethash "phase" report)
              (gethash "from" (gethash "next_window" report))
              (gethash "to" (gethash "next_window" report))))))

(defun interactions-line (store)
  (let ((found (ignore-errors (interactions-scan store 1 (setting :interaction-window-h)))))
    (when found
      (format nil "interactions today: ~{~a~^; ~}"
              (loop for item in (last (reverse found) 3)
                    collect (format nil "~a (~a) ~,1fh from iron~@[, your alternative: ~a~]"
                                    (gethash "substance" item) (gethash "role" item)
                                    (gethash "hours_from_iron" item)
                                    (gethash "alternative" item)))))))

(defun life-line (store)
  (let ((notes (rows store "life")))
    (when notes
      (format nil "life notes: ~{~a~^; ~}"
              (loop for row in (last notes 4)
                    collect (format nil "~a: ~a" (gethash "topic" row) (gethash "note" row)))))))

(defun picture-text ()
  "The quiet picture of today, at most 1200 characters, or NIL when there is
nothing to say. It reads the store only: the hook never calls the network."
  (let* ((store (load-store))
         (lines (remove nil (list (ignore-errors (pressure-line store))
                                  (ignore-errors (cycle-line store))
                                  (ignore-errors (interactions-line store))
                                  (ignore-errors (journal-line store (now-utc)))
                                  (ignore-errors (life-line store))))))
    (when lines
      (let ((text (format nil "~{~a~%~}Background only - a quiet picture of today, not a message. Let it inform any topic: weigh it before suggesting, prefer the person's own alternatives, and do not propose an item flagged under interactions today near its conflicting entry. Do not lecture, diagnose or recite these numbers, and do not mention this note itself."
                          lines)))
        (nlk:clip text 1200)))))

(defun quiet-sections (next session-id)
  "The pre_llm_call hook, as one standing section: today's picture, before the
reply, when the cell runs and the section is on."
  (let* ((sections (funcall next session-id))
         (text (and (setting :quiet-context) (ignore-errors (picture-text)))))
    (if (and text (plusp (length text)))
        (acons "omonpakari" text (remove "omonpakari" sections :key #'car :test #'equal))
        sections)))

(defun show-picture (args session-id)
  "The /omonpakari command: the same picture, said to the person who asked."
  (declare (ignore session-id))
  (declare (ignore args))
  (or (let ((text (ignore-errors (picture-text))))
        (and text (plusp (length text)) text))
      "omonpakari has nothing to say yet: log something with omonpakari_log, or ask for pressure."))

;;; --- the tools' shapes and the declaration ---------------------------------------

(defun prop (type description)
  (nlk:make-json-object "type" type "description" description))

(defun schema (description properties)
  "The plist schema the provider is shown: one shape, no contract record."
  (list :description description
        :parameters (nlk:make-json-object "type" "object" "properties" properties)))

(defparameter *pressure-schema*
  (schema
   "Barometric pressure for a place: now, the past window_h and the forecast ahead, with the level and trend. Only the omonpakari section's key and place are used; a missing key answers a reason."
   (nlk:make-json-object
    "lat" (prop "number" "Latitude in decimal degrees, only when the person gave a place")
    "lon" (prop "number" "Longitude in decimal degrees, only when the person gave a place")
    "remember" (prop "boolean" "Keep this place for later calls, only when the person asks"))))

(defparameter *log-schema*
  (schema
   "The person's own health journal: symptoms, medications, supplements, food and notes, in their words, with severity, dose and when it happened."
   (nlk:make-json-object
    "action" (prop "string" "add, list, delete, set_limit or purge")
    "kind" (prop "string" "symptom, medication, supplement, food or note")
    "label" (prop "string" "The person's own short words for it")
    "severity" (prop "integer" "1-5, only if they said how strong it was")
    "dose" (prop "string" "The dose as they said it, at most 40 characters")
    "note" (prop "string" "A short note, at most 280 characters; never the whole message")
    "at" (prop "string" "When it happened, ISO 8601 with their offset; a time without one is read as UTC")
    "since" (prop "string" "For list: only entries at or after this ISO 8601 time")
    "days" (prop "integer" "For list: how many days back (default 14)")
    "limit" (prop "integer" "For list: how many entries, newest first (default 20, max 200)")
    "id" (prop "integer" "For delete: the entry's id")
    "max_count" (prop "integer" "For set_limit: how many in the window")
    "window_h" (prop "integer" "For set_limit: the window in hours")
    "confirm" (prop "boolean" "For purge: true erases everything"))))

(defparameter *cycle-schema*
  (schema
   "Menstrual cycle marks and windows: mark a start or end, read the observed lengths and the next window as a range of dates."
   (nlk:make-json-object
    "action" (prop "string" "mark, status or delete")
    "mark" (prop "string" "start or end")
    "at" (prop "string" "When, ISO 8601; now when left out")
    "note" (prop "string" "An optional short note")
    "id" (prop "integer" "For delete: the mark's id"))))

(defparameter *interactions-schema*
  (schema
   "Sourced absorption checks: journal entries taken close to an iron entry that inhibit or promote it, with the fact and its source; or the reference table itself."
   (nlk:make-json-object
    "action" (prop "string" "check or table")
    "days" (prop "integer" "For check: how many days back (default 7)")
    "window_h" (prop "number" "For check: hours around an iron entry that count as near"))))

(defparameter *life-schema*
  (schema
   "Standing facts about the person's life in their own words: the morning drink, what warms them up, and what each is their alternative to."
   (nlk:make-json-object
    "action" (prop "string" "set, list or delete")
    "topic" (prop "string" "A short topic, e.g. morning drink")
    "note" (prop "string" "The note in their words, at most 280 characters")
    "relates" (prop "string" "What this is their alternative to, e.g. coffee"))))

(defparameter *waves-schema*
  (schema
   "The composed states of today: pressure, cycle, symptoms, sleep and nutrition, each with the counts behind it. No arguments."
   (nlk:make-json-object)))

(defparameter *correlate-schema*
  (schema
   "Journal entries against the pressure hours they fell in, with the raw counts, the baseline share and the lift."
   (nlk:make-json-object
    "kind" (prop "string" "Which entries to compare (default symptom)")
    "label" (prop "string" "Only entries with this label")
    "days" (prop "integer" "How many days back (default 60)")
    "window_h" (prop "integer" "Hours before each entry the pressure change is measured over (default 12)")
    "drop_hpa" (prop "number" "A drop of at least this many hPa counts as falling")
    "observed_only" (prop "boolean" "Use only measured pressure"))))

(defun coordinate (text low high name)
  "TEXT as a coordinate in [LOW, HIGH], NIL when absent, or a config refusal."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (cond ((or (null text) (zerop (length text))) nil)
          (t (let ((value (ignore-errors
                            (let ((*read-eval* nil)) (read-from-string text)))))
               (unless (and (realp value) (<= low value high))
                 (nlk:config-error "~a must be a number between ~a and ~a" name low high))
               (coerce value 'single-float))))))

(defun cell-settings (values table)
  "The declared VALUES, with latitude and longitude read as coordinates and the
store's directory resolved: read once, here."
  (declare (ignore table))
  (let ((plist (copy-list values)))
    (setf (getf plist :latitude) (coordinate (getf values :latitude) -90 90 "latitude")
          (getf plist :longitude) (coordinate (getf values :longitude) -180 180 "longitude"))
    (setf (getf plist :data-dir)
          (uiop:ensure-directory-pathname
           (nlk:expand-home (or (getf values :data-dir) "~/.nodecode/omonpakari"))))
    plist))

(nle:define-cell omonpakari
  (:section ("omonpakari")
    (:guide "nothing here is required: the journal works without a key; a key adds barometric pressure. \
set latitude and longitude to read pressure for a place without asking each time, then restart the cell")
    ("data_dir" :path :doc "where the journal file lives; under the organism's home by default")
    ("openweather_api_key_env" :env :doc "the environment variable holding an OpenWeather API key (the free tier works)")
    ("latitude" :string :doc "decimal degrees, -90 to 90: the place pressure is read for")
    ("longitude" :string :doc "decimal degrees, -180 to 180")
    ("low_hpa" :integer :default 1010 :doc "pressure below this (hPa) is labelled low")
    ("very_low_hpa" :integer :default 1003 :doc "pressure below this (hPa) is labelled very_low")
    ("high_hpa" :integer :default 1020 :doc "pressure above this (hPa) is labelled high")
    ("drop_hpa" :integer :default 6 :min 1 :doc "a change of at least this many hPa counts as falling or rising")
    ("window_h" :integer :default 24 :min 3 :doc "hours used for the past and forecast change (3-72)")
    ("interaction_window_h" :integer :default 2 :min 1 :doc "hours around an iron entry that count as near")
    ("quiet_context" :boolean :default t :doc "put a short picture of today into the standing context before each reply")
    ("timeout_seconds" :integer :default 10 :min 1 :doc "one OpenWeather lookup's ceiling"))
  (:settings #'cell-settings)
  (:tool "omonpakari_pressure" *pressure-schema* #'pressure-now)
  (:tool "omonpakari_log" *log-schema* #'log-entry)
  (:tool "omonpakari_cycle" *cycle-schema* #'cycle-status)
  (:tool "omonpakari_interactions" *interactions-schema* #'interactions-check)
  (:tool "omonpakari_life" *life-schema* #'life-notes)
  (:tool "omonpakari_waves" *waves-schema* #'waves)
  (:tool "omonpakari_correlate" *correlate-schema* #'correlate)
  (:hook 'nle::read-harness-sections #'quiet-sections)
  (:help :omonpakari
    "Health journal, pressure, cycle windows and interaction checks"
    *manual*)
  (:command "omonpakari" 'show-picture
            :description "Today's quiet picture from the health journal"))
