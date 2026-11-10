;;;; cell.lisp --- OpenRouter's own reported key spend, read from OpenRouter.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Read-only. Two endpoints, and nothing sent anywhere else: OpenRouter's
;;;; /key, which answers the spend its own billing has recorded for this key
;;;; (every application using it, not only this one), with the key in one
;;;; Authorization header; and, unless the operator pins a rate, a public
;;;; USD/INR reference rate that carries no key and no spend data. One tool
;;;; (openrouter_spend), one command (/openrouter-spend), one manual topic,
;;;; and two Lisp functions for eval: (openrouter-spend:report) and
;;;; (openrouter-spend:history).
;;;;
;;;; The original is a Hermes desktop plugin: a status-bar chip and a popup
;;;; page over a FastAPI backend, with hooks and execution middleware that
;;;; kept a ledger of each request's cost. Here the numbers are the
;;;; provider's own counters, and the local record is one reading kept each
;;;; time the cell asks, so the spend between two readings is visible.

(defpackage #:nodecode-openrouter-spend
  (:use #:cl)
  (:nicknames #:openrouter-spend)
  (:export #:report #:history #:credential #:fingerprint #:openrouter-spend-error))

(in-package #:nodecode-openrouter-spend)

(nlk:define-peripheral openrouter-spend :not-running t)

(defparameter +api+ "https://openrouter.ai/api/v1"
  "The accounting base the original plugin read. /key is the one endpoint a
report needs: its counters are the provider's own, so no request of this
cell ever costs a model call.")

(defparameter +fx-url+ "https://api.frankfurter.dev/v2/rate/usd/inr"
  "The public USD/INR reference rate the daily line reads, keyless.")

(defparameter +periods+ '(("today" :today "Today") ("week" :week "This week")
                          ("month" :month "This month") ("total" :total "All time"))
  "The four counters OpenRouter reports, in the order a report lists them.")

(defparameter +history-cap+ 500
  "How many readings this home keeps; the oldest goes first.")

(defvar *http* #'nlk:http
  "The one seam that leaves this image: an HTTP exchange. Tests stub it.")

(defvar *cache* (make-hash-table :test #'equal)
  "What was read while the cell runs: \"key\" -> (FINGERPRINT AT SNAPSHOT),
\"fx\" -> (AT RATE-PLIST).")

;;; --- reading JSON ---------------------------------------------------------------

(defun object-member (object key)
  "OBJECT's member KEY, or NIL."
  (and (hash-table-p object) (gethash key object)))

(defun text-member (object key)
  "OBJECT's member KEY as a non-empty string, or NIL."
  (let ((value (object-member object key)))
    (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))

(defun amount (value)
  "VALUE as a non-negative real, or NIL: a JSON number from the wire, a
string from this cell's own file (\"\" when the provider reported none)."
  (flet ((good (number)
           (let ((number (ignore-errors (coerce number 'double-float))))
             (and number (not (minusp number)) (= number number)
                  (< number most-positive-double-float) number))))
    (cond ((and (realp value) (not (complexp value))) (good value))
          ((stringp value)
           (let ((*read-default-float-format* 'double-float)
                 (*read-eval* nil)
                 (text (nlk:trimmed value)))
             (and (plusp (length text))
                  (ignore-errors (let ((number (read-from-string text)))
                                   (and (realp number) (good number)))))))
          (t nil))))

(defun whole-seconds (value)
  "VALUE as a universal time, or 0."
  (if (and (integerp value) (plusp value)) value 0))

(defun utc (universal)
  "UNIVERSAL as the UTC stamp every line of this cell reads."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (whole-seconds universal) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d UTC" year month day hour minute)))

(defun grouped (text)
  "TEXT with a comma every three digits of its integer part."
  (let* ((dot (position #\. text))
         (whole (if dot (subseq text 0 dot) text))
         (rest (if dot (subseq text dot) ""))
         (sign (and (plusp (length whole)) (char= (char whole 0) #\-))))
    (when sign (setf whole (subseq whole 1)))
    (let ((out '()))
      (loop for i downfrom (1- (length whole)) to 0
            do (push (char whole i) out)
               (when (and (plusp i) (zerop (mod (- (length whole) i) 3)))
                 (push #\, out)))
      (format nil "~@[~a~]~a~a" (and sign "-") (coerce out 'string) rest))))

(defun money (value)
  "VALUE in USD, six decimals as the provider counts them, or UNAVAILABLE."
  (if (null value)
      "unavailable"
      (format nil "$~a" (grouped (format nil "~,6f" (coerce value 'double-float))))))

(defun rupees (value)
  "VALUE in INR, two decimals, or UNAVAILABLE."
  (if (null value)
      "unavailable"
      (format nil "\u20b9~a" (grouped (format nil "~,2f" (coerce value 'double-float))))))

(defun dual (value rate)
  "One amount in both currencies, the way the original's popup read it."
  (if (null value)
      "unavailable"
      (format nil "~a / ~a" (money value)
              (if rate (rupees (* value rate)) "INR unavailable"))))

;;; --- the key, and which key a reading belongs to --------------------------------

(defun credential ()
  "The OpenRouter key the section names, from the variable it names, or a
refusal saying which variable to export."
  (let* ((name (let ((value (ignore-errors (setting :key-env))))
                 (and (stringp value) (nlk:trimmed value)))))
    (unless (and name (plusp (length name)))
      (fail "the section names no variable: set key_env to the variable holding the OpenRouter key"))
    (let ((key (uiop:getenv name)))
      (unless (and key (plusp (length (nlk:trimmed key))))
        (fail "~a is not set: export the OpenRouter key in it, or point key_env at the variable that holds it"
              name))
      (nlk:trimmed key))))

(defun fingerprint (key)
  "KEY's 64-bit FNV-1a, 16 hex digits: which key a reading belongs to, never
the key itself, and the same in every image."
  (let ((hash #xcbf29ce484222325))
    (loop for char across key
          do (setf hash (logand (* (logxor hash (logand (char-code char) #xff))
                                    #x100000001b3)
                                 #xffffffffffffffff)))
    (format nil "~16,'0x" hash)))

;;; --- what leaves the image ------------------------------------------------------

(defun http-json (url headers)
  "GET URL with HEADERS => (values OBJECT STATUS). The body is decoded, never
surfaced: a provider's error body is its own business."
  (multiple-value-bind (body status)
      (funcall *http* :get url :headers headers
                :timeout (setting :timeout-seconds)
                :connect-timeout (setting :timeout-seconds))
    (let ((text (if (stringp body) body (ignore-errors (nlk:body-text body)))))
      (values (and text (plusp (length text)) (ignore-errors (nlk:decode-json text)))
              status))))

(defun fetch-key (key)
  "OpenRouter's accounting for KEY => a plist of the counters it reports."
  (multiple-value-bind (object status)
      (http-json (concatenate 'string +api+ "/key")
                 (list (cons "Authorization" (concatenate 'string "Bearer " key))
                       (cons "Accept" "application/json")))
    (unless (and (integerp status) (<= 200 status 299))
      (fail "OpenRouter answered HTTP ~a for this key's accounting~@[ (a key it does not know answers 401)~]"
            status (and (integerp status) (= status 401))))
    (let ((data (object-member object "data")))
      (unless (hash-table-p data)
        (fail "OpenRouter returned no accounting data for this key"))
      (let ((snapshot (list :today (amount (object-member data "usage_daily"))
                            :week (amount (object-member data "usage_weekly"))
                            :month (amount (object-member data "usage_monthly"))
                            :total (amount (object-member data "usage"))
                            :limit (amount (object-member data "limit"))
                            :remaining (amount (object-member data "limit_remaining"))
                            :byok (amount (object-member data "byok_usage")))))
        (unless (some (lambda (pair) (getf snapshot (second pair))) +periods+)
          (fail "OpenRouter returned no spend counters"))
        snapshot))))

(defun fetch-rate ()
  "The public USD/INR reference rate => a plist. Carries no key and no
spend: one GET for one number a day."
  (multiple-value-bind (object status)
      (http-json +fx-url+ (list (cons "Accept" "application/json")))
    (unless (and (integerp status) (<= 200 status 299))
      (fail "the USD/INR reference rate could not be read (HTTP ~a)" status))
    (let ((rate (amount (object-member object "rate"))))
      (unless (and rate (plusp rate) (<= rate 1000)
                   (equal "USD" (text-member object "base"))
                   (equal "INR" (text-member object "quote")))
        (fail "the USD/INR reference rate answered an unexpected body"))
      (list :rate rate :date (text-member object "date")
            :source "Frankfurter" :stale nil))))

;;; --- the record on disk ---------------------------------------------------------

(defparameter +store-version+ 1)

(defun expand-path (text)
  "TEXT with a leading ~/ read against the operator's home."
  (if (and (stringp text) (uiop:string-prefix-p "~/" text))
      (uiop:native-namestring (merge-pathnames (subseq text 2) (user-homedir-pathname)))
      text))

(defun store-path ()
  "Where the readings are kept: the section's history_file, else this home's
openrouter-spend/spend.json."
  (let ((named (ignore-errors (setting :history-file))))
    (if (and (stringp named) (plusp (length (nlk:trimmed named))))
        (expand-path named)
        (uiop:native-namestring (nlk:home "openrouter-spend/spend.json")))))

(defun read-store ()
  "The record as decoded JSON. An empty one when there is none, or it cannot
be read: a lost record must never stop a reading."
  (handler-case
      (let ((path (store-path)))
        (if (probe-file path)
            (let ((object (nlk:decode-json (uiop:read-file-string path))))
              (if (hash-table-p object) object (nlk:json-object)))
            (nlk:json-object)))
    (error () (nlk:json-object))))

(defun write-store (store)
  "STORE as the record file, written whole or not at all."
  (handler-case
      (let* ((path (store-path))
             (temporary (concatenate 'string path ".new")))
        (ensure-directories-exist path)
        (with-open-file (stream temporary :direction :output :if-exists :supersede
                                         :if-does-not-exist :create :external-format :utf-8)
          (write-string (nlk:encode-json-object store) stream))
        (uiop:rename-file-overwriting-target (pathname temporary) (pathname path))
        t)
    (error () nil)))

(defun readings (store)
  "STORE's readings, oldest first, as a list."
  (let ((raw (object-member store "history")))
    (coerce (if (vectorp raw) raw #()) 'list)))

(defun snapshot-of (object)
  "OBJECT (a decoded reading or its data member) as a snapshot plist."
  (list :today (amount (text-member object "today"))
        :week (amount (text-member object "week"))
        :month (amount (text-member object "month"))
        :total (amount (text-member object "total"))
        :limit (amount (text-member object "limit"))
        :remaining (amount (text-member object "remaining"))
        :byok (amount (text-member object "byok"))))

(defun snapshot-json (snapshot)
  "SNAPSHOT as the record keeps it: the provider's counters as strings, an
absent one as the empty string, so a re-read cannot mistake it for zero."
  (flet ((text (value) (if value (format nil "~,6f" (coerce value 'double-float)) "")))
    (nlk:json-object "today" (text (getf snapshot :today))
                      "week" (text (getf snapshot :week))
                      "month" (text (getf snapshot :month))
                      "total" (text (getf snapshot :total))
                      "limit" (text (getf snapshot :limit))
                      "remaining" (text (getf snapshot :remaining))
                      "byok" (text (getf snapshot :byok)))))

(defun stored-reading (store key-fingerprint)
  "=> (values SNAPSHOT AT): the last reading kept for KEY-FINGERPRINT."
  (let ((entry (object-member (object-member store "keys") key-fingerprint)))
    (when (hash-table-p entry)
      (values (snapshot-of (object-member entry "data"))
              (whole-seconds (object-member entry "fetched_at"))))))

(defun history-row (key-fingerprint snapshot at)
  "One reading as the history keeps it: when, which key, and the counters."
  (let ((data (snapshot-json snapshot)))
    (nlk:json-object "at" at "key" key-fingerprint
                      "today" (gethash "today" data)
                      "week" (gethash "week" data)
                      "month" (gethash "month" data)
                      "total" (gethash "total" data))))

(defun remember (store key-fingerprint snapshot)
  "STORE with this reading kept under KEY-FINGERPRINT and appended to the
history, the oldest readings dropped past the cap, and written out."
  (let* ((keys (or (object-member store "keys") (nlk:json-object)))
         (at (get-universal-time))
         (rows (append (readings store)
                       (list (history-row key-fingerprint snapshot at)))))
    (setf (gethash "version" store) +store-version+
          (gethash "keys" store) keys
          (gethash key-fingerprint keys)
          (nlk:json-object "fetched_at" at "data" (snapshot-json snapshot))
          (gethash "history" store)
          (coerce (if (> (length rows) +history-cap+)
                      (last rows +history-cap+)
                      rows)
                  'vector))
    (write-store store)
    store))

(defun remember-rate (store rate)
  "STORE with RATE kept as the last reference rate, written out."
  (setf (gethash "fx" store)
        (nlk:json-object "fetched_at" (get-universal-time)
                          "rate" (format nil "~,6f" (coerce (getf rate :rate) 'double-float))
                          "date" (or (getf rate :date) "")))
  (write-store store)
  store)

(defun stored-rate (store)
  "=> (values RATE-PLIST AT): the last reference rate kept, or NIL."
  (let ((entry (object-member store "fx")))
    (when (hash-table-p entry)
      (let ((rate (amount (text-member entry "rate"))))
        (when (and rate (plusp rate))
          (values (list :rate rate :date (text-member entry "date")
                        :source "Frankfurter" :stale nil)
                  (whole-seconds (object-member entry "fetched_at"))))))))

;;; --- a reading, and the rate it is shown in -------------------------------------

(defun key-reading (&key refresh)
  "=> (values SNAPSHOT STALE FINGERPRINT AT): OpenRouter's counters for the key
the section names. A reading younger than cache_seconds answers without a
dial; a dial that fails answers the last reading kept, marked STALE."
  (let* ((key (credential))
         (key-fingerprint (fingerprint key))
         (cached (gethash "key" *cache*))
         (ttl (setting :cache-seconds)))
    (if (and (not refresh) cached (equal key-fingerprint (first cached))
             (< (- (get-universal-time) (second cached)) ttl))
        (values (third cached) nil key-fingerprint (second cached))
        (handler-case
            (let ((snapshot (fetch-key key))
                  (at (get-universal-time)))
              (setf (gethash "key" *cache*) (list key-fingerprint at snapshot))
              (ignore-errors (remember (read-store) key-fingerprint snapshot))
              (values snapshot nil key-fingerprint at))
          (openrouter-spend-error (condition)
            (multiple-value-bind (stored at) (stored-reading (read-store) key-fingerprint)
              (if stored
                  (values stored t key-fingerprint at)
                  (error condition))))))))

(defun exchange-rate (&key refresh)
  "=> a plist (:RATE :DATE :SOURCE :STALE), the rate the INR line is shown
at. A rate in the section answers without a dial; otherwise the public
reference rate is asked for once per fx_cache_seconds, and the last one
kept answers while it cannot be reached."
  (let ((manual (amount (setting :usd-inr-rate))))
    (if (and manual (plusp manual) (<= manual 1000))
        (list :rate manual :date nil :source "manual" :stale nil)
        (let* ((cached (gethash "fx" *cache*))
               (ttl (setting :fx-cache-seconds)))
          (if (and (not refresh) cached
                   (< (- (get-universal-time) (first cached)) ttl))
              (second cached)
              (handler-case
                  (let ((rate (fetch-rate)))
                    (setf (gethash "fx" *cache*) (list (get-universal-time) rate))
                    (ignore-errors (remember-rate (read-store) rate))
                    rate)
                (openrouter-spend-error ()
                  (multiple-value-bind (stored at) (stored-rate (read-store))
                    (declare (ignore at))
                    (if stored
                        (list :rate (getf stored :rate) :date (getf stored :date)
                              :source (getf stored :source) :stale t)
                        (list :rate nil :date nil :source "unavailable" :stale t))))))))))

;;; --- the report -----------------------------------------------------------------

(defun period-entry (period)
  "PERIOD's row of +PERIODS+ (name, keyword, label), or a refusal."
  (or (assoc period +periods+ :test #'equal)
      (fail "~s is not a period OpenRouter reports: today, week, month or total" period)))

(defun comparison-line (month rate)
  "The month measured against the operator's own baseline, when they set
one: the original's one line about a subscription it replaces."
  (let ((baseline (amount (setting :comparison-usd)))
        (label (let ((value (ignore-errors (setting :comparison-label))))
                 (and (stringp value) (plusp (length (nlk:trimmed value)))
                      (nlk:trimmed value)))))
    (when (and baseline (plusp baseline) month)
      (let ((share (* 100.0 (/ month baseline))))
        (format nil "Month so far: ~a of a ~a ~a baseline (~,1f%); ~a~[ over it~:; left~]"
                (dual month rate) (money baseline) (or label "monthly") share
                (dual (abs (- baseline month)) rate)
                (if (> month baseline) 0 1))))))

(define-verb report (&key (period nil) refresh)
  "OpenRouter's reported spend for the key this section names, as the text the
openrouter_spend tool and /openrouter-spend show. PERIOD is \"today\",
\"week\", \"month\" or \"total\"; NIL is all four. REFRESH asks OpenRouter
again instead of answering from the cache."
  (let ((period (and period (string-downcase (nlk:trimmed period)))))
    (when (and period (zerop (length period))) (setf period nil))
    (let ((entry (and period (period-entry period))))
      (multiple-value-bind (snapshot stale key-fingerprint at) (key-reading :refresh refresh)
        (let* ((fx (exchange-rate :refresh refresh))
               (rate (getf fx :rate)))
          (with-output-to-string (out)
            (format out "OpenRouter API-key spend - USD / INR~%")
            (format out "Scope: every application using this API key.~%")
            (flet ((print-period (row)
                     (format out "~a: ~a~%" (third row)
                             (dual (getf snapshot (second row)) rate))))
              (if entry
                  (print-period entry)
                  (dolist (row +periods+) (print-period row))))
            (when (getf snapshot :remaining)
              (format out "Key budget remaining: ~a~@[ of ~a~]~%"
                      (dual (getf snapshot :remaining) rate)
                      (and (getf snapshot :limit) (money (getf snapshot :limit)))))
            (when (and (getf snapshot :byok) (plusp (getf snapshot :byok)))
              (format out "BYOK usage reported separately: ~a (not added to spend)~%"
                      (money (getf snapshot :byok))))
            (format out "~a: ~a~%"
                    (if stale "STALE - refresh failed; last successful fetch" "Fetched")
                    (utc at))
            (if rate
                (format out "INR reference: ~a per USD, ~a, ~a~a~%"
                        (rupees (coerce rate 'double-float))
                        (or (getf fx :date) "today")
                        (getf fx :source)
                        (if (getf fx :stale) " (STALE)" ""))
                (format out "INR reference: unavailable; spend is shown in USD only~%"))
            (when (and (not entry) (getf snapshot :month))
              (let ((line (comparison-line (getf snapshot :month) rate)))
                (when line (format out "~a~%" line))))
            (format out "Periods follow OpenRouter's counters; purchase fees are outside this report.~%")
            (format out "Key ~a.~%" key-fingerprint)
            (format out "([/openrouter-spend] today|week|month|total|refresh|ledger|status)~%")))))))

;;; --- the readings this home kept -----------------------------------------------

(defun mine (rows key-fingerprint)
  "ROWS read for KEY-FINGERPRINT, oldest first."
  (remove-if-not (lambda (row) (equal key-fingerprint (text-member row "key"))) rows))

(define-verb history ()
  "The readings this home kept for the key the section names, and the spend
between them, as the text /openrouter-spend ledger shows. A reading is kept
whenever this cell asks OpenRouter, so the difference between two of them is
what the key spent in between, other applications included."
  (let* ((key (credential))
         (key-fingerprint (fingerprint key))
         (rows (mine (readings (read-store)) key-fingerprint))
         (fx (exchange-rate))
         (rate (getf fx :rate)))
    (with-output-to-string (out)
      (format out "Local readings - ~d kept for the key ending ~a~%"
              (length rows) (subseq key-fingerprint (max 0 (- (length key-fingerprint) 6))))
      (cond
        ((null rows)
         (format out "No reading yet: the first is kept the next time this cell asks OpenRouter.~%"))
        (t
         (let* ((newest (car (last rows)))
                (oldest (first rows))
                (now (amount (text-member newest "total")))
                (was (amount (text-member oldest "total"))))
           (format out "Spanning ~a to ~a~%" (utc (object-member oldest "at")) (utc (object-member newest "at")))
           (format out "All time now: ~a~%" (dual now rate))
           (when (and now was)
             (format out "Since the first reading: ~a~%" (dual (- now was) rate)))
           (format out "Newest readings, oldest first:~%")
           (loop with previous = nil
                 for row in (if (> (length rows) 10) (last rows 10) rows)
                 for total = (amount (text-member row "total"))
                 do (format out "  ~a  all time ~a  today ~a  this month ~a~@[  (~a since the one before)~]~%"
                            (utc (object-member row "at"))
                            (money total)
                            (money (amount (text-member row "today")))
                            (money (amount (text-member row "month")))
                            (and total previous (dual (- total previous) rate)))
                    (setf previous total)))))
      (format out "A reading is kept each time this cell asks, at most once per cache window.~%")
      (format out "These are OpenRouter's totals for the whole key; other applications count too.~%"))))

(defun status-text ()
  "This cell's own settings and record, as the text /openrouter-spend status
shows. Reads no endpoint."
  (running-settings)
  (let* ((key (credential))
         (key-fingerprint (fingerprint key))
         (rows (mine (readings (read-store)) key-fingerprint))
         (newest (car (last rows)))
         (manual (amount (setting :usd-inr-rate))))
    (with-output-to-string (out)
      (format out "OpenRouter spend - this cell's settings~%")
      (format out "Key: ~a, fingerprint ~a~%" (setting :key-env) key-fingerprint)
      (format out "Readings: ~d kept in ~a~%" (length rows) (store-path))
      (when newest
        (format out "Last reading: ~a, all time ~a~%"
                (utc (object-member newest "at")) (money (amount (text-member newest "total")))))
      (format out "Cache: ~ds per reading; ~ds per reference rate; timeout ~ds~%"
              (setting :cache-seconds) (setting :fx-cache-seconds) (setting :timeout-seconds))
      (format out "INR: ~a~%"
              (if (and manual (plusp manual))
                  (format nil "a manual ~a per USD" (rupees (coerce manual 'double-float)))
                  "the public daily reference rate, fetched at most once per cache window"))
      (format out "Not a model call: one GET to ~a/key, and nothing leaves the image beside it.~%"
              +api+))))

;;; --- what the model and the operator read ---------------------------------------

(defparameter +primer+
  "OpenRouter spend is available through the nodecode-openrouter-spend cell: OpenRouter's own
accounting for the key this home uses, read from openrouter.ai/api/v1/key.
  (openrouter-spend:report &key period refresh)
      period is \"today\", \"week\", \"month\" or \"total\"; leave it out for all four.
      Answers what OpenRouter billed the key, in USD and in INR at a public daily reference
      rate, the key's budget remaining, and the month against the operator's own baseline.
      The counters cover every application using that key, not only this home.
      refresh asks OpenRouter again instead of answering from a cache of cache_seconds.
  (openrouter-spend:history)
      the readings this home kept, and what the key spent between two of them.
The /openrouter-spend command shows the same text, with ledger and status besides.
It never calls a model and never reads prompts or keys. ERROR: OPENROUTER-SPEND-ERROR
names the problem; when it says a variable is not set, tell the operator to export the key."
  "What (help :openrouter-spend) answers while the cell runs.")

(defparameter +help+
  "/openrouter-spend - OpenRouter's own reported key spend (USD)
openrouter-spend today|week|month|total - the provider's reporting periods
openrouter-spend refresh - bypass the cache_seconds default cache
openrouter-spend ledger - the readings kept in this home
openrouter-spend status - this cell's key, record and cache settings
The key's totals include every application using that key. The local record starts
when this cell first asks. No model calls. No prompts, outputs or API keys are saved."
  "The /openrouter-spend help text.")

(nlk:define-contract spend-query
  (:doc "What the operator's OpenRouter key has spent, as OpenRouter itself reports it, in USD and in INR at a public daily reference rate. One read-only GET to openrouter.ai/api/v1/key with the key the cell's section names; no model call. The totals cover every application using that key, not only this home.")
  ("period" :text :options '("today" "week" "month" "total")
            :doc "which of OpenRouter's reporting periods to answer; leave it out for all four")
  ("refresh" :boolean
             :doc "ask OpenRouter again instead of answering from the cache (cache_seconds); use it when the operator says the numbers moved"))

(defun guarded (body)
  "Run BODY, answering its text; a refusal or a dial that failed becomes the
tool answer ERROR: ..., so the model reads what went wrong."
  (handler-case (funcall body)
    (openrouter-spend-error (condition)
      (nle:failure (format nil "ERROR: OPENROUTER-SPEND-ERROR: ~a" condition)))
    (error (condition)
      (nle:failure (format nil "ERROR: OPENROUTER-SPEND-ERROR: ~a" condition)))))

(defun spend-tool (call)
  "The openrouter_spend tool: the contract's call, answered as the report text."
  (guarded (lambda ()
             (report :period (spend-query-period call)
                     :refresh (spend-query-refresh call)))))

(defun run-command (args session-id)
  "The /openrouter-spend command: no argument is the whole report, a period is
that one line, refresh asks again, ledger is the local record, status is this
cell's settings, help is the words above."
  (declare (ignore session-id))
  (let ((words (and args (remove "" (uiop:split-string (nlk:trimmed args)
                                                        :separator '(#\Space #\Tab))))))
    (guarded
     (lambda ()
       (let ((word (and words (string-downcase (first words)))))
         (cond ((null word) (report))
               ((member word '("help" "?") :test #'equal) +help+)
               ((equal word "refresh") (report :refresh t))
               ((member word '("ledger" "history") :test #'equal) (history))
               ((member word '("status" "config") :test #'equal) (status-text))
               ((assoc word +periods+ :test #'equal) (report :period word))
               (t (format nil "Error: unknown /openrouter-spend option ~s~%~a" (first words) +help+))))))))

;;; --- the section, and the cell -------------------------------------------------

(defun read-section (values table)
  "The section's values, key_env defaulted to OPENROUTER_API_KEY, refusing at
start when that variable holds no key: without one there is nothing to read."
  (declare (ignore table))
  (let* ((given (getf values :key-env))
         (name (if (and (stringp given) (plusp (length (nlk:trimmed given))))
                   (nlk:trimmed given)
                   "OPENROUTER_API_KEY")))
    (setf (getf values :key-env) name)
    (let ((key (uiop:getenv name)))
      (unless (and key (plusp (length (nlk:trimmed key))))
        (nlk:config-error "openrouter-spend: ~a is not set. Export the OpenRouter key in it, or set key_env to the variable that holds it" name)))
    values))

(defun start ()
  (clrhash *cache*)
  (nle:on-stop (lambda () (clrhash *cache*))))

(nle:define-cell openrouter-spend
  (:section ("openrouter-spend")
    (:guide "export the OpenRouter key in OPENROUTER_API_KEY, or name your variable in key_env; usd_inr_rate left empty reads a public daily USD/INR reference rate; /openrouter-spend prints the report")
    ("key_env" :env :default "OPENROUTER_API_KEY"
     :doc "the variable holding the OpenRouter key, never the key itself")
    ("cache_seconds" :integer :default 30 :min 0
     :doc "how long one reading answers before OpenRouter is asked again; 300 is a sensible ceiling, 0 asks every time")
    ("timeout_seconds" :integer :default 8 :min 1
     :doc "the ceiling on one read-only accounting request; 30 is a generous one")
    ("usd_inr_rate" :string :default ""
     :doc "a manual INR per USD rate; empty reads the public daily reference rate")
    ("fx_cache_seconds" :integer :default 43200 :min 300
     :doc "how long the public reference rate is reused before it is read again; a day is 86400")
    ("comparison_usd" :string :default "110"
     :doc "a monthly USD baseline the month is measured against; the default is an illustrative personal one")
    ("comparison_label" :string :default "Codex"
     :doc "what that baseline is called in the one line about it")
    ("history_file" :string :default ""
     :doc "where the readings are kept; empty is this home's openrouter-spend/spend.json"))
  (:settings #'read-section)
  (:tool "openrouter_spend" (nlk:contract 'spend-query) 'spend-tool)
  (:command "openrouter-spend" 'run-command
            :description "OpenRouter-reported key spend, in USD and INR"
            :argument-hint "[today|week|month|total|refresh|ledger|status]")
  (:help :openrouter-spend
         "openrouter-spend: OpenRouter's own reported key spend, in USD and INR - the openrouter_spend tool, /openrouter-spend"
         +primer+)
  (:start #'start))
