;;;; wire.lisp --- Polymarket's two public, keyless endpoints and the market shape.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Gamma answers market metadata, CLOB answers price history. Both are
;;;; public: no key, no wallet, no account, and nothing here ever places an
;;;; order. The allowlist below is the only host this cell will contact, and
;;;; it is checked before every request, so a caller that passes a whole URL
;;;; cannot walk the cell off it.

(in-package #:nodecode-polymarket)

;;; --- the two hosts ---------------------------------------------------------------

(defparameter +gamma+ "https://gamma-api.polymarket.com"
  "Market metadata: lists, search, one market.")

(defparameter +clob+ "https://clob.polymarket.com"
  "Price history.")

(defparameter +user-agent+ "nodecode-polymarket/0.1"
  "What this cell calls itself on the wire.")

(defparameter *cache* (make-hash-table :test #'equal)
  "URL -> (UNIVERSAL-TIME . DATA), so one briefing does not fetch a market twice.")

(defparameter *cache-seconds* 30
  "How long a response is reused; the CLOB history is held longer.")

;;; --- little readers ---------------------------------------------------------------

(defun url-encode (text)
  "TEXT as a query parameter's value: everything but an unreserved byte escaped."
  (with-output-to-string (out)
    (loop for byte across (sb-ext:string-to-octets (princ-to-string text) :external-format :utf-8)
          do (if (or (<= (char-code #\a) byte (char-code #\z))
                     (<= (char-code #\A) byte (char-code #\Z))
                     (<= (char-code #\0) byte (char-code #\9))
                     (find byte "-._~" :key #'char-code))
                 (write-char (code-char byte) out)
                 (format out "%~2,'0X" byte)))))

(defun query-string (params)
  "PARAMS -- an alist of (KEY . VALUE) -- as ?a=b&c=d, or the empty string."
  (let ((pairs (loop for (key . value) in params
                     when (and value (not (equal value "")))
                       collect (format nil "~a=~a" key (url-encode value)))))
    (if pairs (format nil "?~{~a~^&~}" pairs) "")))

(defun json-list (value)
  "VALUE as a list: Gamma spells some arrays as JSON-encoded strings."
  (cond ((and (vectorp value) (not (stringp value))) (coerce value 'list))
        ((and (stringp value) (plusp (length (nlk:trimmed value))))
         (let ((parsed (ignore-errors (nlk:decode-json value))))
           (if (and (vectorp parsed) (not (stringp parsed))) (coerce parsed 'list) '())))
        (t '())))

(defun as-number (value)
  "VALUE as a real, or NIL: a boolean and null are not numbers, and a numeric
string -- Gamma spells some prices that way -- is one."
  (cond ((realp value) (float value 1d0))
        ((and (stringp value) (plusp (length (nlk:trimmed value))))
         (let ((*read-eval* nil))
           (multiple-value-bind (parsed end) (read-from-string (nlk:trimmed value) nil nil)
             (and (realp parsed) (= end (length (nlk:trimmed value))) (float parsed 1d0)))))
        (t nil)))

(defun text (value)
  "VALUE as a trimmed string, or NIL when it says nothing."
  (when (stringp value)
    (let ((trimmed (nlk:trimmed value)))
      (and (plusp (length trimmed)) trimmed))))

(defun parse-time (value)
  "VALUE -- an ISO 8601 instant -- as universal time, or NIL."
  (when (and (stringp value) (>= (length value) 19))
    (ignore-errors
     (labels ((at (start end) (parse-integer value :start start :end end :junk-allowed nil)))
       (let* ((year (at 0 4)) (month (at 5 7)) (day (at 8 10))
              (hour (at 11 13)) (minute (at 14 16)) (second (at 17 19))
              (rest (subseq value 19))
              (offset 0))
         (when (plusp (length rest))
           (let ((sign (char rest 0)))
             (cond ((char= sign #\Z))
                   ((and (member sign '(#\+ #\-)) (>= (length rest) 6))
                    (let ((minutes (+ (* 60 (parse-integer rest :start 1 :end 3))
                                      (parse-integer rest :start 4 :end 6))))
                      (setf offset (* (if (char= sign #\-) -60 60) minutes))))
                   (t (return-from parse-time nil)))))
         (encode-universal-time second minute hour day month year (- offset)))))))

;;; --- one request ------------------------------------------------------------------

(defun iso-time (universal)
  "UNIVERSAL time as the ISO 8601 instant Gamma's end_date bounds take."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun host-of (url)
  "URL's host, or NIL."
  (let* ((start (cl:search "://" url))
         (rest (if start (+ start 3) 0))
         (end (or (position-if (lambda (c) (or (char= c #\/) (char= c #\?) (char= c #\:))) url
                               :start rest)
                  (length url))))
    (subseq url rest end)))

(defun allowed-url-p (url)
  "Whether URL is on one of the two hosts this cell may call."
  (and (or (uiop:string-prefix-p +gamma+ url) (uiop:string-prefix-p +clob+ url))
       (member (host-of url) '("gamma-api.polymarket.com" "clob.polymarket.com")
               :test #'string-equal)))

(defun get-json (url &key (timeout (setting :timeout-seconds)) (fresh nil))
  "URL's JSON, from the cache when it is recent. A refusal names what failed."
  (unless (allowed-url-p url)
    (fail "~a is not a Polymarket host this cell may call" url))
  (let ((now (get-universal-time)))
    (with-polymarket-lock
      (let ((hit (and (not fresh) (gethash url *cache*))))
        (when (and hit (< (- now (car hit)) *cache-seconds*))
          (return-from get-json (cdr hit)))))
    (let ((answer
            (loop for attempt from 1 to 2
                  do (handler-case
                         (multiple-value-bind (body status) (call-once url timeout)
                           (cond ((= status 200) (return (nlk:decode-json body)))
                                 ((and (= attempt 1) (member status '(429 500 502 503 504)))
                                  (sleep 0.5))
                                 (t (fail "Polymarket answered HTTP ~d" status))))
                       (polymarket-error (condition) (error condition))
                       (error (condition)
                         (if (= attempt 1)
                             (sleep 0.3)
                             (fail "could not reach Polymarket (~a)" condition)))))))
      (with-polymarket-lock
        (setf (gethash url *cache*) (cons now answer))
        (when (> (hash-table-count *cache*) 64)
          (clrhash *cache*)
          (setf (gethash url *cache*) (cons now answer))))
      answer)))

(defun call-once (url timeout)
  "One GET of URL => (values BODY STATUS). The timeout is the section's."
  (nlk:http :get url
            :headers (list (cons "Accept" "application/json")
                           (cons "User-Agent" +user-agent+))
            :timeout timeout))

(defun forget-cache ()
  "Drop every cached response."
  (with-polymarket-lock (clrhash *cache*)))

;;; --- the market shape ---------------------------------------------------------------

(defun normalize (raw &key event now)
  "RAW -- one Gamma market -- as the plist the analysis reads, or NIL."
  (when (and (hash-table-p raw) (text (nlk:json-value raw :string "question")))
    (let* ((now (or now (get-universal-time)))
           (outcomes (mapcar (lambda (o) (if (stringp o) o (princ-to-string o)))
                             (json-list (nlk:json-value raw :any "outcomes"))))
           (prices (mapcar #'as-number (json-list (nlk:json-value raw :any "outcomePrices"))))
           (tokens (mapcar (lambda (t0) (if (stringp t0) t0 (princ-to-string t0)))
                           (json-list (nlk:json-value raw :any "clobTokenIds"))))
           (yes-index (or (position "yes" outcomes :test #'string-equal) 0))
           (yes (or (nth yes-index prices) (as-number (nlk:json-value raw :any "lastTradePrice"))))
           (event (or event
                      (let ((events (nlk:json-value raw :array "events")))
                        (and events (plusp (length events))
                             (json-value-object (aref events 0))))))
           (slug (or (text (nlk:json-value raw :string "slug")) ""))
           (event-slug (and event (text (nlk:json-value event :string "slug"))))
           (end-text (or (text (nlk:json-value raw :string "endDate"))
                         (text (nlk:json-value raw :string "endDateIso"))))
           (end (parse-time end-text))
           (volume-24h (or (as-number (nlk:json-value raw :any "volume24hr"))
                           (as-number (nlk:json-value raw :any "volume24hrClob"))
                           0d0)))
      (list :id (or (text (nlk:json-value raw :string "id")) "")
            :slug slug
            :question (nlk:json-value raw :string "question")
            :url (cond (event-slug (format nil "https://polymarket.com/event/~a" event-slug))
                       ((plusp (length slug)) (format nil "https://polymarket.com/market/~a" slug))
                       (t ""))
            :outcomes outcomes
            :prices prices
            :tokens tokens
            :yes-index yes-index
            :yes yes
            :binary (= 2 (length outcomes))
            :volume-24h volume-24h
            :volume-1w (as-number (nlk:json-value raw :any "volume1wk"))
            :volume-total (or (as-number (nlk:json-value raw :any "volumeNum"))
                              (as-number (nlk:json-value raw :any "volume")) 0d0)
            :liquidity (or (as-number (nlk:json-value raw :any "liquidityNum"))
                           (as-number (nlk:json-value raw :any "liquidity")) 0d0)
            :change-1d (as-number (nlk:json-value raw :any "oneDayPriceChange"))
            :change-1w (as-number (nlk:json-value raw :any "oneWeekPriceChange"))
            :bid (as-number (nlk:json-value raw :any "bestBid"))
            :ask (as-number (nlk:json-value raw :any "bestAsk"))
            :spread (as-number (nlk:json-value raw :any "spread"))
            :end end-text
            :hours (and end (/ (- end now) 3600.0))
            :closed (and (nlk:json-value raw :boolean "closed") t)
            :active (multiple-value-bind (value present)
                         (nlk:json-value raw :any "active")
                       (if present (and value t) t))))))

(defun json-value-object (value)
  "VALUE when it is a decoded object."
  (and (hash-table-p value) value))

(defun open-market-p (market)
  "Whether MARKET is still tradable: not closed, and not marked inactive."
  (and market (not (getf market :closed)) (getf market :active)))

(defun normalize-all (rows &key now)
  "ROWS as normalized, still-open markets."
  (loop for raw across (if (vectorp rows) rows #())
        for market = (normalize raw :now now)
        when (open-market-p market) collect market))

;;; --- the three endpoints ------------------------------------------------------------

(defun list-markets (&key (order "volume24hr") (ascending nil) (limit 100)
                          volume-num-min end-date-min end-date-max (now nil))
  "The open markets Gamma lists, in ORDER, normalized."
  (let ((url (format nil "~a/markets~a" +gamma+
                     (query-string
                      (list (cons "closed" "false")
                            (cons "order" order)
                            (cons "ascending" (if ascending "true" "false"))
                            (cons "limit" (max 1 (min (or limit 100) 100)))
                            (cons "volume_num_min" volume-num-min)
                            (cons "end_date_min" end-date-min)
                            (cons "end_date_max" end-date-max))))))
    (normalize-all (get-json url) :now (or now (get-universal-time)))))

(defun search-markets (query &key (limit 10) (now nil))
  "The open markets matching QUERY, busiest first."
  (let* ((url (format nil "~a/public-search~a" +gamma+
                       (query-string (list (cons "q" query)
                                           (cons "limit_per_type" (max 1 (min (or limit 10) 20)))))))
         (data (get-json url))
         (events (and (hash-table-p data) (nlk:json-value data :array "events")))
         (out '())
         (seen (make-hash-table :test #'equal)))
    (loop for event across (or events #())
          when (hash-table-p event)
            do (loop for raw across (or (nlk:json-value event :array "markets") #())
                     for market = (normalize raw :event event :now now)
                     when (and (open-market-p market)
                               (plusp (length (getf market :slug)))
                               (not (gethash (getf market :slug) seen)))
                       do (setf (gethash (getf market :slug) seen) t)
                          (push market out)))
    (let ((sorted (sort out #'> :key (lambda (m) (getf m :volume-24h)))))
      (if (plusp (length sorted)) (subseq sorted 0 (min (length sorted) (max 1 limit))) '()))))

(defun get-market (ref &key (now nil))
  "The market REF names (slug, id, or a polymarket.com link), or NIL."
  (let* ((ref (nlk:trimmed (or ref "")))
         (ref (if (uiop:string-prefix-p "http" ref)
                  (let* ((path (subseq ref (or (cl:search "/" ref :start2 8) (length ref))))
                         (trimmed (string-right-trim "/" path)))
                    (subseq trimmed (1+ (or (position #\/ trimmed :from-end t) -1))))
                  ref)))
    (when (plusp (length ref))
      (let* ((numeric (every #'digit-char-p ref))
             (url (format nil "~a/markets~a" +gamma+
                          (query-string (list (cons (if numeric "id" "slug") ref)))))
             (data (get-json url)))
        (when (and (vectorp data) (plusp (length data)))
          (normalize (aref data 0) :now now))))))

(defun price-history (token &key (interval "1w") (fidelity 60))
  "TOKEN's (TIMESTAMP . PRICE) series from CLOB."
  (let* ((url (format nil "~a/prices-history~a" +clob+
                       (query-string (list (cons "market" token)
                                           (cons "interval" interval)
                                           (cons "fidelity" fidelity)))))
         (data (get-json url :fresh t))
         (rows (and (hash-table-p data) (nlk:json-value data :array "history"))))
    (loop for row across (or rows #())
          for t0 = (and (hash-table-p row) (nlk:json-value row :integer "t"))
          for p = (and (hash-table-p row) (as-number (nlk:json-value row :any "p")))
          when (and t0 p) collect (cons t0 p))))
