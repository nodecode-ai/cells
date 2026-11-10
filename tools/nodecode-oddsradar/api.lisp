;;;; api.lisp --- Polymarket's two public APIs: the hosts, the one network seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Read-only and keyless, as the original's client was: gamma-api.polymarket.com
;;;; for search, markets and events, clob.polymarket.com for live midpoints and
;;;; price history. The original's host allowlist is kept here, so a request to
;;;; anything but those two is refused before it is made. Every call has a
;;;; timeout (the section's timeout_seconds) and no retry: a slow or unreachable
;;;; API refuses in words the model can act on instead of hanging the turn.

(in-package #:nodecode-oddsradar)

(defparameter +gamma+ "https://gamma-api.polymarket.com"
  "The metadata host: search, markets, events.")

(defparameter +clob+ "https://clob.polymarket.com"
  "The order-book host: live midpoints and price history.")

(defparameter +hosts+ (list +gamma+ +clob+)
  "The only two hosts this cell ever contacts.")

(defparameter +user-agent+ "nodecode-oddsradar/0.1.0 (+https://github.com/nodecode-ai/cells)"
  "Who the API sees asking: the cell, and nothing about the operator.")

(defparameter +max-response-bytes+ (* 2 1024 1024)
  "The most one answer may be: a page of markets, and room to spare.")

;;; --- json, read leniently ------------------------------------------------------

(defun json-field (object key)
  "OBJECT's member KEY when OBJECT is a JSON object, else NIL."
  (and (hash-table-p object) (gethash key object)))

(defun jtext (object key &optional default)
  "OBJECT's member KEY as text: DEFAULT when it is absent, null or not text."
  (let ((value (json-field object key)))
    (if (stringp value) value default)))

(defun val->number (value)
  "VALUE as a number, or NIL. Gamma writes prices as numbers and sometimes
as their text, so both read."
  (cond ((numberp value) (float value 1d0))
        ((and (stringp value) (plusp (length (nlk:trimmed value))))
         (let ((*read-default-float-format* 'double-float)
               (*read-eval* nil))
           (multiple-value-bind (number end)
               (ignore-errors (read-from-string (nlk:trimmed value)))
             (and (numberp number) end number))))
        (t nil)))

(defun jnum (object key)
  "OBJECT's member KEY as a number, or NIL."
  (val->number (json-field object key)))

(defun jlist (value)
  "VALUE as a list: a JSON array, or the JSON text Gamma double-encodes an
array as. Anything else is no list at all."
  (cond ((stringp value)
         (if (and (plusp (length value)) (char= (char value 0) #\[))
             (let ((decoded (ignore-errors (nlk:decode-json value))))
               (if (vectorp decoded) (coerce decoded 'list) nil))
             nil))
        ((vectorp value) (coerce value 'list))
        ((consp value) value)
        (t nil)))

(defun truthful-p (value)
  "VALUE as a JSON boolean: false, null and absent are all NIL."
  (and value (not (eq value :false)) t))

;;; --- one request ---------------------------------------------------------------

(defun param-text (value)
  "VALUE as the text of one query parameter."
  (if (stringp value) value (princ-to-string value)))

(defun query-string (params)
  "PARAMS, an alist of (name . value), as an encoded query string. A NIL
value is left out, so an absent bound is an absent parameter."
  (format nil "~{~a=~a~^&~}"
          (loop for (name . value) in params
                unless (null value)
                  append (list (quri:url-encode (string name))
                               (quri:url-encode (param-text value) :encoding :utf-8)))))

(defun http-json (base path params timeout)
  "The cell's one network seam: GET BASE PATH with PARAMS, the body decoded
as JSON. BASE is one of the two allowed hosts, and anything else refuses
here. Tests stub this name, so no test opens a socket."
  (unless (member base +hosts+ :test #'equal)
    (fail "refusing to contact ~a: this cell reads gamma-api and clob only" base))
  (let* ((query (query-string params))
         (url (format nil "~a~a~@[?~a~]" base path query))
         (answer (nlk:http :get url
                           :headers `(("user-agent" . ,+user-agent+)
                                      ("accept" . "application/json"))
                           :timeout timeout
                           :max-bytes +max-response-bytes+)))
    (multiple-value-bind (body status) answer
      (unless (eql status 200)
        (fail "~a answered HTTP ~a" base status))
      (let ((json (ignore-errors
                    (nlk:decode-json (if (stringp body) body (nlk:decode-octets body))))))
        (unless (or (hash-table-p json) (vectorp json) (consp json))
          (fail "unreadable answer from ~a" base))
        json))))

;;; --- the endpoints -------------------------------------------------------------

(defun first-of (json)
  "The first element of JSON when it is an array, else NIL."
  (and (vectorp json) (plusp (length json)) (aref json 0)))

(defun search-events (query limit timeout)
  "The active events Gamma's public search answers for QUERY."
  (let* ((json (http-json +gamma+ "/public-search"
                           `(("q" . ,query)
                             ("limit_per_type" . ,limit)
                             ("events_status" . "active")
                             ("keep_closed_markets" . 0))
                           timeout))
         (events (and (hash-table-p json) (gethash "events" json))))
    (if (vectorp events) (coerce events 'list) nil)))

(defun market-by-slug (slug timeout)
  "Gamma's market record for SLUG, or NIL."
  (first-of (http-json +gamma+ "/markets" `(("slug" . ,slug)) timeout)))

(defun event-by-slug (slug timeout)
  "Gamma's event record for SLUG, or NIL."
  (first-of (http-json +gamma+ "/events" `(("slug" . ,slug)) timeout)))

(defun top-markets (limit timeout)
  "The most-traded active markets, as Gamma answers them."
  (let ((json (http-json +gamma+ "/markets"
                          `(("active" . "true") ("closed" . "false")
                            ("order" . "volume24hr") ("ascending" . "false")
                            ("limit" . ,limit))
                          timeout)))
    (if (vectorp json) (coerce json 'list) nil)))

(defun midpoint (token-id timeout)
  "The live CLOB midpoint for TOKEN-ID, or NIL when it is not a price."
  (let* ((json (http-json +clob+ "/midpoint" `(("token_id" . ,token-id)) timeout))
         (mid (and (hash-table-p json) (gethash "mid" json) (val->number (gethash "mid" json)))))
    (and mid (<= 0.0 mid 1.0) mid)))

(defun price-history (token-id timeout &key (interval "1w") (fidelity 180))
  "The price points of TOKEN-ID over INTERVAL, oldest first, as (values PRICES)."
  (let* ((json (http-json +clob+ "/prices-history"
                           `(("market" . ,token-id) ("interval" . ,interval)
                             ("fidelity" . ,fidelity))
                           timeout))
         (history (and (hash-table-p json) (gethash "history" json)))
         (points (if (vectorp history) (coerce history 'list) nil)))
    (loop for point in points
          for price = (and (hash-table-p point) (val->number (gethash "p" point)))
          when price collect (float price 1d0))))
