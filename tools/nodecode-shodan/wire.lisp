;;;; wire.lisp --- the HTTP layer: one seam, one key, one ledger, one cache.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through HTTP, so a test stubs NLK:HTTP
;;;; and never touches the network. Shodan answers every status with a body,
;;;; so a refusal is a value the caller formats, not an exception: only a
;;;; dial that fails signals, and ANSWERING turns that into the call's text.
;;;;
;;;; Shodan wants its key as a query-string parameter. It is added here, in
;;;; one place, and never logged or cached: a cached entry is keyed by the
;;;; URL without it.

(in-package #:nodecode-shodan)

(defparameter +rest+ "https://api.shodan.io"
  "The main REST API: hosts, search, count, DNS, account, scans, alerts.")

(defparameter +exploits+ "https://exploits.shodan.io/api"
  "The exploit search API.")

(defparameter +trends+ "https://trends.shodan.io"
  "Monthly match counts for a query.")

(defparameter +internetdb+ "https://internetdb.shodan.io"
  "The free keyless host dataset.")

(defparameter +cvedb+ "https://cvedb.shodan.io"
  "The free keyless vulnerability dataset.")

(defparameter +keyless+ (list +internetdb+ +cvedb+)
  "The bases that take no auth at all. They are what makes a keyless install useful.")

(defparameter +retry-statuses+ '(429 500 502 503 504)
  "Statuses asked again after a paced wait. A 401 or a 402 is not retried: it will not change.")

(defparameter +user-agent+ "nodecode-shodan/0.1.0"
  "A plain product token. Some Shodan paths sit behind a challenge that a
default library user agent never clears.")

;;; --- the ledger ---------------------------------------------------------------

(defstruct ledger
  (queries 0 :type integer)
  (scans 0 :type integer)
  (calls 0 :type integer)
  (hits 0 :type integer)
  (cost 0 :type integer))

(defvar *ledger* nil
  "This session's credit ledger, made at start and reset when the conversation changes.")

(defvar *session* nil
  "The session the ledger belongs to, so a second conversation starts at zero.")

(defvar *cache* nil
  "URL -> (TEXT . EXPIRY) for the GETs Shodan answers the same way twice.")

(defvar *last-call* 0
  "Internal-real-time of the last request out, for the 1 request/second pace.")

;;; --- the small helpers ---------------------------------------------------------

(defun pct-encode (text)
  "TEXT percent-encoded for a query string, as UTF-8 octets."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (princ-to-string text) :external-format :utf-8)
          do (if (or (<= 48 octet 57) (<= 65 octet 90) (<= 97 octet 122)
                     (member octet '(45 46 95 126)))
                 (write-char (code-char octet) out)
                 (format out "%~2,'0X" octet)))))

(defun query-string (params)
  "PARAMS, an alist of (NAME . VALUE) with string names, as one query string."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in params
                when (and value (not (equal value "")))
                  collect (format nil "~a=~a" (pct-encode name) (pct-encode value)))))

(defun whole-url (base path params)
  "BASE and PATH with PARAMS appended, absolutely."
  (let ((query (query-string params)))
    (if (plusp (length query))
        (format nil "~a~a~:[~;?~]~a" base path (plusp (length query)) query)
        (format nil "~a~a" base path))))

(defun api-key ()
  "The key this cell runs on, or NIL. Read once at start out of the section,
which took it from the environment variable api_key_env or the literal
api_key."
  (setting :api-key))

(defun need-key (what)
  "The key, or a refusal naming how to give one."
  (or (api-key)
      (fail "no Shodan API key configured, and ~a needs one. Set the environment variable named by api_key_env (SHODAN_API_KEY by default) and (restart-cells \"nodecode-shodan\"), then run it again. Without a key, shodan_host answers from InternetDB and shodan_cve works in full." what)))

;;; --- pacing, cache, ledger -------------------------------------------------------

(defun pace ()
  "Wait until the configured requests-per-second pace allows one more."
  (let* ((rate (setting :rate-limit))
         (interval (if (and rate (plusp rate)) (/ 1.0 rate) 0.0)))
    (when (plusp interval)
      (let ((wait (- (+ *last-call* interval) (get-internal-real-time))))
        (when (plusp wait) (sleep (/ wait internal-time-units-per-second)))))
    (setf *last-call* (get-internal-real-time))))

(defun cached (url)
  "The cached body for URL while it is fresh, else NIL."
  (let ((entry (and *cache* (gethash url *cache*))))
    (when (and entry (< (get-universal-time) (cdr entry)))
      (incf (ledger-hits *ledger*))
      (car entry))))

(defun keep (url text)
  "Remember TEXT for URL for as long as the section says."
  (let ((ttl (setting :cache-ttl)))
    (when (and *cache* ttl (plusp ttl))
      (when (> (hash-table-count *cache*) 512) (clrhash *cache*))
      (setf (gethash url *cache*) (cons text (+ (get-universal-time) ttl))))))

(defun exchange (method url &key content)
  "One HTTP exchange with the pace, the retries and the count. => (values TEXT STATUS)."
  (let ((attempt 0))
    (loop
      (pace)
      (incf (ledger-calls *ledger*))
      (multiple-value-bind (body status headers)
          (nlk:http method url
                    :headers (append (list (cons "accept" "application/json")
                                           (cons "user-agent" +user-agent+))
                                     (and content (list (cons "content-type" "application/json"))))
                    :content content
                    :timeout (setting :timeout)
                    :connect-timeout (min 10 (setting :timeout)))
        (declare (ignore headers))
        (let ((status (if (integerp status) status 0)))
          (if (and (member status +retry-statuses+) (< attempt (setting :retries)))
              (progn (incf attempt)
                     (sleep (+ (expt 2 attempt) (/ (random 40) 100.0))))
              (return (values (if (stringp body) body (nlk:body-text body)) status))))))))

(defun refusal-text (status json text)
  "What to tell the model about a failed STATUS, in Shodan's own words when it
gave any."
  (let ((said (and (hash-table-p json) (let ((e (gethash "error" json)))
                                          (and (stringp e) (nlk:trimmed e)))))
        (why (case status
               (401 "the API key was refused: check it under Account, and the variable api_key_env names")
               (402 "the account has run out of credits for this endpoint")
               (403 "the plan does not include this endpoint")
               (404 "no record")
               (429 "too many requests: Shodan asked to slow down, so wait a moment and ask again")
               (t nil))))
    (format nil "HTTP ~a~@[: ~a~]~@[ (~a)~]~@[. ~a~]"
            status why said
            (and (null said) (plusp (length (nlk:trimmed (or text ""))))
                 (nlk:clip (nlk:one-line text) 200)))))

;;; --- the one request form ----------------------------------------------------------

(defun get-json (path &key (base +rest+) params cacheable (key t))
  "One GET, decoded. => (values JSON STATUS). JSON is NIL when the body is not
JSON. A keyless BASE takes no key; every other one does, and a missing key
refuses here."
  (let* ((params (if (and key (not (member base +keyless+ :test #'string=)))
                     (cons (cons "key" (need-key path)) (copy-list params))
                     (copy-list params)))
         (url (whole-url base path (remove "key" params :key #'car :test #'string=)))
         (hit (and cacheable (cached url))))
    (if hit
        (values (ignore-errors (nlk:decode-json hit)) 200)
        (multiple-value-bind (text status)
            (exchange :get (whole-url base path params))
          (when (and cacheable (integerp status) (< status 300))
            (keep url text))
          (values (and (plusp (length (or text ""))) (ignore-errors (nlk:decode-json text)))
                  status)))))

(defun send-json (method path &key (base +rest+) json params (key t))
  "One write, with JSON BODY. => (values JSON STATUS)."
  (let* ((params (if (and key (not (member base +keyless+ :test #'string=)))
                     (cons (cons "key" (need-key path)) (copy-list params))
                     (copy-list params)))
         (url (whole-url base path params)))
    (multiple-value-bind (text status)
        (exchange method url :content (and json (nlk:encode-json-object json)))
      (values (and (plusp (length (or text ""))) (ignore-errors (nlk:decode-json text)))
              status))))

(defun answer (json status path)
  "JSON, or a refusal built from STATUS and its body."
  (if (and (integerp status) (< status 300) (hash-table-p json))
      json
      (fail "Shodan ~a answered ~a." path (refusal-text status json nil))))

;;; --- the budget -------------------------------------------------------------------

(defun spend (credits kind)
  "Check the session budget, then record. KIND is :query or :scan."
  (let* ((limit (setting (if (eq kind :scan) :scan-credits :query-credits)))
         (spent (if (eq kind :scan) (ledger-scans *ledger*) (ledger-queries *ledger*)))
         (left (max 0 (- limit spent))))
    (when (> credits left)
      (fail "the session budget covers ~d more ~a credit~:p and this call needs ~d of ~d. ~a"
            left (string-downcase (string kind)) credits limit
            (if (eq kind :scan)
                "Raise scan_credits in the shodan section, or scan fewer addresses."
                "shodan_count and shodan_host are free and answer most questions; try one of them.")))
    (setf (ledger-cost *ledger*) credits)
    (if (eq kind :scan)
        (incf (ledger-scans *ledger*) credits)
        (incf (ledger-queries *ledger*) credits))))

(defun ledger-object ()
  "The ledger as the JSON block every credit-costing answer carries."
  (let ((query-limit (setting :query-credits))
        (scan-limit (setting :scan-credits)))
    (nlk:json-object "spent_by_this_call" (ledger-cost *ledger*)
                      "query_credits_spent" (ledger-queries *ledger*)
                      "query_credits_budget" query-limit
                      "query_credits_remaining" (max 0 (- query-limit (ledger-queries *ledger*)))
                      "scan_credits_spent" (ledger-scans *ledger*)
                      "scan_credits_budget" scan-limit
                      "scan_credits_remaining" (max 0 (- scan-limit (ledger-scans *ledger*)))
                      "api_calls" (ledger-calls *ledger*)
                      "credit_saving_cache_hits" (ledger-hits *ledger*))))

(defun reset-ledger (&optional session)
  "Start again at zero: a new conversation, or a fresh start."
  (setf *ledger* (make-ledger) *cache* (make-hash-table :test #'equal) *session* session))
