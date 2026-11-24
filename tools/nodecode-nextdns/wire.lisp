;;;; wire.lisp --- one HTTP seam, one key, one query string.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through HTTP, so a test stubs NLK:HTTP
;;;; and never touches the network. NextDNS answers a refusal with a JSON
;;;; `errors` array carrying the detail a caller can act on ("`limit` must be
;;;; >= 10"), so a refusal is a value the caller formats and not an exception:
;;;; only a dial that fails signals, and ASK turns that into the call's text.
;;;;
;;;; The key rides in an X-Api-Key header, written here and nowhere else, and
;;;; is never put in a URL, a log line or an answer.

(in-package #:nodecode-nextdns)

(defparameter +base+ "https://api.nextdns.io"
  "The one host this cell reads, and the furthest anything leaves the machine.")

(defparameter +max-response-bytes+ (* 2 1024 1024)
  "An answer is a few kilobytes; past this the body is cut, and the JSON read
fails plainly rather than half-reading a shape nobody has seen.")

(defparameter +limit-floor+ 10
  "The API refuses a limit under ten, so the cell raises small values instead of
surfacing a 400 for a number the caller did not have to get right.")

(defparameter +log-limit-ceiling+ 1000
  "The most rows the log endpoint will serve in one page.")

(defparameter +analytics-limit-ceiling+ 500
  "The most rows an analytics endpoint will serve in one answer.")

;;; --- the network seam ------------------------------------------------------------

(defun body-text (body)
  "BODY -- what NLK:HTTP answered under a byte cap, octets or text -- as text."
  (cond ((stringp body) body)
        ((and (vectorp body) (not (stringp body)))
         (handler-case (sb-ext:octets-to-string (coerce body '(vector (unsigned-byte 8)))
                                                :external-format :utf-8)
           (error () "")))
        (t "")))

(defun default-http (url timeout)
  "GET URL from the NextDNS API; => (values BODY STATUS). The key is the
section's, and the name in User-Agent is the section's too: NextDNS refuses
a default library user agent with a 403, so the name is required rather than
cosmetic."
  (multiple-value-bind (body status)
      (nlk:http :get url
                :headers (list (cons "x-api-key" (setting :api-key))
                               (cons "user-agent" (setting :user-agent))
                               (cons "accept" "application/json"))
                :timeout timeout
                :connect-timeout (min 10 timeout)
                :max-bytes +max-response-bytes+)
    (values (body-text body) status)))

(defvar *http* #'default-http
  "The one network seam: (URL TIMEOUT) => (VALUES BODY STATUS), BODY text.
Tests bind this, so no test dials NextDNS.")

;;; --- the address ----------------------------------------------------------------

(defun pct-encode (text)
  "TEXT percent-encoded for a query string, as UTF-8 octets."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (princ-to-string text) :external-format :utf-8)
          do (if (or (<= 48 octet 57) (<= 65 octet 90) (<= 97 octet 122)
                     (member octet '(45 46 95 126)))
                 (write-char (code-char octet) out)
                 (format out "%~2,'0X" octet)))))

(defun query-string (params)
  "PARAMS, an alist of (NAME . VALUE) with string names, as one query string.
A NIL or empty value is left out, which is how an unset filter stays unset."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in params
                when (and value (not (equal value "")))
                  collect (format nil "~a=~a" (pct-encode name) (pct-encode value)))))

(defun address (path params)
  "PATH, an endpoint path with its leading slash, on the section's base, with
PARAMS as its query string."
  (let ((query (and params (query-string params))))
    (format nil "~a~a~@[?~a~]" (setting :base-url) path query)))

;;; --- reading what came back -------------------------------------------------------

(defun decode-body (body)
  "BODY as decoded JSON, or NIL when it is not JSON."
  (and (stringp body)
       (plusp (length (nlk:trimmed body)))
       (handler-case (nlk:decode-json body)
         (error () nil))))

(defun detail-of (json)
  "The API's own words about a refusal: the parameter and detail of every
entry in its `errors` array, joined. The empty string when it named none."
  (format nil "~{~a~^; ~}"
          (loop for entry across (nlk:json-array json "errors")
                for line = (and (hash-table-p entry)
                                (string-trim '(#\Space #\Tab)
                                             (format nil "~@[~a ~]~@[~a~]"
                                                     (nlk:json-value entry :string "source" "parameter")
                                                     (nlk:json-value entry :string "detail"))))
                when (and line (plusp (length line)))
                  collect line)))

(defun ask (path &optional params)
  "One GET of PATH on the API. => (values JSON NIL) on success,
(values NIL WHY) on a refusal or a dial that failed."
  (let ((url (address path params)))
    (handler-case
        (multiple-value-bind (body status) (funcall *http* url (setting :timeout-seconds))
          (let* ((json (decode-body body))
                 (detail (and json (detail-of json))))
            (cond
              ((not (and (integerp status) (<= 200 status 299)))
               (values nil (format nil "HTTP ~a from the NextDNS API~@[: ~a~]"
                                   status (and detail (plusp (length detail)) detail))))
              ((null json)
               (values nil "the NextDNS API answered with something that is not JSON"))
              ((plusp (length detail))
               (values nil (format nil "the NextDNS API said: ~a" detail)))
              (t (values json nil)))))
      (nextdns-error (condition) (signal condition))
      (error (condition)
        (values nil (format nil "could not reach the NextDNS API: ~a"
                            (or (ignore-errors (nle:transport-failure-label condition url))
                                (princ-to-string condition))))))))

(defun ask! (path &optional params)
  "ASK's decoded body, or a NEXTDNS-ERROR that says why there is none."
  (multiple-value-bind (json why) (ask path params)
    (if why (fail "~a" why) (or json (nlk:json-object)))))
