;;;; wire.lisp --- the HTTP layer: one seam, one host, one key.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through HTTP, so a test stubs NLK:HTTP
;;;; and never touches the network. Paystack answers every status with a JSON
;;;; body carrying `status', `message', `data' and `meta', so a refusal is a
;;;; value the caller formats, not an exception: a PAYOPS-ERROR signals, and
;;;; the tool's ANSWERING turns that into the call's text.
;;;;
;;;; The key rides one header, on one host, over https, and is never logged,
;;;; cached or written anywhere: it is read out of the section at each call,
;;;; and the section read it out of the environment variable it names.

(in-package #:nodecode-payops)

(defparameter +default-base+ "https://api.paystack.co"
  "Paystack's REST base. The original pinned this as its only allowed host;
here it is the section's default, and any base_url it names must be https.")

(defparameter +user-agent+ "nodecode-payops/0.1.0"
  "A plain product token, the same one the original sent.")

(defparameter +page-size+ 100
  "Records per page Paystack's list endpoints take, and the most they give.")

(defparameter +max-pages-ceiling+ 20
  "The original's per-call page cap; a call may not ask for more.")

;;; --- the URL ---------------------------------------------------------------

(defun pct-encode (text)
  "TEXT percent-encoded for a query string, as UTF-8 octets."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (princ-to-string text) :external-format :utf-8)
          do (if (or (<= 48 octet 57) (<= 65 octet 90) (<= 97 octet 122)
                     (member octet '(45 46 95 126)))
                 (write-char (code-char octet) out)
                 (format out "%~2,'0X" octet)))))

(defun query-string (params)
  "PARAMS, an alist of (NAME . VALUE), as one query string; a NIL or empty
value is left out, which is how an unfiltered call says nothing."
  (format nil "~{~a~^&~}"
          (loop for (name . value) in params
                when (and value (not (equal value "")))
                  collect (format nil "~a=~a" (pct-encode name) (pct-encode value)))))

(defun api-base ()
  "The section's base_url, an https origin, or a refusal."
  (let ((base (string-right-trim "/" (nlk:trimmed (or (setting :base-url) +default-base+)))))
    (unless (and (> (length base) 8) (uiop:string-prefix-p "https://" base))
      (fail "base_url must be an https URL, not ~s" base))
    (when (find-if (lambda (character) (member character '(#\? #\# #\@ #\Space))) base)
      (fail "base_url must be a bare https origin, with no query, fragment or userinfo: ~s" base))
    base))

(defun need-key (what)
  "The secret key, or a refusal naming where one comes from."
  (or (setting :secret-key)
      (let ((env (nlk:trimmed (or (setting :secret-key-env) ""))))
        (fail "no Paystack secret key is configured, and ~a needs one. Set ~a and (restart-cells \"nodecode-payops\"), then ask again. A test key is enough, and the safe one to use."
              what
              (if (plusp (length env))
                  (format nil "the environment variable ~a" env)
                  "secret_key in the payops section of the config")))))

(defun auth-headers ()
  "The headers every Paystack call carries, the key among them."
  (list (cons "authorization" (format nil "Bearer ~a" (need-key "a Paystack call")))
        (cons "accept" "application/json")
        (cons "user-agent" +user-agent+)))

;;; --- one exchange ----------------------------------------------------------

(defun remote-message (body)
  "Paystack's own `message' out of an error BODY, one line and short, or NIL."
  (let ((json (ignore-errors (nlk:decode-json (or body "")))))
    (when (hash-table-p json)
      (let ((message (nlk:json-value json :string "message")))
        (when (and message (plusp (length message)))
          (nlk:one-line message :cap 140))))))

(defun api-get (path params)
  "GET PATH on the configured base with PARAMS. => the decoded JSON object.
Signals PAYOPS-ERROR for a status outside 2xx, a body that is not JSON, or
Paystack's own status=false."
  (let ((url (format nil "~a/~a~@[?~a~]" (api-base) path (query-string params))))
    (multiple-value-bind (body status)
        (nlk:http :get url
                  :headers (auth-headers)
                  :timeout (setting :timeout)
                  :connect-timeout (min 10 (setting :timeout)))
      (let ((status (if (integerp status) status 0)))
        (unless (<= 200 status 299)
          (let ((message (remote-message body)))
            (fail "Paystack answered HTTP ~d~@[: ~a~]~:[~;. A 401 means the key is not accepted; a 403 that its plan or scope does not allow this read~]"
                  status message (member status '(401 403)))))
        (let ((json (handler-case (nlk:decode-json (or body "")) (error () nil))))
          (unless (hash-table-p json)
            (fail "Paystack answered something that is not a JSON object"))
          (unless (nlk:json-value json :boolean "status")
            (let ((message (nlk:json-value json :string "message")))
              (fail "Paystack reported an unsuccessful response~@[: ~a~]"
                    (and message (nlk:one-line message :cap 140)))))
          json)))))

;;; --- a page cap, and every page under it ------------------------------------

(defun page-cap (asked)
  "ASKED pages when it is 1 to 20, else the section's default, else a refusal."
  (let ((asked (or asked (setting :max-pages))))
    (unless (and (integerp asked) (<= 1 asked +max-pages-ceiling+))
      (fail "max_pages must be an integer from 1 to ~d, not ~s" +max-pages-ceiling+ asked))
    asked))

(defun fetch-rows (path from to max-pages)
  "Every record PATH lists between FROM and TO, at most MAX-PAGES pages of 100.
=> (values ROWS WARNINGS): ROWS the decoded records, oldest page first;
WARNINGS one line each where the page cap stopped short of the range."
  (let ((rows '()) (warnings '()) (page 0) (page-count 1))
    (loop
      (when (>= page max-pages) (return))
      (setf page (1+ page))
      (let* ((response (api-get path
                                 (list (cons "from" from) (cons "to" to)
                                       (cons "perPage" (princ-to-string +page-size+))
                                       (cons "page" (princ-to-string page)))))
             (data (nlk:json-array response "data"))
             (meta (nlk:json-value response :object "meta"))
             (count (or (nlk:json-value meta :integer "pageCount") 1)))
        (setf page-count (max count 1))
        (loop for row across data do (push row rows))
        (when (and (>= page max-pages) (< page page-count))
          (push (format nil "~a: read ~d of ~d pages (max_pages=~d); ask again with a smaller date range for the rest"
                        path page page-count max-pages)
                warnings))
        (when (or (>= page page-count) (zerop (length data)))
          (return))))
    (values (nreverse rows) (nreverse warnings))))
