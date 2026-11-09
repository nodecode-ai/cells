;;;; wire.lisp --- the one network seam, the price table and the spend ledger.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through REQUEST or GET-CONTENT, so a test
;;;; stubs NLK:HTTP once and nothing here reaches OpenAlex. OpenAlex bills by
;;;; call shape rather than by bytes -- an id lookup is free, a list call is
;;;; $0.0001, a search ten times that and a text classification a hundred times
;;;; it -- so the price of a call is known before it is sent, and BUDGET-CHECK
;;;; refuses in advance rather than discovering the wall afterwards.
;;;;
;;;; A 429 here means one of two things and they are told apart the way the
;;;; original told them apart: a Retry-After of about a second is a throttle,
;;;; anything in the tens of thousands is a spent daily budget.

(in-package #:nodecode-openalex)

(defparameter +api-base+ "https://api.openalex.org"
  "OpenAlex's API, where every tool goes unless the section names another.")

(defparameter +content-base+ "https://content.openalex.org"
  "Where the full-text artifacts live; GET-CONTENT refuses any other host.")

(defparameter +anon-daily-usd+ 0.10
  "What OpenAlex's anonymous tier allows per day. A free key raises it to $1.00.")

(defparameter +throttle-retry-ceiling+ 120
  "A 429 whose Retry-After is below this is a throttle; above it, the budget is gone.")

(defparameter *prices* '(("free" . 0d0) ("list" . 0.0001d0) ("search" . 0.001d0)
                          ("text" . 0.01d0) ("content" . 0.01d0))
  "Dollars per call class, as the live /rate-limit reports them. The account
tool adopts the live table when a key is configured.")

(defun price (class)
  "What one call of CLASS costs, in dollars."
  (or (cdr (assoc class *prices* :test #'equal)) 0.0001d0))

(defun set-prices (table)
  "Adopt TABLE -- (CLASS . USD) pairs from the account -- for the classes it
names, and answer the classes whose price changed."
  (loop for (class . usd) in table
        when (and (stringp class) (realp usd) (assoc class *prices* :test #'equal))
          append (unless (= (price class) (coerce usd 'double-float))
                   (list class))
          and do (setf (cdr (assoc class *prices* :test #'equal)) (coerce usd 'double-float))))

;;; --- the ledger --------------------------------------------------------------

(defstruct (ledger (:conc-name ledger-))
  (spent 0d0)
  (calls 0)
  (free 0)
  (classes nil))

(defvar *ledger* (make-ledger)
  "What this start of the cell has spent, by call class. Reset by START-CELL.")

(defun ledger-note (class cost)
  "Charge COST of CLASS to the ledger, and answer it."
  (incf (ledger-calls *ledger*))
  (if (plusp cost)
      (progn (incf (ledger-spent *ledger*) cost)
             (let ((entry (assoc class (ledger-classes *ledger*) :test #'equal)))
               (if entry
                   (incf (cdr entry))
                   (push (cons class 1) (ledger-classes *ledger*)))))
      (incf (ledger-free *ledger*)))
  cost)

(defun budget-usd ()
  "The section's spend ceiling for this cell, in dollars."
  (setting :budget-usd))

(defun budget-left ()
  "What is left of the ceiling; negative once it is overspent."
  (- (budget-usd) (ledger-spent *ledger*)))

(defun budget-check (class)
  "Refuse, before anything is sent, a call whose price the ceiling cannot bear."
  (let ((cost (price class)))
    (when (and (plusp cost) (> cost (+ (budget-left) 1d-9)))
      (fail "openalex: this ~a call costs $~,4f and only $~,4f of the $~,2f budget is left.~
~%Free: openalex_get by id, openalex_resolve, openalex_fields, openalex_account.~
~%A tenth of a search: openalex_count, and openalex_count with group_by.~
~%Raise budget_usd in the openalex section, or answer with the free calls."
            class cost (max 0d0 (budget-left)) (budget-usd)))
    cost))

;;; --- building a request ------------------------------------------------------

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun api-key ()
  "The key the section names inline or by variable, else OPENALEX_API_KEY.
A key is optional: every call works anonymously on a smaller daily allowance."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "OPENALEX_API_KEY"))))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +api-base+)))

(defun headers ()
  "What every request carries. The key rides as a bearer token rather than a
query parameter, so it stays out of URLs, proxy logs and any traceback."
  (let ((key (api-key)))
    (append '(("accept" . "application/json")
              ("user-agent" . "nodecode-openalex"))
            (and key (list (cons "authorization" (format nil "Bearer ~a" key)))))))

(defun url-encode (text)
  "TEXT as a query-string value: the unreserved characters kept, every other
byte of its UTF-8 form written %XX."
  (with-output-to-string (out)
    (loop for character across (or text "")
          for code = (char-code character)
          do (if (or (<= 48 code 57) (<= 65 code 90) (<= 97 code 122)
                     (member character '(#\- #\_ #\. #\~) :test #'char=))
                 (write-char character out)
                 (loop for byte across (sb-ext:string-to-octets
                                        (string character) :external-format :utf-8)
                       do (format out "%~2,'0X" byte))))))

(defun url (path params)
  "The absolute URL of PATH with PARAMS -- an alist of (NAME . VALUE) whose
NIL values are left out, so a call sends only what it was given."
  (let ((pairs (remove-if (lambda (pair) (null (cdr pair))) params)))
    (format nil "~a/~a~@[?~{~a~^&~}~]"
            (root)
            (string-left-trim "/" path)
            (and pairs
                 (loop for (name . value) in pairs
                       collect (format nil "~a=~a"
                                       (url-encode (string name))
                                       (url-encode (princ-to-string value))))))))

(defun search-class (path params)
  "Which price applies to PATH with PARAMS, worked out before it is sent.
A singleton by prefixed id is free and the same record by its URL form is
billed, /autocomplete is free, group_by is list-priced even beside a search,
and any search term is search-priced."
  (let ((path (string-trim "/" path)))
    (cond
      ((or (eql 0 (search "autocomplete" path))
           (eql 0 (search "rate-limit" path)))
       "free")
      ((eql 0 (search "text" path)) "text")
      (t (let* ((slash (position #\/ path))
                (identifier (and slash (subseq path (1+ slash)))))
           (cond
             ((and identifier (plusp (length identifier))
                   (not (string= identifier "random"))
                   (not (eql 0 (search "http" identifier))))
              "free")
             ((cdr (assoc "group_by" params :test #'equal)) "list")
             ((or (cdr (assoc "search" params :test #'equal))
                  (let ((filter (cdr (assoc "filter" params :test #'equal))))
                    (and (stringp filter) (search ".search:" filter))))
              "search")
             (t "list")))))))

;;; --- answers -----------------------------------------------------------------

(defun decode-body (body)
  "BODY -- an HTTP body -- as a decoded JSON object; anything else is an
empty object, so a caller never guards the parse."
  (cond ((hash-table-p body) body)
        ((stringp body)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) body)))
             (handler-case (let ((value (nlk:decode-json body)))
                             (if (hash-table-p value) value (nlk:json-object)))
               (error () (nlk:json-object)))
             (nlk:json-object)))
        (t (nlk:json-object))))

(defun error-text (body status)
  "What a refused answer says, in the one line the model reads."
  (let ((object (decode-body body)))
    (or (nlk:json-value object :text "message")
        (nlk:json-value object :text "error")
        (and (stringp body)
             (subseq body 0 (min 200 (length body))))
        (format nil "HTTP ~a with no message" status))))

(defun retry-after (headers)
  "The Retry-After header of an answer, in seconds, or NIL."
  (let ((entry (and (listp headers)
                    (assoc "retry-after" headers :test #'string-equal))))
    (and entry (ignore-errors (parse-integer (princ-to-string (cdr entry))
                                             :junk-allowed t)))))

(defun rate-limit-message (path headers)
  "One line for a 429: a throttle and an exhausted budget read differently."
  (let ((wait (retry-after headers)))
    (if (and wait (> wait +throttle-retry-ceiling+))
        (format nil "openalex ~a: HTTP 429, the daily budget is spent until it resets, ~
which is ~d seconds away; only free calls work until then" path wait)
        (format nil "openalex ~a: HTTP 429, rate limited~@[ for ~d seconds~]; wait, do not retry at once"
                path wait))))

;;; --- the two requests ----------------------------------------------------------

(defun request (path params)
  "GET PATH with PARAMS => the decoded answer. Charges the ledger for what the
call costs and refuses, before sending, one the budget cannot bear."
  (let* ((class (search-class path params))
         (cost (budget-check class))
         (address (url path params))
         (answer nil)
         (status nil)
         (answer-headers nil))
    (handler-case
        (multiple-value-setq (answer status answer-headers)
          (nlk:http :get address
                    :headers (headers)
                    :timeout (setting :timeout-seconds)
                    :connect-timeout (min 10 (setting :timeout-seconds))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "openalex ~a: ~a" path (nle:transport-failure-label condition address))))
    (cond
      ((and (integerp status) (< status 300))
       (ledger-note class cost)
       (decode-body answer))
      ((eql status 404)
       (error 'openalex-not-found :detail (format nil "openalex ~a: no record" path)))
      ((eql status 401)
       (fail "openalex ~a: HTTP 401, the key was refused; check api_key (or api_key_env) in the ~
openalex section, or unset it to use the anonymous tier" path))
      ((eql status 403)
       (fail "openalex ~a: HTTP 403, the key is not allowed to do this: ~a" path (error-text answer status)))
      ((eql status 429)
       (fail "~a" (rate-limit-message path answer-headers)))
      ((eql status 400)
       (fail "openalex ~a: HTTP 400, the query was refused: ~a" path (error-text answer status)))
      (t (fail "openalex ~a: HTTP ~a~@[: ~a~]" path status (error-text answer status))))))

(defun host-of (address)
  "The host of ADDRESS, lowercased, or NIL."
  (let* ((start (or (search "://" address) -3))
         (rest (subseq address (min (length address) (+ start 3))))
         (end (or (position-if (lambda (c) (member c '(#\/ #\: #\?))) rest)
                  (length rest))))
    (string-downcase (subseq rest 0 end))))

(defun get-content (address)
  "GET ADDRESS -- a full-text artifact -- as text. Only https on the content
host is fetched, the download is capped at 8 MiB, and the gzip the service
returns under a content type (rather than an encoding header) is expanded."
  (unless (and (eql 0 (search "https://" address))
               (equal (host-of address) (host-of +content-base+)))
    (fail "openalex: a full-text url must be https on ~a, not ~a" (host-of +content-base+) address))
  (let ((cost (budget-check "content"))
        (body nil)
        (status nil))
    (handler-case
        (multiple-value-setq (body status)
          (nlk:http :get address
                    :headers '(("accept-encoding" . "identity"))
                    :timeout (max 60 (setting :timeout-seconds))
                    :connect-timeout 10
                    :max-bytes (* 8 1024 1024)
                    :binary t))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "openalex: could not download the full text: ~a"
              (nle:transport-failure-label condition address))))
    (unless (and (integerp status) (< status 300))
      (fail "openalex: the full-text download answered HTTP ~a~@[: ~a~]"
            status (and (stringp body) (subseq body 0 (min 200 (length body))))))
    (ledger-note "content" cost)
    (let ((octets (if (stringp body) (sb-ext:string-to-octets body :external-format :utf-8) body)))
      (when (and (>= (length octets) 2) (= (aref octets 0) #x1f) (= (aref octets 1) #x8b))
        (setf octets (handler-case (chipz:decompress nil 'chipz:gzip octets)
                        (error () octets))))
      (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8)))
                               :external-format :utf-8))))
