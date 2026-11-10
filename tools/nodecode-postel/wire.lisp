;;;; wire.lisp --- Postel's REST API, and the reading of what comes back.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through ASK, so a test stubs one name and
;;;; never touches the network. The original plugin carried no HTTP client of
;;;; its own: it pointed the harness at Postel's hosted MCP server and left the
;;;; transport to the harness. Postel publishes the same operations as a REST
;;;; API under /api/v1, keyed by an API key, so that is what the cell speaks.
;;;;
;;;; Shapes are read leniently: a member is looked for under the names the
;;;; public reference uses and a couple of neighbours, because the hosted
;;;; service moves on its own schedule and a renamed member must not empty a
;;;; tool's answer.

(in-package #:nodecode-postel)

(defparameter +base-url+ "https://www.postel.app/api/v1"
  "Postel's REST API: the address every call goes to unless the section names
another.")
(defparameter +site-url+ "https://www.postel.app"
  "Postel's own site: where an account, a key and a connected network are made.")
(defparameter +docs-url+ "https://www.postel.app/api/docs"
  "The API reference the original skill linked for the full field list.")
(defparameter +user-agent+ "nodecode-postel/0.1.0"
  "What a request calls itself to Postel.")
(defparameter +key-prefix+ "pstl_"
  "What a Postel API key starts with, as the original's own notes said.")
(defparameter +rate-limit+ 60
  "Requests a minute one key is allowed, as the original's notes said.")
(defparameter +blank+ '(#\Space #\Tab #\Newline #\Return)
  "What a body of only whitespace is trimmed of before it counts as empty.")

(defun nonempty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun api-key ()
  "The key the section names inline or by variable, else POSTEL_API_KEY, the
variable the original's REST notes used. NIL is a working state: the cell
starts without one and each tool says what it needs."
  (or (nonempty (setting :api-key))
      (let ((variable (nonempty (setting :api-key-env))))
        (and variable (nonempty (uiop:getenv variable))))
      (nonempty (uiop:getenv "POSTEL_API_KEY"))))

(defun keyed ()
  "The key, or a refusal naming where one is made: every call here needs one,
as every MCP tool but the research one needed an account."
  (or (api-key)
      (fail "postel needs a Postel API key (it starts with ~a): make one at ~a, Settings then API ~
Keys, and set it as api_key in the postel section, or export POSTEL_API_KEY. ~a"
            +key-prefix+ +site-url+ +docs-url+)))

(defun base-url ()
  "The section's address, or Postel's own, without a trailing slash."
  (let ((given (nonempty (setting :base-url))))
    (if given (string-right-trim "/" given) (string-right-trim "/" +base-url+))))

(defun query-string (query)
  "QUERY -- an alist of (NAME . VALUE) -- as ?a=b&c=d, members with no value
left out. A name and a value are both encoded."
  (let ((pairs (loop for (name . value) in query
                     when (and name value)
                       when (not (and (stringp value) (zerop (length (nlk:trimmed value)))))
                         collect (cons name (princ-to-string value)))))
    (when pairs
      (format nil "?~{~a~^&~}"
              (loop for (name . value) in pairs
                    collect (format nil "~a=~a"
                                    (quri:url-encode name :encoding :utf-8)
                                    (quri:url-encode value :encoding :utf-8)))))))

(defun endpoint (path &optional query)
  "PATH under the base address, with QUERY appended."
  (format nil "~a/~a~a" (base-url) (string-left-trim "/" path) (or (query-string query) "")))

(defun headers (&key json)
  "What every request sends: JSON asked for, the bearer key, the client named,
and a JSON content type when there is a body."
  (append (list (cons "accept" "application/json")
                (cons "authorization" (format nil "Bearer ~a" (keyed)))
                (cons "user-agent" +user-agent+))
          (when json (list (cons "content-type" "application/json")))))

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded JSON object or array, or NIL."
  (cond ((hash-table-p answer) answer)
        ((and (vectorp answer) (not (stringp answer))) answer)
        ((stringp answer)
         (if (zerop (length (string-trim +blank+ answer)))
             nil
             (let ((decoded (handler-case (nlk:decode-json answer) (error () nil))))
               (and (or (hash-table-p decoded) (vectorp decoded)) decoded))))
        (t nil)))

(defun message-of (body)
  "Postel's own message, whichever member carries it; the reference says
`error' is written for the user, so it is shown as it is."
  (or (nlk:json-value body :text "error")
      (nlk:json-value body :text "message")
      (nlk:json-value body :text "detail")))

(defun status-hint (status)
  "What to do about a refused STATUS, in the original's own terms."
  (cond ((eql status 401) "the key was refused; check it in the postel section")
        ((eql status 402) "the Postel plan does not cover this; scheduling and publishing need a paid plan")
        ((eql status 403) "the key's workspace may not do this")
        ((eql status 404) "no post with that id in this workspace")
        ((eql status 409) "the post is in a state that does not allow this; check it with postel_post")
        ((eql status 422) "Postel refused the fields; the message says which")
        ((eql status 429) (format nil "~d requests a minute a key; wait for Retry-After and ask again"
                                  +rate-limit+))
        ((and (integerp status) (>= status 500)) "Postel's side failed; try again later")
        (t nil)))

(defun ask (method url &key content json (timeout (setting :timeout-seconds)))
  "METHOD URL as one exchange under the section's ceiling => (values BODY
STATUS). A transport failure is a POSTEL-ERROR, one line the model reads."
  ;; The headers are built first, so a missing key refuses before any dial and is
  ;; not relabelled a transport failure by the catch below.
  (let ((heads (headers :json json)) (answer nil) (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers heads
                    :content content
                    :timeout timeout
                    :connect-timeout (min 10 (max 1 timeout))))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "postel: ~a" (nle:transport-failure-label condition url))))
    (values answer status)))

(defun value-of (answer status where)
  "ANSWER and STATUS of one call => its decoded body, or a POSTEL-ERROR naming
WHERE, Postel's own message and what to do next."
  (let ((body (decode-body answer)))
    (when (and (integerp status) (>= status 400))
      (fail "~a: HTTP ~d~@[ - ~a~]~@[ - ~a~]"
            where status (message-of body) (status-hint status)))
    (unless body
      (fail "~a: Postel answered ~:[nothing that is JSON~;something that is not a JSON object or array~]"
            where (and (stringp answer) (plusp (length (string-trim +blank+ answer))))))
    body))

(defun request (method path &key query content where)
  "METHOD PATH against Postel => its decoded body. CONTENT, when given, is an
event object built with NLK:JSON-OBJECT and encoded here."
  (let ((url (endpoint path query)))
    (multiple-value-bind (answer status)
        (ask method url
             :content (and content (nlk:encode-json-object content))
             :json (and content t))
      (value-of answer status (or where path)))))

(defun call-get (path &key query where)
  "One GET => its decoded body."
  (request :get path :query query :where where))

(defun call-post (path &key content where)
  "One POST => its decoded body."
  (request :post path :content (or content (nlk:json-object)) :where where))

(defun pick (object &rest names)
  "OBJECT's first member named by NAMES that is there, whatever its type."
  (loop for name in names
        for value = (and (hash-table-p object) (nlk:json-value object :any name))
        when value return value))

(defun pick-text (object &rest names)
  "OBJECT's first non-empty text among NAMES, else NIL."
  (loop for name in names
        for value = (and (hash-table-p object) (nlk:json-value object :text name))
        when value return value))

(defun rows-of (body &rest names)
  "BODY as the rows it carries: BODY itself when it is an array, else its
first array member among NAMES. A single object reads as one row."
  (let ((rows (if (vectorp body)
                  body
                  (loop for name in names
                        for value = (and (hash-table-p body) (nlk:json-value body :any name))
                        when (vectorp value) return value))))
    (cond ((vectorp rows) (remove-if-not #'hash-table-p (coerce rows 'list)))
          ((hash-table-p body) (list body))
          (t '()))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying how much was left."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (format nil "~a [...]" (subseq text 0 limit)))))
