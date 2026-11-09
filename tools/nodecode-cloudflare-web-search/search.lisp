;;;; search.lisp --- one POST to Cloudflare Web Search, one answer as lines.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original's provider.py, in Lisp: the same endpoint
;;;; (POST /client/v4/accounts/<account>/ai/websearch/), the same bearer token,
;;;; the same request object -- query, provider, limit, options.gateway.id and
;;;; an optional byokAlias -- and the same top-level items array read back.
;;;; Two things differ, both deliberate. The items become lines a model reads
;;;; rather than a JSON envelope it has to parse, and a failure SIGNALS
;;;; CLOUDFLARE-WEB-SEARCH-ERROR carrying Cloudflare's own sentence, so the
;;;; model gets the reason instead of a {"success": false} member.
;;;;
;;;; Nothing here touches a file or the environment: the only way out is HTTP,
;;;; the one seam a test stubs. SETTINGS is the plist the section declares,
;;;; resolved by READ-SETTINGS in cell.lisp, and passed in -- a call reads the
;;;; image's settings once and carries them, so the verb is callable with a
;;;; test's own plist and no dynamic binding.

(in-package #:nodecode-cloudflare-web-search)

;;; --- what the API is ---------------------------------------------------------

(defparameter +api-root+ "https://api.cloudflare.com/client/v4"
  "Cloudflare's API root. The search endpoint hangs off the account path below it.")

(defparameter +providers+ '("ceramic" "exa" "linkup")
  "The search providers Cloudflare Web Search routes a query to, as its beta API names them.")

(defparameter +result-ceiling+ 10
  "The most results one search may ask for, as the API's own limit says.")

(defparameter +query-ceiling+ 1024
  "The longest query the API takes, in characters.")

(defparameter +token-env+ "CLOUDFLARE_WEB_SEARCH_API_TOKEN"
  "The variable the original read its token from, read here when the section names none.")

(defparameter +account-env+ "CLOUDFLARE_ACCOUNT_ID"
  "The variable naming an account id, so a box can be set up without a section.")

(defparameter +gateway-env+ "CLOUDFLARE_GATEWAY_ID"
  "The variable naming an AI Gateway, so a box can be set up without a section.")

(defparameter +detail-chars+ 1000
  "How much of a failed answer's body a refusal quotes.")

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun clipped (text)
  "TEXT at most +DETAIL-CHARS+ characters, with a note when it was cut."
  (if (<= (length text) +detail-chars+)
      text
      (format nil "~a [~d more characters]" (subseq text 0 +detail-chars+)
              (- (length text) +detail-chars+))))

;;; --- the settings, as the image holds them -------------------------------------

(defun current-settings ()
  "The settings this cell is running on, as the plist READ-SETTINGS built:
the declared members, the token, account and gateway already resolved."
  (list :api-token (setting :api-token)
        :account-id (setting :account-id)
        :gateway-id (setting :gateway-id)
        :provider (setting :provider)
        :byok-alias (setting :byok-alias)
        :base-url (setting :base-url)
        :max-results (setting :max-results)
        :timeout-seconds (setting :timeout-seconds)))

(defun account-id-p (text)
  "TEXT as Cloudflare's 32-character hexadecimal account id, as the original
checked it before any request."
  (and (stringp text)
       (= 32 (length text))
       (every (lambda (character) (digit-char-p character 16)) text)))

(defun alias-p (text)
  "TEXT as a BYOK alias: the API's 1-64 letters, digits, underscores and
hyphens."
  (and (<= 1 (length text) 64)
       (every (lambda (character)
                (or (alphanumericp character) (char= character #\-) (char= character #\_)))
              text)))

(defun settings-problem (settings)
  "What SETTINGS is missing or has wrong, as one sentence the model repairs
from, or NIL. The original validated the same members on every call."
  (let ((token (getf settings :api-token))
        (account (getf settings :account-id))
        (gateway (getf settings :gateway-id))
        (provider (getf settings :provider))
        (alias (getf settings :byok-alias)))
    (cond ((null (nonblank token))
           "no token: set api_token (or api_token_env) to a Cloudflare API token with Workers AI Read and AI Gateway Read")
          ((null (nonblank account))
           "no account: set account_id to the 32-character Cloudflare account id, or export CLOUDFLARE_ACCOUNT_ID")
          ((not (account-id-p account))
           (format nil "account_id ~s is not a 32-character hexadecimal id" account))
          ((null (nonblank gateway))
           "gateway_id must not be empty: name the AI Gateway that routes and bills the search, or default")
          ((not (member provider +providers+ :test #'string=))
           (format nil "provider ~s is not one of ~{~a~^, ~}" provider +providers+))
          ((and (nonblank alias) (not (alias-p alias)))
           (format nil "byok_alias ~s is not 1-64 letters, digits, underscores or hyphens" alias))
          (t nil))))

(defun check-ready (settings)
  "Refuse a call SETTINGS cannot pay for, in the words the model repairs from."
  (let ((problem (settings-problem settings)))
    (when problem (fail "~a" problem))))

;;; --- the request ---------------------------------------------------------------

(defun root (settings)
  "The API root SETTINGS names, without a trailing slash."
  (let ((given (string-right-trim "/" (or (getf settings :base-url) ""))))
    (if (plusp (length given)) given +api-root+)))

(defun endpoint (settings)
  "The account's web-search address: the one place a query is posted."
  (format nil "~a/accounts/~a/ai/websearch/" (root settings) (or (getf settings :account-id) "")))

(defun clamped-count (count settings)
  "COUNT within 1 and +RESULT-CEILING+; the section's max_results answers a
call that names none, as the original clamped both."
  (max 1 (min +result-ceiling+ (or count (getf settings :max-results) 5))))

(defun chosen-provider (provider settings)
  "The provider a call names, else the section's, as the original resolved it."
  (let ((value (string-downcase (or (nonblank provider) (getf settings :provider) "ceramic"))))
    (or (find value +providers+ :test #'string=)
        (fail "provider is ~s: Cloudflare Web Search routes to ~{~a~^, ~}" value +providers+))))

(defun request-body (query settings &key max-results provider)
  "The JSON body one search posts: the query, the provider that routes it,
the count, the gateway it is billed and logged through, and a BYOK alias
when the section names one."
  (apply #'nlk:make-json-object
         (append (list "query" query
                       "provider" (chosen-provider provider settings)
                       "limit" (clamped-count max-results settings)
                       "options" (nlk:make-json-object
                                  "gateway"
                                  (nlk:make-json-object "id" (or (getf settings :gateway-id) "default"))))
                 (let ((alias (nonblank (getf settings :byok-alias))))
                   (when alias (list "byokAlias" alias))))))

;;; --- the one seam ---------------------------------------------------------------

(defun http (url &key headers content timeout)
  "The cell's one network call; a test stubs this name. => (values BODY
STATUS), every status a value. The original asked not to follow redirects;
the image's call follows at most five, which stays on Cloudflare's own
redirect target."
  (let ((seconds (or timeout 30)))
    (nlk:http :post url
              :headers headers
              :content content
              :timeout seconds
              :connect-timeout (min 10 seconds))))

(defun ask-cloudflare (body settings)
  "Post BODY -- a search request -- to the account's endpoint. => (values
BODY STATUS). The token is the section's, in the same Authorization header
the original sent."
  (http (endpoint settings)
        :headers (list (cons "authorization" (format nil "Bearer ~a" (or (getf settings :api-token) "")))
                       (cons "content-type" "application/json")
                       (cons "accept" "application/json"))
        :content (nlk:encode-json-object body)
        :timeout (getf settings :timeout-seconds)))

;;; --- what comes back --------------------------------------------------------------

(defun decoded (body)
  "BODY -- an HTTP body -- as a decoded JSON value, or NIL."
  (and (stringp body) (plusp (length (nlk:trimmed body)))
       (ignore-errors (nlk:decode-json body))))

(defun failure-sentence (body)
  "Cloudflare's own words from a refused answer: the first error's message,
else a clipped body."
  (let ((payload (decoded body)))
    (or (let* ((errors (nlk:json-value payload :array "errors"))
               (first (and errors (plusp (length errors)) (aref errors 0))))
          (nlk:json-value first :text "message"))
        (nlk:json-value payload :text "message")
        (let ((text (and (stringp body) (nlk:trimmed body))))
          (and text (plusp (length text)) (clipped text))))))

(defun payload-of (body status)
  "BODY decoded as Cloudflare's answer, or a refusal naming the status and
Cloudflare's own sentence."
  (when (and (integerp status) (>= status 400))
    (fail "Cloudflare Web Search returned HTTP ~d~@[: ~a~]" status (failure-sentence body)))
  (let ((payload (decoded body)))
    (unless (hash-table-p payload)
      (fail "Cloudflare Web Search returned a non-JSON answer: HTTP ~a~@[: ~a~]"
            status (failure-sentence body)))
    (when (eq :false (nlk:json-value payload :any "success"))
      (fail "Cloudflare Web Search refused the search~@[: ~a~]" (failure-sentence body)))
    payload))

(defun item-of (item position)
  "ITEM -- one result -- as the fields the original lifted, or a refusal when
it carries no title and url, as the original refused one."
  (unless (hash-table-p item)
    (fail "result ~d of Cloudflare's answer is not an object" position))
  (let ((title (nlk:json-value item :text "title"))
        (url (nlk:json-value item :text "url")))
    (unless (and title url)
      (fail "result ~d carries no title and url" position))
    (list :position position
          :title title
          :url url
          :description (or (nlk:json-value item :string "description") ""))))

(defun items-of (payload)
  "PAYLOAD's items array as a list of results, or a refusal when the answer
carries none -- the original's \"response must contain an items array\"."
  (let ((items (nlk:json-value payload :array "items")))
    (unless items
      (fail "Cloudflare's answer carries no items array"))
    (loop for item across items
          for position from 1
          collect (item-of item position))))

(defun metadata-of (payload)
  "PAYLOAD's metadata as (values REQUEST-ID LATENCY-MS), what a support
question needs."
  (let ((meta (nlk:json-value payload :object "metadata")))
    (values (nlk:json-value meta :string "requestId")
            (nlk:json-value meta :integer "latencyMs"))))

;;; --- the answer as lines ----------------------------------------------------------

(defun result-block (result)
  "One RESULT as the model reads it: a numbered title, its url, its
description."
  (with-output-to-string (out)
    (format out "[~d] ~a~%    ~a" (getf result :position) (getf result :title) (getf result :url))
    (let ((description (nlk:trimmed (getf result :description ""))))
      (when (plusp (length description))
        (format out "~%    ~a" description)))))

(defun search-text (query items &key provider gateway request-id latency)
  "What a search answers: one block a result, or a line saying Cloudflare
found none, with the route and the request id."
  (let ((route (format nil "provider ~a, gateway ~a~@[, ~a ms~]~@[, request ~a~]"
                        provider gateway latency request-id)))
    (if (null items)
        (format nil "cloudflare: no results for ~s (~a)" query route)
        (format nil "cloudflare: ~d result~:p for ~s (~a)~2%~{~a~^~2%~}"
                (length items) query route (mapcar #'result-block items)))))

;;; --- the verb ------------------------------------------------------------------

(defun check-query (query)
  "QUERY trimmed, or a refusal naming what the API takes: the original's
1-to-1024-character bound."
  (let ((query (nonblank query)))
    (unless query
      (fail "search needs a query: give one, e.g. (cloudflare-web-search:search \"latest Rust release notes\")"))
    (when (> (length query) +query-ceiling+)
      (fail "the query is ~d characters: Cloudflare takes at most ~d"
            (length query) +query-ceiling+))
    query))

(defun search (query &key max-results provider)
  "Search the web through Cloudflare Web Search, which routes the query to
Ceramic, Exa or Linkup through the operator's own Cloudflare AI Gateway.
QUERY is the search, up to 1024 characters; MAX-RESULTS 1 to 10 (the
section's max_results when absent); PROVIDER ceramic, exa or linkup (the
section's when absent). => the results as lines: title, url, description,
with the route and the gateway's request id."
  (let ((settings (current-settings)))
    (check-ready settings)
    (let ((query (check-query query)))
      (multiple-value-bind (body status)
          (ask-cloudflare (request-body query settings :max-results max-results :provider provider)
                          settings)
        (let ((payload (payload-of body status)))
          (multiple-value-bind (request-id latency) (metadata-of payload)
            (search-text query (items-of payload)
                         :provider (chosen-provider provider settings)
                         :gateway (getf settings :gateway-id)
                         :request-id request-id
                         :latency latency)))))))

;;; --- the tool the model calls -----------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search the live web through Cloudflare Web Search, which routes the query to Ceramic,
Exa or Linkup on the operator's Cloudflare account. Answers numbered results - title, url and
description - and the AI Gateway request id. It searches; it does not read a page.")
  ("query" :text :required t
   :doc "the search query, 1 to 1024 characters")
  ("max_results" :integer :slot result-count
   :doc "how many results, 1 to 10; the section's max_results answers when this is absent")
  ("provider" :string :options '("ceramic" "exa" "linkup")
   :doc "which provider routes the search; the section's provider answers when this is absent"))

(defun search-tool (call)
  "The cloudflare_search tool: the members one call carries, answered as the
verb answers them."
  (search (search-call-query call)
          :max-results (search-call-result-count call)
          :provider (search-call-provider call)))
