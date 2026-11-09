;;;; cell.lisp --- DeepSeek Search and Fetch as two verbs and two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; DeepSeek sells two tools on one API: POST /v1/tools/search (ranked hits,
;;;; optionally with each hit's page text) and POST /v1/tools/fetch (one URL
;;;; as Markdown), both billed per successful call to the account the key
;;;; belongs to. The original plugin was a web backend in another harness,
;;;; where its whole job was to answer that harness's search and extract
;;;; calls; here the same two endpoints answer a model-facing tool each
;;;; (deepseek_search, deepseek_fetch) and an eval verb each
;;;; ((deepseek-search:search ...), (deepseek-search:fetch ...)).
;;;;
;;;; What the original pinned is kept because it is what makes the vendor's
;;;; answers usable: the 128-character query cap (clamped client-side, so a
;;;; long query is never sent as-is), the fetch endpoint's backslash
;;;; escaping reversed so Markdown arrives as Markdown, the two retries on
;;;; 408/429/5xx with a Retry-After honoured when it is affordable, the 402
;;;; top-up sentence, and vendor error text cut to 300 characters so one
;;;; hostile body cannot fill the model's context.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "deepseek-search": {"api_key": "...", "count": 5, "max_chars": 8000}
;;;; The key is never written in this file: it comes from the section, the
;;;; environment variable the section names, or DEEPSEEK_API_KEY.

(defpackage #:nodecode-deepseek-search
  (:use #:cl)
  (:nicknames #:deepseek-search)
  (:shadow #:search)
  (:export #:search #:fetch #:deepseek-search-error))

(in-package #:nodecode-deepseek-search)

;;; FAIL and DEEPSEEK-SEARCH-ERROR, *DEEPSEEK-SEARCH* and RUNNING-SETTINGS,
;;; and DEFINE-VERB (a defun that refuses while the cell is not running) come
;;; from here. SETTING comes from the NLE:DEFINE-CELL declaration below,
;;; which loads last in this file and is the only place the section is named.
(nlk:define-peripheral deepseek-search :not-running t)

;;; --- what the vendor fixes, and we do not --------------------------------

(defparameter +default-base+ "https://api.deepseek.com"
  "The origin both endpoints live on, unless the section names another.")
(defparameter +search-path+ "/v1/tools/search")
(defparameter +fetch-path+ "/v1/tools/fetch")
(defparameter +query-max+ 128
  "The vendor's query ceiling. A longer query is clamped, never sent as-is.")
(defparameter +query-floor+ 96
  "How far back the clamp chases a word boundary before it cuts the query short.")
(defparameter +count-max+ 50
  "The vendor's results ceiling. The tool's own cap asks for at most this.")
(defparameter +retry-backoff+ '(1.0 2.0)
  "The waits before the two retries, each plus a little jitter.")
(defparameter +retry-jitter+ 0.25)
(defparameter +retry-after-max+ 10.0
  "A Retry-After longer than this is not waited out: the call stops retrying.")
(defparameter +detail-cap+ 300
  "How much of a vendor error body reaches the model, in characters.")
(defparameter +default-max-chars+ 8000
  "What one fetched page is cut to, so a long page cannot fill the context.")
(defparameter +missing-key+
  "no DeepSeek API key: set deepseek-search.api_key, or deepseek-search.api_key_env, or DEEPSEEK_API_KEY (keys come from platform.deepseek.com/api_keys)"
  "The refusal every verb signals when there is no key to send.")

(defvar *sleep* #'sleep
  "How a retry waits. A test binds it, so no run of the suite sleeps.")

;;; --- the section's values --------------------------------------------------

(defun trimmed (value)
  "VALUE as a trimmed string, \"\" when it is not one."
  (if (stringp value) (nlk:trimmed value) ""))

(defun base-url ()
  "The origin the two endpoints are asked at, without a trailing slash."
  (let ((raw (trimmed (setting :base-url))))
    (if (plusp (length raw)) (string-right-trim "/" raw) +default-base+)))

(defun api-key ()
  "The bearer key: the section's, the variable it names, or DEEPSEEK_API_KEY."
  (or (let ((written (trimmed (setting :api-key))))
        (and (plusp (length written)) written))
      (let ((named (trimmed (setting :api-key-env))))
        (and (plusp (length named)) (uiop:getenv named)))
      (uiop:getenv "DEEPSEEK_API_KEY")
      ""))

(defun ip-literal-p (text)
  "TEXT as the vendor's `localization' wants it: an IPv4 or IPv6 literal."
  (cond
    ((every (lambda (character) (or (digit-char-p character) (char= character #\.))) text)
     (let ((parts (uiop:split-string text :separator ".")))
       (and (= 4 (length parts))
            (every (lambda (part)
                     (and (plusp (length part))
                          (<= (length part) 3)
                          (let ((number (ignore-errors (parse-integer part :junk-allowed nil))))
                            (and number (<= 0 number 255)))))
                   parts))))
    ((find #\: text)
     (every (lambda (character)
              (or (digit-char-p character 16)
                  (find character ":.")))
            text))
    (t nil)))

(defun localization ()
  "The section's localization IP, or \"\" when what it holds is not one."
  (let ((value (trimmed (setting :localization))))
    (if (and (plusp (length value)) (ip-literal-p value)) value "")))

(defun page-limit ()
  "How many characters of one page a fetch answers, floored at a readable slice."
  (max 200 (or (setting :max-chars) +default-max-chars+)))

;;; --- small text helpers -----------------------------------------------------

(defun clip (text limit)
  "TEXT's first LIMIT characters."
  (if (and (stringp text) (> (length text) limit)) (subseq text 0 limit) text))

(defun ascii-punctuation-p (character)
  "True for the ASCII punctuation the fetch endpoint backslash-escapes."
  (let ((code (char-code character)))
    (or (<= 33 code 47) (<= 58 code 64) (<= 91 code 96) (<= 123 code 126))))

(defun unescape-markdown (text)
  "Undo the fetch endpoint's backslash escaping of ASCII punctuation.

Only a backslash before ASCII punctuation is removed, so a page's own
\\n, \\d or C:\\path survives as written."
  (with-output-to-string (out)
    (loop with length = (length text)
          for index from 0 below length
          for character = (char text index)
          do (if (and (char= character #\\)
                      (< (1+ index) length)
                      (ascii-punctuation-p (char text (1+ index))))
                 (progn (write-char (char text (1+ index)) out)
                        (incf index))
                 (write-char character out)))))

(defun clamp-query (text)
  "TEXT cut to the vendor's 128 characters, at a word boundary when one is close."
  (if (<= (length text) +query-max+)
      text
      (let* ((head (subseq text 0 +query-max+))
             (cut (position #\Space head :from-end t)))
        (string-right-trim
         '(#\Space #\Tab)
         (if (and cut (>= cut +query-floor+)) (subseq head 0 cut) head)))))

(defun header-value (headers name)
  "HEADERS' NAME, case-insensitively: an alist or a hash table."
  (cond ((hash-table-p headers)
         (or (gethash name headers)
             (gethash (string-downcase name) headers)
             (gethash (string-upcase name) headers)))
        ((listp headers) (cdr (assoc name headers :test #'string-equal)))
        (t nil)))

(defun parse-retry-after (value)
  "\"3\" => 3.0, anything else NIL. An HTTP-date is not waited out."
  (let ((text (and (stringp value) (string-trim '(#\Space) value))))
    (when (and text (plusp (length text))
               (every (lambda (character)
                        (or (digit-char-p character) (char= character #\.)))
                      text))
      (let ((number (ignore-errors (read-from-string text))))
        (and (realp number) (plusp number) (float number))))))

;;; --- the one network seam ---------------------------------------------------

(defun exchange (path payload key)
  "POST PAYLOAD as JSON to PATH on KEY's account => (values TEXT STATUS HEADERS)."
  (let ((url (concatenate 'string (base-url) path))
        (timeout (setting :timeout-seconds)))
    (handler-case
        (multiple-value-bind (body status headers)
            (nlk:http :post url
                      :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                                     (cons "content-type" "application/json")
                                     (cons "accept" "application/json"))
                      :content (nlk:encode-json-object payload)
                      :timeout timeout
                      :connect-timeout (min 10 timeout))
          (values body status headers))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" path (nle:transport-failure-label condition url))))))

(defun retryable-p (status)
  "The statuses the vendor says to wait out: 408, 429 and every 5xx."
  (and (integerp status)
       (or (= status 408) (= status 429) (<= 500 status 599))))

(defun retry-suffix (retries)
  "Empty until a retry happened; one retry stays singular."
  (case retries
    (0 "")
    (1 " (after 1 retry)")
    (t (format nil " (after ~d retries)" retries))))

(defun vendor-message (text)
  "The vendor's own words from an error body, capped."
  (let ((body (and (stringp text) (ignore-errors (nlk:decode-json text)))))
    (or (nlk:json-value body :string "error" "message")
        (let ((raw (clip (trimmed text) +detail-cap+)))
          (and (plusp (length raw)) raw)))))

(defun status-refusal (path status text retries)
  "One line naming the status, the vendor's words and what to do about it."
  (format nil "~a: HTTP ~@[~a~]~@[: ~a~]~a~@[ ~a~]"
          path (or status "no status")
          (and (stringp text) (let ((message (vendor-message text)))
                                (and message (clip message +detail-cap+))))
          (retry-suffix retries)
          (and (eql status 402) "Top up the DeepSeek account before retrying.")))

(defun retry-wait (headers attempt)
  "The wait before retry ATTEMPT + 1: Retry-After when affordable, else backoff."
  (let ((after (parse-retry-after (header-value headers "retry-after"))))
    (if (and after (<= after +retry-after-max+))
        after
        (+ (nth attempt +retry-backoff+) (random +retry-jitter+)))))

(defun post-json (path payload)
  "POST PAYLOAD to PATH => the decoded answer object; a failure is a refusal."
  (let ((key (api-key)))
    (when (not (plusp (length key))) (fail "~a" +missing-key+))
    (loop with attempt = 0
          do (multiple-value-bind (text status headers) (exchange path payload key)
               (cond
                 ((and (integerp status) (<= 200 status 299))
                  (let ((body (and (plusp (length (trimmed text)))
                                   (ignore-errors (nlk:decode-json text)))))
                    (unless (hash-table-p body)
                      (fail "~a answered no JSON object: ~s" path (clip (or text "") 200)))
                    (return body)))
                 ((and (retryable-p status) (< attempt (length +retry-backoff+)))
                  (funcall *sleep* (retry-wait headers attempt))
                  (incf attempt))
                 (t (fail "~a" (status-refusal path status text attempt))))))))

;;; --- search -----------------------------------------------------------------

(defun format-number (value)
  "VALUE as a short number, or NIL when it is not one."
  (cond ((integerp value) (princ-to-string value))
        ((floatp value) (format nil "~,2f" value))
        (t nil)))

(defun hit-line (out index row want-content)
  "One search hit: its number, title, url, description and vendor signals."
  (let* ((title (or (nlk:json-value row :string "title") ""))
         (url (or (nlk:json-value row :string "url") ""))
         (description (or (nlk:json-value row :string "summary")
                          (nlk:json-value row :string "snippet")
                          (nlk:json-value row :string "description")
                          ""))
         (signals (remove nil
                          (list (nlk:json-value row :string "date")
                                (nlk:json-value row :string "site_name")
                                (let ((score (format-number (nlk:json-value row :number "rerank_score"))))
                                  (and score (format nil "rerank ~a" score)))
                                (let ((authority (nlk:json-value row :integer "authority")))
                                  (and authority (>= authority 0)
                                       (format nil "authority ~a" authority)))))))
    (format out "~d. ~a~%~@[   ~a~%~]~@[   ~a~%~]~@[   ~{~a~^, ~}~%~]"
            index (if (plusp (length title)) title "(untitled)") url description signals)
    (when want-content
      (let ((content (nlk:json-value row :string "content")))
        (when (and content (plusp (length content)))
          (format out "   ~a~%" (clip content 2000)))))))

(defun format-search (query answer &key want-content)
  "ANSWER, one search response, as the numbered list the model reads."
  (let ((rows (nlk:json-value answer :array "results")))
    (if (or (null rows) (zerop (length rows)))
        (format nil "No DeepSeek results for \"~a\"." query)
        (with-output-to-string (out)
          (format out "~d DeepSeek result~:p for \"~a\":~%" (length rows) query)
          (loop for index from 1
                for row across rows
                when (hash-table-p row)
                  do (hit-line out index row want-content))))))

(defun search-request (query &key count need-content)
  "QUERY against the search endpoint => the decoded answer."
  (let ((payload (nlk:make-json-object "query" (clamp-query query) "count" count)))
    (when need-content (setf (gethash "need_content" payload) t))
    (let ((where (localization)))
      (unless (zerop (length where)) (setf (gethash "localization" payload) where)))
    (post-json +search-path+ payload)))

(define-verb search (query &key (count nil) (need-content nil))
  "Search the web through DeepSeek and answer the hits as a numbered list."
  (let* ((query (trimmed (princ-to-string query)))
         (want (if (null need-content) (setting :need-content) need-content)))
    (when (zerop (length query)) (fail "a query is required"))
    (format-search query
                   (search-request query
                                   :count (max 1 (min +count-max+ (or count (setting :count))))
                                   :need-content want)
                   :want-content want)))

;;; --- fetch ------------------------------------------------------------------

(defun format-page (url answer)
  "ANSWER, one fetch response, as the page: title, then its Markdown."
  (let* ((title (nlk:json-value answer :string "title"))
         (content (unescape-markdown (or (nlk:json-value answer :string "content") "")))
         (limit (page-limit)))
    (with-output-to-string (out)
      (format out "~a~@[~%# ~a~]~%" url title)
      (when (plusp (length content))
        (format out "~a~%" (clip content limit)))
      (when (> (length content) limit)
        (format out "[cut at ~d of ~d characters; fetch it again for the rest]~%"
                limit (length content))))))

(defun url-list (urls)
  "URLS as a list of non-empty trimmed strings: a string is one URL."
  (let ((items (cond ((stringp urls) (list urls))
                     ((listp urls) urls)
                     ((vectorp urls) (coerce urls 'list))
                     (t nil))))
    (remove-if-not (lambda (url) (and (stringp url) (plusp (length (trimmed url)))))
                   (mapcar #'trimmed items))))

(define-verb fetch (urls)
  "Fetch each URL as Markdown through DeepSeek; one page's failure does not sink the rest."
  (let ((pages (url-list urls)))
    (when (null pages) (fail "a url is required"))
    (with-output-to-string (out)
      (loop for url in pages
            for first = t then nil
            do (unless first (terpri out))
               (handler-case
                   (write-string (format-page url (post-json +fetch-path+
                                                             (nlk:make-json-object "url" url)))
                                 out)
                 (deepseek-search-error (condition)
                   (format out "~a~%ERROR: ~a~%" url condition)))))))

;;; --- the tools --------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type
                                                                     "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *search-schema*
  (tool-schema
   "Search the web through DeepSeek and answer ranked hits: title, url, description, and the vendor's date, site and quality signals. Use it to discover pages; fetch a url you already know with deepseek_fetch. It is billed per successful call, so ask a real question rather than rewording the last one."
   '(("query" "string" "What to search for, at most 128 characters (a longer one is cut at a word boundary)." t)
     ("count" "integer" "How many hits (default: the section's count, at most 50)." nil)
     ("need_content" "boolean" "Also carry each hit's page text as Markdown (large; default: the section's need_content)." nil)))
  "The deepseek_search tool.")

(defparameter *fetch-schema*
  (tool-schema
   "Read a page as Markdown through DeepSeek's fetch endpoint, one URL per request, billed per successful call. Use it after deepseek_search named the url, or on a url the operator gave you."
   '(("url" "string" "The page to read." nil)
     ("urls" "array" "Several pages at once, read one after another." nil)))
  "The deepseek_fetch tool.")

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the ERROR: line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: deepseek-search: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (trimmed value))))
        (trimmed value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-integer (args name)
  "ARGS' NAME as an integer, or NIL."
  (let ((value (gethash name args)))
    (and (integerp value) value)))

(defun argument-boolean (args name default)
  "ARGS' NAME as a boolean, DEFAULT when the call did not carry it."
  (multiple-value-bind (value present) (gethash name args)
    (if present (and value t) default)))

(defun tool-search (args)
  (answering
    (let* ((query (argument-text args "query"))
           (want (argument-boolean args "need_content" (setting :need-content))))
      (format-search
       query
       (search-request query
                       :count (max 1 (min +count-max+
                                          (or (argument-integer args "count") (setting :count))))
                       :need-content want)
       :want-content want))))

(defun tool-fetch (args)
  (answering
    (let ((pages (append (url-list (gethash "url" args))
                           (url-list (gethash "urls" args)))))
      (when (null pages) (fail "url is required"))
      (fetch pages))))

;;; --- the manual --------------------------------------------------------------

(defparameter +primer+
  "DeepSeek Search & Fetch is available through the nodecode-deepseek-search cell: two tools and
two Lisp functions in the deepseek-search: package, called through eval. Both answer a string.
  deepseek_search (query, count?, need_content?)   ranked hits: number, title, url, description,
      then the vendor's date, site, rerank score and authority when it sends them. count is at
      most 50; a query over 128 characters is clamped at a word boundary.
  deepseek_fetch (url, or urls)   the page as Markdown, one request per url, cut at max_chars.
      (deepseek-search:search \"query\" :count 5), (deepseek-search:fetch \"https://...\")
Use search to discover and fetch a url you already know; cite the urls in your answer. It is
billed per successful call to the operator's DeepSeek account ($0.75 per 1,000 requests), so ask
once, read the hits, and do not reword a question you already asked. A 402 means the account is
out of credit; it is said in the answer. ERROR: DEEPSEEK-SEARCH-ERROR names the problem; a
missing key tells the operator to set DEEPSEEK_API_KEY."
  "What (help :deepseek-search) answers while the cell runs.")

(defun check-config ()
  "Refuse a start with no key to send: the section, its variable, or DEEPSEEK_API_KEY."
  (when (zerop (length (api-key)))
    (nlk:config-error "~a" +missing-key+)))

(nle:define-cell deepseek-search
  (:section ("deepseek-search")
    (:guide "get a key from platform.deepseek.com/api_keys, then either set api_key here or put it in the variable api_key_env names (DEEPSEEK_API_KEY is read either way); count is how many hits a search asks for, max_chars how much of a fetched page comes back; next step: ask the model to search for something")
    ("api_key" :secret
     :doc "the DeepSeek API key (billed per successful request)")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here")
    ("base_url" :string :default "https://api.deepseek.com"
     :doc "the search and fetch origin; only tests point it elsewhere")
    ("count" :integer :default 5 :min 1
     :doc "how many hits one search asks for unless the call says otherwise (at most 50)")
    ("need_content" :boolean :default nil
     :doc "carry each hit's page text as Markdown by default (large: a single hit can be 20 kB)")
    ("localization" :string :default ""
     :doc "an IPv4 or IPv6 address the search localises results to; empty sends nothing")
    ("max_chars" :integer :default 8000 :min 200
     :doc "how much of one fetched page comes back, in characters")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one request's ceiling"))
  (:help :deepseek-search
         "deepseek-search: deepseek_search finds pages through DeepSeek, deepseek_fetch reads one as Markdown"
         +primer+)
  (:tool "deepseek_search" *search-schema* 'tool-search)
  (:tool "deepseek_fetch" *fetch-schema* 'tool-fetch)
  (:start #'check-config))
