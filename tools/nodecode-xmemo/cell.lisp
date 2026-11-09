;;;; cell.lisp --- XMemo: cloud long-term memory, six tools and /xmemo.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of yonro's hermes-xmemo-plugin (see NOTICE). The wire is the
;;;; original's: X-API-Key against https://xmemo.dev, POST /v1/recall/context
;;;; for a bounded context pack, GET /v1/memories/search for semantic search,
;;;; GET /v1/memories/<id>/explain for one memory read whole, POST
;;;; /v1/remember for a durable fact and POST /v1/update_state for working
;;;; state with a TTL.
;;;;
;;;; What is carried over: the six core tool schemas and their arguments,
;;;; the bilingual search aliases, the RFC 3339 filter validation, the
;;;; bounded context pack, the provenance labels, and the five-failure
;;;; breaker reads pause behind. What the harness gives instead of a provider
;;;; interface: the section is the config, the tools are
;;;; NLE:REGISTER-TOOL registrations, /xmemo is a command, and a finished
;;;; turn arrives on the :frame point.

(defpackage #:nodecode-xmemo
  (:use #:cl)
  (:export #:xmemo-error #:xmemo-error-detail))

(in-package #:nodecode-xmemo)

(nlk:define-peripheral xmemo :not-running t :lock "xmemo")

(defparameter +default-root+ "https://xmemo.dev"
  "XMemo's API, where requests go unless base_url names another.")

(defparameter +breaker-threshold+ 5
  "Consecutive backend failures before reads pause.")

(defparameter +breaker-cooldown+ 120
  "Seconds an open breaker waits before it tries the backend again.")

(defparameter +alias-groups+
  '((("xmemo" "memory-os" "memory os" "记忆库" "记忆体")
     . "XMemo Memory OS xmemo memory-os")
    (("记忆" "回忆" "memory" "memories" "recall")
     . "memory memories recall saved facts")
    (("删除" "删掉" "移除" "forget" "delete" "deleted")
     . "delete deleted forget removal trash")
    (("恢复" "还原" "找回" "restore" "recover" "recovery")
     . "restore recover recovery undo")
    (("改进" "优化" "方案" "计划" "improve" "improvement" "plan")
     . "improvement plan optimization proposal")
    (("回收站" "垃圾箱" "trash" "recycle")
     . "trash recycle bin deleted memories")
    (("误删" "事故" "incident" "accidental")
     . "incident accidental deletion recovery")
    (("chatgpt" "claude" "讨论" "discussion")
     . "ChatGPT Claude discussion"))
  "The original's bilingual alias table: a query that hits a trigger gains the
expansion's words, so one mixed-language search finds what two would.")

(defparameter +temporal-fields+
  '("since" "until" "time_order" "owner_timezone" "reference_time"
    "temporal_axis_mode" "query_mode")
  "The optional filters every read accepts, as the wire spells them.")

(defparameter +temporal-enums+
  '(("time_order" "asc" "desc")
    ("temporal_axis_mode" "created_at" "updated_at" "split_filter_created_order_updated")
    ("query_mode" "topic" "activity_log"))
  "The closed sets the original validates before a request leaves.")

(defparameter +instant-pattern+
  "^\\d{4}-\\d{2}-\\d{2}[Tt]\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?(?:[Zz]|[+-]\\d{2}:\\d{2})$"
  "An RFC 3339 instant with a timezone offset, as the original requires.")

(defvar *breaker-failures* 0
  "Consecutive backend failures on the read path, for the breaker.")

(defvar *breaker-until* 0
  "Universal time the open breaker may try the backend again.")

(defvar *instance-id* ""
  "This start's opaque agent-instance id, sent as provenance.")

(defvar *turn-inputs* (make-hash-table :test #'equal)
  "Each running turn's request text, by turn id, until the turn completes.")

;;; --- the section, read back --------------------------------------------------

(defun api-key ()
  "The key the section names, inline or by environment variable, else XMEMO_KEY."
  (or (let ((inline (setting :api-key))) (and inline (plusp (length inline)) inline))
      (let ((variable (setting :api-key-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))
      (uiop:getenv "XMEMO_KEY")))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-root+)))

(defun agent-id () (setting :agent-id))

(defun write-bucket () (setting :bucket))

(defun write-scope () (setting :scope))

(defun read-bucket ()
  (let ((given (setting :read-bucket)))
    (if (and given (plusp (length given))) given "%")))

(defun read-scope ()
  (let ((given (setting :read-scope)))
    (and given (plusp (length given)) given)))

(defun check-config ()
  "Refuse a section that could never reach the backend, naming the member."
  (when (null (api-key))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "xmemo: needs a key: set XMEMO_KEY, or \"api_key\" "
                                 "(or \"api_key_env\") in the xmemo section"))))

(defun random-hex (&optional (bytes 16))
  (let ((*random-state* (make-random-state t)))
    (with-output-to-string (out)
      (loop repeat bytes do (format out "~2,'0x" (random 256))))))

(defun idempotency-key () (format nil "nc-~a-~a" (get-universal-time) (random-hex 6)))

;;; --- the wire -----------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER --- an HTTP body --- as a decoded value; an empty body is an object."
  (let ((body (if (stringp answer) (nlk:body-text answer) answer)))
    (cond ((and (stringp body) (plusp (length (nlk:trimmed body))))
           (handler-case (nlk:decode-json body) (error () (nlk:json-object))))
          ((vectorp body) body)
          (t (nlk:json-object)))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "message")
        (nlk:json-value body :string "error")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun auth-headers ()
  (list (cons "x-api-key" (or (api-key) ""))
        (cons "x-memory-os-agent-id" (agent-id))
        (cons "x-memory-os-agent-instance-id" *instance-id*)))

(defun without-nil (object)
  "OBJECT without its null members, as the original drops unset parameters."
  (let ((copy (nlk:make-json-object)))
    (maphash (lambda (key value) (unless (null value) (setf (gethash key copy) value))) object)
    copy))

(defun request-url (path params &aux (base (root)))
  (let ((kept (loop for (key . value) in params
                    for text = (and value (princ-to-string value))
                    when (and text (plusp (length text)))
                      collect (cons key text))))
    (format nil "~a~a~@[?~a~]" base path (and kept (quri:url-encode-params kept)))))

(defun call (method path &key params body key)
  "METHOD PATH on the configured root: the decoded answer.
A failure below HTTP, a refused status, or a 404 signals XMEMO-ERROR."
  (let* ((url (request-url path params))
         (limit (setting :timeout-seconds))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (append (cons '("content-type" . "application/json") (auth-headers))
                                     (when key (list (cons "idempotency-key" key)
                                                     (cons "x-idempotency-key" key))))
                    :content body
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" (string-downcase (string method))
              (nle:transport-failure-label condition url))))
    (cond ((and (integerp status) (< status 300)) (decode-body answer))
          ((eql status 404) (fail "~a: HTTP 404, nothing of that id on ~a" path (root)))
          (t (fail "~a ~a: HTTP ~d~@[: ~a~]" (string-upcase (string method)) path status
                   (body-error-text answer))))))

(defun write-call (path body key)
  "POST BODY's JSON text to PATH under KEY, the idempotency key of this write."
  (setf (gethash "idempotency_key" body) key)
  (call :post path :body (nlk:encode-json-object body) :key key))

(defun results-of (body)
  "BODY's result objects: the \"results\" array of an object, or the array itself."
  (coerce (or (cond ((hash-table-p body) (nlk:json-value body :array "results"))
                    ((vectorp body) (and (not (stringp body)) body)))
               #())
          'list))

;;; --- the breaker ----------------------------------------------------------------

(defun breaker-open-p ()
  (cond ((< *breaker-failures* +breaker-threshold+) nil)
        ((< (get-universal-time) *breaker-until*) t)
        (t (setf *breaker-failures* 0) nil)))

(defun record-success () (setf *breaker-failures* 0))

(defun record-failure ()
  (when (>= (incf *breaker-failures*) +breaker-threshold+)
    (setf *breaker-until* (+ (get-universal-time) +breaker-cooldown+))))

(defmacro reading (&body body)
  "BODY against the backend, behind the breaker that pauses a failing backend."
  `(progn
     (when (breaker-open-p)
       (fail "reads are paused after ~d failures in a row; the backend is tried again shortly"
             +breaker-threshold+))
     (handler-case (multiple-value-prog1 (progn ,@body) (record-success))
       (xmemo-error (condition) (record-failure) (error condition)))))

;;; --- the model's answer -----------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: xmemo: ~a" condition)))))

(defun argument-optional (args name)
  "ARGS' NAME as a trimmed non-empty string, or NIL."
  (let ((value (gethash name args)))
    (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (or (argument-optional args name)
      (fail "~a is required" (substitute #\_ #\- name))))

(defun argument-integer (args name default minimum maximum)
  (let ((value (gethash name args)))
    (if (integerp value) (max minimum (min value maximum)) default)))

(defun argument-number (args name)
  "ARGS' NAME as a double, or NIL: a schema number arrives decoded, never read."
  (let ((value (gethash name args)))
    (and (realp value) (float value 1.0d0))))

;;; --- temporal filters -------------------------------------------------------------

(defun instant-seconds (text field)
  "TEXT, an RFC 3339 instant, as universal seconds; a refusal names FIELD.
Fractional seconds are carried in TEXT unchanged and compared to the second."
  (unless (cl-ppcre:scan +instant-pattern+ text)
    (fail "~a must be an RFC 3339 timestamp with a timezone offset" field))
  (let* ((year (parse-integer text :start 0 :end 4))
         (month (parse-integer text :start 5 :end 7))
         (day (parse-integer text :start 8 :end 10))
         (hour (parse-integer text :start 11 :end 13))
         (minute (parse-integer text :start 14 :end 16))
         (second (parse-integer text :start 17 :end 19))
         (zulu (member (char text (1- (length text))) '(#\Z #\z)))
         (offset (if zulu "+00:00" (subseq text (- (length text) 6))))
         (sign (if (char= (char offset 0) #\-) -1 1))
         (offset-hours (parse-integer offset :start 1 :end 3))
         (offset-minutes (parse-integer offset :start 4 :end 6)))
    (when (or (> offset-hours 23) (> offset-minutes 59) (> hour 23) (> minute 59) (> second 59))
      (fail "~a has an invalid time or timezone offset" field))
    (let ((seconds (handler-case (encode-universal-time second minute hour day month year 0)
                     (error () (fail "~a is not a calendar date and time" field)))))
      (- seconds (* sign (+ (* offset-hours 3600) (* offset-minutes 60)))))))

(defun validate-temporal (args &key allow-hypothetical)
  "The temporal filters ARGS carries, validated, as an alist of (NAME . TEXT)."
  (let ((kept '()))
    (dolist (name +temporal-fields+)
      (let ((value (gethash name args)))
        (when value
          (unless (and (stringp value) (plusp (length (nlk:trimmed value))))
            (fail "~a must be a non-empty string" name))
          (let ((text (nlk:trimmed value))
                (allowed (cdr (assoc name +temporal-enums+ :test #'string=))))
            (when (and allowed (not (member text allowed :test #'string=)))
              (fail "~a must be one of ~{~a~^, ~}" name allowed))
            (push (cons name text) kept)))))
    (when allow-hypothetical
      (let ((value (gethash "hypothetical_answer" args)))
        (when value
          (unless (and (stringp value) (plusp (length (nlk:trimmed value))))
            (fail "hypothetical_answer must be a non-empty string"))
          (push (cons "hypothetical_answer" (nlk:trimmed value)) kept))))
    (setf kept (nreverse kept))
    (dolist (name '("since" "until" "reference_time"))
      (let ((value (cdr (assoc name kept :test #'string=))))
        (when value (instant-seconds value name))))
    (let ((start (cdr (assoc "since" kept :test #'string=)))
          (end (cdr (assoc "until" kept :test #'string=))))
      (when (and start end (<= (instant-seconds end "until") (instant-seconds start "since")))
        (fail "until must be strictly later than since")))
    (when (and (equal "activity_log" (cdr (assoc "query_mode" kept :test #'string=)))
               (not (and (assoc "since" kept :test #'string=)
                         (assoc "until" kept :test #'string=))))
      (fail "activity_log requires both since and until"))
    kept))

;;; --- the bilingual aliases ----------------------------------------------------------

(defun ascii-word-char-p (character)
  (or (alphanumericp character) (char= character #\_)))

(defun word-trigger-p (trigger)
  (every #'ascii-word-char-p trigger))

(defun trigger-present-p (text trigger)
  "TEXT contains TRIGGER: on word boundaries for an ASCII word, plainly otherwise."
  (if (not (word-trigger-p trigger))
      (not (null (search trigger text)))
      (loop with length = (length trigger)
            for start = (search trigger text)
            then (search trigger text :start2 (1+ start))
            while start
            thereis (and (or (zerop start)
                             (not (ascii-word-char-p (char text (1- start)))))
                         (or (>= (+ start length) (length text))
                             (not (ascii-word-char-p (char text (+ start length)))))))))

(defun augment-query (query)
  "QUERY with the alias words its triggers name, so one search spans both languages."
  (let ((cleaned (nlk:trimmed (or query ""))))
    (if (zerop (length cleaned))
        cleaned
        (let ((lowered (string-downcase cleaned))
              (additions '()))
          (dolist (group +alias-groups+)
            (when (some (lambda (trigger) (trigger-present-p lowered trigger)) (car group))
              (dolist (word (uiop:split-string (cdr group) :separator " "))
                (unless (trigger-present-p lowered (string-downcase word))
                  (pushnew word additions :test #'string=)))))
          (if (null additions)
              cleaned
              (format nil "~a ~{~a~^ ~}" cleaned (nreverse additions)))))))

;;; --- formatting ---------------------------------------------------------------------

(defun memory-id (item)
  (or (nlk:json-value item :string "id") (nlk:json-value item :string "memory_id")))

(defun memory-content (item)
  (nlk:json-value item :text "content"))

(defun memory-score (item)
  (or (nlk:json-value item :number "similarity") (nlk:json-value item :number "score")))

(defun boundary-label (item)
  "ITEM's agent-boundary provenance, self or other_agent, as the original shows it."
  (let ((boundary (nlk:json-value item :any "agent_boundary")))
    (cond ((stringp boundary) boundary)
          ((hash-table-p boundary)
           (or (nlk:json-value boundary :string "boundary")
               (nlk:json-value boundary :string "agent_boundary")
               (nlk:json-value boundary :string "relationship")
               (nlk:json-value boundary :string "ownership")
               ""))
          (t (or (nlk:json-value item :string "ownership") "")))))

(defun with-memories (results)
  (remove-if-not (lambda (item) (and (hash-table-p item) (memory-content item))) results))

(defun preview (text max)
  (if (> (length text) max) (concatenate 'string (subseq text 0 max) "...") text))

(defun format-search-results (query results)
  "RESULTS as the lines the model reads: one per fact, its id and its score."
  (let ((kept (with-memories results)))
    (if (null kept)
        (format nil "No relevant XMemo memories found for ~s." query)
        (with-output-to-string (out)
          (format out "~d memories for ~s:" (length kept) query)
          (loop for item in kept
                for index from 1
                do (format out "~% ~d. [~a]~@[ [~a]~]~@[ ~a~]~@[ (sim ~,3f)~]~@[ id=~a~]"
                           index
                           (or (nlk:json-value item :string "memory_type") "semantic")
                           (let ((boundary (boundary-label item)))
                             (and (plusp (length boundary)) boundary))
                           (nlk:json-value item :string "path")
                           (memory-score item)
                           (memory-id item))
                   (format out "~%    ~a" (nlk:trimmed (memory-content item))))))))

(defun format-list-results (path results)
  (let ((kept (with-memories results)))
    (if (null kept)
        (format nil "No XMemo memories found~@[ under ~s~]."
                (and path (plusp (length path)) path))
        (with-output-to-string (out)
          (format out "~d memories~@[ under ~s~]:" (length kept)
                  (and path (plusp (length path)) path))
          (dolist (item kept)
            (format out "~% - ~@[~a ~]~a" (memory-id item)
                    (preview (nlk:trimmed (memory-content item)) 150)))))))

(defun format-context (context)
  "CONTEXT --- a /v1/recall/context answer --- as the block the model reads."
  (let ((text (nlk:json-value context :text "context_text")))
    (cond (text text)
          (t (let ((items (nlk:json-value context :array "items")))
               (if (or (null items) (zerop (length items)))
                   "No relevant XMemo context found."
                   (with-output-to-string (out)
                     (loop for item across items
                           for index from 1
                           do (format out "~d.~@[ [~a]~] ~a~%"
                                      index
                                      (let ((boundary (boundary-label item)))
                                        (and (plusp (length boundary)) boundary))
                                      (or (memory-content item) ""))))))))))

;;; --- the API reads ---------------------------------------------------------------

(defun search-memories (query &key limit memory-type path temporal)
  "The memories nearest QUERY, as the backend's own result objects."
  (results-of
   (reading
     (call :get "/v1/memories/search"
           :params (append (list (cons "query" query)
                                 (cons "bucket" (read-bucket))
                                 (cons "scope" (read-scope))
                                 (cons "memory_type" (or memory-type "%"))
                                 (cons "limit" limit))
                           (when path (list (cons "path" path)))
                           temporal)))))

(defun recall-context (query &key max-items memory-type temporal)
  (reading
    (call :post "/v1/recall/context"
          :body (nlk:encode-json-object
                 (without-nil
                  (let ((body (nlk:make-json-object
                               "query" query
                               "bucket" (read-bucket)
                               "scope" (read-scope)
                               "max_items" max-items
                               "max_tokens" (setting :max-tokens)
                               "memory_type" (or memory-type "auto")
                               "prefer_working" t)))
                    (dolist (pair temporal body)
                      (setf (gethash (car pair) body) (cdr pair)))))))))

(defun explain-memory (memory-id)
  (reading (call :get (format nil "/v1/memories/~a/explain" memory-id))))

;;; --- the tools ---------------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES --- (NAME TYPE DOC REQUIRED-P) ... --- as a tool schema."
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
   "Search all visible user-owned XMemo memories by natural-language query, including memories written by other agents connected to the same XMemo account. Returns relevant facts ranked by semantic similarity, with the id each further tool takes. Prefer one mixed multilingual query that combines Chinese and English keywords over several paraphrased searches. Use this when the user asks about saved information or when prior context could change the answer."
   '(("query" "string" "What to search for; put both language variants in one query, e.g. '删除恢复记忆 改进方案 delete restore memory improvement'." t)
     ("limit" "integer" "Max results (default: the section's search_limit, at most 50)." nil)
     ("memory_type" "string" "Optional memory type filter (semantic, episodic, working, identity, procedural)." nil)
     ("since" "string" "Inclusive start of [since, until); RFC 3339 with a timezone offset." nil)
     ("until" "string" "Exclusive end of [since, until); RFC 3339, strictly later than since." nil)
     ("time_order" "string" "asc (oldest first) or desc (newest first)." nil)
     ("owner_timezone" "string" "IANA timezone name, such as Asia/Tokyo." nil)
     ("reference_time" "string" "An optional reference instant, RFC 3339 with a timezone offset." nil)
     ("temporal_axis_mode" "string" "created_at, updated_at, or split_filter_created_order_updated." nil)
     ("query_mode" "string" "topic retrieves relevant memories; activity_log needs both since and until." nil)))
  "The xmemo_search tool, as the original's SEARCH_SCHEMA has it.")

(defparameter *get-schema*
  (tool-schema
   "Retrieve the full, complete content of a single memory from XMemo by its memory_id. Use this when search or recall returned an item whose content was truncated, or when you need the complete text and metadata of a specific memory."
   '(("memory_id" "string" "The exact ID of the memory to retrieve." t)))
  "The xmemo_get tool.")

(defparameter *list-schema*
  (tool-schema
   "List memories under an optional logical path or category in XMemo. Use this to browse available knowledge or view what is stored in a specific category (e.g. 'notes' or 'nodecode/preferences')."
   '(("path" "string" "Optional path prefix or category to filter by." nil)
     ("limit" "integer" "Max results to return (default 20, at most 100)." nil)))
  "The xmemo_list tool.")

(defparameter *recall-schema*
  (tool-schema
   "Build a bounded, ranked context pack from all visible user-owned XMemo memories, including memories written by other agents on the same account. Use when you need a focused memory summary rather than raw search results."
   '(("query" "string" "What to recall context for." t)
     ("max_items" "integer" "Max memory items to include (default: the section's max_items, at most 20)." nil)
     ("memory_type" "string" "Optional memory type filter (semantic, episodic, working, identity, procedural)." nil)
     ("hypothetical_answer" "string" "An optional hypothetical answer or semantic expansion to guide retrieval." nil)
     ("since" "string" "Inclusive start of [since, until); RFC 3339 with a timezone offset." nil)
     ("until" "string" "Exclusive end of [since, until); RFC 3339, strictly later than since." nil)
     ("time_order" "string" "asc (oldest first) or desc (newest first)." nil)
     ("owner_timezone" "string" "IANA timezone name, such as Asia/Tokyo." nil)
     ("reference_time" "string" "An optional reference instant, RFC 3339 with a timezone offset." nil)
     ("temporal_axis_mode" "string" "created_at, updated_at, or split_filter_created_order_updated." nil)
     ("query_mode" "string" "topic retrieves relevant memories; activity_log needs both since and until." nil)))
  "The xmemo_recall_context tool.")

(defparameter *remember-schema*
  (tool-schema
   "Save a durable fact to XMemo. Use for explicit preferences, decisions, conventions, architecture notes, action items, or bug-fix context that should survive across sessions. Skip transient chat."
   '(("content" "string" "The fact to remember; one clear concept per call." t)
     ("path" "string" "Logical path or category, e.g. 'notes/decisions' or 'nodecode/preferences'." t)
     ("memory_type" "string" "semantic, episodic, procedural, working or identity (default semantic)." nil)
     ("importance" "number" "Importance from 0.0 to 1.0." nil)))
  "The xmemo_remember tool.")

(defparameter *state-schema*
  (tool-schema
   "Save the current working state to XMemo with a TTL. Use for the active task, the next action, or a blocker during long-running work so a later session can resume."
   '(("current_task" "string" "Short description of the active task." nil)
     ("next_action" "string" "The very next step to take." nil)
     ("blocked_reason" "string" "Why work is blocked, if applicable." nil)
     ("ttl_seconds" "integer" "Time to live in seconds (default 86400, one day)." nil)))
  "The xmemo_update_state tool.")

(defun tool-search (args)
  (answering
    (let* ((query (argument-text args "query"))
           (temporal (validate-temporal args))
           (results (search-memories (augment-query query)
                                     :limit (argument-integer args "limit"
                                                              (setting :search-limit) 1 50)
                                     :memory-type (argument-optional args "memory_type")
                                     :temporal temporal)))
      (format-search-results query results))))

(defun tool-get (args)
  (answering
    (let* ((id (argument-text args "memory_id"))
           (body (explain-memory id))
           (data (or (nlk:json-value body :object "memory") body))
           (content (nlk:json-value data :text "content")))
      (unless content (fail "memory ~a has no content on ~a" id (root)))
      (format nil "~a~@[~%path: ~a~]~@[~%type: ~a~]~@[~%created: ~a~]~@[~%id: ~a~]"
              (nlk:trimmed content)
              (nlk:json-value data :string "path")
              (nlk:json-value data :string "memory_type")
              (nlk:json-value data :string "created_at")
              (or (nlk:json-value data :string "id") id)))))

(defun tool-list (args)
  (answering
    (let* ((path (or (argument-optional args "path") "%"))
           (search-path (if (or (equal path "%")
                                (member (char path (1- (length path))) '(#\% #\*)))
                            path
                            (concatenate 'string path "%")))
           (results (search-memories "*"
                                     :limit (argument-integer args "limit" 20 1 100)
                                     :path search-path)))
      (format-list-results (unless (equal path "%") path) results))))

(defun tool-recall (args)
  (answering
    (let* ((query (argument-text args "query"))
           (temporal (validate-temporal args :allow-hypothetical t))
           (context (recall-context query
                                     :max-items (argument-integer args "max_items"
                                                                  (setting :max-items) 1 20)
                                     :memory-type (argument-optional args "memory_type")
                                     :temporal temporal)))
      (format-context context))))

(defun tool-remember (args)
  (answering
    (let* ((content (argument-text args "content"))
           (path (argument-text args "path"))
           (body (nlk:make-json-object "content" content
                                        "path" path
                                        "bucket" (write-bucket)
                                        "scope" (write-scope)
                                        "memory_type" (or (argument-optional args "memory_type") "semantic")
                                        "dedupe" t))
           (importance (argument-number args "importance")))
      (when importance (setf (gethash "importance" body) importance))
      (let ((answer (write-call "/v1/remember" body (idempotency-key))))
        (format nil "Stored under ~s~@[ as memory ~a~]."
                path (nlk:json-value answer :string "id"))))))

(defun tool-update-state (args)
  (answering
    (let ((task (or (argument-optional args "current_task") ""))
          (next (or (argument-optional args "next_action") ""))
          (blocked (or (argument-optional args "blocked_reason") ""))
          (ttl (argument-integer args "ttl_seconds" 86400 60 2592000)))
      (when (and (string= "" task) (string= "" next) (string= "" blocked))
        (fail "at least one of current_task, next_action or blocked_reason is required"))
      (let ((body (nlk:make-json-object "state_key" "active_task"
                                         "bucket" (write-bucket)
                                         "scope" (write-scope)
                                         "ttl_seconds" ttl)))
        (unless (string= "" task) (setf (gethash "current_task" body) task))
        (unless (string= "" next) (setf (gethash "next_action" body) next))
        (unless (string= "" blocked) (setf (gethash "blocked_reason" body) blocked))
        (write-call "/v1/update_state" body (idempotency-key))
        (format nil "Working state saved; it lapses after ~d seconds unless refreshed." ttl)))))

;;; --- the opt-in timeline -------------------------------------------------------------

(defun remember-turn-input (turn text)
  (when (and (stringp turn) (stringp text) (plusp (length text)))
    (with-xmemo-lock (setf (gethash turn *turn-inputs*) text))))

(defun take-turn-input (turn)
  (with-xmemo-lock (prog1 (gethash turn *turn-inputs*) (remhash turn *turn-inputs*))))

(defun record-turn-event (turn session-id)
  "Record TURN's request on the XMemo timeline, best effort, when the section asks."
  (let ((text (take-turn-input turn)))
    (when text
      (handler-case
          (call :post "/v1/timeline/events"
                :body (nlk:encode-json-object
                       (nlk:make-json-object
                        "content" (preview text 240)
                        "event_type" "session_event"
                        "bucket" (write-bucket)
                        "scope" (write-scope)
                        "session_id" (or session-id ""))))
        (error (condition) (warn "xmemo: a turn was not recorded: ~a" condition))))))

(defun observe-frame (op)
  "Fold one published frame: a turn's request kept, a finished turn recorded."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when (and type turn *xmemo* (setting :capture-timeline))
      (cond ((equal type "turn.input_committed")
             (remember-turn-input turn (nlk:fact-message-content payload)))
            ((equal type "turn.completed")
             (record-turn-event turn (getf op :session-id)))))))

;;; --- the manual and /xmemo -----------------------------------------------------------

(defparameter +primer+
  "XMemo keeps a user-owned memory in the cloud: durable facts, a working state with a TTL,
and a bounded context pack, found again by meaning. (help :xmemo) is this page.
  xmemo_search {query, limit?, memory_type?, since?, until?, ...}   facts nearest a question, with ids
  xmemo_get {memory_id}                                            one memory, whole, untruncated
  xmemo_list {path?, limit?}                                       browse a path or category
  xmemo_recall_context {query, max_items?, hypothetical_answer?}    a bounded, ranked pack
  xmemo_remember {content, path, memory_type?, importance?}         store one durable fact
  xmemo_update_state {current_task?, next_action?, blocked_reason?} save working state with a TTL
Search or recall before answering anything that may depend on saved facts, and prefer one mixed
Chinese/English query over several paraphrases: the cell adds its own aliases. Remember only what
should survive the session: preferences, decisions, conventions, action items. Write paths are
stamped with the section's bucket and scope; reads span every bucket unless read_bucket narrows
them, which is how memories written by the account's other agents are found. results may carry
Agent_boundary/provenance, self or other_agent. A read that fails five times in a row pauses for
two minutes rather than hammering the backend. The xmemo section names the API key (XMEMO_KEY, or
api_key, or api_key_env), the base_url, and the bucket and scope new memories land in."
  "What (help :xmemo) answers while the cell runs.")

(defun status-line ()
  (format nil "XMemo: ~a, agent ~s; new memories go to ~a/~a, reads span ~a~@[/~a~].~%~
               /xmemo QUERY searches; the tools xmemo_search, xmemo_get, xmemo_list, xmemo_recall_context, xmemo_remember and xmemo_update_state do the rest."
          (root) (agent-id) (write-bucket) (write-scope) (read-bucket) (read-scope)))

(defun run-command (args session-id)
  "One /xmemo invocation: a query searches XMemo, no argument shows the backend."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (answering
          (format-search-results
           query
           (search-memories (augment-query query) :limit (setting :search-limit))))
        (status-line))))

;;; --- the cell -------------------------------------------------------------------------

(nle:define-cell xmemo
  (:section ("xmemo")
    (:guide "needs an XMemo API key: set XMEMO_KEY, or api_key (or api_key_env) here; bucket and scope say where writes land; next step: /xmemo shows the backend")
    ("api_key" :secret
     :doc "the XMemo API key")
    ("api_key_env" :env
     :doc "the environment variable holding the XMemo API key, when it is not written here (XMEMO_KEY is read either way)")
    ("base_url" :string :default "https://xmemo.dev"
     :doc "the XMemo API address")
    ("agent_id" :string :default "nodecode"
     :doc "the agent id every request is stamped with")
    ("instance_id" :string
     :doc "this install's opaque id; generated per start when unset")
    ("bucket" :string :default "work"
     :doc "the bucket new memories are written to")
    ("scope" :string :default "nodecode/default"
     :doc "the scope new memories are written to")
    ("read_bucket" :string :default "%"
     :doc "the bucket reads span; % means every bucket")
    ("read_scope" :string
     :doc "the scope reads span; empty means every scope")
    ("search_limit" :integer :default 10 :min 1
     :doc "how many memories one search asks for unless the call says otherwise")
    ("max_items" :integer :default 5 :min 1
     :doc "how many items one context pack carries unless the call says otherwise")
    ("max_tokens" :integer :default 900 :min 100
     :doc "the token budget of one context pack")
    ("capture_timeline" :boolean :default nil
     :doc "record each finished turn's request on the XMemo timeline")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "one request's ceiling"))
  (:help :xmemo "xmemo: XMemo cloud memory - xmemo_search recalls saved facts, xmemo_remember stores one, xmemo_recall_context builds a pack"
                 +primer+)
  (:command "xmemo" 'run-command
            :description "Show the XMemo backend, or search it"
            :argument-hint "QUERY")
  (:tool "xmemo_search" *search-schema* 'tool-search)
  (:tool "xmemo_get" *get-schema* 'tool-get)
  (:tool "xmemo_list" *list-schema* 'tool-list)
  (:tool "xmemo_recall_context" *recall-schema* 'tool-recall)
  (:tool "xmemo_remember" *remember-schema* 'tool-remember)
  (:tool "xmemo_update_state" *state-schema* 'tool-update-state)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-xmemo"))
  (:start (lambda ()
            (check-config)
            (setf *breaker-failures* 0
                  *breaker-until* 0
                  *turn-inputs* (make-hash-table :test #'equal)
                  *instance-id* (let ((given (setting :instance-id)))
                                  (if (and given (plusp (length given))) given (random-hex))))
            (nle:on-stop (lambda ()
                           (setf *turn-inputs* (make-hash-table :test #'equal)
                                 *instance-id* ""))))))
