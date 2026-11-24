;;;; cell.lisp --- Severian memory: cited recall over its HTTP API, five tools, /severian.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of Sahil-SS9/Severian's Hermes memory-provider plugin (see
;;;; NOTICE). The original hands Hermes a MemoryProvider that runs inside the
;;;; Python engine; the cell talks to the same engine over its HTTP API, which
;;;; is the engine's own boundary: five scope dimensions on every request, an
;;;; observation in, cited memories out.
;;;;
;;;; What came across: the tool names and their jobs (severian_search,
;;;; severian_ingest, severian_get_observation, severian_search_shared), the
;;;; five-dimensional scope as the security boundary, the shared read over an
;;;; explicit list of agent ids, and a finished turn written as one
;;;; observation ("user: ...\nassistant: ...").
;;;;
;;;; What did not: correct and forget, which the HTTP API does not route (the
;;;; engine's Python service carries them), task progress, and the in-process
;;;; context assembler. README's "Not ported" says why.

(defpackage #:nodecode-severian
  (:use #:cl)
  (:export #:severian-error #:severian-error-detail))

(in-package #:nodecode-severian)

(nlk:define-peripheral severian :not-running t :lock "severian")

(defparameter +api-prefix+ "/api/v1"
  "The engine's versioned HTTP API, as src/severian/api/app.py prefixes it.")

(defparameter +sentence-ends+ '(#\. #\! #\? #\。 #\！ #\？)
  "A clipped turn breaks after the last of these inside the budget.")

(defparameter +breaker-threshold+ 5
  "Consecutive capture failures before the breaker pauses the turn path.")

(defparameter +breaker-cooldown+ 120
  "Seconds an open breaker waits before it tries the server again.")

(defparameter +queued-turns+ 8
  "Turns waiting to be written: the newest are kept, so a stall loses the oldest.")

(defvar *queued* '()
  "Finished turns waiting to be written, newest first: (TURN-ID . CONTENT).")

(defvar *turns* (make-hash-table :test #'equal)
  "Each running turn's two texts, by turn id: (USER . ANSWER), until it completes.")

(defvar *worker* nil
  "The capture worker (NLK:WORKER-START) while the cell runs, or NIL.")

(defvar *breaker-failures* 0
  "Consecutive capture failures, for the breaker.")

(defvar *breaker-until* 0
  "Universal time the open breaker may try the server again.")

;;; --- the section, read back --------------------------------------------------

(defun refuse (control &rest arguments)
  "A CONFIG-REFUSAL whose detail names what to fix."
  (error 'nlk:config-refusal :detail (apply #'format nil control arguments)))

(defun root ()
  "The server address this cell talks to, without a trailing slash."
  (string-right-trim "/" (or (setting :base-url) "")))

(defun api-key ()
  "The Bearer key, inline or by environment variable, or NIL."
  (or (let ((inline (setting :api-key))) (and inline (plusp (length inline)) inline))
      (let ((variable (setting :api-key-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))))

(defun check-config ()
  "Refuse a section that could never reach a server, naming the member."
  (let ((base (root)))
    (when (zerop (length base))
      (refuse "severian: \"base_url\" is required: the address of your severian server, ~
e.g. http://127.0.0.1:8000, started with `severian serve --port 8000'"))
    (unless (or (uiop:string-prefix-p "http://" base) (uiop:string-prefix-p "https://" base))
      (refuse "severian: \"base_url\" must start with http:// or https://, not ~s" base))))

(defun url-encode (text)
  (quri:url-encode (or text "") :encoding :utf-8))

(defun scope-object (&key (agent-id (setting :agent-id)))
  "The five scope dimensions as the API's ScopeModel wants them, AGENT-ID swapped."
  (nlk:json-object "tenant_id" (setting :tenant-id)
                    "profile_id" (setting :profile-id)
                    "collection_id" (setting :collection-id)
                    "agent_id" agent-id
                    "session_id" (setting :session-id)))

(defun scope-query (&key (agent-id (setting :agent-id)))
  "The same five dimensions as a GET query string: the API takes no scope body on a read."
  (format nil "tenant_id=~a&profile_id=~a&collection_id=~a&agent_id=~a&session_id=~a"
          (url-encode (setting :tenant-id))
          (url-encode (setting :profile-id))
          (url-encode (setting :collection-id))
          (url-encode agent-id)
          (url-encode (setting :session-id))))

;;; --- the wire -----------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "detail")
        (nlk:json-value body :string "message")
        (nlk:json-value body :string "error")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun auth-headers ()
  (let ((key (api-key)))
    (if (and key (plusp (length key)))
        (list (cons "authorization" (format nil "Bearer ~a" key)))
        '())))

(defun call (method path &key body)
  "METHOD PATH on the configured server, BODY its JSON text => the decoded answer.
A failure below HTTP, a refused status, or a 404 signals SEVERIAN-ERROR."
  (let* ((url (concatenate 'string (root) path))
         (limit (setting :timeout-seconds))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (append (when body '(("content-type" . "application/json")))
                                     (auth-headers))
                    :content body
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" (string-downcase (string method))
              (nle:transport-failure-label condition url))))
    (cond ((< status 300) (decode-body answer))
          ((= status 404) (fail "~a: HTTP 404, nothing of that id in this scope" path))
          (t (fail "~a ~a: HTTP ~d~@[: ~a~]" (string-upcase (string method)) path status
                   (body-error-text answer))))))

(defun hits-of (body)
  "BODY's search hits, as a list."
  (coerce (or (nlk:json-value body :array "hits") #()) 'list))

(defun search-hits (query &key (limit (setting :search-limit)) (agent-id (setting :agent-id)))
  "The memories nearest QUERY in AGENT-ID's scope, as the API's own hit objects."
  (hits-of (call :post (concatenate 'string +api-prefix+ "/search")
                 :body (nlk:encode-json-object
                        (nlk:json-object "scope" (scope-object :agent-id agent-id)
                                          "query" query
                                          "limit" limit
                                          "derive" (and (setting :derive) t))))))

(defun ingest-observation (content &optional source)
  "Write CONTENT as one observation in this scope, with SOURCE metadata."
  (call :post (concatenate 'string +api-prefix+ "/observations")
        :body (nlk:encode-json-object
               (nlk:json-object "scope" (scope-object)
                                "content" content
                                "source" (if (hash-table-p source) source (nlk:json-object))))))

(defun get-observation (id)
  (call :get (format nil "~a/observations/~a?~a" +api-prefix+ (url-encode id) (scope-query))))

(defun get-memory (id)
  (call :get (format nil "~a/memories/~a?~a" +api-prefix+ (url-encode id) (scope-query))))

(defun search-shared (agent-ids query limit)
  "The memories of this scope and of each of AGENT-IDS, merged as the original
merges them: each listed agent's exact scope queried on its own (never a
wildcard), deduplicated by memory id, re-ranked by fused score, tie broken by
content then id, and cut to LIMIT."
  (let ((seen (make-hash-table :test #'equal))
        (merged '()))
    (dolist (agent-id (cons (setting :agent-id) agent-ids))
      (dolist (hit (search-hits query :limit (max limit 50) :agent-id agent-id))
        (let ((id (or (nlk:json-value hit :string "memory_id")
                      (format nil "score-~a" (length merged)))))
          (unless (gethash id seen)
            (setf (gethash id seen) t)
            (push hit merged)))))
    (let ((ranked (sort merged #'hit-before-p)))
      (if (> (length ranked) limit) (subseq ranked 0 limit) ranked))))

(defun hit-before-p (a b)
  "A ranks before B: higher fused score, then content, then id."
  (let ((sa (or (nlk:json-value a :number "fused_score") 0))
        (sb (or (nlk:json-value b :number "fused_score") 0)))
    (cond ((/= sa sb) (> sa sb))
          (t (let ((ca (or (nlk:json-value a :string "content") ""))
                   (cb (or (nlk:json-value b :string "content") "")))
               (cond ((not (string= ca cb)) (string< ca cb))
                     (t (string< (or (nlk:json-value a :string "memory_id") "")
                                  (or (nlk:json-value b :string "memory_id") "")))))))))

;;; --- answers the model reads ---------------------------------------------------

(defun hit-line (hit)
  "One hit: its id, its text, its fused score, and the observations it cites."
  (let ((sources (nlk:json-value hit :array "source_observation_ids"))
        (score (nlk:json-value hit :number "fused_score")))
    (format nil " - [~a] ~a~@[ (score ~,2f)~]~@[ <- obs ~{~a~^, ~}~]"
            (or (nlk:json-value hit :string "memory_id") "?")
            (or (nlk:json-value hit :string "content") "")
            score
            (and sources (plusp (length sources)) (coerce sources 'list)))))

(defun format-hits (query hits &optional (lead nil))
  "HITS as the lines the model reads, under a line naming QUERY and LEAD."
  (if (null hits)
      (format nil "No memory in this scope matched ~s." query)
      (with-output-to-string (out)
        (format out "~d ~a for ~s~@[ (~a)~]:"
                (length hits) (if (= 1 (length hits)) "memory" "memories") query lead)
        (dolist (hit hits) (format out "~%~a" (hit-line hit))))))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: severian: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-integer (args name default)
  (let ((value (gethash name args)))
    (if (integerp value) (max 1 (min value 200)) default)))

(defun argument-strings (args name)
  "ARGS' NAME as a list of non-empty strings, or a refusal naming it."
  (let ((value (gethash name args)))
    (cond ((and (vectorp value) (plusp (length value))
                (every (lambda (item) (and (stringp item) (plusp (length (string-trim '(#\Space) item)))))
                       value))
           (coerce value 'list))
          (t (fail "~a must be a non-empty array of ids" (substitute #\_ #\- name))))))

;;; --- the tools ------------------------------------------------------------------

(defun tool-search (args)
  (answering
    (let ((query (argument-text args "query")))
      (format-hits query (search-hits query :limit (argument-integer args "limit" (setting :search-limit)))))))

(defun tool-ingest (args)
  (answering
    (let* ((content (argument-text args "content"))
           (answer (ingest-observation content (gethash "source" args))))
      (format nil "Stored observation ~a~@[ (derive job ~a)~]."
              (or (nlk:json-value answer :string "observation_id") "?")
              (nlk:json-value answer :string "job_id")))))

(defun tool-get-observation (args)
  (answering
    (let* ((id (argument-text args "observation_id"))
           (answer (get-observation id)))
      (format nil "Observation ~a (~a, hash ~a)~%~
                   scope tenant ~s profile ~s collection ~s agent ~s session ~s~%~
                   ~a"
              (or (nlk:json-value answer :string "observation_id") id)
              (or (nlk:json-value answer :string "created_at") "time unknown")
              (or (nlk:json-value answer :string "content_hash") "?")
              (nlk:json-value answer :string "scope" "tenant_id")
              (nlk:json-value answer :string "scope" "profile_id")
              (nlk:json-value answer :string "scope" "collection_id")
              (nlk:json-value answer :string "scope" "agent_id")
              (nlk:json-value answer :string "scope" "session_id")
              (or (nlk:json-value answer :string "content") "")))))

(defun tool-get-memory (args)
  (answering
    (let* ((id (argument-text args "memory_id"))
           (answer (get-memory id))
           (sources (nlk:json-value answer :array "source_observation_ids")))
      (format nil "Memory ~a (~a, ~a)~@[ <- obs ~{~a~^, ~}~]~%~a"
              (or (nlk:json-value answer :string "memory_id") id)
              (or (nlk:json-value answer :string "kind") "memory")
              (or (nlk:json-value answer :string "created_at") "time unknown")
              (and sources (plusp (length sources)) (coerce sources 'list))
              (or (nlk:json-value answer :string "content") "")))))

(defun tool-search-shared (args)
  (answering
    (let* ((agent-ids (argument-strings args "agent_ids"))
           (query (argument-text args "query"))
           (limit (argument-integer args "limit" (setting :search-limit))))
      (format-hits query (search-shared agent-ids query limit)
                   (format nil "this agent and ~{~a~^, ~}" agent-ids)))))

(defparameter *search-schema*
  (list :description
        "Search the Severian memory store in this cell's scope. Every hit cites the observations it came from and carries a fused score. Use it before answering anything that may depend on what you know about the user -- preferences, facts, people, projects, past decisions -- and search again with different wording when a question has several parts."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties"
         (nlk:json-object
          "query" (nlk:json-object "type" "string" "description" "What to search for.")
          "limit" (nlk:json-object "type" "integer" "minimum" 1 "maximum" 200
                                    "description" "Max results (default: the section's search_limit)."))
         "required" (vector "query")))
  "The severian_search tool.")

(defparameter *ingest-schema*
  (list :description
        "Write an observation into the Severian memory store in this cell's scope, verbatim. The server derives searchable memories from it and keeps it as the citation every derived memory points back to. Use it the moment the user states something worth recalling later, and skip transient chatter."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties"
         (nlk:json-object
          "content" (nlk:json-object "type" "string" "description" "What was said or learned, in full.")
          "source" (nlk:json-object "type" "object" "description" "Optional metadata: where it came from."))
         "required" (vector "content")))
  "The severian_ingest tool.")

(defparameter *observation-schema*
  (list :description
        "Fetch an observation by id within this scope: the exact words that were stored, its content hash and when it was written. Take the id from a search hit's source_observation_ids."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties"
         (nlk:json-object
          "observation_id" (nlk:json-object "type" "string" "description" "Observation id to read."))
         "required" (vector "observation_id")))
  "The severian_get_observation tool.")

(defparameter *memory-schema*
  (list :description
        "Fetch one derived memory by id, with the observations it came from. Take the id from a severian_search hit."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties"
         (nlk:json-object
          "memory_id" (nlk:json-object "type" "string" "description" "Memory id to read."))
         "required" (vector "memory_id")))
  "The severian_get_memory tool.")

(defparameter *shared-schema*
  (list :description
        "Read across a set of agent identities that share this profile: each listed agent's exact scope is queried on its own and the results merged and re-ranked. Only this agent and the listed agent ids are touched -- no wildcard, no other tenant."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties"
         (nlk:json-object
          "agent_ids" (nlk:json-object "type" "array" "minItems" 1
                                        "items" (nlk:json-object "type" "string")
                                        "description" "Agent identities whose memory to share into this read.")
          "query" (nlk:json-object "type" "string" "description" "What to search for.")
          "limit" (nlk:json-object "type" "integer" "minimum" 1 "maximum" 200
                                    "description" "Max results (default: the section's search_limit)."))
         "required" (vector "agent_ids" "query")))
  "The severian_search_shared tool.")

;;; --- the turn written as one observation ---------------------------------------

(defun clip (text max)
  "TEXT, cut at its last sentence end inside MAX characters, else at MAX."
  (let ((text (or text "")))
    (if (<= (length text) max)
        text
        (let ((window (subseq text 0 max))
              (cut -1))
          (loop for character across window for index from 0
                when (member character +sentence-ends+) do (setf cut index))
          (if (> cut (floor max 3)) (subseq text 0 (1+ cut)) window)))))

(defun breaker-open-p ()
  (cond ((< *breaker-failures* +breaker-threshold+) nil)
        ((< (get-universal-time) *breaker-until*) t)
        (t (setf *breaker-failures* 0) nil)))

(defun record-success () (setf *breaker-failures* 0))

(defun record-failure ()
  (when (>= (incf *breaker-failures*) +breaker-threshold+)
    (setf *breaker-until* (+ (get-universal-time) +breaker-cooldown+))))

(defun remember-turn (turn key text)
  "Keep TEXT, the :USER or :ANSWER half of TURN, until the turn completes."
  (when (and (stringp turn) (stringp text) (plusp (length text)))
    (with-severian-lock
      (let ((entry (or (gethash turn *turns*)
                       (setf (gethash turn *turns*) (cons nil nil)))))
        (ecase key
          (:user (setf (car entry) text))
          (:answer (setf (cdr entry) text)))))))

(defun take-turn (turn)
  (with-severian-lock (prog1 (gethash turn *turns*) (remhash turn *turns*))))

(defun turn-content (texts)
  "TEXTS -- (USER . ANSWER) -- as the original's sync_turn writes one observation."
  (let ((budget (setting :max-chars)))
    (format nil "user: ~a~@[~%assistant: ~a~]"
            (clip (car texts) budget)
            (and (cdr texts) (plusp (length (cdr texts))) (clip (cdr texts) budget)))))

(defun enqueue-turn (turn texts)
  "Queue TURN's texts for writing; the newest turns are kept."
  (when (and (stringp turn) (car texts) (cdr texts))
    (with-severian-lock
      (setf *queued* (cons (cons turn (turn-content texts)) *queued*))
      (when (> (length *queued*) +queued-turns+)
        (setf *queued* (subseq *queued* 0 +queued-turns+))))
    (nlk:worker-poke *worker*)))

(defun pop-queued (&aux (entry nil))
  (with-severian-lock
    (when *queued* (setf entry (first *queued*) *queued* (rest *queued*))))
  entry)

(defun capture-turn (turn content)
  "Write one turn as an observation, best effort: a memory server that is down
must not fail the turn that produced it."
  (unless (breaker-open-p)
    (handler-case
        (progn
          (ingest-observation content
                               (nlk:json-object "provider" "nodecode" "turn" turn))
          (record-success))
      (error (condition)
        (record-failure)
        (warn "severian: a turn was not written: ~a" condition)))))

(defun capture-step ()
  "The worker's step: write every queued turn."
  (loop for entry = (pop-queued)
        while entry
        do (capture-turn (car entry) (cdr entry)))
  nil)

(defun observe-frame (op)
  "Fold one published frame: a turn's two texts kept, a finished turn queued."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when (and type turn *severian* (setting :capture))
      (cond ((equal type "turn.input_committed")
             (remember-turn turn :user (nlk:fact-message-content payload)))
            ((equal type "turn.assistant_message_completed")
             (remember-turn turn :answer (nlk:fact-message-content payload)))
            ((equal type "turn.completed")
             (let ((texts (take-turn turn)))
               (when texts (enqueue-turn turn texts))))))))

;;; --- the manual and /severian ---------------------------------------------------

(defparameter +primer+
  "Severian is long-term memory that runs on the user's own machine and cites itself: every
memory names the observation it was derived from. (help :severian) is this page; the tools
are the five below.
  severian_search {query, limit?}              the memories nearest a question, each with its id
                                               and its source observations
  severian_ingest {content, source?}           write an observation verbatim
  severian_get_observation {observation_id}    the exact words stored, with their hash
  severian_get_memory {memory_id}              one derived memory and where it came from
  severian_search_shared {agent_ids, query}    read named teammates' scopes as well as our own
Search before answering anything that may depend on what you know about the user; for a
multi-part question, search several times with different wording. Take an id from a search
result before reading it back.
The scope is the security boundary: every request names a tenant, a profile, a collection, an
agent and a session, and the severian section of config.jsonc sets them once. There is no
default scope on the server and another scope's memories are never returned.
The cell talks to `severian serve --port 8000', whose address is the section's base_url; a
finished turn is written as one observation unless capture is off."
  "What (help :severian) answers while the cell runs.")

(defun status-line ()
  (format nil "Severian memory: ~a, tenant ~s, profile ~s, collection ~s, agent ~s, session ~s.~%~
               ~d hits per search; capture is ~:[off~;on~]~@[ (breaker open)~].~%~
               /severian QUERY searches; the tools severian_search, severian_ingest, ~
               severian_get_observation, severian_get_memory and severian_search_shared do the rest."
          (root) (setting :tenant-id) (setting :profile-id) (setting :collection-id)
          (setting :agent-id) (setting :session-id) (setting :search-limit)
          (setting :capture) (breaker-open-p)))

(defun run-command (args session-id)
  "One /severian invocation: a query answers what the store holds, no argument the scope."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (handler-case (format-hits query (search-hits query))
          ((or error sb-sys:deadline-timeout) (condition)
            (format nil "ERROR: severian: ~a" condition)))
        (status-line))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell severian
  (:section ("severian")
    (:guide "base_url is your running severian server (severian serve --port 8000); the five scope ids name whose memory this is; api_key only when the server runs with auth; next step: /severian shows the scope")
    ("base_url" :string
     :doc "the address of your severian server, e.g. http://127.0.0.1:8000")
    ("api_key" :secret
     :doc "the Bearer key, when the server runs with auth")
    ("api_key_env" :env
     :doc "the environment variable holding that key, when it is not written here")
    ("tenant_id" :string :default "nodecode"
     :doc "the tenant: one store-wide owner, and the first half of the isolation boundary")
    ("profile_id" :string :default "default"
     :doc "the profile: one person or one line of work inside the tenant")
    ("collection_id" :string :default "profile"
     :doc "the collection: the original keeps one profile-wide memory and calls it profile")
    ("agent_id" :string :default "nodecode"
     :doc "this agent's identity: writes are stamped with it, and severian_search_shared names others")
    ("session_id" :string :default "shared"
     :doc "the session; shared keeps one memory across chats, a session id keeps this one apart")
    ("search_limit" :integer :default 10 :min 1
     :doc "how many memories one search asks for unless the call says otherwise")
    ("derive" :boolean :default nil
     :doc "run the server's derivation pass before a search, so a turn just written is searchable at once")
    ("capture" :boolean :default t
     :doc "write each finished turn into the store as one observation")
    ("max_chars" :integer :default 4000 :min 50
     :doc "how much of each message a captured turn sends, cut at a sentence end")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "one request's ceiling"))
  (:help :severian "severian: cited long-term memory; severian_search recalls, severian_ingest records, severian_search_shared reads teammates'" +primer+)
  (:command "severian" 'run-command
            :description "Show the Severian memory scope, or search it"
            :argument-hint "QUERY")
  (:tool "severian_search" *search-schema* 'tool-search)
  (:tool "severian_ingest" *ingest-schema* 'tool-ingest)
  (:tool "severian_get_observation" *observation-schema* 'tool-get-observation)
  (:tool "severian_get_memory" *memory-schema* 'tool-get-memory)
  (:tool "severian_search_shared" *shared-schema* 'tool-search-shared)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-severian"))
  (:start (lambda ()
            (check-config)
            (setf *turns* (make-hash-table :test #'equal)
                  *queued* '()
                  *breaker-failures* 0
                  *breaker-until* 0)
            (setf *worker* (nlk:worker-start "severian-capture" #'capture-step :wake 60))
            (nle:on-stop (lambda ()
                           (nlk:worker-stop *worker*)
                           (setf *worker* nil
                                 *queued* '()
                                 *turns* (make-hash-table :test #'equal)))))))
