;;;; cell.lisp --- Mem0 memory: four tools, /mem0, and the turn capture.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of mem0ai/mem0's Hermes memory-provider plugin (see NOTICE).
;;;; The modes, the wire shapes, the tool set, the circuit breaker, the
;;;; sentence-boundary clip and the block Mem0 writes are the original's; the
;;;; harness seams are Nodecode's: the section is the config, the tools are
;;;; NLE:REGISTER-TOOL registrations, and a finished turn arrives on the
;;;; :frame point instead of through a provider interface.
;;;;
;;;; Two backends, as the original has:
;;;;   platform     Mem0's cloud, Authorization: Token KEY, /v3/memories/add/,
;;;;                /v3/memories/search/ and /v1/memories/<id>/.
;;;;   self-hosted  your own Mem0 server, X-API-Key when it wants one,
;;;;                POST /search, POST /memories and /memories/<id>.
;;;; No OSS mode: the in-process SDK (Qdrant, Ollama, OpenAI) is a Python
;;;; library, and a cell bundles none.

(defpackage #:nodecode-mem0
  (:use #:cl)
  (:export #:mem0-error #:mem0-error-detail))

(in-package #:nodecode-mem0)

(nlk:define-peripheral mem0 :not-running t :lock "mem0")

(defparameter +platform-root+ "https://api.mem0.ai"
  "Mem0's cloud API, where platform mode goes unless base_url names another.")

(defparameter +sentence-ends+ '(#\. #\! #\? #\。 #\！ #\？)
  "A clipped message breaks after the last of these inside the budget, as the
original's _SYNC_SENTENCE_ENDS does -- CJK stops included.")

(defparameter +breaker-threshold+ 5
  "Consecutive backend failures before the breaker pauses the hot path.")

(defparameter +breaker-cooldown+ 120
  "Seconds an open breaker waits before it tries the backend again.")

(defparameter +queued-turns+ 8
  "Turns waiting for extraction: the newest are kept, as the original skips a
turn whose sync is still busy.")

(defvar *queued* '()
  "Finished turns waiting for extraction, newest first: (TURN-ID . ANSWER).")

(defvar *turns* (make-hash-table :test #'equal)
  "Each running turn's two texts, by turn id: (USER . ANSWER), until it completes.")

(defvar *worker* nil
  "The capture worker (NLK:WORKER-START) while the cell runs, or NIL.")

(defvar *breaker-failures* 0
  "Consecutive backend failures on the capture path, for the breaker.")

(defvar *breaker-until* 0
  "Universal time the open breaker may try the backend again.")

;;; --- the section, read back --------------------------------------------------

(defun self-hosted-p ()
  (equal (setting :mode) "self-hosted"))

(defun api-key ()
  "The key the section names, inline or by environment variable, else MEM0_API_KEY."
  (or (let ((inline (setting :api-key))) (and inline (plusp (length inline)) inline))
      (let ((variable (setting :api-key-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))
      (uiop:getenv "MEM0_API_KEY")))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +platform-root+)))

(defun check-config ()
  "Refuse a section that could never reach a backend, naming the member."
  (when (and (not (self-hosted-p)) (null (api-key)))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "mem0: platform mode needs a key: set MEM0_API_KEY, or \"api_key\" "
                                 "(or \"api_key_env\") in the mem0 section; or \"mode\": \"self-hosted\" "
                                 "with \"base_url\" naming your Mem0 server")))
  (when (and (self-hosted-p) (notany #'alpha-char-p (root)))
    (error 'nlk:config-refusal
           :detail "mem0: self-hosted mode needs \"base_url\": the address of your Mem0 server")))

;;; --- the wire -----------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER — an HTTP body — as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "message")
        (nlk:json-value body :string "error")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun auth-headers ()
  (let ((key (api-key)))
    (if (self-hosted-p)
        (if (and key (plusp (length key))) (list (cons "x-api-key" key)) '())
        (list (cons "authorization" (format nil "Token ~a" (or key "")))))))

(defun call (method path &key body)
  "METHOD PATH on the configured root, BODY its JSON text => the decoded answer.
A failure below HTTP, a refused status, or a 404 signals MEM0-ERROR."
  (let* ((url (concatenate 'string (root) path))
         (limit (setting :timeout-seconds))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (cons '("content-type" . "application/json") (auth-headers))
                    :content body
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" (string-downcase (string method))
              (nle:transport-failure-label condition url))))
    (cond ((< status 300) (decode-body answer))
          ((= status 404) (fail "~a: HTTP 404, nothing of that id on ~a" path (root)))
          (t (fail "~a ~a: HTTP ~d~@[: ~a~]" (string-upcase (string method)) path status
                   (body-error-text answer))))))

(defun results-of (body)
  "BODY's result objects: the \"results\" array of an object, or the array itself."
  (coerce (or (cond ((hash-table-p body) (nlk:json-value body :array "results"))
                    ((vectorp body) body))
               #())
          'list))

(defun memory-path (id)
  (if (self-hosted-p)
      (format nil "/memories/~a" id)
      (format nil "/v1/memories/~a/" id)))

(defun search-memories (query &key top-k rerank)
  "The memories nearest QUERY for this user, as the backend's own result objects."
  (let* ((limit (or top-k (setting :search-limit)))
         (body (apply #'nlk:make-json-object
                      (append (list "query" query
                                    "top_k" limit
                                    "filters" (nlk:json-object "user_id" (setting :user-id)))
                              ;; rerank is platform-only: the self-hosted /search has no such field
                              (when (and rerank (not (self-hosted-p))) (list "rerank" t))))))
    (results-of (call :post (if (self-hosted-p) "/search" "/v3/memories/search/")
                      :body (nlk:encode-json-object body)))))

(defun add-memory (messages &key infer)
  "Store MESSAGES -- a list of wire message objects -- for this user and agent."
  (let ((body (nlk:json-object "messages" (coerce messages 'vector)
                                "user_id" (setting :user-id)
                                "agent_id" (setting :agent-id)
                                "infer" (and infer t)
                                "metadata" (nlk:json-object "channel" "nodecode"))))
    (call :post (if (self-hosted-p) "/memories" "/v3/memories/add/")
          :body (nlk:encode-json-object body))))

(defun own-memory (id)
  "ID's memory, refused when it belongs to another user, as the original's
_ensure_owns_memory does."
  (let* ((body (call :get (memory-path id)))
         (owner (nlk:json-value body :string "user_id")))
    (when (and owner (not (equal owner (setting :user-id))))
      (fail "memory ~a belongs to ~s, not to ~s" id owner (setting :user-id)))
    body))

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: mem0: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-integer (args name default)
  (let ((value (gethash name args)))
    (if (integerp value) (max 1 (min value 50)) default)))

(defun argument-boolean (args name default)
  (multiple-value-bind (value present) (gethash name args)
    (declare (ignore value))
    (if present (and (gethash name args) t) default)))

(defun format-results (query results)
  "RESULTS as the lines the model reads: one per fact, its id and its score."
  (let ((facts (remove-if-not (lambda (result)
                                 (and (hash-table-p result)
                                      (let ((text (nlk:json-value result :string "memory")))
                                        (and text (plusp (length text))))))
                               results)))
    (if (null facts)
        (format nil "No memory matched ~s." query)
        (with-output-to-string (out)
          (format out "~d memories for ~s:" (length facts) query)
          (dolist (result facts)
            (format out "~% - [~a] ~a~@[ (score ~,2f)~]"
                    (or (nlk:json-value result :string "id") "?")
                    (nlk:json-value result :string "memory")
                    (nlk:json-value result :number "score")))))))

(defun tool-search (args)
  (answering
    (let ((query (argument-text args "query")))
      (format-results query
                      (search-memories query
                                        :top-k (argument-integer args "top_k" nil)
                                        :rerank (argument-boolean args "rerank" (setting :rerank)))))))

(defun tool-add (args)
  (answering
    (let* ((content (argument-text args "content"))
           (answer (add-memory (list (nlk:json-object "role" "user" "content" content)))))
      (format nil "~a~@[ (event ~a)~]"
              (if (self-hosted-p) "Stored." "Queued for storage.")
              (nlk:json-value answer :string "event_id")))))

(defun tool-update (args)
  (answering
    (let ((id (argument-text args "memory_id"))
          (text (argument-text args "text")))
      (own-memory id)
      (call :put (memory-path id)
            :body (nlk:encode-json-object (nlk:json-object "text" text)))
      (format nil "Updated memory ~a." id))))

(defun tool-delete (args)
  (answering
    (let ((id (argument-text args "memory_id")))
      (own-memory id)
      (call :delete (memory-path id))
      (format nil "Deleted memory ~a." id))))

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
   "Search the user's memories by meaning; returns facts ranked by relevance. Use this before answering any question that may depend on what you know about the user (preferences, facts, history, people, projects, past decisions). For multi-part or multi-hop questions, call it several times -- vary the wording and run follow-up searches on what earlier results reveal; one search is rarely enough."
   '(("query" "string" "What to search for." t)
     ("top_k" "integer" "Max results (default: the section's search_limit, at most 50)." nil)
     ("rerank" "boolean" "Rerank results for relevance (default: the section's rerank; platform mode only)." nil)))
  "The mem0_search tool, as the original's TOOL_SCHEMAS has it.")

(defparameter *add-schema*
  (tool-schema
   "Store a durable fact about the user, verbatim (no LLM extraction). Call this the moment the user states a lasting preference, correction, decision, or personal detail worth recalling on future turns -- don't wait to be asked to remember. Skip transient chit-chat and facts you've already stored."
   '(("content" "string" "The fact to store." t)))
  "The mem0_add tool, as the original's TOOL_SCHEMAS has it.")

(defparameter *update-schema*
  (tool-schema
   "Replace the text of an existing memory by its ID (take the ID from a mem0_search result). Use when a stored fact has changed or was wrong -- correct it in place instead of adding a duplicate."
   '(("memory_id" "string" "Memory id to update." t)
     ("text" "string" "New text content." t)))
  "The mem0_update tool, as the original's TOOL_SCHEMAS has it.")

(defparameter *delete-schema*
  (tool-schema
   "Delete a memory by its ID (take the ID from a mem0_search result). Use when a stored fact is obsolete or the user asks you to forget it; prefer mem0_update if the fact merely changed."
   '(("memory_id" "string" "Memory id to delete." t)))
  "The mem0_delete tool, as the original's TOOL_SCHEMAS has it.")

;;; --- capture: the turn's two messages, extracted in the background ------------

(defun clip (text max)
  "TEXT, cut at its last sentence end inside MAX characters, else at MAX.
The original's _truncate_for_sync: a small embedder's window loses no
sentence it could have kept."
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
    (with-mem0-lock
      (let ((entry (or (gethash turn *turns*)
                       (setf (gethash turn *turns*) (cons nil nil)))))
        (ecase key
          (:user (setf (car entry) text))
          (:answer (setf (cdr entry) text)))))))

(defun take-turn (turn)
  (with-mem0-lock (prog1 (gethash turn *turns*) (remhash turn *turns*))))

(defun enqueue-turn (turn texts)
  "Queue TURN's (USER . ANSWER) for extraction; the newest turns are kept."
  (when (and (stringp turn) (car texts) (cdr texts))
    (with-mem0-lock
      (setf *queued* (cons (cons turn texts) *queued*))
      (when (> (length *queued*) +queued-turns+)
        (setf *queued* (subseq *queued* 0 +queued-turns+))))
    (nlk:worker-poke *worker*)))

(defun pop-queued (&aux (entry nil))
  (with-mem0-lock
    (when *queued* (setf entry (first *queued*) *queued* (rest *queued*))))
  entry)

(defun capture-turn (user answer)
  "Send one turn's two messages to Mem0 for extraction, best effort."
  (unless (breaker-open-p)
    (let ((budget (setting :max-chars)))
      (handler-case
          (progn
            (add-memory (list (nlk:json-object "role" "user" "content" (clip user budget))
                              (nlk:json-object "role" "assistant" "content" (clip answer budget)))
                        :infer t)
            (record-success))
        (error (condition)
          (record-failure)
          (warn "mem0: a turn was not extracted: ~a" condition))))))

(defun capture-step ()
  "The worker's step: send every queued turn."
  (loop for entry = (pop-queued)
        while entry
        do (capture-turn (cadr entry) (cddr entry)))
  nil)

(defun observe-frame (op)
  "Fold one published frame: a turn's two texts kept, a finished turn queued."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when (and type turn *mem0* (setting :capture))
      (cond ((equal type "turn.input_committed")
             (remember-turn turn :user (nlk:fact-message-content payload)))
            ((equal type "turn.assistant_message_completed")
             (remember-turn turn :answer (nlk:fact-message-content payload)))
            ((equal type "turn.completed")
             (let ((texts (take-turn turn)))
               (when texts (enqueue-turn turn texts))))))))

;;; --- the manual and /mem0 -------------------------------------------------------

(defparameter +primer+
  "Mem0 keeps a user's durable facts, server-side: it extracts them from what is said and finds
them again by meaning. (help :mem0) is this page; the tools are the four below.
  mem0_search {query, top_k?, rerank?}  the facts nearest a question, with their ids
  mem0_add {content}                    store one fact verbatim, no extraction
  mem0_update {memory_id, text}         replace a stored fact's text
  mem0_delete {memory_id}               forget a stored fact
Search before answering anything that may depend on what you know about the user; for a
multi-part question, search several times with different wording. Take an id from a search
result before you update or delete. Automatic recall before a reply is not wired here: the
model calls mem0_search. Every finished turn is still sent to Mem0 for extraction.
The mem0 section of config.jsonc names the backend: platform (Mem0's cloud, a key from
MEM0_API_KEY) or self-hosted (base_url, and a key only when the server asks for one), the
user_id whose memories these are, and the agent_id writes are stamped with."
  "What (help :mem0) answers while the cell runs.")

(defun status-line ()
  (format nil "Mem0 memory: ~a at ~a, user ~s, agent ~s.~%~
               /mem0 QUERY searches; the tools mem0_search, mem0_add, mem0_update and mem0_delete do the rest."
          (if (self-hosted-p) "self-hosted server" "platform")
          (root) (setting :user-id) (setting :agent-id)))

(defun run-command (args session-id)
  "One /mem0 invocation: a query answers what Mem0 holds, no argument the backend."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (format-results query (search-memories query))
        (status-line))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell mem0
  (:section ("mem0")
    (:guide "platform needs a Mem0 API key: set MEM0_API_KEY, or api_key (or api_key_env) here; self-hosted needs base_url, and a key only when the server asks for one; user_id is whose memories these are; capture sends each finished turn for extraction; next step: /mem0 shows the backend")
    ("mode" :choice :options '("platform" "self-hosted") :default "platform"
     :doc "platform for Mem0's cloud, self-hosted for your own Mem0 server")
    ("base_url" :string
     :doc "the API address: https://api.mem0.ai unless answered, and required in self-hosted mode")
    ("api_key" :secret
     :doc "the Mem0 API key")
    ("api_key_env" :env
     :doc "the environment variable holding the Mem0 API key, when it is not written here (MEM0_API_KEY is read either way)")
    ("user_id" :string :default "nodecode-user"
     :doc "whose memories these are; the same id across sessions finds the same facts")
    ("agent_id" :string :default "nodecode"
     :doc "the agent id every write is stamped with")
    ("rerank" :boolean :default nil
     :doc "rerank search results for relevance (platform mode only)")
    ("search_limit" :integer :default 10 :min 1
     :doc "how many memories one search asks for unless the call says otherwise")
    ("capture" :boolean :default t
     :doc "send each finished turn to Mem0 for fact extraction")
    ("max_chars" :integer :default 450 :min 50
     :doc "how much of each message one turn's extraction sends, cut at a sentence end")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "one request's ceiling"))
  (:help :mem0 "mem0: mem0_search recalls a user's stored facts; mem0_add, mem0_update and mem0_delete keep them" +primer+)
  (:command "mem0" 'run-command
            :description "Show the Mem0 memory backend, or search it"
            :argument-hint "QUERY")
  (:tool "mem0_search" *search-schema* 'tool-search)
  (:tool "mem0_add" *add-schema* 'tool-add)
  (:tool "mem0_update" *update-schema* 'tool-update)
  (:tool "mem0_delete" *delete-schema* 'tool-delete)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-mem0"))
  (:start (lambda ()
            (check-config)
            (setf *turns* (make-hash-table :test #'equal)
                  *queued* '()
                  *breaker-failures* 0
                  *breaker-until* 0)
            (setf *worker* (nlk:worker-start "mem0-capture" #'capture-step :wake 60))
            (nle:on-stop (lambda ()
                           (nlk:worker-stop *worker*)
                           (setf *worker* nil
                                 *queued* '()
                                 *turns* (make-hash-table :test #'equal)))))))
