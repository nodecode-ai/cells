;;;; cell.lisp --- Upstash Redis memory: four tools, /upstash, recall and transcripts.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of upstash/agentkit's Hermes memory provider (see NOTICE).
;;;; The key layout, the document shapes, the shared search index, the tool
;;;; set and the recall block are the original's, so a memory written by an
;;;; AgentKit app on the same database is recalled here and the other way
;;;; round; the harness seams are Nodecode's: the section is the config, the
;;;; tools are NLE:REGISTER-TOOL registrations, a turn's facts arrive on the
;;;; :frame point, and what was recalled rides the session's harness block.
;;;;
;;;; Layout, as the original and @upstash/agentkit-sdk write it:
;;;;   memory  <namespace>:memory:<user>:<id>  {text, userId, createdAt, ...}
;;;;   chat    <namespace>:chat:<user>:<session> {kind: "chat", userId,
;;;;           sessionId, userMessages, modelMessages, messages, createdAt,
;;;;           updatedAt}
;;;;   index   one per namespace, named after it, over both prefixes, because
;;;;           Upstash's Free plan allows one search index per database.

(defpackage #:nodecode-upstash
  (:use #:cl)
  (:export #:upstash-error #:upstash-error-detail))

(in-package #:nodecode-upstash)

(nlk:define-peripheral upstash :not-running t :lock "upstash")

(defparameter +default-namespace+ "agentkit"
  "The namespace the original and the TS SDK default to: their keys are shared.")

(defparameter +excerpt-chars+ 300
  "How much of each side of a past session a chat search result shows.")

(defparameter +max-limit+ 50
  "The most one search answers with, whatever the call asks for.")

(defparameter +harness-section+ "upstash-memory"
  "The session block recalled memories ride; the model reads it every round.")

(defparameter +schema+
  '(("kind" "TEXT" t) ("userId" "TEXT" t) ("text" "TEXT" nil) ("sessionId" "TEXT" t)
    ("userMessages" "TEXT" nil) ("modelMessages" "TEXT" nil))
  "The one shared index's fields, as the TS SDK declares them: (NAME TYPE NOTOKENIZED-P).")

(defvar *index-ready* nil
  "T when this start has created the namespace's index already.")

;;; --- the section, read back --------------------------------------------------

(defun env (name)
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)) value)))

(defun endpoint ()
  "The database's REST address, without a trailing slash."
  (string-right-trim "/" (or (setting :url) (env "UPSTASH_REDIS_REST_URL") "")))

(defun token ()
  "The REST token: the section's, else the environment's."
  (or (let ((inline (setting :token))) (and inline (plusp (length inline)) inline))
      (env "UPSTASH_REDIS_REST_TOKEN")))

(defun user-id ()
  (or (setting :user-id) "default"))

(defun namespace ()
  (let ((given (string-right-trim ":" (or (setting :namespace) ""))))
    (if (plusp (length given)) given +default-namespace+)))

(defun check-config ()
  "Refuse a section that could never reach the database, naming the member."
  (unless (plusp (length (endpoint)))
    (error 'nlk:config-refusal
           :detail "upstash: needs \"url\": the REST address from the Upstash console (https://<name>.upstash.io), or UPSTASH_REDIS_REST_URL in the environment"))
  (unless (token)
    (error 'nlk:config-refusal
           :detail "upstash: needs \"token\": the database's REST token, or UPSTASH_REDIS_REST_TOKEN in the environment"))
  (when (find #\: (user-id))
    (error 'nlk:config-refusal
           :detail "upstash: user_id may not contain ':' (it separates the parts of a key)")))

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
    (or (nlk:json-value body :string "error")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun auth-headers ()
  (list (cons "authorization" (format nil "Bearer ~a" (token)))))

(defun telemetry-headers ()
  "The opt-in telemetry header the original sends to the same database; nothing else."
  (when (setting :telemetry)
    (list (cons "upstash-telemetry-sdk" "nodecode-upstash@0.1.0"))))

(defun command (&rest parts)
  "PARTS -- one Redis command's arguments -- run over the database's REST API.
Answers the command's RESULT, decoded; a transport failure, a refused status or
an {\"error\": ...} answer signals UPSTASH-ERROR."
  (let ((url (endpoint))
        (answer nil)
        (status nil)
        (limit (setting :timeout-seconds)))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http :post url
                    :headers (append '(("content-type" . "application/json"))
                                     (auth-headers) (telemetry-headers))
                    :content (nlk:encode-json-object (coerce parts 'vector))
                    :timeout limit :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" (string-downcase (string (first parts)))
              (nle:transport-failure-label condition url))))
    (when (>= status 300)
      (fail "~a: HTTP ~d~@[: ~a~]" (string-downcase (string (first parts))) status
            (body-error-text answer)))
    (let ((body (decode-body answer)))
      (when (hash-table-p body)
        (let ((problem (nlk:json-value body :any "error")))
          (when problem
            (fail "~a: ~a" (string-downcase (string (first parts)))
                  (if (stringp problem) problem (nlk:encode-json-object problem))))))
      (json-result (nlk:json-value body :any "result")))))

(defun json-result (value)
  "VALUE as a decoded value: the REST answer hands a JSON document back as text."
  (if (and (stringp value)
           (plusp (length (string-trim '(#\Space) value)))
           (find (char (string-trim '(#\Space) value) 0) "{["))
      (or (ignore-errors (nlk:decode-json value)) value)
      value))

(defun document-of (value)
  "VALUE -- a REST result -- as one JSON object, or NIL when there is none.
JSON.GET without a path answers the object; JSON.GET '$' answers a one-element array."
  (let ((value (json-result value)))
    (cond ((hash-table-p value) value)
          ((and (vectorp value) (plusp (length value)))
           (let ((first (aref value 0)))
             (and (hash-table-p first) first))))))

;;; --- the index ------------------------------------------------------------------

(defun index-name ()
  "The namespace's index name: every character outside [A-Za-z0-9_] becomes '_', as the SDK's."
  (map 'string (lambda (character)
                  (if (or (char= character #\_)
                          (and (< (char-code character) 128) (alphanumericp character)))
                      character #\_))
       (namespace)))

(defun ensure-index ()
  "Create the namespace's shared index on first use; a no-op after that."
  (unless *index-ready*
    (apply #'command `("SEARCH.CREATE" ,(index-name) "EXISTSOK" "ON" "JSON" "PREFIX" "2"
                       ,(format nil "~a:memory:" (namespace))
                       ,(format nil "~a:chat:" (namespace)) "SCHEMA"
                       ,@(loop for (name type no-tokenize) in +schema+
                               append (append (list name type)
                                              (when no-tokenize (list "NOTOKENIZE"))))))
    (command "SEARCH.WAITINDEXING" (index-name))
    (setf *index-ready* t)))

(defun filter-json (object)
  (nlk:encode-json-object object))

(defun rows-of (result)
  "RESULT's rows as a list, each a {key, score, data} object."
  (let ((value (json-result result)))
    (if (vectorp value) (coerce value 'list) '())))

(defun row-key (row) (nlk:json-value row :string "key"))
(defun row-score (row) (nlk:json-value row :number "score"))
(defun row-data (row) (document-of (nlk:json-value row :any "data")))

(defun query-rows (filter &key limit)
  "The index rows FILTER admits, at most LIMIT of them."
  (ensure-index)
  (let ((rows (apply #'command "SEARCH.QUERY" (index-name) (filter-json filter)
                      (when limit (list "LIMIT" (princ-to-string limit))))))
    (when (vectorp rows) (setf rows (coerce rows 'list)))
    (when (and limit (> (length rows) limit)) (setf rows (subseq rows 0 limit)))
    rows))

;;; --- memory ----------------------------------------------------------------------

(defun now-ms ()
  "The epoch in milliseconds, as the documents stamp it."
  (- (get-universal-time) 2208988800))

(defun ms ()
  (* 1000 (now-ms)))

(defun memory-prefix ()
  (format nil "~a:memory:~a:" (namespace) (user-id)))

(defun memory-key (id)
  (format nil "~a~a" (memory-prefix) id))

(defun new-id ()
  "A fresh memory id: no ':' needed, only uniqueness within a user."
  (format nil "nc-~36r-~36r" (get-universal-time) (random (expt 36 10))))

(defun memory-filter (&key text)
  "This user's memory rows: chats on the shared index carry kind \"chat\" and
are excluded in the query, not after it, so they never use up a limit."
  (apply #'nlk:make-json-object
         (append (list "userId" (nlk:json-object "$eq" (user-id))
                       "$mustNot" (vector (nlk:json-object "kind"
                                                            (nlk:json-object "$eq" "chat"))))
                 (when text (list "text" (nlk:json-object "$smart" text))))))

(defun records-of (rows)
  "ROWS of this store as plists (:ID :TEXT :SCORE); a row outside this user's
prefix is not this store's, even when the index matched it."
  (let ((prefix (memory-prefix)))
    (loop for row in rows
          for key = (row-key row)
          when (and key (<= (length prefix) (length key))
                    (string= prefix key :end2 (length prefix)))
            collect (list :id (subseq key (length prefix))
                          :text (or (nlk:json-value (row-data row) :string "text") "")
                          :score (row-score row)))))

(defun recall (query &key (limit (setting :recall-limit)))
  "This user's memories nearest QUERY; an empty query lists them instead."
  (let ((query (nlk:trimmed (or query ""))))
    (records-of (query-rows (memory-filter :text (and (plusp (length query)) query))
                             :limit (max 1 (min (or limit (setting :recall-limit)) +max-limit+))))))

(defun add-memory (text)
  "Store TEXT verbatim under a fresh id => the id.
The index is created first and waited on, so the fact is searchable by the time
this returns: a fact saved in one turn is recalled in the next."
  (ensure-index)
  (let* ((id (new-id))
         (doc (nlk:json-object "text" text "userId" (user-id)
                                "createdAt" (ms) "source" "nodecode")))
    (command "JSON.SET" (memory-key id) "$" (nlk:encode-json-object doc))
    (command "SEARCH.WAITINDEXING" (index-name))
    id))

(defun forget-memory (id)
  "Delete ID => T when a key went away."
  (plusp (or (let ((result (command "DEL" (memory-key id))))
               (if (integerp result) result 0))
             0)))

;;; --- chat history ----------------------------------------------------------------

(defun chat-prefix ()
  (format nil "~a:chat:~a:" (namespace) (user-id)))

(defun chat-key (session-id)
  (format nil "~a~a" (chat-prefix) session-id))

(defun get-chat (session-id)
  "SESSION-ID's saved document for this user, or NIL."
  (document-of (command "JSON.GET" (chat-key session-id))))

(defun message (role text)
  (nlk:json-object "role" role "content" text))

(defun side-text (messages role)
  "MESSAGES' ROLE lines, one per message, as the document's searchable side."
  (let ((lines (loop for entry in messages
                     for entry-role = (nlk:json-value entry :string "role")
                     for text = (nlk:json-value entry :string "content")
                     when (and text (plusp (length text))
                               (if (equal role "user") (equal entry-role "user")
                                   (member entry-role '("assistant" "model") :test #'equal)))
                       collect text)))
    (with-output-to-string (out)
      (loop for (line . more) on lines
            do (write-string line out)
            when more do (write-char #\Newline out)))))

(defun save-chat (session-id messages)
  "Write SESSION-ID's transcript, keeping the createdAt (and title) of a prior save."
  (let* ((existing (or (get-chat session-id) (nlk:json-object)))
         (created (or (nlk:json-value existing :integer "createdAt") (ms)))
         (title (nlk:json-value existing :string "title"))
         (document (nlk:make-json-object
                    "kind" "chat"
                    "userId" (user-id)
                    "sessionId" session-id
                    "userMessages" (side-text messages "user")
                    "modelMessages" (side-text messages "assistant")
                    "messages" (coerce messages 'vector)
                    "createdAt" created
                    "updatedAt" (ms))))
    (when title (setf (gethash "title" document) title))
    (ensure-index)
    (command "JSON.SET" (chat-key session-id) "$" (nlk:encode-json-object document))
    (command "SEARCH.WAITINDEXING" (index-name))
    document))

(defun append-turn (session-id user answer)
  "Append one turn to SESSION-ID's transcript: the whole document, rewritten."
  (let* ((existing (get-chat session-id))
         (saved (coerce (nlk:json-array existing "messages") 'list))
         (new (append (when (and user (plusp (length user))) (list (message "user" user)))
                      (when (and answer (plusp (length answer))) (list (message "assistant" answer))))))
    (when new (save-chat session-id (append saved new)))))

(defun chat-filter (query)
  (nlk:json-object
   "$and" (vector (nlk:json-object "kind" (nlk:json-object "$eq" "chat"))
                   (nlk:json-object "userId" (nlk:json-object "$eq" (user-id)))
                   (nlk:json-object "$or" (vector (nlk:json-object "userMessages"
                                                                   (nlk:json-object "$smart" query))
                                                   (nlk:json-object "modelMessages"
                                                                   (nlk:json-object "$smart" query)))))))

(defun chat-hits (query &key (limit 5))
  "This user's past sessions matching QUERY, with an excerpt of each side."
  (loop for row in (query-rows (chat-filter query)
                                :limit (max 1 (min limit +max-limit+)))
        for data = (row-data row)
        for key = (row-key row)
        for session = (or (nlk:json-value data :string "sessionId")
                          (and key (subseq key (1+ (or (position #\: key :from-end t) -1)))))
        when session
          collect (list :session session
                        :score (row-score row)
                        :title (nlk:json-value data :string "title")
                        :updated (or (nlk:json-value data :integer "updatedAt") 0)
                        :user (excerpt (nlk:json-value data :string "userMessages"))
                        :model (excerpt (nlk:json-value data :string "modelMessages")))))

(defun excerpt (text)
  (and text (subseq text 0 (min +excerpt-chars+ (length text)))))

;;; --- recall into the prompt and the turn's transcript -----------------------------

(defun prefetch (query session-id)
  "Recall for QUERY and put it in the session's harness block, where every round
of the turn reads it. A database that is down never breaks a turn."
  (when (and session-id (plusp (length query)) (nlk:store-open-p))
    (handler-case
        (let ((hits (recall query)))
          (if hits
              (nlk:set-harness-section session-id +harness-section+
                                        (format nil "## Upstash Memory~%~{- ~a~%~}"
                                                (mapcar (lambda (hit) (getf hit :text)) hits)))
              (nlk:clear-harness-section session-id +harness-section+)))
      (error (condition) (warn "upstash: recall skipped: ~a" condition)))))

(defun capture-turn (session-id user answer)
  (when (and session-id)
    (handler-case (append-turn session-id user answer)
      (error (condition) (warn "upstash: transcript not saved: ~a" condition)))))

(defvar *turns* (make-hash-table :test #'equal)
  "Each running turn's two texts, by turn id: (USER . ANSWER), until it completes.")

(defun remember-turn (turn key text)
  (when (and (stringp turn) (stringp text) (plusp (length text)))
    (let ((entry (or (gethash turn *turns*)
                     (setf (gethash turn *turns*) (cons nil nil)))))
      (ecase key
        (:user (setf (car entry) text))
        (:answer (setf (cdr entry) text))))))

(defun take-turn (turn)
  (prog1 (gethash turn *turns*) (remhash turn *turns*)))

(defun observe-frame (op)
  "Fold one published frame: the turn's user text is recalled from before the
first round, its answer kept, and the pair written to the transcript at the end."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when (and type turn *upstash*)
      (let ((session (getf op :session-id)))
        (cond ((equal type "turn.input_committed")
               (let ((text (nlk:fact-message-content payload)))
                 (remember-turn turn :user text)
                 (when (setting :recall) (prefetch text session))))
              ((equal type "turn.assistant_message_completed")
               (remember-turn turn :answer (nlk:fact-message-content payload)))
              ((equal type "turn.completed")
               (let ((texts (take-turn turn)))
                 (when (and texts (setting :capture))
                   (capture-turn session (car texts) (cdr texts))))))))))

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: upstash: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-query (args)
  (let ((value (gethash "query" args)))
    (if (stringp value) (nlk:trimmed value) "")))

(defun argument-limit (args default)
  (let ((value (gethash "limit" args)))
    (if (integerp value) (max 1 (min value +max-limit+)) default)))

(defparameter *save-schema*
  (list :description
        "Save one durable fact about the user or their work to long-term memory (preferences, decisions, people, projects). Write it as a short standalone sentence."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object "text" (nlk:json-object "type" "string"
                                                               "description" "The fact to remember."))
         "required" (vector "text")))
  "The upstash_memory_save tool, as the original's SAVE_SCHEMA has it.")

(defparameter *search-schema*
  (list :description "Search long-term memory for facts related to a query. Returns ids and text."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                        "query" (nlk:json-object "type" "string" "description" "What to look for.")
                        "limit" (nlk:json-object "type" "integer"
                                                  "description" "Maximum results (default 5)."))
         "required" (vector "query")))
  "The upstash_memory_search tool, as the original's SEARCH_SCHEMA has it.")

(defparameter *forget-schema*
  (list :description "Delete one memory by the id that upstash_memory_search returned."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object "id" (nlk:json-object "type" "string"
                                                               "description" "The memory id."))
         "required" (vector "id")))
  "The upstash_memory_forget tool, as the original's FORGET_SCHEMA has it.")

(defparameter *chat-schema*
  (list :description
        "Search the transcripts of past conversations with this user. Returns matching sessions with an excerpt of each side."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (nlk:json-object
                        "query" (nlk:json-object "type" "string" "description" "What to look for.")
                        "limit" (nlk:json-object "type" "integer"
                                                  "description" "Maximum sessions (default 5)."))
         "required" (vector "query")))
  "The upstash_chat_search tool, as the original's CHAT_SEARCH_SCHEMA has it.")

(defun format-results (query records)
  "RECORDS as the lines /upstash shows: one per fact, its id and its score."
  (if (null records)
      (format nil "No memory matched ~s." query)
      (with-output-to-string (out)
        (format out "~d memories for ~s:" (length records) query)
        (dolist (record records)
          (format out "~% - [~a] ~a~@[ (score ~,2f)~]"
                  (getf record :id) (getf record :text) (getf record :score))))))

(defun tool-save (args)
  (answering
    (let ((text (argument-text args "text")))
      (nlk:encode-json-object (nlk:json-object "saved" t "id" (add-memory text))))))

(defun tool-search (args)
  (answering
    (let ((query (argument-query args))
          (limit (argument-limit args 5)))
      (nlk:encode-json-object
       (nlk:json-object
        "results" (coerce (loop for record in (recall query :limit limit)
                                 collect (nlk:json-object "id" (getf record :id)
                                                           "text" (getf record :text)
                                                           "score" (getf record :score)))
                           'vector))))))

(defun tool-forget (args)
  (answering
    (let ((id (argument-text args "id")))
      (nlk:encode-json-object (nlk:json-object "forgotten" (and (forget-memory id) t))))))

(defun tool-chat-search (args)
  (answering
    (let ((query (argument-text args "query"))
          (limit (argument-limit args 5)))
      (nlk:encode-json-object
       (nlk:json-object
        "sessions" (coerce (loop for hit in (chat-hits query :limit limit)
                                  collect (nlk:json-object "session_id" (getf hit :session)
                                                            "score" (getf hit :score)
                                                            "title" (getf hit :title)
                                                            "updated_at" (getf hit :updated)
                                                            "user_excerpt" (getf hit :user)
                                                            "assistant_excerpt" (getf hit :model)))
                            'vector))))))

;;; --- the manual and /upstash ------------------------------------------------------

(defparameter +primer+
  "Upstash Redis holds a user's durable facts and their past conversations, in a database you name.
(help :upstash) is this page; the tools are the four below.
  upstash_memory_save {text}        store one durable fact, verbatim
  upstash_memory_search {query, limit?}  the facts nearest a question, with their ids
  upstash_memory_forget {id}        delete one fact by the id a search returned
  upstash_chat_search {query, limit?}   past sessions matching a query, with excerpts
Before each turn the memories nearest the user's message are recalled into the harness block
`upstash-memory`, and each finished turn is appended to the session's transcript, so both are
searchable later. Keys and documents are the ones @upstash/agentkit-sdk writes, so an AgentKit app
on the same database at namespace agentkit shares them: <namespace>:memory:<user>:<id> and
<namespace>:chat:<user>:<session>, in one search index named after the namespace.
The upstash section of config.jsonc names the database: url, token, user_id, namespace, whether
to recall into the prompt and to save transcripts."
  "What (help :upstash) answers while the cell runs.")

(defun status-line ()
  (format nil "Upstash memory: ~a, user ~s, namespace ~s.~%  ~d tools; /upstash QUERY searches."
          (endpoint) (user-id) (namespace) 4))

(defun run-command (args session-id)
  "One /upstash invocation: a query answers what the database holds, no argument the settings."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (handler-case (format-results query (recall query))
          ((or error sb-sys:deadline-timeout) (condition) (format nil "ERROR: upstash: ~a" condition)))
        (status-line))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell upstash
  (:section ("upstash")
    (:guide "url is the database's REST address and token its REST token, both from the Upstash console; user_id is whose memories these are; namespace agentkit shares keys with AgentKit apps; next step: /upstash shows the settings")
    ("url" :string
     :doc "the database's REST address (https://<name>.upstash.io); UPSTASH_REDIS_REST_URL is read when it is absent")
    ("token" :secret
     :doc "the database's REST token; UPSTASH_REDIS_REST_TOKEN is read when it is absent")
    ("user_id" :string :default "default"
     :doc "whose memories these are; the same id across sessions finds the same facts")
    ("namespace" :string :default "agentkit"
     :doc "the key namespace, agentkit so AgentKit apps on the same database share the keys")
    ("recall" :boolean :default t
     :doc "recall the memories nearest each user message into the prompt")
    ("recall_limit" :integer :default 5 :min 1
     :doc "how many memories one recall injects")
    ("capture" :boolean :default t
     :doc "append each finished turn to the session's searchable transcript")
    ("telemetry" :boolean :default nil
     :doc "opt in to the telemetry header the original sends with requests to your own database")
    ("timeout_seconds" :integer :default 15 :min 1
     :doc "one request's ceiling"))
  (:help :upstash "upstash: durable memories and searchable transcripts in your own Upstash Redis database" +primer+)
  (:command "upstash" 'run-command
            :description "Show the Upstash memory database, or search it"
            :argument-hint "QUERY")
  (:tool "upstash_memory_save" *save-schema* 'tool-save)
  (:tool "upstash_memory_search" *search-schema* 'tool-search)
  (:tool "upstash_memory_forget" *forget-schema* 'tool-forget)
  (:tool "upstash_chat_search" *chat-schema* 'tool-chat-search)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-upstash"))
  (:start (lambda ()
            (check-config)
            (setf *index-ready* nil
                  *turns* (make-hash-table :test #'equal)))))
