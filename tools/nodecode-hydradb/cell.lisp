;;;; cell.lisp --- HydraDB long-term memory: recall, remember, forget, and the turn capture.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of hydra-db/hydradb-hermes-plugin (see NOTICE). The plugin is a
;;;; Hermes memory provider: it recalls before a turn (POST /query, preferring
;;;; the server's own llm_prompt) and stores the finished turn off-thread
;;;; (POST /context/ingest). The wire shapes, the {"success","data","error"}
;;;; envelope, the chunk unwrap and the Bearer key are the original client's;
;;;; the harness seams are Nodecode's: the section is the config, the tools are
;;;; NLE:REGISTER-TOOL registrations, and a finished turn arrives on the :frame
;;;; point instead of through a provider interface.
;;;;
;;;; The original's three public endpoints, and no other:
;;;;   POST /query           hybrid recall (JSON body), data.chunks and data.llm_prompt
;;;;   POST /context/ingest  store knowledge (multipart app_knowledge)
;;;;   DELETE /context       remove context by id (JSON body)
;;;; Writes are knowledge whatever "kind" recall reads: the original says the
;;;; memory-family write route is not on the public API (client.py:9-11).

(defpackage #:nodecode-hydradb
  (:use #:cl)
  (:export #:hydradb-error #:hydradb-error-detail))

(in-package #:nodecode-hydradb)

(nlk:define-peripheral hydradb :not-running t :lock "hydradb")

(defparameter +default-root+ "https://api.hydradb.com"
  "HydraDB's public API, where the cell goes unless base_url names another.")

(defparameter +write-kind+ "knowledge"
  "The family every write lands in: the public ingest route carries app_knowledge only.")

(defparameter +blank+ '(#\Space #\Tab #\Newline #\Return)
  "What a wire string is trimmed of before it is read, as the original's strip() is.")

(defparameter +breaker-threshold+ 5
  "Consecutive backend failures before the breaker pauses the capture path.")

(defparameter +breaker-cooldown+ 120
  "Seconds an open breaker waits before it tries the backend again.")

(defparameter +queued-turns+ 8
  "Finished turns waiting for the ingest, newest kept: a memory write must not grow without bound.")

(defvar *queued* '()
  "Finished turns waiting for the ingest, newest first: (TURN-ID . (USER . ANSWER)).")

(defvar *turns* (make-hash-table :test #'equal)
  "Each running turn's two texts, by turn id: (USER . ANSWER), until it completes.")

(defvar *worker* nil
  "The capture worker (NLK:WORKER-START) while the cell runs, or NIL.")

(defvar *breaker-failures* 0
  "Consecutive backend failures on the capture path, for the breaker.")

(defvar *breaker-until* 0
  "Universal time the open breaker may try the backend again.")

;;; --- the section, read back --------------------------------------------------

(defun non-empty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (string-trim +blank+ value))) value))

(defun env-value (name)
  "The environment variable NAME, when it is set to something."
  (and (stringp name) (plusp (length name)) (non-empty (uiop:getenv name))))

(defun api-key ()
  "The key the section names, inline or by environment variable, else HYDRADB_API_KEY."
  (or (non-empty (setting :api-key))
      (env-value (setting :api-key-env))
      (env-value "HYDRADB_API_KEY")))

(defun database ()
  "The database (tenant) recall reads and writes land in, else HYDRADB_DATABASE."
  (or (non-empty (setting :database)) (env-value "HYDRADB_DATABASE")))

(defun collection ()
  "The collection (sub-tenant) inside the database, else HYDRADB_COLLECTION, else nothing."
  (or (non-empty (setting :collection)) (env-value "HYDRADB_COLLECTION") ""))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (non-empty (setting :base-url))
                                           (env-value "HYDRADB_BASE_URL")
                                           ""))))
    (if (plusp (length given)) given +default-root+)))

(defun check-config ()
  "Refuse a section that could never reach HydraDB, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "hydradb: a key is required: set HYDRADB_API_KEY, or \"api_key\" "
                                 "(or \"api_key_env\") in the hydradb section")))
  (unless (database)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "hydradb: \"database\" is required: the HydraDB database (tenant) "
                                 "recall reads and capture writes to, or HYDRADB_DATABASE"))))

;;; --- the wire -----------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim +blank+ answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((hash-table-p answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (and (hash-table-p body) (nlk:json-value body :string "message"))
        (and (hash-table-p body) (nlk:json-value body :string "error"))
        (and (stringp answer) (subseq answer 0 (min 200 (length answer))))
        "no answer")))

(defun data-of (body answer)
  "BODY's data member, refusing the envelope's error first: the original's _unwrap,
where success false raises the envelope's own code and message."
  (unless (and (hash-table-p body) (nlk:json-value body :boolean "success"))
    (let ((error-object (and (hash-table-p body) (nlk:json-value body :object "error"))))
      (fail "~a~@[: ~a~]"
            (or (and error-object (nlk:json-value error-object :string "code"))
                "ERROR")
            (or (and error-object (nlk:json-value error-object :string "message"))
                (body-error-text answer)))))
  (or (and (hash-table-p body) (nlk:json-value body :object "data"))
      (nlk:json-object)))

(defun auth-headers ()
  (list (cons "authorization" (format nil "Bearer ~a" (or (api-key) "")))))

(defun json-headers ()
  '(("content-type" . "application/json")))

(defun call (method path &key body content headers)
  "METHOD PATH on the configured root, BODY the JSON text, CONTENT a multipart
alist => the envelope's data. A failure below HTTP, or a refused envelope,
signals the cell's condition."
  (let* ((url (concatenate 'string (root) path))
         (limit (setting :timeout-seconds))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (append headers (auth-headers))
                    :content (or body content)
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" (string-downcase (string method))
              (nle:transport-failure-label condition url))))
    (cond ((< status 300) (data-of (decode-body answer) answer))
          (t (fail "~a ~a: HTTP ~d~@[: ~a~]" (string-upcase (string method)) path status
                   (body-error-text answer))))))

;;; --- recall --------------------------------------------------------------------

(defun object-end (text)
  "TEXT's first JSON object ends before this index, or NIL when it never closes.
Strings and their escapes are stepped over, so a brace inside one is text."
  (let ((depth 0) (in-string nil) (escaped nil))
    (loop for character across text for index from 0
          do (cond (escaped (setf escaped nil))
                   ((and in-string (eql character #\\)) (setf escaped t))
                   (in-string (when (eql character #\") (setf in-string nil)))
                   ((eql character #\") (setf in-string t))
                   ((eql character #\{) (incf depth))
                   ((eql character #\}) (when (zerop (decf depth)) (return (1+ index)))))
          finally (return nil))))

(defun envelope-prose (object)
  "OBJECT's own prose -- content.text, content.markdown, content or text -- when it has some."
  (when (hash-table-p object)
    (let ((content (nlk:json-value object :object "content")))
      (or (and content (non-empty (nlk:json-value content :string "text")))
          (and content (non-empty (nlk:json-value content :string "markdown")))
          (non-empty (nlk:json-value object :string "content"))
          (non-empty (nlk:json-value object :string "text"))))))

(defun prose-of (value)
  "VALUE -- a chunk's text -- with a stored content envelope unwrapped to its prose.
Recall may return the stored envelope JSON followed by trailing graph enrichment: the
envelope is decoded from the front, its prose kept and the enrichment appended, as the
original's _extract_text does. Text that merely looks JSON-ish is passed through."
  (if (not (and (stringp value) (plusp (length (string-trim +blank+ value)))))
      (or value "")
      (let ((stripped (string-trim +blank+ value)))
        (if (not (eql #\{ (char stripped 0)))
            value
            (let ((end (object-end stripped)))
              (if (null end)
                  value
                  (let* ((object (handler-case (nlk:decode-json (subseq stripped 0 end))
                                    (error () nil)))
                         (prose (envelope-prose object))
                         (trailing (string-trim +blank+ (subseq stripped end))))
                    (cond ((null prose) value)
                          ((plusp (length trailing)) (format nil "~a~%~%~a" prose trailing))
                          (t prose)))))))))

(defun chunk-prose (chunk)
  (prose-of (or (nlk:json-value chunk :string "chunk_content")
                (nlk:json-value chunk :string "text")
                (nlk:json-value chunk :string "content")
                "")))

(defun chunk-id (chunk)
  (or (nlk:json-value chunk :string "chunk_uuid") (nlk:json-value chunk :string "id") ""))

(defun chunk-score (chunk)
  (or (nlk:json-value chunk :number "relevancy_score")
      (nlk:json-value chunk :number "score")
      (nlk:json-value chunk :number "relevance_score")))

(defun format-passages (query chunks)
  "CHUNKS as the lines the model reads: one per passage, its id and its score."
  (let ((passages (remove-if-not (lambda (chunk) (plusp (length (chunk-prose chunk))))
                                  chunks)))
    (when passages
      (with-output-to-string (out)
        (format out "~d passage~p for ~s:" (length passages) (length passages) query)
        (dolist (chunk passages)
          (let ((id (chunk-id chunk)))
            (format out "~% - [~a] ~a~@[ (score ~,2f)~]"
                    (if (plusp (length id)) id "?") (chunk-prose chunk)
                    (chunk-score chunk))))))))

(defun recall-text (query &key (kind (setting :kind)) (max-results (setting :search-limit)))
  "QUERY's recalled context, ready to show: the server-rendered llm_prompt when there
is one, else the passages as lines, else a line saying nothing matched."
  (let* ((payload (nlk:json-object "tenant_id" (database)
                                    "sub_tenant_id" (collection)
                                    "query" query
                                    "kind" kind
                                    "mode" "fast"
                                    "max_results" max-results))
         (data (call :post "/query"
                     :body (nlk:encode-json-object payload)
                     :headers (json-headers)))
         (prompt (non-empty (nlk:json-value data :string "llm_prompt")))
         (chunks (coerce (or (nlk:json-value data :array "chunks") #()) 'list)))
    (or prompt
        (format-passages query chunks)
        (format nil "No HydraDB context matched ~s." query))))

;;; --- what is stored -------------------------------------------------------------

(defun note-id ()
  "A fresh note id: the original's `note-' and 32 random hexadecimal digits."
  (format nil "note-~(~{~2,'0x~}~)" (coerce (nlk:random-bytes 16) 'list)))

(defun ingest (items)
  "Store ITEMS -- ingest item objects -- on the public knowledge route.
The multipart body is the original's add_knowledge: tenant_id, sub_tenant_id, kind and
the app_knowledge array as one form field."
  (call :post "/context/ingest"
        :content (list (cons "tenant_id" (database))
                       (cons "sub_tenant_id" (collection))
                       (cons "kind" +write-kind+)
                       (cons "app_knowledge"
                             (nlk:encode-json-object (coerce items 'vector))))))

(defun add-text (text &key title infer metadata source-id)
  "Store one note as knowledge: TEXT, a TITLE, and whether HydraDB infers facts from it."
  (ingest (list (nlk:json-object "id" (or source-id (note-id))
                                  "title" (or title "note")
                                  "content" (nlk:json-object "text" text)
                                  "metadata" (or metadata (nlk:json-object))
                                  "infer" (and infer t)))))

(defun add-conversation (user answer &key infer)
  "Store one finished turn as the one note the original's add_conversation writes."
  (add-text (format nil "User: ~a~%Assistant: ~a" user answer)
            :infer infer :title "conversation"))

(defun forget-contexts (ids &key (kind +write-kind+))
  "Remove IDS from the configured database: the original client's delete, which the
plugin itself never called."
  (when ids
    (call :delete "/context"
          :body (nlk:encode-json-object (nlk:json-object "tenant_id" (database)
                                                          "sub_tenant_id" (collection)
                                                          "ids" (coerce ids 'vector)
                                                          "kind" kind))
          :headers (json-headers))))

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: hydradb: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim +blank+ value))))
        (string-trim +blank+ value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-integer (args name default)
  (let ((value (gethash name args)))
    (if (integerp value) (max 1 (min value 50)) default)))

(defun argument-boolean (args name default)
  (multiple-value-bind (value present) (gethash name args)
    (declare (ignore value))
    (if present (and (gethash name args) t) default)))

(defun argument-strings (args name)
  "ARGS' NAME -- a string or an array of strings -- as the list of non-empty strings."
  (let ((value (gethash name args)))
    (remove-if-not (lambda (item) (and (stringp item) (plusp (length item))))
                   (cond ((stringp value) (list (string-trim +blank+ value)))
                         ((vectorp value) (coerce value 'list))
                         (t '())))))

(defun tool-recall (args)
  (answering
    (let ((query (argument-text args "query")))
      (recall-text query
                   :max-results (argument-integer args "max_results" nil)
                   :kind (or (non-empty (gethash "kind" args)) (setting :kind))))))

(defun tool-remember (args)
  (answering
    (let* ((text (argument-text args "text"))
           (title (non-empty (gethash "title" args)))
           (infer (argument-boolean args "infer" (setting :infer))))
      (add-text text :title title :infer infer)
      (format nil "Queued for storage~@[: ~a~]; HydraDB indexes it in the background."
              title))))

(defun tool-forget (args)
  (answering
    (let* ((ids (argument-strings args "ids"))
           (kind (or (non-empty (gethash "kind" args)) (setting :kind))))
      (unless ids (fail "ids is required: the context ids a recall returned"))
      (let ((count (nlk:json-value (forget-contexts ids :kind kind) :integer "deleted_count")))
        (format nil "Removed ~d context~p." (or count (length ids)) (or count (length ids)))))))

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

(defparameter *recall-schema*
  (tool-schema
   "Recall what HydraDB remembers by meaning: the passages nearest a question, with their ids. Use it before answering anything that may depend on what you know about the user or the work -- preferences, facts, decisions, people, past sessions -- and call it again with different wording for a multi-part question. Every finished turn is captured in the background, so a passage may hold something said earlier in this session."
   '(("query" "string" "What to search for." t)
     ("max_results" "integer" "Max passages (default: the section's search_limit, at most 50)." nil)
     ("kind" "string" "The family to search: knowledge (the default) or memory." nil)))
  "The hydradb_recall tool.")

(defparameter *remember-schema*
  (tool-schema
   "Store a note in HydraDB's long-term memory, so a later session recalls it. Call it the moment the user states a lasting preference, correction, decision or detail worth recalling; skip transient chat. With infer on (the section's default) HydraDB extracts facts from the note; off stores it verbatim."
   '(("text" "string" "What to store." t)
     ("title" "string" "A short title for the note." nil)
     ("infer" "boolean" "Let HydraDB extract facts from it (default: the section's infer)." nil)))
  "The hydradb_remember tool.")

(defparameter *forget-schema*
  (tool-schema
   "Remove contexts from HydraDB by id, as a hydradb_recall result returned them. Use it when the user asks to forget something; to correct a fact, store the new one instead."
   '(("ids" "array" "The context ids to remove." t)
     ("kind" "string" "The family they are in: knowledge (the default) or memory." nil)))
  "The hydradb_forget tool.")

;;; --- capture: a finished turn, ingested off the hot path -------------------------

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
    (with-hydradb-lock
      (let ((entry (or (gethash turn *turns*)
                       (setf (gethash turn *turns*) (cons nil nil)))))
        (ecase key
          (:user (setf (car entry) text))
          (:answer (setf (cdr entry) text)))))))

(defun take-turn (turn)
  (with-hydradb-lock (prog1 (gethash turn *turns*) (remhash turn *turns*))))

(defun enqueue-turn (turn texts)
  "Queue TURN's (USER . ANSWER) for the ingest; the newest turns are kept."
  (when (and (stringp turn) (car texts) (cdr texts))
    (with-hydradb-lock
      (setf *queued* (cons (cons turn texts) *queued*))
      (when (> (length *queued*) +queued-turns+)
        (setf *queued* (subseq *queued* 0 +queued-turns+))))
    (nlk:worker-poke *worker*)))

(defun pop-queued (&aux (entry nil))
  (with-hydradb-lock
    (when *queued* (setf entry (first *queued*) *queued* (rest *queued*))))
  entry)

(defun capture-turn (user answer)
  "Send one turn to HydraDB as a knowledge note, best effort: the original's sync_turn
calls add_conversation, which asks for no extraction unless infer_capture says so."
  (unless (breaker-open-p)
    (handler-case
        (progn
          (add-conversation user answer :infer (setting :infer-capture))
          (record-success))
      (error (condition)
        (record-failure)
        (warn "hydradb: a turn was not stored: ~a" condition)))))

(defun capture-step ()
  "The worker's step: ingest every queued turn."
  (loop for entry = (pop-queued)
        while entry
        do (capture-turn (cadr entry) (cddr entry)))
  nil)

(defun observe-frame (op)
  "Fold one published frame: a turn's two texts kept, a finished turn queued."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when (and type turn *hydradb* (setting :capture))
      (cond ((equal type "turn.input_committed")
             (remember-turn turn :user (nlk:fact-message-content payload)))
            ((equal type "turn.assistant_message_completed")
             (remember-turn turn :answer (nlk:fact-message-content payload)))
            ((equal type "turn.completed")
             (let ((texts (take-turn turn)))
               (when texts (enqueue-turn turn texts))))))))

;;; --- the manual and /hydradb ----------------------------------------------------

(defparameter +primer+
  "HydraDB is this session's long-term memory, server-side: it keeps what is said and hands
back the passages nearest a question. (help :hydradb) is this page; the tools are these three.
  hydradb_recall {query, max_results?, kind?}   the passages nearest a question, with their ids
  hydradb_remember {text, title?, infer?}       store one note a later session can recall
  hydradb_forget {ids, kind?}                   remove contexts by the id a recall returned
Recall before answering anything that may depend on what you know about the user or the work;
for a multi-part question, recall several times with different wording, and take an id from a
recall before forgetting it. Every finished turn is stored in the background, so HydraDB
already holds the conversation; the tools add and read on purpose.
The hydradb section of config.jsonc names the backend: the key (api_key, api_key_env, or
HYDRADB_API_KEY), the database (tenant) and its optional collection (sub-tenant), the
base_url, and the kind recall searches -- knowledge, the original's default, which is also
where every write lands."
  "What (help :hydradb) answers while the cell runs.")

(defun status-line ()
  (let ((collection (collection)))
    (format nil "HydraDB memory: database ~s~@[, collection ~s~] at ~a, kind ~a, ~
                 capture ~:[off~;on~].~%~
                 /hydradb QUERY recalls; the tools hydradb_recall, hydradb_remember and ~
                 hydradb_forget do the rest."
            (database) (and (plusp (length collection)) collection) (root)
            (setting :kind) (setting :capture))))

(defun run-command (args session-id)
  "One /hydradb invocation: a query recalls it, no argument the backend."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (answering (recall-text query))
        (status-line))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell hydradb
  (:section ("hydradb")
    (:guide "a HydraDB key and the database it reads are required: set HYDRADB_API_KEY and HYDRADB_DATABASE, or api_key and database here; next step: /hydradb shows the backend")
    ("api_key" :secret
     :doc "the HydraDB API key")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (HYDRADB_API_KEY is read either way)")
    ("database" :string
     :doc "the database (tenant) recall reads and capture writes to; HYDRADB_DATABASE is read when this is unset")
    ("collection" :string
     :doc "the collection (sub-tenant) inside the database; HYDRADB_COLLECTION is read when this is unset")
    ("base_url" :string
     :doc "the API address: https://api.hydradb.com unless answered (HYDRADB_BASE_URL is read too)")
    ("kind" :choice :options '("knowledge" "memory") :default "knowledge"
     :doc "the family recall searches: knowledge, as the original recalls, or memory")
    ("search_limit" :integer :default 6 :min 1
     :doc "how many passages one recall asks for unless the call says otherwise")
    ("infer" :boolean :default t
     :doc "let HydraDB extract facts from what hydradb_remember stores")
    ("infer_capture" :boolean :default nil
     :doc "let HydraDB extract facts from a captured turn too, as the original does not")
    ("capture" :boolean :default t
     :doc "store each finished turn in HydraDB in the background")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "one request's ceiling"))
  (:help :hydradb
   "hydradb: hydradb_recall, hydradb_remember and hydradb_forget over HydraDB's long-term memory"
   +primer+)
  (:command "hydradb" 'run-command
            :description "Show the HydraDB memory backend, or recall from it"
            :argument-hint "QUERY")
  (:tool "hydradb_recall" *recall-schema* 'tool-recall)
  (:tool "hydradb_remember" *remember-schema* 'tool-remember)
  (:tool "hydradb_forget" *forget-schema* 'tool-forget)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-hydradb"))
  (:start (lambda ()
            (check-config)
            (setf *turns* (make-hash-table :test #'equal)
                  *queued* '()
                  *breaker-failures* 0
                  *breaker-until* 0)
            (setf *worker* (nlk:worker-start "hydradb-capture" #'capture-step :wake 60))
            (nle:on-stop (lambda ()
                           (nlk:worker-stop *worker*)
                           (setf *worker* nil
                                 *queued* '()
                                 *turns* (make-hash-table :test #'equal)))))))
