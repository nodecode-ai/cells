;;;; cell.lisp --- Supermemory long-term memory: four tools, /supermemory, and the
;;;; capture of finished turns.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of supermemoryai/hermes-supermemory (see NOTICE).
;;;; What the original gives a user is kept: the four tools and their schemas,
;;;; the container tag, the entity context that steers extraction, the
;;;; [role: x]...[x:end] turn layout, the 4-hour custom_id bucket that makes
;;;; the API append a session's turns to one document, and the bounded retry
;;;; buffer. What changes is the harness: the section is the config, the tools
;;;; are NLE:REGISTER-TOOL registrations, and a finished turn arrives on the
;;;; :frame point instead of through a memory-provider interface.
;;;;
;;;; The wire is Supermemory's REST, the one the Python SDK the original pins
;;;; speaks:
;;;;   POST   /v3/documents   {content, containerTags, metadata, customId, entityContext}
;;;;   POST   /v3/search      {q, containerTag, limit, searchMode}
;;;;   POST   /v4/profile     {containerTag, q}
;;;;   DELETE /v4/memories    {id, containerTag}
;;;; A self-hosted server answering the same shapes is named by base_url.

(defpackage #:nodecode-supermemory
  (:use #:cl)
  (:export #:supermemory-error #:supermemory-error-detail))

(in-package #:nodecode-supermemory)

(nlk:define-peripheral supermemory :not-running t :lock "supermemory")

;;; --- what the original fixes -----------------------------------------------

(defparameter +default-root+ "https://api.supermemory.ai"
  "Supermemory's cloud, where the cell goes unless base_url names another.")

(defparameter +source+ "nodecode"
  "What every write is stamped with -- sm_source in metadata and x-sm-source
on the request. The original's routing key: in the Supermemory app it puts a
write in that source agent's Space, so a user can filter and bulk-manage it.")

(defparameter +default-container+ "nodecode"
  "The data partition writes land in and reads look at, when the section names none.")

(defparameter +search-modes+ '("hybrid" "memories" "documents")
  "The searchMode values the API documents, as the original's _VALID_SEARCH_MODES.")

(defparameter +capture-bucket-hours+ 4
  "One capture document per session per this many hours, as the original's
_CAPTURE_BUCKET_HOURS: within a window the custom_id is the same, so the API
appends the turns to one document instead of starting another.")

(defparameter +max-pending-turns+ 50
  "Turns the retry buffer keeps: a down service must not accumulate forever.")

(defparameter +max-pending-bytes+ (* 256 1024)
  "Bytes of turn text the retry buffer keeps.")

(defparameter +entity-context-limit+ 1500
  "Characters of entity context the API takes.")

(defparameter +default-entity-context+
  (let ((gap (format nil "~%~%")))
    (concatenate 'string
                 "User-assistant conversation. Format: [role: user]...[user:end] and [role: assistant]...[assistant:end]."
                 gap
                 "Only extract things useful in future conversations. Most messages are not worth remembering."
                 gap
                 "Remember lasting personal facts, preferences, routines, tools, ongoing projects, working context, "
                 "and explicit requests to remember something."
                 gap
                 "Do not remember temporary intents, one-time tasks, assistant actions, implementation details, or in-progress status."
                 gap
                 "When in doubt, store less."))
  "The extraction guidance every capture carries, verbatim from the original's
_DEFAULT_ENTITY_CONTEXT; the section's entity_context replaces it.")

(defparameter +sentence-ends+ '(#\. #\! #\? #\。 #\！ #\？))

(defparameter +breaker-threshold+ 5)
(defparameter +breaker-cooldown+ 120)

;;; --- the cell's own state ----------------------------------------------------

(defvar *worker* nil
  "The capture worker (NLK:WORKER-START) while the cell runs, or NIL.")

(defvar *turns* (make-hash-table :test #'equal)
  "Each running turn's two texts, by (SESSION . TURN), until it completes.")

(defvar *pending* '()
  "Turns waiting for a write, oldest first: (:session S :user U :answer A).")

(defvar *breaker-failures* 0)
(defvar *breaker-until* 0)

;;; --- the section, read back --------------------------------------------------

(defun non-empty (value)
  "VALUE when it is a string with something in it, else NIL."
  (and (stringp value) (plusp (length (nlk:trimmed value))) value))

(defun api-key ()
  "The key the section names, inline or by environment variable, else SUPERMEMORY_API_KEY."
  (or (non-empty (setting :api-key))
      (let ((variable (non-empty (setting :api-key-env))))
        (and variable (non-empty (uiop:getenv variable))))
      (non-empty (uiop:getenv "SUPERMEMORY_API_KEY"))))

(defun root ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-root+)))

(defun sanitize-tag (raw)
  "RAW as a container tag: [a-zA-Z0-9_] only, runs of _ folded, as the original's
_sanitize_tag; an empty answer is the default container."
  (let* ((spaced (cl-ppcre:regex-replace-all "[^a-zA-Z0-9_]" (or raw "") "_"))
         (folded (cl-ppcre:regex-replace-all "_+" spaced "_"))
         (trimmed (string-trim "_" folded)))
    (if (plusp (length trimmed)) trimmed +default-container+)))

(defun container-tag ()
  "The data partition this cell writes to and reads from."
  (sanitize-tag (setting :container-tag)))

(defun entity-context ()
  "The extraction guidance a capture carries: the section's, clipped to what
the API takes, else the original's default."
  (let ((given (non-empty (setting :entity-context))))
    (if given
        (subseq (nlk:trimmed given) 0 (min (length (nlk:trimmed given)) +entity-context-limit+))
        +default-entity-context+)))

(defun check-config ()
  "Refuse a section that could never reach the service, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "supermemory: needs an API key: set SUPERMEMORY_API_KEY, or \"api_key\" "
                                 "(or \"api_key_env\") in the supermemory section; a key is made at "
                                 "https://console.supermemory.ai/keys"))))

;;; --- the wire -----------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (or answer (nlk:json-object)))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "error")
        (nlk:json-value body :string "message")
        (nlk:json-value body :string "details")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun request-headers ()
  "The headers every call carries: JSON in, the key, and the source stamp."
  (list (cons "content-type" "application/json")
        (cons "authorization" (format nil "Bearer ~a" (or (api-key) "")))
        (cons "x-sm-source" +source+)))

(defun call (method path &key body)
  "METHOD PATH on the configured root, BODY its JSON text => the decoded answer.
A failure below HTTP or a refused status signals SUPERMEMORY-ERROR."
  (let* ((url (concatenate 'string (root) path))
         (limit (setting :timeout-seconds))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (request-headers)
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

;;; --- the four calls -------------------------------------------------------------

(defun merge-metadata (metadata)
  "METADATA -- an object the caller gave -- with this source stamped in, as the
original's _merge_metadata: sm_source first, the caller's own keys over it, and
a legacy \"source\" read as \"type\"."
  (let ((merged (nlk:copy-json-object metadata "sm_source" +source+)))
    (let ((legacy (gethash "source" merged)))
      (when (and (stringp legacy) (plusp (length legacy)))
        (remhash "source" merged)
        (unless (gethash "type" merged) (setf (gethash "type" merged) legacy))))
    merged))

(defun add-document (content &key metadata custom-id entity-context)
  "POST CONTENT as one document of this container; answers the decoded body."
  (call :post "/v3/documents"
        :body (nlk:encode-json-object
               (nlk:json-object
                "content" (nlk:trimmed content)
                "containerTags" (vector (container-tag))
                :opt "metadata" (and metadata (merge-metadata metadata))
                :opt "customId" (non-empty custom-id)
                :opt "entityContext" (non-empty entity-context)))))

(defun search-memories (query &key (limit (setting :search-limit)) mode)
  "The memories nearest QUERY in this container, as the service's result objects."
  (let ((body (nlk:json-object
               "q" query
               "containerTag" (container-tag)
               "limit" (max 1 (min 20 (or limit (setting :search-limit))))
               :when (member (or mode (setting :search-mode)) +search-modes+ :test #'equal)
               "searchMode" (or mode (setting :search-mode)))))
    (results-of (call :post "/v3/search" :body (nlk:encode-json-object body)))))

(defun fetch-profile (&optional query)
  "The profile of this container -- its lasting facts and the memories nearest
QUERY -- as the service answers it."
  (call :post "/v4/profile"
        :body (nlk:encode-json-object (nlk:json-object
                                       "containerTag" (container-tag)
                                       :opt "q" (non-empty query)))))

(defun forget-memory (id)
  "Forget the memory ID in this container."
  (call :delete "/v4/memories"
        :body (nlk:encode-json-object (nlk:json-object
                                       "id" id
                                       "containerTag" (container-tag)))))

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: supermemory: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (nlk:trimmed value))))
        (nlk:trimmed value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-integer (args name default)
  (let ((value (gethash name args)))
    (cond ((integerp value) value)
          ((and (stringp value) (ignore-errors (parse-integer value))) (parse-integer value))
          (t default))))

(defun format-results (query results)
  "RESULTS as the lines the model reads: one per memory, its id and its score."
  (let ((facts (remove-if-not (lambda (result)
                                 (and (hash-table-p result)
                                      (let ((text (nlk:json-value result :string "memory")))
                                        (and text (plusp (length text))))))
                               results)))
    (if (null facts)
        (format nil "No memory matched ~s in container ~s." query (container-tag))
        (with-output-to-string (out)
          (format out "~d ~a in container ~s for ~s:"
                  (length facts) (if (= 1 (length facts)) "memory" "memories")
                  (container-tag) query)
          (dolist (result facts)
            (format out "~% - [~a] ~a~@[ (similarity ~,2f)~]"
                    (or (nlk:json-value result :string "id") "?")
                    (nlk:json-value result :string "memory")
                    (nlk:json-value result :number "similarity")))))))

(defun tool-store (args)
  (answering
    (let* ((content (argument-text args "content"))
           (metadata (gethash "metadata" args))
           (answer (add-document content
                                  :metadata (and (hash-table-p metadata) metadata))))
      (format nil "Stored in container ~s.~@[ (id ~a)~]"
              (container-tag) (nlk:json-value answer :string "id")))))

(defun tool-search (args)
  (answering
    (let ((query (argument-text args "query")))
      (format-results query
                      (search-memories query
                                        :limit (argument-integer args "limit" nil))))))

(defun tool-forget (args)
  (answering
    (let ((id (non-empty (gethash "id" args)))
          (query (non-empty (gethash "query" args))))
      (cond
        (id (forget-memory (nlk:trimmed id))
            (format nil "Forgot memory ~a." (nlk:trimmed id)))
        (query
         (let ((results (search-memories (nlk:trimmed query) :limit 5)))
           (cond
             ((null results) (format nil "No memory matched ~s to forget." (nlk:trimmed query)))
             (t (let* ((best (first results))
                       (best-id (nlk:json-value best :string "id"))
                       (text (nlk:json-value best :string "memory")))
                  (unless best-id (fail "the best matching memory has no id"))
                  (forget-memory best-id)
                  (format nil "Forgot: \"~a\" (~a)" (subseq (or text "") 0 (min 100 (length (or text ""))))
                          best-id))))))
        (t (fail "either id or query is required"))))))

(defun tool-profile (args)
  (answering
    (let* ((query (non-empty (gethash "query" args)))
           (body (fetch-profile (and query (nlk:trimmed query))))
           (static (nlk:json-value body :array "profile" "static"))
           (dynamic (nlk:json-value body :array "profile" "dynamic"))
           (related (results-of (or (nlk:json-value body :object "searchResults") body))))
      (with-output-to-string (out)
        (format out "Profile of container ~s~@[, focused on ~s~]." (container-tag) query)
        (flet ((fact-block (title items)
                 (when (plusp (length (or items #())))
                   (format out "~%~a:" title)
                   (loop for item across items
                         when (stringp item)
                           do (format out "~% - ~a" item)))))
          (fact-block "Lasting facts" (and (vectorp static) static))
          (fact-block "Recent context" (and (vectorp dynamic) dynamic))
          (fact-block "Related memories"
                 (coerce (loop for result in related
                               for text = (and (hash-table-p result)
                                               (or (nlk:json-value result :string "memory")
                                                   (nlk:json-value result :string "content")))
                               when (and text (plusp (length text))) collect text)
                         'vector)))))))

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

(defparameter *store-schema*
  (tool-schema "Store an explicit memory for future recall. Use it the moment the user states a lasting preference, correction, decision or personal detail worth recalling on a later turn."
                '(("content" "string" "The memory content to store." t)
                  ("metadata" "object" "Optional metadata attached to the memory." nil)))
  "supermemory_store, as the original's _BASE_SCHEMAS has it.")

(defparameter *search-schema*
  (tool-schema "Search long-term memory by semantic similarity. Use it before answering anything that may depend on what you know about the user, and search several times with different wording for a multi-part question."
                '(("query" "string" "What to search for." t)
                  ("limit" "integer" "Maximum results to return, 1 to 20." nil)))
  "supermemory_search, as the original's _BASE_SCHEMAS has it.")

(defparameter *forget-schema*
  (tool-schema "Forget a memory by exact id or by best-match query."
                '(("id" "string" "Exact memory id to delete." nil)
                  ("query" "string" "Query used to find the memory to forget." nil)))
  "supermemory_forget, as the original's _BASE_SCHEMAS has it.")

(defparameter *profile-schema*
  (tool-schema "Retrieve persistent profile facts and recent memory context."
                '(("query" "string" "Optional query to focus the profile response." nil)))
  "supermemory_profile, as the original's _BASE_SCHEMAS has it.")

;;; --- capture: finished turns written as conversation documents -------------------

(defun clean-text (text)
  "TEXT without the base64 data URIs a pasted image leaves, as the original's
_clean_text_for_capture: useless as memory text."
  (when (stringp text)
    (nlk:trimmed (cl-ppcre:regex-replace-all "data:[^;,\\s]+;base64,[A-Za-z0-9+/=]+" text "[image]"))))

(defun iso-timestamp (&optional (universal (get-universal-time)))
  "UNIVERSAL as an ISO 8601 UTC stamp, the shape a document's metadata carries."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun join-text (pieces separator)
  "PIECES joined by SEPARATOR, in order."
  (with-output-to-string (out)
    (loop for piece in pieces
          for first = t then nil
          do (unless first (write-string separator out))
             (write-string piece out))))

(defun format-turn (user answer)
  "One turn in the [role: x]...[x:end] layout the entity context describes."
  (join-text (loop for (role text) in `(("user" ,user) ("assistant" ,answer))
                    when (non-empty text)
                      collect (format nil "[role: ~a]~%~a~%[~a:end]" role text role))
             (format nil "~%~%")))

(defun capture-custom-id (session &optional (universal (get-universal-time)))
  "<session>_<YYYY-MM-DD>_b<0..5>: the same id inside one bucket window, so the
API appends a session's turns to the one document."
  (multiple-value-bind (second minute hour day month year) (decode-universal-time universal 0)
    (declare (ignore second minute))
    (format nil "~a_~4,'0d-~2,'0d-~2,'0d_b~d"
            (sanitize-tag session) year month day (floor hour +capture-bucket-hours+))))

(defun remember-turn (session turn key text)
  "Keep TEXT, the :USER or :ANSWER half of TURN, until the turn completes."
  (when (and (stringp session) (stringp turn) (non-empty text))
    (with-supermemory-lock
      (let ((entry (or (gethash (cons session turn) *turns*)
                       (setf (gethash (cons session turn) *turns*) (list session nil nil)))))
        (ecase key
          (:user (setf (second entry) text))
          (:answer (setf (third entry) text)))))))

(defun take-turn (session turn)
  (with-supermemory-lock
    (prog1 (gethash (cons session turn) *turns*)
      (remhash (cons session turn) *turns*))))

(defun pending-bytes (turns)
  (loop for turn in turns
        sum (+ (length (or (getf turn :user) "")) (length (or (getf turn :answer) "")))))

(defun bound-pending ()
  "Keep the retry buffer bounded: drop the OLDEST entries past the turn or
byte cap, as the original's _bound_pending_turns."
  (with-supermemory-lock
    (let ((kept *pending*))
      (loop while (and kept
                       (or (> (length kept) +max-pending-turns+)
                           (> (pending-bytes kept) +max-pending-bytes+)))
            do (pop kept))
      (setf *pending* kept))))

(defun enqueue-turn (session user answer)
  "Queue one finished turn for the next write."
  (when (and (non-empty user) (non-empty answer))
    (with-supermemory-lock
      (setf *pending* (append *pending* (list (list :session session :user user :answer answer)))))
    (bound-pending)
    (nlk:worker-poke *worker*)))

(defun group-by-session (turns)
  "TURNS grouped by session, sessions in the order they first appear."
  (let ((order '()) (groups (make-hash-table :test #'equal)))
    (dolist (turn turns)
      (let ((session (getf turn :session)))
        (unless (gethash session groups)
          (push session order)
          (setf (gethash session groups) '()))
        (setf (gethash session groups) (append (gethash session groups) (list turn)))))
    (loop for session in (reverse order)
          collect (cons session (gethash session groups)))))

(defun breaker-open-p ()
  (cond ((< *breaker-failures* +breaker-threshold+) nil)
        ((< (get-universal-time) *breaker-until*) t)
        (t (setf *breaker-failures* 0) nil)))

(defun record-success () (setf *breaker-failures* 0))

(defun record-failure ()
  (when (>= (incf *breaker-failures*) +breaker-threshold+)
    (setf *breaker-until* (+ (get-universal-time) +breaker-cooldown+))))

(defun write-batch (session turns)
  "One documents.add for SESSION's TURNS: the same custom_id inside one 4-hour
window, so the API appends."
  (add-document (join-text (mapcar (lambda (turn)
                                      (format-turn (getf turn :user) (getf turn :answer)))
                                    turns)
                            (format nil "~%~%"))
                :metadata (nlk:json-object
                           "type" "conversation"
                           "session_id" session
                           "timestamp" (iso-timestamp))
                :custom-id (capture-custom-id session)
                :entity-context (entity-context)))

(defun capture-pending ()
  "Write every queued turn, one document per session; a batch the service
refused stays pending, so a later tick retries it."
  (when (and *supermemory* (not (breaker-open-p)))
    (let ((batch (with-supermemory-lock (prog1 *pending* (setf *pending* '())))))
      (when batch
        (let ((failed '()))
          (dolist (group (group-by-session batch))
            (handler-case
                (progn (write-batch (car group) (cdr group))
                       (record-success))
              (error (condition)
                (record-failure)
                (setf failed (append failed (cdr group)))
                (warn "supermemory: ~d turn(s) of a session were not written: ~a"
                      (length (cdr group)) condition))))
          (when failed
            (with-supermemory-lock (setf *pending* (append failed *pending*)))
            (bound-pending)))))))

(defun capture-step ()
  "The worker's step: write what is queued."
  (capture-pending)
  nil)

(defun observe-frame (op)
  "Fold one published frame: a turn's two texts kept, a finished turn queued."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (let ((session (or (getf op :session-id) "default")))
      (when (and type turn *supermemory* (setting :capture))
        (cond ((equal type "turn.input_committed")
               (remember-turn session turn :user (clean-text (nlk:fact-message-content payload))))
              ((equal type "turn.assistant_message_completed")
               (remember-turn session turn :answer (clean-text (nlk:fact-message-content payload))))
              ((equal type "turn.completed")
               (let ((texts (take-turn session turn)))
                 (when texts (enqueue-turn session (second texts) (third texts))))))))))

;;; --- the manual and /supermemory -------------------------------------------------

(defparameter +primer+
  "Supermemory keeps a user's long-term memory server-side, in one container (a data partition):
finished turns are written to it as conversation documents, and facts are found again by meaning.
(help :supermemory) is this page; the tools are the four below.
  supermemory_store {content, metadata?}   store one explicit memory
  supermemory_search {query, limit?}       the memories nearest a question, with their ids
  supermemory_forget {id | query}          forget one memory, by id or by best match
  supermemory_profile {query?}             the profile's lasting facts and recent context
Search before answering anything that may depend on what you know about the user; for a
multi-part question, search several times with different wording. Take an id from a search
result before you forget it. Automatic recall before a reply is not wired here: the model
calls supermemory_search. With capture on, every finished turn is still written in the
background, one document per session per 4-hour window.
The supermemory section of config.jsonc names the key (SUPERMEMORY_API_KEY is read too), the
container, the search mode and limit, and whether turns are captured."
  "What (help :supermemory) answers while the cell runs.")

(defun status-line ()
  (format nil "Supermemory: ~a, container ~s, ~a search (~d results), capture ~a.~%Type /supermemory QUERY to search; supermemory_store, supermemory_search, supermemory_forget and supermemory_profile do the rest."
          (root) (container-tag) (setting :search-mode) (setting :search-limit)
          (if (setting :capture) "on" "off")))

(defun run-command (args session-id)
  "One /supermemory invocation: a query answers what memory holds, no argument the backend."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (handler-case (format-results query (search-memories query))
          ((or error sb-sys:deadline-timeout) (condition)
            (format nil "ERROR: supermemory: ~a" condition)))
        (status-line))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell supermemory
  (:section ("supermemory")
    (:guide "a key is required: set SUPERMEMORY_API_KEY, or api_key (or api_key_env) here; a key is made at https://console.supermemory.ai/keys?create=true; container_tag is the data partition, and capture writes every finished turn to it; next step: /supermemory shows the backend")
    ("api_key" :secret
     :doc "the Supermemory API key")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (SUPERMEMORY_API_KEY is read either way)")
    ("base_url" :string
     :doc "the API address: https://api.supermemory.ai unless answered; a self-hosted server answering the same shapes goes here")
    ("container_tag" :string :default +default-container+
     :doc "the data partition memories live in; the same tag across sessions finds the same memories")
    ("search_mode" :choice :options +search-modes+ :default "hybrid"
     :doc "what a search looks at: learned memories with their source chunks (hybrid), memories only, or source documents only")
    ("search_limit" :integer :default 10 :min 1
     :doc "how many memories one search asks for unless the call says otherwise (1 to 20)")
    ("capture" :boolean :default t
     :doc "write each finished turn to the container as a conversation, so facts are extracted without being asked")
    ("entity_context" :string
     :doc "guidance for what a capture is worth extracting (at most 1500 characters); empty uses the built-in")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "one request's ceiling"))
  (:help :supermemory "supermemory: save, search, forget and profile long-term memory on Supermemory" +primer+)
  (:command "supermemory" 'run-command
            :description "Show the Supermemory container, or search memory"
            :argument-hint "QUERY")
  (:tool "supermemory_store" *store-schema* 'tool-store)
  (:tool "supermemory_search" *search-schema* 'tool-search)
  (:tool "supermemory_forget" *forget-schema* 'tool-forget)
  (:tool "supermemory_profile" *profile-schema* 'tool-profile)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-supermemory"))
  (:start (lambda ()
            (check-config)
            (setf *turns* (make-hash-table :test #'equal)
                  *pending* '()
                  *breaker-failures* 0
                  *breaker-until* 0)
            (setf *worker* (nlk:worker-start "supermemory-capture" #'capture-step :wake 60))
            (nle:on-stop (lambda ()
                           (nlk:worker-stop *worker*)
                           (setf *worker* nil
                                 *pending* '()
                                 *turns* (make-hash-table :test #'equal)))))))
