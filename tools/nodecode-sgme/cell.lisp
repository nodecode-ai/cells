;;;; cell.lisp --- SGME: the memory engine's tools, /sgme, and the turn capture.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of freehul/sgme's Hermes memory-provider plugin (see NOTICE).
;;;; The wire shapes, the tool names and what each one asks the server are
;;;; the original's; the harness seams are Nodecode's: the section is the
;;;; config, the tools are NLE:DEFINE-CELL registrations, a finished turn
;;;; arrives on the :frame point, and the profile block rides the :round
;;;; point as one system-reminder instead of a provider's static prompt.
;;;;
;;;; Nothing heavy lives here, as in the original: no LLM, no database, no
;;;; embedding. Every request goes to the one configured SGME server, which
;;;; does refinement, recall, skills, the wiki, care signals and roles.

(defpackage #:nodecode-sgme
  (:use #:cl)
  (:export #:sgme-error #:sgme-error-detail))

(in-package #:nodecode-sgme)

(nlk:define-peripheral sgme :not-running t :lock "sgme")

(defparameter +default-root+ "http://127.0.0.1:9910"
  "The original's default: the SGME server on this machine.")

(defparameter +default-scopes+ '("memory" "skills")
  "The original's default search: the memory pool and the skills layer.")

(defparameter +scopes+ '("memory" "skills" "wiki" "wiki_pages" "sessions")
  "Every scope /v1/search answers.")

(defparameter +question-types+ '("temporal" "aggregate" "generic"))
(defparameter +ack-statuses+ '("claimed" "acked" "failed"))

;;; --- the section, read back --------------------------------------------------

(defun present-text (name)
  "Setting NAME as a trimmed non-empty string, else NIL."
  (let ((value (setting name)))
    (and (stringp value)
         (plusp (length (string-trim '(#\Space #\Tab) value)))
         (string-trim '(#\Space #\Tab) value))))

(defun key-from (inline variable default)
  "A key: the section's inline value, else the variable the section names,
else DEFAULT's name in the environment."
  (or (present-text inline)
      (let ((named (present-text variable)))
        (and named (plusp (length named)) (uiop:getenv named)))
      (uiop:getenv default)))

(defun agent-key () (key-from :agent-key :agent-key-env "SGME_AGENT_KEY"))
(defun admin-key () (key-from :admin-key :admin-key-env "SGME_ADMIN_KEY"))

(defun root ()
  "The SGME server this cell talks to, without a trailing slash."
  (string-right-trim "/" (or (present-text :base-url) +default-root+)))

(defun loopback-p ()
  "Whether the configured server is on this machine, where a dev key suffices."
  (let ((url (root)))
    (or (search "127.0.0.1" url)
        (search "//localhost" url)
        (search "::1" url))))

(defun check-config ()
  "Refuse a section that could never reach a server, naming the member."
  (let ((url (root)))
    (unless (plusp (length url))
      (error 'nlk:config-refusal
             :detail (concatenate 'string
                                   "sgme: \"base_url\" is empty: name the SGME server, "
                                   "http://127.0.0.1:9910 when it runs on this machine")))
    (when (and (not (loopback-p)) (null (agent-key)))
      (error 'nlk:config-refusal
             :detail (concatenate 'string
                                   "sgme: a server that is not on this machine needs an agent key: "
                                   "set SGME_AGENT_KEY, or \"agent_key_env\" (or \"agent_key\") in the "
                                   "sgme section")))))

(defun admin-needed ()
  "The admin key, or a refusal that says which member to set."
  (or (admin-key)
      (fail "this tool needs the admin key: set SGME_ADMIN_KEY, or \"admin_key_env\" (or \"admin_key\") in the sgme section")))

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
    (or (nlk:json-value body :text "message")
        (nlk:json-value body :text "error")
        (nlk:json-value body :text "detail")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun query-text (params)
  "PARAMS -- (NAME . VALUE) pairs -- as ?a=b&c=d, values url-encoded."
  (let ((pairs (loop for (name . value) in params
                      when (and value (plusp (length (princ-to-string value))))
                        collect (format nil "~a=~a" name
                                        (quri:url-encode (princ-to-string value) :encoding :utf-8)))))
    (when pairs (format nil "?~{~a~^&~}" pairs))))

(defun call (method path &key key params body timeout &aux (answer nil) (status nil))
  "METHOD PATH on the configured server => the decoded answer.
A failure below HTTP or a refused status signals SGME-ERROR."
  (let* ((url (concatenate 'string (root) path (or (query-text params) "")))
         (limit (or timeout (setting :timeout-seconds)))
         (headers (list (cons "content-type" "application/json"))))
    (when (and key (plusp (length key)))
      (push (cons "x-api-key" key) headers))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers headers
                    :content (and body (nlk:encode-json-object body))
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" (string-downcase (string method))
              (nle:transport-failure-label condition url))))
    (cond ((< status 300) (decode-body answer))
          (t (fail "~a ~a: HTTP ~d~@[: ~a~]" (string-upcase (string method)) path status
                   (body-error-text answer))))))

(defun results-of (body)
  "BODY's result objects: the \"results\" array of an object, or the array itself."
  (coerce (cond ((hash-table-p body) (nlk:json-array body "results"))
                ((vectorp body) body)
                (t #()))
          'list))

(defun json-text (value)
  "VALUE as the JSON text the model reads."
  (cond ((hash-table-p value) (nlk:encode-json-object value))
        ((vectorp value) (nlk:encode-json-object (nlk:json-object "results" value)))
        (t (princ-to-string value))))

;;; --- the tools' arguments -------------------------------------------------------

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-text-or (args name default)
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        default)))

(defun argument-integer (args name default maximum)
  (let ((value (gethash name args)))
    (if (integerp value) (max 1 (min value maximum)) default)))

(defun argument-list (args name default)
  "ARGS' NAME as a list of non-empty strings, else DEFAULT."
  (let ((value (gethash name args)))
    (cond ((vectorp value)
           (let ((items (loop for item across value
                              when (and (stringp item) (plusp (length (string-trim '(#\Space) item))))
                                collect (string-trim '(#\Space) item))))
             (or items default)))
          ((and (stringp value) (plusp (length (string-trim '(#\Space) value))))
           (list (string-trim '(#\Space) value)))
          (t default))))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P ITEMS) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (if (equal type "array")
                                                        (nlk:json-object "type" "array"
                                                                          "items" (nlk:json-object "type" "string")
                                                                          "description" doc)
                                                        (nlk:json-object "type" type
                                                                          "description" doc)))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: sgme: ~a" condition)))))

;;; --- search and its formatting -----------------------------------------------------

(defun result-line (result)
  "One result as the line the model reads: its id, its text, its score."
  (let ((text (or (nlk:json-value result :text "memory")
                  (nlk:json-value result :text "text")
                  (nlk:json-value result :text "content")
                  (nlk:json-value result :text "title")
                  (nlk:json-value result :text "name")
                  (json-text result)))
        (id (or (nlk:json-value result :text "id")
                (nlk:json-value result :text "memory_id")
                (nlk:json-value result :text "name")
                "?"))
        (score (nlk:json-value result :number "score")))
    (format nil " - [~a] ~a~@[ (score ~,2f)~]" id text score)))

(defun format-results (query results)
  "RESULTS as the lines the model reads, or one line saying nothing matched."
  (if (null results)
      (format nil "Nothing in the memory pool matched ~s; say so rather than guessing." query)
      (with-output-to-string (out)
        (format out "~d result~:p for ~s:" (length results) query)
        (dolist (result results) (format out "~%~a" (result-line result))))))

(defun sgme-search (query scopes limit)
  (results-of (call :post "/v1/search"
                    :key (agent-key)
                    :body (nlk:json-object "query" query
                                            "scopes" (coerce scopes 'vector)
                                            "limit" limit))))

(defun tool-memory-search (args)
  (answering
    (let* ((query (argument-text args "query"))
           (limit (argument-integer args "limit" (setting :search-limit) 20))
           (scopes (argument-list args "scopes" +default-scopes+)))
      (format-results query (sgme-search query scopes limit)))))

(defun tool-conversation-search (args)
  (answering
    (let ((query (argument-text args "query"))
          (limit (argument-integer args "limit" 5 20)))
      (format-results query (sgme-search query '("sessions") limit)))))

(defun tool-skill-search (args)
  (answering
    (let ((query (argument-text args "query"))
          (limit (argument-integer args "limit" 5 20)))
      (let ((found (sgme-search query '("skills") limit)))
        (if (null found)
            (format nil "No skill matched ~s." query)
            (with-output-to-string (out)
              (format out "~d skill~:p for ~s, name and trigger only; sgme_skill_get takes the full text:"
                      (length found) query)
              (dolist (result found) (format out "~%~a" (result-line result)))))))))

(defun tool-answer (args)
  (answering
    (let* ((query (argument-text args "query"))
           (kind (argument-text-or args "question_type" nil))
           (limit (gethash "limit" args)))
      (json-text
       (call :post "/v1/answer"
             :key (agent-key)
             :body (apply #'nlk:make-json-object
                          (append (list "query" query)
                                  (when (member kind +question-types+ :test #'equal)
                                    (list "question_type" kind))
                                  (when (integerp limit) (list "limit" (max 1 (min limit 20)))))))))))

(defun tool-inject (args)
  (answering
    (let ((mode (or (argument-text-or args "mode" nil) (setting :inject-mode))))
      (json-text (call :post "/v1/inject"
                       :key (agent-key)
                       :body (nlk:json-object "mode" mode
                                               "max_tokens" (setting :inject-max-tokens)))))))

(defun tool-wiki-search (args)
  (answering
    (let ((query (argument-text args "query"))
          (limit (argument-integer args "limit" 10 50)))
      (format-results query (results-of (call :get "/v1/wiki/search"
                                               :key (agent-key)
                                               :params (list (cons "q" query) (cons "limit" limit))))))))

(defun tool-wiki-pages (args)
  (answering
    (let ((category (argument-text-or args "category" nil)))
      (json-text (call :get "/v1/wiki/pages"
                       :key (agent-key)
                       :params (append (list (cons "limit" (argument-integer args "limit" 50 200))
                                             (cons "offset" (argument-integer args "offset" 0 100000)))
                                       (when category (list (cons "category" category)))))))))

(defun tool-skill-get (args)
  (answering
    (let ((name (argument-text args "name"))
          (section (argument-text-or args "section" nil)))
      (json-text (call :get (format nil "/v1/skills/~a" (quri:url-encode name :encoding :utf-8))
                       :key (agent-key)
                       :params (when section (list (cons "section" section))))))))

(defun tool-skill-list (args)
  (answering
    (json-text (call :get "/v1/skills"
                     :key (agent-key)
                     :params (list (cons "limit" (argument-integer args "limit" 50 500))
                                   (cons "offset" (argument-integer args "offset" 0 100000)))))))

;;; --- care signals -----------------------------------------------------------------

(defun tool-signal-pull (args)
  (answering
    (let* ((limit (argument-integer args "limit" 20 20))
           (body (call :get "/v1/events/pull"
                       :key (agent-key)
                       :params (list (cons "subscriber_id" (setting :agent-id))
                                     (cons "limit" limit))))
           (events (coerce (nlk:json-array body "events") 'list)))
      (if (null events)
          "No unconsumed care signals."
          (with-output-to-string (out)
            (format out "~d care signal~:p; claim one with sgme_signal_claim, then ack it with sgme_signal_ack:"
                    (length events))
            (dolist (event events)
              (format out "~% - [~a] ~a @~a ~a"
                      (or (nlk:json-value event :text "id") (nlk:json-value event :text "event_id") "?")
                      (or (nlk:json-value event :text "type") (nlk:json-value event :text "event_type") "?")
                      (or (nlk:json-value event :text "ts") (nlk:json-value event :text "timestamp") "")
                      (let ((payload (nlk:json-value event :any "payload")))
                        (if (stringp payload) payload (json-text payload))))))))))

(defun tool-signal-claim (args)
  (answering
    (let* ((event-id (argument-text args "event_id"))
           (path (format nil "/v1/admin/care/signals/~a/consume"
                         (quri:url-encode event-id :encoding :utf-8))))
      (handler-case
          (progn
            (call :post path :key (or (admin-key) (agent-key)))
            (format nil "Claimed signal ~a." event-id))
        (sgme-error (condition)
          (if (search "HTTP 409" (princ-to-string condition))
              (format nil "Signal ~a is already claimed by another agent; skip it." event-id)
              (error condition)))))))

(defun tool-signal-ack (args)
  (answering
    (let* ((event-id (argument-text args "event_id"))
           (status (or (argument-text-or args "status" nil) "acked"))
           (result (argument-text-or args "result" nil))
           (path (format nil "/v1/admin/care/signals/~a/ack"
                         (quri:url-encode event-id :encoding :utf-8))))
      (unless (member status +ack-statuses+ :test #'equal)
        (fail "status is claimed, acked or failed, not ~s" status))
      (call :post path
            :key (or (admin-key) (agent-key))
            :body (nlk:json-object "status" status "result" (or result "")))
      (format nil "Acked signal ~a as ~a." event-id status))))

(defun tool-health (args)
  (declare (ignore args))
  (answering
    (let ((body (call :get "/v1/health" :timeout (min (setting :timeout-seconds) 10))))
      (format nil "SGME at ~a answers: ~a" (root) (json-text body)))))

(defun tool-stats (args)
  (declare (ignore args))
  (answering (json-text (call :get "/v1/admin/stats" :key (admin-needed)))))

;;; --- capture: a finished turn to /v1/append, then a refinement ---------------------

(defvar *turns* (make-hash-table :test #'equal)
  "Each running turn's (SESSION USER ANSWER), until it completes.")

(defvar *queued* '()
  "Finished turns waiting for their append, oldest first.")

(defvar *worker* nil
  "The capture worker (NLK:WORKER-START) while the cell runs, or NIL.")

(defparameter +queued-turns+ 8
  "Turns waiting for their append; beyond this the oldest is dropped.")

(defvar *started-at* nil
  "When this cell started, as the ISO stamp /v1/append carries.")

(defun iso-now ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun remember-turn (turn session key text)
  "Keep TEXT, the :USER or :ANSWER half of TURN, in SESSION, until the turn completes."
  (when (and (stringp turn) (stringp text) (plusp (length text)))
    (with-sgme-lock
      (let ((entry (or (gethash turn *turns*)
                       (setf (gethash turn *turns*) (list session nil nil)))))
        (ecase key
          (:user (setf (second entry) text))
          (:answer (setf (third entry) text)))))))

(defun take-turn (turn)
  (with-sgme-lock (prog1 (gethash turn *turns*) (remhash turn *turns*))))

(defun enqueue-turn (entry)
  "Queue ENTRY -- (SESSION USER ANSWER) -- for its append; the newest are kept."
  (when (and (second entry) (third entry))
    (with-sgme-lock
      (setf *queued* (nconc *queued* (list entry)))
      (when (> (length *queued*) +queued-turns+)
        (setf *queued* (last *queued* +queued-turns+))))
    (nlk:worker-poke *worker*)))

(defun pop-queued (&aux (entry nil))
  (with-sgme-lock (when *queued* (setf entry (first *queued*) *queued* (rest *queued*))))
  entry)

(defun append-turn (entry)
  "Send ENTRY -- (SESSION USER ANSWER) -- to /v1/append, best effort."
  (handler-case
      (call :post "/v1/append"
            :key (agent-key)
            :body (nlk:json-object
                   "session_key" (format nil "nodecode-~a" (or (first entry) "session"))
                   "started_at" (or *started-at* (iso-now))
                   "agent_id" (setting :agent-id)
                   "content" (with-output-to-string (out)
                               (format out "# ~a user~%~a~%" (iso-now) (second entry))
                               (format out "# ~a assistant~%~a~%" (iso-now) (third entry)))))
    (error (condition) (warn "sgme: a turn was not appended: ~a" condition))))

(defun refine-trigger ()
  "Ask the server to refine what was appended; async there, best effort."
  (when (and (setting :refine-on-end) (admin-key))
    (handler-case
        (call :post "/v1/admin/refine/trigger_async"
              :key (admin-key) :timeout 5
              :body (nlk:json-object "limit" 50))
      (error (condition) (warn "sgme: a refinement was not triggered: ~a" condition)))))

(defun capture-step ()
  "The worker's step: append every queued turn, then refresh the profile block."
  (loop for entry = (pop-queued)
        while entry
        do (append-turn entry) (refine-trigger))
  (refresh-profile)
  nil)

(defun observe-frame (op)
  "Fold one published frame: a turn's two texts kept, a finished turn queued."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when (and type turn *sgme* (setting :capture))
      (let ((session (getf op :session-id)))
        (cond ((equal type "turn.input_committed")
               (remember-turn turn session :user (nlk:fact-message-content payload)))
              ((equal type "turn.assistant_message_completed")
               (remember-turn turn session :answer (nlk:fact-message-content payload)))
              ((equal type "turn.completed")
               (let ((entry (take-turn turn)))
                 (when entry (enqueue-turn entry)))))))))

;;; --- inject: the profile and the role, in the round's own prompt ----------------

(defvar *profile* nil
  "The block the server's profile and role answer, or NIL before it was fetched.")

(defvar *injected-turn* nil
  "The turn *PROFILE* was last recorded in, so one turn records it once.")

(defun profile-block ()
  "The server's profile as the block the model reads: the role prompt, then
each profile section with its items, or NIL when the server answers nothing."
  (let ((role (role-block))
        (lines '()))
    (handler-case
        (let ((body (call :post "/v1/inject"
                          :key (agent-key)
                          :body (nlk:json-object "mode" (setting :inject-mode)
                                                  "max_tokens" (setting :inject-max-tokens)))))
          (loop for block across (nlk:json-array body "blocks")
                for title = (or (nlk:json-value block :text "title") "")
                for items = (nlk:json-array block "items")
                when (plusp (length items))
                  do (push (format nil "[~a]" title) lines)
                     (loop for item across items
                           for content = (nlk:json-value item :text "content")
                           when content do (push (format nil "- ~a" content) lines))))
      (error (condition) (warn "sgme: the profile was not fetched: ~a" condition)))
    (let ((profile (when lines
                     (with-output-to-string (out)
                       (format out "# User profile (SGME)~%")
                       (dolist (line (nreverse lines)) (format out "~a~%" line))))))
      (cond ((and role profile) (format nil "~a~%~a" role profile))
            (role role)
            (profile profile)
            (t nil)))))

(defun role-block ()
  "The server's active role prompt, as the original asks for it, or NIL."
  (handler-case
      (let* ((active (call :get "/v1/admin/care/active-role" :key (admin-key)))
             (role-id (nlk:json-value active :text "role_id")))
        (when (and role-id (plusp (length role-id)))
          (let* ((assembled (call :get (format nil "/v1/admin/roles/~a/assemble"
                                                (quri:url-encode role-id :encoding :utf-8))
                                  :key (admin-key)))
                 (prompt (nlk:json-value assembled :text "system_prompt"))
                 (name (or (nlk:json-value assembled :text "role_name") role-id)))
            (when (and prompt (plusp (length prompt)))
              (format nil "# Communication role: ~a (SGME's current role)~%~a~%~
                           (style only: memory and facts stay the memory pool's.)"
                      name prompt)))))
    (error () nil)))

(defun refresh-profile ()
  (setf *profile* (profile-block))
  *profile*)

(defun observe-round (op next)
  "Record the profile block once per turn, in that turn's own request."
  (when (and *sgme* *profile* (getf op :turn) (setting :inject)
             (not (equal (getf op :turn) *injected-turn*)))
    (handler-case
        (progn
          (nlk:record-turn-context-prompt
           (getf op :turn)
           :text (format nil "<system-reminder>~%~a~%</system-reminder>" *profile*))
          (setf *injected-turn* (getf op :turn)))
      (error (condition) (warn "sgme: the profile was not recorded: ~a" condition))))
  (funcall next op))

;;; --- the manual and /sgme -------------------------------------------------------

(defparameter +primer+
  "SGME is a self-hosted memory engine: it distils each session into tagged memories and finds them
again by meaning, with the sources they came from. (help :sgme) is this page. The tools:
  sgme_memory_search {query, limit?, scopes?}   the memories (and skills) nearest a question
  sgme_conversation_search {query}              the raw words of a past session
  sgme_answer {query, question_type?}           one synthesised answer across sessions
  sgme_inject {mode?}                           the profile block, on demand
  sgme_wiki_search {query}, sgme_wiki_pages {category?}   the wiki knowledge base
  sgme_skill_search {query}, sgme_skill_get {name}, sgme_skill_list   the skills library
  sgme_signal_pull, sgme_signal_claim {event_id}, sgme_signal_ack {event_id, status?}   care signals
  sgme_health, sgme_stats                       whether the server answers, what it holds
Search before answering anything that may depend on an earlier session; when nothing is found, say
so rather than guessing. Every finished turn is appended to the server, which refines it in the
background, so what is said today is recalled tomorrow."
  "What (help :sgme) answers while the cell runs.")

(defun status-line ()
  (running-settings)
  (format nil "SGME at ~a, user ~s, capture ~:[off~;on~], refinement ~:[off~;on~].~%~
               /sgme QUERY searches the memory pool; the sgme_* tools do the rest."
          (root) (setting :agent-id) (setting :capture) (setting :refine-on-end)))

(defun run-command (args session-id)
  "One /sgme invocation: a query searches, no argument shows the server."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (answering (format-results query (sgme-search query +default-scopes+ (setting :search-limit))))
        (status-line))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell sgme
  (:section ("sgme")
    (:guide "name the SGME server: base_url, http://127.0.0.1:9910 when it runs here; a server elsewhere needs an agent key (SGME_AGENT_KEY, or agent_key here), the admin key unlocks stats and refinement; agent_id is this agent's tag on every append; next step: /sgme shows the server")
    ("base_url" :string :default "http://127.0.0.1:9910"
     :doc "the SGME server's address")
    ("agent_key" :secret
     :doc "the agent key every read and write carries")
    ("agent_key_env" :env :default "SGME_AGENT_KEY"
     :doc "the environment variable holding the agent key, when it is not written here")
    ("admin_key" :secret
     :doc "the admin key: stats, refinement, roles and care signals")
    ("admin_key_env" :env :default "SGME_ADMIN_KEY"
     :doc "the environment variable holding the admin key")
    ("agent_id" :string :default "nodecode"
     :doc "the provenance tag every appended turn and every pulled signal carries")
    ("inject" :boolean :default t
     :doc "put the server's profile block into each turn's request")
    ("inject_mode" :choice :options '("daily" "coding" "work" "full") :default "daily"
     :doc "which profile template the block asks for")
    ("inject_max_tokens" :integer :default 800 :min 1
     :doc "the profile block's token budget")
    ("search_limit" :integer :default 5 :min 1
     :doc "how many results one search asks for unless the call says otherwise")
    ("capture" :boolean :default t
     :doc "append each finished turn to SGME")
    ("refine_on_end" :boolean :default t
     :doc "ask the server to refine what was appended, after each finished turn")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one request's ceiling"))
  (:help :sgme "sgme: recall, skills, wiki and care signals from a self-hosted SGME memory engine" +primer+)
  (:command "sgme" 'run-command
            :description "Show the SGME server, or search the memory pool"
            :argument-hint "QUERY")
  (:tool "sgme_memory_search"
   (tool-schema
    "Search the SGME memory pool -- tagged long-term memories of earlier sessions -- and, by default, the skills layer too. Use it before answering anything that may depend on what happened before: the user's facts, preferences, projects, decisions. Results carry their sources. When nothing matches, say the memory pool has nothing rather than guessing."
    '(("query" "string" "The keywords or the question to search for." t)
      ("limit" "integer" "How many results (default: the section's search_limit, at most 20)." nil)
      ("scopes" "array" "The layers to search: memory, skills, wiki, wiki_pages, sessions (default: memory and skills)." nil)))
   'tool-memory-search)
  (:tool "sgme_conversation_search"
   (tool-schema
    "Search the raw session layer: the words of earlier conversations, matched rather than distilled. Use it to find what was actually said on some occasion, where sgme_memory_search answers the fact distilled from it."
    '(("query" "string" "The keywords to find in past sessions." t)
      ("limit" "integer" "How many results (default 5, at most 20)." nil)))
   'tool-conversation-search)
  (:tool "sgme_answer"
   (tool-schema
    "Ask SGME an aggregate question -- how often something was mentioned, when something changed, which projects did a thing -- and get one synthesised answer, at the cost of one server-side model call. Use plain sgme_memory_search for a lookup."
    '(("query" "string" "The question, in one sentence." t)
      ("question_type" "string" "temporal, aggregate or generic." nil)
      ("limit" "integer" "How many candidates the server reads (at most 20)." nil)))
   'tool-answer)
  (:tool "sgme_inject"
   (tool-schema
    "Fetch the profile SGME would inject for a scene: daily, coding, work or full. The cell already puts this block into each turn's request when inject is on; call this to read it, or to see another mode."
    '(("mode" "string" "daily, coding, work or full (default: the section's inject_mode)." nil)))
   'tool-inject)
  (:tool "sgme_wiki_search"
   (tool-schema
    "Search the SGME wiki: the knowledge pages the engine keeps, including skill manuals."
    '(("query" "string" "What to look for." t)
      ("limit" "integer" "How many pages (default 10, at most 50)." nil)))
   'tool-wiki-search)
  (:tool "sgme_wiki_pages"
   (tool-schema
    "List the SGME wiki's pages, newest first, optionally only one category's."
    '(("category" "string" "Only pages of this category." nil)
      ("limit" "integer" "How many pages (default 50, at most 200)." nil)
      ("offset" "integer" "How many to skip, for the next page." nil)))
   'tool-wiki-pages)
  (:tool "sgme_skill_search"
   (tool-schema
    "Search the skills library by what a skill is for; the hits carry the skill's name and trigger, not its text. Take the full text with sgme_skill_get before using one."
    '(("query" "string" "The task or the trigger to search for." t)
      ("limit" "integer" "How many skills (default 5, at most 20)." nil)))
   'tool-skill-search)
  (:tool "sgme_skill_get"
   (tool-schema
    "Read one skill's full text, or one section of it."
    '(("name" "string" "The skill's name, from sgme_skill_search." t)
      ("section" "string" "Only this section of the skill." nil)))
   'tool-skill-get)
  (:tool "sgme_skill_list"
   (tool-schema
    "List the skills library's index, a page at a time."
    '(("limit" "integer" "How many skills (default 50, at most 500)." nil)
      ("offset" "integer" "How many to skip." nil)))
   'tool-skill-list)
  (:tool "sgme_signal_pull"
   (tool-schema
    "Pull the care signals waiting for this agent -- a due item, an overwork warning -- without consuming them. Claim one with sgme_signal_claim, care for the user, then report with sgme_signal_ack."
    '(("limit" "integer" "How many signals (default 20, at most 20)." nil)))
   'tool-signal-pull)
  (:tool "sgme_signal_claim"
   (tool-schema
    "Claim one care signal so no other agent acts on it. A signal someone else already claimed answers as such and is skipped, not retried."
    '(("event_id" "string" "The signal's id, from sgme_signal_pull." t)))
   'tool-signal-claim)
  (:tool "sgme_signal_ack"
   (tool-schema
    "Report what came of a claimed care signal, for the record the engine keeps."
    '(("event_id" "string" "The signal's id." t)
      ("status" "string" "claimed, acked or failed (default acked)." nil)
      ("result" "string" "One line on what was done." nil)))
   'tool-signal-ack)
  (:tool "sgme_health"
   (tool-schema "Ask the SGME server whether it answers, and what it says of itself." '())
   'tool-health)
  (:tool "sgme_stats"
   (tool-schema "What the SGME server holds: memory, session and refinement counts. Needs the admin key." '())
   'tool-stats)
  (:hook :frame (nlk:observer #'observe-frame "nodecode-sgme"))
  (:hook :round #'observe-round)
  (:start (lambda ()
            (check-config)
            (setf *turns* (make-hash-table :test #'equal)
                  *queued* '()
                  *profile* nil
                  *injected-turn* nil
                  *started-at* (iso-now))
            (setf *profile* (profile-block))
            (setf *worker* (nlk:worker-start "sgme-capture" #'capture-step :wake 600))
            (nle:on-stop (lambda ()
                           (nlk:worker-stop *worker*)
                           (setf *worker* nil
                                 *queued* '()
                                 *profile* nil
                                 *injected-turn* nil
                                 *turns* (make-hash-table :test #'equal)))))))
