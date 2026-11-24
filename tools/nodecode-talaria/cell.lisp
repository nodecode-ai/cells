;;;; cell.lisp --- the talaria-webui plugin's HTTP routes, as four tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :talaria) answers the primer below, and every request's help
;;;; section carries one line naming it. Nothing hooks the provider path: the
;;;; plugin answers ordinary HTTP, and NLK:HTTP is the one seam.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "talaria": {"url": "http://127.0.0.1:8080", "key_env": "HERMES_API_KEY"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from liamsmith86's Hermes Agent plugin (see NOTICE): the routes it
;;;; registers on the API server under /talaria/v1 and the shapes they answer
;;;; with. The plugin's own half of the pair -- the route handlers, the
;;;; rewind, compress, branch and live-prompt machinery, the observations
;;;; store inside Hermes -- stays in the server; this cell is its client.

(in-package #:nodecode-talaria)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Talaria WebUI sits on a Hermes Agent API server: the talaria-webui plugin adds /talaria/v1
routes to it, and this cell reads them. Four tools, which a person can also call from eval in
the talaria package.
  talaria_capabilities {full}
      What the server's plugin supports: its version, whether session changes (rewind,
      compress, branch) and history search are on, how many native commands it exposes. Call
      it first when another tool answers that the plugin is not enabled for the profile.
  talaria_search_history {query, offset}
      Search the message history of every session the API key can see. Answers pages of
      twenty matches: session id, session title, the message's role and a snippet. Use the
      session id it returns with the two tools below.
  talaria_reply_details {session_id, message_id}
      One reply's own record: finish reason, model, token counts (input, output, reasoning,
      cache reads), duration, and whether reasoning was involved. Answers that nothing was
      recorded for replies whose turn ended before the server observed them.
  talaria_context_usage {session_id}
      How full a session's context window is: the input tokens last sent, the model's
      maximum, and when that was observed. Nothing a session never used.
Every tool is a GET: nothing here writes to the server, so a search cannot disturb a session
a person is working in. The key is the API server's own; the cell stores none of its own."
  "What (help :talaria) answers while the cell runs.")

(defparameter *prefix* "/talaria/v1"
  "Where the plugin mounts its routes on the API server.")

(defparameter +session-id-chars+ "-_."
  "Characters a session id may carry beside letters and digits.")

(defparameter +max-session-id+ 128
  "The longest session id this cell puts in a path.")

(defparameter +max-query+ 200
  "The longest search the server accepts, in characters.")

;;; --- one exchange ---------------------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, or NIL when it is not a string or is empty."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun answer-detail (body)
  "The server's own words for a failed call, clipped, or NIL."
  (let ((text (nlk:trimmed (or body ""))))
    (and (plusp (length text)) (nlk:clip text 300))))

(defun url-encode (text)
  "TEXT as one query-string value: everything but the unreserved characters
percent-encoded, so a space or an ampersand cannot leave its slot."
  (with-output-to-string (out)
    (loop for character across text
          for code = (char-code character)
          do (if (or (alphanumericp character) (find character "-_.~"))
                 (write-char character out)
                 (format out "%~2,'0X" code)))))

(defun read-settings (values table)
  "The declared VALUES, plus the API key the section's variable holds: read once, here."
  (declare (ignore table))
  (let* ((raw (or (getf values :url) ""))
         (url (string-right-trim "/" (nlk:trimmed (if (stringp raw) raw ""))))
         (env (nonblank (getf values :key-env)))
         (plist (copy-list values)))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (nlk:config-error "url ~s has no http:// or https:// scheme: set it to the base URL of the Hermes API server, e.g. \"http://127.0.0.1:8080\"" raw))
    (unless env
      (nlk:config-error "no key: set key_env to the environment variable holding the Hermes API server key"))
    (setf (getf plist :url) url
          (getf plist :key)
          (or (nonblank (nle::credential-env env))
              (nlk:config-error "key_env names ~a, which is unset or empty" env)))
    plist))

(defun exchange (method path &key query)
  "One exchange with the API server => (values BODY STATUS). A dial that fails
is a refusal the model can read."
  (let ((url (concatenate 'string (setting :url) *prefix* path
                           (if (nonblank query) (concatenate 'string "?" query) ""))))
    (handler-case
        (multiple-value-bind (body status)
            (nlk:http method url
                      :headers (list (cons "authorization"
                                           (format nil "Bearer ~a" (setting :key)))
                                     (cons "accept" "application/json"))
                      :timeout (setting :timeout)
                      :connect-timeout (setting :timeout))
          (values (if (stringp body) body (nlk:body-text body)) status))
      (error (condition)
        (fail "~a ~a did not answer: ~a" method url
              (nlk:one-line (princ-to-string condition)))))))

(defun api-call (method path &key query)
  "PATH's decoded JSON answer, or a refusal carrying the server's own words.
The plugin answers 404 when it is not enabled for the profile."
  (multiple-value-bind (body status) (exchange method path :query query)
    (if (and (integerp status) (< status 400))
        (or (ignore-errors (nlk:decode-json body))
            (fail "~a ~a answered no JSON~@[: ~a~]" method path (answer-detail body)))
        (fail "~a ~a answered HTTP ~a~@[: ~a~]" method path status (answer-detail body)))))

(defun session-id (request value)
  "VALUE as a session id this cell will put in a path, or a refusal naming
what to pass. The ids the server mints are letters, digits, dash and underscore."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (unless (and text (plusp (length text)) (<= (length text) +max-session-id+)
                 (every (lambda (character)
                          (or (alphanumericp character)
                              (find character +session-id-chars+)))
                        text))
      (fail "~a takes the session id a session is stored as: letters, digits, dash, underscore (got ~s)"
            request value))
    text))

(defun message-id (request value)
  "VALUE as a stored message id, or a refusal naming what to pass."
  (unless (and (integerp value) (plusp value) (< value (expt 2 62)))
    (fail "~a takes the message id of a stored message: a positive integer (got ~s)"
          request value))
  value)

;;; --- talaria_capabilities ---------------------------------------------------------

(defun capabilities (request)
  (let ((body (api-call :get "/capabilities"))
        (full (server-info-full request)))
    (when full
      (return-from capabilities (nlk:encode-json-object body)))
    (nlk:encode-json-object
     (nlk:json-object
      "plugin_version" (or (nlk:json-value body :string "plugin_version") "")
      "revision" (or (nlk:json-value body :string "revision") "")
      "rewind" (eq t (nlk:json-value body :boolean "rewind"))
      "history_search" (eq t (nlk:json-value body :boolean "history_search"))
      "commands" (length (or (nlk:json-array body "commands") #()))
      "agent_name" (or (nlk:json-value body :string "agent" "name") "")))))

;;; --- talaria_search_history --------------------------------------------------------

(defun search-history (request)
  (let* ((query (nlk:trimmed (history-search-query request)))
         (offset (or (history-search-offset request) 0)))
    (when (zerop (length query))
      (fail "talaria_search_history needs at least one character to look for"))
    (when (> (length query) +max-query+)
      (fail "talaria_search_history takes at most ~a characters (this one has ~a)"
            +max-query+ (length query)))
    (when (or (minusp offset) (> offset 10000))
      (fail "offset must be between 0 and 10000 (got ~a)" offset))
    (let* ((body (api-call :get "/search"
                            :query (format nil "q=~a&offset=~a" (url-encode query) offset)))
           (rows (or (nlk:json-array body "data") #()))
           (results '()))
      (loop for row across rows
            do (push (nlk:json-object
                      "session_id" (or (nlk:json-value row :string "session_id") "")
                      "title" (or (nlk:json-value row :string "title") "Untitled session")
                      "role" (or (nlk:json-value row :string "role") "")
                      "snippet" (or (nlk:json-value row :string "snippet") ""))
                     results))
      (nlk:encode-json-object
       (nlk:json-object
        "query" query
        "count" (length results)
        "has_more" (eq t (nlk:json-value body :boolean "has_more"))
        "next_offset" (or (nlk:json-value body :integer "next_offset") offset)
        "results" (coerce (reverse results) 'vector))))))

;;; --- talaria_reply_details ----------------------------------------------------------

(defun reply-details (request)
  (let* ((sid (session-id "talaria_reply_details" (reply-details-session-id request)))
         (mid (message-id "talaria_reply_details" (reply-details-message-id request)))
         (body (api-call :get (format nil "/sessions/~a/response" sid)
                         :query (format nil "message_id=~d" mid)))
         (usage (nlk:json-value body :object "usage")))
    (nlk:encode-json-object
     (nlk:json-object
      "session_id" sid
      "message_id" (or (nlk:json-value body :integer "message_id") mid)
      "timestamp" (or (nlk:json-value body :number "timestamp")
                      (nlk:json-value body :integer "timestamp"))
      "finish_reason" (or (nlk:json-value body :string "finish_reason") "")
      "model" (or (nlk:json-value body :string "model") "")
      "provider" (or (nlk:json-value body :string "provider") "")
      "has_reasoning" (eq t (nlk:json-value body :boolean "has_reasoning"))
      "input_tokens" (nlk:json-value usage :integer "input_tokens")
      "output_tokens" (nlk:json-value usage :integer "output_tokens")
      "observed" (if (nlk:json-value body :string "status")
                     (nlk:json-value body :string "status")
                     "unobserved")))))

;;; --- talaria_context_usage ----------------------------------------------------------

(defun context-usage (request)
  (let* ((sid (session-id "talaria_context_usage" (context-usage-session-id request)))
         (body (api-call :get (format nil "/sessions/~a/context" sid)))
         (context (nlk:json-value body :object "context")))
    (nlk:encode-json-object
     (if context
         (nlk:json-object
          "session_id" sid
          "used" (nlk:json-value context :integer "used")
          "maximum" (nlk:json-value context :integer "maximum")
          "model" (or (nlk:json-value context :string "model") "")
          "observed_at" (or (nlk:json-value context :number "observed_at")
                            (nlk:json-value context :integer "observed_at")))
         (nlk:json-object
          "session_id" sid
          "observed" nil
          "note" "no reply in this session has been observed yet: the server records its context use when a turn finishes")))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell talaria
  (:section ("talaria")
    (:guide "the base URL of a Hermes Agent API server running the talaria-webui plugin, and the environment variable holding that server's API key; next step: ask what the server supports")
    ("url" :string :required t
     :doc "the API server's base URL, http:// or https:// included")
    ("key_env" :env :required t
     :doc "the environment variable holding the API server key")
    ("timeout" :integer :default 15 :min 1
     :doc "how long one request to the server may take, in seconds"))
  (:settings #'read-settings)
  (:help :talaria
         "talaria: search Hermes session history, read a reply's details and a session's context use through the talaria-webui plugin"
         *manual*)
  (:tool "talaria_capabilities" (nlk:contract 'server-info) #'capabilities)
  (:tool "talaria_search_history" (nlk:contract 'history-search) #'search-history)
  (:tool "talaria_reply_details" (nlk:contract 'reply-details) #'reply-details)
  (:tool "talaria_context_usage" (nlk:contract 'context-usage) #'context-usage))
