;;;; cell.lisp --- one Hermes Agent API server, as six tools and a command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Hermes Agent profile whose API server is switched on answers /v1/runs,
;;;; /v1/runs/{id}, /v1/runs/{id}/stop, /v1/models, /v1/health and
;;;; /v1/chat/completions. That server is the thing this cell talks to, over
;;;; NLK:HTTP; nothing here reads the Hermes installation, its files or its
;;;; config. The one profile the section names is the one every tool reaches,
;;;; where the original plugin configured many and named one per call.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "hermes": {"name": "reviewer", "url": "http://hermes.local:8651",
;;;;              "api_key_env": "HERMES_API_KEY", "model": "reviewer-fast"}
;;;; A vetoed section ("enabled": false) installs nothing; an empty url
;;;; refuses at start, because no verb works without one.
;;;;
;;;; Ported from bennybuoy's Hermes Herald (see NOTICE): the dispatch, check,
;;;; collect, status, cancel, chat, ping and model-listing tools, their
;;;; schemas, the wire each one uses, the run-id rules and the audit record.

(in-package #:nodecode-hermes-herald)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "A Hermes Agent profile whose API server is on is reachable through the nodecode-hermes-herald
cell: six tools, which a person can also call from eval in the hermes-herald package.
  hermes_dispatch {message, instructions, model}
      Start a task on that profile and answer its run id at once. The run is its own session
      there, with its own model and tools, and it keeps going after this call returns. The brief
      does not inherit this conversation: write the goal, paths, constraints, output format and
      how to verify in message. A model you name must be an alias hermes_models lists.
  hermes_check {run_ids}
      One run's status, or several at once: running, completed, failed, cancelled or
      waiting_for_approval, with the final summary once there is one. The target is the
      authority; a run it forgot answers not_found. poll it rather than assume.
  hermes_cancel {run_id}
      Ask the target to stop a run. Cancellation is cooperative: it finishes the step it is on.
  hermes_models {query}
      The model routes the profile advertises for dispatch, each alias with the model and the
      provider it is pinned to. The list is the target's own, not its whole provider catalog.
  hermes_chat {message, instructions, model, new_session}
      Talk to the profile synchronously and answer its reply in this turn, for a conversation
      whose later turns depend on earlier ones: the target keeps the session, so a second call
      continues it unless new_session is true. This blocks the turn until the target answers.
  hermes_runs {limit}
      The runs dispatched from this machine, newest first, as this side recorded them. Follow a
      record with hermes_check: the target decides what a run is doing now.
A target's own summary is a self-report: read back a file, a URL or a commit before reporting
the work as done. The section's url is the profile's API-server address, and api_key_env names
the environment variable holding its bearer token."
  "What (help :hermes-herald) answers while the cell runs.")

;;; --- answers, and the one condition ---------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (hermes-herald-error (condition)
       (nle:failure (format nil "ERROR: ~a" (hermes-herald-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Hermes Herald: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  "TEXT trimmed when it has anything in it, else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun clip (text limit)
  "TEXT's first LIMIT characters, marked when cut."
  (let ((text (or text "")))
    (if (<= (length text) limit) text (concatenate 'string (subseq text 0 limit) " ..."))))

(defun stringify (value)
  "VALUE as text: a string as it is, anything else as its JSON."
  (cond ((stringp value) value)
        ((null value) "")
        (t (or (ignore-errors (nlk:encode-json-object value)) ""))))

(defun run-id-ok-p (run-id)
  "Whether RUN-ID can stand in a request path: letters, digits and . _ - only."
  (and (stringp run-id)
       (plusp (length run-id))
       (<= (length run-id) 128)
       (every (lambda (character)
                (or (alphanumericp character) (find character "-._")))
              run-id)))

(defun run-id (value)
  "VALUE as a run id, or a refusal."
  (let ((id (nonblank value)))
    (unless (and id (run-id-ok-p id))
      (fail "~s is not a run id: letters, digits, . _ and - only" value))
    id))

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the bearer token the section names: read once, here."
  (declare (ignore table))
  (let* ((url (string-right-trim "/" (or (getf values :url) "")))
         (env (nonblank (getf values :api-key-env)))
         (plist (copy-list values)))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (nlk:config-error "url ~s is not the target's base URL: set it to the full http:// or https:// address of the profile's API server, e.g. \"http://hermes.local:8651\"" url))
    (setf (getf plist :url) url
          (getf plist :name) (or (nonblank (getf values :name)) "hermes")
          (getf plist :model) (or (nonblank (getf values :model)) "")
          (getf plist :api-key)
          (and env (or (nonblank (nle::credential-env env))
                       (nlk:config-error "api_key_env names ~a, which is unset or empty" env))))
    plist))

;;; --- one exchange ---------------------------------------------------------------

(defun request-headers (&key json session)
  "The headers every request carries: JSON in, JSON out, the bearer token when
the section names one, and the target session for a chat."
  (append (list (cons "accept" "application/json"))
          (when json (list (cons "content-type" "application/json")))
          (when session (list (cons "x-hermes-session-id" session)))
          (let ((token (setting :api-key)))
            (when token (list (cons "authorization" (format nil "Bearer ~a" token)))))))

(defun exchange (method path &key payload (timeout (setting :timeout-seconds)) session)
  "One exchange with the profile => (values BODY STATUS HEADERS), BODY as text.
A dial that fails is a refusal naming the address."
  (let ((url (concatenate 'string (setting :url) path)))
    (handler-case
        (multiple-value-bind (body status headers)
            (nlk:http method url
                      :headers (request-headers :json (and payload t) :session session)
                      :content (and payload (nlk:encode-json-object payload))
                      :timeout timeout)
          (values (if (stringp body) body (nlk:body-text body)) status headers))
      (error (condition)
        (fail "~a ~a did not answer: ~a" method url
              (nlk:one-line (princ-to-string condition)))))))

(defun decoded (body)
  "BODY as a decoded value, or its text when it is not JSON."
  (let ((text (nlk:trimmed (or body ""))))
    (when (plusp (length text))
      (or (ignore-errors (nlk:decode-json text)) text))))

(defun api-request (method path &key payload (timeout (setting :timeout-seconds)) session)
  "PATH's decoded answer => (values VALUE STATUS HEADERS); a failed status is
a refusal carrying the target's own words."
  (multiple-value-bind (body status headers) (exchange method path :payload payload
                                                             :timeout timeout :session session)
    (if (and (integerp status) (< status 400))
        (values (decoded body) status headers)
        (fail "~a ~a answered HTTP ~a~@[: ~a~]" method path status (clip body 300)))))

;;; --- the runs this side remembers ------------------------------------------------

(defparameter *ledger-limit* 100
  "How many dispatched runs the local record keeps, newest first.")

(defun ledger-path ()
  (nlk:home "hermes-herald/runs.json"))

(defun load-runs ()
  "The recorded runs as a vector of objects, newest first."
  (let ((path (ledger-path)))
    (if (probe-file path)
        (nlk:json-array (ignore-errors (nlk:decode-json (uiop:read-file-string path))) "runs")
        #())))

(defun remember-run (id status &key summary model message)
  "Record ID's STATUS in the local ledger, replacing an earlier record for the
same run; answers the record."
  (let* ((runs (load-runs))
         (old (find id runs :test #'equal
                    :key (lambda (row) (nlk:json-value row :string "run_id"))))
         (entry (nlk:json-object "run_id" id
                                  "target" (setting :name)
                                  "status" status
                                  "model" (or model (and old (nlk:json-value old :string "model")) "")
                                  "created" (or (and old (nlk:json-value old :string "created")) (now-iso))
                                  "updated" (now-iso)
                                  "message" (clip (or message (and old (nlk:json-value old :string "message")) "") 200)
                                  "summary" (clip (or summary (and old (nlk:json-value old :string "summary")) "") 2000)))
         (kept (cons entry (loop for row across runs
                                 unless (equal id (nlk:json-value row :string "run_id"))
                                   collect row))))
    (when (> (length kept) *ledger-limit*)
      (setf kept (subseq kept 0 *ledger-limit*)))
    (let ((path (ledger-path)))
      (ensure-directories-exist path)
      (ignore-errors
        (nlk:write-file-atomically path
                                    (nlk:encode-json-object (nlk:json-object "runs" (coerce kept 'vector)))
                                    :mode #o600)))
    entry))

(defun now-iso ()
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

;;; --- the saved target session ----------------------------------------------------

(defun session-path ()
  (nlk:home "hermes-herald/session.txt"))

(defun saved-session ()
  (let ((path (session-path)))
    (and (probe-file path) (nonblank (uiop:read-file-string path)))))

(defun save-session (id)
  (when (nonblank id)
    (let ((path (session-path)))
      (ensure-directories-exist path)
      (ignore-errors (nlk:write-file-atomically path (nlk:trimmed id) :mode #o600)))))

;;; --- hermes_dispatch --------------------------------------------------------------

(defun dispatch (request)
  "hermes_dispatch: POST /v1/runs."
  (let* ((message (dispatch-request-message request))
         (instructions (nonblank (dispatch-request-instructions request)))
         (model (or (nonblank (dispatch-request-model request)) (nonblank (setting :model))))
         (payload (nlk:json-object "input" message
                                    :opt "instructions" instructions
                                    :opt "model" model)))
    (multiple-value-bind (answer status) (api-request :post "/v1/runs" :payload payload)
      (declare (ignore status))
      (let ((id (nlk:json-value answer :string "run_id")))
        (unless (and id (run-id-ok-p id))
          (fail "the target answered with no run id: ~a" (clip (stringify answer) 300)))
        (remember-run id (or (nlk:json-value answer :string "status") "submitted")
                      :model (or model "") :message message)
        (nlk:encode-json-object
         (nlk:json-object "target" (setting :name)
                          "run_id" id
                          "model" (if model model "target default")
                          "status" (or (nlk:json-value answer :string "status") "submitted")
                          "hint" "the run continues on the target: read it with hermes_check, stop it with hermes_cancel"))))))

;;; --- hermes_check -----------------------------------------------------------------

(defun run-row (id answer)
  "One run's answer as the model reads it."
  (let ((summary (or (nlk:json-value answer :string "summary")
                     (nlk:json-value answer :string "output"))))
    (nlk:json-object "run_id" id
                     "status" (or (nlk:json-value answer :string "status") "unknown")
                     "model" (or (nlk:json-value answer :string "model") "")
                     "duration_seconds" (or (nlk:json-value answer :number "duration_seconds") 0)
                     "summary" (clip (stringify summary) 2000))))

(defun check (request)
  "hermes_check: GET /v1/runs/{id} for each id, in one answer."
  (let ((ids (coerce (run-check-run-ids request) 'list)))
    (unless ids (fail "give at least one run id"))
    (when (> (length ids) 20) (fail "at most 20 run ids in one call"))
    (let ((rows '()))
      (dolist (id ids)
        (run-id id)
        (multiple-value-bind (answer status) (api-request :get (format nil "/v1/runs/~a" id) :timeout 20)
          (declare (ignore status))
          (let ((row (run-row id answer)))
            (remember-run id (nlk:json-value row :string "status")
                          :summary (nlk:json-value row :string "summary"))
            (push row rows))))
      (nlk:encode-json-object (nlk:json-object "target" (setting :name)
                                                "runs" (coerce (nreverse rows) 'vector))))))

;;; --- hermes_cancel -----------------------------------------------------------------

(defun cancel (request)
  "hermes_cancel: POST /v1/runs/{id}/stop."
  (let ((id (run-id (run-cancel-run-id request))))
    (multiple-value-bind (answer status) (api-request :post (format nil "/v1/runs/~a/stop" id)
                                                       :payload (nlk:json-object) :timeout 20)
      (declare (ignore status))
      (remember-run id "cancelling")
      (nlk:encode-json-object
       (nlk:json-object "target" (setting :name)
                         "run_id" id
                         "status" "cancelling"
                         "answer" (clip (stringify answer) 400)
                         "hint" "cancellation is cooperative: the target finishes its current step, so poll hermes_check")))))

;;; --- hermes_models -----------------------------------------------------------------

(defun models (request)
  "hermes_models: GET /v1/models, the profile's dispatchable route aliases."
  (let* ((answer (api-request :get "/v1/models" :timeout 20))
         (entries (nlk:json-array answer "data"))
         (query (string-downcase (or (nonblank (model-list-query request)) "")))
         (primary "")
         (aliases '()))
    (loop for entry across entries
          for id = (nlk:json-value entry :string "id")
          do (when id
               (if (nlk:json-value entry :string "parent")
                   (push (nlk:json-object "alias" id
                                           "model" (or (nlk:json-value entry :string "root") id)
                                           "provider" (or (nlk:json-value entry :string "provider")
                                                          (nlk:json-value entry :string "owned_by")
                                                          ""))
                         aliases)
                   (when (zerop (length primary)) (setf primary id)))))
    (let ((rows (nreverse aliases)))
      (when (plusp (length query))
        (setf rows (remove-if-not (lambda (row)
                                     (search query (string-downcase (nlk:json-value row :string "alias"))))
                                   rows)))
      (nlk:encode-json-object
       (nlk:json-object "target" (setting :name)
                         "primary" primary
                         "aliases" (coerce rows 'vector)
                         "count" (length rows)
                         "hint" (if rows
                                    "pass an alias as model to hermes_dispatch or hermes_chat; omit it for the target default"
                                    "no dispatchable alias: the target declares model_routes under its own api_server settings"))))))

;;; --- hermes_chat -------------------------------------------------------------------

(defun reply-text (answer)
  "The assistant text of an OpenAI-compatible answer."
  (let* ((choices (nlk:json-array answer "choices"))
         (first (and (plusp (length choices)) (aref choices 0))))
    (or (nlk:json-value first :string "message" "content")
        (nlk:json-value first :string "text")
        (nlk:json-value answer :string "reply")
        (nlk:json-value answer :string "output")
        (stringify answer))))

(defun chat (request)
  "hermes_chat: POST /v1/chat/completions, one turn of a target-held session."
  (let* ((message (chat-request-message request))
         (instructions (nonblank (chat-request-instructions request)))
         (model (or (nonblank (chat-request-model request)) (nonblank (setting :model)) "hermes-agent"))
         (session (unless (chat-request-new-session request) (saved-session)))
         (messages (if instructions
                       (vector (nlk:json-object "role" "system" "content" instructions)
                               (nlk:json-object "role" "user" "content" message))
                       (vector (nlk:json-object "role" "user" "content" message))))
         (payload (nlk:json-object "model" model "messages" messages "stream" nil)))
    (multiple-value-bind (answer status headers)
        (api-request :post "/v1/chat/completions" :payload payload
                                                   :session session
                                                   :timeout (setting :timeout-seconds))
      (declare (ignore status))
      (let ((returned (or (and (listp headers)
                                (cdr (assoc "x-hermes-session-id" headers :test #'string-equal)))
                          (nlk:json-value answer :string "session_id"))))
        (save-session returned)
        (remember-run (or returned "") "chat" :model model :message message
                      :summary (clip (reply-text answer) 500))
        (nlk:encode-json-object
         (nlk:json-object "target" (setting :name)
                           "model" model
                           "session" (or returned session "")
                           "reply" (reply-text answer)))))))

;;; --- hermes_runs --------------------------------------------------------------------

(defun runs (request)
  "hermes_runs: the local record of dispatched runs."
  (let* ((limit (max 1 (run-ledger-max-runs request)))
         (recorded (load-runs))
         (shown (subseq recorded 0 (min (length recorded) limit))))
    (nlk:encode-json-object (nlk:json-object "target" (setting :name)
                                              "count" (length recorded)
                                              "runs" (coerce (coerce shown 'list) 'vector)))))

;;; --- the target, in one line (the /hermes command) --------------------------------

(defun health-line ()
  "The target's reachability in a few words."
  (let ((start (get-internal-real-time)))
    (handler-case
        (multiple-value-bind (body status) (nlk:http :get (concatenate 'string (setting :url) "/v1/health")
                                                      :headers (request-headers)
                                                      :timeout 5 :connect-timeout 5)
          (declare (ignore body))
          (let ((milliseconds (round (- (get-internal-real-time) start)
                                     (/ internal-time-units-per-second 1000))))
            (cond ((and (integerp status) (= status 200)) (format nil "up (~ams)" milliseconds))
                  ((member status '(401 403)) (format nil "up, key refused (HTTP ~a)" status))
                  (t (format nil "answered HTTP ~a" status)))))
      (error (condition)
        (format nil "down: ~a" (nlk:one-line (princ-to-string condition)))))))

(defun report (args session-id)
  "The /hermes answer: the target, its reachability and either its recent runs
or its model routes."
  (declare (ignore session-id))
  (answering
   (let ((what (string-downcase (nlk:trimmed (or args "")))))
    (cond
      ((member what '("" "status") :test #'string=)
       (let ((recorded (load-runs)))
         (format nil "~a at ~a: ~a; ~d model route~:p advertised; ~d run~:p dispatched from here~a. /hermes runs, /hermes models."
                 (setting :name) (setting :url) (health-line)
                 (or (ignore-errors (length (model-aliases))) 0) (length recorded)
                 (if (plusp (length recorded))
                     (format nil " (newest ~a, ~a)"
                             (nlk:json-value (aref recorded 0) :string "run_id")
                             (nlk:json-value (aref recorded 0) :string "status"))
                     ""))))
      ((string= what "runs")
       (let ((recorded (load-runs)))
         (if (zerop (length recorded))
             (format nil "no run dispatched from here yet; hermes_dispatch starts one on ~a" (setting :name))
             (with-output-to-string (out)
               (format out "~d run~:p dispatched from here, newest first:~%" (length recorded))
               (loop for row across recorded
                     for count from 1 to 20
                     do (format out "~2d. ~a  ~a  ~a  ~@[~a~]~%"
                                count
                                (or (nlk:json-value row :string "run_id") "?")
                                (or (nlk:json-value row :string "status") "?")
                                (or (nlk:json-value row :string "model") "-")
                                (nlk:json-value row :string "message")))))))
      ((string= what "models")
       (let ((answer (api-request :get "/v1/models" :timeout 20)))
         (let ((aliases (nlk:json-array answer "data")))
           (format nil "~d model route~:p on ~a: ~{~a~^, ~}"
                   (length aliases) (setting :name)
                   (loop for entry across aliases
                         for id = (nlk:json-value entry :string "id")
                         when id collect id)))))
      (t (format nil "usage: /hermes (status), /hermes runs, /hermes models"))))))

(defun model-aliases ()
  (nlk:json-array (api-request :get "/v1/models" :timeout 20) "data"))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell hermes-herald
  (:section ("hermes")
    (:guide "the API-server address of one Hermes Agent profile, scheme included, and the environment variable holding its bearer token; model is the optional route alias to ask for by default; next step: ask for /hermes status")
    ("name" :string :default "hermes"
     :doc "what to call this target in answers, e.g. the profile's name")
    ("url" :string
     :doc "the profile's API-server base URL, http:// or https:// included, e.g. http://hermes.local:8651")
    ("api_key_env" :env
     :doc "the environment variable holding the profile API server's bearer token; unset, requests carry none, which a loopback server may accept")
    ("model" :string
     :doc "the model route alias to send when a call names none; unset, the target's own default answers")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "how long one request may take, in seconds; a chat turn can be long"))
  (:settings #'read-settings)
  (:help :hermes-herald
         "hermes-herald: hermes_dispatch, hermes_check, hermes_cancel, hermes_models, hermes_chat and hermes_runs reach one Hermes Agent API server"
         *manual*)
  (:tool "hermes_dispatch" (nlk:contract 'dispatch-request) #'dispatch)
  (:tool "hermes_check" (nlk:contract 'run-check) #'check)
  (:tool "hermes_cancel" (nlk:contract 'run-cancel) #'cancel)
  (:tool "hermes_models" (nlk:contract 'model-list) #'models)
  (:tool "hermes_chat" (nlk:contract 'chat-request) #'chat)
  (:tool "hermes_runs" (nlk:contract 'run-ledger) #'runs)
  (:command "hermes" 'report
            :description "The Hermes target: status, runs, models"))
