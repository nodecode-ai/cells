;;;; cell.lisp --- Pushary: the operator's phone, behind six tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :pushary) answers the primer below, and every request's help
;;;; section carries one line naming it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "pushary": {"api_key_env": "PUSHARY_API_KEY"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from Pushary's Hermes Agent plugin (see NOTICE): its six tools,
;;;; their arguments, and the MCP endpoint they speak — one JSON-RPC
;;;; tools/call to https://pushary.com/api/mcp/mcp with the account's bearer
;;;; key, asked once and polled once for an answer.

(in-package #:nodecode-pushary)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Pushary reaches the operator's phone from the nodecode-pushary cell: a notification is
something they should know now and are not watching for, and a question is one they answer
on the phone and this call waits for.
  pushary_notify      {title, body, context_type, summary, details, files_changed,
                        error_message, next_steps}
      body is the notification, plain text, at most 500 characters and best under 200: it is
      read on a lock screen, so lead with the fact. context_type (success, error, info) with
      the fields beside it adds a detail page behind it.
  pushary_ask          {question, type, options, placeholder, context, wait, timeout_ms,
                        tool_name, tool_target, intent, action, blocker, action_body}
      asks on the phone and, by default, waits: type is confirm (yes/no), select (one of
      options, 2 to 6) or input (free text). The answer is JSON — answered, value,
      correlationId. wait false answers a correlationId at once; pushary_wait polls once for
      that answer. For a dangerous step, fill tool_name, action and blocker so the phone
      shows what is being asked, and why.
  pushary_wait         {correlation_id, timeout_ms}   one poll for a question already asked.
  pushary_cancel       {correlation_id}               a question that stopped being relevant.
  pushary_remind       {body, title, in_minutes, at, cancel_reminder_id, reminder_id}
      a reminder saved on Pushary, which reaches the phone after this session ends; no
      arguments lists the pending ones.
  pushary_propose_scope {done_when, allowed_paths, off_limits_paths, promises, timeout_ms}
      agrees the boundary of a run once, on the phone, instead of asking per file.
Use a notification for what the person should know now, a question for a decision this turn
needs, and never one per turn: the answer itself is what the transcript carries.
/ pushary status says what the tools would use; /pushary test sends one notification."
  "What (help :pushary) answers while the cell runs.")

(defparameter +default-base-url+ "https://pushary.com"
  "Pushary's own address; the section may name another.")

(defparameter +mcp-path+ "/api/mcp/mcp"
  "The one endpoint these tools speak: JSON-RPC tools/call, the account's bearer key.")

(defparameter +max-wait-ms+ 55000
  "The server's own ceiling on one blocking wait.")

(defparameter +default-wait-ms+ 30000
  "How long a call waits when the caller did not say.")

(defparameter +max-body-chars+ 500
  "Pushary's own ceiling on a notification body.")

;;; --- answers, and the one condition ---------------------------------------------

(defun network-failure (condition)
  "Pushary did not answer at all: the words, as the call's failure."
  (nle:failure (format nil "ERROR: Pushary did not answer: ~a"
                        (nlk:one-line (princ-to-string condition)))))

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (pushary-error (condition)
       (nle:failure (format nil "ERROR: ~a" (pushary-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Pushary: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun started-p ()
  "Whether the section is read and the cell's verbs may run."
  (and *pushary* t))

(defun clip-text (text limit)
  (if (and (stringp text) (> (length text) limit))
      (concatenate 'string (subseq text 0 (- limit 3)) "...")
      (or text "")))

;;; --- JSON, both ways ----------------------------------------------------------

(defun empty-value-p (value)
  (or (null value)
      (and (stringp value) (zerop (length (nlk:trimmed value))))
      (and (vectorp value) (zerop (length value)))))

(defun object-of (pairs)
  "PAIRS — string NAME and VALUE, alternating — as a JSON object with the
empty members left out."
  (apply #'nlk:make-json-object
         (loop for (name value) on pairs by #'cddr
               unless (empty-value-p value)
                 append (list name value))))

(defun context-of (call)
  "The detail page pushary_notify offers, or NIL when context_type is empty."
  (let ((type (nonblank (notice-context-type call))))
    (and type
         (object-of (list "type" type
                          "summary" (notice-summary call)
                          "details" (notice-details call)
                          "filesChanged" (notice-files-changed call)
                          "errorMessage" (notice-error-message call)
                          "nextSteps" (notice-next-steps call))))))

(defun identity-of ()
  "The identity members every call carries when the section names them."
  (list "sessionId" (nonblank (setting :session-id))
        "machineId" (nonblank (setting :machine-id))
        "agentName" (nonblank (setting :agent-name))))

(defun object-pairs (object)
  "OBJECT's members as a flat (NAME VALUE ...) list, for merging one answer
into another."
  (when (hash-table-p object)
    (loop for name being the hash-keys of object using (hash-value value)
          append (list name value))))

(defun json-text (object)
  (nlk:encode-json-object object))

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the key the section names: read once, here."
  (declare (ignore table))
  (let* ((env (or (nonblank (getf values :api-key-env)) "PUSHARY_API_KEY"))
         (base (or (nonblank (getf values :base-url)) +default-base-url+))
         (plist (copy-list values)))
    (setf (getf plist :api-key-env) env
          (getf plist :base-url) (string-right-trim "/" base)
          (getf plist :api-key)
          (or (nonblank (nle::credential-env env))
              (nlk:config-error
               "~a is unset or empty: save your Pushary API key in it (hermes and nodecode both read the environment), or name another variable in api_key_env; a key comes from pushary.com"
               env))
          (getf plist :agent-name) (or (nonblank (getf values :agent-name)) "Nodecode")
          (getf plist :session-id) (or (nonblank (getf values :session-id)) "")
          (getf plist :machine-id) (or (nonblank (getf values :machine-id)) "")
          (getf plist :timeout) (or (getf values :timeout) 20))
    plist))

;;; --- the one HTTPS exchange -----------------------------------------------------

(defun endpoint ()
  (format nil "~a~a" (setting :base-url) +mcp-path+))

(defun parse-sse (text)
  "The last data: payload an event-stream TEXT carries, or NIL."
  (let ((last nil))
    (dolist (line (uiop:split-string text :separator '(#\Newline)))
      (let ((line (string-right-trim '(#\Return) line)))
        (when (and (> (length line) 5)
                   (string= "data:" line :end1 5 :end2 (min 5 (length line))))
          (let ((data (string-trim '(#\Space #\Tab) (subseq line 5))))
            (when (plusp (length data))
              (setf last data))))))
    last))

(defun decode-answer (text)
  "Pushary's answer as a JSON object: a plain body, or the last event of a
stream, since a blocking question may come back as text/event-stream."
  (let* ((trimmed (and (stringp text) (nlk:trimmed text)))
         (payload (cond ((null trimmed) nil)
                        ((search "data:" trimmed) (parse-sse trimmed))
                        (t trimmed))))
    (when (and payload (plusp (length payload)))
      (ignore-errors (nlk:decode-json payload)))))

(defun answer-error (object)
  "Pushary's own words for a call it refused, or NIL for one it took."
  (when (hash-table-p object)
    (let ((error (nlk:json-value object :any "error")))
      (when error
        (if (hash-table-p error)
            (or (nlk:json-value error :string "message")
                (json-text error))
            (princ-to-string error))))))

(defun answer-payload (object)
  "The JSON text a result carries in its first content block."
  (let* ((result (nlk:json-value object :any "result"))
         (content (and (hash-table-p result)
                       (nlk:json-value result :array "content"))))
    (when (and content (plusp (length content)))
      (nlk:json-value (aref content 0) :string "text"))))

(defun blocking-timeout (timeout-ms)
  "How long an NLK:HTTP exchange may take for a call that waits TIMEOUT-MS,
with a little room over the server's own cap."
  (+ 2 (ceiling (min (max (or timeout-ms +default-wait-ms+) 1000) +max-wait-ms+) 1000)))

(defun mcp-call (tool arguments &key (timeout (setting :timeout)))
  "One MCP tools/call of TOOL with the JSON object ARGUMENTS => (values
OBJECT NIL), or (values NIL WHY-IT-FAILED) in Pushary's own words."
  (handler-case
      (multiple-value-bind (text status)
          (nlk:http :post (endpoint)
                    :headers (list (cons "authorization" (format nil "Bearer ~a" (setting :api-key)))
                                   (cons "accept" "application/json, text/event-stream")
                                   (cons "content-type" "application/json"))
                    :content (json-text
                              (nlk:json-object
                               "jsonrpc" "2.0" "id" 1 "method" "tools/call"
                               "params" (nlk:json-object "name" tool "arguments" arguments)))
                    :timeout timeout
                    :connect-timeout (min timeout (setting :timeout)))
        (declare (ignore status))
        (let* ((body (if (stringp text) text (nlk:body-text text)))
               (object (decode-answer body)))
          (cond
            ((null object)
             (values nil "it answered with nothing this cell could read"))
            ((answer-error object)
             (values nil (answer-error object)))
            (t
             (let ((payload (answer-payload object)))
               (if (or (null payload) (zerop (length payload)))
                   (values (nlk:json-object "ok" t) nil)
                   (let ((answer (ignore-errors (nlk:decode-json payload))))
                     (if (hash-table-p answer)
                         (values answer nil)
                         (values (nlk:json-object "raw" payload) nil)))))))))
    (error (condition) (network-failure condition))))

(defun call-tool (tool pairs &key timeout)
  "MCP-call TOOL with PAIRS plus the identity members => (values OBJECT NIL)
or (values NIL WHY)."
  (apply #'mcp-call tool (object-of (append pairs (identity-of)))
         (and timeout (list :timeout timeout))))

;;; --- pushary_notify --------------------------------------------------------------

(defun send-notification (call)
  "The pushary_notify tool: one push, from the model."
  (answering
    (let ((body (nlk:trimmed (or (notice-body call) "")))
          (title (or (nonblank (notice-title call)) "")))
      (unless (plusp (length body))
        (return-from send-notification
          (refuse "the body is empty: say the one fact the person should read")))
      (multiple-value-bind (object error)
          (call-tool "send_notification"
                     (list "title" title
                           "body" (clip-text body +max-body-chars+)
                           "context" (context-of call)))
        (if error
            (nle:failure (format nil "ERROR: Pushary did not send it: ~a" error))
            (json-text object))))))

(defun notify (body &rest pairs)
  "The tool's own body, from eval: (pushary:notify \"the deploy is live\" :title \"Deploy\").
=> the answer the model would read."
  (send-notification
   (decode-notice
    (apply #'nlk:make-json-object
           "body" body
           (loop for (key value) on pairs by #'cddr
                 append (list (string-downcase (substitute #\_ #\- (symbol-name key))) value))))))

;;; --- pushary_ask, pushary_wait, pushary_cancel -----------------------------------

(defun decision-pairs (call)
  "The approval members of an ask, when the model filled them."
  (list "toolName" (ask-tool-name call)
        "toolTarget" (ask-tool-target call)
        "intent" (ask-intent call)
        "action" (ask-action call)
        "blocker" (ask-blocker call)
        "actionBody" (ask-action-body call)))

(defun next-action (object)
  "Which step Pushary says comes next, if it says."
  (and (hash-table-p object)
       (or (nlk:json-value object :string "handoffAction")
           (nlk:json-value object :string "nextAction"))))

(defun merge-answer (object correlation question type)
  "OBJECT's members with the question's own identity in front of them."
  (apply #'nlk:make-json-object
         (append (list "correlationId" correlation "question" question "type" type)
                 (object-pairs object))))

(defun ask-user (call)
  "The pushary_ask tool: ask on the phone, and wait as the caller asked."
  (answering
    (let* ((question (nlk:trimmed (or (ask-question call) "")))
           (type (or (nonblank (ask-kind call)) "confirm"))
           (timeout (min (max (or (ask-timeout-ms call) +default-wait-ms+) 1000) +max-wait-ms+))
           (wait (ask-wait call)))
      (unless (plusp (length question))
        (return-from ask-user (refuse "the question is empty: say what needs deciding")))
      (multiple-value-bind (object error)
          (call-tool "ask_user"
                     (append (list "question" question
                                   "type" type
                                   "options" (ask-options call)
                                   "placeholder" (ask-placeholder call)
                                   "context" (ask-context call)
                                   "wait" (if wait t :false)
                                   "timeoutMs" timeout)
                             (decision-pairs call))
                     :timeout (and wait (blocking-timeout timeout)))
        (cond
          (error (nle:failure (format nil "ERROR: Pushary did not ask: ~a" error)))
          (t
           (let ((correlation (nlk:json-value object :string "correlationId"))
                 (answered (nlk:json-value object :boolean "answered"))
                 (next (next-action object)))
             (if (and wait (not answered) correlation
                      (or (null next) (string= next "wait_for_answer")))
                 (multiple-value-bind (polled perror)
                     (call-tool "wait_for_answer"
                                (list "correlationId" correlation "timeoutMs" timeout)
                                :timeout (blocking-timeout timeout))
                   (if perror
                       (nle:failure (format nil "ERROR: Pushary asked, the wait failed: ~a" perror))
                       (json-text (if (hash-table-p polled)
                                      (merge-answer polled correlation question type)
                                      object))))
                 (json-text object)))))))))

(defun wait-call (call)
  "The pushary_wait tool: one poll for a question already asked."
  (answering
    (let ((correlation (nonblank (poll-correlation-id call)))
          (timeout (min (max (or (poll-timeout-ms call) +default-wait-ms+) 1000) +max-wait-ms+)))
      (unless correlation
        (return-from wait-call (refuse "correlation_id is empty: pass the one pushary_ask returned")))
      (multiple-value-bind (object error)
          (call-tool "wait_for_answer"
                     (list "correlationId" correlation "timeoutMs" timeout)
                     :timeout (blocking-timeout timeout))
        (if error
            (nle:failure (format nil "ERROR: Pushary did not answer: ~a" error))
            (json-text object))))))

(defun cancel-call (call)
  "The pushary_cancel tool: one question stops waiting."
  (answering
    (let ((correlation (nonblank (cancel-correlation-id call))))
      (unless correlation
        (return-from cancel-call (refuse "correlation_id is empty: pass the one pushary_ask returned")))
      (multiple-value-bind (object error)
          (call-tool "cancel_question" (list "correlationId" correlation))
        (if error
            (nle:failure (format nil "ERROR: Pushary did not cancel it: ~a" error))
            (json-text object))))))

;;; --- pushary_remind ---------------------------------------------------------------

(defun request-id ()
  "One operation id, so a retried reminder never schedules twice."
  (format nil "nodecode-~(~36r~)-~(~36r~)"
          (get-universal-time)
          (random (expt 36 8))))

(defun remind-call (call)
  "The pushary_remind tool: schedule, list or cancel a reminder on Pushary."
  (answering
    (let* ((body (nonblank (reminder-body call)))
           (title (nonblank (reminder-title call)))
           (at (nonblank (reminder-at call)))
           (in-minutes (reminder-in-minutes call))
           (cancel (nonblank (reminder-cancel-reminder-id call)))
           (lookup (nonblank (reminder-reminder-id call)))
           (scheduling (and body (not cancel) (not lookup))))
      (multiple-value-bind (object error)
          (call-tool "schedule_reminder"
                     (list "body" body
                           "title" title
                           "inMinutes" in-minutes
                           "at" at
                           "cancelReminderId" cancel
                           "reminderId" lookup
                           "requestId" (and scheduling (request-id))))
        (if error
            (nle:failure (format nil "ERROR: Pushary did not take the reminder: ~a" error))
            (json-text object))))))

;;; --- pushary_propose_scope ----------------------------------------------------------

(defun propose-scope-call (call)
  "The pushary_propose_scope tool: the run's boundary, agreed once on the phone."
  (answering
    (let ((done-when (nlk:trimmed (or (scope-done-when call) "")))
          (timeout (min (max (or (scope-timeout-ms call) 50000) 1000) +max-wait-ms+)))
      (unless (plusp (length done-when))
        (return-from propose-scope-call (refuse "done_when is empty: say what finished means for this run")))
      (multiple-value-bind (object error)
          (call-tool "propose_scope"
                     (list "doneWhen" done-when
                           "allowedPaths" (scope-allowed-paths call)
                           "offLimitsPaths" (scope-off-limits-paths call)
                           "promises" (scope-promises call)
                           "timeoutMs" timeout)
                     :timeout (blocking-timeout timeout))
        (if error
            (nle:failure (format nil "ERROR: Pushary did not take the scope: ~a" error))
            (json-text object))))))

;;; --- the same verbs, from eval -------------------------------------------------------

(defun wire-pairs (pairs)
  "PAIRS — a keyword and a value, alternating — as the wire's own names."
  (loop for (key value) on pairs by #'cddr
        append (list (string-downcase
                      (substitute #\_ #\- (string-left-trim ":" (symbol-name key))))
                     value)))

(defun ask (question &rest pairs)
  "From eval: (pushary:ask \"ship it?\" :context \"deploy\") => the answer the model reads."
  (ask-user (decode-ask (apply #'nlk:make-json-object "question" question (wire-pairs pairs)))))

(defun wait-for-answer (correlation-id &key (timeout-ms 30000))
  "From eval: (pushary:wait-for-answer \"c-1\") => one poll's answer."
  (wait-call (decode-poll (nlk:make-json-object "correlation_id" correlation-id
                                                 "timeout_ms" timeout-ms))))

(defun cancel (correlation-id)
  "From eval: (pushary:cancel \"c-1\")."
  (cancel-call (decode-cancel (nlk:make-json-object "correlation_id" correlation-id))))

(defun remind (body &rest pairs)
  "From eval: (pushary:remind \"water the plants\" :in-minutes 30)."
  (remind-call (decode-reminder (apply #'nlk:make-json-object "body" body (wire-pairs pairs)))))

(defun propose-scope (done-when &rest pairs)
  "From eval: (pushary:propose-scope \"the tests pass\" :allowed-paths (vector \"src/**\"))."
  (propose-scope-call (decode-scope (apply #'nlk:make-json-object "done_when" done-when
                                            (wire-pairs pairs)))))

;;; --- what the operator reads --------------------------------------------------------

(defun status ()
  "What /pushary answers: what the tools would use, in words."
  (if (not (started-p))
      (format nil "Pushary is not running: turn the nodecode-pushary cell on in /cells, ~
or (restart-cells \"nodecode-pushary\").")
      (format nil "~{~a~^~%~}"
              (list "Pushary (nodecode-pushary):"
                    (format nil "  key        ~a (set)" (setting :api-key-env))
                    (format nil "  endpoint   ~a" (endpoint))
                    (format nil "  agent name ~a" (setting :agent-name))
                    (format nil "  machine id ~a" (if (plusp (length (setting :machine-id)))
                                                       (setting :machine-id)
                                                       "the server's own"))
                    (format nil "  wait       up to ~a ms per call" +max-wait-ms+)
                    "  The model reaches six tools — pushary_notify, pushary_ask, pushary_wait,"
                    "  pushary_cancel, pushary_remind, pushary_propose_scope — and /pushary test"
                    "  sends one notification now."))))

(defun test-push ()
  "Send one test notification, and answer what Pushary said."
  (handler-case
      (multiple-value-bind (object error)
          (call-tool "send_notification"
                     (list "title" "Nodecode"
                           "body" "A test notification from Nodecode. Nothing is wrong: this is what the operator's phone shows."))
        (if error
            (format nil "Pushary refused the test notification: ~a" error)
            (format nil "A test notification went (request ~a). Check the phone."
                    (or (nlk:json-value object :string "requestId")
                        (nlk:json-value object :string "id")
                        "sent"))))
    (pushary-error (condition)
      (format nil "No notification went: ~a" (pushary-error-detail condition)))
    (error (condition)
      (format nil "No notification went: ~a" (nlk:one-line (princ-to-string condition))))))

(defun status-dialog ()
  "The rows the bare /pushary offers under its answer."
  (nle:list-dialog
   :title "Pushary"
   :summary "Push notifications, questions and scope approvals on the operator's phone."
   :rows (list (nle:ask-row "Send a test notification" "Nothing to fill in" "/pushary test"
                             :verb "send"))))

(defun run-command (args session-id)
  "The /pushary command: status by default, one test notification on `test'."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((or (string= verb "") (string= verb "status"))
           (values (status) (and (started-p) (status-dialog))))
          ((string= verb "test")
           (values (if (started-p) (test-push) (status)) (status-dialog)))
          (t (values "Usage: /pushary [status|test]. status says what the tools would use; test sends one notification now."
                      (and (started-p) (status-dialog)))))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell pushary
  (:section ("pushary")
    (:guide "make an account at pushary.com and copy its API key into a variable in the shell that starts Nodecode (PUSHARY_API_KEY); the phone app pairs with the same account; next step: /pushary test")
    ("api_key_env" :env :default "PUSHARY_API_KEY"
     :doc "the environment variable holding your Pushary API key; the key itself stays in the environment, never in the config")
    ("base_url" :string :default "https://pushary.com"
     :doc "Pushary's address; only a self-hosted copy changes it")
    ("agent_name" :string :default "Nodecode"
     :doc "the name the phone shows for this machine's agent")
    ("session_id" :string :default ""
     :doc "the session these calls are attributed to; empty, Pushary groups them on its own")
    ("machine_id" :string :default ""
     :doc "the id Pushary counts this machine under, shared with the operator's other agents; empty lets the server choose")
    ("timeout" :integer :default 20 :min 1
     :doc "how long one non-blocking Pushary request may take, in seconds"))
  (:settings #'read-settings)
  (:help :pushary
         "pushary: pushary_notify pushes to the operator's phone, pushary_ask waits for their answer"
         *manual*)
  (:tool "pushary_notify" (nlk:contract 'notice) #'send-notification)
  (:tool "pushary_ask" (nlk:contract 'ask) #'ask-user)
  (:tool "pushary_wait" (nlk:contract 'poll) #'wait-call)
  (:tool "pushary_cancel" (nlk:contract 'cancel) #'cancel-call)
  (:tool "pushary_remind" (nlk:contract 'reminder) #'remind-call)
  (:tool "pushary_propose_scope" (nlk:contract 'scope) #'propose-scope-call)
  (:command "pushary" 'run-command
            :description "Pushary: what the tools would use, or a test notification to the phone"
            :argument-hint "status | test"))
