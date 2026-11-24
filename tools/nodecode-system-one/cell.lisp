;;;; cell.lisp --- Local System One's three gates, as verbs, two tools and a command.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of integrations/hermes/local-system-one-hermes in
;;;; NMTZ-z/system-one-benchmark-lab (see NOTICE). The original hooks a
;;;; Hermes turn: before the provider call it asks a local sidecar whether the
;;;; task needs the public Web (Search Gate) and whether it is a fast or a
;;;; strong tier (Model Tier Gate), and after the turn whether the event is
;;;; silent, digest or notify_now (Notification Gate). Nodecode's one tool is
;;;; eval, so the port is the same three gates as Lisp verbs, the two that
;;;; answer before work is done also as model tools, and /system-one for the
;;;; operator.
;;;;
;;;; What it does NOT do is rewrite a request. The original's Canary mode, once
;;;; acknowledged, hides the web tools on the first provider call of a turn
;;;; (audited hard no-Web rules) and can lower reasoning effort for
;;;; gemini-3.8-flash-tiered (audited hard-fast rules). Nodecode has no
;;;; provider-request seam a cell can edit, so a rule the original would act on
;;;; is said here as advice on the decision's own line, under `canary:'.
;;;;
;;;; Every gate fails open, as the original does: a sidecar that is not
;;;; running, is slow, or answers nothing readable is `undecided', never an
;;;; error the turn has to recover from.

(in-package #:nodecode-system-one)

;;; --- the section, read back ---------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, when it is a non-blank string; else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun bounded (text limit)
  "TEXT trimmed, when it is a non-blank string, cut to LIMIT characters."
  (let ((text (nonblank text)))
    (and text (subseq text 0 (min (length text) limit)))))

(defun read-settings (values table)
  "The declared VALUES, plus the bearer token they authenticate with, read
once, here. The token is auth_token written inline, else the variable
auth_token_env names; the original sent none, and nor does this one unless
one of the two is set. The service_url must carry a scheme: the original
clamped a bad one to loopback silently, which hid the typo."
  (declare (ignore table))
  (let* ((env (nonblank (getf values :auth-token-env)))
         (inline (nonblank (getf values :auth-token)))
         (token (or inline (and env (nonblank (uiop:getenv env)))))
         (root (nonblank (getf values :service-url)))
         (plist (copy-list values)))
    (when (and env (null token))
      (nlk:config-error "auth_token_env names ~a, but nothing is set there: put the sidecar's bearer token in it, or clear auth_token_env when the sidecar was started without one" env))
    (unless (and root
                 (or (uiop:string-prefix-p "http://" root)
                     (uiop:string-prefix-p "https://" root)))
      (nlk:config-error "service_url ~s has no http:// or https:// scheme: set it to where the Local System One sidecar answers, as in http://127.0.0.1:8787" (getf values :service-url)))
    (setf (getf plist :auth-token) token
          (getf plist :service-url) (string-right-trim "/" root))
    plist))

;;; --- one request -------------------------------------------------------------

(defun timeout-seconds ()
  "The section's timeout_ms in seconds, clamped to 50 ms .. 5 s as the original clamps it."
  (max 0.05 (min (/ (setting :timeout-ms) 1000.0) 5.0)))

(defun endpoint (path)
  "PATH on the sidecar the section points at."
  (concatenate 'string (setting :service-url) path))

(defun headers ()
  "JSON in and out, and the bearer token when the section carries one."
  (let ((headers (list (cons "content-type" "application/json")
                       (cons "accept" "application/json"))))
    (when (setting :auth-token)
      (push (cons "authorization" (format nil "Bearer ~a" (setting :auth-token))) headers))
    headers))

(defun detail-of (answer)
  "The sidecar's own words for a failed call, clipped."
  (let ((text (and (stringp answer) (bounded answer 300))))
    (when text
      (let ((object (ignore-errors (nlk:decode-json answer))))
        (when (hash-table-p object)
          (let ((message (or (nlk:json-value object :string "error")
                             (nlk:json-value object :string "detail"))))
            (when message (setf text (bounded message 300))))))
      text)))

(defun call-gate (path payload)
  "POST PAYLOAD to PATH on the sidecar => (values OBJECT ERROR-TEXT). OBJECT
is the decoded JSON object; ERROR-TEXT says why there is none -- a sidecar
that is down, slow or answers nothing readable is not an error to raise."
  (let ((url (endpoint path))
        (timeout (timeout-seconds)))
    (handler-case
        (multiple-value-bind (answer status)
            (nlk:http :post url
                      :headers (headers)
                      :content (nlk:encode-json-object payload)
                      :timeout timeout
                      :connect-timeout (min timeout 1.0))
          (if (and (integerp status) (< status 300))
              (let ((object (ignore-errors (nlk:decode-json answer))))
                (if (hash-table-p object)
                    (values object nil)
                    (values nil (format nil "answered no JSON object on ~a" path))))
              (values nil (format nil "answered HTTP ~a on ~a~@[: ~a~]"
                                  status path (detail-of answer)))))
      (error (condition)
        (values nil (nlk:one-line (princ-to-string condition)))))))

;;; --- what a gate answered ----------------------------------------------------

(defun read-decision (gate object)
  "OBJECT, one gate's answer, as the decision plist this file reads."
  (let* ((field (case gate (:search "decision") (:tier "tier") (t "delivery")))
         (label (nlk:json-value object :string field)))
    (if (null label)
        (list :gate gate :ok nil :error (format nil "its answer carried no \"~a\" field" field))
        (list :gate gate
              :ok t
              :label label
              :source (nlk:json-value object :string "decision_source")
              :reason (nlk:json-value object :string "reason")
              :backend (nlk:json-value object :string "backend")
              :score-name (case gate (:search "p_web") (:tier "p_strong") (t "priority"))
              :score (nlk:json-value object :number
                                      (case gate (:search "probability_search")
                                                 (:tier "probability_strong")
                                                 (t "priority_score")))
              :notify-now (and (eq gate :notify)
                                (nlk:json-value object :boolean "notify_now"))))))

(defun gate-payload (task context)
  "The body the original sends both pre-turn gates: the bounded task, and the
bounded recent context when there is any."
  (nlk:json-object "task" task
                    :opt "context" context))

(defun notify-payload (event context urgency action blocking routine deadline)
  "The body the original sends its Notification Gate, field for field."
  (nlk:json-object "event" event
                    :opt "context" context
                    "urgency" urgency
                    "user_action_required" (and action t)
                    "blocking_failure" (and blocking t)
                    "routine_update" (and routine t)
                    :opt "deadline_minutes" deadline))

(defun ask (gate payload)
  "Ask GATE with PAYLOAD => its decision, or the undecided one saying why."
  (multiple-value-bind (object error)
      (call-gate (case gate (:search +search-path+) (:tier +tier-path+) (t +notify-path+)) payload)
    (if object
        (read-decision gate object)
        (list :gate gate :ok nil :error (or error "no answer")))))

;;; --- what the model reads ----------------------------------------------------

(defun gate-name (gate)
  (case gate (:search "search") (:tier "model tier") (t "notification")))

(defun gate-meaning (decision)
  "What DECISION's label means for the caller, in words."
  (case (getf decision :gate)
    (:search (if (equal (getf decision :label) "search")
                 "this task needs the public web"
                 "no public web needed"))
    (:tier (if (equal (getf decision :label) "fast")
               "a fast, cheap tier is enough"
               "the strong tier is worth it"))
    (t (cond ((equal (getf decision :label) "notify_now") "interrupt the user now")
             ((equal (getf decision :label) "digest") "add it to a digest, do not interrupt")
             ((equal (getf decision :label) "silent") "stay silent")
             (t "no delivery advice")))))

(defun canary-advice (decision)
  "What the original's Canary mode would have done with DECISION, when the
rule that decided it is one of the audited ones it acts on."
  (when (and (setting :canary-advice)
             (getf decision :ok)
             (equal (getf decision :source) "rule"))
    (case (getf decision :gate)
      (:search (when (and (equal (getf decision :label) "no_search")
                          (member (getf decision :reason) +no-web-reasons+ :test #'equal))
                 "an audited hard no-Web rule -- in acknowledged Canary mode the original hides the web tools on the turn's first provider call"))
      (:tier (when (and (equal (getf decision :label) "fast")
                        (member (getf decision :reason) +fast-reasons+ :test #'equal))
               "an audited hard-fast rule -- with its reasoning downgrade on and a gemini-3.8-flash-tiered request, the original lowers reasoning effort for one call"))
      (t nil))))

(defun decision-line (decision)
  "DECISION as the one line the model reads."
  (let ((name (gate-name (getf decision :gate))))
    (if (not (getf decision :ok))
        (format nil "~a: undecided - ~a; fail-open, the caller's own choice stands"
                name (getf decision :error))
        (with-output-to-string (out)
          (format out "~a: ~a (~a" name (getf decision :label)
                  (or (getf decision :source) "no source"))
          (when (getf decision :reason)
            (format out ", ~a" (getf decision :reason)))
          (when (getf decision :score)
            (format out "; ~a ~,3f" (getf decision :score-name) (getf decision :score)))
          (format out ") - ~a" (gate-meaning decision))
          (let ((advice (canary-advice decision)))
            (when advice
              (format out "~%  canary: ~a" advice)))))))

;;; --- the verbs ----------------------------------------------------------------

(define-verb route (task &key context)
  "Ask the sidecar's Search Gate and Model Tier Gate about TASK, the pair the
original asks before every turn. CONTEXT is bounded recent conversation.
=> the lines the model reads, and the decisions as a plist."
  (let* ((task (or (bounded task +max-task-chars+) ""))
         (context (bounded context +max-context-chars+))
         (search (and (setting :search-gate) (ask :search (gate-payload task context))))
         (tier (and (setting :model-tier-gate) (ask :tier (gate-payload task context)))))
    (values (if (or search tier)
                (format nil "~{~a~^~%~}"
                        (loop for decision in (list search tier)
                              when decision collect (decision-line decision)))
                "search and model tier: both gates are off in the system-one section; nothing was asked")
            (list :search search :tier tier))))

(define-verb search-gate (task &key context)
  "Ask the sidecar's Search Gate whether TASK needs the public Web. => its
line, and the decision it read."
  (let ((decision (ask :search (gate-payload (or (bounded task +max-task-chars+) "")
                                             (bounded context +max-context-chars+)))))
    (values (decision-line decision) decision)))

(define-verb tier-gate (task &key context)
  "Ask the sidecar's Model Tier Gate whether TASK wants the fast or the strong
tier. => its line, and the decision it read."
  (let ((decision (ask :tier (gate-payload (or (bounded task +max-task-chars+) "")
                                           (bounded context +max-context-chars+)))))
    (values (decision-line decision) decision)))

(define-verb notify-gate (event &key context urgency user-action-required blocking-failure
                                   routine-update deadline-minutes)
  "Ask the sidecar's Notification Gate how urgent EVENT is: silent, digest or
notify_now. => its line, and the decision it read."
  (unless (setting :notification-gate)
    (return-from notify-gate
      (values "notification: the gate is off in the system-one section; nothing was asked" nil)))
  (let ((decision (ask :notify
                       (notify-payload (or (bounded event +max-event-chars+) "")
                                        (bounded context +max-context-chars+)
                                        (or (nonblank urgency) "auto")
                                        user-action-required
                                        blocking-failure
                                        routine-update
                                        deadline-minutes))))
    (values (decision-line decision) decision)))

(defun switch (on)
  (if on "on" "off"))

(defun health-summary (answer)
  "ANSWER, /health's body, in one clause."
  (let ((object (ignore-errors (nlk:decode-json answer))))
    (if (hash-table-p object)
        (let ((service (nlk:json-value object :string "service"))
              (ane (nlk:json-value object :object "ane")))
          (format nil "answers~@[ (service ~a)~]~@[, neural engine ~a~]"
                  service
                  (and ane (if (nlk:json-value ane :boolean "healthy") "healthy" "unhealthy"))))
        (or (bounded answer 120) "answers"))))

(defun health-line ()
  "The sidecar's /health, or what went wrong asking for it."
  (handler-case
      (multiple-value-bind (answer status)
          (nlk:http :get (endpoint "/health")
                    :headers (headers)
                    :timeout (min 1.0 (timeout-seconds))
                    :connect-timeout 1.0)
        (if (and (integerp status) (< status 300))
            (format nil "health: ~a" (health-summary answer))
            (format nil "health: answered HTTP ~a" status)))
    (error (condition)
      (format nil "health: no answer at ~a (~a)"
              (endpoint "/health") (nlk:one-line (princ-to-string condition))))))

(define-verb health ()
  "Ask the sidecar's /health: is it running, and which backend answers. =>
one line."
  (health-line))

(define-verb status-line ()
  "Where the section points, what is on, and whether the sidecar answers. =>
one block of lines."
  (format nil "Local System One at ~a (a gate times out at ~ams): search ~a, model tier ~a, notification ~a~%~a"
          (setting :service-url) (setting :timeout-ms)
          (switch (setting :search-gate))
          (switch (setting :model-tier-gate))
          (switch (setting :notification-gate))
          (health-line)))

;;; --- the two boundaries -------------------------------------------------------

(nlk:define-contract system-one-route
  (:doc "Ask a local System One sidecar how to route a task before you work on it: does it need the public Web, and is it a fast or a strong-tier task. The sidecar is a small local classifier next to Nodecode; the answer is advice, and a sidecar that is not running answers undecided.")
  ("task" :text :required t
          :doc "the task in the user's words, at most 4000 characters")
  ("context" :text
             :doc "recent conversation, for references such as 'this one'; optional, at most 2000 characters"))

(nlk:define-contract system-one-notify
  (:doc "Ask a local System One sidecar how much attention one finished event deserves: stay silent, add it to a digest, or interrupt the user now. Use it before a channel reply or a notification decision.")
  ("event" :text :required t
           :doc "the event or final answer to judge, at most 6000 characters")
  ("urgency" :text :default "auto" :options '("auto" "low" "medium" "high" "critical")
             :doc "how urgent it is, when you know; auto lets the gate decide")
  ("user_action_required" :boolean :default nil
                           :doc "true when the user must do something")
  ("blocking_failure" :boolean :default nil
                       :doc "true when work stopped on a failure")
  ("routine_update" :boolean :default nil
                     :doc "true when it is a routine progress update")
  ("deadline_minutes" :integer
                       :doc "minutes left before a deadline, when there is one"))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (system-one-error (condition)
       (nle:failure (format nil "ERROR: ~a" (system-one-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Local System One: ~a"
                            (nlk:one-line (princ-to-string condition)))))))

(defun route-tool (call)
  "The system_one_route tool: both pre-turn gates about CALL's task."
  (answering
    (route (system-one-route-task call)
           :context (system-one-route-context call))))

(defun notify-tool (call)
  "The system_one_notify tool: CALL's event through the Notification Gate."
  (answering
    (notify-gate (system-one-notify-event call)
                 :urgency (system-one-notify-urgency call)
                 :user-action-required (system-one-notify-user-action-required call)
                 :blocking-failure (system-one-notify-blocking-failure call)
                 :routine-update (system-one-notify-routine-update call)
                 :deadline-minutes (system-one-notify-deadline-minutes call))))

;;; --- /system-one ----------------------------------------------------------------

(defun run-command (args session-id)
  "/system-one TASK: what the gates say about TASK. With no argument, where
the cell points and whether the sidecar answers."
  (declare (ignore session-id))
  (let ((task (nlk:trimmed (or args ""))))
    (if (plusp (length task))
        (route task)
        (status-line))))

;;; --- the declaration -------------------------------------------------------------

(defparameter +primer+
  "A local System One sidecar (a small classifier the operator runs beside Nodecode) answers three
routing questions. Call it before work that a wrong choice would waste, not on every turn.
  (system-one:route \"task\" &key context)   needs public web? which tier? - both gates, one call
  (system-one:tier-gate \"task\" ...)        fast or strong, one gate
  (system-one:notify-gate \"event\" ...)     silent, digest or notify_now
  (system-one:health)                        is the sidecar up
Or call the tools system_one_route {task, context?} and system_one_notify {event, urgency?, ...}.
Answers are one line each: label (source, reason; score) - what it means, and under `canary:' what
the original plugin would have changed. undecided means the sidecar did not answer: decide as you
would have, nothing is blocked. The sidecar sees the task text and the context you pass it and
nothing else; say so before sending anything private."
  "What (help :system-one) answers while the cell runs.")

(defun start ()
  "Nothing to run: the gates are called, not scheduled."
  nil)

(nle:define-cell system-one
  (:section ("system-one")
    (:guide "run the Local System One sidecar (a separate install from the same repository, Apple silicon) and point service_url at it; the default is its loopback port 8787; auth_token is needed only when the sidecar was started with one; next: ask (system-one:route \"your task\") from eval, or call the system_one_route tool")
    ("service_url" :string :default "http://127.0.0.1:8787"
     :doc "where the Local System One sidecar answers")
    ("timeout_ms" :integer :default 500 :min 50
     :doc "one gate's HTTP ceiling in milliseconds, clamped to at most 5000; a slow gate fails open")
    ("search_gate" :boolean :default t
     :doc "ask whether a task needs the public web, in route and search-gate")
    ("model_tier_gate" :boolean :default t
     :doc "ask whether a task is fast or strong tier, in route and tier-gate")
    ("notification_gate" :boolean :default t
     :doc "ask how urgent an event is, in notify-gate and system_one_notify")
    ("canary_advice" :boolean :default t
     :doc "say, under canary:, what the original plugin's acknowledged Canary mode would have changed")
    ("auth_token" :secret
     :doc "the bearer token the sidecar was started with, written here (optional)")
    ("auth_token_env" :env
     :doc "the environment variable holding that token, when it is not written here (optional)"))
  (:settings #'read-settings)
  (:help :system-one
         "system-one: ask a local System One sidecar whether a task needs the web, its model tier, and whether an event should notify"
         +primer+)
  (:tool "system_one_route" (nlk:contract 'system-one-route) #'route-tool)
  (:tool "system_one_notify" (nlk:contract 'system-one-notify) #'notify-tool)
  (:command "system-one" 'run-command
            :description "Ask the local System One sidecar about a task"
            :argument-hint "TASK")
  (:start #'start))
