;;;; cell.lisp --- one route a turn: which model, and how hard it should think.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of jev-effort-router (see NOTICE). The original sits
;;;; between Hermes and its own model pick: on every llm_request it asks
;;;; TypeSafe's Jev decision endpoint one Choice question for the model and
;;;; one for the reasoning effort, then rewrites those two fields of the
;;;; request. Here the seam is the one Nodecode freezes a turn's provider
;;;; settings at, NLE::SNAPSHOT-CONTEXT-REQUEST: while the advice runs,
;;;; *MODEL* and *REASONING-EFFORT* are what the turn's frozen provider
;;;; config takes, so a decided route rides this turn and no other.
;;;;
;;;; Three rules carried over from the original, in the order they matter:
;;;;   - fail open. No key, no route, a wrong answer, a timeout, a model off
;;;;     the grid: the turn goes out byte-identical to an unrouted one.
;;;;   - one decision a turn. The rounds of a turn share one route (the memo),
;;;;     so a tool loop costs one decision call, not one per round.
;;;;   - one provider. A turn whose provider is not the section's is not
;;;;     routed at all: the grid names models that provider must serve.

(in-package #:nodecode-effort-router)

(nlk:define-peripheral effort-router :not-running t :lock "effort-router" :recursive t)

;;; --- data ----------------------------------------------------------------------
;;; Data over code: every string this cell sends to the decision model lives
;;; here, so a layer can rebind it and no rewrite touches the wire code.

(defparameter +effort-levels+ '("low" "medium" "high")
  "The rungs the effort question offers. The provider lane clamps each to what
the routed model actually admits, so a level the model lacks is never sent.")

(defparameter +effort-criteria+
  '(("low" . "a short, simple request: one fact, one line, a greeting; no reasoning needed")
    ("medium" . "everyday work: ordinary coding, explanation, summarising, a few steps of thought")
    ("high" . "hard work: multi-step reasoning, tricky debugging, mathematics, architecture, an answer that must be right"))
  "What each rung is for, sent to the decision model verbatim, in English.")

(defparameter +default-grid+
  '(("deepseek-v4.1-flash" . "the usual choice for general work: everyday writing, explanation, summarising, ordinary coding and tool use; cheap for its size")
    ("kimi-k3" . "strongest at complex code and long agentic tasks: multi-file refactors, deep debugging, large repositories; slow and the most expensive")
    ("glm-5.3" . "strongest at rigorous reasoning: mathematics, logic, science, quantitative and financial analysis, where a wrong answer is costly")
    ("glm-5.3-flash" . "best reasoning-per-cost on large text: drafting, summarising, translating and structured extraction over long documents; fast")
    ("minimax-m3" . "fast tool calling: long sequences of API or command-line actions, repetitive automation, high throughput")
    ("nemotron-3-nano:30b" . "highest throughput and lowest cost: trivial single-step requests only; weak at reasoning and at long context"))
  "The grid the original measured, kept as its data: (model . profile) pairs.
The profile line names a task family, never praises a model in the abstract.")

(defparameter +default-key-env+ "OPENROUTER_API_KEY"
  "Where the OpenRouter key is read from when the section names no variable.")

(defparameter +default-endpoint+ "https://openrouter.ai/api/alpha/decisions"
  "Jev's decision endpoint: not the OpenAI-compatible chat endpoint.")

(defparameter +default-decision-model+ "typesafe/jev-1.13"
  "The decision model. Pinned rather than \"latest\", so a model swap upstream
cannot change routing under the operator.")

(defparameter +default-provider+ "ollama-cloud"
  "The Nodecode provider a turn must be on to be routed.")

;;; --- what the running cell knows --------------------------------------------------
;;; Live-only: emptied at every start and at every stop.

(defvar *routes* nil
  "Memo key -> the route decided for it, or :NONE for a decision that failed.
A turn's rounds read one entry, so a tool loop decides once.")

(defvar *last* nil
  "The newest decision this boot, as a plist (:MODEL :EFFORT :CONFIDENCE :REASON),
or NIL when nothing was asked yet.")

(defvar *decisions* 0
  "How many decisions this boot asked the endpoint for, refusals included.")

;;; --- the section, read once the cell runs -------------------------------------

(defun api-key ()
  "The OpenRouter key named by the section, or NIL."
  (let ((name (or (setting :api-key-env) +default-key-env+)))
    (and (plusp (length (or name ""))) (nle::credential-env name))))

(defun split-entry (text)
  "(\"model\" . \"profile\") from \"model: profile\"; TEXT itself as the model
when no \": \" separates a profile from it."
  (let ((colon (search ": " text)))
    (if colon
        (cons (subseq text 0 colon) (subseq text (+ colon 2)))
        (cons (string-trim '(#\Space #\Tab) text) ""))))

(defun parse-grid (entries)
  "ENTRIES, the section's grid, as (model . profile) pairs, in order."
  (loop for entry in entries
        for text = (and (stringp entry) (string-trim '(#\Space #\Tab) entry))
        when (and text (plusp (length text))) collect (split-entry text)))

(defun grid ()
  "The grid this cell routes over: the section's, else the measured default."
  (let ((given (setting :grid)))
    (or (and given (plusp (length given)) (parse-grid given)) +default-grid+)))

(defun confidence-floor ()
  "The share a decision's confidence must reach to be taken, as a fraction."
  (/ (setting :confidence-percent) 100.0))

(defun routed-provider ()
  "The provider whose turns are routed."
  (let ((named (setting :provider)))
    (if (and named (plusp (length named))) named +default-provider+)))

;;; --- the wire ------------------------------------------------------------------

(defun criteria-object (pairs)
  "PAIRS, a choice question's (key . text) alist, as the JSON object the
endpoint reads: {\"<key>\": \"<text>\", ...}."
  (apply #'nlk:make-json-object
         (loop for (key . text) in pairs append (list key text))))

(defun choice-question (instructions pairs)
  "One Choice question: the instruction, and the criteria keyed so the answer
is a key this cell maps back, never the description text."
  (nlk:make-json-object "type" "choice"
                         "instructions" instructions
                         "criteria" (criteria-object pairs)))

(defun route-criteria ()
  "The model question's criteria: one numbered key per grid entry."
  (loop for (model . profile) in (grid)
        for index from 1
        collect (cons (princ-to-string index) (format nil "~a: ~a" model profile))))

(defun decisions-payload (text recent)
  "The whole request body: the model, the state, and the two independent
questions. They are answered in parallel upstream, so no question names the
other's answer."
  (nlk:make-json-object
   "model" (setting :decision-model)
   "state" (nlk:make-json-object
            "user_message" (nlk:clip text 4000)
            "recent_context" (nlk:clip (or recent "") 12000)
            "surface" "cli"
            "provider" (routed-provider))
   "questions"
   (nlk:make-json-object
    "model_route"
    (choice-question "Which model is best suited to handle this task?" (route-criteria))
    "reasoning_effort"
    (choice-question "Which level of reasoning effort does this message require?"
                      +effort-criteria+))))

(defun read-decision (body)
  "BODY, the endpoint's answer => (values ROUTE REASON). ROUTE is
(:model M :effort E :confidence C), or NIL with the one-line REASON why the
turn keeps what it had."
  (let ((object (and (stringp body) (plusp (length body)) (ignore-errors (nlk:decode-json body)))))
    (if (not (hash-table-p object))
        (values nil "the answer was not JSON")
        (let* ((answers (gethash "answers" object))
               (model-answer (and (hash-table-p answers) (gethash "model_route" answers)))
               (effort-answer (and (hash-table-p answers) (gethash "reasoning_effort" answers)))
               (choice (and (hash-table-p model-answer) (gethash "choice" model-answer)))
               (confidence (and (hash-table-p model-answer) (gethash "confidence" model-answer)))
               (level (and (hash-table-p effort-answer) (gethash "choice" effort-answer))))
          (cond ((not (and (stringp choice) (stringp level)))
                 (values nil "the answer lacked one of the two choices"))
                ((not (member choice (mapcar #'car (grid)) :test #'string=))
                 (values nil (format nil "~a is not in the grid" choice)))
                ((not (member level +effort-levels+ :test #'string=))
                 (values nil (format nil "~a is not an effort level" level)))
                ((and (realp confidence) (< confidence (confidence-floor)))
                 (values nil (format nil "confidence ~,2f is under the floor ~,2f"
                                     confidence (confidence-floor))))
                (t (values (list :model choice
                                 :effort level
                                 :confidence (and (realp confidence) confidence))
                           nil)))))))

(defun decide (text &optional (recent ""))
  "Ask the decision endpoint about TEXT with RECENT for context =>
(values ROUTE REASON), as READ-DECISION answers. Every failure of the wire
itself is a REASON, never a signal: the turn must not wait on a router."
  (let ((endpoint (setting :endpoint)))
    (cond ((not (and endpoint (plusp (length endpoint))))
           (values nil "no endpoint is set"))
          ((null (api-key))
           (values nil "no OpenRouter key"))
          (t
           (handler-case
               (multiple-value-bind (body status)
                   (nlk:http :post endpoint
                             :headers (list (cons "authorization"
                                                  (format nil "Bearer ~a" (api-key)))
                                            (cons "content-type" "application/json")
                                            (cons "accept" "application/json"))
                             :content (nlk:encode-json-object (decisions-payload text recent))
                             :timeout (setting :timeout-seconds))
                 (if (and (integerp status) (>= status 300))
                     (values nil (format nil "the endpoint answered HTTP ~d" status))
                     (read-decision body)))
             (nlk:turn-cancelled-condition (condition) (error condition))
             (error (condition)
               (values nil (format nil "~a" (type-of condition)))))))))

;;; --- the audit trail -------------------------------------------------------------
;;; One line a decision, under the cell's own home: the operator's answer to
;;; "why is this turn on that model". Nothing else is written, ever.

(defun audit-line (text route reason)
  "One JSON line for ROUTE (or the REASON no route was taken) about TEXT."
  (format nil "{\"at\":~d,\"model\":~s,\"effort\":~s,\"confidence\":~s,\"reason\":~s,\"ask\":~s}~%"
          (get-universal-time)
          (or (getf route :model) "")
          (or (getf route :effort) "")
          (let ((confidence (getf route :confidence)))
            (if confidence (format nil "~,3f" confidence) ""))
          (or reason "")
          (nlk:clip (nlk:one-line (or text "")) 160)))

(defun append-audit (line)
  "LINE appended to routes.jsonl under the cell's home, when the section keeps
one. A file that cannot be written is not a turn that fails."
  (when (setting :audit)
    (handler-case
        (let ((path (nlk:home "effort-router/routes.jsonl")))
          (ensure-directories-exist path)
          (with-open-file (out path :direction :output :if-exists :append
                                    :if-does-not-exist :create)
            (write-string line out)))
      (error (condition) (declare (ignore condition)) nil))))

(defun note-decision (text route reason)
  "Record ROUTE (or REASON) as this boot's newest decision, and audit it."
  (with-effort-router-lock
    (incf *decisions*)
    (setf *last* (if route
                     (list :model (getf route :model) :effort (getf route :effort)
                           :confidence (getf route :confidence))
                     (list :reason reason))))
  (append-audit (audit-line text route reason))
  route)

;;; --- a turn's route ---------------------------------------------------------------

(defun message-list (messages)
  "MESSAGES as a list: a request carries a list, a compiled context a vector."
  (cond ((null messages) '())
        ((listp messages) messages)
        ((vectorp messages) (coerce messages 'list))
        (t '())))

(defun user-text-of (messages)
  "The newest user message's text among MESSAGES, or NIL."
  (loop for message in (reverse (message-list messages))
        when (and (hash-table-p message) (nle::user-role-message-p message))
          do (let ((text (nle:message-content message)))
               (when (and text (plusp (length text))) (return text)))))

(defun recent-text-of (messages)
  "The last few messages of the turn, as the context the question may read."
  (let ((texts '()))
    (dolist (message (last (message-list messages) 6))
      (let ((text (and (hash-table-p message) (nle:message-content message))))
        (when (and text (plusp (length text))) (push (format nil "~a: ~a"
                                                             (gethash "role" message)
                                                             text)
                                                     texts))))
    (nlk:clip (format nil "~{~a~^~%~}" (nreverse texts)) 12000)))

(defun turn-key (session-id)
  "The memo key: the turn when a decision is per turn, else the session."
  (if (setting :route-per-turn)
      (or (getf (nle:turn) :turn-id) session-id)
      session-id))

(defun routing-live-p ()
  "Whether this turn is one this cell routes: a key, an endpoint, and the
section's provider in front of it."
  (let ((provider nle::*provider*))
    (and (api-key)
         (stringp provider)
         (string-equal provider (routed-provider))
         (let ((endpoint (setting :endpoint)))
           (and (stringp endpoint) (plusp (length endpoint)))))))

(defun route-for (messages session-id)
  "The route this turn takes, decided once and remembered => ROUTE or NIL."
  (when (routing-live-p)
    (let ((key (turn-key session-id))
          (text (user-text-of messages)))
      (when (and key text)
        (multiple-value-bind (remembered found)
            (with-effort-router-lock (gethash key *routes*))
          (cond (found (and (consp remembered) remembered))
                (t
                 (multiple-value-bind (route reason)
                     (decide text (recent-text-of messages))
                   (with-effort-router-lock
                     (when (> (hash-table-count *routes*) 512) (clrhash *routes*))
                     (setf (gethash key *routes*) (or route :none)))
                   (note-decision text route reason)))))))))

(defun advice-argument (arguments key)
  "KEY's value among the ARGUMENTS a context request is made with:
(SESSION-ID &key PRIOR-CONTEXT PROMPT TOOL-BINDINGS MESSAGES ...), so the
keys are the rest of the list, paired up."
  (loop for (name value) on (rest arguments) by #'cddr
        when (eq name key) return value))

(defun route-around (next &rest arguments)
  "Advice on NLE::SNAPSHOT-CONTEXT-REQUEST: the route decided for this turn
is what its frozen provider config is taken under, so *MODEL* and
*REASONING-EFFORT* are the turn's, and only this turn's. With no route the
call is the one it would have been."
  (let ((route (route-for (advice-argument arguments :messages)
                           (let ((head (first arguments)))
                             (and (not (keywordp head)) head)))))
    (if route
        (let ((nle::*model* (getf route :model))
              (nle::*reasoning-effort* (getf route :effort)))
          (apply next arguments))
        (apply next arguments))))

;;; --- what a person and the model read ---------------------------------------------

(defun describe-route (route)
  "One line naming ROUTE."
  (format nil "~a with reasoning effort ~a~@[ (confidence ~,2f)~]"
          (getf route :model) (getf route :effort) (getf route :confidence)))

(defun route-answer (text &optional (recent ""))
  "The routing decision for TEXT, decided now and audited, as one sentence
for the model or the operator."
  (multiple-value-bind (route reason) (decide text recent)
    (note-decision text route reason)
    (if route
        (format nil "effort-router: ~a. A turn on ~a asking ~s would run that way; reasoning effort is clamped to what the model admits."
                (describe-route route) (routed-provider) (nlk:clip (nlk:one-line text) 80))
        (format nil "effort-router: no route (~a), so a turn would keep the model and effort it already has."
                reason))))

(defun last-decision-text ()
  "The newest decision this boot, one line, or that there was none."
  (let ((last *last*))
    (cond ((null last) "none yet")
          ((getf last :reason) (format nil "no route (~a)" (getf last :reason)))
          (t (describe-route last)))))

(defun status-text ()
  "What the cell is set to and what it did, as the operator reads it."
  (format nil "effort-router: ~a~%  routed provider: ~a~%  decision model: ~a~%  endpoint: ~a~%  key: ~a~%  grid (~d): ~{~a~^, ~}~%  confidence floor: ~,2f~%  route per turn: ~:[no~;yes~]~%  decisions this boot: ~d~%  last decision: ~a~%  audit: ~:[off~;one line a decision, under the cell's home~]"
          (if *effort-router* "running" "not running")
          (routed-provider)
          (setting :decision-model)
          (setting :endpoint)
          (if (api-key) "set" "unset")
          (length (grid))
          (mapcar #'car (grid))
          (confidence-floor)
          (setting :route-per-turn)
          *decisions*
          (last-decision-text)
          (setting :audit)))

;;; --- the tools, the command, the manual --------------------------------------------

(nlk:define-contract route-ask
  (:doc "Ask the effort router how it would route a text — which model and which reasoning effort — without running a turn on it.")
  ("text" :text :required t :doc "the ask to route, as the operator would send it")
  ("recent" :string :default "" :doc "optional earlier context, one paragraph, when the ask needs it"))

(defparameter *status-schema*
  (list :description "What the effort router is set to, and the newest route it decided."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:make-json-object)))
  "No arguments: what the cell is set to, and what it last decided.")

(defun tool-route (ask)
  "The effort_route tool: one decision, answered as a sentence."
  (route-answer (route-ask-text ask) (or (route-ask-recent ask) "")))

(defun tool-status (arguments)
  "The effort_status tool: the section as it runs, and the newest decision."
  (declare (ignore arguments))
  (status-text))

(defun run-command (args session-id)
  "One /effort-router invocation: the status and a row to route a text, or —
with a text — the decision for it."
  (declare (ignore session-id))
  (let ((text (and args (string-trim '(#\Space #\Tab) args))))
    (cond ((or (null text) (zerop (length text)))
           (let ((status (status-text)))
             (values status
                     (nle:list-dialog
                      :title "Effort router"
                      :summary "Model and reasoning effort, chosen a turn at a time."
                      :rows (list (nle:ask-row "Route a text" "Ask" "/effort-router"
                                                :placeholder "what the turn should do"
                                                :verb "route"))))))
          ((string-equal text "status") (values (status-text) nil))
          (t (values (route-answer
                      (if (and (> (length text) 6) (string-equal (subseq text 0 6) "route "))
                          (string-trim '(#\Space #\Tab) (subseq text 6))
                          text))
                     nil)))))

(defun check-config ()
  "Refuse a section this cell could never route with, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (format nil "effort-router: no key: put an OpenRouter API key in the environment variable ~a, or name the variable that holds one in the effort-router section's api_key_env"
                           (or (setting :api-key-env) +default-key-env+))))
  (when (null (grid))
    (error 'nlk:config-refusal
           :detail "effort-router: the grid is empty: list the models to route over, one \"model: what it is for\" per entry")))

(defparameter +primer+
  "Effort routing is on: each turn on the routed provider is sent to a decision model
which picks the model and the reasoning effort for that turn, and the turn runs on them.
One verb, in eval, answering a sentence:
  (nodecode-effort-router:decide \"the ask\")   the route it would take, without running one;
      a second value says why there is none.
The router never blocks a turn: no key, no answer, a low-confidence answer or a model off
the grid leaves the turn exactly as it was."
  "What (help :effort-router) answers while the cell runs.")

(nle:define-cell effort-router
  (:section ("effort-router")
    (:guide "put an OpenRouter API key where api_key_env names (OPENROUTER_API_KEY by default; https://openrouter.ai/settings/keys); provider is the one Nodecode provider whose turns are routed; grid lists the models it may pick, as \"model: what it is for\"; next step: /effort-router shows what the cell is set to, and effort_route asks it about a text")
    ("api_key_env" :env :required t
                   :doc "the environment variable holding the OpenRouter API key (OPENROUTER_API_KEY unless named here)")
    ("provider" :string :default +default-provider+
                :doc "the Nodecode provider whose turns are routed; every other provider's turn is left alone")
    ("grid" :list
            :doc "the models the decision may choose, one \"model: what it is for\" per entry; empty means the measured six")
    ("decision_model" :string :default +default-decision-model+
                      :doc "the decision model id on OpenRouter; pinned so an upstream swap cannot change routing")
    ("endpoint" :string :default +default-endpoint+
                :doc "the decision endpoint: OpenRouter's Decisions API, not its chat endpoint")
    ("confidence_percent" :integer :default 50 :min 0
                          :doc "a decision under this confidence is dropped and the turn keeps its model")
    ("route_per_turn" :boolean :default t
                      :doc "decide for every turn; off, one decision serves a whole session")
    ("timeout_seconds" :integer :default 8 :min 1
                       :doc "one decision's ceiling; past it the turn goes on unrouted")
    ("audit" :boolean :default t
             :doc "keep one line a decision in routes.jsonl under the cell's home"))
  (:tool "effort_route" (nlk:contract 'route-ask) 'tool-route)
  (:tool "effort_status" *status-schema* 'tool-status)
  (:command "effort-router" 'run-command
            :description "Show the effort router, or ask it about a text"
            :argument-hint "TEXT")
  (:help :effort-router "effort-router:decide asks the routing model which model and reasoning effort an ask wants" +primer+)
  (:hook 'nle::snapshot-context-request #'route-around)
  (:start (lambda ()
            (check-config)
            (setf *routes* (make-hash-table :test #'equal)
                  *decisions* 0
                  *last* nil)
            (nle:on-stop (lambda ()
                           (setf *routes* nil *decisions* 0 *last* nil)))
            (nle:notice (format nil "effort-router: routing ~a turns through ~a"
                                (routed-provider) (setting :decision-model))
                        :key "nodecode-effort-router")
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-effort-router"))))))
