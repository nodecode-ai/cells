;;;; provider.lisp --- what Requesty is: its address, its key, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Rewritten from requestyai/hermes-provider-requesty (see NOTICE):
;;;; __init__.py's profile, after-install.md and plugin.yaml.
;;;;
;;;; Requesty speaks the OpenAI chat wire at https://router.requesty.ai/v1
;;;; with a bearer key from app.requesty.ai. What the original adds:
;;;;
;;;;   - a model is one of Requesty's managed routing policies -- a curated
;;;;     route with failover -- listed from GET /v1/models/managed, the ones
;;;;     that take tool calls; /requesty reads that catalog on demand and puts
;;;;     it in /models, and the bundled rows stand in until it is read
;;;;   - reasoning rides as a top-level reasoning_effort in Requesty's
;;;;     spelling (min, never minimal; no xhigh), and only to a model the
;;;;     catalog marks reasoning; an off selection is sent as "none"
;;;;   - the session id rides as requesty.trace_id, so one conversation
;;;;     groups in the Requesty dashboard

(in-package #:nodecode-requesty)

(defparameter +base+ "https://router.requesty.ai/v1"
  "Where Requesty is served: the base the chat lane appends /chat/completions to.")

(defparameter +env+ '("REQUESTY_API_KEY")
  "The environment variables a key is read from, in order.")

(defparameter +key-page+ "https://app.requesty.ai/api-keys"
  "Where a key is made.")

(defparameter +efforts+ '("minimal" "low" "medium" "high" "max")
  "The rungs a reasoning Requesty model is offered: Requesty takes
none/min/low/medium/high/max, and xhigh is an OpenAI-only extra.")

(defparameter +wire-efforts+ '(("minimal" . "min")
                               ("low" . "low")
                               ("medium" . "medium")
                               ("high" . "high")
                               ("xhigh" . "high")
                               ("max" . "max")
                               ("off" . "none")
                               ("none" . "none"))
  "Nodecode's effort rung as Requesty spells it on the wire.")

(defparameter +models+
  '(("claude-sonnet-5-5" . "Claude Sonnet 5.5")
    ("claude-opus-5-5" . "Claude Opus 5.5")
    ("claude-haiku-5-5" . "Claude Haiku 5.5")
    ("gpt-6.1-sol" . "GPT 6.1 Sol")
    ("gemini-3.8-flash" . "Gemini 3.8 Flash")
    ("grok-4.7" . "Grok 4.7")
    ("kimi-k3" . "Kimi K3")
    ("glm-5.3" . "GLM 5.3"))
  "The managed policies the original's profile lists as its fallback, shown
until /requesty reads the live catalog: (ID . NAME).")

;;; --- the models ----------------------------------------------------------------

(defun row-model (id name &key (reasoning t) context)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (nle::make-catalog-model name context nil #("text") #("text")
                            (and reasoning +efforts+) reasoning nil t nil))

(defun policy-entry-p (entry)
  "Whether ENTRY, one of the managed catalog's rows, is a route a turn can
take: it names an id, takes tool calls, and is a chat lane."
  (and (hash-table-p entry)
       (stringp (nlk:json-value entry :text "id"))
       (eq t (nlk:json-value entry :boolean "supports_tool_calling"))
       (let ((api (nlk:json-value entry :text "api")))
         (or (null api) (equal api "chat")))))

(defun managed-model (entry)
  "ENTRY, a managed policy, as the catalog keeps a model."
  (let ((id (nlk:json-value entry :text "id"))
        (context (nlk:json-value entry :integer "context_window")))
    (row-model id id
               :reasoning (eq t (nlk:json-value entry :boolean "supports_reasoning"))
               :context (and context (plusp context) context))))

(defvar *managed* nil
  "The managed policies /requesty last read from the gateway, as its entries;
NIL before the command runs.")

(defun catalog-row (&optional prior)
  "Requesty as a models.dev provider: its id and name, the chat lane's
package, this section's base, the key variable, and the managed policies
/requesty read over the bundled rows, over PRIOR's (when models.dev published
a requesty row of its own)."
  (let ((models (make-hash-table :test 'equal)))
    (when (hash-table-p prior)
      (let ((published (nlk:json-value prior :object "models")))
        (when (hash-table-p published)
          (maphash (lambda (id model) (setf (gethash id models) model)) published))))
    (dolist (row +models+)
      (setf (gethash (car row) models) (row-model (car row) (cdr row))))
    (dolist (entry *managed*)
      (setf (gethash (nlk:json-value entry :text "id") models) (managed-model entry)))
    (nlk:json-object "name" "Requesty"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defvar *catalog* nil
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Requesty's row in it; NIL before the first read.")

(defun catalog (next &aux (base (funcall next)))
  "MODELS-CATALOG-TABLE advice: the catalog with Requesty's row, made once per
catalog the core reads and again after /requesty read a new policy list."
  (if (and *catalog* (eq (car *catalog*) base))
      (cdr *catalog*)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun forget-catalog ()
  "Drop the merged catalog and the policy list /requesty read, so a restart
with another base builds both again."
  (setf *catalog* nil *managed* nil))

;;; --- the key -------------------------------------------------------------------

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun credential (op next)
  "The :CREDENTIAL answer for requesty: REQUESTY_API_KEY, else none. A key
/connect saved in auth.json answers before this point."
  ;; Never NEXT for requesty: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to Requesty.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

;;; --- the request ------------------------------------------------------------------

(defun reasons-p (model)
  "Whether the catalog says MODEL on Requesty reasons."
  (let ((capability (ignore-errors (nle::resolve-model-capability model +provider+))))
    (and capability (nle::model-capability-reasoning-p capability))))

(defun wire-effort (effort)
  "EFFORT, a Nodecode rung, as Requesty spells it."
  (or (cdr (assoc (string-downcase (string effort)) +wire-efforts+ :test #'equal))
      (string-downcase (string effort))))

(defun shape-body (body context config)
  "BODY, the chat request the core built for the Requesty round CONFIG, as the
original sends it: reasoning_effort in Requesty's spelling and only to a model
the catalog marks reasoning, and the session id as Requesty's trace."
  (let* ((model (nle::effective-provider-config-model config))
         (selected (nle::effective-provider-config-reasoning-effort config))
         (asked (gethash "reasoning_effort" body))
         (effort (and (reasons-p model) (or asked selected))))
    ;; The core's chat lane omits the key for an off selection; Requesty's
    ;; documented control spells that "none", so it is put back here.
    (remhash "reasoning_effort" body)
    (when effort
      (setf (gethash "reasoning_effort" body) (wire-effort effort)))
    (alexandria:when-let (session (nle::compiled-turn-context-session-id context))
      (setf (gethash "requesty" body) (nlk:json-object "trace_id" session)))
    body))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a requesty round is shaped as the original sends it."
  (when (and config
             (equal (nle::effective-provider-config-provider config) +provider+)
             (hash-table-p body))
    (shape-body body context config))
  body)

;;; --- the managed catalog --------------------------------------------------------------

(defun base ()
  "The base this section names, without a trailing slash."
  (string-right-trim "/" (setting :base-url)))

(defun fetch-managed (&optional (timeout 15))
  "GET <base>/models/managed: the managed policies a turn can take, and NIL;
or NIL and the reason it could not be read."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get (format nil "~a/models/managed" (base))
                    :headers '(("accept" . "application/json"))
                    :timeout timeout)
        (if (and (integerp status) (= status 200))
            (let* ((document (handler-case (nlk:decode-json body) (error () nil)))
                   (data (and document (nlk:json-value document :array "data")))
                   (policies (remove-if-not #'policy-entry-p (coerce (or data #()) 'list))))
              (values policies nil))
            (values nil (format nil "HTTP ~a" status))))
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun run-command (args session-id)
  "The /requesty command: read Requesty's managed policies and list them, so
/models offers what this key routes; ARGS narrows the list by substring."
  (declare (ignore session-id))
  (multiple-value-bind (policies why) (fetch-managed)
    (cond
      ((null policies)
       (format nil "Requesty: the managed catalog at ~a did not answer (~a)." (base) why))
      (t
       (setf *catalog* nil)
       (setf *managed* policies)
       (let* ((needle (string-downcase (or args "")))
              (shown (remove-if-not
                      (lambda (entry)
                        (search needle (string-downcase (nlk:json-value entry :text "id"))))
                      policies)))
         (if (null shown)
             (format nil "Requesty: ~a managed route~:p read; none matches ~s." (length policies) args)
             (with-output-to-string (out)
               (format out "Requesty ~a managed route~:p read, ~a listed; /models offers them now:~%"
                       (length policies) (length shown))
               (dolist (entry (subseq shown 0 (min 25 (length shown))))
                 (format out "  ~a~%" (nlk:json-value entry :text "id")))
               (when (> (length shown) 25)
                 (format out "  ... and ~a more~%" (- (length shown) 25))))))))))
