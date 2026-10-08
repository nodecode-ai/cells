;;;; provider.lisp --- what GitLab Duo's chat is: its gateway, its model identities, its three wires.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): ai/src/providers/gitlab-duo.ts (the
;;;; direct-access exchange and the dispatch onto three wires),
;;;; catalog/src/provider-models/special.ts (the Duo aliases and the
;;;; upstream ids they stand for), runtime/behavior.kdl's
;;;; `api-routes provider="gitlab-duo"' (which wire a model rides),
;;;; providers/gitlab-duo.kdl (the GPT fronts take no sampling parameters),
;;;; ai/src/providers/anthropic.ts (the bearer an OAuth Messages round sends,
;;;; the budget thinking block) and the bundled rows of catalog/src/models.json,
;;;; which models.json in this folder carries (tools/omp-models.py wrote it).
;;;;
;;;; A GitLab token does not reach a model. Each round trades it at
;;;; gitlab.com for a Duo direct-access token and the headers GitLab's AI
;;;; gateway wants beside it (kept 25 minutes per GitLab token), then goes to
;;;; cloud.gitlab.com: Claude models to its Anthropic proxy on the Messages
;;;; wire, the GPT-5 codex models to its OpenAI proxy on the Responses wire,
;;;; every other model to that proxy on the chat wire. The model the proxy is
;;;; asked for is the upstream id the Duo alias stands for.

(in-package #:nodecode-gitlab-duo)

(defparameter +gitlab-url+ "https://gitlab.com"
  "The GitLab instance the sign-in and the direct-access exchange talk to.")

(defparameter +gateway-url+ "https://cloud.gitlab.com"
  "GitLab's AI gateway, whose proxies serve the models.")

(defparameter +env+ '("GITLAB_TOKEN")
  "The environment variables a GitLab token is read from: a personal access
token with the api scope does what the sign-in's token does.")

(defparameter +direct-access-seconds+ (* 25 60)
  "How long a direct-access token is reused: omp's DIRECT_ACCESS_TTL_MS.")

(defparameter +identities+
  '(("duo-chat-opus-4-6" "claude-opus-4-6")
    ("duo-chat-sonnet-4-6" "claude-sonnet-4-6")
    ("duo-chat-opus-4-5" "claude-opus-4-5-20251101")
    ("duo-chat-sonnet-4-5" "claude-sonnet-4-5-20250929")
    ("duo-chat-haiku-4-5" "claude-haiku-4-5-20251001")
    ("duo-chat-gpt-5-1" "gpt-5.1-2025-11-13")
    ("duo-chat-gpt-5-2" "gpt-5.2-2025-12-11")
    ("duo-chat-gpt-5-mini" "gpt-5-mini-2025-08-07")
    ("duo-chat-gpt-5-codex" "gpt-5-codex")
    ("duo-chat-gpt-5-2-codex" "gpt-5.2-codex"))
  "(ALIAS UPSTREAM): omp's GITLAB_DUO_MODEL_IDENTITIES, the Duo alias a
picker shows and the model the proxy is asked for.")

(defparameter +anthropic-prefixes+
  '("duo-chat-opus-" "duo-chat-sonnet-" "duo-chat-haiku-" "claude-opus-" "claude-sonnet-" "claude-haiku-")
  "The ids that ride the Anthropic proxy on the Messages wire.")

(defparameter +responses-models+
  '("duo-chat-gpt-5-codex" "duo-chat-gpt-5-2-codex" "gpt-5-codex" "gpt-5.2-codex")
  "The ids that ride the OpenAI proxy on the Responses wire; every other id
not on the Messages wire rides its chat wire.")

(defparameter +budget-effort-models+ '("claude-opus-4-5-20251101")
  "The rows omp gives thinking-mode anthropic-budget-effort: the effort rides
beside the budget block. Every other Claude row thinks on the budget alone.")

(defparameter +thinking-budgets+
  '(("minimal" . 1024) ("low" . 4096) ("medium" . 8192) ("high" . 16384)
    ("xhigh" . 32768) ("max" . 32768))
  "omp's ANTHROPIC_THINKING: budget_tokens per effort.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-gitlab-duo" "models.json")))
  "omp's bundled gitlab-duo rows, read when this file loads: a vector of objects.")

(defun upstream-id (model-id)
  "The model the proxy is asked for when the picker says MODEL-ID: the
upstream id of a Duo alias, an upstream id as it is, else NIL. omp's
resolveGitLabDuoModelIdentity."
  (or (second (assoc model-id +identities+ :test #'equal))
      (and (find model-id +identities+ :key #'second :test #'equal) model-id)))

(defun model-lane (model-id)
  "The lane MODEL-ID rides, by omp's routes: anthropic, openai-responses, or
the chat lane."
  (cond ((some (lambda (prefix) (uiop:string-prefix-p prefix model-id)) +anthropic-prefixes+) "anthropic")
        ((member model-id +responses-models+ :test #'equal) "openai-responses")
        (t "openai-completions")))

(defun catalog-model (row)
  "ROW as the catalog keeps a model (NLE::MAKE-CATALOG-MODEL's fields)."
  (flet ((value (type key) (nlk:json-value row type key)))
    (let ((cost (value :object "cost")))
      (nle::make-catalog-model
       (value :string "name")
       (value :integer "context")
       (value :integer "output")
       (or (value :array "input") #("text"))
       #("text")
       (sort (remove-if-not #'nle::effort-rank (coerce (or (value :array "efforts") #()) 'list))
             #'< :key #'nle::effort-rank)
       (value :boolean "reasoning")
       nil
       t
       ;; CATALOG-PRICE's shape; a row priced at nothing is a plan's, not a price
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun proxy (lane)
  "The base of the gateway proxy LANE goes to, as the lane composes it."
  (let ((gateway (string-right-trim "/" (setting :gateway-url))))
    (if (equal lane "anthropic")
        (concatenate 'string gateway "/ai/v1/proxy/anthropic/v1")
        (concatenate 'string gateway "/ai/v1/proxy/openai/v1"))))

(defun catalog-row ()
  "GitLab Duo as a models.dev provider: the Messages lane's package and the
Anthropic proxy as its own (the default model is a Claude), the token
variable, omp's bundled models."
  (let ((models (make-hash-table :test 'equal)))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "GitLab Duo Non-Agentic"
                     "npm" "@ai-sdk/anthropic"
                     "api" (proxy "anthropic")
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first token one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

(defun operator-lane-p (model)
  "Whether providers.gitlab-duo in the shared config names a wire for MODEL."
  (or (nle::trimmed-config-string (nle::configured-provider-entry +provider+) "sdk")
      (nle::trimmed-config-string (nle::configured-model-entry +provider+ model) "sdk")))

;;; --- one HTTP exchange --------------------------------------------------------------

(defun body-string (body)
  "BODY, as dexador answered it, as a string."
  (typecase body
    (string body)
    ((vector (unsigned-byte 8)) (sb-ext:octets-to-string body :external-format :utf-8))
    (null "")
    (t (princ-to-string body))))

(defun http (method url &key headers content)
  "(values TEXT STATUS) of one request, a refusal's status and body included."
  (handler-case
      (multiple-value-bind (body status)
          (ecase method
            (:get (dex:get url :headers headers :connect-timeout 30 :read-timeout 30))
            (:post (dex:post url :headers headers :content content
                                 :connect-timeout 30 :read-timeout 30)))
        (values (body-string body) status))
    (dex:http-request-failed (condition)
      (values (body-string (dex:response-body condition)) (dex:response-status condition)))))

(defun ok-p (status)
  "Whether STATUS is a 2xx."
  (and (integerp status) (<= 200 status 299)))

;;; --- the direct-access token ---------------------------------------------------------

(defvar *direct-access* (make-hash-table :test 'equal :synchronized t)
  "GitLab token -> (TOKEN HEADERS EXPIRES): the direct-access grants in hand.")

(defun clear-direct-access ()
  "Forget every direct-access grant: a new sign-in, a refresh, a stop."
  (clrhash *direct-access*))

(defun direct-access (gitlab-token)
  "(values TOKEN HEADERS): the Duo direct-access grant GITLAB-TOKEN buys,
reused for 25 minutes. A refusal is a provider error with GitLab's status."
  (let ((held (gethash gitlab-token *direct-access*)))
    (if (and held (> (third held) (get-universal-time)))
        (values (first held) (second held))
        (multiple-value-bind (text status)
            (http :post (format nil "~a/api/v4/ai/third_party_agents/direct_access"
                                (string-right-trim "/" (setting :gitlab-url)))
                  :headers `(("Authorization" . ,(format nil "Bearer ~a" gitlab-token))
                             ("Content-Type" . "application/json"))
                  :content (nlk:encode-json-object
                            (nlk:json-object "feature_flags" (nlk:json-object "DuoAgentPlatformNext" t))))
          (unless (ok-p status)
            (error 'nle::provider-error
                   :status status :scope :request
                   :detail (if (eql status 403)
                               (format nil "GitLab Duo access denied. Ensure Duo is enabled for this account. ~a" text)
                               (format nil "Failed to get GitLab Duo direct access token: ~a ~a" status text))))
          (let* ((payload (ignore-errors (nlk:decode-json text)))
                 (token (nlk:json-value payload :text "token"))
                 (headers (nlk:json-value payload :object "headers")))
            (unless token
              (error 'nle::provider-error :scope :request
                                          :detail "GitLab Duo direct access response missing token"))
            (unless headers
              (error 'nle::provider-error :scope :request
                                          :detail "GitLab Duo direct access response missing headers"))
            (let ((alist (loop for name being the hash-keys of headers using (hash-value value)
                               when (stringp value) collect (cons name value))))
              (setf (gethash gitlab-token *direct-access*)
                    (list token alist (+ (get-universal-time) +direct-access-seconds+)))
              (values token alist)))))))

(defun gateway-headers (headers gitlab-token)
  "HEADERS, a round's, for the gateway: the lane's own credential header out,
the direct-access token in as a bearer, the gateway's headers beside it."
  (multiple-value-bind (token extra) (direct-access gitlab-token)
    (append (list (cons "authorization" (format nil "Bearer ~a" token)))
            extra
            (remove-if (lambda (pair)
                         (or (member (car pair) '("x-api-key" "authorization") :test #'string-equal)
                             (assoc (car pair) extra :test #'string-equal)))
                       headers))))

;;; --- the bodies ---------------------------------------------------------------------

(defun budget-thinking (body model)
  "BODY, a Messages request for the upstream MODEL, thinking on omp's budget
block; adaptive thinking, which the core asks of a model whose ladder reaches
past high, the Duo rows do not declare."
  (let* ((thinking (nlk:json-value body :object "thinking"))
         (effort (nlk:json-value body :string "output_config" "effort"))
         (max-tokens (or (nlk:json-value body :integer "max_tokens") 32000)))
    (when (equal "adaptive" (nlk:json-value thinking :string "type"))
      (let* ((tier (cdr (assoc effort +thinking-budgets+ :test #'string-equal)))
             (budget (and tier (min tier (max 0 (- max-tokens 1024))))))
        (if (and budget (>= budget 1024))
            (setf (gethash "thinking" body) (nlk:json-object "type" "enabled" "budget_tokens" budget))
            (remhash "thinking" body))))
    (when (equal "enabled" (nlk:json-value body :string "thinking" "type"))
      (setf (gethash "display" (gethash "thinking" body)) "summarized"))
    (unless (and (member model +budget-effort-models+ :test #'equal)
                 (equal "enabled" (nlk:json-value body :string "thinking" "type")))
      (remhash "output_config" body))
    body))

(defun upstream-body (body model)
  "BODY asking for MODEL's upstream id; a GPT-5 front takes no sampling
parameters."
  (setf (gethash "model" body) (or (upstream-id model) model))
  (when (search "gpt-5" model)
    (remhash "temperature" body)
    (remhash "top_p" body))
  body)
