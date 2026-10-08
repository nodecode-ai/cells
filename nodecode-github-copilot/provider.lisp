;;;; provider.lisp --- what GitHub Copilot is: its addresses, its identity, its models, its wires.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/github-copilot.kdl and providers/github-copilot.kdl, catalog/src/
;;;; wire/github-copilot.ts (the API identity headers, the enterprise and
;;;; plan endpoints, the key envelope), ai/src/providers/github-copilot-
;;;; headers.ts (the per-request headers), the github-copilot api-routes of
;;;; catalog/src/compat/rules.json, and the bundled rows of catalog/src/
;;;; models.json, which models.json in this folder carries (tools/omp-
;;;; models.py wrote it).
;;;;
;;;; Copilot serves three wires at one address, https://api.githubcopilot.com:
;;;; the Claude models speak Anthropic Messages (/v1/messages), the newer
;;;; OpenAI, Grok and MAI models the Responses API (/responses), and the rest
;;;; OpenAI chat (/chat/completions). Every request carries the GitHub token
;;;; as a bearer and the Copilot CLI's identity headers. A personal account
;;;; is served at the plan's own host (api.individual.githubcopilot.com and
;;;; the like), which GitHub names for the token; a GitHub Enterprise account
;;;; at copilot-api.<its domain>.

(in-package #:nodecode-github-copilot)

(defparameter +base+ "https://api.githubcopilot.com"
  "Where Copilot is served for a personal account whose plan host is not known.")

(defparameter +env+ '("COPILOT_GITHUB_TOKEN")
  "The environment variables a GitHub token for Copilot is read from, in order.")

(defparameter +cli-version+ "1.0.82"
  "The Copilot CLI version the identity headers mirror.")

(defparameter +api-version+ "2026-08-01"
  "The Copilot API version every api.githubcopilot.com request names: newer
versions unlock the long-context tiers.")

(defparameter +chat-integration-id+ "copilot-chat"
  "The chat surface's integration id: what a personal account's requests name,
since some Business organizations admit chat clients and refuse the CLI.")

(defparameter +cli-integration-id+ "copilot-developer-cli"
  "The Copilot CLI's integration id: an Enterprise account's, and the one
retry a refused chat identity gets.")

(defun cli-user-agent ()
  "The Copilot CLI's user agent."
  (format nil "copilot/~a" +cli-version+))

(defun identity-headers ()
  "The Copilot CLI identity a request to the Copilot API carries
(COPILOT_CAPI_IDENTITY_HEADERS)."
  `(("User-Agent" . ,(cli-user-agent))
    ("Editor-Version" . ,(cli-user-agent))
    ("Copilot-Integration-Id" . ,+cli-integration-id+)
    ("Copilot-Harness-Id" . "copilot-sdk")
    ("Openai-Intent" . "conversation-agent")))

(defun api-headers ()
  "The headers every Copilot model request starts from (COPILOT_API_HEADERS):
the identity and the API version."
  (append (identity-headers) `(("X-GitHub-Api-Version" . ,+api-version+))))

(defun normalize-integration-id (value)
  "VALUE as a Copilot-Integration-Id, or NIL when it is blank or carries a
line break."
  (let ((trimmed (and (stringp value) (nlk:trimmed value))))
    (and trimmed (plusp (length trimmed))
         (not (find #\Return trimmed)) (not (find #\Newline trimmed))
         trimmed)))

(defun pinned-integration-id ()
  "The integration id COPILOT_INTEGRATION_ID pins, or NIL: a pin is never
second-guessed by the identity retry."
  (normalize-integration-id (uiop:getenv "COPILOT_INTEGRATION_ID")))

;;; --- the addresses ---------------------------------------------------------------

(defparameter +public-hosts+ '("api.github.com" "github.com" "www.github.com")
  "The hosts that mean public github.com, not an Enterprise instance.")

(defun public-host-p (host)
  "Whether HOST is public GitHub."
  (and (stringp host) (member (string-downcase (nlk:trimmed host)) +public-hosts+ :test #'string=) t))

(defun normalize-domain (input)
  "The host INPUT names (a domain or a URL), or NIL when it names none."
  (let ((trimmed (and (stringp input) (nlk:trimmed input))))
    (when (and trimmed (plusp (length trimmed)))
      (let ((host (ignore-errors
                   (quri:uri-host (quri:uri (if (search "://" trimmed)
                                                trimmed
                                                (concatenate 'string "https://" trimmed)))))))
        (and (stringp host) (plusp (length host)) (string-downcase host))))))

(defun enterprise-domain (input)
  "The GitHub Enterprise domain INPUT names, or NIL for public GitHub or nothing."
  (let ((domain (normalize-domain input)))
    (and domain (not (public-host-p domain)) domain)))

(defun enterprise-base (domain)
  "The Copilot API base of the Enterprise instance at DOMAIN."
  (format nil "https://~a" (if (uiop:string-prefix-p "copilot-api." domain)
                               domain
                               (concatenate 'string "copilot-api." domain))))

(defun normalize-api-endpoint (input)
  "INPUT as a plan endpoint: an https URL with a host, its trailing slashes
trimmed, or NIL."
  (let ((trimmed (and (stringp input) (nlk:trimmed input))))
    (and trimmed (uiop:string-prefix-p "https://" trimmed)
         (ignore-errors (plusp (length (quri:uri-host (quri:uri trimmed)))))
         (string-right-trim "/" trimmed))))

(defun account-base (configured &key enterprise api-endpoint)
  "Where an account is served when the section says CONFIGURED: the plan's
endpoint GitHub named, else the Enterprise instance's, as long as CONFIGURED
is Copilot's own host; a relay the operator configured stays."
  ;; resolveGitHubCopilotBaseUrl
  (let ((copilot-p (search "githubcopilot.com" configured)))
    (cond ((and api-endpoint copilot-p) api-endpoint)
          ((and enterprise copilot-p) (enterprise-base enterprise))
          (t configured))))

(defun parse-api-key (raw)
  "(values TOKEN ENTERPRISE API-ENDPOINT) of the key RAW: omp's structured
key, a JSON object {token, enterpriseUrl, apiEndpoint}, or a bare token."
  (let ((parsed (and (stringp raw) (uiop:string-prefix-p "{" (nlk:trimmed raw))
                     (ignore-errors (nlk:decode-json raw)))))
    (alexandria:if-let (token (nlk:json-value parsed :text "token"))
      (values token
              (enterprise-domain (nlk:json-value parsed :string "enterpriseUrl"))
              (normalize-api-endpoint (nlk:json-value parsed :string "apiEndpoint")))
      (values raw nil nil))))

(defun env-key ()
  "The first GitHub token one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-github-copilot" "models.json")))
  "omp's bundled Copilot rows, read when this file loads: a vector of objects.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defparameter +api-lanes+
  '(("anthropic-messages" . "anthropic")
    ("openai-responses" . "openai-responses")
    ("openai-completions" . "openai-completions"))
  "omp's api names, as the Nodecode lane that speaks each.")

(defun routed-api (model-id)
  "The api omp's github-copilot api-routes give MODEL-ID, a model the bundled
rows do not carry: Claude on Messages, the newer OpenAI, Grok and MAI models
on Responses, the rest on chat."
  (let ((id (string-downcase model-id)))
    (cond ((some (lambda (family) (uiop:string-prefix-p family id))
                 '("claude-haiku-" "claude-sonnet-" "claude-opus-" "claude-fable-" "claude-mythos-"))
           "anthropic-messages")
          ((some (lambda (prefix) (uiop:string-prefix-p prefix id))
                 '("grok-4." "gpt-5" "gpt-6" "oswe" "mai-"))
           "openai-responses")
          (t "openai-completions"))))

(defun model-lane (model-id)
  "The Nodecode lane MODEL-ID rides at Copilot."
  (cdr (assoc (or (nlk:json-value (model-row model-id) :string "api") (routed-api (or model-id "")))
              +api-lanes+ :test #'equal)))

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

(defun catalog-row (&optional prior)
  "GitHub Copilot as a models.dev provider: the chat lane's package (the
lane hooks route each model to its own), this section's base, the token
variable, and the bundled models over PRIOR's (the row models.dev itself
published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "GitHub Copilot"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))
