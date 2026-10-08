;;;; provider.lisp --- what Factory Droid is: its hosts, its roster, its routes, its identity.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; providers/factory-droid.kdl (the registry: each model's upstream
;;;; rotation, region overrides, effort ladder and default), runtime/
;;;; behavior.kdl (api-routes: each model's wire), packages/catalog/src/wire/
;;;; factory-droid.ts (the client version and the residency hosts),
;;;; packages/ai/src/providers/factory-droid.ts (the route, the effort, the
;;;; identity headers every wire sends), and discovery/factory-droid.ts,
;;;; whose offline answer is the roster models.json carries
;;;; (discovery-models.py wrote it; see the README).
;;;;
;;;; Factory's LLM proxy multiplexes four wires behind one WorkOS bearer, and
;;;; each model rides the one its upstream speaks:
;;;;
;;;;   openai-completions   /api/llm/o/v1/chat/completions   Kimi, GLM, DeepSeek, ...
;;;;   openai-responses     /api/llm/o/v1/responses          GPT, Grok
;;;;   anthropic-messages   /api/llm/a/v1/messages           Claude, MiniMax M2.7
;;;;   google-generate      /api/llm/g/v1/generate           Gemini
;;;;
;;;; on https://api.factory.ai, or https://api.eu.factory.ai for an account
;;;; whose residency is the EU. Every request names the upstream that serves
;;;; it (x-api-provider: the first of the model's rotation the account's
;;;; inference region serves), the session, a fresh message id, the org, and
;;;; the Factory CLI's own identity; the SDK fingerprint the CLI's clients
;;;; send rides along. The system prompt opens with Droid's identity line.

(in-package #:nodecode-factory-droid)

(defparameter +host+ "https://api.factory.ai"
  "Factory's global host, the section's default base.")

(defparameter +eu-host+ "https://api.eu.factory.ai"
  "Factory's host for an account whose residency is the EU.")

(defparameter +client-version+ "0.230.0"
  "The Factory CLI version the identity headers mirror.")

(defparameter +runtime-version+ "v26.3.0"
  "The Node version the SDK fingerprint names.")

(defparameter +identity+ "You are Droid, an AI software engineering agent built by Factory."
  "The line every Factory system prompt opens with.")

(defparameter +openai-platform+ "org-bHuLtG1fGmYk5YaOihAAXFBw"
  "The OpenAI-Platform hint a request routed to OpenAI or Azure OpenAI carries.")

(defparameter +wires+
  '(("openai-completions" :lane "openai-completions" :path "/api/llm/o/v1/chat/completions")
    ("openai-responses" :lane "openai-responses" :path "/api/llm/o/v1/responses")
    ("anthropic-messages" :lane "anthropic" :path "/api/llm/a/v1/messages")
    ("google-generate" :lane "google" :path "/api/llm/g/v1/generate"))
  "Each Factory wire: the Nodecode lane that speaks it and its path on the host.")

;;; --- the roster ------------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-factory-droid" "models.json")))
  "The roster omp's discovery answers with no live flags: a vector of objects.")

(defun model-row (model-id)
  "The roster row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun model-lane (model-id)
  "The lane MODEL-ID rides: its wire's; a model off the roster rides chat
completions, as omp's api-routes default says."
  (let ((wire (or (nlk:json-value (model-row model-id) :string "api") "openai-completions")))
    (getf (cdr (assoc wire +wires+ :test #'equal)) :lane)))

(defun lane-wire (lane)
  "The Factory wire the Nodecode LANE speaks."
  (car (find lane +wires+ :key (lambda (wire) (getf (cdr wire) :lane)) :test #'equal)))

(defun lane-path (lane)
  "LANE's path on a Factory host."
  (getf (cdr (assoc (lane-wire lane) +wires+ :test #'equal)) :path))

(defun host (region)
  "The host a REGION account is served at: the section's base, moved to the
EU host for EU residency while the base is Factory's own."
  (let ((base (string-right-trim "/" (setting :base-url))))
    (if (and (equal region "eu") (equal base +host+)) +eu-host+ base)))

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
       ;; the upstream's list price; Factory itself bills credits
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "Factory Droid as a models.dev provider: the chat lane's package (each
model's own lane is RESOLVE-MODEL-LANE's answer), the chat base, no key
variable (the sign-in is the only credential), and the roster over PRIOR's
models."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Factory Droid"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (concatenate 'string (string-right-trim "/" (setting :base-url)) "/api/llm/o/v1")
                     "env" #()
                     "models" models)))

(defun listing-rows ()
  "The roster as a provider listing answers it: Factory has no listing
endpoint, so the picker is answered from the roster without a request."
  (loop for row across +models+
        collect (list :id (nlk:json-value row :string "id")
                      :display (nlk:json-value row :string "name")
                      :context-window (nlk:json-value row :integer "context"))))

;;; --- one round's route -----------------------------------------------------------

(defun round-effort (row effort)
  "The effort a round on ROW runs at, from the session's EFFORT pick, or NIL
for thinking off: the pick, else the model's own default level, which is off
where the registry says so."
  (cond ((null effort)
         (and row (not (nlk:json-value row :boolean "default_off"))
              (nlk:json-value row :string "default_effort")))
        ((member effort '("off" "none") :test #'string-equal) nil)
        (t effort)))

(defun upstream (row region)
  "The upstream that serves ROW in the inference REGION: the first of its
rotation the region serves, or NIL when the region serves none."
  (let ((upstreams (nlk:json-value row :array "upstreams" region)))
    (and upstreams (plusp (length upstreams)) (aref upstreams 0))))

(defun output-limit (row region)
  "ROW's output ceiling in REGION: the regional one where the registry names it."
  (let ((global (nlk:json-value row :integer "output"))
        (regional (and (equal region "eu") (nlk:json-value row :integer "limits_eu" "output"))))
    (if (and global regional) (min global regional) (or regional global))))

(defun session-id ()
  "The session the round on this thread belongs to, or NIL outside a turn."
  (let ((turn nle::*current-durable-turn*))
    (and turn (nlk:durable-turn-session-id turn))))

(defun deterministic-uuid (seed)
  "The leading 128 bits of SEED's SHA-256 in a UUID's 8-4-4-4-12 shape: the
same session is the same id at every round, with nothing kept."
  (let ((hex (subseq (nlk::sha256-text seed) 7)))
    (format nil "~a-~a-~a-~a-~a" (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
            (subseq hex 16 20) (subseq hex 20 32))))

(defvar *random* (make-random-state t)
  "The random state fresh ids are drawn from.")

(defun random-uuid ()
  "A fresh version 4 UUID."
  (let ((bytes (loop repeat 16 collect (random 256 *random*))))
    (setf (nth 6 bytes) (logior #x40 (logand (nth 6 bytes) #x0f))
          (nth 8 bytes) (logior #x80 (logand (nth 8 bytes) #x3f)))
    (let ((hex (format nil "~(~{~2,'0x~}~)" bytes)))
      (format nil "~a-~a-~a-~a-~a" (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
              (subseq hex 16 20) (subseq hex 20 32)))))

;;; One round's facts, worked out where the body is built and read again where
;;; the request is sent: the session id and the message id the body names
;;; must be the ones the headers name. Keyed by the round's frozen config,
;;; which both seams are handed, and weak on it, so a finished round leaves
;;; nothing behind.
(defvar *rounds* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Frozen provider config -> the round's facts (a plist).")

(defun credential-region (config)
  "The residency region CONFIG's credential carries: eu or global."
  (or (nle::credential-attribute config :region) "global"))

(defun inference-region (config)
  "The inference region CONFIG's credential carries: global, us or eu."
  (or (nle::credential-attribute config :inference-region)
      (if (equal (credential-region config) "eu") "eu" "global")))

(defun assistant-p (context)
  "Whether CONTEXT's history holds an assistant message."
  (some (lambda (message) (equal "assistant" (nlk:json-value message :string "role")))
        (coerce (nle::request-messages context) 'list)))

(defun note-round (context)
  "Work out the facts of the round CONTEXT is, keep them for the send, and
answer them. A model its account's region does not serve is refused here."
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model (nle::effective-provider-config-model config))
         (row (model-row model))
         (region (inference-region config))
         (upstream (and row (upstream row region)))
         (session (session-id))
         (request (random-uuid)))
    (when (and row (null upstream))
      (error 'nle::provider-config-error
             :detail "Factory Droid model is unavailable in this account region."))
    (setf (gethash config *rounds*)
          (list :model model :row row :region region :upstream upstream
                :effort (round-effort row (nle::effective-provider-config-reasoning-effort config))
                :request request
                ;; no session: the message id stands in for it, as omp's does
                :session (if session (deterministic-uuid session) request)
                :assistant-p (assistant-p context)))))

(defun round-facts (config)
  "The facts NOTE-ROUND kept for CONFIG's round."
  (gethash config *rounds*))

;;; --- the identity a request carries --------------------------------------------

(defun stainless-arch ()
  "The architecture as the Stainless SDKs name it."
  (let ((arch (string-downcase (uiop:architecture))))
    (cond ((member arch '("x64" "amd64") :test #'equal) "x64")
          ((member arch '("arm64" "aarch64") :test #'equal) "arm64")
          ((member arch '("x86" "386" "ia32") :test #'equal) "x86")
          (t (format nil "other::~a" arch)))))

(defun stainless-os ()
  "The platform as the Stainless SDKs name it."
  (case (uiop:operating-system)
    (:macosx "MacOS")
    (:windows "Windows")
    (:linux "Linux")
    (:freebsd "FreeBSD")
    (t (format nil "Other::~(~a~)" (uiop:operating-system)))))

(defun jwt-claims (token)
  "The claims of the JWT TOKEN, unverified, or NIL when it is not one."
  (let* ((parts (and (stringp token) (uiop:split-string token :separator ".")))
         (text (and (= (length parts) 3) (second parts)))
         ;; base64url, unpadded: cl-base64's URI alphabet, padded with its dots
         (payload (and text
                       (ignore-errors
                        (cl-base64:base64-string-to-string
                         (concatenate 'string text (make-string (mod (- (length text)) 4)
                                                                :initial-element #\.))
                         :uri t)))))
    (let ((claims (and payload (ignore-errors (nlk:decode-json payload)))))
      (and (hash-table-p claims) claims))))

(defun org-id (config)
  "The Factory org CONFIG's round is billed to: the one the sign-in resolved,
else the token's external_org_id claim (WorkOS's own organization id is
internal and never this header)."
  (or (nle::credential-attribute config :org-id)
      (nlk:json-value (jwt-claims (nle::effective-provider-config-api-key config))
                      :text "external_org_id")))

(defun identity-headers (config lane facts)
  "The headers a Factory request on LANE carries beyond the lane's own."
  (let* ((wire (lane-wire lane))
         (upstream (getf facts :upstream))
         (org (org-id config))
         (locked (or (getf facts :assistant-p)
                     (member upstream '("xai" "google") :test #'equal))))
    (append
     `(("Authorization" . ,(format nil "Bearer ~a" (nle::effective-provider-config-api-key config)))
       ("User-Agent" . ,(format nil "factory-cli/~a" +client-version+))
       ("X-Client-Version" . ,+client-version+)
       ("X-Factory-Client" . "cli"))
     (when org `(("X-Factory-Org-Id" . ,org)))
     (when upstream `(("x-api-provider" . ,upstream)))
     `(("x-provider-routing-source" . ,(if locked "session_lock" "registry_default"))
       ("x-session-id" . ,(getf facts :session))
       ("x-assistant-message-id" . ,(getf facts :request)))
     (when (member wire '("openai-completions" "openai-responses") :test #'equal)
       `(("Accept" . "application/json")
         ,@(when (member upstream '("openai" "azure_openai") :test #'equal)
             `(("OpenAI-Platform" . ,+openai-platform+)))))
     (unless (equal wire "google-generate")
       `(("X-Stainless-Lang" . "js")
         ("X-Stainless-Package-Version" . ,(if (equal wire "anthropic-messages") "0.70.1" "6.25.0"))
         ("X-Stainless-Runtime" . "node")
         ("X-Stainless-Runtime-Version" . ,+runtime-version+)
         ("X-Stainless-Arch" . ,(stainless-arch))
         ("X-Stainless-OS" . ,(stainless-os))
         ("X-Stainless-Retry-Count" . "0")))
     (when (equal wire "anthropic-messages")
       ;; the Anthropic SDK's own: a 600 s client, a key it insists on, and
       ;; the headers omp's Anthropic client sends any host but Anthropic's
       '(("X-Stainless-Timeout" . "600")
         ("x-api-key" . "placeholder")
         ("Accept" . "text/event-stream")
         ("anthropic-dangerous-direct-browser-access" . "true")
         ("x-app" . "cli")))
     (when (equal wire "google-generate")
       '(("Accept" . "*/*"))))))

(defun request-headers (headers config lane facts)
  "HEADERS, the lane's own, with its credential headers replaced by Factory's
identity: the WorkOS bearer, never a lane's x-api-key or x-goog-api-key."
  (append (remove-if (lambda (name)
                       (member name '("authorization" "x-api-key" "x-goog-api-key") :test #'string-equal))
                     headers :key #'car)
          (identity-headers config lane facts)))
