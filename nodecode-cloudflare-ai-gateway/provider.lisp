;;;; provider.lisp --- what Cloudflare AI Gateway is: its address, its token, its routes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/cloudflare-ai-gateway.kdl and providers/cloudflare-ai-gateway.kdl,
;;;; runtime/behavior.kdl (the api-routes: which wire each namespace rides),
;;;; catalog/src/wire/cloudflare-ai-gateway.ts (the gateway's bases and the
;;;; stored credential's shape), ai/src/registry/cloudflare-ai-gateway.ts (the
;;;; per-request route, ids and auth), ai/src/registry/oauth/cloudflare-ai-
;;;; gateway.ts (the login: a token, an account id, a gateway id),
;;;; ai/src/providers/anthropic.ts (buildAnthropicHeaders' gateway branch),
;;;; and the bundled rows of catalog/src/models.json, which models.json in
;;;; this folder carries (tools/omp-models.py wrote it).
;;;;
;;;; The gateway lives at https://gateway.ai.cloudflare.com/v1/<account>/
;;;; <gateway>, and a model id's namespace picks the upstream route and wire:
;;;;
;;;;   anthropic/<id>    /anthropic, the Messages wire, sent as <id> with
;;;;                     dots turned to dashes (claude-sonnet-4.5 ->
;;;;                     claude-sonnet-4-5)
;;;;   openai/<id>       /openai, the chat wire, sent as <id>
;;;;   workers-ai/<id>   /compat, the chat wire, sent whole
;;;;   anything else     the wire its bundled row names, at its base, sent whole
;;;;
;;;; Every route authenticates to the gateway alone, with
;;;; `cf-aig-authorization: Bearer <token>': no Authorization and no x-api-key
;;;; ever leaves for the upstream.

(in-package #:nodecode-cloudflare-ai-gateway)

(defparameter +base+ "https://gateway.ai.cloudflare.com/v1"
  "The gateway's root, before /<account>/<gateway>.")

(defparameter +env+ '("CLOUDFLARE_AI_GATEWAY_API_KEY")
  "The environment variables a gateway token is read from, in order.")

(defparameter +account-env+ "CLOUDFLARE_ACCOUNT_ID"
  "Where omp reads the account id when the stored credential names none.")

(defparameter +gateway-env+ "CLOUDFLARE_GATEWAY_ID"
  "Where omp reads the gateway id when the stored credential names none.")

(defparameter +token-page+ "https://developers.cloudflare.com/ai-gateway/configuration/authentication/"
  "Where omp's login sends the operator: make an AI Gateway token with Run permission.")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-cloudflare-ai-gateway" "models.json")))
  "omp's bundled Cloudflare AI Gateway rows, read when this file loads: a vector of objects.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

(defun prefixed (prefix model-id)
  "MODEL-ID without PREFIX when it starts with it, else NIL."
  (and (stringp model-id) (uiop:string-prefix-p prefix model-id)
       (subseq model-id (length prefix))))

(defun route (model-id)
  "(values LANE WIRE-ID SEGMENT): the Nodecode lane MODEL-ID rides, the id
the gateway is asked for, and the gateway segment that serves it (omp's
api-routes and cloudflareAiGatewayTransport.prepareModel)."
  (alexandria:if-let (bare (prefixed "anthropic/" model-id))
    (values "anthropic" (substitute #\- #\. bare) "anthropic")
    (alexandria:if-let (bare (prefixed "openai/" model-id))
      (values "openai-completions" bare "openai")
      (if (prefixed "workers-ai/" model-id)
          (values "openai-completions" model-id "compat")
          ;; no route: the row's own wire and base, the id as it is; a model
          ;; off the roster rides the Messages route, as omp's discovery does
          (if (equal (nlk:json-value (model-row model-id) :string "api") "openai-completions")
              (values "openai-completions" model-id "compat")
              (values "anthropic" model-id "anthropic"))))))

(defun lane-segment (model-id lane)
  "The gateway segment a round of MODEL-ID on LANE goes to: the route's when
LANE is the route's own, else the segment that speaks LANE."
  (multiple-value-bind (route-lane wire segment) (route model-id)
    (declare (ignore wire))
    (cond ((equal lane route-lane) segment)
          ((equal lane "anthropic") "anthropic")
          ((prefixed "openai/" model-id) "openai")
          (t "compat"))))

(defun wire-id (model-id lane)
  "The id a round of MODEL-ID on LANE names: the route's when LANE is the
route's own lane, else the id as it is."
  (multiple-value-bind (route-lane wire) (route model-id)
    (if (equal lane route-lane) wire model-id)))

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
       ;; CATALOG-PRICE's shape; the upstream's list price, which the gateway passes on
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

;;; --- the gateway's address -------------------------------------------------------

(defun section-or-env (key variable)
  "The section's KEY when it is set, else the environment's VARIABLE, else NIL."
  (let ((value (setting key)))
    (if (and (stringp value) (plusp (length (string-trim " " value))))
        (string-trim " " value)
        (nle::credential-env variable))))

(defun parse-credential (text)
  "(values TOKEN ACCOUNT GATEWAY) out of a stored credential TEXT: a bare
token, or the JSON omp's login stores ({token, accountId, gatewayId});
NIL for anything else (omp's parseCloudflareAiGatewayCredential)."
  (let ((value (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (cond ((zerop (length value)) nil)
          ((char/= (char value 0) #\{) value)
          (t (let ((parsed (ignore-errors (nlk:decode-json value))))
               (flet ((field (key)
                        (let ((field (nlk:json-value parsed :string key)))
                          (and field (plusp (length (string-trim " " field))) (string-trim " " field)))))
                 (when (and (hash-table-p parsed) (field "token")
                            (typep (gethash "accountId" parsed) '(or null string))
                            (typep (gethash "gatewayId" parsed) '(or null string)))
                   (values (field "token") (field "accountId") (field "gatewayId")))))))))

(defun gateway-root (account gateway)
  "The gateway's own root for ACCOUNT and GATEWAY, placeholders where either is NIL."
  (format nil "~a/~a/~a" (string-right-trim "/" (setting :base-url))
          (or account "<account>") (or gateway "<gateway>")))

(defun round-route (credential)
  "(values TOKEN ROOT) for a round sent with CREDENTIAL, the frozen key: the
token, and the gateway root its account and gateway name. The account and
the gateway come from the credential, else the section, else
CLOUDFLARE_ACCOUNT_ID and CLOUDFLARE_GATEWAY_ID; one missing is refused in
the words omp uses."
  (multiple-value-bind (token account gateway) (parse-credential credential)
    (unless (and token (string/= token "public"))
      (error 'nle::provider-config-error
             :detail (format nil "no Cloudflare AI Gateway token: make one with Run permission (~a), ~
                                  then save it with /connect or set ~a" +token-page+ (first +env+))))
    (let ((account (or account (section-or-env :account-id +account-env+)))
          (gateway (or gateway (section-or-env :gateway-id +gateway-env+))))
      (unless account
        (error 'nle::provider-config-error
               :detail (format nil "Cloudflare account ID is required: set cloudflare-ai-gateway.account_id ~
                                    or ~a" +account-env+)))
      (unless gateway
        (error 'nle::provider-config-error
               :detail (format nil "Cloudflare AI Gateway ID is required: set cloudflare-ai-gateway.gateway_id ~
                                    or ~a" +gateway-env+)))
      (values token (gateway-root account gateway)))))

(defun round-endpoint (root model-id lane)
  "The address a round of MODEL-ID on LANE posts to under the gateway ROOT."
  (format nil "~a/~a~a" root (lane-segment model-id lane)
          (if (equal lane "anthropic") "/v1/messages" "/chat/completions")))

(defun round-headers (headers token)
  "HEADERS, the lane's own, with every credential header taken out and the
gateway's in its place (omp's prepareRequest and buildAnthropicHeaders)."
  (append (remove-if (lambda (name) (member name '("authorization" "x-api-key") :test #'string-equal))
                     headers :key #'car)
          `(("cf-aig-authorization" . ,(format nil "Bearer ~a" token)))))

(defun catalog-row (&optional prior)
  "Cloudflare AI Gateway as a models.dev provider: the Messages lane's
package (each model's own lane is RESOLVE-MODEL-LANE's answer), the
gateway's Messages base for the section's account and gateway, the token
variable, and the bundled models over PRIOR's."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Cloudflare AI Gateway"
                     "npm" "@ai-sdk/anthropic"
                     "api" (format nil "~a/anthropic/v1"
                                   (gateway-root (section-or-env :account-id +account-env+)
                                                 (section-or-env :gateway-id +gateway-env+)))
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun listing-rows ()
  "The roster as a provider listing answers it: the gateway's listing sits
behind the account and the gateway the catalog cannot know, so the picker is
answered from the roster without a request."
  (loop for row across +models+
        collect (list :id (nlk:json-value row :string "id")
                      :display (nlk:json-value row :string "name")
                      :context-window (nlk:json-value row :integer "context"))))

(defun env-key ()
  "The first token one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))
