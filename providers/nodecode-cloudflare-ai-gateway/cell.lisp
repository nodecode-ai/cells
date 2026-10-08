;;;; cell.lisp --- the cell: Cloudflare AI Gateway among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Seven hooks, each declining for every provider but cloudflare-ai-gateway:
;;;;
;;;;   MODELS-CATALOG-TABLE     the catalog carries the gateway's row: omp's
;;;;                            bundled models over whatever models.dev
;;;;                            published, so /connect offers it and /models
;;;;                            lists them
;;;;   LIST-PROVIDER-MODELS     the listing is the roster: the gateway's own
;;;;                            sits behind an account and a gateway id
;;;;   RESOLVE-MODEL-LANE       each model rides its route's wire (Messages
;;;;                            or chat), unless the operator's config pins one
;;;;   :CREDENTIAL              a token from CLOUDFLARE_AI_GATEWAY_API_KEY, and
;;;;                            no other variable; a token /connect saved in
;;;;                            auth.json answers before this point does
;;;;   REQUEST-BODY             a chat round names the route's model id
;;;;   ANTHROPIC-REQUEST-BODY   a Messages round names the route's model id
;;;;   WALK-PROVIDER-STREAM     the round goes to its route under the account
;;;;                            and gateway, authenticated to the gateway alone
;;;;
;;;; omp's login is `custom': it asks for the token, the account id and the
;;;; gateway id, and stores the three as one credential. Here the token is the
;;;; secret /connect saves (or the variable), and the two ids are the
;;;; section's settings, which are not secret. A credential saved as omp's
;;;; JSON ({"token", "accountId", "gatewayId"}) is read too, its ids first.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "cloudflare-ai-gateway": {"account_id": "...", "gateway_id": "default"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-cloudflare-ai-gateway)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a cloudflare-ai-gateway round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
the gateway's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with the gateway's row, made once
per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun listing (next provider &rest keys &key key &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: the gateway's listing is its roster. Asked
with KEY, which only /connect's key check does, it says why the token was
not checked."
  ;; The roster asks the gateway nothing, and the check reads a NIL second
  ;; value as a token the gateway took: any token read `works'.
  (if (equal provider +provider+)
      (values (listing-rows)
              (and key "the gateway's model list sits behind the account and gateway ids, so the first turn tries the token"))
      (apply next provider keys)))

(defun pinned-p (provider model)
  "Whether the operator's config pins the wire PROVIDER's MODEL rides."
  (or (nle::trimmed-config-string (nle::configured-model-entry provider model) "sdk")
      (nle::trimmed-config-string (nle::configured-provider-entry provider) "sdk")))

(defun model-lane (next provider model)
  "RESOLVE-MODEL-LANE advice: a gateway model rides its route's lane."
  (if (and (equal provider +provider+) (not (pinned-p provider model)))
      (values (route model))
      (funcall next provider model)))

(defun credential (op next)
  "The :CREDENTIAL answer for cloudflare-ai-gateway: the token
CLOUDFLARE_AI_GATEWAY_API_KEY holds, else none."
  ;; Never NEXT for this provider: the ladder behind this point falls back to
  ;; the lane family's default variable, and would send ANTHROPIC_API_KEY to
  ;; the gateway.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun config-of (context)
  (nle::compiled-turn-context-provider-config context))

(defun chat (next context &aux (body (funcall next context)) (config (config-of context)))
  "REQUEST-BODY advice: a gateway chat round names its route's model id, its
cap as max_completion_tokens and no cache key, as omp's chat compat does for
a host that is not OpenAI's own."
  (when (and (ours-p config) (hash-table-p body))
    (setf (gethash "model" body) (wire-id (nle::effective-provider-config-model config)
                                          (nle::effective-provider-config-lane config)))
    (multiple-value-bind (cap present) (gethash "max_tokens" body)
      (when present
        (remhash "max_tokens" body)
        (setf (gethash "max_completion_tokens" body) cap)))
    (remhash "prompt_cache_key" body))
  body)

(defun messages (next context)
  "ANTHROPIC-REQUEST-BODY advice: a gateway Messages round names its route's
model id. The lane answers the body and its betas; both pass through."
  (let ((answer (multiple-value-list (funcall next context)))
        (config (config-of context)))
    (when (and (ours-p config) (hash-table-p (first answer)))
      (setf (gethash "model" (first answer))
            (wire-id (nle::effective-provider-config-model config)
                     (nle::effective-provider-config-lane config))))
    (values-list answer)))

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a gateway round goes to its route under the
account and gateway, with the gateway's authorization in place of the lane's."
  (if (ours-p config)
      (multiple-value-bind (token root) (round-route (nle::effective-provider-config-api-key config))
        (apply next fold
               :headers (round-headers headers token)
               :endpoint (round-endpoint root (nle::effective-provider-config-model config)
                                         (nle::effective-provider-config-lane config))
               (alexandria:remove-from-plist keys :headers :endpoint)))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with other settings builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell cloudflare-ai-gateway
  (:section ("cloudflare-ai-gateway")
    (:guide "make an AI Gateway token with Run permission (https://developers.cloudflare.com/ai-gateway/configuration/authentication/); save it with /connect or set CLOUDFLARE_AI_GATEWAY_API_KEY; account_id and gateway_id name the gateway (else CLOUDFLARE_ACCOUNT_ID and CLOUDFLARE_GATEWAY_ID)")
    ("account_id" :string :doc "the Cloudflare account id, 32 characters (else CLOUDFLARE_ACCOUNT_ID)")
    ("gateway_id" :string :doc "the AI Gateway id, `default' for the account's default gateway (else CLOUDFLARE_GATEWAY_ID)")
    ("base_url" :string :default +base+
     :doc "the gateway's root, before /<account>/<gateway>"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::list-provider-models #'listing)
  (:hook 'nle::resolve-model-lane #'model-lane)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'chat)
  (:hook 'nle::anthropic-request-body #'messages)
  (:hook 'nle::walk-provider-stream #'walk))
