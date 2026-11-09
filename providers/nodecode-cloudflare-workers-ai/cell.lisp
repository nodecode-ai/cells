;;;; cell.lisp --- the cell: Cloudflare Workers AI among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Four hooks, each declining for every provider but cloudflare-workers-ai:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Workers AI's row: the chat
;;;;                           lane's package, this section's base, and the
;;;;                           built-in models over whatever models.dev
;;;;                           published, so /connect offers it, /models lists
;;;;                           its models and a turn resolves its lane and
;;;;                           address the way it does any catalog provider's
;;;;   LIST-PROVIDER-MODELS   the listing is the roster: Cloudflare's
;;;;                           OpenAI-compatible /models answers HTTP 405
;;;;   :CREDENTIAL            a token from CLOUDFLARE_WORKERS_AI_API_TOKEN,
;;;;                           and no other variable; a token /connect saved
;;;;                           in auth.json answers before this point does
;;;;   REQUEST-BODY            the round carries the one reasoning_effort value
;;;;                           its model takes, or none at all
;;;;
;;;; Config, a sibling top-level key:
;;;;   "cloudflare-workers-ai": {"account_id": "...", "base_url": ""}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-cloudflare-workers-ai)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a cloudflare-workers-ai round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Workers AI's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Workers AI's row, made once
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
  "LIST-PROVIDER-MODELS advice: Workers AI's listing is its roster. Asked
with KEY, which only /connect's key check does, it says why the token was not
checked."
  ;; The roster asks Cloudflare nothing, and the check reads a NIL second value
  ;; as a token the endpoint took: any token reads `works'.
  (if (equal provider +provider+)
      (values (listing-rows)
              (and key "Cloudflare's OpenAI-compatible /models answers HTTP 405, so the first turn tries the token"))
      (apply next provider keys)))

(defun credential (op next)
  "The :CREDENTIAL answer for cloudflare-workers-ai: the token
CLOUDFLARE_WORKERS_AI_API_TOKEN holds, else none."
  ;; Never NEXT for this provider: the ladder behind this point falls back to
  ;; the chat family's default variable, and would send OPENAI_API_KEY to
  ;; Cloudflare.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a Workers AI round sends the reasoning_effort its
model takes, and none for a model that takes no field."
  (when (and (ours-p config) (hash-table-p body))
    (shape-effort body
                  (nle::effective-provider-config-model config)
                  (nle::effective-provider-config-reasoning-effort config)))
  body)

(defun forget ()
  "Drop the merged catalog, so a restart with other settings builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell cloudflare-workers-ai
  (:section ("cloudflare-workers-ai")
    (:guide "make a Workers AI API token in the Cloudflare dashboard (AI > Workers AI > Use REST API); save it with /connect or set CLOUDFLARE_WORKERS_AI_API_TOKEN; account_id is the account the token belongs to (else CLOUDFLARE_ACCOUNT_ID); base_url overrides the account's address")
    ("account_id" :string :doc "the Cloudflare account id the Workers AI token belongs to (else CLOUDFLARE_ACCOUNT_ID)")
    ("base_url" :string :default ""
     :doc "the OpenAI-compatible base to use instead of the account's own, https://api.cloudflare.com/client/v4/accounts/<account>/ai/v1 (else CLOUDFLARE_WORKERS_AI_BASE_URL)"))
  (:start (lambda () (forget) (nle:on-stop #'forget)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook 'nle::list-provider-models #'listing)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body))
