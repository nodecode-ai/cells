;;;; cell.lisp --- the cell: Mistral Vibe among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but mistral-vibe, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Mistral Vibe's row: the
;;;;                          chat lane's package, this section's base, the
;;;;                          key variable and the rows provider.lisp
;;;;                          publishes, so /connect offers it, /models lists
;;;;                          the fallback and GLM models, and a turn resolves
;;;;                          its lane and address the way it does any catalog
;;;;                          provider's. The account's own listing comes from
;;;;                          GET /models, which /models already asks.
;;;;   :CREDENTIAL            MISTRAL_VIBE_API_KEY and no other variable: the
;;;;                          ladder behind this point would send a plain
;;;;                          MISTRAL_API_KEY or the chat family's default.
;;;;   REQUEST-BODY           a mistral-vibe round sends `reasoning_effort' as
;;;;                          the relay's per-model vocabulary takes it
;;;;                          (provider.lisp's SHAPE-EFFORT).
;;;;
;;;;   /mistral-vibe login | status | logout              (signin.lisp)
;;;;
;;;; Config, a sibling top-level key:
;;;;   "mistral-vibe": {"base_url": "https://api.mistral.ai/v1"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-mistral-vibe)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a mistral-vibe round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Mistral Vibe's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Mistral Vibe's row, made
once per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged)
              (catalog-row (and (hash-table-p base) (gethash +provider+ base))))
        (setf *catalog* (cons base merged))
        merged)))

(defun credential (op next)
  "The :CREDENTIAL answer for mistral-vibe: the key MISTRAL_VIBE_API_KEY
holds, else none; a key /connect or the sign-in saved in auth.json answers
before this point does."
  ;; Never NEXT for mistral-vibe: the ladder behind this point falls back to
  ;; the SDK family's default variable, and would send OPENAI_API_KEY, or a
  ;; plain MISTRAL_API_KEY through the core's own name, to the relay.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a mistral-vibe round spells its effort the relay's way."
  (when (and (ours-p config) (hash-table-p body))
    (shape-effort body (nle::effective-provider-config-model config)))
  body)

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(defparameter +manual+
  "Mistral Vibe on api.mistral.ai: the provider `mistral-vibe' signs in with
the console's browser flow (/mistral-vibe login) or reads MISTRAL_VIBE_API_KEY,
then serves the account's chat models on the OpenAI wire at
https://api.mistral.ai/v1. Models come from the account's own listing; the
fallback is mistral-vibe-cli-latest. Reasoning effort is the relay's
per-model vocabulary: GLM-5.3, 5 and latest take low/high/max, GLM-5.2 adds
medium, a stronger pick clamps down to one of them, and any other model is
sent none. Billing is not guaranteed to be covered by a Vibe plan.")

(defun start ()
  "Start clean: one catalog, no sign-in left behind a stop."
  (forget-catalog)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop #'cancel-sign-in))

(nle:define-cell mistral-vibe
  (:section ("mistral-vibe")
    (:guide "sign in with /mistral-vibe login, or save a key with /connect or MISTRAL_VIBE_API_KEY; base_url is where the relay is served")
    ("base_url" :string :default +base+
     :doc "the relay's API base, the one the chat lane appends /chat/completions to"))
  (:help :mistral-vibe "mistral-vibe: Mistral Vibe models on api.mistral.ai - /mistral-vibe login" +manual+)
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body)
  (:command "mistral-vibe" #'run-command
            :description "Mistral Vibe: sign in from the browser, or its key's state"))
