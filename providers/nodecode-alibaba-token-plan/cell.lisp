;;;; cell.lisp --- the cell: the QwenCloud Token Plan among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but alibaba-token-plan:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries the Token Plan's row: the
;;;;                          chat lane's package, the base of the section's
;;;;                          region (or its base_url), and omp's seed models
;;;;                          over whatever models.dev published, so /connect
;;;;                          offers it, /models lists its models and a turn
;;;;                          resolves its lane and address the way it does
;;;;                          any catalog provider's
;;;;   :CREDENTIAL            a key from ALIBABA_TOKEN_PLAN_API_KEY or
;;;;                          BAILIAN_TOKEN_PLAN_API_KEY; a key /connect saved
;;;;                          in auth.json answers before this point does
;;;;   REQUEST-BODY           a reasoning model is asked to think in its dialect
;;;;
;;;; Config, a sibling top-level key:
;;;;   "alibaba-token-plan": {"region": "international"}
;;;; "region" is international (Singapore) or china (Beijing); "base_url",
;;;; when set, is a custom endpoint and wins over the region. A vetoed section
;;;; ("enabled": false) installs nothing.

(in-package #:nodecode-alibaba-token-plan)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a Token Plan round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
the Token Plan's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with the Token Plan's row, made
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
  "The :CREDENTIAL answer for alibaba-token-plan: the key one of its
variables holds, else none."
  ;; Never NEXT for this provider: the ladder behind this point falls back to
  ;; the chat family's default variable, and omp refuses exactly that here --
  ;; OPENAI_API_KEY matches the sk-* grammar and would be sent to QwenCloud
  ;; as a bearer.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a Token Plan round asks a reasoning model to think in
its dialect."
  (when (and (ours-p config) (hash-table-p body))
    (shape-thinking body
                    (nle::effective-provider-config-model config)
                    (nle::effective-provider-config-reasoning-effort config)))
  body)

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another region builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell alibaba-token-plan
  (:section ("alibaba-token-plan")
    (:guide "pick the region your Token Plan was bought in: international, Singapore (subscribe and copy its key at https://home.qwencloud.com/billing/subscription/token-plan-individual) or china, Beijing (https://www.aliyun.com/benefit/scene/tokenplan), the keys do not cross; save the key with /connect or set ALIBABA_TOKEN_PLAN_API_KEY; base_url is a custom endpoint and wins over region")
    ("region" :choice :options +region-names+ :default "international"
     :doc "where the plan is served: international (token-plan.ap-southeast-1.maas.aliyuncs.com) or china (token-plan.cn-beijing.maas.aliyuncs.com)")
    ("base_url" :string
     :doc "a custom endpoint the chat lane appends /chat/completions to; set, it wins over region"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body))
