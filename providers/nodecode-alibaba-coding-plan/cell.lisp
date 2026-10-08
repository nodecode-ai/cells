;;;; cell.lisp --- the cell: Alibaba Coding Plan among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but alibaba-coding-plan:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries the Coding Plan's row: the
;;;;                          chat lane's package, the base of the section's
;;;;                          region (or its base_url), and omp's bundled
;;;;                          models over whatever models.dev published, so
;;;;                          /connect offers it, /models lists its models and
;;;;                          a turn resolves its lane and address the way it
;;;;                          does any catalog provider's
;;;;   :CREDENTIAL            a key from ALIBABA_CODING_PLAN_API_KEY; a key
;;;;                          /connect saved in auth.json answers before this
;;;;                          point does
;;;;   REQUEST-BODY           a reasoning model is asked to think the Qwen way
;;;;
;;;; Config, a sibling top-level key:
;;;;   "alibaba-coding-plan": {"region": "international"}
;;;; "region" is international or china; "base_url", when set, is a custom
;;;; endpoint and wins over the region. A vetoed section ("enabled": false)
;;;; installs nothing.

(in-package #:nodecode-alibaba-coding-plan)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a Coding Plan round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
the Coding Plan's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with the Coding Plan's row, made
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
  "The :CREDENTIAL answer for alibaba-coding-plan: the key
ALIBABA_CODING_PLAN_API_KEY holds, else none."
  ;; Never NEXT for this provider: the ladder behind this point falls back to
  ;; the chat family's default variable, and would send OPENAI_API_KEY to
  ;; Alibaba.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a Coding Plan round asks a reasoning model to think in
the Qwen dialect."
  (when (and (ours-p config) (hash-table-p body))
    (shape-thinking body
                    (nle::effective-provider-config-model config)
                    (nle::effective-provider-config-reasoning-effort config)))
  body)

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another region builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell alibaba-coding-plan
  (:section ("alibaba-coding-plan")
    (:guide "pick the region your plan was bought in: international (key at https://modelstudio.console.alibabacloud.com/) or china (key at https://bailian.console.aliyun.com/?tab=model#/api-key), the keys do not cross; save the key with /connect or set ALIBABA_CODING_PLAN_API_KEY; base_url is a custom endpoint, a proxy, and wins over region")
    ("region" :choice :options +region-names+ :default "international"
     :doc "where the plan is served: international (coding-intl.dashscope.aliyuncs.com) or china (coding.dashscope.aliyuncs.com)")
    ("base_url" :string
     :doc "a custom endpoint the chat lane appends /chat/completions to; set, it wins over region"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body))
