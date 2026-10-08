;;;; cell.lisp --- the cell: the GLM Coding Plan among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Seven hooks, each declining for every provider but zai-coding-plan, and
;;;; one command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries the plan's row: models.dev's
;;;;                          chat endpoint and models, and omp's zai models
;;;;                          over them, so /connect offers it, /models lists
;;;;                          GLM-5.3 and its kin, and a turn resolves its
;;;;                          lane and address the way it does any catalog
;;;;                          provider's
;;;;   :CREDENTIAL            the key the sign-in minted, else ZAI_API_KEY or
;;;;                          ZAI_CODING_PLAN_API_KEY, else no key: never the
;;;;                          chat family's default variable; a key /connect
;;;;                          saved in auth.json answers before this point does
;;;;   RESOLVE-MODEL-LANE     a model omp sends on the Messages endpoint rides
;;;;                          the anthropic lane, GLM-5.3-Flash the chat lane;
;;;;                          a model omp has no row for keeps the chat lane
;;;;   LANE-ENDPOINT          the anthropic lane's address is the Messages
;;;;                          endpoint, not the chat base
;;;;   ANTHROPIC-REQUEST-BODY thinking on a budget, as the endpoint takes it
;;;;   REQUEST-BODY           Z.AI's thinking switch beside reasoning_effort
;;;;   WALK-PROVIDER-STREAM   a Messages round sends its key as a bearer
;;;;
;;;;   /zai-coding-plan login | code TEXT | logout | status   (signin.lisp)
;;;;
;;;; Config, a sibling top-level key:
;;;;   "zai-coding-plan": {"base_url": "https://api.z.ai/api/coding/paas/v4",
;;;;                       "anthropic_base_url": "https://api.z.ai/api/anthropic/v1"}
;;;; A vetoed section ("enabled": false) installs nothing, and the plan is
;;;; then whatever models.dev alone makes of it.

(in-package #:nodecode-zai-coding-plan)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a zai-coding-plan round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defun on-lane-p (config lane)
  "Whether CONFIG is a zai-coding-plan round on LANE."
  (and (ours-p config) (equal (nle::effective-provider-config-lane config) lane)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
the plan's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with the plan's row, made once
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

(defun lane (next provider model)
  "RESOLVE-MODEL-LANE advice: the lane omp sends MODEL on, unless the
operator's config names one."
  (or (and (equal provider +provider+)
           (not (operator-lane-p model))
           (model-lane model))
      (funcall next provider model)))

(defun endpoint (next provider lane)
  "LANE-ENDPOINT advice: the anthropic lane of the plan is the Messages
endpoint, unless the operator's config names a base_url."
  (if (and (equal provider +provider+)
           (not (operator-base-p))
           (equal "anthropic" (or lane (nle::resolve-model-lane provider nle::*model*))))
      (concatenate 'string (string-right-trim "/" (setting :anthropic-base-url)) "/messages")
      (funcall next provider lane)))

(defun anthropic-body (next context)
  "ANTHROPIC-REQUEST-BODY advice: a plan round thinks on a budget."
  (let ((values (multiple-value-list (funcall next context)))
        (config (nle::compiled-turn-context-provider-config context)))
    (when (and (ours-p config) (hash-table-p (first values)))
      (budget-thinking (first values) (nle::effective-provider-config-model config)))
    (values-list values)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a plan round on the chat lane turns thinking on or off."
  (when (and (ours-p config) (hash-table-p body))
    (chat-thinking body (nle::effective-provider-config-reasoning-effort config)))
  body)

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a plan round on the Messages endpoint sends
its key as a bearer."
  (if (on-lane-p config "anthropic")
      (apply next fold :headers (bearer-headers headers (nle::effective-provider-config-api-key config))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base builds a new one."
  (setf *catalog* (cons nil nil)))

(defun start ()
  "Start clean, and leave no sign-in waiting behind a stop."
  (forget-catalog)
  (nle:on-stop #'forget-catalog)
  (nle:on-stop #'cancel-flow))

(nle:define-cell zai-coding-plan
  (:section ("zai-coding-plan")
    (:guide "sign in with /zai-coding-plan login, or make a key at https://z.ai/manage-apikey/apikey-list and save it with /connect or ZAI_API_KEY; base_url is the chat endpoint, anthropic_base_url the Messages one")
    ("base_url" :string :default +base+
     :doc "Z.AI's chat endpoint, the base the chat lane appends /chat/completions to")
    ("anthropic_base_url" :string :default +anthropic-base+
     :doc "Z.AI's Anthropic Messages endpoint, the base the anthropic lane appends /messages to"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::resolve-model-lane #'lane)
  (:hook 'nle::lane-endpoint #'endpoint)
  (:hook 'nle::anthropic-request-body #'anthropic-body)
  (:hook 'nle::request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "zai-coding-plan" #'run-command
            :description "Z.AI GLM Coding Plan: sign in from the browser"
            :argument-hint "login | code <address or code> | logout | status"))
