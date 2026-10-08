;;;; provider.lisp --- what Xiaomi MiMo is: its endpoints, its keys, its models, its wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): packages/catalog/src/compat/rules/
;;;; auth/xiaomi.kdl and providers/xiaomi.kdl, ai/src/registry/oauth/
;;;; xiaomi.ts (the two kinds of key and the Token Plan clusters' order),
;;;; catalog/src/provider-models/openai-compat.ts (a Token Plan key's
;;;; discovery), ai/src/providers/openai-shared.ts (the zai thinking
;;;; dialect), and the bundled rows of catalog/src/models.json, which
;;;; models.json in this folder carries (tools/omp-models.py wrote it).
;;;;
;;;; Xiaomi MiMo speaks the OpenAI chat wire with a bearer key, from one of
;;;; two places. A pay-as-you-go key (sk-...) is served at
;;;; https://api.xiaomimimo.com/v1. A Token Plan key (tp-...) is served only
;;;; by the plan's regional clusters: Singapore, Amsterdam, China. omp's
;;;; login takes either key and, for a tp- key, tries the clusters in that
;;;; order; its discovery does the same and the models it finds carry the
;;;; cluster that answered, so a turn goes there. This cell keeps that rule:
;;;; a tp- key is sent to the cluster the section's token_plan_region names,
;;;; or, at "auto", to the first of the three whose /models takes the key.
;;;; MiMo models think in the zai dialect: thinking {type: enabled|disabled},
;;;; and no reasoning_effort, which MiMo does not take.

(in-package #:nodecode-xiaomi)

(defparameter +base+ "https://api.xiaomimimo.com/v1"
  "Where a pay-as-you-go key is served: the base the chat lane appends
/chat/completions to.")

(defparameter +clusters+
  '(("sgp" . "https://token-plan-sgp.xiaomimimo.com/v1")
    ("ams" . "https://token-plan-ams.xiaomimimo.com/v1")
    ("cn" . "https://token-plan-cn.xiaomimimo.com/v1"))
  "The Token Plan's regional clusters, in the order omp tries them.")

(defparameter +regions+ (cons "auto" (mapcar #'car +clusters+))
  "What the section's token_plan_region may name.")

(defparameter +env+ '("XIAOMI_API_KEY")
  "The environment variables a Xiaomi key is read from, in order.")

(defparameter +probe-seconds+ 15
  "How long one cluster may take to answer whether it takes a key (omp's
VALIDATION_TIMEOUT_MS, fresh for each cluster).")

;;; --- the models ----------------------------------------------------------------

(defparameter +models+
  (nlk:decode-json
   (uiop:read-file-string
    (asdf:system-relative-pathname "nodecode-xiaomi" "models.json")))
  "omp's bundled Xiaomi rows, read when this file loads: a vector of objects.")

(defun model-row (model-id)
  "The bundled row of MODEL-ID, or NIL."
  (find model-id +models+ :key (lambda (row) (nlk:json-value row :string "id")) :test #'equal))

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
       ;; CATALOG-PRICE's shape: the pay-as-you-go price
       (let ((input (nlk:json-value cost :number "input"))
             (output (nlk:json-value cost :number "output")))
         (and input output (or (plusp input) (plusp output))
              (list input output
                    (nlk:json-value cost :number "cacheRead")
                    (nlk:json-value cost :number "cacheWrite"))))))))

(defun catalog-row (&optional prior)
  "Xiaomi MiMo as a models.dev provider: the chat lane's package, this
section's base, the key variable, and the bundled models over PRIOR's (the
row models.dev itself published, when it did)."
  (let ((models (make-hash-table :test 'equal)))
    (alexandria:when-let (published (nlk:json-value prior :object "models"))
      (maphash (lambda (id model) (setf (gethash id models) model)) published))
    (loop for row across +models+
          do (setf (gethash (nlk:json-value row :string "id") models) (catalog-model row)))
    (nlk:json-object "name" "Xiaomi MiMo"
                     "npm" "@ai-sdk/openai-compatible"
                     "api" (setting :base-url)
                     "env" (coerce +env+ 'vector)
                     "models" models)))

(defun env-key ()
  "The first key one of +ENV+ holds, or NIL."
  (some #'nle::credential-env +env+))

;;; --- a Token Plan key's cluster ------------------------------------------------------

(defun token-plan-key-p (key)
  "Whether KEY is a Token Plan key, which only the plan's clusters serve."
  (and (stringp key) (uiop:string-prefix-p "tp-" key)))

(defvar *found* (make-hash-table :test 'equal :synchronized t)
  "A Token Plan key -> the base of the cluster that took it, found at \"auto\"
by a listing or a round, for as long as this process runs.")

(defun pinned-cluster ()
  "The base of the cluster the section names, or NIL at \"auto\"."
  (cdr (assoc (setting :token-plan-region) +clusters+ :test #'string=)))

(defun clusters-to-try (key)
  "The bases a Token Plan KEY may be served at, in the order to try them: the
pinned cluster alone, the one found for KEY before, else all three."
  (let ((known (or (pinned-cluster) (gethash key *found*))))
    (if known (list known) (mapcar #'cdr +clusters+))))

(defun remember (key base)
  "Note that the cluster at BASE took KEY, unless the section pins one."
  (unless (pinned-cluster)
    (setf (gethash key *found*) base)))

(defun takes-key-p (base key)
  "Whether the cluster at BASE answers GET /models with KEY as its bearer."
  ;; Any failure -- a refusal, a timeout, a dead cluster -- is a no, as omp's
  ;; discovery reads it: the next cluster is asked.
  (handler-case
      (sb-sys:with-deadline (:seconds +probe-seconds+)
        (let ((status (nth-value 1 (dex:get (format nil "~a/models" base)
                                            :headers `(("authorization" . ,(format nil "Bearer ~a" key)))
                                            :connect-timeout +probe-seconds+
                                            :read-timeout +probe-seconds+
                                            :use-connection-pool nil))))
          (and (integerp status) (<= 200 status 299))))
    ((or error sb-sys:deadline-timeout) () nil)))

(defun token-plan-base (key)
  "The base a round on the Token Plan KEY goes to: the first cluster to try
that takes it, else the first of them, whose refusal the round then shows."
  (let ((bases (clusters-to-try key)))
    (if (rest bases)
        (let ((base (find-if (lambda (base) (takes-key-p base key)) bases)))
          (when base (remember key base))
          (or base (first bases)))
        (first bases))))

;;; --- the zai thinking dialect -----------------------------------------------------
;;; omp's rule for the MiMo family is thinking-format "zai": thinking
;;; {type: enabled} turns a reasoning model's thinking on and {type: disabled}
;;; off, and reasoning_effort is never sent (MiMo does not support it).

(defun shape-thinking (body model effort)
  "BODY, the chat request for MODEL at EFFORT, in the zai dialect."
  ;; Only a reasoning model is told anything, as in omp. An unset effort is
  ;; the provider's own default, as the chat lane leaves it; "off" disables,
  ;; any rung enables.
  (when (and effort (nlk:json-value (model-row model) :boolean "reasoning"))
    (remhash "reasoning_effort" body)
    (setf (gethash "thinking" body)
          (nlk:json-object "type" (if (string-equal effort "off") "disabled" "enabled"))))
  body)
