;;;; cell.lisp --- the cell: Xiaomi MiMo among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Five hooks, each declining for every provider but xiaomi:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Xiaomi MiMo's row: the chat
;;;;                          lane's package, this section's base, and omp's
;;;;                          bundled models over whatever models.dev
;;;;                          published, so /connect offers it, /models lists
;;;;                          its models and a turn resolves its lane and
;;;;                          address the way it does any catalog provider's
;;;;   :CREDENTIAL            a key from XIAOMI_API_KEY; a key /connect saved
;;;;                          in auth.json answers before this point does
;;;;   REQUEST-BODY           a reasoning model is asked to think the zai way
;;;;   WALK-PROVIDER-STREAM   a round on a Token Plan key (tp-...) goes to the
;;;;                          plan's cluster, not the pay-as-you-go base
;;;;   LIST-PROVIDER-MODELS   ... and so does the listing, trying the clusters
;;;;                          in omp's order at "auto"
;;;;
;;;; Config, a sibling top-level key:
;;;;   "xiaomi": {"base_url": "https://api.xiaomimimo.com/v1",
;;;;              "token_plan_region": "auto"}
;;;; A vetoed section ("enabled": false) installs nothing, and Xiaomi is then
;;;; whatever models.dev alone makes of it.

(in-package #:nodecode-xiaomi)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a xiaomi round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Xiaomi MiMo's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Xiaomi MiMo's row, made once
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

(defun credential (op next)
  "The :CREDENTIAL answer for xiaomi: the key XIAOMI_API_KEY holds, else none."
  ;; Never NEXT for xiaomi: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to Xiaomi.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (env-key))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a xiaomi round asks a reasoning model to think in the
zai dialect."
  (when (and (ours-p config) (hash-table-p body))
    (shape-thinking body
                    (nle::effective-provider-config-model config)
                    (nle::effective-provider-config-reasoning-effort config)))
  body)

(defun walk (next fold &rest keys &key config &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a round on a Token Plan key goes to its cluster."
  ;; omp's rule: a tp- key always rides a Token Plan cluster; the standard
  ;; base would only refuse it.
  (let ((key (and (ours-p config) (nle::effective-provider-config-api-key config))))
    (if (token-plan-key-p key)
        (apply next fold
               :endpoint (concatenate 'string (token-plan-base key) "/chat/completions")
               (alexandria:remove-from-plist keys :endpoint))
        (apply next fold keys))))

(defun listing (next provider &rest keys &key key &allow-other-keys)
  "LIST-PROVIDER-MODELS advice: a Token Plan key lists from its cluster, the
clusters tried in omp's order until one answers."
  (let ((key (and (equal provider +provider+)
                  (or key (ignore-errors
                           (nle:credential-key (nle::resolve-provider-credential provider)))))))
    (if (not (token-plan-key-p key))
        (apply next provider keys)
        (let ((last-error nil))
          (dolist (base (clusters-to-try key) (values nil last-error))
            (multiple-value-bind (rows error) (funcall next provider :key key :base base)
              (if rows
                  (progn (remember key base) (return (values rows nil)))
                  (setf last-error error))))))))

(defun forget ()
  "Drop the merged catalog and the clusters found, so a restart with another
section starts clean."
  (setf *catalog* (cons nil nil))
  (clrhash *found*))

(nle:define-cell xiaomi
  (:section ("xiaomi")
    (:guide "make a key at https://platform.xiaomimimo.com/#/console/api-keys (pay-as-you-go, sk-...) or take your Token Plan key (tp-...) from the plan page; save it with /connect or set XIAOMI_API_KEY; base_url is where a pay-as-you-go key is served; token_plan_region pins the cluster a tp- key goes to (sgp, ams, cn), auto tries them in that order")
    ("base_url" :string :default +base+
     :doc "the pay-as-you-go base the chat lane appends /chat/completions to")
    ("token_plan_region" :choice :options +regions+ :default "auto"
     :doc "the Token Plan cluster a tp- key is sent to: sgp, ams or cn; auto tries them in that order and keeps the first that takes the key"))
  (:start (lambda () (forget) (nle:on-stop #'forget)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:hook 'nle::list-provider-models #'listing))
