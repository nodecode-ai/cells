;;;; cell.lisp --- the cell: GitLab Duo's chat models among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Eight hooks, each declining for every provider but gitlab-duo, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE    the catalog carries GitLab Duo's row: omp's
;;;;                           Duo models, the Anthropic proxy as its base
;;;;   :CREDENTIAL             the signed-in GitLab token, refreshed before it
;;;;                           expires, else GITLAB_TOKEN
;;;;   RESOLVE-MODEL-LANE      a Claude model rides the anthropic lane, a GPT
;;;;                           codex model the Responses lane, the rest the
;;;;                           chat lane: omp's routes
;;;;   LANE-ENDPOINT           each lane's address is its gateway proxy
;;;;   ANTHROPIC-REQUEST-BODY  the upstream model, thinking on a budget
;;;;   REQUEST-BODY            the upstream model, no sampling for a GPT-5 front
;;;;   RESPONSES-REQUEST-BODY  the same, on the Responses wire
;;;;   WALK-PROVIDER-STREAM    the GitLab token traded for the direct-access
;;;;                           grant, which the round sends instead
;;;;
;;;;   /gitlab-duo login | code TEXT | logout | status        (signin.lisp)
;;;;
;;;; Config, a sibling top-level key:
;;;;   "gitlab-duo": {"gitlab_url": "https://gitlab.com",
;;;;                  "gateway_url": "https://cloud.gitlab.com"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-gitlab-duo)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a gitlab-duo round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
GitLab Duo's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with GitLab Duo's row, made once
per catalog the core reads."
  (if (and (car memo) (eq (car memo) base))
      (cdr memo)
      (let ((merged (make-hash-table :test 'equal)))
        (when (hash-table-p base)
          (maphash (lambda (id provider) (setf (gethash id merged) provider)) base))
        (setf (gethash +provider+ merged) (catalog-row))
        (setf *catalog* (cons base merged))
        merged)))

(defun lane (next provider model)
  "RESOLVE-MODEL-LANE advice: the lane omp routes MODEL to, unless the
operator's config names one."
  (if (and (equal provider +provider+) (not (operator-lane-p model)))
      (model-lane model)
      (funcall next provider model)))

(defun endpoint (next provider lane)
  "LANE-ENDPOINT advice: a gitlab-duo lane's address is its gateway proxy."
  (if (equal provider +provider+)
      (let ((lane (or lane (nle::resolve-model-lane provider nle::*model*))))
        (cond ((equal lane "anthropic") (concatenate 'string (proxy lane) "/messages"))
              ((equal lane "openai-responses") (concatenate 'string (proxy lane) "/responses"))
              ((equal lane "openai-completions") (concatenate 'string (proxy lane) "/chat/completions"))
              (t (funcall next provider lane))))
      (funcall next provider lane)))

(defun anthropic-body (next context)
  "ANTHROPIC-REQUEST-BODY advice: a Duo round asks for the upstream model
and thinks on a budget."
  (let ((values (multiple-value-list (funcall next context)))
        (config (nle::compiled-turn-context-provider-config context)))
    (when (and (ours-p config) (hash-table-p (first values)))
      (let ((model (nle::effective-provider-config-model config)))
        (budget-thinking (upstream-body (first values) model) model)))
    (values-list values)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY and RESPONSES-REQUEST-BODY advice: a Duo round asks for the
upstream model."
  (when (and (ours-p config) (hash-table-p body))
    (upstream-body body (nle::effective-provider-config-model config)))
  body)

(defun walk (next fold &rest keys &key config headers &allow-other-keys)
  "WALK-PROVIDER-STREAM advice: a Duo round sends the direct-access grant
its GitLab token buys, in place of the token."
  (if (ours-p config)
      (apply next fold :headers (gateway-headers headers (nle::effective-provider-config-api-key config))
             (alexandria:remove-from-plist keys :headers))
      (apply next fold keys)))

(defun forget ()
  "Drop the merged catalog and every direct-access grant."
  (setf *catalog* (cons nil nil))
  (clear-direct-access))

(defun start ()
  "Start clean, and leave no sign-in listening behind a stop."
  (forget)
  (nle:on-stop #'forget)
  (nle:on-stop #'cancel-flow))

(nle:define-cell gitlab-duo
  (:section ("gitlab-duo")
    (:guide "sign in with /gitlab-duo login, or set GITLAB_TOKEN to a personal access token with the api scope; Duo must be enabled for the account")
    ("gitlab_url" :string :default +gitlab-url+
     :doc "the GitLab instance the sign-in and the Duo direct-access exchange go to")
    ("gateway_url" :string :default +gateway-url+
     :doc "GitLab's AI gateway, whose Anthropic and OpenAI proxies serve the models"))
  (:start #'start)
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::resolve-model-lane #'lane)
  (:hook 'nle::lane-endpoint #'endpoint)
  (:hook 'nle::anthropic-request-body #'anthropic-body)
  (:hook 'nle::request-body #'body)
  (:hook 'nle::responses-request-body #'body)
  (:hook 'nle::walk-provider-stream #'walk)
  (:command "gitlab-duo" #'run-command
            :description "GitLab Duo (non-agentic chat): sign in with GitLab"
            :argument-hint "login | code <address or code> | logout | status"))
