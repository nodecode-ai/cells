;;;; cell.lisp --- the cell: Crowbot among the organism's providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Three hooks, each declining for every provider but crowbot, and one
;;;; command:
;;;;
;;;;   MODELS-CATALOG-TABLE   the catalog carries Crowbot's row: the chat
;;;;                          lane's package, this section's base, the key
;;;;                          variables, and the models the original names
;;;;                          (with the relay's whole reasoning ladder) plus
;;;;                          the ones /crowbot last read, so /connect offers
;;;;                          it, /models lists its models and a turn
;;;;                          resolves its lane and address the way it does
;;;;                          any catalog provider's
;;;;   :CREDENTIAL            an account number from CROWBOT_API_KEY, else the
;;;;                          variable a Hermes install already holds, and no
;;;;                          other; a key /connect saved answers before this
;;;;                          point does
;;;;   REQUEST-BODY           a round whose operator picked no effort is sent
;;;;                          the section's default_effort (medium unless the
;;;;                          section says otherwise), never the relay's own
;;;;                          ceiling; a pick, `off' included, is left alone
;;;;
;;;;   /crowbot [TEXT]        read GET /models and list the ids, so /models
;;;;                          offers what the account can reach
;;;;
;;;; Config, a sibling top-level key:
;;;;   "crowbot": {"base_url": "https://api.crowbot.sh/v1",
;;;;               "context_tokens": 262144, "default_effort": "medium"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-crowbot)

(defun ours-p (config)
  "Whether the frozen provider CONFIG is a crowbot round."
  (and config (equal (nle::effective-provider-config-provider config) +provider+)))

(defvar *catalog* (cons nil nil)
  "(BASE . MERGED): the last catalog the core answered, and that catalog with
Crowbot's row in it.")

(defun catalog (next &aux (base (funcall next)) (memo *catalog*))
  "MODELS-CATALOG-TABLE advice: the catalog with Crowbot's row, made once per
catalog the core reads."
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
  "The :CREDENTIAL answer for crowbot: the account number CROWBOT_API_KEY
holds, else the one a Hermes install keeps, else none."
  ;; Never NEXT for crowbot: the ladder behind this point falls back to the
  ;; chat family's default variable, and would send OPENAI_API_KEY to Crowbot.
  (if (equal (getf op :provider) +provider+)
      (alexandria:if-let (key (or (env-key) (nle::provider-env-key +provider+)))
        (nle:make-credential key :env)
        (nle:make-credential "public" :public))
      (funcall next op)))

(defun body (next context &aux (body (funcall next context))
                                (config (nle::compiled-turn-context-provider-config context)))
  "REQUEST-BODY advice: a crowbot round the operator gave no effort to is sent
the section's default_effort."
  ;; The original pins medium because the relay reads no effort as its
  ;; ceiling; a pick the operator did make -- `off' included -- is theirs, and
  ;; the chat lane has already spelled it (or left it off for `off').
  (when (and (ours-p config)
             (hash-table-p body)
             (null (nle::effective-provider-config-reasoning-effort config))
             (not (equal (setting :default-effort) "endpoint")))
    (setf (gethash "reasoning_effort" body) (setting :default-effort)))
  body)

(defun forget-catalog ()
  "Drop the merged catalog, so a restart with another base, or a /crowbot read
that found new models, builds a new one."
  (setf *catalog* (cons nil nil)))

(nle:define-cell crowbot
  (:section ("crowbot")
    (:guide "make an account at https://crowbot.sh, find the 16-digit account number, and save it with /connect or set CROWBOT_API_KEY; /crowbot [TEXT] reads the relay's model list into /models; default_effort is what a round is sent when you pick no effort, since the relay would otherwise spend its own ceiling")
    ("base_url" :string :default +base+
     :doc "the Crowbot API base the chat lane appends /chat/completions to")
    ("context_tokens" :integer :default 262144 :min 1
     :doc "the window /models reports for a model, since the relay's model list names none")
    ("default_effort" :choice :options '("endpoint" "minimal" "low" "medium" "high" "xhigh" "max") :default "medium"
     :doc "the reasoning effort a round is sent when no effort is picked: a rung, or endpoint to leave it to the relay"))
  (:start (lambda () (forget-catalog) (nle:on-stop #'forget-catalog)))
  (:hook 'nle::models-catalog-table #'catalog)
  (:hook :credential #'credential)
  (:hook 'nle::request-body #'body)
  (:command "crowbot" #'run-command
            :description "Crowbot: read the relay's model list"
            :argument-hint "TEXT"))
