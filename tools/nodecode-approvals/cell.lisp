;;;; cell.lisp --- the manual, the settings, /approvals, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A command review is a judgement, not a conversation, so it is no lane:
;;;; it is the verbs in the approvals: package the model calls through eval,
;;;; the way it reaches the web through web:search. While the cell runs,
;;;; (help :approvals) answers the manual, every request's help section
;;;; carries one line naming the verbs, and /approvals reviews what is typed
;;;; after it.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "approvals": {"base_url": "", "model": "", "key_env": "", "audit": true}
;;;; Empty means the public TypeSafe root and jev-latest; the key is the
;;;; variable key_env names, else auth.json's api_keys.typesafe, else
;;;; TYPESAFE_API_KEY. An openrouter.ai base_url takes its own route and key.

(in-package #:nodecode-approvals)

(defparameter +manual+
  "Approvals reviews one shell command before it runs: APPROVE, DENY or ESCALATE, from TypeSafe's
Jev decision model, with typed probabilities instead of prose. The command text leaves the machine,
redacted (passwords, tokens, cookie and URL credentials) and capped at 4000 characters.
  (approvals:review \"rm -rf build/\" &key policy flagged-as)
      Asks one request of the decision endpoint. POLICY is the operator's trusted policy text, when
      there is one; FLAGGED-AS is the line a gate flagged the command as. Answers a plist:
        (:VERDICT \"APPROVE\" :RULE \"model_verdict\" :REASON \"model verdict (conf 0.82)\"
         :CONFIDENCE 0.82 :BLAST-RADIUS 0.4 :SELF-ADVOCATING 0.01 :POLICY-ALLOWS 0.0
         :READS-SECRETS 0.0 :SENDS-OUTBOUND 0.0 :TRUNCATED NIL :HAS-POLICY NIL :MODEL \"jev-latest\")
      RULE names the rule that decided: model_verdict, low_confidence, high_blast, self_advocating,
      secrets_read, secrets_exfil, policy_allow, truncated or invalid_verdict. A DENY means the model
      would refuse; an ESCALATE means a person should look. Each decision is appended to
      approvals-decisions.jsonl in the nodecode home, unless the section turns audit off.
  (approvals:decide :verdict \"APPROVE\" :confidence 0.9 :blast-radius 0.2 &key self-advocating
                    policy-allows reads-secrets sends-outbound has-policy truncated)
      The rule chain alone, on answers you already hold: no network, no key. => (:VERDICT :RULE :REASON).
      Use it to replay or test a policy: an APPROVE under 0.55 confidence escalates, a blast radius of
      1.6 or more escalates, and a command that both reads secrets and sends outbound is denied.
  (approvals:decisions &key limit)   the last decisions this machine logged, oldest first.
ERROR: APPROVALS-ERROR names the problem: no key, an unreachable endpoint, or an answer that does not
keep the contract (a missing probability, a distribution that is not the one asked)."
  "What (help :approvals) answers while the cell runs.")

;;; --- /approvals -------------------------------------------------------------------

(defun status ()
  "Where the key comes from, the endpoint and the model, in a line; no network."
  (multiple-value-bind (endpoint kind) (route)
    (multiple-value-bind (key source) (api-key kind)
      (format nil "approvals: ~a; ~a, model ~a, policy ~a"
              (cond ((and key source) (format nil "key from ~a" source))
                    ((eq kind :custom) "no key, none sent")
                    (t (format nil "no key (make one at ~a, then set ~a, or save api_keys.typesafe in auth.json)"
                               +key-page+ +typesafe-env+)))
              endpoint (model) +policy-version+))))

(defun verdict-line (result)
  "One line a person reads: the verdict, the rule and why."
  (format nil "~a [~a]: ~a" (getf result :verdict) (getf result :rule) (getf result :reason)))

(defun panel (summary)
  "The dialog bare /approvals answers: SUMMARY over one row that asks for a command."
  (nle:list-dialog
   :title "Approvals" :summary summary
   :rows (list (list :label "Review a command"
                     :ask (list :label "Command" :line "/approvals"
                                :placeholder "rm -rf build/" :verb "review")
                     :detail "the command is redacted and capped before it leaves this machine"))))

(defun run-slash (args session-id)
  "/approvals typed alone: where the key comes from, and its panel. With a
command after it, that command reviewed and the verdict said in a line."
  (declare (ignore session-id))
  (let ((typed (nlk:trimmed (or args ""))))
    (if (zerop (length typed))
        (let ((text (status))) (values text (panel text)))
        (verdict-line (review typed)))))

(nle:define-cell approvals
  (:section ("approvals")
    (:guide "make a key at https://console.typesafe.ai/ and set TYPESAFE_API_KEY, or name another variable in key_env; base_url and model are optional; a base_url of your own may take no key")
    ("base_url" :string :default ""
     :doc "the decision API root; empty is https://api.typesafe.ai/v1, and an openrouter.ai base selects its own route and model")
    ("model" :string :default ""
     :doc "the decision model; empty is jev-latest, or ~typesafe/jev-latest on the openrouter.ai route")
    ("key_env" :env
     :doc "the environment variable holding the key; empty reads TYPESAFE_API_KEY, or OPENROUTER_API_KEY on the openrouter.ai route")
    ("timeout" :integer :default 20 :min 5
     :doc "seconds one attempt may take")
    ("audit" :boolean :default t
     :doc "append every decision to approvals-decisions.jsonl in the nodecode home"))
  (:help :approvals "approvals:review \"<shell command>\" answers APPROVE, DENY or ESCALATE for it, from TypeSafe's Jev decision model; approvals:decide replays the rule chain on answers you hold" +manual+)
  (:command "approvals" 'run-slash
   :description "Review a shell command: APPROVE, DENY or ESCALATE"
   :argument-hint "COMMAND"
   :session nil))
