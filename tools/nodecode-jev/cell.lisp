;;;; cell.lisp --- the manual, the settings, /jev, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Jev answers typed questions, not conversations, so it is no lane and no
;;;; catalog row: it is a handful of functions in the jev: package the model
;;;; calls through eval, the way it reaches the web through web:search. While
;;;; the cell runs, (help :jev) answers the manual, every request's help
;;;; section carries one line naming the verbs, and /jev says what is
;;;; configured without touching the network.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "jev": {"backend": "typesafe", "account_id": "", "gateway_id": "",
;;;;           "model": "", "review_threshold": ""}
;;;; The backend is chosen explicitly, as the original insists; there is no
;;;; fallback from one route to the other. Only the account id (32 hex) enters
;;;; a URL.

(in-package #:nodecode-jev)

(defparameter +manual+
  "Jev answers small, explicit decisions through the nodecode-jev cell: typed questions about one
state, answered as probabilities by TypeSafe's System One, called through eval. It is advisory: it
executes nothing and grants no approval.
  (jev:triage state)          what kind of work is this (bug, feature, research, docs, operations,
                              unknown), is context missing, does it need authorization?
  (jev:next-step state)       one next step from a goal and recent attempts: continue, retry,
                              change_approach, ask_user, escalate, stop; and whether it is stuck
  (jev:relevance state)       how directly an item helps a goal, 0 (unrelated) to 2 (useful)
  (jev:ask state :preset :task-triage)                 the same three, by name
  (jev:ask state :questions '(:id (:type \"choice\" :instructions TEXT :criteria (:a \"rubric\" :b nil))))
      STATE is a string, or a JSON object or array (a hash table, a plist like (:diff \"...\" :goal
      \"...\"), a list): put the goal, the item, the attempt and the observations in it, as text you
      would want a careful reader to see. A question is :type \"choice\" (two or more options, each
      a rubric or nil), \"bool\" (yes/no, criteria optional: (:true TEXT :false TEXT)) or \"score\"
      (two or more levels, lowest first). Give exactly one of :preset and :questions.
      => (values ANSWERS REVIEW BACKEND): ANSWERS a plist of each id's keyword and
        (:type :bool :bool P-YES) | (:type :choice :choice LABEL :probabilities ((LABEL . P) ...)
         :confidence C) | (:type :score :score LEVEL :probabilities ((\"0\" . P) ...) :confidence C);
      REVIEW (:enabled T :threshold N :review-questions (ID ...) :signals (ID (:needs-review T|NIL
         :basis \"...\" :probability-yes P :confidence C) ...)) and is empty when review_threshold is;
      BACKEND the route and model that answered, e.g. \"typesafe/jev-latest\".
      Send minimal, non-secret state. Use it for finite judgments, not generation, arithmetic or
      deep reasoning.
  (jev:presets)                the three rubrics, offline, before you pay for a call
  (jev:status)                 backend, model, where the key comes from, heuristic on or off; no network
ERROR: JEV-ERROR names the problem: a refused question, no key, or the API's own answer. The key
never rides the text."
  "What (help :jev) answers while the cell runs.")

(defun run-slash (args session-id)
  "/jev typed alone: what is configured, offline, and what to call next."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((equal verb "") (status))
          ((equal verb "status") (status))
          ((member verb '("presets" "rubrics") :test #'equal) (presets))
          (t (format nil "jev: ~a is not one of its choices (status, presets)" verb)))))

(nle:define-cell jev
  (:section ("jev")
    (:guide "pick the backend you hold a key for: typesafe (make a key at https://console.typesafe.ai/) or cloudflare (the 32-hex account id, a Workers AI token with jev on an AI Gateway); set TYPESAFE_API_KEY or CLOUDFLARE_JEV_API_TOKEN, or save the key in auth.json under api_keys.typesafe or api_keys.cloudflare-jev; next step: (jev:triage \"...\") in eval, or /jev for what is configured")
    ("backend" :choice :options '("typesafe" "cloudflare") :required t
     :doc "which route answers: TypeSafe's System One, or Cloudflare's AI run route for the same model; no fallback between them")
    ("account_id" :string :default ""
     :doc "the Cloudflare account id, 32 hex digits; cloudflare only")
    ("gateway_id" :string :default ""
     :doc "an optional AI Gateway id to route through; cloudflare only, lowercase words joined by single hyphens")
    ("model" :string :default ""
     :doc "the model id; empty is jev-latest at typesafe, typesafe/jev at cloudflare")
    ("review_threshold" :string :default ""
     :doc "empty turns the review heuristic off; otherwise a number between 0.5 and 1, e.g. 0.75, naming the answers worth a human look"))
  (:help :jev "jev:triage, jev:next-step, jev:relevance and jev:ask put typed questions to Jev for a fast advisory judgment; jev:presets lists the rubrics" +manual+)
  (:command "jev" 'run-slash
   :description "Jev decisions: backend, model, key and presets"
   :session nil))
