;;;; cell.lisp --- the manual, /glasser, the section, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the seven tools from their own descriptions, and the
;;;; working agreement from (help :glasser): the original ships that guidance
;;;; as a skill beside the manifest (skills/glasser/SKILL.md), and here it is
;;;; the manual topic every request's help section names. Nothing hooks the
;;;; provider path: Glasser is data, not a model.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "glasser": {"api_key": "...", "endpoint": "https://api.glasser.ai/mcp"}
;;;; A vetoed section ("enabled": false) installs nothing, and a section
;;;; with no key refuses at start, naming the way to make one.

(in-package #:nodecode-glasser)

(defparameter +manual+
  "Glasser is available through the nodecode-glasser cell: one remote MCP server holding 1,000+ paid
data endpoints behind one key -- people and company enrichment, web, news, places, scholar, SEO
metrics, social posts, US property, stock quotes -- as seven tools.
  glasser_search   what data this key can buy, as endpoints. Costs nothing.
  glasser_inspect  one endpoint's input schema, exact price with every charge clause, run mode. Free.
  glasser_run      run one endpoint and charge the published price per call. Needs the endpoint id and
                   its input; the cell adds an idempotency key when you leave it out -- a retry with
                   the same key returns the first run, a new key is a new charge.
  glasser_runs_get / glasser_runs_list / glasser_runs_stop   one run, this workspace's runs, and
                   stopping one that has not been dispatched (STOPPED and free before dispatch).
  glasser_balance  balance, held and available. Free; the probe that says whether the key works.
Also, through eval: (glasser:call \"balance\") and (glasser:call \"runs_list\" (nlk:json-object \"limit\" 5)).
How to work: search first, then inspect, then run -- the schema says what an endpoint takes and the
price says what one call costs. Say the per-call price and get the operator's go-ahead before a
speculative, looped or bulk run, and start volume parameters small. A run that comes back QUEUED or
RUNNING is polled with glasser_runs_get; read charge_usd on every run, since a provider 404 is a
COMPLETED run charged per its clauses. Refusals are answers: insufficient_balance, rate_limited and
policy refusals come back with the message to act on, so top up, wait, or pick another endpoint
rather than retrying blindly. The operator's own keys and integrations outrank Glasser: use it to
fill a gap, never to route around what they already have. Report what the data says, which provider
served it, and the total charge."
  "What (help :glasser) answers while the cell runs.")

;;; --- /glasser -------------------------------------------------------------------

(defun key-source ()
  "Where the key comes from, in a few words; no network."
  (cond ((non-empty (setting :api-key)) "the section's api_key")
        ((let ((variable (non-empty (setting :api-key-env))))
           (and variable (non-empty (uiop:getenv variable))))
         (format nil "the variable ~a" (setting :api-key-env)))
        ((non-empty (uiop:getenv "GLASSER_API_KEY")) "GLASSER_API_KEY")
        (t (format nil "no key yet (make one at ~a)" +key-page+))))

(defun status ()
  "Where the key comes from and where the cell talks, in one line."
  (format nil "glasser: ~a; endpoint ~a; a call costs the endpoint's published price"
          (key-source) (endpoint)))

(defun panel (summary)
  "The dialog bare /glasser answers: SUMMARY over the balance check."
  (nle:list-dialog
   :title "Glasser" :summary summary
   :rows (list (list :label "Check the key"
                     :run (list :enter "/glasser balance")
                     :detail "ask the server for the workspace balance; costs nothing"))))

(defun run-slash (args session-id)
  "/glasser typed alone: where the key comes from, and its panel; /glasser
balance asks the server for the balance."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((equal verb "") (let ((text (status))) (values text (panel text))))
          ((equal verb "balance") (call "balance"))
          (t (values (format nil "glasser: ~a is not one of its choices" verb) (panel (status)))))))

;;; --- the section and the tools --------------------------------------------------

(defun start ()
  "A section with no key and none in the environment refuses here, naming
the way to make one."
  (check-config))

(nle:define-cell glasser
  (:section ("glasser")
    (:guide "make a key at https://app.glasser.ai/keys; set api_key (or name its variable with api_key_env, or export GLASSER_API_KEY); endpoint and timeout_seconds are optional; every run is charged the endpoint's published price")
    ("api_key" :secret
     :doc "the Glasser key; empty reads api_key_env, else GLASSER_API_KEY")
    ("api_key_env" :env
     :doc "the environment variable holding the key")
    ("endpoint" :string :default "https://api.glasser.ai/mcp"
     :doc "the MCP endpoint; the public server by default")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one call's ceiling, a run included"))
  (:tool "glasser_search" *search-schema* 'tool-search)
  (:tool "glasser_inspect" *inspect-schema* 'tool-inspect)
  (:tool "glasser_run" *run-schema* 'tool-run)
  (:tool "glasser_runs_get" *runs-get-schema* 'tool-runs-get)
  (:tool "glasser_runs_list" *runs-list-schema* 'tool-runs-list)
  (:tool "glasser_runs_stop" *runs-stop-schema* 'tool-runs-stop)
  (:tool "glasser_balance" *balance-schema* 'tool-balance)
  (:help :glasser "glasser: paid data endpoints as seven tools - search, inspect, run, runs, balance" +manual+)
  (:command "glasser" 'run-slash
   :description "Glasser: the key it uses; check it against the balance"
   :session nil)
  (:start #'start))
