;;;; cell.lisp --- the section, the two tools, the /understudy command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was two tools, a slash command and a report page. This cell
;;;; keeps the first two as tools the model calls and the command as
;;;; /understudy; the page is the scorecard's text.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "understudy": {"base_url": "https://openrouter.ai/api/v1",
;;;;                  "api_key_env": "OPENROUTER_API_KEY",
;;;;                  "judge_model": "anthropic/claude-sonnet-5",
;;;;                  "sample": 8, "days": 14, "timeout_seconds": 120,
;;;;                  "budget_usd": "1.50",
;;;;                  "prices": ["moonshotai/kimi-k3=0.6/2.5"]}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-understudy)

;;; --- the manual -----------------------------------------------------------------

(defparameter +primer+
  "Understudy answers one question: could a cheaper model take the driver's seat on your own
work, and what would that save? It reads your recent turns from this home's own session
history, replays a sample of the questions on the candidate models you name, has a judge
model compare each answer to the one you actually got, and reports the takeover rate, the
misses and the tokens spent.
  understudy_audition  models (a list of provider:model ids on the configured endpoint),
                       sample (how many past decisions), days (how far back). Every call
                       spends real tokens on the operator's account; say the cost before a
                       second run, and never widen sample or days without being asked.
  understudy_report    run: \"last\", an id, or empty for the newest. The stored scorecard.
Read a takeover rate as evidence, not proof: a small sample says little, and a candidate
that misses one decision of five may still be the wrong switch. Quote the rate with its
sample size. The operator runs one from the keyboard with /understudy <model>."
  "What (help :understudy) answers while the cell runs.")

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's values, parsed."
  (declare (ignore table))
  (list :base-url (or (nonempty (getf values :base-url)) +default-base-url+)
        :api-key (nonempty (getf values :api-key))
        :api-key-env (getf values :api-key-env)
        :judge-model (or (nonempty (getf values :judge-model)) "")
        :sample (getf values :sample)
        :days (getf values :days)
        :timeout-seconds (getf values :timeout-seconds)
        :budget-usd (parse-budget (getf values :budget-usd))
        :prices (parse-prices (getf values :prices))))

;;; --- the boundaries the tools cross ------------------------------------------------

(nlk:define-contract audition-call
  (:doc "Replay your own past decisions on candidate models and report how often each one
could have taken over. Costs real tokens: one call per decision per candidate, plus one
judging call each. Say the sample size and the models before you run it.")
  ("models" :array :of :string :required t
   :doc "candidate model ids served by the endpoint, e.g. moonshotai/kimi-k3")
  ("sample" :integer :doc "past decisions to replay; the section's sample when absent")
  ("days" :integer :doc "how far back to look for conversations; the section's days"))

(nlk:define-contract report-call
  (:doc "Read a stored understudy scorecard: the takeover rate, the misses and the tokens
each candidate spent on the last auditions.")
  ("run" :string :doc "\"last\" or empty for the newest scorecard; else a run id"))

;;; --- the tools the model calls ----------------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal or a dead endpoint becomes that text, never an
unhandled error."
  `(handler-case (progn ,@body)
     (understudy-error (condition)
       (concatenate 'string "ERROR: " (understudy-error-detail condition)))
     (error (condition)
       (format nil "ERROR: understudy: ~a" (nlk:one-line (princ-to-string condition))))))

(defun tool-audition (call)
  "understudy_audition: the replay, as the scorecard the model reads."
  (answering
   (audition (coerce (or (audition-call-models call) #()) 'list)
             :sample (audition-call-sample call)
             :days (audition-call-days call))))

(defun tool-report (call)
  "understudy_report: one stored scorecard."
  (let ((run (nonempty (report-call-run call))))
    (cond ((or (null run) (equalp run "last"))
           (or (latest-report)
               "understudy: no audition is stored yet; call understudy_audition first"))
          (t (or (stored-report run)
                 (format nil "understudy: no run ~a is stored; the ids are ~{~a~^, ~}; ~
call understudy_audition to make one"
                         run (or (run-ids) '("none"))))))))

;;; --- the operator's command -------------------------------------------------------

(defun status-text ()
  "What this cell is connected to, without ever printing the key."
  (format nil "understudy: ~a, judge ~a, key ~a; sample ~d decisions over ~d days, \
budget $~,2f~%~@[~a~%~]Run one with /understudy <model> [<model>…], or /understudy last."
          (root)
          (or (judge-model) "MISSING")
          (if (setting-api-key) "set" "MISSING")
          (setting :sample)
          (setting :days)
          (setting :budget-usd)
          (when (null (judge-model))
            "Set judge_model in the understudy section first.")))

(defun run-command (args session-id)
  "One /understudy invocation: no argument is the connection, last and runs read
the shelf, anything else is a model list to audition."
  (declare (ignore session-id))
  (let ((words (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space))))
    (cond ((null words) (status-text))
          ((equalp (first words) "runs") (runs-text))
          ((equalp (first words) "last")
           (or (latest-report) "understudy: no audition is stored yet"))
          (t (answering (audition words))))))

;;; --- the cell ---------------------------------------------------------------------

(nle:define-cell understudy
  (:section ("understudy")
    (:guide "put an OpenAI-compatible endpoint in base_url (OpenRouter unless you name another), a key in api_key or in the variable api_key_env names, and the judge's model id in judge_model; budget_usd caps one run; prices makes the scorecard say what it cost; next step: /understudy shows the connection")
    ("base_url" :string
     :doc "an OpenAI-compatible endpoint: https://openrouter.ai/api/v1 unless you name another")
    ("api_key" :secret
     :doc "the endpoint's API key")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here")
    ("judge_model" :string :required t
     :doc "the model id that judges a replay, served by the same base_url")
    ("sample" :integer :default 8 :min 1
     :doc "past decisions replayed per candidate")
    ("days" :integer :default 14 :min 1
     :doc "how many days of conversation to sample from")
    ("timeout_seconds" :integer :default 120 :min 5
     :doc "one model call's ceiling")
    ("budget_usd" :string :default "1.50"
     :doc "a hard spending cap for one audition, in USD; 0 means no cap")
    ("prices" :list
     :doc "USD per 1M tokens as \"model=input/output\", for the models you audition"))
  (:settings 'read-settings)
  (:help :understudy
         "understudy: audition cheaper models on your own past decisions - (help :understudy)"
         +primer+)
  (:command "understudy" 'run-command
            :description "Audition models on your own past decisions"
            :argument-hint "MODEL | last | runs")
  (:tool "understudy_audition" (nlk:contract 'audition-call) #'tool-audition)
  (:tool "understudy_report" (nlk:contract 'report-call) #'tool-report)
  (:start (lambda ()
            (check-config))))
