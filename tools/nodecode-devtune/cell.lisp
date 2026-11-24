;;;; cell.lisp --- the primer, the section, the tool, /devtune, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is two skill prompts and an MCP server. Its guidance lives
;;;; here in *PRIMER*, which (help :devtune) answers and every request carries
;;;; one line of; its tools live in api.lisp, one read each; its key moves
;;;; from DEVTUNE_API_KEY into the `devtune' section, which still reads that
;;;; variable when nothing is pasted.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "devtune": {"api_key": "", "api_key_env": "DEVTUNE_API_KEY",
;;;;               "project_id": "", "base_url": "https://devtune.ai/api/v2",
;;;;               "window_days": 30, "timeout_seconds": 30}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-devtune)

;;; --- the manual ---------------------------------------------------------------
;;; (help :devtune) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "DevTune measures what AI answer engines tell buyers about a product, ranks the fixes
most likely to change it, measures each fix after it ships, and runs Agent Picks: which
tool a coding agent installs when asked to add auth, payments, email or your category.
One tool, devtune_read, and the same call from eval:
  (devtune:report REPORT &key window-days cursor setup id)
      One report from the DevTune project the key reaches, answered as DevTune sent
      it: its own members, unrounded, with pagination.nextCursor when a list went on.
      REPORT is one of, and (devtune:reports) lists them:
        :connection              the project the key reaches, and whether it is paused
        :visibility-summary      brand awareness, citation presence, share of voice
        :visibility-diff         the same, and how it moved
        :competitive-position    the brand beside the competitors it names
        :citation-domains        the sites AI answers cite
        :citation-pages          the pages AI answers cite
        :visibility-responses    the answers themselves, listed
        :visibility-response     one answer, with :id its executionId
        :content-gaps            topics a competitor is cited for and this brand is not
        :content-gap-evidence    the evidence behind one gap, with :id its gapId
        :traffic-summary         AI crawls, answer fetches and AI-referred visits
        :traffic-platforms       the same, one AI platform at a time
        :ai-referrals            visits an AI answer referred
        :404-demand              pages AI keeps asking for that do not exist
        :actions                 what to fix next, ranked
        :action-brief            one action's brief, with :id its actionId
        :audit-summary           site readiness for AI: crawlability, llms.txt
        :audit-findings          what the audit found, one finding at a time
        :outcomes-ledger         whether shipped fixes moved the numbers
        :agent-picks-summary     pick and install rates for one Agent Picks test
        :agent-picks-cells       the same by test app, request, agent and model
        :agent-picks-history     a trend across tests
        :agent-picks-runs        individual runs
        :agent-picks-run-trace   one run, with :id its runId
        :agent-picks-actions     the losing scenarios, and what to fix
        :agent-picks-test        one test's status and credits, with :id its test id
        :agent-picks-schedule    when tests run
        :agent-picks-catalogue   the categories, tools, test apps and models to set up
      window-days is 30 or 90 for a measurement read (7, 30, 90 or 365 for the results
      ones) and the section's window_days answers when a call names none; cursor is
      the pagination.nextCursor of the page before; setup picks one Agent Picks setup by
      id or category slug when a project has several.
Report the numbers the way DevTune measures them, and keep what DevTune returned apart
from your own reading of it:
- Brand awareness, citation presence, share of voice and their ranges are percentages
  from 0 to 100: 0.23 is 0.23%, not 23%. Advertising rates are fractions from 0 to 1:
  0.0009 is 0.09%. Check a rate against its successes and n before reporting it.
- A citation is an AI answer linking to a url; a mention is the brand named without
  one. Keep them separate. Share of voice compares this brand with its configured
  competitors only, not with everything cited.
- Crawls, answer fetches and AI-referred visits are different counts: report each on
  its own.
- A null, a no_data or a day without coverage was not measured: say so, never zero,
  never an estimate. Give a range, or say the sample is small, when the answer carries
  one or a directional flag.
- An outcome is improved, mixed, no clear change, declined, still measuring or
  unmeasured, and shows what moved after a fix shipped, not that the fix caused it.
- Agent Picks rates are fractions from 0 to 1: 0.42 is 42%. Give each with its
  rate_low and rate_high and its coverage; rates count completed runs only. Pick rate
  and install rate are different numbers. Built its own and No provider chosen are
  outcomes like any tool, never folded into a competitor. A public test's figures are
  the category's, not the user's. Compare two tests only when their comparison_key
  matches, and trends only while judging_key stays the same. A pick records what the
  agent added: it does not prove the integration works.
- Tell the operator when you stopped while hasMore was still true.
This cell READS: it creates, updates, archives and deletes nothing, starts no test and
spends no credits, so a read needs no approval. A refusal naming a scope or a feature
means the key or the plan is short of it: read :connection and say which. DevTune
publishes no page and opens no pull request of its own."
  "What (help :devtune) answers while the cell runs.")

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with the key read here, once: the inline secret,
else the variable api_key_env names, else a refusal."
  (declare (ignore table))
  (let* ((key (nonblank (getf values :api-key)))
         (env (nonblank (getf values :api-key-env)))
         (plist (copy-list values)))
    (setf (getf plist :api-key)
          (or key
              (and env (nonblank (nle::credential-env env)))
              (nlk:config-error
               "no key: paste a DevTune project API key into api_key, or name the ~
variable holding it in api_key_env~@[ (~a is unset or empty)~]"
               env)))
    plist))

;;; --- start and stop -----------------------------------------------------------------

(defun start ()
  "Forget the project the last start resolved; the key's own answers it again."
  (setf *project-id* nil)
  (nle:on-stop (lambda () (setf *project-id* nil))))

;;; --- the operator's own door ------------------------------------------------------

(defun run-slash (args session-id)
  "/devtune REPORT: one read of the connected project, answered as the model's
tool answers it; with no report, what the key reaches and what it can read."
  (declare (ignore session-id))
  (let ((word (first (uiop:split-string (or (nonblank args) "") :separator '(#\Space #\Tab)))))
    (handler-case
        (if (and word (plusp (length word)))
            (report word)
            (connection-line))
      (devtune-error (condition) (princ-to-string condition)))))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell devtune
  (:section ("devtune")
    (:guide "make a project API key under API Keys in DevTune, then paste it into api_key or name the variable holding it in api_key_env; project_id is read from the key's own connection when left empty")
    ("api_key" :secret
               :doc "the DevTune project API key, written here; left empty, the variable api_key_env names is read")
    ("api_key_env" :env :default "DEVTUNE_API_KEY"
                   :doc "the environment variable holding the key, read when api_key is empty")
    ("project_id" :string
                  :doc "the DevTune project to read; empty, GET /connection answers the one the key reaches")
    ("base_url" :string :default "https://devtune.ai/api/v2"
                :doc "the REST root this cell reads; only a proxy or a mirror needs another")
    ("window_days" :integer :default 30 :min 1
                   :doc "the measurement window a read that names none asks for: 30 or 90")
    ("timeout_seconds" :integer :default 30 :min 1
                       :doc "seconds one read may take"))
  (:settings #'read-settings)
  (:tool "devtune_read" (nlk:contract 'devtune-read-request) #'read-tool)
  (:command "devtune" 'run-slash
            :description "Read a DevTune report about your product"
            :argument-hint "REPORT")
  (:help :devtune
   "devtune: what AI answers say about a product, and which tools coding agents pick - the devtune_read tool, and (devtune:report ...)"
   *primer*)
  (:start #'start))
