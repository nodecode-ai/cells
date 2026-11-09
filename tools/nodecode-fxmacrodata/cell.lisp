;;;; cell.lisp --- the section, the hundred tools, /fxmacrodata, the manual.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original is a Hermes plugin: a manifest naming 100 tools, a handler
;;;; each, a skill and a system-prompt section. Here the same 100 tools are one
;;;; NLE:DEFINE-CELL declaration -- the `fxmacrodata' section they run on, a
;;;; command the operator types, a topic on the manual, and a stop derived from
;;;; them all.
;;;;
;;;; No key is required: the USD catalogue, indicator history and release
;;;; calendar are public, as the original's README says. A key only lifts the
;;;; ceiling on protected datasets, and a call without one is a call the
;;;; service answers with its own notice.

(in-package #:nodecode-fxmacrodata)

;;; --- the words ----------------------------------------------------------------

(defparameter +guidance+
  "Discover indicator slugs with fxmd_data_catalogue before requesting history. Public USD
catalogue, indicator history and calendar need no API key. Keep source URLs and timestamp
fields in answers. Distinguish observed releases, scheduled releases, market consensus and
FXMacroData-generated predictions. Empty records mean unavailable; never invent data. When
a result carries notices, relay them: free-tier data can be delayed or limited to a recent
window. MCP resources are preserved as structured results. Publication status and
assumed-time flags qualify release clocks; capture and schedule timestamps are not
confirmed publication. Individual API tools return pages; use fxmd_indicator_history_snapshot
for a version-pinned traversal of the requested, authorised window."
  "What the model is told about this cell's data, from the original's system-prompt section.")

(defparameter +primer+
  "fxmacrodata: FXMacroData's macro and FX research data as 100 read-only tools, every
name starting fxmd_. Each answers one JSON object: operation, data (the payload exactly
as the service sent it), records (an additive row projection), source_url, citations, and
notices when the free tier delays or narrows a window. Nothing is derived here.

  discovery    fxmd_data_catalogue (currency, e.g. USD) -- the real indicator slugs
  history      fxmd_indicator_history (a page), fxmd_indicator_history_snapshot (every
               page of a window, pinned to its first dataset_version)
  calendar     fxmd_release_calendar (explicit dates, timezone UTC), fxmd_calendar_snapshot,
               fxmd_calendar_archive_coverage, fxmd_calendar_versions
  releases     fxmd_latest_announcements, fxmd_announcement_changes, fxmd_press_releases
  forecasts    fxmd_event_predictions, fxmd_prediction_coverage_index,
               fxmd_predictions_missing_indicator
  FX           fxmd_forex, fxmd_reference_rates, fxmd_session_rates, fxmd_fx_sources,
               fxmd_rate_differentials, fxmd_curves, fxmd_factors
  positioning  fxmd_cot, fxmd_positioning_history, fxmd_risk_sentiment
  commodities  fxmd_latest_commodities, fxmd_commodities, fxmd_settlement_keys
  MCP          the fxmd_mcp_* tools reach the same service's hosted MCP server: news,
               seasonality, visual artifacts, briefing and scenario tasks, and the rest.
  brief        fxmd_usd_macro_brief -- catalogue, ten history rows and a seven-day UTC
               calendar window in one answer, complete only when all three succeed.

How to work: a currency is three letters (usd or USD; the calendar also takes COMM); a
date is YYYY-MM-DD; a `date' is an observation period, not proof the figure was public
then. Keep announcement_date, time_announced, publication_time_status and capture clocks
together with the value, and keep the source links. A null stays null. Free-tier notices
are the service speaking about its own window -- relay them rather than presenting a
delayed figure as current.

Setup: nothing is required. A key in the fxmacrodata section's api_key, or the variable
api_key_env names, unlocks protected datasets; it rides X-API-Key on REST and a Bearer
header on MCP, never a URL.
From eval the same call is (fxmacrodata:call-tool \"fxmd_data_catalogue\" (list \"currency\" \"USD\")).
(help :fxmacrodata) has this while the cell runs."
  "What (help :fxmacrodata) answers while the cell runs.")

;;; --- /fxmacrodata ----------------------------------------------------------------

(defun key-state ()
  "One line saying whether a key answers, and where from."
  (cond ((and (stringp (setting :api-key)) (plusp (length (nlk:trimmed (setting :api-key)))))
         "set in the section")
        ((api-key) (format nil "set: the environment variable ~a" (setting :api-key-env)))
        (t (format nil "none, and none is needed for USD catalogue, history and calendar ~
(write api_key in the fxmacrodata section, or export the variable api_key_env names (~a), for protected datasets)"
                   (or (setting :api-key-env) +default-key-env+)))))

(defun run-command (args session-id)
  "(/fxmacrodata) says which service this cell talks to, whether a key answers and what it
registered; (/fxmacrodata INDICATOR) builds the USD macro research brief in JSON."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text))
        (format nil "fxmacrodata: ~a (REST), ~a (MCP)~%key: ~a~%100 tools: fxmd_* operations, fxmd_mcp_* hosted tools, fxmd_usd_macro_brief, fxmd_indicator_history_snapshot~%(/fxmacrodata INDICATOR builds the USD brief; (help :fxmacrodata) has the rest)"
                (base-url) (mcp-url) (key-state))
        (nlk:encode-json-object (usd-brief (nlk:json-object "indicator" text))))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell fxmacrodata
  ;; Nothing here is required: the public USD baseline answers without a key,
  ;; as the original's README promised, and an unanswered section still
  ;; starts. The timeout is the one member with a shape to get wrong.
  (:section ("fxmacrodata")
    (:guide "no key is needed for the public USD catalogue, history and calendar; a key lifts the ceiling on protected datasets and is pasted here or named by api_key_env")
    ("api_key" :secret
               :doc "your FXMacroData key, sent in an X-API-Key header (Bearer on MCP) and never in the URL")
    ("api_key_env" :string :default "FXMACRODATA_API_KEY"
                   :doc "the environment variable read when api_key is unset")
    ("base_url" :string :default "https://api.fxmacrodata.com"
                :doc "where the REST API answers; the one REST host this cell dials")
    ("mcp_url" :string :default "https://mcp.fxmacrodata.com/mcp"
                :doc "where the hosted MCP server answers; the one MCP endpoint this cell dials")
    ("timeout_seconds" :integer :default 30 :min 1
                        :doc "seconds one exchange may take, bounded to 120"))
  (:command "fxmacrodata" 'run-command
            :description "FXMacroData: the key, the tools, or the USD research brief"
            :argument-hint "[INDICATOR]")
  ;; The original carried two texts: a skill and a system-prompt section.
  ;; Here both are one manual topic -- the primer, then the standing
  ;; instruction -- so (help :fxmacrodata) answers exactly what the model was
  ;; told, and the request's help line names it.
  (:help :fxmacrodata "FXMacroData: macro and FX research data as 100 tools - indicator history, release calendars, rates, positioning, commodities"
         (concatenate 'string +primer+ (string #\Newline) (string #\Newline) +guidance+))
  ;; The tools are a table (operations.lisp), not a hundred clauses: one
  ;; install registers them all and one stop takes them all back.
  (:start (lambda () (install-tools))))
