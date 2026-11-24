;;;; cell.lisp --- the section, the manual, the two tools and the /command.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The server is the product: it holds one athlete's own training and
;;;; health data, gathered from the apps they connected, and it serves its
;;;; own tool set at run time. So this cell registers two verbs that outlive
;;;; any version of that set -- one that lists it, one that calls into it --
;;;; instead of copying the names it happened to serve on the day.
;;;;
;;;; Write tools: the server marks the tools that only read, and one that is
;;;; not marked so is taken as writing. A writing tool runs only with
;;;; confirm true (or allow_writes in the section), so the agent asks the
;;;; athlete before a workout lands on their watch.

(in-package #:nodecode-athletedata)

(nlk:define-peripheral athletedata :not-running t)

(defparameter +primer+
  "The athletedata server holds this operator's own training and health data, collected from
the apps they connected at athletedata.health: Garmin, Oura, WHOOP, Hevy, Apple Health,
Intervals.icu and more. Two tools reach it.
  athletedata_tools            the server's tools, one line each, a * marking one that
                                writes to the athlete's accounts. refresh asks again
                                instead of answering from this process's copy.
  athletedata_call             one of them: tool is its name exactly as listed, arguments
                                its own arguments as an object, and confirm true only for a
                                * tool, after the operator agreed to the change.
Start broad (a summary, readiness today, training load), then drill into one activity or one
provider's tools; names are prefixed by source (garmin_, oura_, whoop_, hevy_, intervals_),
and only an app the athlete connected answers. Dates are in the athlete's time zone. Quote
the numbers the server returns. A failed call answers what the server said; a key that is
missing or refused says so and the operator sets it (the /athletedata command shows
whether one is set). Strava data is not available: its API terms do not allow it."
  "What (help :athletedata) answers while the cell runs.")

;;; --- the boundaries the model crosses ------------------------------------------------

(nlk:define-contract athletedata-tools
  (:doc "List the tools the athletedata server offers for this athlete's own Garmin, Oura, WHOOP, Hevy and other apps' training and health data. Call this first, then athletedata_call with one tool's name.")
  ("refresh" :boolean :default nil
   :doc "ask the server again instead of answering from the copy this process already has"))

(nlk:define-contract athletedata-call
  (:doc "Call one tool of the athletedata MCP server: the athlete's own training and health data, or a change to their accounts for a tool marked as writing.")
  ("tool" :text :required t
   :doc "the tool's name, exactly as athletedata_tools lists it")
  ("arguments" :object
   :doc "the tool's own arguments as a JSON object, as its description asks; leave it out when the tool takes none")
  ("confirm" :boolean :default nil
   :doc "true to let a tool that writes change the athlete's accounts, after you asked them; ignored for a tool that only reads"))

;;; --- the two verbs -------------------------------------------------------------------

(defun list-tools (arguments)
  "athletedata_tools: the server's own tool list, as text."
  (multiple-value-bind (tools error) (tools-result (athletedata-tools-refresh arguments))
    (or error (format-tools tools))))

(defun call-tool (arguments)
  "athletedata_call: one tool of the server, its answer as text."
  (let* ((name (athletedata-call-tool arguments))
         (given (athletedata-call-arguments arguments))
         (confirm (athletedata-call-confirm arguments)))
    (multiple-value-bind (tools error) (tools-result nil)
      (when error (return-from call-tool error))
      (let ((tool (find name tools :key #'tool-name :test #'string=)))
        (when (and tool (not (read-only-p tool)) (not confirm) (not (setting :allow-writes)))
          (return-from call-tool
            (format nil "athletedata: ~a writes to the athlete's accounts (~a), so it did not run yet. Ask them, then call again with confirm true."
                    name (nlk:one-line (or (nlk:json-value tool :string "description") "") :cap 200))))
        (multiple-value-bind (result error) (call "tools/call"
                                                  (nlk:json-object "name" name
                                                                   "arguments" (or given (nlk:json-object)))
                                                  t)
          (or error (format-result result)))))))

;;; --- the command ---------------------------------------------------------------------

(defun run-command (args session-id)
  "The /athletedata command: whether a key is set, and the server's tools."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond
      ((member word '("" "status") :test #'string=)
       (format nil "athletedata: ~a~%key: ~:[not set~;set~] (~a)~%tools: ~:[not asked yet~;~:*~d known~]~%~
/athletedata tools lists them; /athletedata refresh asks the server again"
               (setting :url)
               (and (api-key) t)
               (or (setting :api-key-env) "")
               (length *tools*)))
      ((string= word "tools") (list-tools (nlk:json-object "refresh" nil)))
      ((string= word "refresh") (list-tools (nlk:json-object "refresh" t)))
      (t "usage: /athletedata - whether a key is set; /athletedata tools - the server's tools; /athletedata refresh - ask the server again"))))

;;; --- the entry -------------------------------------------------------------------------

(defun install ()
  "A refused section leaves nothing installed; a good one keeps no session
between starts, so a restart really is a new session."
  (check-config)
  (setf *initialized* nil *session-id* nil *tools* nil)
  (nle:on-stop (lambda () (setf *initialized* nil *session-id* nil *tools* nil))))

(nle:define-cell athletedata
  (:section ("athletedata")
    (:guide "put your athletedata key in the environment variable the section's api_key_env names (sk_soma_..., from the athletedata dashboard); url and timeout_seconds are already right; allow_writes lets a tool that writes change your accounts without confirm")
    ("api_key_env" :env :default "ATHLETEDATA_API_KEY"
     :doc "the environment variable holding the athletedata API key (sk_soma_...)")
    ("url" :string :default "https://mcp.athletedata.health/mcp"
     :doc "the server's Streamable HTTP endpoint")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one exchange's ceiling, in seconds")
    ("allow_writes" :boolean :default nil
     :doc "let a tool that writes to the athlete's accounts run without confirm"))
  (:start #'install)
  (:help :athletedata
         "athletedata: the operator's own training and health data from Garmin, Oura, WHOOP, Hevy and 35 more apps, through the athletedata server"
         +primer+)
  (:tool "athletedata_tools" (nlk:contract 'athletedata-tools) #'list-tools)
  (:tool "athletedata_call" (nlk:contract 'athletedata-call) #'call-tool)
  (:command "athletedata" #'run-command
            :description "The athlete's training and health data: whether a key is set, and the server's tools"
            :argument-hint "tools|refresh"))
