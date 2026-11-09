;;;; cell.lisp --- the primer, the section, the /scalekit command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was four tools and one skill. The tools are tools.lisp's; the
;;;; skill is +PRIMER+, which the model reads with (help :scalekit) and whose
;;;; one line rides every request. Nothing hooks the provider path: no model
;;;; is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "scalekit": {"client_id": "skc_...", "client_secret": "test_...",
;;;;                "environment_url": "https://your-env.scalekit.dev",
;;;;                "identifier": "", "timeout_seconds": 30, "max_chars": 4000}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-scalekit)

(defparameter +primer+
  "Scalekit is available through the nodecode-scalekit cell: four tools that act in an app on the
Scalekit account these credentials belong to, and the credentials stay here, not in the answer.
  scalekit_list_connections  the connection names this environment holds, with type and status.
  scalekit_authorize         approve one connection for the person this machine acts for. Answers
                              ACTIVE, or a link to open; a link means show it and stop.
  scalekit_execute_tool      call one of a connection's tools: connection_name, tool_name,
                              tool_input. Refused until the account is ACTIVE.
  scalekit_proxy             the connector's API for what no named tool covers: path and method.
How to work: list the connections before naming one; approve before acting; pass identifier only
when someone other than this machine's person is meant. Provider tokens stay in Scalekit, so never
ask for a Gmail, Slack or GitHub token and never write one down. A connection that is not COMPLETED,
or a dashboard that holds no such connection, is the operator's step, not a retry. The operator
runs /scalekit from the keyboard: no argument lists the connections, a connection name opens its
approval link."
  "What (help :scalekit) answers while the cell runs: the plugin's skill in one topic.")

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's values, with each Scalekit
value falling back to the environment variable the original plugin named."
  (declare (ignore table))
  (flet ((resolved (key variable)
           (or (nonempty (getf values key)) (nonempty (uiop:getenv variable)))))
    (list :client-id (resolved :client-id "SCALEKIT_CLIENT_ID")
          :client-secret (resolved :client-secret "SCALEKIT_CLIENT_SECRET")
          :environment-url (resolved :environment-url "SCALEKIT_ENVIRONMENT_URL")
          :identifier (nonempty (getf values :identifier))
          :timeout-seconds (getf values :timeout-seconds)
          :max-chars (getf values :max-chars))))

;;; --- the operator's command --------------------------------------------------

(defun connection-text ()
  "The connections this environment holds, without ever printing a token."
  (tool-list-connections nil))

(defun run-command (args session-id)
  "One /scalekit invocation: with no argument, what is connected; with one, a
connection's approval state and, when it needs one, its link."
  (declare (ignore session-id))
  (let ((name (nlk:trimmed (or args ""))))
    (if (plusp (length name))
        (format-gate (authorize-gate name (ensure-identifier)))
        (connection-text))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell scalekit
  (:section ("scalekit")
    (:guide "copy client_id, client_secret and environment_url from the Scalekit dashboard (Developers, Settings, API Credentials), or export SCALEKIT_CLIENT_ID, SCALEKIT_CLIENT_SECRET and SCALEKIT_ENVIRONMENT_URL; identifier is the person the connections act for, empty for the one this machine generates; next step: /scalekit lists the connections")
    ("client_id" :secret
     :doc "the Scalekit client id (skc_...), from API Credentials")
    ("client_secret" :secret
     :doc "the Scalekit client secret: the account secret, not a Gmail or Slack token")
    ("environment_url" :string
     :doc "the environment's own address, https://your-env.scalekit.dev")
    ("identifier" :string
     :doc "the person id these connections act for; empty uses the one this machine generated once")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one Scalekit request's ceiling")
    ("max_chars" :integer :default 4000 :min 200
     :doc "how much of one tool result an answer carries"))
  (:settings 'read-settings)
  (:help :scalekit
         "scalekit: act in any app through a Scalekit connection - (help :scalekit)"
         +primer+)
  (:command "scalekit" 'run-command
            :description "Scalekit: what is connected, or one connection's approval link"
            :argument-hint "CONNECTION"
            :session nil)
  (:tool "scalekit_list_connections" +list-schema+ 'tool-list-connections)
  (:tool "scalekit_authorize" (nlk:contract 'authorize-call) #'tool-authorize)
  (:tool "scalekit_execute_tool" (nlk:contract 'execute-call) #'tool-execute)
  (:tool "scalekit_proxy" (nlk:contract 'proxy-call) #'tool-proxy)
  (:start (lambda ()
            (check-config)
            (drop-token)
            (nle:on-stop #'drop-token))))
