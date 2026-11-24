;;;; cell.lisp --- the primer, the section, /unbrowse, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs
;;;; (help :unbrowse) answers the primer below, and every request carries its
;;;; one line. The operator's /unbrowse reads the account's balance.
;;;;
;;;; Hermes registered the 24 tools in its own toolset and gated them on a key;
;;;; here the verbs answer a refusal naming the key instead, because a Lisp
;;;; function that is absent teaches nothing, and Nodecode's vocabulary is the
;;;; manual plus eval.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "unbrowse": {"api_key": "...", "mcp_url": "https://unbrowse.ai/mcp",
;;;;                "end_user": "", "timeout_seconds": 180}

(in-package #:nodecode-unbrowse)

(defparameter +primer+
  "Unbrowse is a hosted website service: read a page as clean markdown, run a task on a site through
a learned route, or drive a cloud browser when nothing fits. Execution, learned routes and website
sessions stay in Unbrowse's cloud. One Lisp function per Unbrowse tool, in the unbrowse package,
called through eval. Every one answers a string; a refusal is ERROR: UNBROWSE-ERROR with the
server's code (browser_capacity, unauthorized, no_capability, input_required) in it.

  (unbrowse:scrape \"url\" &key formats only-main-content render)   one page, main content, markdown.
  (unbrowse:map \"url\" &key search limit)   a site's URLs, from its sitemaps.
  (unbrowse:sites &key site)   what Unbrowse knows about each site this workspace touched.
  (unbrowse:discover \"what you need\")   the capabilities this workspace already has.
  (unbrowse:run &key capability task input url idempotency-key)   start a durable run.
  (unbrowse:inspect \"runId\")   status, open requirements, the verified result, effects.
  (unbrowse:resume \"runId\" :answers (nlk:json-object \"field\" \"value\"))   answer and go on.
  (unbrowse:cancel \"runId\")   receipt of dispatched, uncertain and unstarted effects.
  (unbrowse:index \"url\" &key focus max-capabilities)   learn a whole site in the background;
      (unbrowse:index-status :job-id \"...\") follows it.
  (unbrowse:browse-open \"url\" &key task)  (unbrowse:browse-snapshot \"sessionId\")
  (unbrowse:browse-act \"sessionId\" \"click|fill|select|check|press|wait|autofill\" &key ref value
                        name vault credential)  (unbrowse:browse-finish \"sessionId\")
  (unbrowse:browse-close \"sessionId\" &key discard)
  (unbrowse:credentials-list &key origin)  (unbrowse:credentials-request \"origin\")
  (unbrowse:credentials-status \"requestId\" &key wait-seconds)
  (unbrowse:credits &key pack)  (unbrowse:usage)  (unbrowse:learn &key goal har traces title)
  (unbrowse:forget \"capability\")  (unbrowse:canvas-read)  (unbrowse:canvas-put ...)
  (unbrowse:call-tool \"unbrowse.some.tool\" (nlk:json-object \"arg\" \"value\"))   any other tool
      the workspace has, learned and indexed ones included; (unbrowse:tools) lists them.

Execute a task like this: (unbrowse:discover \"...\"), then (unbrowse:run :capability \"id\" :input
(nlk:json-object ...)). input_required means answer the open requirements on the same run with
unbrowse:resume. no_capability means do it through browse-open / browse-act / browse-finish.
A run's status and its verified result are the only proof of completion: a transport success, a
screenshot or a plan proves nothing, and outcome_unknown means inspect the effects before retrying.
Never type a password into a tool argument: use browse-act with vault, or send the user the signIn
link credentials-request answers.

browser_capacity means the cloud browsers are busy: wait about thirty seconds and retry. A 401 means
the key in the unbrowse section (or UNBROWSE_API_KEY) is missing, wrong or expired. Public writes -
posting, sending, buying - want the user's authorization first. One page read costs credits;
(unbrowse:credits) shows the balance."
  "What (help :unbrowse) answers while the cell runs.")

(defun start ()
  "Nothing to run: every call is its own POST. The RPC id counter starts over."
  (setf *rpc-id* 0))

(defun run-command (args session-id)
  "The operator's /unbrowse: the account's balance, or the key it is missing."
  (declare (ignore session-id))
  (values
   (handler-case
       (let ((what (string-downcase (string-trim '(#\Space #\Tab)
                                                 (or args "")))))
         (cond
           ((member what '("" "status") :test #'string=)
            (if (api-key)
                (format nil "Unbrowse at ~a~@[ (end user ~a)~]~%~a~%~a"
                        (endpoint) (end-user) (credits) (usage))
                (format nil "Unbrowse at ~a~%No API key yet: create one at ~a, then set it in the ~
unbrowse section's api_key (or export UNBROWSE_API_KEY)."
                        (endpoint) +key-page+)))
           ((string= what "credits") (credits))
           ((string= what "usage") (usage))
           ((string= what "tools") (tools))
           (t (format nil "/unbrowse [status|credits|usage|tools]: Unbrowse's balance and the tools ~
this key can call."))))
     (unbrowse-error (condition)
       (format nil "Unbrowse: ~a" condition)))
   nil))

(nle:define-cell unbrowse
  (:section ("unbrowse")
    (:guide "put an Unbrowse API key in api_key (create one at https://unbrowse.ai/app); the verbs refuse until one is there; mcp_url, end_user and timeout_seconds can stay as they are")
    ("api_key" :secret
     :doc "Unbrowse API key (ub_live_...) or OAuth access token; UNBROWSE_API_KEY is read when this is empty")
    ("mcp_url" :string :default "https://unbrowse.ai/mcp"
     :doc "the hosted MCP endpoint every call goes to")
    ("end_user" :string :default ""
     :doc "org keys only: the user each call is made for (X-Unbrowse-End-User); UNBROWSE_END_USER wins")
    ("timeout_seconds" :integer :default 180 :min 5
     :doc "the ceiling on one Unbrowse call, in seconds"))
  (:start #'start)
  (:help :unbrowse
         "unbrowse:scrape, discover, run and browse-open reach the hosted Unbrowse site tools"
         +primer+)
  (:command "unbrowse" 'run-command
            :description "Unbrowse: the account's credits and usage"
            :argument-hint "status|credits|usage|tools"
            :session nil))
