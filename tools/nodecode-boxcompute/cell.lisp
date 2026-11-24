;;;; cell.lisp --- the primer, the section, the /boxcompute command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was a portable manifest, a skill and an MCP server entry the
;;;; user added by hand. The entry is wire.lisp, the tools are tools.lisp, and
;;;; the skill is +PRIMER+, which the model reads with (help :boxcompute) and
;;;; whose one line rides every request. Nothing hooks the provider path: no
;;;; model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "boxcompute": {"api_key": "...", "api_key_env": "BOXCOMPUTE_API_KEY",
;;;;                  "url": "https://mcp.boxcompute.ai/mcp",
;;;;                  "timeout_seconds": 60}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-boxcompute)

(defparameter +primer+
  "BoxCompute gives the user Ubuntu Linux VMs in the cloud, through the nodecode-boxcompute cell:
six tools that act on the account the key belongs to. Use a VM when the work should not touch the
user's machine (untrusted code, a system-wide install, a clean-room test), when it needs Linux or
more CPU and memory than they have, or when other people should open what it serves. For ordinary
local edits, keep using the local tools.
  boxcompute_sandbox  action list, get, create, start, stop or delete. list first: the user may
      already have a VM with their files. create takes size (small, large, xl, xxl) and internet;
      it waits for the boot, usually under a minute. stop keeps /workspace; delete removes the VM
      and its files for good, so delete only when the user asks.
  boxcompute_run      one shell command, bash -lc, up to 60 seconds, 48 KiB of output per stream.
  boxcompute_process  a command out of the way, up to fifteen minutes: start, then get, output,
      cancel. Installs, builds, test suites and servers belong here.
  boxcompute_file     read, list, write and edit files under /workspace; edit is a guarded
      replace that fails with EDIT_CONFLICT when the file changed.
  boxcompute_preview  a public https link to a port in the VM, for one hour, that anyone can
      open: say so when you share one. close shuts it at once.
  boxcompute_tool     any of the server's own tools by name, with its own arguments, for what the
      five do not carry (account_get, workspace_list, email_send). (boxcompute:tools) lists the
      catalog and (boxcompute:schema \"sandbox_create\") one tool's exact arguments, live.
How to work: pick a VM before making one, start small and size up only when the task needs it, run
a long job as a process rather than a command, and finish tidy -- a running VM spends the account's
credit every minute and stops by itself 30 minutes after its last command, file or preview call.
The next command or file call wakes a stopped VM. A failed call answers CODE: message:
INSUFFICIENT_CREDIT means top up and do not retry; QUOTA_EXCEEDED means offer to stop or delete a
VM and let the user choose; FEATURE_ACCESS_DENIED means that size is not on the plan;
SANDBOX_UNAVAILABLE while a VM is still starting means call again.
The operator runs a sandbox action from the keyboard with /boxcompute list, create, get, start,
stop or delete."
  "What (help :boxcompute) answers while the cell runs: the plugin's skill in one topic.")

;;; --- the operator's command ----------------------------------------------------

(defparameter +actions+ '("list" "get" "create" "start" "stop" "delete")
  "The sandbox actions /boxcompute takes as its one word.")

(defun status-text ()
  "What this cell is connected to, without ever printing the key."
  (format nil "boxcompute: ~a, key ~a~@[, session ~a~]; six tools~%\
/boxcompute list|get|create|start|stop|delete runs one sandbox call; (boxcompute:tools) lists the server's catalog."
          (endpoint)
          (if (api-key) "set" "MISSING")
          *session-id*))

(defun run-command (args session-id)
  "One /boxcompute invocation: with no argument, what the cell is connected
to; with one of the sandbox actions, that call on the account's VMs."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond ((zerop (length word)) (status-text))
          ((member word +actions+ :test #'string=)
           (tool-sandbox (decode-sandbox-call (nlk:make-json-object "action" word))))
          (t (format nil "~a~%[no action called ~a]" (status-text) word)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell boxcompute
  (:section ("boxcompute")
    (:guide "make a key in the BoxCompute app under Menu -> API keys and paste it in api_key, or export BOXCOMPUTE_API_KEY (or name the variable in api_key_env); the cell talks to BoxCompute's MCP server at mcp.boxcompute.ai and needs nothing installed; next step: /boxcompute shows the connection")
    ("api_key" :secret
     :doc "the BoxCompute API key")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (BOXCOMPUTE_API_KEY is read either way)")
    ("url" :string
     :doc "the MCP address: https://mcp.boxcompute.ai/mcp unless you go through a gateway of your own")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one request's ceiling in seconds; command_run itself waits up to 60"))
  (:help :boxcompute
         "boxcompute: cloud Linux VMs - create one, run commands, edit files, share a preview link - (help :boxcompute)"
         +primer+)
  (:command "boxcompute" 'run-command
            :description "BoxCompute: the connection, or one sandbox action"
            :argument-hint "list|get|create|start|stop|delete"
            :session nil)
  (:tool "boxcompute_sandbox" (nlk:contract 'sandbox-call) #'tool-sandbox)
  (:tool "boxcompute_run" (nlk:contract 'run-call) #'tool-run)
  (:tool "boxcompute_process" (nlk:contract 'process-call) #'tool-process)
  (:tool "boxcompute_file" (nlk:contract 'file-call) #'tool-file)
  (:tool "boxcompute_preview" (nlk:contract 'preview-call) #'tool-preview)
  (:tool "boxcompute_tool" (nlk:contract 'any-call) #'tool-any)
  (:start (lambda ()
            (check-config))))
