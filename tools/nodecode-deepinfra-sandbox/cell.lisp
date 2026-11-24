;;;; cell.lisp --- the primer, the section, the /deepinfra command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original was a terminal.backend plugin: it swapped the agent's shell and
;;;; file calls for DeepInfra's cloud sandbox, run through the Python SDK. Here
;;;; the sandbox is four tools and one command, and the model learns them from
;;;; the manual: while the cell runs, (help :deepinfra-sandbox) answers the
;;;; primer below, and every session's help section carries one line naming it.
;;;; Nothing hooks the provider path: no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "deepinfra_sandbox": {"api_key": "...", "plan": "small",
;;;;                          "timeout_seconds": 60, "idle_minutes": 60,
;;;;                          "keep_workspace": true}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-deepinfra-sandbox)

(defparameter +primer+
  "The nodecode-deepinfra-sandbox cell runs commands in an isolated DeepInfra Linux microVM -- a
cloud sandbox of its own, on the account's DeepInfra key, never the user's machine. Use it when
the work should not touch their computer (untrusted code, a scratch build, a clean-room test), when
it wants a plain Linux box, or when a long job should outlive the turn.
  deepinfra_run      one shell command, bash -c, up to 1800 seconds; answers its output and exit
      code. The sandbox is woken first when it was stopped, which takes a few seconds.
  deepinfra_read     a file's text out of /workspace.
  deepinfra_write    a whole file into /workspace, folders made.
  deepinfra_sandbox  status (dials nothing), create, start, stop, delete.
How to work: only /workspace survives a stop -- a package installed with pip, a file in /tmp, a
background process, all go on the next start, so keep what matters under /workspace and redo the
rest. The key is not in the sandbox's environment. A stopped sandbox costs nothing and the next
command wakes it; delete removes /workspace for good, so delete only when the user asks. A
command past its timeout is killed by the service: for a long job write a script with
deepinfra_write and run it with a timeout that fits, or run it under nohup and poll it.
The operator sees the sandbox from the keyboard with /deepinfra, or status, start, stop, delete."
  "What (help :deepinfra-sandbox) answers while the cell runs.")

;;; --- the operator's command ----------------------------------------------------

(defparameter +actions+ '("status" "create" "start" "stop" "delete")
  "The lifecycle actions /deepinfra takes as its one word.")

(defun run-command (args session-id)
  "One /deepinfra invocation: with no argument, what the sandbox is; with one
of the lifecycle actions, that action."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or args "")))))
    (cond ((zerop (length word)) (status-text))
          ((member word +actions+ :test #'string=)
           (tool-lifecycle (decode-lifecycle-call (nlk:make-json-object "action" word))))
          (t (format nil "~a~%[no action called ~a; the actions are ~{~a~^, ~}]"
                     (status-text) word +actions+)))))

;;; --- start and stop ------------------------------------------------------------

(defun shutdown ()
  "Leave the sandbox as the section says: stopped, /workspace kept, or deleted."
  (let ((id (or *sandbox-id* (ignore-errors (remembered-id)))))
    (when id
      (ignore-errors
        (if (setting :keep-workspace)
            (stop-remote id)
            (delete-remote id))))
    (setf *state-read* nil *sandbox-id* nil *sandbox-state* nil)
    nil))

(defun start ()
  "Read the sandbox this home last drove, and on stop leave it as the section says."
  (setf *state-read* nil *sandbox-id* nil *sandbox-state* nil)
  (check-config)
  (nle:on-stop #'shutdown))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell deepinfra-sandbox
  (:section ("deepinfra-sandbox")
    (:guide "make a key at deepinfra.com/dash/api_keys -- the same DeepInfra key the models use -- and paste it in api_key, or export DEEPINFRA_API_KEY (or name the variable in api_key_env); the cell talks to api.deepinfra.com and needs nothing installed; next step: /deepinfra shows the sandbox, and a first deepinfra_run makes one")
    ("api_key" :secret
     :doc "the DeepInfra API key")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (DEEPINFRA_API_KEY is read either way)")
    ("plan" :string
     :doc "the sandbox plan tier, for example small or large; unset is the service's own default (medium), which is not the cheapest")
    ("base_url" :string
     :doc "https://api.deepinfra.com unless you go through a gateway of your own")
    ("timeout_seconds" :integer :default 60 :min 5
     :doc "one request's ceiling in seconds, apart from a command's own timeout")
    ("idle_minutes" :integer :default 60 :min 5
     :doc "how long a sandbox may sit idle before the service stops it; /workspace is kept")
    ("keep_workspace" :boolean :default t
     :doc "when the cell stops, stop the sandbox and keep /workspace (false: delete the sandbox and its files)"))
  (:help :deepinfra-sandbox
         "deepinfra-sandbox: run commands in an isolated DeepInfra cloud sandbox - (help :deepinfra-sandbox)"
         +primer+)
  (:command "deepinfra" 'run-command
            :description "DeepInfra sandbox: its state, or one lifecycle action"
            :argument-hint "status|create|start|stop|delete"
            :session nil)
  (:tool "deepinfra_run" (nlk:contract 'run-call) #'tool-run)
  (:tool "deepinfra_read" (nlk:contract 'read-call) #'tool-read)
  (:tool "deepinfra_write" (nlk:contract 'write-call) #'tool-write)
  (:tool "deepinfra_sandbox" (nlk:contract 'lifecycle-call) #'tool-lifecycle)
  (:start #'start))
