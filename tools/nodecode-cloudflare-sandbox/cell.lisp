;;;; cell.lisp --- the contract, the tool, /cloudflare-sandbox, the primer, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites swaps Hermes's terminal for one whose
;;;; commands run in a Cloudflare Container; here the same Worker answers one
;;;; model-facing tool, cf_sandbox, and the eval verbs in CF-SANDBOX. Nothing
;;;; hooks the shell: this machine's own sh is untouched, and what runs
;;;; inside the sandbox is the operator's own Cloudflare account's work.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "cloudflare-sandbox": {"endpoint": "https://x.workers.dev",
;;;;                           "token_env": "CLOUDFLARE_SANDBOX_TOKEN"}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-cloudflare-sandbox)

;;; --- the boundary -----------------------------------------------------------------

(nlk:define-contract sandbox-command
  (:doc "Run one command in the operator's Cloudflare sandbox: a Linux container on their own
Cloudflare account with bash, python3 and node, whose files survive between calls when the cell
snapshots its workspace. Use it for work that should not touch this machine -- an unfamiliar
script, a build for another architecture, a service to poke at. The command's own output and
exit status come back together; a nonzero exit is an answer, not a refusal.")
  ("command" :text :required t
   :doc "the command, as bash reads it (/bin/bash -lc); relative paths resolve from cwd")
  ("cwd" :text :doc "the absolute directory in the sandbox to run it in; the cell's cwd when unset")
  ("stdin" :text :doc "text handed to the command's standard input; NUL is refused by the Worker")
  ("timeout" :integer :default 120
   :doc "seconds before the Worker kills the command's process group"))

;;; --- what the model reads --------------------------------------------------------------

(defparameter +primer+
  "The Cloudflare sandbox runs commands in a Linux container on the operator's own Cloudflare
account, through the Worker they deployed; this machine is not touched.
  cf_sandbox(command, cwd, stdin, timeout)   -- the tool: one command, its output and exit status
  (cf-sandbox:run \"ls -la\" :cwd \"/workspace\") ; => (:exit-code 0 :output \"...\")
  (cf-sandbox:read-file \"/workspace/notes.txt\") ; => contents, exit code
  (cf-sandbox:write-file \"/workspace/notes.txt\" \"text\") ; text through stdin, nothing quoted
  (cf-sandbox:health) (cf-sandbox:release) (cf-sandbox:reset)
      release snapshots the workspace and stops its container (files stay, the machine does
      not); reset discards the workspace, so the next command starts a clean one. With the
      section's persistent false, every start is a new workspace.
The container has outbound internet; host environment variables, skills and credentials are not
copied into it. /cloudflare-sandbox answers its health, workspace and image, and takes
release or reset. ERROR: CLOUDFLARE-SANDBOX-ERROR names the problem; a command whose answer was
lost may already have run, so read the workspace before repeating it."
  "What (help :cloudflare-sandbox) answers while the cell runs.")

(defparameter +output-cap+ 12000
  "How much of a command's output one tool answer carries.")

(defun tool-text (result)
  "RESULT, a run's plist, as the text the model reads."
  (let ((output (or (getf result :output) "")))
    (with-output-to-string (out)
      (format out "exit ~a" (getf result :exit-code))
      (when (plusp (length output))
        (format out "~%")
        (if (> (length output) +output-cap+)
            (progn (write-string (subseq output 0 +output-cap+) out)
                   (format out "~%[~d more characters]"
                           (- (length output) +output-cap+)))
            (write-string output out))))))

(defun run-tool (request)
  "The cf_sandbox tool: one command in the sandbox, its output and exit status."
  (tool-text (run (sandbox-command-command request)
                  :cwd (sandbox-command-cwd request)
                  :stdin (sandbox-command-stdin request)
                  :timeout (sandbox-command-timeout request))))

;;; --- the operator's command --------------------------------------------------------------

(defun run-command (args session-id)
  "The answer to one /cloudflare-sandbox invocation: its health and workspace,
or release / reset."
  (declare (ignore session-id))
  (let ((word (string-downcase (or (trimmed (or args "")) ""))))
    (handler-case
        (cond ((equal word "release")
               (release)
               "cloudflare-sandbox: the workspace is snapshotted; its container is stopping. The files stay.")
              ((equal word "reset")
               (reset)
               "cloudflare-sandbox: the workspace is gone; the next command starts a clean one.")
              (t (health)
                 (format nil "cloudflare-sandbox: ~a answers~%workspace ~a~%image ~a~@[ on ~a~]~%cwd ~a; ~:[one workspace per start~;snapshotted when idle~]"
                         (setting :endpoint) (sandbox-name) (setting :image) (setting :instance)
                         (setting :cwd) (setting :persistent))))
      (cloudflare-sandbox-error (condition)
        (format nil "cloudflare-sandbox: ~a" (cloudflare-sandbox-error-detail condition))))))

;;; --- start --------------------------------------------------------------------------------

(defun install ()
  "Refuse without an endpoint and a token, name the workspace, and say where
commands go; stopping forgets the name."
  (unless (setting :endpoint)
    (nlk:config-error "endpoint names no Worker: deploy the plugin's worker/ folder to your Cloudflare account, then set cloudflare-sandbox.endpoint to its origin"))
  (unless (setting :token)
    (nlk:config-error "~a names no token: save the Worker's SANDBOX_API_TOKEN as cloudflare-sandbox.token, or point token_env at an environment variable holding it"
                       (if (setting :token-env)
                           (format nil "token_env (~a)" (setting :token-env))
                           "neither token nor token_env")))
  (install-sandbox-name)
  (nle:notice (format nil "cloudflare-sandbox: commands run in workspace ~a" (sandbox-name))
              :key "nodecode-cloudflare-sandbox")
  (nle:on-stop (lambda ()
                 (setf *sandbox-name* nil)
                 (nle:notice nil :key "nodecode-cloudflare-sandbox"))))

(nle:define-cell cloudflare-sandbox
  (:section ("cloudflare-sandbox")
    (:guide "deploy the plugin's worker/ folder to your own Cloudflare account and set SANDBOX_API_TOKEN on it; endpoint is its origin; token_env names an environment variable holding that token, or token is the token itself; persistent keeps the workspace's files by snapshotting it when idle")
    ("endpoint" :string :required t
     :doc "the origin of the Worker you deployed, https (http only on a loopback Worker)")
    ("token_env" :env :doc "environment variable holding the Worker's SANDBOX_API_TOKEN")
    ("token" :secret :doc "the Worker's SANDBOX_API_TOKEN itself, kept in a 0600 file; token_env names one instead")
    ("namespace" :string :default "default" :doc "the group the workspace name is hashed under")
    ("workspace" :string :doc "the workspace's own name; unset, this machine's home names it, hashed")
    ("image" :string :default "hermes" :doc "an image name wrangler.jsonc deploys")
    ("instance" :string :doc "the container size: lite or standard-1 .. standard-4; unset, the Worker's own")
    ("persistent" :boolean :default t :doc "snapshot the workspace when idle, so files survive between commands")
    ("cwd" :string :default "/workspace" :doc "the directory commands start in")
    ("timeout_seconds" :integer :default 120 :min 1 :doc "how long one command may run, in seconds"))
  (:settings 'read-settings)
  (:tool "cf_sandbox" (nlk:contract 'sandbox-command) 'run-tool)
  (:help :cloudflare-sandbox
         "cloudflare-sandbox: cf_sandbox runs one command in the operator's Cloudflare Container; files stay by snapshot"
         +primer+)
  (:command "cloudflare-sandbox" 'run-command
            :description "The Cloudflare sandbox: its health and workspace, or release and reset"
            :argument-hint "HEALTH | RELEASE | RESET")
  (:start #'install))
