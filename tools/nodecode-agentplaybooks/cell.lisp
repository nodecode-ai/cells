;;;; cell.lisp --- the declaration: four tools, one command, one manual topic.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin gives a host agent one prose skill and leaves the
;;;; typing to it: pick the profile, keep `--profile' on every command, preview
;;;; before `--apply', never ask for a key in chat. Here the argv is the cell's
;;;; job, "a call without apply is a preview" is the tools' shape, and the rest
;;;; of the prose is the (help :agentplaybooks) topic the model reads first.

(in-package #:nodecode-agentplaybooks)

(nlk:define-peripheral agentplaybooks :not-running t)

(defparameter +primer+
  "AgentPlaybooks holds playbooks: bundles of skills, MCP servers and federated OpenAPI tools. The
nodecode-agentplaybooks cell drives AgentPlaybooks' own `apb' CLI, which the operator installs
separately (npm install -g @agentplaybooks/cli, Node.js 20 or later); the cell installs nothing and
carries no account.
  apb_doctor   read-only: the CLI version, the target, destination permissions and whether the
      playbook key is present. Without check it makes no network call; with check it probes the
      playbook MCP connection with this process's key environment, without invoking any tool.
  apb_setup    a playbook's guided setup: its skill trees restored into the project and its harness
      profile config merged. Hosted tools stay behind one playbook MCP endpoint; upstream service
      keys stay in the AgentPlaybooks vault. tools_only skips the file restore and needs no login.
  apb_connect  the playbook connection alone: a scoped playbook key, or account for account-wide
      tools, which reaches more and is worth preferring a scoped key to.
  apb_project  pull restores a playbook's skills into the project (private ones need `apb login'
      first), sync registers them and merges MCP definitions, push uploads reviewed project config.
Every write is a preview until apply is true: show the plan to the operator, then call again with
apply. Existing differing definitions are conflicts: report them, do not overwrite them. A missing
or rejected key gives exit 2 after files were installed, so say that distinction rather than
claiming the connection works. Keys are never requested in chat and never written to config: the
operator sets them in the environment the target profile runs with, then reruns doctor with check."
  "What (help :agentplaybooks) answers while the cell runs.")

;;; --- the boundary of each tool --------------------------------------------

(nlk:define-contract doctor-query
  (:doc "Check a playbook against this machine without changing anything: the CLI, the target,
destination permissions and whether the playbook key is present. Answers `apb TARGET doctor GUID .
[--profile=NAME] [--check]'.")
  ("playbook" :string :default ""
              :doc "the playbook guid to check; empty uses the `playbook' the section names")
  ("check" :boolean :default nil
           :doc "also probe the playbook's MCP connection with this process's key environment (a network call; no tool is invoked)"))

(nlk:define-contract setup-query
  (:doc "Preview a playbook's guided setup -- its skill trees restored and its profile config merged
-- and with apply, run it. Answers `apb TARGET setup GUID . [--tools-only] [--apply]'.")
  ("playbook" :string :default ""
              :doc "the playbook guid to set up; empty uses the `playbook' the section names")
  ("tools_only" :boolean :default nil
                :doc "only the hosted MCP connection: no skill files restored and no account login")
  ("apply" :boolean :default nil :slot apply-it
           :doc "carry out the plan shown by the same call without it; false only previews"))

(nlk:define-contract connect-query
  (:doc "Preview, or with apply write, the playbook MCP connection the target profile uses. Answers
`apb connect GUID --target=TARGET [--apply]', or `apb connect --account --target=TARGET'.")
  ("playbook" :string :default ""
              :doc "the playbook guid to connect; empty uses the section's `playbook'")
  ("account" :boolean :default nil
             :doc "one account-wide connection instead of a scoped playbook one; it reaches every tool of the account")
  ("apply" :boolean :default nil :slot apply-it
           :doc "write the plan; false only previews"))

(nlk:define-contract project-query
  (:doc "A playbook project as files: pull a playbook's skills into the project, sync them into the
harness, or push the reviewed project config. Answers `apb pull GUID .', `apb sync .' or `apb push .'.")
  ("command" :string :required t :options '("pull" "sync" "push")
             :doc "pull restores skills into the project, sync registers them and merges MCP definitions, push uploads the reviewed project config")
  ("playbook" :string :default ""
              :doc "the playbook guid pull restores; empty uses the section's `playbook'")
  ("apply" :boolean :default nil :slot apply-it
           :doc "write or upload what the same call without it showed; false only previews"))

;;; --- the argv every tool shares -------------------------------------------

(defun non-empty (text)
  "TEXT when it is a string with something in it, else NIL."
  (and (stringp text) (plusp (length text)) text))

(defun target ()
  "The harness the CLI writes for: `apb TARGET setup' and `--target=TARGET'."
  (setting :target))

(defun profile-arguments ()
  "`--profile=NAME' when the section names one, else nothing: the selection
 the original skill tells the model to keep on every command."
  (let ((profile (non-empty (setting :profile))))
    (when profile (list (format nil "--profile=~a" profile)))))

(defun home-arguments ()
  "`--hermes-home=DIR' when the section names one, else nothing."
  (let ((home (non-empty (setting :hermes-home))))
    (when home (list (format nil "--hermes-home=~a" home)))))

(defun playbook-for (given tool)
  "GIVEN when it names a playbook, else the section's default; a refusal
 naming TOOL when neither does."
  (or (non-empty given)
      (non-empty (setting :playbook))
      (fail "~a needs a playbook guid: pass one, or set `playbook' in the agentplaybooks ~
section of ~/.nodecode/config.jsonc" tool)))

(defun preview-note (apply text)
  "TEXT, with the reminder the original skill repeats, when APPLY did not
 happen: a plan nobody applied changed nothing."
  (if apply
      text
      (format nil "~a~%[preview only: nothing was written; the same call with apply does it]" text)))

;;; --- the four tools --------------------------------------------------------

(defun apb-doctor (query)
  "The apb_doctor tool: the CLI as this machine has it, then the playbook's
 read-only preflight."
  (running-settings)
  (let* ((guid (playbook-for (doctor-query-playbook query) "apb_doctor"))
         (version (or (version-of (setting :binary))
                      (format nil "~s not found on PATH" (setting :binary))))
         (answer (call "apb doctor"
                       (append (apb-argv (target) "doctor" guid ".")
                               (profile-arguments)
                               (home-arguments)
                               (when (doctor-query-check query) (list "--check"))))))
    (format nil "~a~%~a" version answer)))

(defun apb-setup (query)
  "The apb_setup tool: one playbook's guided setup, previewed unless apply."
  (running-settings)
  (let* ((guid (playbook-for (setup-query-playbook query) "apb_setup"))
         (apply (setup-query-apply-it query))
         (answer (call "apb setup"
                       (append (apb-argv (target) "setup" guid ".")
                               (profile-arguments)
                               (home-arguments)
                               (when (setup-query-tools-only query) (list "--tools-only"))
                               (when apply (list "--apply"))))))
    (preview-note apply answer)))

(defun apb-connect (query)
  "The apb_connect tool: the playbook MCP connection, scoped or account-wide,
 previewed unless apply."
  (running-settings)
  (let* ((apply (connect-query-apply-it query))
         (account (connect-query-account query))
         (guid (unless account (playbook-for (connect-query-playbook query) "apb_connect")))
         (answer (call "apb connect"
                       (append (apb-argv "connect")
                               (if account (list "--account") (list guid))
                               (list (format nil "--target=~a" (target)))
                               (profile-arguments)
                               (home-arguments)
                               (when apply (list "--apply"))))))
    (preview-note apply answer)))

(defun apb-project (query)
  "The apb_project tool: pull a playbook's skills into the project, sync them
 into the harness, or push the reviewed project config."
  (running-settings)
  (let* ((command (project-query-command query))
         (apply (project-query-apply-it query))
         (write-args (when apply (list "--apply")))
         (answer
           (call (format nil "apb ~a" command)
                 (cond
                   ((string= command "pull")
                    (append (apb-argv "pull"
                                       (playbook-for (project-query-playbook query) "apb_project pull")
                                       ".")
                            (profile-arguments) (home-arguments) write-args))
                   ((string= command "sync")
                    (append (apb-argv "sync" ".")
                            (list (format nil "--target=~a" (target)))
                            (profile-arguments) (home-arguments) write-args))
                   (t (append (apb-argv "push" ".") write-args))))))
    (preview-note apply answer)))

;;; --- the command and the prerequisite --------------------------------------

(defun status-text ()
  "What `/apb' answers with no arguments: the CLI as this machine has it, and
 where the tools are."
  (format nil "apb: ~a~%~%~a"
          (or (version-of (setting :binary))
              (format nil "not found as ~s on PATH; /apb again after installing it"
                      (setting :binary)))
          "The tools are apb_doctor, apb_setup, apb_connect and apb_project; (help :agentplaybooks)
says what each answers, and /apb ARGS... runs the CLI itself, for login and for anything else."))

(defun run-command (args session-id)
  "The /apb command: with no arguments, whether the CLI is installed and where
 the tools are; with arguments, `apb ARGS...' itself -- `login' lives here."
  (declare (ignore session-id))
  (running-settings)
  (let ((typed (nlk:trimmed (or args ""))))
    (if (plusp (length typed))
        (call "apb" (append (apb-argv) (words typed)))
        (status-text))))

(defun start ()
  "Say once, on the board, what is missing: the prerequisite the model would
 otherwise discover through a tool that cannot start."
  (unless (non-empty (setting :playbook))
    (nlk:config-error "agentplaybooks.playbook is required: the guid of the playbook you work with, ~
                        the one `apb` lists or the playbook's page on agentplaybooks.ai"))
  (unless (version-of (setting :binary))
    (nle:notice (format nil "agentplaybooks: ~s not found on PATH; the apb tools explain the ~
prerequisite until AgentPlaybooks' CLI is installed, or the agentplaybooks section names where it is"
                        (setting :binary))
                :level :warning :key "nodecode-agentplaybooks")
    (nle:on-stop (lambda () (nle:notice nil :key "nodecode-agentplaybooks")))))

(nle:define-cell agentplaybooks
  (:section ("agentplaybooks")
    (:guide "set playbook to the guid of the playbook you work with; the apb CLI is looked for on PATH; profile picks the harness profile the CLI writes, directory the project it runs in; every write is a preview until a tool is called with apply")
    ("binary" :string :default "apb"
              :doc "the AgentPlaybooks CLI executable, found on PATH (npm @agentplaybooks/cli, Node.js 20 or later)")
    ("playbook" :string :required t
                :doc "the guid of the playbook you work with, used when a tool names none")
    ("target" :string :default "hermes"
              :doc "the harness the CLI writes for: `apb TARGET setup' and `--target=TARGET'")
    ("profile" :string :default ""
               :doc "the harness profile the CLI writes, as --profile=NAME; empty uses the CLI's default")
    ("hermes_home" :string :default ""
                   :doc "the installation directory the CLI writes into, as --hermes-home=DIR; empty uses the CLI's default")
    ("directory" :string :default ""
                 :doc "the project the CLI runs in; the asking session's directory when empty")
    ("timeout" :integer :default 180 :min 30
               :doc "seconds one CLI call may take before its whole process tree is killed")
    ("max_chars" :integer :default 20000 :min 500
                 :doc "how much of one answer the model is given"))
  (:tool "apb_doctor" (nlk:contract 'doctor-query) 'apb-doctor)
  (:tool "apb_setup" (nlk:contract 'setup-query) 'apb-setup)
  (:tool "apb_connect" (nlk:contract 'connect-query) 'apb-connect)
  (:tool "apb_project" (nlk:contract 'project-query) 'apb-project)
  (:help :agentplaybooks "agentplaybooks: AgentPlaybooks' skills, MCP servers and OpenAPI tools, through its own apb CLI" +primer+)
  (:command "apb" 'run-command
            :description "Whether AgentPlaybooks' CLI is installed; or run apb itself, login included"
            :argument-hint "login")
  (:start #'start))
