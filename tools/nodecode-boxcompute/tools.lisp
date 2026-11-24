;;;; tools.lisp --- the six tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The server lists about twenty tools; the plugin's skill taught the model
;;;; to reach for them through a shape that fits in a prompt. These six are
;;;; that shape: one per group of calls, each naming the server's own tool
;;;; and carrying the arguments the call named, plus BOXCOMPUTE_TOOL for any
;;;; member this cell does not declare. Nothing is guessed and nothing is
;;;; hidden: when the server's schemas differ from these declarations, the
;;;; catalog (BOXCOMPUTE:TOOLS, BOXCOMPUTE:SCHEMA) is the authority, and
;;;; every answer is the server's own text.
;;;;
;;;; A tool answers text: the model reads it, and the ids, paths and urls in
;;;; it stay where the next call can use them.

(in-package #:nodecode-boxcompute)

;;; --- what a call needs --------------------------------------------------------

(defun needed (value label)
  "VALUE when it is text, else a refusal naming LABEL -- the member the call
left out, which the model repairs from."
  (or (nonempty value)
      (fail "boxcompute: this call needs \"~a\"" label)))

(defun object (&rest pairs)
  "A JSON object of PAIRS, leaving out every NIL value: a call sends only
what it named."
  (loop for (key value) on pairs by #'cddr
        when value append (list key value) into kept
        finally (return (apply #'nlk:make-json-object kept))))

;;; --- boxcompute_sandbox -------------------------------------------------------

(nlk:define-contract sandbox-call
  (:doc "A BoxCompute Linux VM: list the account's VMs, create one, start, stop or delete one.
Start with action list: the user may already have a VM holding their files. A running VM spends
the account's credit every minute, so stop it when the work is done -- stop keeps /workspace,
delete does not. Delete only when the user asks.")
  ("action" :text :required t :options '("list" "get" "create" "start" "stop" "delete")
   :doc "list and get never wake a VM; create makes and starts one; stop keeps its files; delete removes the VM and its disk for good")
  ("sandbox" :string
   :doc "the VM's id, as list answers it; not needed for list or create")
  ("size" :string :options '("small" "large" "xl" "xxl") :default "small"
   :doc "small (0.5 vCPU, 1 GiB), large (1.5 vCPU, 3 GiB), xl (4 vCPU, 8 GiB) or xxl (8 vCPU, 16 GiB); xl and xxl need a Starter plan, and bigger costs more per minute")
  ("internet" :boolean :default t
   :doc "whether the VM has a network at all; false makes a sealed machine"))

(defun tool-sandbox (call)
  "boxcompute_sandbox: one call on the server's sandbox_* tool of that name."
  (let ((action (sandbox-call-action call)))
    (tool-call (format nil "sandbox_~a" action)
               (cond ((string= action "create")
                      (object "size" (sandbox-call-size call)
                              "internet" (sandbox-call-internet call)))
                     ((string= action "list") (nlk:json-object))
                     (t (object "sandbox" (needed (sandbox-call-sandbox call) "sandbox")))))))

;;; --- boxcompute_run -----------------------------------------------------------

(nlk:define-contract run-call
  (:doc "Run one shell command in a BoxCompute VM, as bash -lc, and answer what it printed. It
waits up to 60 seconds and keeps 48 KiB of output per stream, so an install, a build or a test
suite belongs in boxcompute_process instead. The next command wakes a stopped VM.")
  ("sandbox" :text :required t
   :doc "the VM's id, from boxcompute_sandbox with action list, or the one create answered")
  ("command" :text :required t
   :doc "the shell command; it runs in the VM, under /workspace, never on the user's machine"))

(defun tool-run (call)
  "boxcompute_run: the server's command_run."
  (tool-call "command_run"
             (object "sandbox" (run-call-sandbox call)
                     "command" (run-call-command call))))

;;; --- boxcompute_process -------------------------------------------------------

(nlk:define-contract process-call
  (:doc "A long command in a VM, out of the way: start one and it keeps running for up to fifteen
minutes, then poll it with get, read it with output and stop it with cancel. Use it for installs,
builds, test suites and servers. Start answers the process id every later call takes.")
  ("action" :text :required t :options '("start" "get" "output" "cancel")
   :doc "start needs command; get, output and cancel need process")
  ("sandbox" :text :required t :doc "the VM's id")
  ("command" :string :doc "the shell command to start, for action start")
  ("process" :string :doc "the process id start answered, for get, output and cancel"))

(defun tool-process (call)
  "boxcompute_process: one call on the server's process_* tool of that name."
  (let* ((action (process-call-action call))
         (arguments (cond ((string= action "start")
                           (object "sandbox" (process-call-sandbox call)
                                   "command" (needed (process-call-command call) "command")))
                          (t (object "sandbox" (process-call-sandbox call)
                                     "process" (needed (process-call-process call) "process"))))))
    (tool-call (format nil "process_~a" action) arguments)))

;;; --- boxcompute_file ----------------------------------------------------------

(nlk:define-contract file-call
  (:doc "Read, list, write and edit files in a VM. Every path is under /workspace: read answers
one file's text, list a folder's entries, write creates a file and its parent folders (up to 2
MiB), and edit replaces one passage and fails with EDIT_CONFLICT when the file changed under it.")
  ("action" :text :required t :options '("read" "list" "write" "edit")
   :doc "write replaces the whole file; edit replaces old with new and refuses a file that moved")
  ("sandbox" :text :required t :doc "the VM's id")
  ("path" :text :required t :doc "the file or folder, under /workspace, for example /workspace/app.py")
  ("content" :string :doc "the whole file, for action write")
  ("old" :string :doc "the exact passage to replace, for action edit")
  ("new" :string :doc "what replaces it, for action edit"))

(defun tool-file (call)
  "boxcompute_file: one call on the server's file_* tool of that name."
  (let ((action (file-call-action call)))
    (tool-call (format nil "file_~a" action)
               (object "sandbox" (file-call-sandbox call)
                       "path" (file-call-path call)
                       "content" (file-call-content call)
                       "old" (file-call-old call)
                       "new" (file-call-new call)))))

;;; --- boxcompute_preview -------------------------------------------------------

(nlk:define-contract preview-call
  (:doc "A public https link to a port a VM is serving, so the user can open an app on their
phone. The link works for one hour and anyone who has it can open it -- say so when you share
one. list shows a VM's links; close shuts one at once.")
  ("action" :text :required t :options '("create" "list" "close")
   :doc "create needs port; close needs preview")
  ("sandbox" :text :required t :doc "the VM's id")
  ("port" :integer :doc "the port the app listens on in the VM, for action create")
  ("preview" :string :doc "the link's id, as list answers it, for action close"))

(defun tool-preview (call)
  "boxcompute_preview: one call on the server's preview_* tool of that name."
  (let* ((action (preview-call-action call))
         (arguments (cond ((string= action "create")
                           (object "sandbox" (preview-call-sandbox call)
                                   "port" (preview-call-port call)))
                          ((string= action "close")
                           (object "sandbox" (preview-call-sandbox call)
                                   "preview" (needed (preview-call-preview call) "preview")))
                          (t (object "sandbox" (preview-call-sandbox call))))))
    (tool-call (format nil "preview_~a" action) arguments)))

;;; --- boxcompute_tool ----------------------------------------------------------

(nlk:define-contract any-call
  (:doc "Any of the server's own tools, by name, with its own arguments: use it for what the
named tools do not carry (account_get, workspace_list, email_send), and for a call the server
refused because a member is shaped differently from what this cell declared. boxcompute:tools
lists the catalog and boxcompute:schema \"name\" one tool's exact arguments.")
  ("tool" :text :required t
   :doc "the server's tool name, for example account_get")
  ("arguments" :object
   :doc "the tool's arguments, as the server declares them: partner names to values"))

(defun tool-any (call)
  "boxcompute_tool: the named server tool, as it is."
  (tool-call (any-call-tool call) (any-call-arguments call)))

;;; --- the catalog, for eval ----------------------------------------------------

(defun catalog ()
  "The server's tools, as a list of plists: (:name :description :schema)."
  (let ((result (rpc "tools/list" nil)))
    (loop for tool across (nlk:json-value result :array "tools")
          collect (list :name (nlk:json-value tool :text "name")
                        :description (nlk:json-value tool :text "description")
                        :schema (nlk:json-value tool :object "inputSchema")))))

(defun tools (&optional name)
  "The server's catalog, one line a tool, or NAME's arguments when one is
named; live, so it answers what the server carries now rather than what this
cell declared."
  (let ((catalog (catalog)))
    (if (nonempty name)
        (let ((entry (find-if (lambda (one) (equal (getf one :name) name)) catalog)))
          (unless entry (fail "boxcompute: the server has no tool named ~a" name))
          (format nil "~a: ~a~%~a" (getf entry :name) (or (getf entry :description) "")
                  (nlk:pretty-json (getf entry :schema))))
        (format nil "~{~a~^~%~}"
                (loop for entry in catalog
                      when (getf entry :name)
                        collect (format nil "~a - ~a" (getf entry :name)
                                        (or (getf entry :description) "")))))))

(defun schema (name)
  "One tool's input schema, as JSON text."
  (tools name))
