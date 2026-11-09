;;;; cell.lisp --- the contract, the tools, /wslc, the primer, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin this cell rewrites swaps Hermes's terminal for one whose
;;;; commands run in a Linux container on Microsoft's WSL Containers runtime;
;;;; here the same runtime answers one model-facing tool, wslc_exec, and the
;;;; eval verbs in the WSLC package. Nothing hooks the shell: this machine's
;;;; own sh is untouched, and what runs inside the sandbox is the operator's
;;;; own wslc runtime's work.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "wslc": {"image": "alpine:3.20", "persistent": true}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-wslc)

;;; --- the boundary -----------------------------------------------------------------

(nlk:define-contract sandbox-command
  (:doc "Run one command in the operator's WSL Containers sandbox: a Linux container on the
WSL Containers runtime that ships with WSL 3.0.3+, driven through the wslc CLI. Use it for
work that should not touch this machine -- an unfamiliar script, a build, a service to poke
at. The command's own output and exit status come back together; a nonzero exit is an
answer, not a refusal.")
  ("command" :text :required t
   :doc "the command, as bash reads it (/bin/bash -lc); relative paths resolve from cwd")
  ("cwd" :text :doc "the absolute directory inside the sandbox to run it in; the section's cwd when unset")
  ("timeout" :integer :default 120
   :doc "seconds before the command is killed; the section's timeout_seconds bounds it"))

(nlk:define-contract sandbox-list
  (:doc "List the container sandboxes the WSL Containers runtime holds: the ones this cell
started, or every container on the runtime.")
  ("all" :boolean :default nil
   :doc "true lists every container on the runtime, not only this cell's sandboxes"))

(nlk:define-contract sandbox-target
  (:doc "Remove one container this cell started, by the name wslc_containers prints. A
persistent sandbox's files go with it.")
  ("name" :text :required t :doc "the container's name, as wslc_containers lists it"))

;;; --- what the model reads ------------------------------------------------------------

(defparameter +primer+
  "The WSL Containers sandbox runs commands in a Linux container on this Windows machine's
WSL Containers runtime, through the wslc CLI; this machine's own shell is not touched.
  wslc_exec(command, cwd, timeout)   -- the tool: one command, its output and exit status
  (wslc:exec \"uname -a\")            ; => (:exit-code 0 :output \"...\")
  (wslc:sandboxes)                   ; => the containers this cell started
  (wslc:remove-sandbox \"nodecode-wslc-default\") ; removes one, files and all
  (wslc:binary) (wslc:version)       ; the CLI it drives
      A persistent sandbox (the default) keeps its files between commands and is stopped,
      not removed, when the cell stops; a non-persistent one is a fresh container per start
      and is removed at the end of it. The container has outbound internet and bind mounts
      are read-write unless :ro says so; host environment variables enter it only when
      wslc.forward_env names them. /wslc reports the CLI, the image, the sandbox's state and
      what this wslc build cannot harden. ERROR: WSLC-ERROR names the problem; a command
      whose answer was lost may already have run, so read the sandbox before repeating it."
  "What (help :wslc) answers while the cell runs.")

(defparameter +output-cap+ 12000
  "How much of a command's output one answer carries.")

(defvar *sandbox* nil
  "The container this run is bound to, named at start.")

(defvar *made* nil
  "Whether this run created or adopted its container, so a stop may end it.")

(defun command-text (result)
  "RESULT, a run's plist (:exit-code :output), as the text the model reads."
  (let ((output (or (getf result :output) "")))
    (with-output-to-string (out)
      (format out "exit ~a" (getf result :exit-code))
      (when (plusp (length output))
        (format out "~%")
        (if (> (length output) +output-cap+)
            (progn (write-string (subseq output 0 +output-cap+) out)
                   (format out "~%[~d more characters]" (- (length output) +output-cap+)))
            (write-string output out))))))

(defun row-line (row)
  "One `wslc ps' row as a line: name, state, image."
  (format nil "~a  ~a  ~a"
          (or (field row "Names") (field row "Name") "?")
          (or (field row "State") "?")
          (or (field row "Image") "?")))

(defun sandbox-rows (&key all)
  "The containers ALL of them, or only the ones this cell started: by label."
  (let ((rows (containers :all t)))
    (if all
        rows
        (remove-if-not (lambda (row)
                         (let ((labels (field row "Labels")))
                           (and (stringp labels) (search (concatenate 'string +name-prefix+ "=1") labels))))
                       rows))))

;;; --- the verbs the eval snippet calls ------------------------------------------------

(define-verb exec (command &key cwd timeout)
  "Run COMMAND in the sandbox, answering (:exit-code N :output TEXT). The
container is created on the first command, and from then on reused."
  (let* ((exe (binary))
         (name (ensure-sandbox))
         (line (if (and cwd (plusp (length cwd)))
                   (format nil "cd ~a && ~a" (shell-quote cwd) command)
                   command))
         (seconds (or timeout (setting :timeout-seconds))))
    (multiple-value-bind (out err status)
        (call-cli (list exe "exec" name "bash" "-lc" line) :seconds seconds)
      (when (eq status :timeout)
        (fail "the command did not finish in ~a seconds and was killed: it may have changed the sandbox before it died, so read it before repeating it" seconds))
      (list :exit-code status :output (concatenate 'string (or out "") (or err ""))))))

(define-verb sandboxes (&key all)
  "The sandbox containers this cell started, or with ALL every container the
runtime holds, as the `wslc ps' rows."
  (let ((rows (sandbox-rows :all all)))
    (if all
        rows
        (remove-if-not (lambda (row) (own-name-p (or (field row "Names") ""))) rows))))

(define-verb remove-sandbox (name)
  "Remove NAME, one of this cell's own containers, answering whether it went."
  (unless (own-name-p name)
    (fail "~s is not a sandbox this cell started: only a name beginning ~a- is its own to remove"
          name +name-prefix+))
  (let ((gone (remove-container (binary) name)))
    (when (and gone (equal name *sandbox*))
      (setf *made* nil))
    gone))

;;; --- the container's lifecycle --------------------------------------------------------

(defun setting-persistent-p ()
  "Whether this run's sandbox keeps its container between sessions."
  (setting :persistent))

(defun container-env ()
  "The container's variables: the host ones `forward_env' names, then the
section's own `env', which wins."
  (let ((pairs '()))
    (dolist (name (setting :forward-env))
      (let ((value (and (stringp name) (uiop:getenv name))))
        (when value (push (cons name value) pairs))))
    (append (nreverse pairs) (setting :env))))

(defun create-sandbox (exe name)
  "Create this run's container from the section's image; answer its name."
  (let ((argv (build-run-argv exe name (setting :image)
                               :workdir (setting :cwd)
                               :env (container-env)
                               :volumes (setting :volumes)
                               :cpus (setting :cpus)
                               :memory-mb (setting :memory-mb)
                               :network (setting :network)
                               :labels (list (cons +name-prefix+ "1")
                                             (cons (concatenate 'string +name-prefix+ ".sandbox")
                                                   (safe-name (setting :sandbox)))))))
    (multiple-value-bind (out err status) (call-cli argv :seconds 300)
      (declare (ignore out))
      (unless (eql 0 status)
        (fail "wslc could not start a sandbox from image ~s (exit ~a~@[: ~a~]): the image has to be a Linux image, and the runtime has to be able to pull it from its own registry configuration"
              (setting :image) status (first-line err)))
      (setf *made* t)
      name)))

(defun ensure-sandbox ()
  "The sandbox's container name: adopted when it is there and running,
started when it is stopped, created otherwise."
  (let* ((exe (binary))
         (name (sandbox-name))
         (state (inspect-container exe name)))
    (cond
      ((and state (container-running-p state))
       (setf *made* t)
       name)
      ((and state (not (setting-persistent-p)))
       ;; a non-persistent name is unique per start, so whatever is under it
       ;; is the leftover of a run that died before its teardown
       (remove-container exe name)
       (create-sandbox exe name))
      (state
       (start-container exe name)
       (setf *made* t)
       name)
      (t (create-sandbox exe name)))))

(defun sandbox-name ()
  "The name this run's container has, or is to have."
  (or *sandbox*
      (setf *sandbox* (sandbox-container-name (setting :sandbox)
                                               :persistent (setting-persistent-p)))))

(defun sandbox-state-text (exe name)
  "NAME's state, as one word for the operator."
  (let ((state (handler-case (inspect-container exe name)
                 (wslc-error () nil))))
    (cond ((null state) "not created yet")
          ((container-running-p state) "running")
          (t "stopped, its files kept"))))

;;; --- the operator's command -------------------------------------------------------------

(defun status-text ()
  "What /wslc answers with no word: the CLI, the section, and the sandbox's state."
  (let* ((exe (binary))
         (missing (missing-hardening-flags exe)))
    (format nil "wslc: ~a~%version ~a~%image ~a~%sandbox ~a: ~a~%cwd ~a; ~:[a fresh container each start, removed at the end of it~;stopped at the end of a session, its files kept~]~@[~%not hardened like Docker: ~{~a~^ ~} are not there, so dangerous-command approval stays on~]~@[~%bind mounts, read-write unless :ro: ~{~a~^, ~}~]"
            exe (or (version exe) "not answering") (setting :image) (sandbox-name)
            (sandbox-state-text exe (sandbox-name)) (setting :cwd) (setting-persistent-p)
            missing (setting :volumes))))

(defun containers-text (&key all)
  "The runtime's containers, one a line."
  (let ((rows (sandbox-rows :all all)))
    (if rows
        (format nil "~{~a~^~%~}" (mapcar #'row-line rows))
        (if all
            "wslc: the runtime reports no containers"
            "wslc: no sandbox of this cell is on the runtime yet; the first command creates one"))))

(defun run-command (args session-id)
  "The answer to one /wslc invocation: its state, its containers, a start, or
the removal of one it started."
  (declare (ignore session-id))
  (let* ((words (uiop:split-string (or (nlk:trimmed (or args "")) "")
                                   :separator '(#\Space #\Tab)))
         (word (string-downcase (or (first words) ""))))
    (handler-case
        (cond ((member word '("" "status") :test #'equal) (status-text))
              ((equal word "ps") (containers-text :all t))
              ((equal word "mine") (containers-text))
              ((equal word "start") (ensure-sandbox) (status-text))
              ((equal word "rm")
               (let ((name (second words)))
                 (if name
                     (format nil "wslc: ~a" (if (remove-sandbox name)
                                                  (format nil "~a is gone" name)
                                                  (format nil "~a could not be removed" name)))
                     "wslc: rm needs a container name; /wslc ps lists them")))
              (t (format nil "wslc: ~a is not a word it knows: status, ps, mine, start, rm NAME" word)))
      (wslc-error (condition)
        (format nil "wslc: ~a" condition)))))

;;; --- start ---------------------------------------------------------------------------------

(defun read-settings (values table)
  "VALUES, the declaration's own, plus :ENV: the object `env' carries, as
(NAME . VALUE) pairs, which the section declaration cannot type."
  (list* :env (env-object table) values))

(defun env-object (table)
  "TABLE's `env' object as (NAME . VALUE) pairs, or NIL."
  (let ((object (and (hash-table-p table) (gethash "env" table))))
    (when (hash-table-p object)
      (loop for name being the hash-keys of object using (hash-value value)
            when (and (stringp name) (plusp (length name)))
              collect (cons name (if (stringp value) value (princ-to-string value)))))))

(defun teardown ()
  "End this run's container: stopped when the sandbox is persistent, removed
when it is not. Nothing when this run never made one."
  (when (and *made* *sandbox*)
    (handler-case
        (let ((exe (binary)))
          (if (setting-persistent-p)
              (stop-container exe *sandbox*)
              (remove-container exe *sandbox*)))
      (wslc-error (condition)
        (nle:notice (format nil "wslc: ~a" condition) :level :warning :key "nodecode-wslc")))
    (setf *made* nil)))

(defun install ()
  "Refuse without an image, name the sandbox, and say which CLI drives it;
stopping ends the container the section says to."
  (unless (plusp (length (or (setting :image) "")))
    (nlk:config-error "image names no container image: set wslc.image to a Linux image the WSL Containers runtime can pull, e.g. \"alpine:3.20\""))
  (setf *binary* nil
        *made* nil
        *sandbox* (sandbox-container-name (setting :sandbox)
                                           :persistent (setting-persistent-p)))
  (nle:on-stop (lambda ()
                 (teardown)
                 (setf *sandbox* nil *binary* nil)
                 (nle:notice nil :key "nodecode-wslc")))
  (handler-case
      (let ((exe (binary)))
        (nle:notice (format nil "wslc: commands run in sandbox ~a, from image ~a"
                            *sandbox* (setting :image))
                    :key "nodecode-wslc"))
    (wslc-error (condition)
      (nle:notice (format nil "wslc: ~a" condition) :level :warning :key "nodecode-wslc"))))

(defun exec-tool (request)
  "The wslc_exec tool: one command in the sandbox, its output and exit status."
  (command-text (exec (sandbox-command-command request)
                      :cwd (sandbox-command-cwd request)
                      :timeout (sandbox-command-timeout request))))

(defun containers-tool (request)
  "The wslc_containers tool: the runtime's containers, one a line."
  (containers-text :all (sandbox-list-all request)))

(defun remove-tool (request)
  "The wslc_remove tool: remove one container this cell started."
  (let ((name (sandbox-target-name request)))
    (if (remove-sandbox name)
        (format nil "wslc: ~a is gone" name)
        (format nil "wslc: ~a could not be removed" name))))

(nle:define-cell wslc
  (:section ("wslc")
    (:guide "image is the Linux image the sandbox runs (e.g. alpine:3.20); binary names the wslc CLI when it is not on PATH; persistent keeps the container between sessions, stopped rather than removed; an env object adds variables of its own, which win over forward_env")
    ("image" :string :required t
     :doc "the Linux image the sandbox runs; a Windows image is refused by the runtime")
    ("binary" :path :doc "the wslc executable; unset, PATH is searched, then the default WSL install location")
    ("sandbox" :string :default "default" :doc "this sandbox's own name; it is part of the container's name")
    ("persistent" :boolean :default t :doc "keep the container between sessions, stopped rather than removed")
    ("network" :boolean :default t :doc "let the sandbox reach the network; off runs it with --network none")
    ("volumes" :list :doc "bind mounts, as wslc takes them: host:container[:ro]")
    ("forward_env" :list :doc "names of host environment variables to copy into the container")
    ("cpus" :integer :default 0 :min 0 :doc "cores for the container; 0 leaves it to the runtime")
    ("memory_mb" :integer :default 0 :min 0 :doc "memory ceiling in MB; 0 leaves it to the runtime")
    ("cwd" :string :default "/root" :doc "the directory commands start in inside the container")
    ("timeout_seconds" :integer :default 120 :min 1 :doc "how long one command may run, in seconds"))
  (:settings 'read-settings)
  (:tool "wslc_exec" (nlk:contract 'sandbox-command) 'exec-tool)
  (:tool "wslc_containers" (nlk:contract 'sandbox-list) 'containers-tool)
  (:tool "wslc_remove" (nlk:contract 'sandbox-target) 'remove-tool)
  (:help :wslc
         "wslc: wslc_exec runs one command in a Linux container on the WSL Containers runtime, through the wslc CLI"
         +primer+)
  (:command "wslc" 'run-command
            :description "The WSL Containers sandbox: its state, its containers, a start, a removal")
  (:start #'install))
