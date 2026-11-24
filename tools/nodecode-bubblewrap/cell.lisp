;;;; cell.lisp --- the sandbox: what a command may see, and the two tools.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin was a terminal backend: every terminal and
;;;; execute_code command of the harness ran inside its own bwrap sandbox on
;;;; the host, at the host's own paths. This cell builds the same argv and
;;;; gives the model the box as a tool it calls (bubblewrap: allows a shell
;;;; command, a probe of the sandbox itself), with the boundary on the manual
;;;; as (help :bubblewrap).
;;;;
;;;; The rules of the original, kept:
;;;;   read-only root at the host's paths, one writable directory;
;;;;   dot entries of the home directory hidden off an allowlist, credential
;;;;     stores always hidden;
;;;;   a fresh /dev, /proc and /tmp per command, /run/user emptied of agents;
;;;;   SSH_AUTH_SOCK, GPG_AGENT_INFO and DBUS_SESSION_BUS_ADDRESS removed;
;;;;   memory and CPU limits around bwrap, a process limit inside it;
;;;;   --die-with-parent and --new-session;
;;;;   three profiles: restricted, workspace, network (the default).

(in-package #:nodecode-bubblewrap)

;;; --- the manual -----------------------------------------------------------------

(defparameter +primer+
  "Every command you run through the sandbox tool -- or through (bubblewrap:sandbox \"...\")
in eval -- runs inside its own bubblewrap (bwrap) box on this machine, at the host's own paths.
Read this before you read a refusal as a broken host:

1. The filesystem is read-only except the one writable directory the section names (root),
   and the working directory only under the workspace and network profiles. A
   \"Read-only file system\" error is that boundary: report it, do not detour through sudo,
   another path or another tool.
2. The dot entries of the home directory are hidden unless they are on the allowlist, and
   the credential stores (.ssh, .aws, .gnupg, .kube, .docker, .netrc, .config/gh,
   .config/gcloud, the browser profiles, the keyrings) are hidden always: a missing or
   empty ~/.ssh is the box, not a broken account. An allowed entry is read-only, so a
   command cannot edit ~/.gitconfig or ~/.bashrc.
3. /tmp and /proc are private to the command and /run/user is empty, so ssh-agent,
   gpg-agent, keyring and D-Bus sockets are unreachable and the docker socket is gone.
   Each command's box ends with the command: background processes do not survive it.
4. The profile decides the network: network shares the host network with no egress filter,
   workspace and restricted have loopback only -- a refused connection there is the profile.
5. Memory, CPU and process limits are per command and deliberately tight; a compiler or a
   test suite that exceeds them is killed (exit 137, or a MemoryError). Say so instead of
   retrying the same command.
6. Everything outside what is hidden is readable: keep secrets in the hidden paths."
  "What (help :bubblewrap) answers while the cell runs: the original's prompt note, as a manual topic.")

;;; --- what a command may not see -----------------------------------------------------

(defparameter +credential-stores+
  '(".ssh" ".aws" ".azure" ".gnupg" ".kube" ".docker" ".netrc" ".npmrc" ".pypirc"
    ".pgpass" ".my.cnf" ".git-credentials" ".vault-token" ".terraform.d"
    ".password-store" ".config/gh" ".config/gcloud" ".config/op" ".config/sops"
    ".config/hub" ".config/age" ".local/share/keyrings" ".mozilla" ".thunderbird"
    ".config/google-chrome" ".config/chromium" ".config/BraveSoftware"
    ".config/keepassxc" ".electrum" ".bitcoin")
  "The stores hid whatever the allowlist says, as the original's own deny list has it.")

(defparameter +home-allow+
  '(".bashrc" ".bash_profile" ".bash_logout" ".profile" ".zshrc" ".zprofile" ".zshenv"
    ".gitconfig" ".cache" ".cargo" ".rustup" ".nvm" ".pyenv" ".bun" ".deno"
    ".local/bin" ".config/git" ".config/nvim" ".config/pip" ".config/npm")
  "The dot entries of the home directory a command may still see, read-only.")

(defparameter +socket-vars+ '("SSH_AUTH_SOCK" "GPG_AGENT_INFO" "DBUS_SESSION_BUS_ADDRESS")
  "Removed from the command's environment: a socket the box does not show.")

(defparameter +nested-homes+ '(".config" ".local" ".local/share")
  "Directories whose children follow the allowlist one level down.")

;;; --- the host, as this cell reads it (every seam a test binds) ----------------------

(defvar *home* nil
  "The home directory to hide things in; NIL reads HOME, then the user's own.")

;; The three function seams, *KIND*, *ENTRIES* and *RUN*, follow the
;; definitions they bind, below: a test rebinds them by LET.

(defun home-directory ()
  "The home directory this box hides things in, without a trailing slash."
  (string-right-trim
   "/" (or *home*
           (nonblank (uiop:getenv "HOME"))
           (namestring (user-homedir-pathname)))))

(defun host-kind (path)
  "What PATH is on this host: :DIRECTORY, :FILE, or NIL when it is not there."
  (let ((truename (ignore-errors (probe-file path))))
    (cond ((null truename) nil)
          ((uiop:directory-pathname-p truename) :directory)
          (t :file))))

(defun host-entries (directory)
  "The entries in DIRECTORY as (NAME . DIRECTORY-P), or NIL when it is not there."
  (let ((dir (uiop:ensure-directory-pathname (format nil "~a/" (string-right-trim "/" directory)))))
    (ignore-errors
     (append (loop for file in (uiop:directory-files dir)
                   collect (cons (file-namestring file) nil))
             (loop for sub in (uiop:subdirectories dir)
                   for name = (car (last (pathname-directory sub)))
                   when (stringp name) collect (cons name t))))))

(defun find-program (name)
  "NAME as the absolute path of an executable on this host, or NIL: PATH decides."
  (when (nonblank name)
    (if (find #\/ name)
        (and (funcall *kind* name) name)
        (loop for dir in (uiop:split-string (or (uiop:getenv "PATH") "")
                                            :separator '(#\:))
              for candidate = (join (if (string= dir "") "." dir) name)
              when (eq :file (funcall *kind* candidate))
                return candidate))))

(defun run-on-host (argv timeout)
  "Run ARGV with no shell of ours => (VALUES STDOUT STDERR EXIT TIMED-OUT-P).
EXIT is 124 for a command that outlived TIMEOUT seconds, 127 for one that
could not start."
  (handler-case
      (let* ((out (make-string-output-stream))
             (err (make-string-output-stream))
             (process (sb-ext:run-program (first argv) (rest argv)
                                          :search nil :wait nil :input nil
                                          :output out :error err))
             (deadline (+ (get-internal-real-time)
                           (* (max 1 timeout) internal-time-units-per-second)))
             (timed nil))
        (loop
          (cond ((not (sb-ext:process-alive-p process)) (return))
                ((> (get-internal-real-time) deadline)
                 (setf timed t)
                 (ignore-errors (sb-ext:process-kill process 9))
                 (return))
                (t (sleep 1/20))))
        (ignore-errors (sb-ext:process-wait process))
        (values (get-output-stream-string out) (get-output-stream-string err)
                (if timed 124 (or (sb-ext:process-exit-code process) 1))
                timed))
    (error (condition)
      (values "" (nlk:one-line (princ-to-string condition)) 127 nil))))

(defvar *kind* #'host-kind
  "How the cell asks what a path is: (funcall *KIND* PATH) => :DIRECTORY, :FILE or NIL.")

(defvar *entries* #'host-entries
  "How the cell lists a directory: (funcall *ENTRIES* DIR) => ((NAME . DIRECTORY-P) ...).")

(defvar *run* #'run-on-host
  "How a sandbox, a version check and a probe are run:
(funcall *RUN* ARGV TIMEOUT) => (VALUES STDOUT STDERR EXIT TIMED-OUT-P).")

;;; --- small words -------------------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun join (directory name)
  (format nil "~a/~a" (string-right-trim "/" directory) name))

(defun clip (text limit)
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a~%... (~d more characters)" (subseq text 0 limit)
                (- (length text) limit)))))

(defmacro answering (&body body)
  "BODY's text; a refusal or a failed spawn becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (bubblewrap-error (condition)
       (nle:failure (format nil "ERROR: ~a" (bubblewrap-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: bubblewrap: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

;;; --- profiles ------------------------------------------------------------------------

(defparameter +profiles+
  '(("restricted" :writable nil :net nil)
    ("workspace" :writable t :net nil)
    ("network" :writable t :net t))
  "The original's three profiles: the working directory writable, and the host network.")

(defun profile-of (name)
  "The profile the section names, as (:NAME :WRITABLE :NET); network when unknown."
  (or (assoc (string-downcase (or name "network")) +profiles+ :test #'string=)
      (assoc "network" +profiles+ :test #'string=)))

(defun profile-words (profile)
  "PROFILE as the one line the operator and the model read."
  (format nil "~:[read-only working directory~;writable working directory~], ~
               ~:[no network~;the host network~]"
          (getf (rest profile) :writable)
          (getf (rest profile) :net)))

;;; --- what is hidden, and how it is hid ----------------------------------------------

(defun visible-name-p (name allow mode)
  "Whether the home directory's entry NAME stays visible: allowlisted, or a
non-dot entry under the DOT default."
  (cond ((member name allow :test #'string=) t)
        ((eq mode :all) nil)
        ((and (plusp (length name)) (char= (char name 0) #\.)) nil)
        (t t)))

(defun shown-parent-p (name allow)
  "Whether NAME stays visible because an allowlisted entry sits under it."
  (let ((prefix (format nil "~a/" name)))
    (some (lambda (entry) (and (> (length entry) (length prefix))
                               (string= prefix entry :end2 (length prefix))))
          allow)))

(defun hidden-paths (settings)
  "The paths a command must not see: the home directory's hidden entries, the
credential stores, and the section's own `hide' list. Absolute, no trailing slash."
  (let* ((home (home-directory))
         (mode (if (string-equal (or (getf settings :home-default-deny) "dot") "all") :all :dot))
         (allow (append +home-allow+ (getf settings :home-allow)))
         (hidden '()))
    (flet ((hide (path) (when (nonblank path) (pushnew path hidden :test #'string=))))
      (dolist (entry (funcall *entries* home))
        (let ((name (car entry)))
          (unless (or (visible-name-p name allow mode) (shown-parent-p name allow))
            (hide (join home name)))))
      (dolist (parent +nested-homes+)
        (when (shown-parent-p parent allow)
          (dolist (entry (funcall *entries* (join home parent)))
            (let ((relative (format nil "~a/~a" parent (car entry))))
              (unless (member relative allow :test #'string=)
                (hide (join home relative)))))))
      (dolist (store +credential-stores+) (hide (join home store)))
      (dolist (extra (getf settings :hide))
        (hide (if (and (plusp (length extra)) (char= (char extra 0) #\/))
                  (string-right-trim "/" extra)
                  (join home extra)))))
    (sort hidden #'string<)))

(defun mask-args (paths)
  "The bwrap mounts that hide PATHS: a fresh tmpfs over a directory, an empty
file bound over a file. A path that is not there is skipped."
  (loop for path in paths
        for kind = (funcall *kind* path)
        when kind
          append (if (eq kind :directory)
                     (list "--tmpfs" path)
                     (list "--ro-bind" "/dev/null" path))))

;;; --- the sandbox's argv ------------------------------------------------------------

(defun limit-argv (settings)
  "The prlimit prefix that sets memory and CPU before bwrap starts; NIL when both are off."
  (let ((memory (getf settings :memory-mb))
        (cpu (getf settings :cpu-seconds)))
    (if (or (and memory (plusp memory)) (and cpu (plusp cpu)))
        (append (list (getf settings :prlimit-path))
                (when (and memory (plusp memory))
                  (list (format nil "--as=~d" (* memory 1024 1024))))
                (when (and cpu (plusp cpu))
                  (list (format nil "--cpu=~d" cpu)))
                (list "--"))
        '())))

(defun inner-command (settings command)
  "The command the box runs: the process limit inside it, then a shell."
  (let ((procs (getf settings :max-procs)))
    (append (when (and procs (plusp procs))
              (list (getf settings :prlimit-path)
                    (format nil "--nproc=~d" procs) "--"))
            (list "sh" "-c" command))))

(defun start-directory (settings cwd)
  "Where a command starts: the section's root, or CWD under it."
  (let ((root (getf settings :root))
        (given (nonblank cwd)))
    (cond ((null given) root)
          ((char= (char given 0) #\/) given)
          (t (join root given)))))

(defun sandbox-argv (settings command cwd)
  "The argv that runs COMMAND inside a box made from SETTINGS, starting in CWD.
The order is the original's: limits in front of bwrap, then bwrap's own
unsharing, the read-only root and the writable directory, the hidden paths
masked last, then the command."
  (let* ((profile (profile-of (getf settings :profile)))
         (writable (getf (rest profile) :writable))
         (root (string-right-trim "/" (getf settings :root))))
    (append (limit-argv settings)
            (list (getf settings :bwrap-path) "--unshare-all" "--die-with-parent"
                  "--new-session" "--unshare-cgroup-try")
            (when (getf (rest profile) :net) (list "--share-net"))
            (loop for name in +socket-vars+ append (list "--unsetenv" name))
            (list "--ro-bind" "/" "/" "--dev" "/dev" "--proc" "/proc" "--tmpfs" "/tmp")
            (when (eq :directory (funcall *kind* "/run/user")) (list "--tmpfs" "/run/user"))
            (if writable
                (list "--bind-try" root root)
                (list "--ro-bind-try" root root))
            (loop for source in (getf settings :binds)
                  append (list "--ro-bind-try" source source))
            (loop for source in (getf settings :bind-rw)
                  append (list "--bind-try" source source))
            (mask-args (hidden-paths settings))
            (list "--chdir" (start-directory settings cwd)
                  "--setenv" "TMPDIR" "/tmp"
                  "--setenv" "TMP" "/tmp"
                  "--setenv" "TEMP" "/tmp"
                  "--")
            (inner-command settings command))))

;;; --- running one command -------------------------------------------------------------

(defun call-settings (profile)
  "The running settings, with an optional per-call PROFILE laid over them."
  (let ((settings (copy-list (running-settings))))
    (when (nonblank profile)
      (setf (getf settings :profile) (string-downcase profile)))
    settings))

(defun required-programs (settings)
  "BWAP and PRLIMIT as absolute paths, or the refusal naming the one that is missing."
  (let ((bwrap (find-program (getf settings :bwrap-path)))
        (prlimit (find-program (getf settings :prlimit-path))))
    (unless bwrap
      (fail "no bubblewrap: install the bubblewrap package (apt, dnf or pacman: bubblewrap) ~
             and put bwrap on PATH, then (restart-cells \"nodecode-bubblewrap\")"))
    (unless prlimit
      (fail "no prlimit: it comes with util-linux, which mainstream distributions install; ~
             put it on PATH, then (restart-cells \"nodecode-bubblewrap\")"))
    (list bwrap prlimit)))

(defun sandbox (command &key profile cwd timeout-seconds)
  "Run COMMAND inside its own bwrap sandbox and answer what it printed, with its
exit status. PROFILE overrides the section's for this one command."
  (let* ((settings (call-settings profile))
         (programs (required-programs settings))
         (timeout (or timeout-seconds (getf settings :timeout-seconds)))
         (limit (getf settings :max-output-chars)))
    ;; The absolute paths the lookup found, so PATH decides and the argv names
    ;; the program a probe would find.
    (setf (getf settings :bwrap-path) (first programs)
          (getf settings :prlimit-path) (second programs))
    (multiple-value-bind (out err code timed)
        (funcall *run* (sandbox-argv settings command cwd) timeout)
      (with-output-to-string (text)
        (format text "bwrap (~a): exit ~d~:[~;, timed out after ~ds~]~%"
                (first (profile-of (getf settings :profile))) code timed timeout)
        (unless (zerop (length out))
          (format text "~a~%" (clip out limit)))
        (when (nonblank err)
          (format text "stderr~%~a~%" (clip err limit)))
        (when (and (zerop (length out)) (not (nonblank err)))
          (format text "(no output)~%"))))))

;;; --- the sandbox's own report ------------------------------------------------------

(defun version-of (bwrap)
  "The version bwrap reports, or \"version unknown\"."
  (multiple-value-bind (out err code) (funcall *run* (list bwrap "--version") 5)
    (declare (ignore code))
    (or (nonblank out) (nonblank err) "version unknown")))

(defun probe-words (bwrap)
  "The original's probe: can this user start a bwrap namespace at all?"
  (multiple-value-bind (out err code)
      (funcall *run* (list bwrap "--unshare-user" "--ro-bind" "/" "/" "true") 10)
    (if (eql 0 code)
        "ok: unprivileged user namespaces work for this user"
        (format nil "failed: ~a (allow unprivileged user namespaces for this user)"
                (or (nonblank err) (nonblank out) (format nil "exit ~d" code))))))

(defun status (&key profile verbose)
  "What the sandbox is: the two programs, the probe, the profile, what is
writable, what is hidden, and the limits one command runs under."
  (let* ((settings (call-settings profile))
         (bwrap (find-program (getf settings :bwrap-path)))
         (prlimit (find-program (getf settings :prlimit-path)))
         (chosen (profile-of (getf settings :profile)))
         (hidden (remove-if-not (lambda (path) (funcall *kind* path))
                                 (hidden-paths settings))))
    (with-output-to-string (text)
      (format text "Bubblewrap sandbox~%")
      (if bwrap
          (format text "  bwrap:    ~a (~a)~%" bwrap (version-of bwrap))
          (format text "  bwrap:    not found: install the bubblewrap package (apt, dnf or pacman: bubblewrap)~%"))
      (if prlimit
          (format text "  prlimit:  ~a~%" prlimit)
          (format text "  prlimit:  not found: it comes with util-linux~%"))
      (when bwrap
        (format text "  probe:    ~a~%" (probe-words bwrap)))
      (format text "  profile:  ~a - ~a~%" (first chosen) (profile-words chosen))
      (format text "  writable: ~a~%"
              (if (getf (rest chosen) :writable)
                  (format nil "~a (the working directory)" (getf settings :root))
                  (format nil "nothing: ~a is read-only under this profile" (getf settings :root))))
      (format text "  read-only: everything else, at the host's own paths~%")
      (format text "  hidden:   ~a~%"
              (cond ((null hidden) "nothing (no home directory entries were found)")
                    ((and verbose (<= (length hidden) 40)) (format nil "~{~a~^, ~}" hidden))
                    (t (format nil "~{~a~^, ~}~@[, ... ~d more~]"
                               (subseq hidden 0 (min 8 (length hidden)))
                               (max 0 (- (length hidden) 8))))))
      (format text "  limits:   ~a~%"
              (let ((words
                      (remove nil
                              (list (let ((memory (getf settings :memory-mb)))
                                      (and memory (plusp memory)
                                           (format nil "~d MB of address space" memory)))
                                    (let ((cpu (getf settings :cpu-seconds)))
                                      (and cpu (plusp cpu) (format nil "~d s of CPU" cpu)))
                                    (let ((procs (getf settings :max-procs)))
                                      (and procs (plusp procs) (format nil "~d processes" procs)))))))
                (if words (format nil "~{~a~^, ~} per command" words)
                    "none: no limit on a command")))
      (when (or (getf settings :binds) (getf settings :bind-rw))
        (format text "  binds:    ~{~a~^, ~}~%"
                (append (getf settings :binds) (getf settings :bind-rw))))
      (unless (or bwrap prlimit)
        (format text "~%Install bubblewrap (bwrap) and util-linux's prlimit, then (restart-cells \"nodecode-bubblewrap\").~%")))))

;;; --- the tools ---------------------------------------------------------------------

(nlk:define-contract sandbox-call
  (:doc "Run one shell command inside its own bubblewrap sandbox on this machine: the host
filesystem read-only at the host's own paths, one writable directory, the home directory's
dot entries and the credential stores hidden, a fresh /tmp and /proc, /run/user emptied of
agents, and memory, CPU and process limits. Answers what the command printed and its exit
status. The command's sandbox ends with it, so background processes do not survive.")
  ("command" :text :required t :doc "the shell command, run by sh -c inside the box")
  ("profile" :text :options '("restricted" "workspace" "network")
             :doc "one call's profile: restricted (read-only, no network), workspace (writable, no network), network (writable, the host network); the section's when left out")
  ("cwd" :text :doc "where the command starts, inside the writable directory")
  ("timeout_seconds" :integer :doc "one command's ceiling, in seconds; the section's timeout_seconds when left out"))

(nlk:define-contract status-call
  (:doc "What the bubblewrap sandbox is on this machine: whether bwrap and prlimit are
installed, whether bwrap's user-namespace probe passes, which profile is in force, what a
command may write, what is hidden from it, and the limits it runs under.")
  ("profile" :text :options '("restricted" "workspace" "network")
             :doc "report this profile instead of the section's"))

(defun run-sandbox (call)
  "The sandbox tool: one command, inside its own box."
  (answering
    (sandbox (sandbox-call-command call)
             :profile (sandbox-call-profile call)
             :cwd (sandbox-call-cwd call)
             :timeout-seconds (sandbox-call-timeout-seconds call))))

(defun report-status (call)
  "The sandbox_status tool: what the box is, in words."
  (answering (status :profile (status-call-profile call) :verbose t)))

;;; --- the operator's command ----------------------------------------------------------

(defun run-command (args session-id)
  "One /bubblewrap invocation: with no argument the sandbox's report; with a
command, that command run inside the box and its output answered."
  (declare (ignore session-id))
  (answering
    (let ((wanted (nlk:trimmed (or args ""))))
      (if (zerop (length wanted))
          (status :verbose t)
          (sandbox wanted)))))

;;; --- the cell ------------------------------------------------------------------------

(defun check-config ()
  "Refuse a section whose root a command could not run in, naming the member to set."
  (flet ((refuse (format-control &rest arguments)
           (error 'nlk:config-refusal
                  :detail (format nil "bubblewrap: ~a; then (restart-cells \"nodecode-bubblewrap\")"
                                  (apply #'format nil format-control arguments)))))
    (unless (nonblank (setting :root))
      (refuse "root is empty: name the one directory commands may write, a project or a scratch directory"))
    (let ((root (setting :root)))
      (unless (char= (char root 0) #\/)
        (refuse "root is \"~a\": it must be an absolute path" root))
      (when (string= (string-right-trim "/" root) "")
        (refuse "root is \"/\": a command could write the whole host; name a project or a scratch directory"))
      (unless (eq :directory (funcall *kind* root))
        (refuse "root is \"~a\": there is no such directory" root)))))

(nle:define-cell bubblewrap
  (:section ("bubblewrap")
    (:guide "root is the one directory a command may write: a project or a scratch directory; profile decides the network and whether root is writable; bwrap comes from your distribution and prlimit from util-linux; then /bubblewrap reports the box")
    ("root" :string :required t
     :doc "the one directory commands may write, absolute: a project or a scratch directory")
    ("profile" :choice :options '("restricted" "workspace" "network") :default "network"
     :doc "restricted: read-only, no network; workspace: writable, no network; network: writable, the host network")
    ("memory_mb" :integer :default 2048 :min 0
     :doc "address space one command may use, in MB; 0 disables it")
    ("cpu_seconds" :integer :default 30 :min 0
     :doc "CPU time one command may use, in seconds; 0 disables it")
    ("max_procs" :integer :default 256 :min 0
     :doc "processes and threads one command may run; 0 disables it")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "how long one command may run before it is killed")
    ("max_output_chars" :integer :default 12000 :min 200
     :doc "how much of one command's output the model reads")
    ("home_default_deny" :choice :options '("dot" "all") :default "dot"
     :doc "dot: hide the home directory's dot entries off the allowlist; all: hide every entry off it")
    ("home_allow" :list
     :doc "more home directory entries to show read-only, as [\".zz-tool\", \".config/nvim\"]")
    ("hide" :list
     :doc "more paths to hide, absolute or relative to the home directory, as [\"~/Documents/keys\"]")
    ("binds" :list
     :doc "more host paths to mount read-only at their own paths, as [\"/srv/models\"]")
    ("bind_rw" :list
     :doc "more host paths to mount read-write at their own paths")
    ("bwrap_path" :string :default "bwrap" :doc "the bwrap executable, as a name on PATH or a path")
    ("prlimit_path" :string :default "prlimit" :doc "the prlimit executable, from util-linux"))
  (:help :bubblewrap
         "bubblewrap: run a command in its own bwrap sandbox - read-only host, secrets hidden, limits - (bubblewrap:sandbox \"make test\")"
         +primer+)
  (:command "bubblewrap" 'run-command
            :description "The bubblewrap sandbox: its report, or a command run inside it"
            :argument-hint "COMMAND"
            :session nil)
  (:tool "sandbox" (nlk:contract 'sandbox-call) 'run-sandbox)
  (:tool "sandbox_status" (nlk:contract 'status-call) 'report-status)
  (:start (lambda ()
            (check-config)
            ;; A box whose programs are missing still answers its tools, with the
            ;; degraded result the original gave: the package named, not a silent
            ;; command outside it. The standing notice says so until it is fixed.
            (unless (and (find-program (getf (running-settings) :bwrap-path))
                         (find-program (getf (running-settings) :prlimit-path)))
              (nle:notice "bubblewrap: bwrap or prlimit is not on PATH; commands cannot be sandboxed until one is installed"
                          :level :warning :key "nodecode-bubblewrap")
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-bubblewrap")))))))
