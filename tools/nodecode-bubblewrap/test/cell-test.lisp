;;;; cell-test.lisp --- the cell against a stubbed host.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here runs bwrap, reads a real home directory or starts a process:
;;;; *KIND*, *ENTRIES* and *RUN* are bound, so the argv the cell builds and
;;;; the text it answers are asserted offline. What is proved: the section (a
;;;; root a command could not run in refuses), the lifecycle, the two tools
;;;; through the dispatch the model calls them by, the /bubblewrap command,
;;;; and the argv itself -- the profiles, the writable directory, the hidden
;;;; credential stores, the limits, and the command at the end of it.

(in-package #:nodecode.test)

(define-test-slice "bubblewrap" "BUBBLEWRAP-CELL-"
  :start nodecode-bubblewrap:start-cell)

;;; --- the stubbed host --------------------------------------------------------------

(defparameter *runs* '()
  "Every run the stub answered, newest first, as (ARGV TIMEOUT).")

(defparameter *argv* nil
  "The argv of the newest run the stub answered.")

(defparameter *out* "total 0~%drwxr-xr-x 2 tester 4096 work~%"
  "What the stub's command prints.")

(defparameter *err* "")

(defparameter *code* 0)

(defparameter *timed* nil)

(defparameter *probe-fails* nil
  "When true the user-namespace probe answers a failure the way a locked-down host does.")

(defparameter *files*
  '(("/usr/bin/bwrap" . :file) ("/usr/bin/prlimit" . :file) ("/work" . :directory)
    ("/work/sub" . :directory)
    ("/home/tester/.ssh" . :directory) ("/home/tester/.aws" . :directory)
    ("/home/tester/.zz-tool" . :directory) ("/home/tester/.netrc" . :file)
    ("/home/tester/.config" . :directory) ("/home/tester/projects" . :directory)
    ("/home/tester/.config/gh" . :directory) ("/home/tester/.config/git" . :directory))
  "What the stubbed host has, as (PATH . :DIRECTORY|:FILE).")

(defparameter *listing*
  '(("/home/tester" (".ssh" . t) (".aws" . t) (".bashrc" . nil) (".gitconfig" . nil)
                    (".netrc" . nil) (".config" . t) (".zz-tool" . t) ("projects" . t))
    ("/home/tester/.config" ("nvim" . t) ("gh" . t) ("git" . t)))
  "The stubbed host's home directory entries.")

(defun stub-kind (path)
  (cdr (assoc path *files* :test #'string=)))

(defun stub-entries (directory)
  (cdr (assoc directory *listing* :test #'string=)))

(defun stub-run (argv timeout)
  "Answer ARGV the way this host would: a version, a probe, or the command."
  (push (list argv timeout) *runs*)
  (setf *argv* argv)
  (cond ((equal (last argv 1) '("--version")) (values "bubblewrap 0.11.1" "" 0 nil))
        ((member "true" argv :test #'equal)
         (if *probe-fails*
             (values "" "bwrap: setting up uid map: Permission denied" 1 nil)
             (values "" "" 0 nil)))
        (t (values *out* *err* *code* *timed*))))

(defparameter *out-default* *out*)

(defun pair-present-p (flag first-value &optional second-value)
  "Whether the newest argv carries FLAG FIRST-VALUE [SECOND-VALUE] at any position."
  (loop for rest on *argv*
        thereis (and (equal (first rest) flag)
                     (equal (second rest) first-value)
                     (or (null second-value) (equal (third rest) second-value)))))

(defun run-argument (flag &optional (offset 1))
  "The argument after the first FLAG in the newest argv."
  (let ((cell (member flag *argv* :test #'equal)))
    (and cell (nth offset cell))))

(defun run-before (flag)
  "The newest argv up to FLAG."
  (let ((cell (member flag *argv* :test #'equal)))
    (and cell (ldiff *argv* cell))))

(defun run-from (flag)
  "The newest argv from FLAG on."
  (member flag *argv* :test #'equal))

(defmacro with-host (&body body)
  "BODY with every seam of the host stubbed: three known programs, a fake home
directory, and a run that answers from the parameters above."
  `(let ((nodecode-bubblewrap::*home* "/home/tester")
         (nodecode-bubblewrap::*kind* #'stub-kind)
         (nodecode-bubblewrap::*entries* #'stub-entries)
         (nodecode-bubblewrap::*run* #'stub-run)
         (*runs* '()) (*argv* nil) (*out* *out-default*) (*err* "") (*code* 0)
         (*timed* nil) (*probe-fails* nil) (*home* "/home/tester"))

     ,@body))

(defmacro with-running (&body body)
  "BODY with the cell started on a root, a profile and the two programs, stopped on unwind."
  `(with-cell-stop ((bubblewrap-start "root" "/work" "bwrap_path" "/usr/bin/bwrap"
                                      "prlimit_path" "/usr/bin/prlimit"))
     ,@body))

(defun call-tool (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "bubble-wrap" name arguments)))

(defun command (&optional (arguments ""))
  "One /bubblewrap invocation, as the operator reads it."
  (nodecode-bubblewrap::run-command arguments nil))

;;; --- lifecycle ---------------------------------------------------------------------

(define-cell-lifecycle-tests "bubblewrap"
  (:config "root" "/work" "bwrap_path" "/usr/bin/bwrap" "prlimit_path" "/usr/bin/prlimit")
  (:fixture with-host)
  (:help :bubblewrap)
  (:command "bubblewrap")
  (:running
   (is (nle::find-tool "sandbox") "the sandbox tool is registered")
   (is (nle::find-tool "sandbox_status") "and the report on the box"))
  (:stopped
   (is (null (nle::find-tool "sandbox")) "and stop takes both back")
   (is (null (nle::find-tool "sandbox_status"))))
  (:refused ("root" "") ("root" "relative/dir") ("root" "/no/such/dir-here")
            ("profile" "elsewhere")))

(deftest bubblewrap-cell-a-box-with-no-root-refuses-before-anything-else (with-host)
  ;; The root is the one directory a command may write, so a cell without one
  ;; would run every command read-only without saying so: it refuses instead.
  (let ((condition (signals-error nlk:config-refusal
                      (bubblewrap-start "bwrap_path" "/usr/bin/bwrap"))))
    (is (search "root" (princ-to-string condition)) "the member to set is named")
    (is (null (nle::find-tool "sandbox")) "and nothing was installed")))

(deftest bubblewrap-cell-a-missing-program-is-a-standing-notice (with-host)
  (with-cell-stop ((bubblewrap-start "root" "/work" "bwrap_path" "/nope/bwrap"
                                     "prlimit_path" "/usr/bin/prlimit"))
    (is (nle::find-tool "sandbox") "the tool is there")
    (let ((answer (call-tool "sandbox" "{\"command\": \"ls\"}")))
      (is (search "install the bubblewrap package" answer) "the package is named"))))

;;; --- the argv: profiles, mounts, the command ------------------------------------------

(deftest bubblewrap-cell-the-default-profile-runs-writable-with-the-host-network (with-host)
  (with-running
    (call-tool "sandbox" "{\"command\": \"ls -la\"}")
    (is (equal "/usr/bin/prlimit" (first *argv*)) "the limits are set before bwrap starts")
    (is (member "--as=2147483648" *argv* :test #'equal) "2048 MB of address space by default")
    (is (member "--cpu=30" *argv* :test #'equal) "30 s of CPU by default")
    (is (member "--share-net" *argv* :test #'equal) "the network profile shares the host network")
    (is (pair-present-p "--bind-try" "/work" "/work") "the root is bound writable")
    (is (pair-present-p "--ro-bind" "/" "/") "and the host root is read-only")
    (is (equal '("sh" "-c" "ls -la") (last *argv* 3)) "the command is the last thing bwrap runs")))

(deftest bubblewrap-cell-workspace-has-no-network-and-restricted-writes-nothing (with-host)
  (with-running
    (call-tool "sandbox" "{\"command\": \"make\", \"profile\": \"workspace\"}")
    (is (not (member "--share-net" *argv* :test #'equal)) "workspace keeps loopback only")
    (is (member "--bind-try" *argv* :test #'equal) "and its working directory is writable")
    (call-tool "sandbox" "{\"command\": \"cat /etc/hostname\", \"profile\": \"restricted\"}")
    (is (not (member "--share-net" *argv* :test #'equal)) "restricted has no network either")
    (is (member "--ro-bind-try" *argv* :test #'equal) "and the root is only readable")
    (is (not (member "--bind-try" *argv* :test #'equal)) "so nothing is bound writable")))

(deftest bubblewrap-cell-credential-stores-are-hid-and-the-allowlist-is-shown (with-host)
  (with-running
    (call-tool "sandbox" "{\"command\": \"ls ~\"}")
    (is (pair-present-p "--tmpfs" "/home/tester/.ssh") "ssh keys do not exist in the box")
    (is (pair-present-p "--ro-bind" "/dev/null" "/home/tester/.netrc") "a credential file is empty")
    (is (pair-present-p "--tmpfs" "/home/tester/.zz-tool") "an unlisted dot entry is hidden")
    (is (not (find "/home/tester/.bashrc" *argv* :test #'equal)) "an allowlisted one stays")
    (is (not (find "/home/tester/projects" *argv* :test #'equal)) "and a non-dot entry stays")
    (is (pair-present-p "--tmpfs" "/home/tester/.config/gh") "a hidden child of .config")))

(deftest bubblewrap-cell-hide-and-home-default-deny-hide-more (with-host)
  (with-cell-stop ((bubblewrap-start "root" "/work" "bwrap_path" "/usr/bin/bwrap"
                                     "prlimit_path" "/usr/bin/prlimit"
                                     "home_default_deny" "all"
                                     "hide" (vector "/home/tester/projects")))
    (call-tool "sandbox" "{\"command\": \"ls ~\"}")
    (is (pair-present-p "--tmpfs" "/home/tester/projects") "the section's own path")
    (let ((bashrc (find "/home/tester/.bashrc" *argv* :test #'equal)))
      (is (null bashrc) "under all, an allowlisted entry is hidden, not masked")
      (is (pair-present-p "--tmpfs" "/home/tester/.ssh") "and the stores still are"))))

(deftest bubblewrap-cell-a-process-limit-runs-inside-the-box (with-host)
  (with-cell-stop ((bubblewrap-start "root" "/work" "bwrap_path" "/usr/bin/bwrap"
                                     "prlimit_path" "/usr/bin/prlimit"
                                     "memory_mb" 0 "cpu_seconds" 0 "max_procs" 0))
    (call-tool "sandbox" "{\"command\": \"ls\"}")
    (is (equal '("/usr/bin/bwrap") (run-before "--unshare-all")) "no prlimit in front")
    (is (equal "/usr/bin/bwrap" (first *argv*)) "so bwrap starts the box itself")
    (is (equal '("sh" "-c" "ls") (last *argv* 3)) "and no prlimit sits inside it"))
  (with-cell-stop ((bubblewrap-start "root" "/work" "bwrap_path" "/usr/bin/bwrap"
                                     "prlimit_path" "/usr/bin/prlimit" "max_procs" 64))
    (call-tool "sandbox" "{\"command\": \"ls\"}")
    (let ((inner (last *argv* 6)))
      (is (equal '("/usr/bin/prlimit" "--nproc=64" "--" "sh" "-c" "ls") inner) "set inside"))))

(deftest bubblewrap-cell-a-cwd-starts-under-the-root (with-host)
  (with-running
    (call-tool "sandbox" "{\"command\": \"ls\", \"cwd\": \"sub\"}")
    (is (equal "/work/sub" (run-argument "--chdir")) "a relative cwd starts under the root")
    (call-tool "sandbox" "{\"command\": \"ls\", \"cwd\": \"/work/sub\"}")
    (is (equal "/work/sub" (run-argument "--chdir")) "an absolute one is used as it is")))

;;; --- what the model reads back ---------------------------------------------------------

(deftest bubblewrap-cell-a-command-answers-its-output-and-its-exit (with-host)
  (with-running
    (let ((answer (call-tool "sandbox" "{\"command\": \"ls -la\"}")))
      (is (search "exit 0" answer) "the exit status is said")
      (is (search "drwxr-xr-x" answer) "and the command's own output reaches the model"))
    (setf *code* 2 *err* "ls: cannot access 'nope'" *out* "")
    (let ((answer (call-tool "sandbox" "{\"command\": \"ls nope\"}")))
      (is (search "exit 2" answer) "a failure says its status")
      (is (search "cannot access" answer) "with what the command complained about"))))

(deftest bubblewrap-cell-a-command-that-outlives-its-ceiling-says-so (with-host)
  (with-running
    (setf *timed* t *code* 124 *out* "")
    (let ((answer (call-tool "sandbox" "{\"command\": \"sleep 300\", \"timeout_seconds\": 5}")))
      (is (search "timed out after 5s" answer) "the ceiling that was hit is named")
      (is (eql 5 (second (first *runs*))) "and the run was given that ceiling"))))

(deftest bubblewrap-cell-a-call-without-a-command-refuses (with-host)
  (with-running
    (let ((payload (nlk:decode-json "{}")))
      (is (signals-error nlk:contract-refusal
            (nodecode-bubblewrap::decode-sandbox-call payload)) "the member is required"))
    (let ((answer (call-tool "sandbox" "{}")))
      (is (search "command" answer) "and the model is told which member"))))

;;; --- the report --------------------------------------------------------------------------

(deftest bubblewrap-cell-the-status-tool-reports-the-box (with-host)
  (with-running
    (let ((answer (call-tool "sandbox_status" "{}")))
      (is (search "bubblewrap 0.11.1" answer) "the bwrap version is said")
      (is (search "probe:    ok" answer) "and the user-namespace probe")
      (is (search "network - writable working directory" answer) "the profile in force")
      (is (search "/work (the working directory)" answer) "what a command may write")
      (is (search "/home/tester/.ssh" answer) "and what is hidden from it")
      (is (search "2048 MB of address space" answer) "the limits it runs under"))))

(deftest bubblewrap-cell-a-failing-probe-is-a-failure-with-a-remedy (with-host)
  (with-running
    (setf *probe-fails* t)
    (let ((answer (call-tool "sandbox_status" "{}")))
      (is (search "probe:    failed" answer) "a probe that fails says so")
      (is (search "unprivileged user namespaces" answer) "and what to allow"))))

;;; --- the operator's command ----------------------------------------------------------------

(deftest bubblewrap-cell-the-command-reports-and-runs (with-host)
  (with-running
    (is (search "Bubblewrap sandbox" (command)) "no argument answers the report")
    (is (search "drwxr-xr-x" (command "uname -a")) "an argument is run inside the box")
    (is (equal '("sh" "-c" "uname -a") (last *argv* 3)) "as the command bwrap runs")))
