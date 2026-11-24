;;;; cell.lisp --- the whole cell: one :TOOL hook that sends a shell command
;;;; through the rtk binary before it runs, for the backends a profile opts in
;;;; to.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A Nodecode rewrite of petertaras/hermes-rtk-rewrite-with-docker-support
;;;; (Apache-2.0), itself a fork of kerrz2020/hermes-rtk-rewrite, of the RTK
;;;; project's own Hermes hook. The original registers ONE hook, pre_tool_call,
;;;; and for a terminal call hands the command string to `rtk rewrite'; when RTK
;;;; answers an accepted rewrite (exit 0 or 3) the call runs the rewritten
;;;; command (__init__.py:199-222). It fails open throughout: no binary on PATH,
;;;; a timeout, an exit code outside the protocol or an exception of ours leaves
;;;; the command exactly as written.
;;;;
;;;; The fork's own addition is the backend gate (__init__.py:19-27, 98-136):
;;;; a rewritten command needs `rtk' on the machine that RUNS it, so a command
;;;; whose own program hands the work to a container or another host -- docker,
;;;; podman, kubectl, ssh -- is rewritten only when the config opts that backend
;;;; in; the profile's default is local commands only. Nodecode has one shell
;;;; where Hermes reads its terminal policy, so the backend is read off the
;;;; command itself: its first program, after the prefixes that only pass a
;;;; command along (sudo, env, a VAR=value assignment).
;;;;
;;;; Where the original sees Hermes's terminal tool, this sees the core's
;;;; :TOOL point -- the one seam every tool call crosses -- so it rewrites the
;;;; `command' argument of a shell tool and the (sh "...") forms of an eval
;;;; snippet, the two ways a command is written here. All rewrite logic is
;;;; RTK's; the cell only runs it, as the original does.

(defpackage #:nodecode-rtk-backends
  (:use #:cl)
  ;; The slash command's entry point, for the tests and for anyone reading the
  ;; manual; START-CELL the loader finds for itself.
  (:export #:report))

(in-package #:nodecode-rtk-backends)

(nlk:define-peripheral rtk-backends :not-running t)

(defparameter *shell-tools* '("terminal" "sh" "bash" "shell" "exec_command" "run")
  "Tool names whose COMMAND argument is the command that runs, as the
original's host adapters name them. A tool of another name is rewritten only
when the config lists it in extra_tools.")

(defparameter *extra-tools* nil
  "The configured extra_tools, materialized at start.")

(defparameter *seen* 0
  "Commands the hook has looked at since the start.")
(defparameter *rewritten* 0
  "How many of those rtk rewrote.")
(defparameter *passthrough* 0
  "How many rtk left alone, by the protocol's expected codes (1 and 2).")
(defparameter *failed* 0
  "How many rtk answered with a code outside the protocol.")
(defparameter *skipped* 0
  "How many were left alone because their backend is not opted in -- the
fork's default, local commands only (__init__.py:206-213).")

(defparameter *available* :unknown
  "T when the binary answered a probe, NIL when it could not be started, and
:UNKNOWN until the first probe -- the original's shutil.which once per start
(__init__.py:25-33), here re-askable through /rtk check.")

(defparameter *version* nil
  "The version line the binary answered when it was probed, or NIL.")

;;; --- running the binary ----------------------------------------------------

(defun rtk-run (seconds &rest argv)
  "Run the rtk binary with ARGV under SECONDS, no shell in between. => (values
OUTPUT STATUS ERROR-P), ERROR-P when the binary could not be started at all."
  (handler-case
      (multiple-value-bind (out error status)
          (nlk:run-bounded (cons (setting :binary) argv) :seconds seconds)
        (declare (ignore error))
        (values (or out "") status nil))
    (error () (values nil nil t))))

(defun first-line (text)
  (let ((end (position #\Newline text)))
    (string-trim '(#\Space #\Tab #\Return)
                 (if end (subseq text 0 end) text))))

(defun probe ()
  "Whether the binary answers, and its version line when it does."
  (multiple-value-bind (out status error-p) (rtk-run (max 5 (setting :timeout-seconds)) "--version")
    (setf *available* (and (not error-p) (integerp status) (zerop status) t)
          *version* (and *available* (first-line out)))
    *available*))

(defun available-p ()
  (if (eq *available* :unknown) (probe) *available*))

(defun rewrite-command (command)
  "COMMAND as rtk rewrites it, or NIL when rtk leaves it alone, cannot be
reached, or answers a code the original treats as a failure (__init__.py:63-79)."
  (when (and (stringp command) (plusp (length (string-trim '(#\Space #\Tab #\Newline) command))))
    (incf *seen*)
    (cond
      ;; The fork's gate: a command that runs in a backend nobody opted in for
      ;; is passed through unasked, so rtk is never put inside a container or
      ;; another host that may not carry it (__init__.py:206-213).
      ((not (backend-opted-in-p (command-backend command)))
       (incf *skipped*) nil)
      ((available-p)
      (multiple-value-bind (out status error-p)
          (rtk-run (setting :timeout-seconds) "rewrite" command)
        (let ((new (and out (string-trim '(#\Space #\Tab #\Newline #\Return) out))))
          (cond
            ;; 0 and 3 are the protocol's "rewrite accepted".
            ((and (not error-p) (member status '(0 3)))
             (if (and new (plusp (length new)) (not (string= new command)))
                 (progn (incf *rewritten*) new)
                 (progn (incf *passthrough*) nil)))
            ;; 1 and 2 are the expected passthrough: the command runs as written.
            ((member status '(1 2)) (incf *passthrough*) nil)
            (t (incf *failed*)
               (when error-p (setf *available* nil))
               nil))))))))

;;; --- reading a Lisp string literal ------------------------------------------

(defun literal-end (text open)
  "The index just past the string literal TEXT opens at OPEN, or NIL when it
never closes. A backslash escapes the character after it, as the reader reads it."
  (let ((i (1+ open)) (limit (length text)))
    (loop while (< i limit)
          do (let ((c (char text i)))
               (cond ((char= c #\\) (incf i 2))
                     ((char= c #\") (return (1+ i)))
                     (t (incf i))))
          finally (return nil))))

(defun decode-literal (text start end)
  "TEXT between START and END read as a string literal's body: a backslash
escapes the character after it and nothing else, which is how this reader
reads one."
  (with-output-to-string (out)
    (let ((i start))
      (loop while (< i end)
            do (let ((c (char text i)))
                 (cond ((and (char= c #\\) (< (1+ i) end))
                        (write-char (char text (1+ i)) out) (incf i 2))
                       (t (write-char c out) (incf i))))))))

(defun encode-literal (string)
  "STRING as the body of a Lisp string literal: only its quote and its
backslash need an escape."
  (with-output-to-string (out)
    (loop for c across string
          do (case c
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (t (write-char c out))))))

;;; --- finding the commands in a snippet -------------------------------------

(defparameter +shell-call+
  (ppcre:create-scanner
   "^[ \\t]*\\((?:[[:alnum:]_.:+-]+:)?(?:sh|shell|bash|run)(?=[ \\t)])"
   :case-insensitive-mode t :multi-line-mode t)
  "A line whose first form opens with a shell verb: the shape a command takes
in an eval snippet, (sh \"...\"), wherever it sits in the snippet.")

(defparameter +strings-argument+
  (ppcre:create-scanner "^[(][ \\t]*(?:[[:alnum:]_.:+-]+:)?\\$[ \\t]+\"([^\"\\\\]*)\""
                         :case-insensitive-mode t)
  "(sh ($ \"key\")): the command is the strings entry KEY, not a literal.")

(defun skip-blanks (text start)
  (let ((i start) (limit (length text)))
    (loop while (and (< i limit) (member (char text i) '(#\Space #\Tab #\Newline #\Return)))
          do (incf i))
    (min i limit)))

(defun strings-call-key (text start)
  "The entry name a (sh ($ \"key\")) call at START names, or NIL."
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings +strings-argument+ (subseq text start))
    (and match groups (aref groups 0))))

(defun apply-edits (text edits)
  "TEXT with each (START END REPLACEMENT) of EDITS spliced in, in order."
  (with-output-to-string (out)
    (let ((pos 0))
      (dolist (edit edits)
        (destructuring-bind (start end replacement) edit
          (write-string text out :start pos :end start)
          (write-string replacement out)
          (setf pos end)))
      (write-string text out :start pos))))

(defun rewrite-form (text strings)
  "TEXT with the command of every toplevel shell call rewritten, and STRINGS
-- the call's own strings object -- with the entry a (sh ($ \"key\")) names
rewritten in place. Answers the new TEXT, or NIL when no command changed."
  (let ((edits '()) (pos 0) (limit (length text)))
    (loop
      (multiple-value-bind (start end) (ppcre:scan +shell-call+ text :start pos)
        (unless start (return))
        (setf pos (max (1+ end) end))
        (let ((argument (skip-blanks text end)))
          (cond
            ;; (sh "command" ...): the literal itself is the command.
            ((and (< argument limit) (char= #\" (char text argument)))
             (let ((close (literal-end text argument)))
               (when close
                 (setf pos close)
                 (let ((new (rewrite-command (decode-literal text (1+ argument) (1- close)))))
                   (when new
                     (push (list argument close (format nil "\"~a\"" (encode-literal new))) edits))))))
            ;; (sh ($ "key")): the command is the strings object's entry.
            ((and (< argument limit) (char= #\( (char text argument)) (hash-table-p strings))
             (let* ((key (strings-call-key text argument))
                    (old (and key (gethash key strings)))
                    (new (and (stringp old) (rewrite-command old))))
               (when new (setf (gethash key strings) new))))))))
    (when edits (apply-edits text (nreverse edits)))))

;;; --- which backend runs the command -----------------------------------------

(defparameter *transparent-programs* '("sudo" "doas" "env" "nohup" "time" "exec" "command" "builtin")
  "Programs that only pass a command along: the command's program is the one
after them, so `sudo docker ps' is a docker command.")

(defparameter *backend-programs*
  '(("docker" . "docker") ("docker-compose" . "docker") ("podman" . "podman")
    ("podman-compose" . "podman") ("nerdctl" . "docker") ("kubectl" . "kubectl")
    ("oc" . "kubectl") ("ssh" . "ssh") ("lxc" . "lxc") ("distrobox" . "distrobox")
    ("lima" . "lima") ("colima" . "colima") ("daytona" . "daytona") ("modal" . "modal")
    ("apptainer" . "apptainer") ("singularity" . "apptainer"))
  "Programs whose arguments run in another backend, and the backend name each
is opted in by -- the fork's TERMINAL_ENV names (__init__.py:19-27).")

(defun program-stem (word)
  "WORD's program name, without its directory: /usr/bin/docker => docker."
  (string-downcase (subseq word (1+ (or (position #\/ word :from-end t) -1)))))

(defun assignment-p (word)
  "Whether WORD is a VAR=value assignment, which is no program."
  (let ((equals (position #\= word)))
    (and equals (plusp equals)
         (every (lambda (c) (or (alphanumericp c) (member c '(#\_ #\-))))
                (subseq word 0 equals)))))

(defun command-backend (command)
  "The backend COMMAND's own program runs in: \"local\" for everything that
is not a container or a remote host, and that backend's own name when it is
(__init__.py:98-136, read off the command where Hermes reads its profile)."
  (let ((words (uiop:split-string (string-trim '(#\Space #\Tab #\Newline #\Return) command)
                                   :separator '(#\Space #\Tab #\Newline #\Return))))
    (loop for word in words
          for stem = (program-stem word)
          do (cond ((or (zerop (length word)) (assignment-p word)))   ; nothing, or VAR=value
                   ((member stem *transparent-programs* :test #'string=))
                   ((char= #\- (char word 0)))                  ; an option of a prefix
                   (t (return (or (cdr (assoc stem *backend-programs* :test #'string=))
                                  "local"))))
          finally (return "local"))))

(defun backend-opted-in-p (backend &optional (backends (setting :backends)))
  "Whether BACKENDS names BACKEND; `all' names every one. Unknown is not
opted in, as the original's unknown backend never rewrites (__init__.py:135-139)."
  (and (stringp backend)
       (plusp (length backend))
       (some (lambda (name)
               (let ((name (string-downcase (string-trim '(#\Space #\Tab) name))))
                 (or (string= name "all") (string= name backend))))
             backends)))

;;; --- the hook -------------------------------------------------------------

(defun rewrite-call (name arguments)
  "Rewrite the command ARGUMENTS carry, in place. => whether anything changed."
  (when (and (stringp name) (hash-table-p arguments))
    (cond
      ((or (member name *shell-tools* :test #'string=)
           (member name *extra-tools* :test #'string=))
       (let ((command (gethash "command" arguments)))
         (when (stringp command)
           (let ((new (rewrite-command command)))
             (when new (setf (gethash "command" arguments) new) t)))))
      ((and (string= name "eval") (setting :rewrite-eval))
       (let ((form (gethash "form" arguments))
             (strings (gethash "strings" arguments)))
         (when (stringp form)
           (let ((new (rewrite-form form (and (hash-table-p strings) strings))))
             (when new (setf (gethash "form" arguments) new) t))))))))

(defun rewrite-tool-call (op next)
  "The :TOOL point: rewrite the command before the shell runs it, then run the
call. Anything that goes wrong leaves the call exactly as the model wrote it --
the original's fail-open rule (__init__.py:44-80)."
  (when *rtk-backends*
    (handler-case (rewrite-call (getf op :name) (getf op :arguments))
      (error () nil)))
  (funcall next op))

;;; --- /rtk-backends ---------------------------------------------------------

(defun status-line ()
  (if (available-p)
      (format nil "binary ~a~@[ (~a)~]" (setting :binary) *version*)
      (format nil "binary ~a does not answer, so commands run as written" (setting :binary))))

(defun backends-line ()
  (format nil "backends ~{~a~^, ~}~@[ (and all)~]"
          (setting :backends)
          (member "all" (setting :backends) :test #'string=)))

(defun report (args session-id)
  "One /rtk-backends: what the hook has rewritten since the start, for which
backends, and whether the binary answers. ARGS `check' probes the binary again;
`reset' zeroes the counts. ARGS naming a backend answers whether it is opted in."
  (declare (ignore session-id))
  (running-settings)
  (let ((argument (and (stringp args)
                       (string-downcase (string-trim '(#\Space #\Tab) args)))))
    (cond
      ((equal argument "check")
       (setf *available* :unknown *version* nil)
       (if (available-p)
           (format nil "rtk-backends: ~a answers~@[ (~a)~]; ~a; a command of another backend is rewritten only when it is opted in"
                   (setting :binary) *version* (backends-line))
           (format nil "rtk-backends: ~a did not answer -- install the rtk binary, or point `binary' at it; ~
                        every command runs as written"
                   (setting :binary))))
      ((equal argument "reset")
       (setf *seen* 0 *rewritten* 0 *passthrough* 0 *failed* 0 *skipped* 0)
       "rtk-backends: counts reset")
      ((plusp (length argument))
       (if (backend-opted-in-p argument)
           (format nil "rtk-backends: ~a is opted in~@[ -- `rtk' must be installed there~]"
                   argument (not (string= argument "local")))
           (format nil "rtk-backends: ~a is not opted in, so its commands run as written; ~
                        add it to `backends' in the rtk-backends section"
                   argument)))
      (t
       (format nil "rtk-backends: rewrote ~:d of ~:d command~:p it looked at (~:d left as written, ~
                    ~:d in a backend not opted in, ~:d answered something unexpected); ~a; ~a; ~
                    ~:[eval snippets are left alone~;eval snippets are rewritten too~]"
               *rewritten* *seen* *passthrough* *skipped* *failed*
               (status-line) (backends-line) (setting :rewrite-eval))))))

(nle:define-cell rtk-backends
  (:section ("rtk-backends")
    (:guide "install the rtk binary and it works for local commands: `rtk --version' answers, and ~
             a command is rewritten before it runs; add `docker', `kubectl', `ssh' or `all' to ~
             backends once rtk is installed IN those backends too, set binary when rtk is not on ~
             PATH, and list extra_tools for a shell tool of another name")
    ("binary"          :string  :default "rtk" :doc "the rtk program, searched on PATH")
    ("timeout_seconds" :integer :default 2 :min 1 :doc "one rewrite's ceiling; past it the command runs as written")
    ("rewrite_eval"    :boolean :default t :doc "rewrite (sh \"...\") calls inside an eval snippet")
    ("extra_tools"     :list :doc "more tool names whose \"command\" argument is rewritten")
    ("backends"        :list :default (list "local")
     :doc "backends whose commands may be rewritten: local, docker, podman, kubectl, ssh, or all; rtk must be installed in each"))
  (:hook :tool #'rewrite-tool-call)
  (:command "rtk-backends" 'report
            :description "What command rewriting has done, for which backends, and whether rtk answers"
            :argument-hint "[check|reset|BACKEND]")
  (:help :rtk-backends
         "rtk-backends: shell commands rewritten through the rtk binary, for the backends you opt in to"
         "A shell command written here -- the \"command\" argument of a shell tool, or a (sh \"...\") form in an eval snippet -- is handed to `rtk rewrite' before it runs; when rtk accepts a rewrite (exit 0 or 3) the call runs the rewritten command, so rtks compacted output is what the model reads. The backend gate is the fork's: a rewritten command needs rtk on the machine that runs it, so a command whose own program hands the work to a container or another host (docker, podman, kubectl, ssh) is rewritten only when `backends' opts that backend in -- by default only local commands are. It fails open: a missing binary, a timeout past timeout_seconds, an exit outside the protocol or an error leaves the command exactly as written. The section `rtk-backends' sets binary, timeout_seconds, rewrite_eval, extra_tools and backends; /rtk-backends says what it has rewritten so far, /rtk-backends check probes the binary again, /rtk-backends reset zeroes the counts, and /rtk-backends docker asks whether one backend is opted in.")
  (:start (lambda ()
            (setf *extra-tools* (setting :extra-tools)
                  *seen* 0 *rewritten* 0 *passthrough* 0 *failed* 0 *skipped* 0
                  *available* :unknown *version* nil)
            ;; The original warns once, at registration, when the binary is absent.
            (unless (available-p)
              (nle:notice (format nil "rtk-backends: ~a does not answer -- shell commands run as written until ~
                                      it is installed; set `binary' in the rtk-backends section or put rtk ~
                                      on PATH, then /rtk-backends check"
                                   (setting :binary))
                          :level :warning :key "nodecode-rtk-backends"))
            (nle:on-stop
             (lambda ()
               (nle:notice nil :key "nodecode-rtk-backends")
               (setf *extra-tools* nil
                     *seen* 0 *rewritten* 0 *passthrough* 0 *failed* 0 *skipped* 0
                     *available* :unknown *version* nil))))))
