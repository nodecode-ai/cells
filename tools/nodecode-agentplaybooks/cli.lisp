;;;; cli.lisp --- one seam to the apb CLI: a list of arguments in, the text a
;;;; model reads out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin is prose: it asks a host agent to type `apb' commands
;;;; itself and to remember, every time, that a command without `--apply' only
;;;; shows its plan. Here the argv is built once, the preview is the default,
;;;; and the answer is the CLI's own output, cut at `max_chars'. Nothing goes
;;;; through a shell, so a guid, a profile name or a path is one argument
;;;; however it is spelled, and the CLI is the operator's own binary on PATH.

(in-package #:nodecode-agentplaybooks)

(defparameter +install-hint+
  "Install AgentPlaybooks' CLI separately -- `npm install -g @agentplaybooks/cli' with Node.js 20 or
later, then check `apb --version'. This cell installs nothing and needs no account of its own."
  "The prerequisite the original skill explains, said once here.")

(defun %trim (text)
  "TEXT without the blank a CLI leaves at either end, or \"\" for NIL."
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun run-cli (argv &key directory seconds)
  "Run ARGV -- the program first, searched on PATH -- in DIRECTORY.
 => (values OUT ERR STATUS), both texts trimmed; STATUS :TIMEOUT when the
 bound fell first. The one function every CLI call goes through, so a test
 stubs it and nothing spawns a process."
  (multiple-value-bind (out err status)
      (nlk:run-bounded argv
                       :directory directory
                       :seconds (or seconds (setting :timeout)))
    (values (%trim out) (%trim err) status)))

(defun working-directory ()
  "The directory the CLI runs in: the `directory' setting when it is set,
 else the directory of the session asking -- the project a playbook syncs into."
  (let ((configured (setting :directory)))
    (or (and (stringp configured) (plusp (length configured)) configured)
        (ignore-errors
          (and (stringp nlk:*scribe-session-id*)
               (nlk:find-session-cwd nlk:*scribe-session-id*))))))

(defun clip (text)
  "TEXT, cut at `max_chars' with a note saying where, so one loud plan or one
 long doctor report cannot bury the turn."
  (let ((limit (setting :max-chars)))
    (if (> (length text) limit)
        (format nil "~a~%[cut at ~d of ~d characters]"
                (subseq text 0 limit) limit (length text))
        text)))

(defun apb-argv (&rest arguments)
  "ARGUMENTS with the `apb' executable -- `binary' -- in front."
  (cons (setting :binary) arguments))

(defun call (tool argv)
  "Run ARGV for TOOL (`apb setup', `/apb') and answer what the model reads:
 the CLI's own output, or why it could not be had."
  ;; A failing CLI never signals here: its output is the answer and its
  ;; status the verdict, the same posture the eval snippet's (sh ...) has.
  (handler-case
      (multiple-value-bind (out err status)
          (run-cli argv :directory (working-directory))
        (cond
          ((eq status :timeout)
           (format nil "ERROR: ~a did not finish in ~a s; its whole process tree was killed. ~
Raise `timeout' in the agentplaybooks section of your Nodecode config, or run one command at a time."
                   tool (setting :timeout)))
          ((plusp (length out))
           (if (zerop (or status 0))
               (clip out)
               (format nil "~a~%[~a exited ~a~@[~%~a~]]"
                       (clip out) tool status (unless (zerop (length err)) (clip err)))))
          (t (format nil "ERROR: ~a exited ~a with no output~@[: ~a~]"
                     tool status (unless (zerop (length err)) (clip err))))))
    (error (problem)
      (format nil "ERROR: ~a could not be started: ~a~%~a" tool problem +install-hint+))))

(defun words (text)
  "TEXT split on runs of whitespace, in order, no shell in between."
  (let ((words '())
        (start nil))
    (loop for index from 0 below (length text)
          for character = (char text index)
          do (if (member character '(#\Space #\Tab #\Newline #\Return))
                 (when start (push (subseq text start index) words) (setf start nil))
                 (unless start (setf start index))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(defun first-line (text)
  "TEXT's first non-blank line, or NIL."
  (find-if-not (lambda (line) (zerop (length line)))
                (uiop:split-string (or text "") :separator '(#\Newline #\Return))))

(defun version-of (program)
  "PROGRAM's `--version' line, or NIL when it is not there: the prerequisite
 probe the original skill asks the host agent to run first."
  (handler-case
      (multiple-value-bind (out err status)
          (run-cli (list program "--version") :seconds 15)
        (declare (ignore err))
        (and (zerop (or status 0))
             (first-line out)))
    (error () nil)))
