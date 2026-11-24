;;;; cli.lisp --- one seam to the two CLIs: a list of arguments in, the text
;;;; a model reads out.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin asks a host agent to type shell commands at Sem and
;;;; Weave, prefixing each one with SEM_NO_NETWORK=1 SEM_NO_TELEMETRY=1 by
;;;; hand. Here the argv is built once, the prefix is laid on every call
;;;; (while `offline' is on), and the answer is the CLI's own output, cut at
;;;; `max_chars'. Nothing is passed through a shell, so a branch name or a
;;;; path is one argument however it is spelled, and the CLIs are the user's
;;;; own binaries found on PATH.

(in-package #:nodecode-sem)

(defparameter +offline-environment+ '("SEM_NO_NETWORK=1" "SEM_NO_TELEMETRY=1")
  "What every Sem call carries while `offline' is on: no cloud index, no
 telemetry, whatever the user's own Sem config says.")

(defparameter +install-hint+
  "Install Ataraxy Labs' Sem (https://github.com/Ataraxy-Labs/sem#install) and check `sem --version':
GNU Parallel also ships an unrelated executable named `sem'. Merge previews need Weave
(https://github.com/Ataraxy-Labs/weave#install). Neither is installed by this cell."
  "The prerequisite the original skills explain, said once here.")

(defun %trim (text)
  "TEXT without the blank a CLI leaves at either end, or \"\" for NIL."
  (if (stringp text)
      (string-trim '(#\Space #\Tab #\Newline #\Return) text)
      ""))

(defun run-cli (argv &key directory seconds)
  "Run ARGV -- the program first, searched on PATH -- in DIRECTORY.
 => (values OUT ERR STATUS), both texts trimmed; STATUS :TIMEOUT when the
 bound fell first. The one function every CLI call goes through, so a test
 stubs it and nothing spawns."
  (multiple-value-bind (out err status)
      (nlk:run-bounded argv
                       :directory directory
                       :environment (when (setting :offline) +offline-environment+)
                       :seconds (or seconds (setting :timeout)))
    (values (%trim out) (%trim err) status)))

(defun working-directory ()
  "The directory the CLIs run in: the `directory' setting when it is set,
 else the directory of the session asking."
  (let ((configured (setting :directory)))
    (or (and (stringp configured) (plusp (length configured)) configured)
        (ignore-errors
          (and (stringp nlk:*scribe-session-id*)
               (nlk:find-session-cwd nlk:*scribe-session-id*))))))

(defun clip (text)
  "TEXT, cut at `max_chars' with a note saying where, so one loud answer
 cannot bury the turn."
  (let ((limit (setting :max-chars)))
    (if (> (length text) limit)
        (format nil "~a~%[cut at ~d of ~d characters; ask for less: one name, one file, a smaller budget]"
                (subseq text 0 limit) limit (length text))
        text)))

(defun sem-argv (&rest arguments)
  "ARGUMENTS with `sem' -- sem_path -- in front."
  (cons (setting :sem-path) arguments))

(defun weave-argv (&rest arguments)
  "ARGUMENTS with `weave' -- weave_path -- in front."
  (cons (setting :weave-path) arguments))

(defun call (tool argv)
  "Run ARGV for TOOL (`sem find', `weave preview') and answer what the model
 reads: the CLI's own output, or why it could not be had."
  ;; A failing CLI never signals here: its output is the answer and its
  ;; status the verdict, the same posture the eval snippet's (sh ...) has.
  (handler-case
      (multiple-value-bind (out err status)
          (run-cli argv :directory (working-directory))
        (cond
          ((eq status :timeout)
           (format nil "ERROR: ~a did not finish in ~a s; its whole process tree was killed. ~
Narrow the query (fewer names, one file, a smaller budget) or raise `timeout' in the sem ~
section of your Nodecode config."
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
