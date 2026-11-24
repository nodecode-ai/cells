;;;; git.lisp --- one git run, and how a failure is told.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; NODECODE-CHRONOS::*RUN* is the one seam: every git command line this cell
;;;; builds is handed to it, so a test asserts the argv and answers the output
;;;; with no repository, no git install and no network.
;;;;
;;;; The environment is scrubbed for the reason the original plugin scrubbed
;;;; it: a GIT_DIR or GIT_WORK_TREE inherited from somewhere else must not
;;;; point the shadow repository at another tree, and a prompt would hang a
;;;; hook that runs inside a tool call.

(in-package #:nodecode-chronos)

(defparameter +git-environment+
  '("GIT_TERMINAL_PROMPT=0" "GIT_OPTIONAL_LOCKS=0" "LC_ALL=C"
    "GIT_DIR" "GIT_WORK_TREE" "GIT_INDEX_FILE" "GIT_OBJECT_DIRECTORY"
    "GIT_ALTERNATE_OBJECT_DIRECTORIES" "GIT_COMMON_DIR" "GIT_NAMESPACE" "GIT_PREFIX"
    "GIT_EXTERNAL_DIFF" "GIT_DIFF_OPTS" "GIT_CONFIG_PARAMETERS")
  "What every git run reads: no prompts, no optional locks, the C locale, and
the variables that would point git at another repository or another layout.")

(defun run-git (argv directory)
  "Run ARGV, git's own command line, in DIRECTORY to completion or the
section's timeout_seconds => (values STDOUT STDERR STATUS)."
  (handler-case
      (nlk:run-bounded argv :seconds (setting :timeout-seconds) :directory directory
                            :environment +git-environment+)
    (error (condition)
      (values nil (format nil "~a could not be run: ~a" (first argv) condition) 127))))

(defparameter *run* #'run-git
  "How one git command line runs: (funcall *RUN* ARGV DIRECTORY) => (values
STDOUT STDERR STATUS), STATUS an exit code or :TIMEOUT.")

(define-condition chronos-refusal (error)
  ((detail :initarg :detail :reader chronos-refusal-detail))
  (:documentation "One refusal this cell reports as an error answer: the sentence
the model reads.")
  (:report (lambda (condition stream)
             (write-string (chronos-refusal-detail condition) stream))))

(defun refuse (format-control &rest arguments)
  "Signal a CHRONOS-REFUSAL with the sentence ARGUMENTS format."
  (error 'chronos-refusal :detail (apply #'format nil format-control arguments)))

(defun split-lines (text)
  "TEXT's lines, empty trailing ones dropped."
  (remove-if (lambda (line) (zerop (length line)))
             (uiop:split-string (or text "") :separator '(#\Newline))))

(defun last-line (text)
  "TEXT's last non-empty line, trimmed: what git was really complaining about."
  (let ((lines (remove-if (lambda (line)
                             (zerop (length (string-trim '(#\Space #\Tab #\Return) line))))
                           (split-lines text))))
    (string-trim '(#\Space #\Tab #\Return) (or (car (last lines)) text))))

(defun failure-line (status stderr)
  "How one failed git run is told to the model."
  (let ((text (string-trim '(#\Space #\Newline #\Return) (or stderr ""))))
    (if (plusp (length text)) (last-line text) (format nil "exit code ~a" status))))

(defun run-status (argv directory)
  "One git run => (values STDOUT STDERR STATUS)."
  (funcall *run* argv directory))

(defun git-ok (root &rest arguments)
  "One git run that must succeed => its stdout; a refusal when it did not."
  (multiple-value-bind (out stderr status) (apply #'git-run root arguments)
    (unless (eql status 0)
      (refuse "git ~a failed: ~a" (format nil "~{~a~^ ~}" arguments)
              (failure-line status stderr)))
    (or out "")))
