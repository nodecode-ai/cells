;;;; cli.lisp --- running the Snyk CLI, and turning what it wrote into one string.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin launched Snyk's first-party MCP server and let the
;;;; harness speak JSON-RPC to it, but every one of its tools was a thin
;;;; wrapper over one Snyk CLI subcommand. This cell runs those subcommands
;;;; itself: NLK:RUN-BOUNDED, a bound in seconds, and the child's whole
;;;; process tree killed when it passes.
;;;;
;;;; *RUN* is the one seam: a tool builds its argv and hands it to *RUN*,
;;;; so a test asserts the argv and answers the output with no Snyk install,
;;;; no network and no account.

(in-package #:nodecode-snyk)

(defparameter *run* 'run-cli
  "How a tool runs the Snyk CLI: (funcall *RUN* ARGV) => (values STDOUT
STDERR STATUS), STATUS an exit code or :TIMEOUT.")

(defun run-cli (argv)
  "Run ARGV to completion or the section's timeout_seconds, whichever comes
first => (values STDOUT STDERR STATUS)."
  (handler-case (nlk:run-bounded argv :seconds (setting :timeout-seconds))
    (error (condition)
      ;; A CLI that is not installed is the ordinary first state of this cell,
      ;; so it is words the model can act on rather than a signalled error:
      ;; status 127 is what a shell says for a command it cannot find.
      (values nil
              (format nil "~a could not be run: ~a. Install the Snyk CLI (npm install -g snyk), or set the snyk section's command to [\"npx\", \"-y\", \"snyk@1.1306.0\"]."
                      (first argv) condition)
              127))))

;;; --- the section's values, and the words around them ----------------------

(defun severity (named)
  "The severity floor a scan passes: NAMED when the call gave one, else the
section's severity_threshold."
  (or named (setting :severity-threshold)))

(defun code-severity (value)
  "VALUE as Snyk Code takes it: Snyk Code has no critical rung, so critical
is high there."
  (if (equal value "critical") "high" value))

(defun absolute (path &key (what "path"))
  "PATH with ~ read against the operator's home, refused unless it is
absolute: every Snyk scanner resolves a relative path against its own
working directory, and the original's skill says so too."
  ;; EXPAND-HOME answers a pathname, so the namestring is what runs; a
  ;; directory it expands keeps a trailing separator Snyk does not need.
  (let* ((written (string-trim '(#\Space #\Tab) (or path "")))
         (expanded (if (uiop:string-prefix-p "~" written)
                       (uiop:native-namestring (nlk:expand-home written))
                       written))
         (text (or (let ((trimmed (string-right-trim "/" expanded)))
                     (and (plusp (length trimmed)) trimmed))
                   expanded))
         (rooted (or (and (plusp (length text)) (char= #\/ (char text 0)))
                     ;; a Windows drive, C:\... or C:/...
                     (and (> (length text) 2)
                          (alpha-char-p (char text 0))
                          (char= #\: (char text 1))))))
    (unless rooted
      (fail "~a must be an absolute path, got ~s: run (sh \"pwd\") in the project and pass what it answers"
            what path))
    text))

(defun command-line (subcommands flags &optional positional)
  "One Snyk command line: the section's command prefix, SUBCOMMANDS,
FLAGS, then POSITIONAL."
  (append (setting :command) subcommands flags (and positional (list positional))))

(defun flag-arguments (object spec &optional severity)
  "The flags SPEC names, read off OBJECT — a contract's record encoded back
to the JSON object its members came from — in SPEC's order."
  ;; SPEC is ((MEMBER FLAG KIND) ...): KIND :BOOLEAN for a switch that is
  ;; passed when the member is true, :VALUE for one that is passed as
  ;; FLAG=VALUE when the member has a value at all. SEVERITY, when given,
  ;; is the floor the scan will really pass, written into OBJECT first so
  ;; the record's own member and the flag cannot disagree.
  (when (and severity (find "severity_threshold" spec :key #'first :test #'equal))
    (setf (gethash "severity_threshold" object) severity))
  (loop for (member flag kind) in spec
        append (multiple-value-bind (value present) (gethash member object)
                 (declare (ignore present))
                 (case kind
                   (:boolean (and (eq value t) (list flag)))
                   (:value (cond ((and (stringp value) (plusp (length value)))
                                  (list (concatenate 'string flag "=" value)))
                                 ((integerp value)
                                  (list (format nil "~a=~d" flag value)))
                                 (t nil)))))))

;;; --- what a tool answers --------------------------------------------------

(defparameter +scan-status+
  '((0 . "no issues found") (1 . "issues found") (2 . "the scan failed")
    (3 . "no supported projects found"))
  "The exit codes a Snyk scan answers, in the words the model reads; any
other says its number alone.")

(defun status-line (status scan)
  "How one run ended, in a line."
  (cond ((eq status :timeout)
         (format nil "the run passed timeout_seconds (~d s) and its process tree was killed"
                 (setting :timeout-seconds)))
        (scan (format nil "exit ~d~@[ (~a)~]" status (cdr (assoc status +scan-status+))))
        (t (format nil "exit ~d~@[ (~a)~]" status (and (eql status 0) "ok")))))

(defun clip (text limit)
  "TEXT cut to LIMIT characters, saying so when it was."
  (cond ((not (stringp text)) "")
        ((<= (length text) limit) text)
        (t (format nil "~a~%... cut at ~d of ~d characters (the snyk section's max_output_chars); narrow the scan, or raise it"
                   (subseq text 0 limit) limit (length text)))))

(defun answer (argv out err status &key scan note)
  "The one string a tool answers: the command, how it ended, what the CLI
wrote, then what it complained about."
  (let ((limit (setting :max-output-chars))
        (out (or out ""))
        (err (string-trim '(#\Space #\Tab #\Newline #\Return) (or err ""))))
    (with-output-to-string (stream)
      (when note (format stream "~a~%" note))
      (format stream "$ ~{~a~^ ~}~%" argv)
      (format stream "~a~%" (status-line status scan))
      (if (plusp (length out))
          (progn (write-string (clip out limit) stream) (terpri stream))
          (format stream "(no output)~%"))
      (when (plusp (length err))
        (format stream "stderr: ~a~%" (clip err limit))))))

(defun run-tool (command-line &key scan note)
  "Run COMMAND-LINE through *RUN* and answer it as the tool's string."
  (multiple-value-bind (out err status) (funcall *run* command-line)
    (answer command-line out err status :scan scan :note note)))
