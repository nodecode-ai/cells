;;;; cell.lisp --- the tirith cell: every shell command the agent runs is
;;;; checked with the user's installed tirith before it runs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of sheeki03's hermes-plugin-tirith (MIT) as a Nodecode cell:
;;;; the same program, the same check (`tirith check --json
;;;; --non-interactive --shell posix`, the command on standard input, run in
;;;; the command's own folder), the same verdict table (allow runs; warn runs
;;;; with the warning appended to the result; a block refuses, or asks when
;;;; `block_action` says so; a failed check refuses unless `fail_closed` is
;;;; off), and the same setting names, so a tirith policy is carried over
;;;; unchanged.
;;;;
;;;; WHAT IT HOOKS. One hook on the :TOOL point, the one seam that sees a
;;;; call before it runs. An eval form is READ for the commands it runs --
;;;; (sh "cmd"), (uiop:run-program (list ...)), (sb-ext:launch-program
;;;; ...), (sh ($ "name")) -- and only those texts go to tirith, never the
;;;; Lisp around them, so a comment naming a rule is not a finding. A form
;;;; whose command is built from a variable is checked whole, as text.
;;;;
;;;; THE BINARY IS THE USER'S. The cell never downloads, installs or updates
;;;; tirith, and makes no network request of its own: what tirith's own
;;;; lookups reach is tirith's business, and `offline' turns them off.
;;;;
;;;; ONE SEAM. *RUN* is how every run of the program happens, so a test
;;;; stubs the program and never touches the machine.

(defpackage #:nodecode-tirith
  (:nicknames #:tirith)
  (:use #:cl))

(in-package #:nodecode-tirith)

(nlk:define-peripheral tirith :not-running t)

;;; --- settings helpers -------------------------------------------------------

(defparameter +severities+ '("info" "low" "medium" "high" "critical")
  "The severities tirith reports, quietest first.")

(defun severity-rank (severity)
  "SEVERITY's place in +SEVERITIES+; an unknown one counts as HIGH (3)."
  (or (position (string-downcase (or severity "")) +severities+ :test #'equal) 3))

;; Rule ids whose ordinary warnings can never be filtered away: obfuscation,
;; input tirith could not read, known-malicious intelligence, exfiltration and
;; terminal injection. The original's list, shortened to the ids it says must
;; always show; a CRITICAL finding is protected on its own.
(defparameter +guarded-rules+
  '("analysis_incomplete" "obfuscated_payload" "base64_decode_execute"
    "dynamic_code_execution" "interpreter_suspicious_inline_exec"
    "wrapper_chain_too_deep" "prompt_injection_obfuscated" "double_encoding"
    "pipe_to_interpreter" "curl_pipe_shell" "wget_pipe_shell" "httpie_pipe_shell"
    "xh_pipe_shell" "ps_inline_download_execute" "reverse_shell"
    "ansi_escapes" "control_chars" "bidi_controls" "zero_width_chars"
    "unicode_tags" "hidden_multiline" "confusable_text"
    "threat_malicious_package" "threat_unresolved_malicious_package"))

(defun guarded-rule-p (rule)
  (member (string-downcase (or rule "")) +guarded-rules+ :test #'equal))

(defun ignored-rule-p (rule key)
  (let ((ignored (mapcar (lambda (r) (string-downcase (princ-to-string r)))
                          (or (setting :ignore-rules) '()))))
    (or (member (string-downcase (or rule "")) ignored :test #'equal)
        (member (string-downcase (or key "")) ignored :test #'equal))))

;;; --- versions ---------------------------------------------------------------

(defun parse-version (text)
  "The first version in TEXT as a list of integers, or NIL."
  (when (stringp text)
    (multiple-value-bind (match groups)
        (cl-ppcre:scan-to-strings "(\\d+)\\.(\\d+)\\.(\\d+)" text)
      (declare (ignore match))
      (when groups (map 'list #'parse-integer groups)))))

(defun version-string (version)
  (format nil "~{~d~^.~}" version))

(defun version< (a b)
  (loop for i from 0 below (max (length a) (length b))
        for x = (or (nth i a) 0) and y = (or (nth i b) 0)
        when (< x y) return t
        when (> x y) return nil))

;;; --- text a person is shown -------------------------------------------------

(defun sanitize (text limit)
  "TEXT cut to LIMIT characters with every control, bidi or zero-width
character escaped, so a command cannot rewrite the line that judges it."
  (let ((out (make-string-output-stream))
        (used 0)
        (text (or text "")))
    (loop for char across text
          for code = (char-code char)
          for escaped = (or (< code 32) (= code 127)
                            (<= #x200b code #x200f) (<= #x202a code #x202e)
                            (<= #x2066 code #x2069) (= code #xfeff))
          do (if escaped
                 (progn (format out "\\u{~4,'0X}" code) (incf used 6))
                 (progn (write-char char out) (incf used)))
             (when (> used limit)
               (write-char #\… out)
               (return)))
    (get-output-stream-string out)))

;;; --- running the user's tirith ----------------------------------------------

(defparameter *run* 'run-captured
  "How one run of the program happens:
 (funcall *RUN* ARGV INPUT &key directory timeout)
 => (values RC OUT ERR TIMED-OUT-P ERROR-TEXT). Stubbed by the tests.")

(defun run-captured (argv input &key directory timeout)
  "Run ARGV with INPUT on standard input => (values RC OUT ERR TIMED-OUT-P ERROR-TEXT).
A program that is not installed answers RC NIL and the reason in ERROR-TEXT."
  (handler-case
      (let* ((process (sb-ext:run-program
                       (first argv) (rest argv)
                       :input (make-string-input-stream input)
                       :output :stream :error :stream
                       :wait nil :search t
                       :directory (and directory (pathname directory))
                       :environment (child-environment)
                       :external-format :utf-8))
             (out-stream (sb-ext:process-output process))
             (err-stream (sb-ext:process-error process))
             (deadline (and timeout (+ (get-internal-real-time)
                                       (round (* timeout internal-time-units-per-second)))))
             (timed-out nil))
        (loop while (eq :running (sb-ext:process-status process))
              do (when (and deadline (>= (get-internal-real-time) deadline))
                   (setf timed-out t)
                   (ignore-errors (sb-ext:process-kill process 9))
                   (return))
                 (sleep 0.02))
        (let ((out (ignore-errors (uiop:slurp-stream-string out-stream)))
              (err (ignore-errors (uiop:slurp-stream-string err-stream))))
          (unless timed-out
            (ignore-errors (sb-ext:process-wait process)))
          (values (and (not timed-out) (sb-ext:process-exit-code process))
                  (or out "") (or err "") timed-out nil)))
    (error (condition)
      (values nil "" "" nil (princ-to-string condition)))))

(defparameter +stripped-env+ '("TIRITH" "TIRITH_DEFER" "TIRITH_INTERACTIVE" "TIRITH_OFFLINE")
  "Variables that must not reach the child: an agent-visible bypass request,
a deferred block, an interactive prompt, and an offline flag this cell sets itself.")

(defun child-environment ()
  "The environment every run gets: the person's own, minus +STRIPPED-ENV+, plus
TIRITH_OFFLINE=1 when `offline' is on."
  (let ((entries '()))
    (dolist (entry (sb-ext:posix-environ))
      (let ((equals (position #\= entry)))
        (unless (and equals (member (string-upcase (subseq entry 0 equals))
                                     +stripped-env+ :test #'equal))
          (push entry entries))))
    (when (setting :offline)
      (push "TIRITH_OFFLINE=1" entries))
    (nreverse entries)))

(defun expand-home (path)
  "PATH with a leading ~ replaced by the home folder, since a program name is
not a pathname subject anywhere here."
  (let ((home (uiop:getenv "HOME")))
    (if (and home (plusp (length path)) (char= #\~ (char path 0)))
        (concatenate 'string home (subseq path 1))
        path)))

(defun tirith-program ()
  "The program to run: the configured path with ~ expanded, else tirith from PATH."
  (let ((path (nlk:trimmed (or (setting :path) ""))))
    (if (zerop (length path)) "tirith" (expand-home path))))

(defparameter *probe* nil
  "The binary this cell last found and its version, or NIL. A success is
kept five minutes; a failure is asked again next time, so installing tirith
mid-session is picked up.")

(defun probe-binary ()
  "Find tirith and read its version => a plist
 (:ok T :path ... :version ...) or (:ok NIL :path ... :why TEXT)."
  (when (and *probe* (getf *probe* :ok)
             (< (- (get-universal-time) (getf *probe* :at)) 300))
    (return-from probe-binary *probe*))
  (let* ((path (tirith-program))
         (result (multiple-value-list
                  (funcall *run* (list path "--version") "" :timeout 5))))
    (destructuring-bind (rc out err timed-out error) result
      (declare (ignore err))
      (let ((version (and rc (zerop rc) (parse-version out))))
        (setf *probe*
              (cond ((and version (not (version< version '(0 5 0))))
                     (list :ok t :path path :version (version-string version)
                           :at (get-universal-time)))
                    (error
                     (list :ok nil :path path
                           :why (format nil "~a could not be run: ~a" path error)))
                    (timed-out
                     (list :ok nil :path path :why "tirith --version did not answer in time"))
                    (version
                     (list :ok nil :path path
                           :why (format nil "tirith ~a is older than 0.5.0"
                                         (version-string version))))
                    (t
                     (list :ok nil :path path
                           :why (format nil "~a --version exited with code ~a" path rc)))))))))

;;; --- one check, its verdict ------------------------------------------------

(defparameter +max-command-bytes+ (* 1024 1024)
  "tirith's own standard-input cap; a longer text is refused by it too.")

(defun parse-verdict (text)
  "The last JSON object in TEXT carrying an action and a findings array, or NIL."
  (let ((lines (cons (string-trim '(#\Space #\Newline #\Tab #\Return) (or text ""))
                     (reverse (uiop:split-string (or text "")
                                                  :separator '(#\Newline))))))
    (dolist (line lines)
      (let ((trimmed (string-trim '(#\Space #\Newline #\Tab #\Return) line)))
        (when (and (plusp (length trimmed)) (char= #\{ (char trimmed 0)))
          (let ((value (handler-case (nlk:decode-json trimmed :whole t) (error () nil))))
            (when (and (hash-table-p value)
                       (stringp (gethash "action" value))
                       (nlk:json-value value :array "findings"))
              (return-from parse-verdict value))))))
    nil))

(defun parse-findings (verdict)
  "VERDICT's findings as plists (:key :rule :severity :title), at most 200."
  (let* ((raw (nlk:json-value verdict :array "findings"))
         (count (min 200 (length raw))))
    (loop for index from 0 below count
          for item = (aref raw index)
          for rule = (or (nlk:json-value item :text "rule_id") "unknown")
          for severity = (let ((name (string-upcase (or (nlk:json-value item :text "severity") ""))))
                           (if (member name +severities+ :test #'string-equal) name "HIGH"))
          for custom = (nlk:json-value item :text "custom_rule_id")
          collect (list :key (if (and custom (plusp (length custom)))
                                 (format nil "custom_rule_match:~a" custom)
                                 rule)
                        :rule rule
                        :severity severity
                        :title (or (nlk:json-value item :text "title") rule)))))

(defparameter +strictness+ '(("allow" . 0) ("bypass" . 2) ("warn" . 1) ("warn_ack" . 2)
                             ("error" . 3) ("block" . 4))
  "How strict each verdict kind is, for a run whose exit code and JSON disagree.")

(defun strictness (kind) (or (cdr (assoc kind +strictness+ :test #'equal)) 3))

(defun rc-kind (rc)
  (case rc (0 "allow") (1 "block") (2 "warn") (3 "warn_ack") (4 "block") (t "error")))

(defun action-kind (verdict)
  (let ((action (gethash "action" verdict)))
    (cond ((equal action "allow") (if (eq t (gethash "bypass_honored" verdict)) "bypass" "allow"))
          ((equal action "warn") "warn")
          ((equal action "warn_ack") "warn_ack")
          ((equal action "block") "block")
          (t "error"))))

(defun scan-error (reason-class reason &optional findings)
  (list :kind "error" :reason-class reason-class :reason reason :findings (or findings '())))

(defun classify (rc out timed-out error probe)
  "One run of `tirith check` => a scan plist (:kind :reason :findings ...).
The kinds are the original's: allow, warn, warn_ack, bypass, block, error."
  (let ((common (list :rc rc :version (getf probe :version) :binary (getf probe :path))))
    (cond
      (error (append (scan-error "spawn" error) common))
      (timed-out (append (scan-error "timeout" "tirith did not answer in time") common))
      (t
       (let ((verdict (parse-verdict out)))
         (if (null verdict)
             (append (scan-error (if (member rc '(0 1 2 3 4)) "no_verdict" "exit_code")
                                  (if (eq rc nil)
                                      "tirith printed nothing this cell could read"
                                      (format nil "tirith exited with code ~a without a verdict" rc)))
                     common)
             (let* ((findings (parse-findings verdict))
                    (kind (action-kind verdict))
                    (from-rc (rc-kind rc))
                    (kind (if (and (not (equal from-rc kind))
                                   (not (and (equal from-rc "allow") (equal kind "bypass"))))
                              (if (> (strictness from-rc) (strictness kind)) from-rc kind)
                              kind))
                    (approval (gethash "requires_approval" verdict)))
               (append (list :kind kind
                             :reason (if (equal kind "warn_ack")
                                         "tirith requires this warning to be acknowledged"
                                         "")
                             :findings findings
                             :requires-approval (eq t approval)
                             :deferred (eql rc 4))
                       common))))))))

(defun scan (command &key directory)
  "Check COMMAND with the user's tirith, in DIRECTORY => a scan plist."
  (let ((probe (probe-binary)))
    (if (not (getf probe :ok))
        (append (scan-error "not_found" (getf probe :why)) (list :binary (getf probe :path)))
        (if (> (length command) +max-command-bytes+)
            (scan-error "too_long" "the command is larger than tirith's input limit")
            (multiple-value-bind (rc out err timed-out error)
                (funcall *run*
                         (list (getf probe :path) "check" "--json" "--non-interactive" "--shell" "posix")
                         (format nil "~a~%" command)
                         :directory directory
                         :timeout (setting :timeout))
              (declare (ignore err))
              (classify rc out timed-out error probe))))))

;;; --- what to do about it ----------------------------------------------------

(defun ordered-findings (findings)
  "FINDINGS one per (key severity title), loudest first."
  (let ((seen '()) (kept '()))
    (dolist (finding findings)
      (let ((ident (list (getf finding :key) (getf finding :severity) (getf finding :title))))
        (unless (member ident seen :test #'equal)
          (push ident seen)
          (push finding kept))))
    (sort kept #'> :key (lambda (f) (severity-rank (getf f :severity))))))

(defun findings-text (findings)
  "The findings in one line, as a person reads them."
  (let ((ordered (ordered-findings findings)))
    (if (null ordered)
        "no details"
        (format nil "~{~a~^; ~}"
                (append (loop for finding in (subseq ordered 0 (min 5 (length ordered)))
                              collect (format nil "[~a] ~a (~a)"
                                              (getf finding :severity)
                                              (sanitize (getf finding :title) 120)
                                              (sanitize (getf finding :key) 80)))
                        (when (> (length ordered) 5)
                          (list (format nil "+~a more" (- (length ordered) 5)))))))))

(defun filter-findings (findings)
  "The findings a warning still shows: `min_severity' and `ignore_rules' applied,
except a guarded rule id or a CRITICAL finding, which always shows."
  (let ((floor (severity-rank (setting :min-severity))))
    (remove-if (lambda (finding)
                 (let ((rank (severity-rank (getf finding :severity))))
                   (unless (or (equal (getf finding :severity) "CRITICAL")
                               (guarded-rule-p (getf finding :rule)))
                     (or (< rank floor)
                         (ignored-rule-p (getf finding :rule) (getf finding :key))))))
               findings)))

(defun block-text (findings)
  (let ((first-rule (and findings (getf (first (ordered-findings findings)) :rule))))
    (format nil "ERROR: refused by tirith: ~a. The command did not run -- do not retry it or a variant; tell the operator what tirith reported (a false positive is handled in tirith's own policy~@[; tirith explain --rule ~a~])."
            (findings-text findings) first-rule)))

(defun warning-text (findings scan)
  "What is appended to an allowed command's result."
  (format nil "~%[tirith] Warning about this command (it was allowed): ~a~@[ tirith ~a~]."
          (findings-text findings)
          (getf scan :version)))

(defun ask-once (title detail)
  "Ask the operator Allow once or Deny => T for Allow once; NIL and why otherwise."
  (let ((ask (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
               (and symbol (fboundp symbol) (symbol-function symbol)))))
    (if (null ask)
        (values nil :off)
        (values (equal "Allow once"
                       (funcall ask title
                                :detail detail
                                :choices (list "Allow once" "Deny")
                                :seconds (max 5 (or (setting :ask-seconds) 120))))
                :asked))))

(defun decide (scan command)
  "SCAN's verdict as an action => a plist (:action :run|:ask|:refuse :warning TEXT :message TEXT)."
  (let ((kind (getf scan :kind))
        (findings (getf scan :findings)))
    (cond
      ((equal kind "allow") (list :action :run))
      ((equal kind "bypass")
       (list :action :ask
             :message (format nil "ERROR: refused by tirith: the command asks tirith to skip its check (TIRITH=0), and a prefix written by the agent is not the operator's consent. The command did not run -- run it without the bypass prefix, or tell the operator.")))
      ((equal kind "warn")
       (let ((shown (filter-findings findings)))
         (cond ((null shown) (list :action :run))
               ((equal (setting :warn-action) "refuse") (list :action :refuse :message (block-text shown)))
               (t (list :action :run :warning (warning-text shown scan))))))
      ((member kind '("warn_ack" "block") :test #'equal)
       (cond ((equal (setting :block-action) "refuse")
              (list :action :refuse :message (block-text findings)))
             (t (list :action :ask
                      :message (format nil "ERROR: refused by tirith: ~a; the operator was asked and did not allow it; the command did not run -- give the operator the command to run themselves."
                                       (findings-text findings))))))
      (t ;; error: no verdict, a missing or too-old binary, a timeout
       (cond ((and (not (setting :fail-closed)) (not (getf scan :requires-approval)))
              (list :action :run))
             (t
              (let ((message
                      (format nil "ERROR: refused by tirith: it could not check this command (~a); the command did not run -- the operator can approve it once (block_action ask), or install tirith 0.5.0 or later."
                              (sanitize (getf scan :reason) 300))))
                (if (equal (setting :block-action) "refuse")
                    (list :action :refuse :message message)
                    (list :action :ask :message message)))))))))

;;; --- reading a form for the commands it runs --------------------------------

(defparameter +spawn-operators+ '("sh" "run-program" "launch-program" "run-shell-command")
  "The heads whose argument is a command the form runs.")

(defun operator-name (x)
  "X's symbol name, lowercased, or NIL."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun dollar-name (x)
  (and (consp x) (equal "$" (operator-name (car x))) (stringp (second x)) (second x)))

(defun literal-command (x strings)
  "The text X alone names as a command, or NIL when X hides it."
  (cond ((stringp x) x)
        ((dollar-name x) (and strings (gethash (dollar-name x) strings)))
        ((and (consp x) (member (operator-name (car x)) '("list" "quote") :test #'equal))
         (let ((items (if (equal "quote" (operator-name (car x))) (second x) (rest x))))
           (when (and items (listp items)
                      (every (lambda (item) (or (stringp item) (dollar-name item))) items))
             (format nil "~{~a~^ ~}" (mapcar (lambda (item) (or (dollar-name item) item)) items)))))
        (t nil)))

(defun spawn-command (call strings)
  "CALL's command text => (values TEXT HIDDEN-P)."
  (let ((rest (rest call)) (parts '()) (hidden nil))
    (loop while rest
          do (if (keywordp (first rest))
                 (setf rest (cddr rest))
                 (let ((text (literal-command (first rest) strings)))
                   (if text (push text parts) (setf hidden t))
                   (setf rest (cdr rest)))))
    (values (and parts (format nil "~{~a~^ ~}" (nreverse parts))) hidden)))

(defun read-forms (text)
  "TEXT read as Lisp forms, or NIL when it does not read."
  (handler-case
      (with-input-from-string (in text)
        (loop for form = (read in nil :eof) until (eq form :eof) collect form))
    (error () nil)))

(defun form-commands (form-text strings)
  "The shell commands FORM-TEXT runs => (values COMMANDS HIDDEN-P).
HIDDEN-P says one of them was built where the form cannot show it."
  (let ((commands '()) (hidden nil))
    (labels ((walk (x)
               (when (consp x)
                 (when (member (operator-name (car x)) +spawn-operators+ :test #'equal)
                   (multiple-value-bind (text hiddenp) (spawn-command x strings)
                     (cond (text (push text commands))
                           (hiddenp (setf hidden t)))))
                 (walk (car x))
                 (walk (cdr x)))))
      (mapc #'walk (read-forms form-text)))
    (values (nreverse commands) hidden)))

;;; --- the hook ---------------------------------------------------------------

(defun strictest-scan (scans)
  "The scan whose kind is strictest, or NIL."
  (when scans
    (reduce (lambda (a b) (if (> (strictness (getf b :kind)) (strictness (getf a :kind))) b a))
            (rest scans) :initial-value (first scans))))

(defun check-or-run (op next)
  "The :TOOL point: the commands this call runs are checked; a refusal
answers in place of the call, a warning rides the result."
  (let* ((arguments (getf op :arguments))
         (form (and (hash-table-p arguments) (gethash "form" arguments))))
    (if (not (stringp form))
        (funcall next op)
        (multiple-value-bind (commands hidden)
            (form-commands form (and (hash-table-p arguments) (gethash "strings" arguments)))
          (let ((targets (if commands commands
                             (and hidden (list (sanitize form 4000))))))
            (if (null targets)
                (funcall next op)
                (let* ((verdict (handler-case (strictest-scan (mapcar #'scan targets))
                                  (error () nil)))
                       (decision (handler-case (decide verdict (first targets))
                                   (error (condition)
                                     (list :action :refuse
                                           :message (format nil "ERROR: refused by tirith: the check itself failed (~a); the command did not run -- the operator can run it themselves."
                                                            (sanitize (princ-to-string condition) 200)))))))
                  (case (getf decision :action)
                    (:run
                     (let ((text (funcall next op)))
                       (if (getf decision :warning)
                           (concatenate 'string (or text "") (getf decision :warning))
                           text)))
                    (:ask
                     (multiple-value-bind (allowed why)
                         (ask-once (format nil "tirith flagged a command: ~a"
                                           (findings-text (getf verdict :findings)))
                                   (sanitize (format nil "~{~a~^~%~}" targets) 1500))
                       (declare (ignore why))
                       (if allowed
                           (funcall next op)
                           (nle:failure (getf decision :message)))))
                    (t (nle:failure (getf decision :message)))))))))))

;;; --- /tirith and the tool ---------------------------------------------------

(defun status-text ()
  "What /tirith says with nothing typed: the binary and the settings in force."
  (let ((probe (probe-binary)))
    (if (getf probe :ok)
        (format nil "tirith ~a at ~a: every shell command the agent runs is checked before it runs. ~
Blocks: ~a. Warnings: ~{~a~^, ~}. Failed checks: ~a.~@[ Offline: tirith's own lookups are off.~]"
                (getf probe :version) (getf probe :path)
                (if (equal (setting :block-action) "ask")
                    "ask the operator, Allow once or Deny"
                    "refused")
                (list (setting :warn-action))
                (if (setting :fail-closed) "refused" "run unchecked")
                (setting :offline))
        (format nil "tirith is not installed, or is older than 0.5.0: ~a. ~
Install it (brew install tirith, or see the project's install page); until then ~a."
                (getf probe :why)
                (if (setting :fail-closed)
                    "every shell command is refused (fail_closed)"
                    "commands run unchecked (fail_closed is off)")))))

(defun verdict-text (command)
  "One check of COMMAND in words, for /tirith and the tool."
  (let* ((verdict (scan command :directory (uiop:getcwd)))
         (findings (getf verdict :findings))
         (kind (getf verdict :kind)))
    (cond
      ((equal kind "allow") "tirith allows it: no finding.")
      ((equal kind "warn")
       (format nil "tirith warns (the command still runs): ~a" (findings-text findings)))
      ((equal kind "warn_ack")
       (format nil "tirith requires this warning to be acknowledged: ~a" (findings-text findings)))
      ((equal kind "bypass") "the command asks tirith to skip its check (TIRITH=0).")
      ((equal kind "block") (format nil "tirith BLOCKS it: ~a" (findings-text findings)))
      (t (format nil "tirith could not check it: ~a" (sanitize (getf verdict :reason) 300))))))

(defun run-slash (args session-id)
  "The /tirith command: a command to check, or nothing for the state of the guard."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text)) (status-text) (verdict-text text))))

(defparameter +check-schema+
  (list :description
        "Check a shell command with the operator's installed tirith before running it: lookalike and homograph URLs, pipe-to-shell downloads, obfuscated execution, credential exfiltration, malicious packages, terminal injection. Answers the verdict in words; the terminal tool is checked on its own before it runs."
        :parameters
        (nlk:make-json-object
         "type" "object"
         "properties" (nlk:make-json-object
                       "command" (nlk:make-json-object
                                  "type" "string"
                                  "description" "the shell command to check, exactly as it would run"))
         "required" (vector "command"))))

(defun check-tool (arguments)
  "tirith_check: one command, checked, in words."
  (let ((command (and (hash-table-p arguments) (gethash "command" arguments))))
    (if (and (stringp command) (plusp (length (nlk:trimmed command))))
        (verdict-text command)
        "nothing to check: give the command in command")))

;;; --- the manual -------------------------------------------------------------

(defparameter +manual+
  "tirith is the terminal security scanner, installed by the operator, and this cell runs it on every shell command before the command runs: (sh ...) and the other spawn calls an eval form shows.

What happens: the command text goes to `tirith check --json --non-interactive --shell posix` on standard input, in the session's folder. allow runs it; warn runs it and appends tirith's warning to the result you read; a block refuses it (or asks the operator, when block_action is ask); a check that could not run (no binary, too old, a timeout, no verdict) refuses it too, unless fail_closed is off. Run /tirith COMMAND to check one yourself, or call tirith_check before writing a command into a form.

A refusal is final for that command: do not retry it or a variant, and tell the operator what tirith reported. A false positive is fixed in tirith's own policy, not by working around the cell.")

(nle:define-cell tirith
  (:section ("tirith")
    (:guide "install tirith 0.5.0 or later (brew install tirith, or the project's install page); leave path empty to use the one on PATH; a check that cannot run refuses the command unless fail_closed is off")
    ("path" :string :default ""
     :doc "the tirith executable; empty finds tirith on PATH")
    ("timeout" :integer :default 10 :min 1
     :doc "seconds one check may take before it is stopped and counted as a failed check")
    ("offline" :boolean :default nil
     :doc "run checks with TIRITH_OFFLINE=1: tirith's package and URL lookups use its local cache only")
    ("fail_closed" :boolean :default t
     :doc "refuse a command tirith could not check; off runs it unchecked")
    ("block_action" :choice :options '("refuse" "ask") :default "refuse"
     :doc "tirith blocks a command: refuse it, or ask the operator for Allow once")
    ("ask_seconds" :integer :default 120 :min 5
     :doc "how long the Allow once question waits for an answer")
    ("warn_action" :choice :options '("allow" "refuse") :default "allow"
     :doc "tirith warns: run the command and append the warning, or refuse it")
    ("min_severity" :choice :options '("info" "low" "medium" "high" "critical") :default "low"
     :doc "warnings below this severity are ignored; a critical finding or a guarded rule always shows")
    ("ignore_rules" :list :default nil
     :doc "tirith rule ids whose ordinary warnings are ignored"))
  (:hook :tool #'check-or-run)
  (:command "tirith" 'run-slash
            :description "Check a shell command with tirith"
            :argument-hint "COMMAND")
  (:tool "tirith_check" +check-schema+ #'check-tool)
  (:help :tirith "tirith: every shell command is checked with the operator's tirith before it runs - /tirith COMMAND checks one" +manual+)
  (:start (lambda ()
            (nle:notice (format nil "tirith: ~a" (if (getf (probe-binary) :ok)
                                                      "shell commands are checked before they run"
                                                      "not found; install 0.5.0 or later"))
                        :key "nodecode-tirith")
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-tirith")
                                     (setf *probe* nil))))))
