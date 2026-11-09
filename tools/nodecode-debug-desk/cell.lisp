;;;; cell.lisp --- today's git digest, and the last shell call that failed.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; aydnOktay/hermes-debug-desk's plugin, rewritten as a Nodecode cell:
;;;;
;;;;   - the same two tools the plugin gave the model -- debug_daily_report
;;;;     (git log since local midnight: the count, conventional-commit type
;;;;     counts, the files touched, the subjects) and debug_last_failure (the
;;;;     last failed command, its exit code, the last eight redacted lines);
;;;;   - the same observer the plugin ran after a tool call, here the :TOOL
;;;;     point: a shell call whose answer says it failed is what the desk
;;;;     keeps, debounced by signature so a loop of the same failure is one
;;;;     failure counted, not a page of them;
;;;;   - the same redaction, before anything is written: tokens, keys,
;;;;     passwords and bearer headers never reach the state file;
;;;;   - /debug-desk today | last [summarize] for the operator, and
;;;;     (debug-desk:today), (debug-desk:last-failure) from eval;
;;;;   - the optional written summary, on the session's own model.
;;;;
;;;; What Hermes drew in a desktop pane and a status chip is text here.
;;;; Nothing leaves the machine except the summary call to the user's model;
;;;; git runs on the folder the user names, and the state file is under the
;;;; home.

(in-package #:nodecode-debug-desk)

;;; --- the words ------------------------------------------------------------------

(defparameter *primer*
  "debug-desk is this session's own desk: today's git digest for the folder the
session works in, and the last shell command the agent ran that failed.
  (debug-desk:today)                 commits since local midnight: count,
                                      type counts (feat/fix/wip/...), files, subjects
  (debug-desk:today \"/path/to/repo\")  another repository
  (debug-desk:last-failure)          the last failure: command, exit code, the last
                                      eight lines with tokens and keys redacted
  (debug-desk:last-failure :summarize t)  the same, plus a written summary from
                                      the session's own model
/debug-desk today, /debug-desk last, /debug-desk last summarize is the same for
the operator. The desk sees the agent's own shell calls and nothing else: not a
terminal outside this process. A repeat of the same failure inside
debug-desk.debounce_seconds is one failure, counted."
  "What (help :debug-desk) answers while the cell runs.")

(defparameter +nothing-captured+
  "debug-desk: no failed shell command captured yet. The desk records one when a shell call the agent makes answers a nonzero exit or an error."
  "What the tool answers before the agent has failed at anything.")

(defparameter +missing-repository+ "debug-desk: ~a is in no git repository."
  "What the digest answers for a folder git does not know.")

(defparameter +shell-tools+ '("snippet" "eval" "run" "bash" "shell" "sh" "exec_command")
  "The tool names whose answers are a shell's: the agent's eval snippet, and
whatever else a harness over this one calls its shell.")

(defparameter +max-lines+ 8 "How many lines of a failure are kept.")
(defparameter +max-line-chars+ 240 "How long one kept line may be.")
(defparameter +max-files+ 40 "How many touched files a digest lists.")
(defparameter +max-subjects+ 15 "How many commit subjects a digest lists.")
(defparameter +fresh-seconds+ 900 "A failure younger than this is fresh.")

(defparameter +failure-hints+
  (list "Traceback \\(most recent call last\\)"
        "(?m)^(ERROR|FATAL|FAILED|FAILURE)\\b"
        "Exception: " "Error: " "panic: " "npm ERR!" "FAILED tests/")
  "What an answer has to say to count as a failure when no exit code came
with it: the plugin's own hints, unchanged.")

;;; --- small helpers ---------------------------------------------------------------

(defun blankp (text)
  "Whether TEXT is NIL or nothing but spaces."
  (or (null text) (zerop (length (nlk:trimmed (if (stringp text) text (princ-to-string text)))))))

(defun without-key (plist key)
  "PLIST without KEY, in order."
  (loop for (k v) on plist by #'cddr unless (eq k key) append (list k v)))

(defun clip (text limit)
  "TEXT, at most LIMIT characters."
  (let ((text (or text "")))
    (if (> (length text) limit) (subseq text 0 limit) text)))

(defun directory-name (folder)
  "FOLDER as a string a program can be run in."
  (etypecase folder
    (string folder)
    (pathname (uiop:native-namestring folder))))

(defun folder-of-session (session-id)
  "The folder SESSION-ID works in, else this process's."
  (or (ignore-errors
        (and (nlk:store-open-p) (stringp session-id)
             (nlk:durable-session-cwd (nlk:find-session session-id))))
      (ignore-errors (uiop:getcwd))
      (uiop:temporary-directory)))

(defun session-directory ()
  "The folder the running session works in, else this process's."
  (folder-of-session nlk:*scribe-session-id*))

(defun stamp (universal-time)
  "UNIVERSAL-TIME as a local date and minute."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (or universal-time 0))
    (declare (ignore second))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month date hour minute)))

(defun ago (seconds)
  "SECONDS as a phrase a person reads."
  (cond ((< seconds 60) "just now")
        ((< seconds 3600) (format nil "~d minute~:p ago" (round seconds 60)))
        ((< seconds 86400) (format nil "~d hour~:p ago" (round seconds 3600)))
        (t (format nil "~d day~:p ago" (round seconds 86400)))))

;;; --- redaction -------------------------------------------------------------------

(defparameter +secret-patterns+
  (list "ghp_[A-Za-z0-9]{20,}"
        "github_pat_[A-Za-z0-9_]{20,}"
        "sk-[A-Za-z0-9]{16,}"
        "AKIA[0-9A-Z]{16}"
        "(?i)(api[_-]?key|secret|token|password|passwd|authorization)\\s*[:=]\\s*\\S+"
        "(?i)bearer\\s+[A-Za-z0-9._-]+")
  "The plugin's own secret patterns: what is replaced before a failure is kept.")

(defun redact (text)
  "TEXT with every secret pattern replaced by [REDACTED]."
  (handler-case
      (reduce (lambda (text pattern) (ppcre:regex-replace-all pattern text "[REDACTED]"))
              +secret-patterns+ :initial-value (or text ""))
    (error () (or text ""))))

(defun output-lines (text)
  "TEXT redacted, its blank lines dropped, its last eight lines kept, each
clipped."
  (let* ((lines (remove-if #'blankp (uiop:split-string (redact text) :separator '(#\Newline))))
         (kept (last lines +max-lines+)))
    (mapcar (lambda (line) (clip (string-right-trim '(#\Return #\Space #\Tab) line) +max-line-chars+))
            kept)))

;;; --- the state file ---------------------------------------------------------------

(defvar *state-file* nil
  "Where a failure is kept. NIL, its value while the cell runs, means the
home's own debug-desk/state.json; a test binds a scratch file.")

(defun state-file ()
  (or *state-file* (nlk:home "debug-desk/state.json")))

(defun load-failure ()
  "The last kept failure as a plist, or NIL: (:AT :COMMAND :EXIT :CWD
:OUTPUT :SIGNATURE :REPEATS :SUMMARY)."
  (handler-case
      (let ((path (state-file)))
        (when (probe-file path)
          (let* ((state (nlk:decode-json (uiop:read-file-string path)))
                 (failure (and (hash-table-p state)
                               (nlk:json-value state :object "last_failure"))))
            (when (and (hash-table-p failure)
                       (integerp (nlk:json-value failure :integer "at")))
              (list :at (nlk:json-value failure :integer "at")
                    :command (or (nlk:json-value failure :string "command") "")
                    :exit (nlk:json-value failure :integer "exit")
                    :cwd (or (nlk:json-value failure :string "cwd") "")
                    :output (or (nlk:json-value failure :string "output") "")
                    :signature (or (nlk:json-value failure :string "signature") "")
                    :repeats (or (nlk:json-value failure :integer "repeats") 1)
                    :summary (nlk:json-value failure :string "summary"))))))
    (error () nil)))

(defun save-failure (failure)
  "FAILURE, a plist, written as the state file; answered."
  (let ((state (make-hash-table :test #'equal))
        (object (make-hash-table :test #'equal))
        (path (state-file)))
    (setf (gethash "at" object) (or (getf failure :at) 0)
          (gethash "command" object) (or (getf failure :command) "")
          (gethash "exit" object) (or (getf failure :exit) 0)
          (gethash "cwd" object) (or (getf failure :cwd) "")
          (gethash "output" object) (or (getf failure :output) "")
          (gethash "signature" object) (or (getf failure :signature) "")
          (gethash "repeats" object) (or (getf failure :repeats) 1)
          (gethash "summary" object) (or (getf failure :summary) ""))
    (setf (gethash "last_failure" state) object)
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede
                               :if-does-not-exist :create)
      (write-string (nlk:encode-json-object state) out)
      (terpri out))
    failure))

;;; --- what a shell call leaves behind -----------------------------------------------

(defun shell-tool-p (name)
  (and (stringp name) (member name +shell-tools+ :test #'equal) t))

(defun argument (arguments &rest keys)
  "The first of KEYS present in ARGUMENTS, a JSON object, that is not blank."
  (when (hash-table-p arguments)
    (loop for key in keys
          for value = (gethash key arguments)
          when (and value (not (blankp value))) return value)))

(defun command-text (value)
  "VALUE, a command string or an argv, as one line a person reads."
  (cond ((stringp value) (clip (nlk:trimmed value) 500))
        ((vectorp value) (command-text (coerce value 'list)))
        ((consp value) (clip (format nil "~{~a~^ ~}" (mapcar (lambda (x) (princ-to-string x)) value)) 500))
        (t (clip (princ-to-string (or value "")) 500))))

(defun exit-of (text)
  "The status a shell's answer carried, from its final [exit N] line, or NIL."
  (let ((status nil))
    (handler-case
        (ppcre:do-register-groups (code) ("\\[exit (-?\\d+)\\]" (or text ""))
          (setf status (parse-integer code)))
      (error () nil))
    status))

(defun failure-hint-p (text)
  "Whether TEXT says it failed in words, when no exit code came with it."
  (handler-case
      (some (lambda (pattern) (ppcre:scan pattern (or text ""))) +failure-hints+)
    (error () nil)))

(defun signature-of (command status lines)
  "The one string a repeat of the same failure shares."
  (format nil "~a|~a|~a" (or command "") status (or (car (last lines)) "")))

(defun capture (command status folder output)
  "Keep one failure: COMMAND, its STATUS, the FOLDER, and OUTPUT's tail.
The same signature inside debug-desk.debounce_seconds is the same failure
counted, not a new one."
  (let* ((lines (output-lines output))
         (signature (signature-of command status lines))
         (now (get-universal-time))
         (last (load-failure))
         (window (max 0 (or (ignore-errors (setting :debounce-seconds)) 120))))
    (save-failure
     (if (and last
              (equal signature (getf last :signature))
              (< (- now (or (getf last :at) 0)) window))
         (list :at now
               :command (getf last :command) :exit (getf last :exit)
               :cwd (getf last :cwd) :output (getf last :output)
               :signature signature
               :repeats (1+ (or (getf last :repeats) 1))
               :summary (getf last :summary))
         (list :at now
               :command (clip (redact (or command "")) 500)
               :exit status
               :cwd (clip (redact (or folder "")) 500)
               :output (format nil "~{~a~^~%~}" lines)
               :signature signature
               :repeats 1
               :summary nil)))))

(defun observe-answer (arguments answer)
  "Keep ANSWER, a shell call's answer to a call with ARGUMENTS, when it says
the call failed."
  (let ((status (exit-of answer)))
    (when (or (and (integerp status) (/= status 0))
              (failure-hint-p answer)
              (and (stringp answer) (ppcre:scan "\\AERROR:" answer)))
      (capture (command-text (argument arguments "command" "cmd" "shell_command" "form" "code"))
               status
               (princ-to-string (or (argument arguments "cwd" "directory" "working_directory") ""))
               answer))))

(defun watch-tool (op next)
  "The :TOOL point. OP is (:name NAME :ARGUMENTS OBJECT :CALL-ID ID) and
NEXT runs the call: every call passes through, and a shell call that failed
becomes the desk's last failure. Nothing here may cost a turn its answer, so
every failure of the observer itself is swallowed."
  (let ((name (getf op :name))
        (arguments (getf op :arguments)))
    (handler-case
        (let ((answer (funcall next op)))
          (when (and (shell-tool-p name) (stringp answer))
            (handler-case (observe-answer arguments answer) (error () nil)))
          answer)
      (error (condition)
        ;; the call itself failed: keep what it was, then let it stand
        (handler-case
            (when (shell-tool-p name)
              (observe-answer arguments (format nil "ERROR: ~a: ~a" (type-of condition) condition)))
          (error () nil))
        (error condition)))))

;;; --- the git digest ----------------------------------------------------------------

(defun git (folder &rest args)
  "git ARGS in FOLDER. => (values OUTPUT STATUS): OUTPUT trimmed, STATUS an
integer or NIL when git could not be run at all."
  (handler-case
      (multiple-value-bind (output error status)
          (nlk:run-bounded (list* "git" "-C" (directory-name folder) args) :seconds 15)
        (declare (ignore error))
        (values (nlk:trimmed (or output "")) (and (integerp status) status)))
    (error () (values "" nil))))

(defparameter +type-pattern+
  "(?i)^(feat|fix|wip|docs|chore|refactor|test|style|perf|build|ci)(\\([^)]*\\))?(!)?:\\s*"
  "The conventional-commit prefix the digest counts by, as the plugin counted it.")

(defun classify (subject)
  "SUBJECT's conventional-commit type, or \"other\"."
  (let ((subject (nlk:trimmed (or subject ""))))
    (multiple-value-bind (match groups) (ppcre:scan-to-strings +type-pattern+ subject)
      (declare (ignore groups))
      (cond (match (string-downcase (subseq subject 0 (position #\: subject :test #'char=))))
            ((search "wip" (string-downcase subject)) "wip")
            (t "other")))))

(defun unique-files (text)
  "TEXT, a git log's name-only column, as at most +MAX-FILES+ distinct paths."
  (let ((seen (make-hash-table :test #'equal))
        (files '()))
    (dolist (line (uiop:split-string (or text "") :separator '(#\Newline)))
      (let ((name (substitute #\/ #\\ (nlk:trimmed line))))
        (unless (or (blankp name) (gethash name seen) (>= (length files) +max-files+))
          (setf (gethash name seen) t)
          (push name files))))
    (nreverse files)))

(defun report-text (cwd)
  "The git digest of the repository CWD names, or the session's folder is in:
commits since local midnight, the type counts, the files and the subjects."
  (let ((folder (if (blankp cwd) (session-directory) (nlk:trimmed cwd))))
    (multiple-value-bind (root status) (git folder "rev-parse" "--show-toplevel")
      (cond
        ((not (and (eql 0 status) (plusp (length root))))
         (format nil +missing-repository+ (directory-name folder)))
        (t
         (multiple-value-bind (branch branch-status) (git root "rev-parse" "--abbrev-ref" "HEAD")
           (multiple-value-bind (subjects subjects-status)
               (git root "log" "--since=midnight" "--pretty=format:%s")
             (cond
               ((not (eql 0 subjects-status))
                (format nil "debug-desk: git log failed in ~a: ~a" root subjects))
               (t
                (let* ((list (remove-if #'blankp
                                        (uiop:split-string subjects :separator '(#\Newline))))
                       (counts (make-hash-table :test #'equal))
                       (files (multiple-value-bind (text files-status)
                                  (git root "log" "--since=midnight" "--name-only" "--pretty=format:")
                                (if (eql 0 files-status) (unique-files text) '()))))
                  (dolist (subject list)
                    (incf (gethash (classify subject) counts 0)))
                  (with-output-to-string (out)
                    (format out "debug-desk: ~a~@[, branch ~a~], since local midnight~%"
                            root (and (eql 0 branch-status) (plusp (length branch)) branch))
                    (format out "commits: ~d~%" (length list))
                    (when (plusp (length list))
                      (format out "types: ~{~a~^, ~}~%"
                              (loop for type being the hash-keys of counts using (hash-value n)
                                    collect (format nil "~a ~d" type n)))
                      (format out "files (~d): ~{~a~^, ~}~%"
                              (length files) files)
                      (format out "subjects:~%")
                      (dolist (subject (subseq list 0 (min (length list) +max-subjects+)))
                        (format out "  - ~a~%" (clip subject +max-line-chars+))))
                    (when (zerop (length list))
                      (format out "nothing has landed in this repository since midnight~%")))))))))))))

;;; --- the last failure ---------------------------------------------------------------

(defparameter +summary-system+
  "You summarise a failed terminal command for a developer. Reply with at
most 3 short sentences: what failed, likely cause, one next step. No
preamble, no secrets."
  "The instruction the optional summary is asked under, as the plugin asked it.")

(defvar *ask-model* nil
  "How the summary asks the session's own model: (SYSTEM USER) => text. NIL,
its value while the cell runs, means NLE:COMPLETE, the core's own side call;
a test binds a function and no network is touched.")

(defun ask-model (system user)
  "SYSTEM and USER answered by the session's model, or NIL."
  (let ((ask (or *ask-model*
                 (and (fboundp 'nle:complete)
                      (lambda (system user) (nle:complete system user :max-tokens 160))))))
    (when ask
      (handler-case (let ((text (funcall ask system user)))
                      (and (stringp text) (plusp (length (nlk:trimmed text))) (nlk:trimmed text)))
        (error () nil)))))

(defun summarise (failure)
  "FAILURE's written summary: the one already kept, else a fresh ask of the
session's model, kept for the next reader. NIL when no model answers."
  (or (let ((kept (getf failure :summary)))
        (and (stringp kept) (plusp (length (nlk:trimmed kept))) kept))
      (let ((prompt (format nil "Command: ~a~%Exit: ~a~%~a"
                             (or (getf failure :command) "")
                             (or (getf failure :exit) "?")
                             (or (getf failure :output) ""))))
        (let ((text (ask-model +summary-system+ (clip prompt 4000))))
          (when text
            (save-failure (list* :summary text (without-key failure :summary)))
            text)))))

(defun failure-text (&key summarize)
  "The last kept failure as text, with its written summary when SUMMARIZE."
  (let ((failure (load-failure)))
    (if (null failure)
        +nothing-captured+
        (let* ((age (max 0 (- (get-universal-time) (or (getf failure :at) 0))))
               (summary (and summarize (summarise failure))))
          (with-output-to-string (out)
            (format out "debug-desk: last failure, ~a (~a)~@[, seen ~d times~]~%"
                    (stamp (getf failure :at)) (ago age)
                    (let ((repeats (getf failure :repeats)))
                      (and (integerp repeats) (> repeats 1) repeats)))
            (format out "command: ~a~%" (or (getf failure :command) "(unknown)"))
            (format out "exit: ~a~@[ (fresh)~]~%"
                    (or (getf failure :exit) "unknown")
                    (and (< age +fresh-seconds+) t))
            (when (plusp (length (or (getf failure :cwd) "")))
              (format out "folder: ~a~%" (getf failure :cwd)))
            (format out "output:~%~{  ~a~%~}" (uiop:split-string (or (getf failure :output) "") :separator '(#\Newline)))
            (cond (summary (format out "summary: ~a~%" summary))
                  (summarize (format out "summary: none (no model answered)~%"))))))))

;;; --- the verbs, for eval and for /debug-desk ------------------------------------------

(define-verb today (&optional (cwd ""))
  "Today's git digest for CWD, or for the folder this session works in:
commits since local midnight, the conventional-commit type counts, the files
touched and the subjects. No model is called."
  (report-text cwd))

(define-verb last-failure (&key summarize)
  "The last shell command the agent ran that failed: its command, exit code,
folder and the last eight lines with tokens and keys redacted. SUMMARIZE
asks the session's own model for three sentences on it."
  (failure-text :summarize summarize))

(defun run-slash (args session-id)
  "/debug-desk today | last [summarize]: the operator's own look at the desk."
  (let* ((parts (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space #\Tab)))
         (verb (string-downcase (or (first parts) ""))))
    (cond
      ((member verb '("today" "git" "report") :test #'equal)
       (today (if session-id (namestring (folder-of-session session-id)) "")))
      ((member verb '("last" "fail" "failure") :test #'equal)
       (last-failure :summarize (and (second parts)
                                      (member (string-downcase (second parts))
                                              '("summarize" "llm" "summary") :test #'equal)
                                      t)))
      (t "Usage:
  /debug-desk today            git commits since local midnight
  /debug-desk last             the last failed shell command
  /debug-desk last summarize   the same, with a written summary"))))

;;; --- what the model is asked -------------------------------------------------------

(nlk:define-contract debug-daily-report
  (:doc "Summarise today's git commits in a local repository (since local midnight). Answers the commit count, conventional-commit type counts (feat/fix/wip/...), the files touched and the recent subjects. Does not call a model. Use when the user asks what landed today, for a daily git digest, or before writing a standup note.")
  ("cwd" :string :default ""
         :doc "the repository folder; the folder this session works in when empty"))

(nlk:define-contract debug-last-failure
  (:doc "Return the last shell command the agent ran that failed -- a nonzero exit or traceback-like output -- as its command, exit code, folder, and the last eight lines with tokens and keys redacted. Optionally summarise it with the session's own model. Use when debugging a failed build or test the agent just ran, or when the user asks what broke.")
  ("summarize" :boolean :default nil
               :doc "if true, ask the model for a short actionable summary"))

(defun daily-report-tool (call)
  "The debug_daily_report tool: the digest for the call's folder."
  (today (debug-daily-report-cwd call)))

(defun last-failure-tool (call)
  "The debug_last_failure tool: the last failure, summarised when asked."
  (last-failure :summarize (debug-last-failure-summarize call)))

;;; --- the declaration ----------------------------------------------------------------

(nle:define-cell debug-desk
  (:section ("debug-desk")
    (:guide "nothing has to be set: the cell reads the agent's own shell calls and the session's folder; debounce_seconds folds a repeat of the same failure into one")
    ("debounce_seconds" :integer :default 120 :min 0
                        :doc "ignore a repeat of the same failure inside this many seconds"))
  (:tool "debug_daily_report" (nlk:contract 'debug-daily-report) 'daily-report-tool)
  (:tool "debug_last_failure" (nlk:contract 'debug-last-failure) 'last-failure-tool)
  (:command "debug-desk" 'run-slash
            :description "Today's git digest or the last failed shell command"
            :argument-hint "today|last [summarize]")
  (:help :debug-desk "debug-desk: today's git commits and the last failed shell call - (debug-desk:today)" *primer*)
  (:hook :tool #'watch-tool)
  (:start (lambda ()
            (setf *state-file* nil)
            (nle:on-stop (lambda () (setf *state-file* nil))))))
