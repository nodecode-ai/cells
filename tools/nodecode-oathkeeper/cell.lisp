;;;; cell.lisp --- the oath gate: section, :tool hook, tools, /oathkeeper, help.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The port of the plugin's __init__.py. Hermes gave the plugin three hooks
;;;; around one agent's tool calls; Nodecode gives a cell one around hook on the
;;;; :TOOL point, so the pre-call check and the post-call bookkeeping are the one
;;;; AROUND: everything a permitted call changes is re-baselined after it runs,
;;;; and evidence a require rule waits for is recorded then too.
;;;;
;;;; Where the plugin injected missions on the first turn, this cell keeps one
;;;; standing notice: the model reads the notices block at the tail of every
;;;; round's prompt, and the operator sees the same line on the board.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "oathkeeper": {"oath_file": "", "watch_changes": true,
;;;;                  "hash_limit_bytes": 1000000, "max_files": 5000}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-oathkeeper)

(nlk:define-peripheral oathkeeper :not-running t)

(defvar *session* nil
  "The session a call belongs to when the hook is driven outside a turn (a test,
(help :oathkeeper)'s own dry check). A turn's own :SESSION-ID wins over it.")

(defparameter +notice-key+ "nodecode-oathkeeper"
  "The standing notice's key: the missions, said once and standing.")
(defparameter +changes-key+ "nodecode-oathkeeper-changes"
  "The standing notice's key: what moved outside the gate, until the next clean check.")

;;; --- the oath on disk ---------------------------------------------------------

(defun data-dir ()
  "The folder this cell owns under the organism's home."
  (nlk:home "oathkeeper/"))

(defun oath-file ()
  "The oath file the section names, or <home>/oathkeeper/oath.txt."
  (let ((named (trim-blanks (princ-to-string (or (setting :oath-file) "")))))
    (if (plusp (length named))
        (expand-path-text named)
        (namestring (merge-pathnames "oath.txt" (data-dir))))))

(defun current-session ()
  "The session this call belongs to."
  (or (getf (nle:turn) :session-id) *session* ""))

(defun oath-exists-p ()
  "Is there an oath to enforce? Without one this cell is inert."
  (uiop:file-exists-p (oath-file)))

;;; --- the check ---------------------------------------------------------------

(defparameter +mutating-verbs+ '("modify" "delete" "create")
  "The verbs the oath file's own protection covers.")

(defun violations-of (tool-name arguments session)
  "Every oath line a call would break, as (:reason :rule :verb :target) plists."
  (let ((parsed (load-oath (oath-file)))
        (violations '())
        (oath (normalize-path (oath-file))))
    (when (getf parsed :exists)
      (dolist (action (extract-actions tool-name arguments))
        (let ((verb (getf action :verb))
              (target (getf action :target)))
          (cond
            ((and (string= target oath) (member verb +mutating-verbs+ :test #'string=))
             (push (list :reason "the target is the oath file itself, which only the owner may change"
                         :rule "(built-in) the oath file itself is never modifiable by the agent"
                         :verb verb :target target)
                   violations))
            (t
             (dolist (rule (getf parsed :rules))
               (when (rule-matches-p rule verb target)
                 (cond
                   ((string= (getf rule :kind) "never")
                    (push (list :reason (format nil "oath line ~a forbids ~a on this target"
                                                (getf rule :line) verb)
                                :rule (getf rule :raw) :verb verb :target target)
                          violations))
                   ((not (evidence-satisfied-p (data-dir) session (getf rule :evidence) target))
                    (push (list :reason (format nil "oath line ~a requires evidence '~a' for this target earlier in this session"
                                                (getf rule :line) (getf rule :evidence))
                                :rule (getf rule :raw) :verb verb :target target)
                          violations))
                   (t nil)))))))))
    (nreverse violations)))

(defun block-message (violations)
  "What the model reads instead of the call it asked for."
  (with-output-to-string (out)
    (format out "ERROR: oathkeeper blocked this tool call: it would break the owner's oath.~%")
    (dolist (violation violations)
      (format out "- ~a. Rule: \"~a\". Action: ~a ~a.~%"
              (getf violation :reason) (getf violation :rule)
              (getf violation :verb) (getf violation :target)))
    (format out "Do not retry the same action. Choose an approach the oath allows, ~
  or produce the evidence an 'always require' line asks for and then retry. The oath ~
  itself can only be changed by the owner, outside the agent. This attempt is in the ~
  Oathkeeper ledger.")))

(defun record-block (tool-name session violations)
  "One ledger line per blocked call."
  (append-ledger (data-dir)
                 "event" "blocked"
                 "tool" (or tool-name "")
                 "session_id" session
                 "rules" (coerce (mapcar (lambda (violation) (or (getf violation :rule) ""))
                                          violations)
                                  'vector)))

;;; --- what changed outside the gate ---------------------------------------------

(defun snapshot-now (&key (report nil))
  "Refresh the fingerprints of every sworn surface.
With REPORT, answer the changes since the last baseline, which were made outside
any call this gate approved."
  (let ((parsed (load-oath (oath-file))))
    (when (getf parsed :exists)
      (multiple-value-bind (digest files)
          (build-snapshot (getf parsed :rules) (oath-file)
                          (setting :hash-limit-bytes) (setting :max-files))
        (let* ((fresh (list digest files))
               (old (load-snapshot (data-dir)))
               (changes (and old (diff-snapshots old fresh))))
          (save-snapshot (data-dir) digest files)
          (when report changes))))))

(defun report-outside-changes (session)
  "Say what moved outside the gate, and clear the last report when nothing did."
  (let ((changes (snapshot-now :report t)))
    (cond
      ((null changes) nil)
      (t
       (dolist (change changes)
         (append-ledger (data-dir)
                        "event" "outside_change_detected"
                        "session_id" session
                        "change" (first change)
                        "path" (second change)))
       (nle:notice (format nil "oathkeeper: sworn surfaces changed outside the gate since ~
  the last check (not through a call this gate approved): ~{~a~^; ~}"
                           (mapcar (lambda (change)
                                     (format nil "~a ~a" (first change) (second change)))
                                   (last changes (min 10 (length changes)))))
                   :level :warning :key +changes-key+)
       changes))))

;;; --- evidence -----------------------------------------------------------------

(defun record-evidence (tool-name arguments session)
  "Record the paths a permitted call backed up: what a require rule waits for."
  (dolist (target (backup-targets tool-name arguments))
    (append-ledger (data-dir)
                   "event" "evidence"
                   "kind" "backup"
                   "target" target
                   "session_id" session)))

;;; --- the :TOOL point ----------------------------------------------------------

(defun check-or-run (op next)
  "The :tool hook: the plugin's pre_tool_call and post_tool_call as one around.
A violation answers the call's failure and NEXT never runs; a permitted call
runs, then its evidence is recorded and its changes are baselined."
  (let ((tool-name (getf op :name))
        (arguments (getf op :arguments))
        (session (current-session)))
    (if (not (oath-exists-p))
        (funcall next op)
        (progn
          (when (setting :watch-changes) (ignore-errors (report-outside-changes session)))
          (let ((violations
                  (handler-case (violations-of tool-name arguments session)
                    (error (condition)
                      (list (list :reason (format nil "the oath could not be evaluated (~a), so the call was refused fail-closed"
                                                  condition)
                                  :rule "(fail-closed) an evaluation error"
                                  :verb "?" :target "?"))))))
            (if violations
                (progn (ignore-errors (record-block tool-name session violations))
                       (nle:failure (block-message violations)))
                (prog1 (funcall next op)
                  (ignore-errors (record-evidence tool-name arguments session))
                  (when (setting :watch-changes) (ignore-errors (snapshot-now))))))))))

;;; --- the model's two tools ----------------------------------------------------

;; The status answer takes no arguments, which a contract cannot declare: a
;; schema written by hand is the escape hatch for a shape no declaration carries.
(defparameter +status-schema+
  (list :description "The oath in force: its missions, rules, parse errors and the ledger's counts."
        :parameters (nlk:make-json-object "type" "object" "properties" (nlk:make-json-object)))
  "What the provider is shown of oath_status: an object with no members.")

(nlk:define-contract check-call
  (:doc "Dry-check a hypothetical call against the owner's oath without running it.
Answers whether it would be allowed, and every oath line it would break.")
  ("form" :text :required t
   :doc "the Lisp form the eval call would run, for example (sh \"rm -rf ~/notes\")")
  ("strings" :object
   :doc "the strings object the form names with ($ \"name\"), as an object of name to text"))

(defun check-tool (call)
  "The oath_check tool: one dry check, answered as JSON."
  (let* ((form (check-call-form call))
         (strings (check-call-strings call))
         (arguments (apply #'nlk:make-json-object
                           (append (list "form" form)
                                   (when (and strings (hash-table-p strings))
                                     (list "strings" strings)))))
         (violations (violations-of "eval" arguments (current-session))))
    (nlk:encode-json-object
     (nlk:make-json-object
      "allowed" (null violations)
      "oath" (oath-file)
      "violations" (coerce (mapcar (lambda (violation)
                                      (nlk:make-json-object "rule" (or (getf violation :rule) "")
                                                            "reason" (or (getf violation :reason) "")))
                                    violations)
                            'vector)))))

(defun ledger-counts ()
  "The ledger's events, counted by name."
  (let ((counts (nlk:make-json-object)))
    (dolist (row (read-ledger (data-dir) :limit 500))
      (let ((event (or (row-field row "event") "?")))
        (setf (gethash event counts) (1+ (or (gethash event counts) 0)))))
    counts))

(defun status-tool (call)
  "The oath_status tool: the oath, its parse errors, and the ledger's counts."
  (declare (ignore call))
  (let ((parsed (load-oath (oath-file))))
    (nlk:encode-json-object
     (nlk:make-json-object
      "success" t
      "oath_exists" (and (getf parsed :exists) t)
      "oath_file" (oath-file)
      "missions" (coerce (getf parsed :missions) 'vector)
      "rules" (coerce (mapcar (lambda (rule)
                                 (nlk:make-json-object "line" (getf rule :line)
                                                       "raw" (getf rule :raw)))
                               (getf parsed :rules))
                       'vector)
      "parse_errors" (coerce (getf parsed :errors) 'vector)
      "ledger_counts" (ledger-counts)))))

;;; --- the operator's surfaces --------------------------------------------------

(defun status-text ()
  "The oath in words: what /oathkeeper answers, and the plugin's `oath status`."
  (let ((parsed (load-oath (oath-file))))
    (cond
      ((not (getf parsed :exists))
       (format nil "Oathkeeper is installed but there is no oath at ~a yet. The owner writes one, ~
  one rule a line, to activate the gate: (help :oathkeeper) has the syntax." (oath-file)))
      (t
       (with-output-to-string (out)
         (format out "Oathkeeper oath at ~a:~%" (oath-file))
         (when (getf parsed :missions)
           (format out "Standing missions:~%")
           (dolist (mission (getf parsed :missions)) (format out "- ~a~%" mission)))
         (dolist (rule (getf parsed :rules))
           (format out "- line ~a: ~a~%" (getf rule :line) (getf rule :raw)))
         (when (getf parsed :errors)
           (format out "Parse errors (these lines are ignored):~%")
           (dolist (error (getf parsed :errors)) (format out "- ~a~%" error)))
         (let ((counts '()))
           (maphash (lambda (event count) (push (format nil "~a=~a" event count) counts))
                    (ledger-counts))
           (format out "~a~%"
                   (if counts
                       (format nil "Ledger: ~{~a~^, ~}" (sort counts #'string<))
                       "Ledger: no events yet."))))))))

(defun run-command (args session-id)
  "One /oathkeeper invocation: the oath, its rules and the ledger's counts."
  (declare (ignore args session-id))
  (status-text))

(defparameter +manual+
  "Oathkeeper enforces an owner's oath at the tool boundary: the rules the owner writes once in a
plain file, checked deterministically on every call the agent makes -- no model, no network.
  <home>/oathkeeper/oath.txt, or the file the `oath_file' setting names, holds the oath:
    # my agent's oath
    standing mission: keep the owner's work moving without being asked twice
    never delete: ~/notes/**
    never modify: ~/.nodecode/cells/**
    always require backup before modify: ~/notes/**
  A line is one of three things. `standing mission: <text>' is briefed in the standing notice.
  `never <verb>: <pattern>' blocks a call whose intended change matches, citing the line.
  `always require <evidence> before <verb>: <pattern>' blocks until evidence of that kind (a
  backup by cp, rsync, tar, zip or scp of the same tree) was recorded earlier in the same session.
  Verbs are delete, modify, create and any. A pattern ending in /** covers a tree, one carrying
  * ? or [ is a glob, any other is the path itself or its subtree; ~, $HOME and $NODECODE_HOME
  expand. The oath file itself is never modifiable by the agent, and this cell registers no tool
  that can write, weaken or delete it: changing the oath is an owner act.
  The oath_check tool dry-checks one hypothetical call: its form, and the strings object it names.
  dry-checks one hypothetical call. oath_status answers the oath, its parse errors and the
  ledger's counts; /oathkeeper says the same to the operator, and (help :oathkeeper) is this page.
  A call is read, not grepped: the spawns in an eval form ((sh ...), run-program) and the file
  verbs it calls (delete-file, write-file, edit) give the intended changes, and a shell command's
  rm, mv, cp, dd, truncate, tee, sed -i, redirection and mutating git verbs are scanned over its
  first 4096 characters. Inline interpreter code is not parsed: the fingerprint snapshot is the
  second line of defence, reporting files under the oath's patterns that changed outside the gate.
  Honest limits: a change made and reverted between two checks leaves no trace, files above
  hash_limit_bytes are fingerprinted by size and time, and the snapshot covers at most max_files
  files per pattern."
  "What (help :oathkeeper) answers while the cell runs.")

;;; --- the cell -----------------------------------------------------------------

(nle:define-cell oathkeeper
  (:section ("oathkeeper")
    (:guide "write the oath in ~/.nodecode/oathkeeper/oath.txt, one rule a line (never delete: ~/notes/**); ~
  oath_file names another file, watch_changes reports files changed outside the gate; next: /oathkeeper shows
  what is enforced")
    ("oath_file" :path
     :doc "the oath file the owner writes; ~/.nodecode/oathkeeper/oath.txt unless named here")
    ("watch_changes" :boolean :default t
     :doc "report files under the oath's patterns that changed outside an approved call")
    ("hash_limit_bytes" :integer :default 1000000 :min 1
     :doc "files up to this size are fingerprinted by content digest, larger ones by size and time")
    ("max_files" :integer :default 5000 :min 1
     :doc "how many files one pattern's fingerprint covers"))
  (:hook :tool #'check-or-run)
  (:tool "oath_check" (nlk:contract 'check-call) #'check-tool)
  (:tool "oath_status" +status-schema+ #'status-tool)
  (:command "oathkeeper" 'run-command
            :description "The oath in force, and what the gate has done")
  (:help :oathkeeper
         "oathkeeper: the owner's oath enforced on every tool call - (help :oathkeeper)"
         +manual+)
  (:start (lambda ()
            (when (setting :watch-changes) (ignore-errors (snapshot-now)))
            (let ((parsed (load-oath (oath-file))))
              (if (getf parsed :exists)
                  (nle:notice
                   (format nil "oathkeeper: ~d oath line~:p in force from ~a~a"
                           (length (getf parsed :rules)) (oath-file)
                           (if (getf parsed :missions)
                               (format nil " - standing missions: ~{~a~^; ~}" (getf parsed :missions))
                               ""))
                   :level :info :key +notice-key+)
                  (nle:notice (format nil "oathkeeper: no oath at ~a yet, so the gate lets everything pass"
                                      (oath-file))
                              :level :info :key +notice-key+)))
            (nle:on-stop (lambda ()
                           (nle:notice nil :key +notice-key+)
                           (nle:notice nil :key +changes-key+))))))
