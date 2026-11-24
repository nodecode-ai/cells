;;;; cell.lisp --- project stewardship: one project, its autonomy, its health.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original Hermes plugin keeps durable ownership of a project: a
;;;; settings record with an autonomy level, health cycles measured by
;;;; running the project's own declared checks, initiatives a model may
;;;; propose but a person approves, and a memory its agents read. This cell
;;;; is that workflow against one JSON file under the Nodecode home -- no
;;;; server, no dashboard, no kanban board: the five tools below are the
;;;; surface, /steward is the operator's, and the :ROUND hook gives a bound
;;;; session its project's line the way the original's pre_llm_call hook
;;;; gave a bot its project's memory.
;;;;
;;;; What it may run is what the project declares: an allowlist of bare
;;;; executable names, one argv per check, no shell, a deadline, and the
;;;; project's own directory as the working directory.

(in-package #:nodecode-steward)

(nlk:define-peripheral steward :not-running t)

;;; --- the six levels, and what each grants -------------------------------------

(defparameter +levels+ '("assistant" "investigator" "planner" "builder" "maintainer" "steward")
  "The autonomy ladder, weakest first: a project at a level has every
capability granted at it or below it, and none above.")

(defparameter +grants+
  '(("assistant" "observe")
    ("investigator" "research" "diagnostics")
    ("planner" "create_initiative")
    ("builder" "write_code" "run_command_evaluator")
    ("maintainer" "merge_low_risk")
    ("steward" "merge_any" "release_publish" "notify_gateway" "spawn_subagents"))
  "Each level's addition to the one below: the policy only ever widens the
ladder by adding, so a level's set is the union of every grant up to it.")

(defparameter +risks+ '("low" "medium" "high" "critical")
  "A proposed initiative's risk, as the original's RiskLevel.")

(defun level-p (level)
  "Whether LEVEL is one of the six names, in any case."
  (and (stringp level) (member (string-downcase level) +levels+ :test #'string=) t))

(defun require-level (level)
  (or (and (level-p level) (string-downcase level))
      (fail "~s is not an autonomy level: one of ~{~a~^, ~}" level +levels+)))

(defun capabilities-of (level)
  "Every capability a project at LEVEL grants: the union of the grants up to it."
  (let ((level (string-downcase (or level "assistant"))))
    (unless (level-p level) (return-from capabilities-of '()))
    (loop with all = '()
          for (name . grants) in +grants+
          do (setf all (append all grants))
          when (string= name level) return (remove-duplicates all :test #'string=))))

(defun permits-p (level capability)
  "Whether a project at LEVEL may use CAPABILITY."
  (and (member capability (capabilities-of (or level "assistant")) :test #'string=) t))

;;; --- the file this cell keeps ------------------------------------------------

(defvar *data-file* nil
  "A path bound by a test (or by an operator's eval) that stands in for the
section's data_file. NIL reads the setting.")

(defun data-path ()
  "Where the registry lives: the section's data_file, under the home."
  (or *data-file*
      (nlk:home (or (setting :data-file) "steward/projects.json"))))

(defun registry-or-nil ()
  "The decoded registry file, or NIL when there is none yet."
  (let ((path (data-path)))
    (when (probe-file path)
      (let ((text (ignore-errors (nlk:read-text path))))
        (when (and text (plusp (length text)))
          (let ((decoded (ignore-errors (nlk:decode-json text))))
            (when (hash-table-p decoded) decoded)))))))

(defun registry ()
  "The registry: projects by id, and which session follows which project."
  (let ((registry (registry-or-nil)))
    (cond ((null registry)
           (nlk:make-json-object "projects" (nlk:make-json-object)
                                  "sessions" (nlk:make-json-object)))
          ((null (lookup registry "projects"))
           (setf (gethash "projects" registry) (nlk:make-json-object))
           registry)
          (t registry))))

(defun save-registry (registry)
  "Write REGISTRY back, whole and atomically."
  (let ((path (data-path)))
    (ensure-directories-exist path)
    (nlk:write-file-atomically path (nlk:encode-json-object registry))
    registry))

;;; --- reading the decoded objects ----------------------------------------------

(defun lookup (table name &optional default)
  (if (hash-table-p table)
      (multiple-value-bind (value present) (gethash name table)
        (if present value default))
      default))

(defun put (table name value)
  (setf (gethash name table) value))

(defun text-member (table name &optional default)
  (or (nlk:json-value table :text name) default))

(defun list-of (value)
  "VALUE as a Lisp list: a decoded JSON array, or an empty one."
  (if (vectorp value) (coerce value 'list) '()))

(defun strings-member (table name)
  (remove-if-not #'stringp (list-of (lookup table name))))

(defun vector-of (list)
  (coerce (or list '()) 'vector))

(defun normalise-id (id)
  "A project id as it is filed: trimmed and lowercased."
  (let ((id (nlk:trimmed (or id ""))))
    (if (plusp (length id))
        (string-downcase id)
        (fail "a project needs an id: letters, digits, dashes"))))

;;; --- projects -----------------------------------------------------------------

(defun projects-table (registry)
  (or (lookup registry "projects") (put registry "projects" (nlk:make-json-object))))

(defun find-project (registry id)
  (and id (lookup (projects-table registry) (string-downcase id))))

(defun require-project (registry id)
  (or (find-project registry (or id ""))
      (fail "no project ~s yet: open it with steward_open first" id)))

(defun project-line (project)
  "One line: what a project is and where it stands."
  (let* ((id (text-member project "id" "?"))
         (autonomy (text-member project "autonomy" "assistant"))
         (health (lookup project "health"))
         (initiatives (list-of (lookup project "initiatives")))
         (open (count-if (lambda (one) (member (text-member one "state") '("proposed" "approved") :test #'string=))
                         initiatives))
         (checks (strings-member project "checks")))
    (format nil "~a~@[ (~a)~]: ~a~@[, ~d open initiative~:p~]~@[, ~d check~:p~]"
            id
            (text-member project "title")
            (if (hash-table-p health)
                (format nil "health ~a, measured ~a"
                        (text-member health "status" "unknown")
                        (text-member health "measured_at" "never"))
                "health not measured yet (steward_cycle measures it)")
            (and (plusp open) open)
            (and (plusp (length checks)) (length checks)))))

(defun new-project (id title path autonomy)
  (nlk:make-json-object "id" id
                         "title" (or title "")
                         "path" (or path "")
                         "autonomy" autonomy
                         "checks" (vector)
                         "allow" (vector)
                         "briefing" ""
                         "health" :null
                         "next_initiative" 1
                         "initiatives" (vector)
                         "created_at" (nlk:iso-now)))

;;; --- running a check -------------------------------------------------------------

(defparameter *split-chars* '(#\Space #\Tab #\Newline #\Return)
  "What separates one argument of a declared check from the next. No quoting:
a check that needs an argument holding a space declares it another way.")

(defun words (text)
  "TEXT split on whitespace: an argv, no shell in the way."
  (let ((words '()) (start nil))
    (loop for index from 0 below (length text)
          for char = (char text index)
          for space = (member char *split-chars*)
          do (cond (space
                    (when start (push (subseq text start index) words) (setf start nil)))
                   ((null start) (setf start index)))
          finally (when start (push (subseq text start) words)))
    (nreverse words)))

(defun check-parts (text)
  "A declared check as (values NAME HIGH COMMAND): `tests: make test', an
optional `name: ' before the command; a leading `!' on the name marks the
check high severity, whose failure alone degrades health."
  (let* ((text (nlk:trimmed text))
         (colon (search ": " text))
         (name (if colon (subseq text 0 colon) nil))
         (command (if colon (nlk:trimmed (subseq text (+ colon 2))) text))
         (high (and name (plusp (length name)) (char= #\! (char name 0))))
         (name (cond ((and name high) (subseq name 1))
                     (name name)
                     ((plusp (length (words command))) (first (words command)))
                     (t "check"))))
    (values name high command)))

(defun exec (argv directory seconds)
  "The one place a program runs: ARGV in DIRECTORY, bounded by SECONDS =>
(values OUTPUT ERROR STATUS). Tests stub this name."
  (nlk:run-bounded argv :seconds seconds :directory directory
                        :output :string :error-output :string))

(defun run-check (project check &key (seconds nil))
  "Run one declared CHECK in the project's own directory, and answer the
evidence: what ran, how it ended, and the head of what it said."
  (multiple-value-bind (name high command) (check-parts check)
    (let* ((argv (words command))
           (program (first argv))
           (allow (strings-member project "allow"))
           (directory (text-member project "path"))
           (seconds (or seconds (setting :timeout-seconds)))
           (result (nlk:make-json-object "name" name "high" (and high t)
                                          "command" command "state" "unmeasured")))
      (cond
        ((null argv)
         (put result "detail" "the check declares no command"))
        ((null allow)
         (put result "detail" "the project allows no command: name the executables in steward_open's allow"))
        ((not (member program allow :test #'string=))
         (put result "detail" (format nil "~a is not on this project's allowlist (~{~a~^, ~})" program allow)))
        ((or (null directory) (not (uiop:directory-exists-p directory)))
         (put result "detail" (format nil "the project's directory ~s is not there" directory)))
        (t
         (handler-case
             (multiple-value-bind (out err status)
                 (exec argv (uiop:ensure-directory-pathname directory) seconds)
               (declare (ignore err))
               (put result "command" (format nil "~{~a~^ ~}" argv))
               (put result "state" (cond ((eq status :timeout) "unmeasured")
                                          ((eql status 0) "passed")
                                          (t "failed")))
               (put result "exit" (if (integerp status) status :null))
               (when (eq status :timeout)
                 (put result "detail" (format nil "it did not finish within ~ds" seconds)))
               (put result "evidence" (nlk:one-line (or out "") :cap (setting :evidence-chars)))
               result)
           (steward-error (condition) (put result "detail" (princ-to-string condition)) result)
           (error (condition)
             (put result "detail" (format nil "could not start it: ~a" (nlk:one-line (princ-to-string condition) :cap 200)))
             result)))
        )
      result)))

(defun grade-check-results (results)
  "The original's health state machine over measured checks: anything that
could not run leaves the state unknown (fail closed); a high-severity
failure or two failures is degraded; one failure is watch; none is healthy."
  (let ((unmeasured (count "unmeasured" results :key (lambda (one) (text-member one "state")) :test #'string=))
        (failed (remove-if-not (lambda (one) (string= "failed" (text-member one "state"))) results)))
    (cond ((null results) "unknown")
          ((plusp unmeasured) "unknown")
          ((or (plusp (count-if (lambda (one) (and (text-member one "high") t)) failed))
               (>= (length failed) 2))
           "degraded")
          ((plusp (length failed)) "watch")
          (t "healthy"))))

(defun measure (project)
  "Run every check PROJECT declares and answer (values SNAPSHOT SUMMARY)."
  (let* ((checks (strings-member project "checks"))
         (results (mapcar (lambda (check) (run-check project check)) checks))
         (status (grade-check-results results))
         (snapshot (nlk:make-json-object
                    "status" status
                    "measured_at" (nlk:iso-now)
                    "failed" (count "failed" results
                                    :key (lambda (one) (text-member one "state")) :test #'string=)
                    "checks" (vector-of results))))
    (put project "health" snapshot)
    (values snapshot results)))

;;; --- the model-facing verbs --------------------------------------------------------

(defun arg (arguments name)
  "TABLE's member NAME, whether the decoder kept it as a string or a keyword."
  (when (hash-table-p arguments)
    (multiple-value-bind (value present) (gethash name arguments)
      (if present value (gethash (intern (string-upcase name) :keyword) arguments)))))

(defun arg-text (arguments name)
  (let ((value (arg arguments name)))
    (and (stringp value) (plusp (length (nlk:trimmed value))) (nlk:trimmed value))))

(defun arg-list (arguments name)
  (remove-if-not #'stringp (list-of (arg arguments name))))

(defun open-project (arguments)
  "steward_open: register a project, or change what its record says."
  (let* ((id (normalise-id (arg-text arguments "project")))
         (registry (registry))
         (table (projects-table registry))
         (project (or (lookup table id)
                      (put table id (new-project id nil nil "assistant"))))
         (checks (arg-list arguments "checks"))
         (allow (arg-list arguments "allow"))
         (autonomy (arg-text arguments "autonomy"))
         (path (arg-text arguments "path"))
         (title (arg-text arguments "title"))
         (briefing (arg arguments "briefing")))
    (when path
      (let ((directory (uiop:ensure-directory-pathname path)))
        (unless (uiop:directory-exists-p directory)
          (fail "the project's directory ~s is not there" path))
        (put project "path" (namestring directory))))
    (when title (put project "title" title))
    (when autonomy (put project "autonomy" (require-level autonomy)))
    (when (arg arguments "checks") (put project "checks" (vector-of checks)))
    (when (arg arguments "allow") (put project "allow" (vector-of allow)))
    (when (and briefing (stringp briefing)) (put project "briefing" briefing))
    (save-registry registry)
    (format nil "opened ~a~%~@[~a~]~@[autonomy: ~a~%~]~@[checks: ~{~a~^; ~}~%~]~@[allow: ~{~a~^, ~}~]"
            (project-line project)
            (and (text-member project "path") (format nil "directory: ~a" (text-member project "path")))
            (text-member project "autonomy")
            (strings-member project "checks")
            (strings-member project "allow"))))

(defun status-of (arguments)
  "steward_status: one project's settings, health and initiatives, or the board."
  (let* ((registry (registry))
         (id (arg-text arguments "project")))
    (if id
        (let* ((project (require-project registry id))
               (health (lookup project "health"))
               (initiatives (list-of (lookup project "initiatives"))))
          (format nil "~a~%~@[~a~]~@[~%briefing: ~a~]~%health: ~a~%~{  ~a~^~%~}~%initiatives:~%~{  ~a~^~%~}~%allow: ~{~a~^, ~}"
                  (project-line project)
                  (and (text-member project "path")
                       (format nil "directory: ~a" (text-member project "path")))
                  (let ((briefing (text-member project "briefing")))
                    (and briefing (plusp (length briefing)) briefing))
                  (if (hash-table-p health) (text-member health "status" "unknown") "not measured")
                  (loop for check across (or (nlk:json-array health "checks") #())
                        collect (format nil "- ~a: ~a~@[ (exit ~a)~]~@[ - ~a~]"
                                        (text-member check "name" "?")
                                        (text-member check "state" "?")
                                        (lookup check "exit")
                                        (text-member check "detail")))
                  (if initiatives
                      (loop for one in initiatives
                            collect (format nil "~a  ~a [~a~@[ ~a~]]"
                                            (text-member one "id" "?")
                                            (text-member one "title" "?")
                                            (text-member one "state" "?")
                                            (text-member one "risk")))
                      (list "  none"))
                  (strings-member project "allow")))
        (let ((projects (loop for id being the hash-keys of (projects-table registry)
                                using (hash-value project)
                              collect (project-line project))))
          (if projects
              (format nil "~d project~:p:~%~{~a~^~%~}" (length projects) (sort projects #'string<))
              "no projects yet: steward_open opens one")))))

(defun cycle-of (arguments)
  "steward_cycle: run the project's checks and record what they measured."
  (let* ((registry (registry))
         (project (require-project registry (arg-text arguments "project")))
         (autonomy (text-member project "autonomy" "assistant"))
         (checks (strings-member project "checks")))
    (unless (permits-p autonomy "run_command_evaluator")
      (fail "~a is at autonomy ~a, which does not permit run_command_evaluator: a cycle at builder or above may run its checks"
            (text-member project "id") autonomy))
    (when (null checks)
      (fail "~a declares no checks: name them in steward_open's checks, one `name: command' each"
            (text-member project "id")))
    (multiple-value-bind (snapshot results)
        (measure project)
      (save-registry registry)
      (format nil "~a: ~a, ~d check~:p measured on ~a~%~{~a~^~%~}"
              (text-member project "id")
              (text-member snapshot "status")
              (length results)
              (text-member snapshot "measured_at")
              (loop for one in results
                    collect (format nil "- ~a: ~a~@[ (exit ~a)~]~@[ - ~a~]~@[~%    ~a~]"
                                    (text-member one "name" "?")
                                    (text-member one "state" "?")
                                    (lookup one "exit")
                                    (text-member one "detail")
                                    (text-member one "evidence")))))))

(defun propose-initiative (arguments)
  "steward_propose: one initiative, under the same gates the cycle obeys."
  (let* ((registry (registry))
         (project (require-project registry (arg-text arguments "project")))
         (autonomy (text-member project "autonomy" "assistant"))
         (title (arg-text arguments "title"))
         (rationale (arg-text arguments "rationale"))
         (outcome (or (arg-text arguments "expected_outcome") ""))
         (risk (or (arg-text arguments "risk") "medium"))
         (health (lookup project "health")))
    (unless (permits-p autonomy "create_initiative")
      (fail "~a is at autonomy ~a, which does not permit create_initiative: raise it to planner or above"
            (text-member project "id") autonomy))
    (unless (hash-table-p health)
      (fail "no health snapshot for ~a: run steward_cycle so the proposal rests on a measurement"
            (text-member project "id")))
    (when (member (text-member health "status") '("unknown" "critical") :test #'string=)
      (fail "fail-closed: health=~a: no initiative is proposed on unverified or critical state"
            (text-member health "status")))
    (unless (member risk +risks+ :test #'string=)
      (fail "~s is not a risk: one of ~{~a~^, ~}" risk +risks+))
    (let* ((number (or (lookup project "next_initiative") 1))
           (initiative (nlk:make-json-object
                        "id" (format nil "i~d" number)
                        "title" title
                        "rationale" (or rationale "")
                        "expected_outcome" outcome
                        "risk" risk
                        "state" "proposed"
                        "proposed_at" (nlk:iso-now))))
      (put project "next_initiative" (1+ number))
      (put project "initiatives" (vector-of (append (list-of (lookup project "initiatives"))
                                                    (list initiative))))
      (save-registry registry)
      (format nil "proposed ~a: ~a (risk ~a) - approve it with /steward approve ~a ~a"
              (text-member initiative "id") title risk
              (text-member project "id") (text-member initiative "id")))))

(defun decide-initiative (registry id initiative-id state)
  "Move one initiative to STATE: what the operator's /steward approve or
reject does, and what a model may not do to its own proposal."
  (let* ((project (require-project registry id))
         (initiatives (list-of (lookup project "initiatives")))
         (wanted (find initiative-id initiatives
                       :key (lambda (one) (text-member one "id")) :test #'string-equal)))
    (unless wanted
      (fail "~a has no initiative ~s" (text-member project "id") initiative-id))
    (put wanted "state" state)
    (put wanted "decided_at" (nlk:iso-now))
    (put project "initiatives" (vector-of initiatives))
    (save-registry registry)
    (format nil "~a is now ~a: ~a" (text-member wanted "id") state (text-member wanted "title" "?"))))

;;; --- a session that follows a project -----------------------------------------

(defparameter +binding-key+ "project"
  "The registry's sessions map: a session id to the project it follows.")

(defun bound-project (session-id)
  "The project SESSION-ID follows, or NIL: the pre_llm_call hook's answer."
  (and session-id
       (let* ((registry (ignore-errors (registry-or-nil)))
              (id (and registry (text-member (lookup registry "sessions") session-id)))
              (project (and id (find-project registry id))))
         (and project (text-member project "id") project))))

(defun bind-session (session-id id)
  "Point SESSION-ID at ID, or at nothing when ID is empty."
  (let* ((registry (registry))
         (sessions (or (lookup registry "sessions")
                       (put registry "sessions" (nlk:make-json-object)))))
    (cond ((and id (plusp (length id)))
           (let ((project (require-project registry id)))
             (put sessions session-id (text-member project "id"))
             (save-registry registry)
             (format nil "this session follows ~a: its line rides each request" (text-member project "id"))))
          (t (remhash session-id sessions)
             (save-registry registry)
             "this session follows no project"))))

(defun memory-text (project)
  "The project's line as a session reads it: state first, then what is open."
  (let ((initiatives (remove-if-not (lambda (one) (member (text-member one "state")
                                                          '("proposed" "approved") :test #'string=))
                                    (list-of (lookup project "initiatives"))))
        (health (lookup project "health"))
        (briefing (text-member project "briefing")))
    (format nil "steward: ~a is at autonomy ~a; health ~a~@[ (measured ~a)~].~@[~%Open initiatives: ~{~a~^; ~}.~]~@[~%Project briefing: ~a~]"
            (text-member project "id")
            (text-member project "autonomy" "assistant")
            (if (hash-table-p health) (text-member health "status" "unknown") "not measured")
            (and (hash-table-p health) (text-member health "measured_at"))
            (and initiatives
                 (loop for one in initiatives
                       collect (format nil "~a ~a [~a]"
                                       (text-member one "id")
                                       (text-member one "title")
                                       (text-member one "state"))))
            (and briefing (plusp (length briefing)) briefing))))

(defun deliver (op)
  "At each model request: a bound session's project line, in that round."
  (let ((session (getf op :session-id))
        (turn (getf op :turn)))
    (when (and session turn)
      (nlk:when-let (project (bound-project session))
        (nlk:record-turn-context-prompt turn :text (memory-text project))))))

(defun follow (arguments)
  "steward_bind: this session reads ID's line each round; empty unbinds."
  (let ((session (or (arg-text arguments "session") nlk:*scribe-session-id*)))
    (unless session
      (fail "steward_bind needs the session: pass session, or call it from a session's eval"))
    (bind-session session (or (arg-text arguments "project") ""))))

;;; --- the operator's /steward ------------------------------------------------------

(defun steer (args session-id)
  "One /steward: the board, one project, a review decision, or the binding."
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (head (if space (subseq text 0 space) text))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (cond
      ((zerop (length text)) (status-of (nlk:make-json-object)))
      ((string-equal head "bind")
       (let ((rest (if (string-equal (subseq rest 0 (min 5 (length rest))) "this ") (subseq rest 5) rest)))
         (bind-session session-id (or rest ""))))
      ((string-equal head "cycle") (cycle-of (nlk:json-object "project" rest)))
      ((string-equal head "approve")
       (multiple-value-bind (id initiative) (two-words rest)
         (decide-initiative (registry) id initiative "approved")))
      ((string-equal head "reject")
       (multiple-value-bind (id initiative) (two-words rest)
         (decide-initiative (registry) id initiative "rejected")))
      (t (status-of (nlk:json-object "project" text))))))

(defun two-words (text)
  "TEXT's first two words: a project and an initiative."
  (let* ((words (words text))
         (id (first words))
         (initiative (second words)))
    (unless (and id initiative)
      (fail "/steward needs a project and an initiative number, e.g. /steward approve alpha i1"))
    (values id initiative)))

(defparameter +primer+
  "Project stewardship keeps the durable state of a project a session is responsible for: its autonomy
level, the health its own checks measured, and the initiatives proposed for it. It answers five tools:

- steward_open {project, title?, path?, autonomy?, checks?, allow?, briefing?} registers a project, or
  changes it. autonomy is one of assistant, investigator, planner, builder, maintainer, steward, weakest
  first; it only ever widens what the project permits. checks are strings, one `name: command' each
  (`!name: command' makes a failure high severity); allow names the executables a check may use, bare
  names only -- an empty allow runs nothing.
- steward_status {project?} answers one project's settings, last measurement and initiatives, or the
  whole board when project is left out.
- steward_cycle {project} runs the project's checks in its own directory and records the result: healthy
  when every check passed, watch when one failed, degraded when two failed or a high-severity one did,
  unknown when a check could not run. A cycle needs autonomy builder or above.
- steward_propose {project, title, rationale, expected_outcome?, risk?} files an initiative and leaves it
  for a person: it needs planner or above and a measured health that is neither unknown nor critical.
- steward_bind {project} makes this session read the project's line at each request; call it with no
  project to stop.

A person approves or rejects with /steward approve PROJECT INITIATIVE, or /steward reject. Health is
what the checks measured, never a model's opinion: report it as measured, and never as a percentage.")

;;; --- the schemas the model is shown -------------------------------------------------

(defun object-schema (&rest properties)
  "A JSON-Schema object over PROPERTIES, each (NAME TYPE DESCRIPTION)."
  (nlk:json-object "type" "object"
                    "properties" (apply #'nlk:make-json-object
                                        (loop for (name type about) on properties by #'cddr
                                              append (list name (nlk:make-json-object "type" type
                                                                                      "description" about))))
                    "additionalProperties" t))

(defparameter +open-schema+
  (list :description "Register a project this session is responsible for, or change its record: its directory, its autonomy level, the checks that measure its health, and the executables they may run."
        :parameters (object-schema
                     "project" "string" "the project's id: short, lowercase, dashes"
                     "title" "string" "its human name"
                     "path" "string" "its directory: where its checks run"
                     "autonomy" "string" "assistant, investigator, planner, builder, maintainer or steward; weakest first"
                     "checks" "array" "one string per check, `name: command' (`!name: command' is high severity)"
                     "allow" "array" "the bare executable names a check may run; empty runs nothing"
                     "briefing" "string" "what an agent working here should know: rules, decisions, facts")))

(defparameter +status-schema+
  (list :description "One project's settings, its last measured health and its initiatives; with no project, the whole board."
        :parameters (object-schema
                     "project" "string" "the project's id; leave it out for every project")))

(defparameter +cycle-schema+
  (list :description "Run a project's declared checks in its own directory and record the health they measured: healthy, watch, degraded, or unknown when a check could not run. Needs autonomy builder or above."
        :parameters (object-schema
                     "project" "string" "the project's id")))

(defparameter +propose-schema+
  (list :description "Propose one initiative for a project, for a person to approve. Needs autonomy planner or above and a measured health that is neither unknown nor critical."
        :parameters (object-schema
                     "project" "string" "the project's id"
                     "title" "string" "one line: what the initiative does"
                     "rationale" "string" "why it is worth doing now, from what was measured"
                     "expected_outcome" "string" "how anyone can tell it worked"
                     "risk" "string" "low, medium, high or critical")))

(defparameter +bind-schema+
  (list :description "Make this session follow a project: its line rides each model request. Call it with no project to stop following."
        :parameters (object-schema
                     "project" "string" "the project's id; leave it out to unbind"
                     "session" "string" "the session to bind; default this one")))

;;; --- the section -----------------------------------------------------------------------

(defun install ()
  "Start: the registry's folder is there, so the first write never fails."
  (let ((path (data-path)))
    (ignore-errors (ensure-directories-exist path)))
  nil)

(nle:define-cell steward
  (:section ("steward")
    (:guide "no key and no server: one JSON file holds the projects; /steward shows the board, and steward_open opens the first project")
    ("data_file" :string :default "steward/projects.json"
                 :doc "the registry, relative to the Nodecode home")
    ("timeout_seconds" :integer :default 60 :min 1
                       :doc "one check's ceiling, in seconds")
    ("evidence_chars" :integer :default 400 :min 40
                      :doc "how much of a check's output is kept as evidence"))
  (:tool "steward_open" +open-schema+ 'open-project)
  (:tool "steward_status" +status-schema+ 'status-of)
  (:tool "steward_cycle" +cycle-schema+ 'cycle-of)
  (:tool "steward_propose" +propose-schema+ 'propose-initiative)
  (:tool "steward_bind" +bind-schema+ 'follow)
  (:help :steward
         "steward: a project's autonomy, measured health and proposed initiatives; the tools are steward_open, steward_status, steward_cycle, steward_propose and steward_bind; /steward shows the board"
         +primer+)
  (:command "steward" 'steer
            :description "Project stewardship: the board, one project, or a review decision"
            :argument-hint "PROJECT | bind PROJECT | approve PROJECT INITIATIVE | cycle PROJECT")
  (:hook :round (nle:observer #'deliver "steward"))
  (:start #'install))
