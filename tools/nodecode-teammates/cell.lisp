;;;; cell.lisp --- named teammates: a roster of roles, and the runs they do.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The operator writes a roster in the `teammates' section: one entry per
;;;; role, holding the line the agent reads to pick it and the instructions it
;;;; works under, and optionally the provider and model it runs on. The agent
;;;; picks a NAME, never a provider or a model.
;;;;
;;;; A run is an ordinary durable session under the session that assigned it:
;;;; its transcript is its memory, its id is what every other tool takes, and
;;;; /sessions draws it under its assigner. A follow-up (`continue') goes to
;;;; the run's own session, which is what carries the earlier work.
;;;;
;;;; Six tools -- teammates_roster, teammate_assign, teammate_check,
;;;; teammate_wait, teammate_message, teammate_stop -- and /teammates. The
;;;; run ids are recorded on the assigning session (its own key in the store's
;;;; per-session state), so what this conversation assigned survives a restart
;;;; and needs no ledger of its own. See NOTICE for what of the original is
;;;; not here.

(defpackage #:nodecode-teammates
  (:use #:cl))

(in-package #:nodecode-teammates)

(nlk:define-peripheral teammates :not-running t)

;;; --- text ----------------------------------------------------------------------

(defun squeeze (value)
  "VALUE as one line of text: runs of whitespace collapsed to one space."
  (if (stringp value)
      (with-output-to-string (out)
        (let ((space nil))
          (loop for char across (string-trim '(#\Space #\Tab #\Newline #\Return) value)
                do (if (member char '(#\Space #\Tab #\Newline #\Return))
                       (setf space t)
                       (progn (when space (write-char #\Space out))
                              (setf space nil)
                              (write-char char out))))))
      ""))

(defun clip (text limit)
  (cond ((not (stringp text)) nil)
        ((<= (length text) limit) text)
        (t (concatenate 'string (subseq text 0 limit) "..."))))

;;; --- the roster ----------------------------------------------------------------

(defstruct (teammate (:constructor make-teammate (name description instructions provider model effort)))
  "One named teammate: what the agent picks, the line it reads, the
instructions the run works under, and what it runs on when the operator
pinned a provider and a model."
  name description instructions provider model effort)

(defun teammate-name-p (name)
  "Whether NAME is a teammate name: a lowercase letter, then lowercase
letters, digits, '-' or '_', up to 32."
  (and (stringp name)
       (<= 1 (length name) 32)
       (lower-case-p (char name 0))
       (every (lambda (char)
                (or (lower-case-p char) (digit-char-p char) (member char '(#\- #\_))))
              name)))

(defun teammate-entry (name object)
  "One roster entry => a TEAMMATE, or a string naming what is wrong with it."
  (if (not (hash-table-p object))
      (format nil "teammate ~a is not an object with a description" name)
      (flet ((text (key) (squeeze (nlk:json-value object :any key))))
        (let ((description (text "description")) (instructions (text "instructions"))
              (provider (text "provider")) (model (text "model")) (effort (text "reasoning_effort")))
          (cond ((not (teammate-name-p name))
                 (format nil "~a is not a teammate name: lowercase letters, digits, '-' or '_', ~
                              up to 32, starting with a letter" name))
                ((zerop (length description))
                 (format nil "teammate ~a needs a description: it is what the agent reads to choose" name))
                ((and (plusp (length provider)) (zerop (length model)))
                 (format nil "teammate ~a names a provider without a model" name))
                ((and (plusp (length model)) (zerop (length provider)))
                 (format nil "teammate ~a names a model without a provider" name))
                (t (make-teammate name description
                                   (if (plusp (length instructions)) instructions nil)
                                   (if (plusp (length provider)) provider nil)
                                   (if (plusp (length model)) model nil)
                                   (if (plusp (length effort)) effort nil))))))))

(defun read-roster (raw)
  "RAW, the section's `teammates' object, as (VALUES TEAMMATES PROBLEMS): one
TEAMMATE per usable entry, by name, and one line per entry skipped. A
malformed entry is skipped, never guessed at."
  (let ((mates '()) (problems '()))
    (cond ((null raw) nil)
          ((hash-table-p raw)
           (let ((pairs '()))
             (maphash (lambda (name object) (push (cons name object) pairs)) raw)
             ;; A JSON object arrives as a hash table, which keeps no order;
             ;; sorted by name, the list reads the same every time.
             (dolist (pair (sort pairs #'string< :key #'car))
               (let ((entry (teammate-entry (car pair) (cdr pair))))
                 (if (stringp entry) (push entry problems) (push entry mates))))))
          (t (push "teammates must be an object: one entry per teammate, keyed by name" problems)))
    (values (nreverse mates) (nreverse problems))))

(defun teammates () (setting :teammates))

(defun teammate-names () (mapcar #'teammate-name (teammates)))

(defun teammate-for (name)
  (and (stringp name) (stringp (squeeze name))
       (find (squeeze name) (teammates) :key #'teammate-name :test #'string=)))

(defun teammate-runs-on (mate)
  "`provider · model [· reasoning X]' for one teammate, or NIL when it runs
on whatever the assigning session runs on."
  (when (and (teammate-provider mate) (teammate-model mate))
    (format nil "~a · ~a~@[ · reasoning ~a~]"
            (teammate-provider mate) (teammate-model mate) (teammate-effort mate))))

(defun teammate-lines ()
  "One line per teammate for a tool description: its name and its line."
  (with-output-to-string (out)
    (loop for mate in (teammates)
          for sep = "" then (string #\Newline)
          do (format out "~a~a: ~a" sep (teammate-name mate) (teammate-description mate)))))

;;; --- runs ----------------------------------------------------------------------

(defparameter +runs-key+ "teammates.runs"
  "The assigning session's own key holding the run ids it started, newest
first, one per line.")

(defparameter +run-prefix+ "mate-"
  "Every run's session id opens with this: it is how a run is told from any
other session before the cell touches it.")

(defun teammate-run-p (run)
  (and (stringp run) (uiop:string-prefix-p +run-prefix+ run)))

(defun read-runs (session)
  "The run ids SESSION started, newest first, off its own per-session state."
  (let ((text (and (stringp session)
                   (ignore-errors (nlk:session-state-get session +runs-key+)))))
    (if (and (stringp text) (plusp (length text)))
        (remove "" (uiop:split-string text :separator '(#\Newline)) :test #'string=)
        '())))

(defun remember-run (session run)
  "Record RUN on SESSION's own state: what this conversation assigned is
durable, and needs no ledger beside the sessions themselves."
  (when (and (stringp session) (plusp (length session)))
    (ignore-errors
      (nlk:session-state-put session +runs-key+
                              (format nil "~{~a~%~}"
                                      (cons run (remove run (read-runs session) :test #'equal)))))))

(defun recent-runs (session &optional (limit 10))
  (let ((runs (read-runs session)))
    (subseq runs 0 (min limit (length runs)))))

;;; --- what a run stands at ---------------------------------------------------------

(defun safe-count (run kind)
  (or (ignore-errors (nlk:events :session-id run :kind kind :as :count)) 0))

(defun active-turn-id (run)
  "The id of RUN's turn without a terminal fact, or NIL when it is at rest."
  (let ((payload (and (ignore-errors (nlk:events :session-id run :kind "turn.started"
                                                 :order :newest :as :payload)))))
    (and payload (nlk:json-value payload :string "turn-id"))))

(defun last-reply (run)
  "The newest assistant text of RUN, or NIL."
  (let* ((payload (ignore-errors
                   (nlk:events :session-id run :kind "turn.assistant_message_completed"
                               :order :newest :as :payload)))
         (message (and payload (nlk:json-value payload :any "message")))
         (text (and message (nle::content-text (nlk:json-value message :any "content")))))
    (and (stringp text) (plusp (length text)) text)))

(defun inspect-run (run)
  "What RUN stands at, as a plist: :RUN, :STATE, :TURNS, :ANSWER. STATE is
:UNKNOWN (no such session), :STARTING (made, no turn yet), :RUNNING (a turn
without a terminal fact), :DONE, or :FAILED (at rest with no answer after a
turn ended in an error)."
  (if (not (and (teammate-run-p run) (ignore-errors (nlk:session-exists-p run))))
      (list :run run :state :unknown :turns 0 :answer nil)
      (let* ((turns (safe-count run "turn.started"))
             (ended (+ (safe-count run "turn.completed") (safe-count run "turn.failed")
                       (safe-count run "turn.cancelled")))
             (answer (ignore-errors (last-reply run)))
             (failed (safe-count run "turn.failed")))
        (list :run run :turns turns :answer answer
              :state (cond ((zerop turns) :starting)
                           ((> turns ended) :running)
                           ((and (null answer) (plusp failed)) :failed)
                           (t :done))))))

(defun at-rest-p (state)
  (not (member state '(:running :starting))))

(defun state-label (state)
  (case state
    (:running "running") (:starting "starting")
    (:done "done") (:failed "failed: its turn ended in an error, with no answer")
    (:unknown "not a run this image knows") (t (string-downcase (princ-to-string state)))))

(defun run-lines (run info)
  "One run's line, then its answer when it is at rest."
  (with-output-to-string (out)
    (format out "~a  ~a~@[  · ~d turn~:p~]" run (state-label (getf info :state)) (getf info :turns))
    (when (and (at-rest-p (getf info :state)) (getf info :answer))
      (format out "~%  ~a" (clip (getf info :answer) 2000)))))

;;; --- the prompt a run is given ----------------------------------------------------

(defun run-prompt (mate goal context continued)
  "What a teammate reads: the instructions the operator wrote, the goal
whole, the context the assigning agent passed, and what is expected back."
  (with-output-to-string (out)
    (format out "You are ~s, a teammate working for the session that assigned this task.~@[ ~a~]~%~%"
            (teammate-name mate) (teammate-instructions mate))
    (format out "The task:~%~a" goal)
    (when (and (stringp context) (plusp (length (squeeze context))))
      (format out "~%~%What the assigner passed to you with it:~%~a" context))
    (when continued
      (format out "~%~%This continues the work you did in this session; your transcript here is all of it."))
    (format out "~%~%Answer with your result: the assigner reads only your last message.~%")))

;;; --- launching, steering, stopping --------------------------------------------------

(defun live-runs (session)
  "The runs of SESSION that still have a turn open."
  (loop for run in (read-runs session)
        for info = (ignore-errors (inspect-run run))
        when (and info (member (getf info :state) '(:running :starting))) collect run))

(defun launch (mate goal context continue)
  "Start a run of MATE, or put GOAL to the run named by CONTINUE => (values
RUN-ID STATE)."
  (unless (nlk:store-open-p) (fail "no store is open; a teammate run is a durable session"))
  (let* ((me nlk:*scribe-session-id*)
         (prompt (run-prompt mate goal context (and continue t)))
         (live (and me (live-runs me))))
    (cond
      ((and continue (not (teammate-run-p continue)))
       (fail "~a is not a teammate run id: they read ~a<name>-XXXX" continue +run-prefix+))
      ((and (not continue) (>= (length live) (setting :max-live-runs)))
       (fail "~d teammate runs of this session are still live (~{~a~^, ~}); check or stop one, or raise ~
              teammates.max_live_runs" (setting :max-live-runs) live))
      (continue
       (nle:submit continue prompt :source "teammates" :source-id me)
       (values continue :running))
      (t
       (let* ((id (nlk:make-durable-id (format nil "~a~a" +run-prefix+ (teammate-name mate))))
              (cwd (or (and me (ignore-errors (nlk:find-session-cwd me))) (uiop:getcwd))))
         (nlk:create-session :id id :cwd (namestring cwd)
                              :parent (and me (nlk:session-exists-p me) me) :detached t)
         ;; After the create, so the teammate's pin wins over the parent's.
         (when (teammate-runs-on mate)
           (nlk:record-session-model-selection id
                                               :provider (teammate-provider mate)
                                               :model (teammate-model mate)
                                               :effort (teammate-effort mate)))
         (remember-run me id)
         (nle:submit id prompt :source "teammates" :source-id me)
         (values id :running))))))

(defun send (run text)
  "Put TEXT to RUN: a steer of the turn it runs, or its next turn when it is
at rest => a line saying which."
  (let* ((turn (active-turn-id run))
         (admission (nle:submit run text :steer turn :source "teammates"
                                     :source-id nlk:*scribe-session-id*))
         (disposition (nlk:active-input-admission-disposition admission)))
    (format nil "~a: ~a" run
            (case disposition
              (:started "took it as its next turn")
              (:queued "queued behind the turn it is running")
              (:steered "steers the turn it runs; it reads it at the next round boundary")
              (:duplicate "already had this exact input")
              (t (string-downcase (princ-to-string disposition)))))))

(defun halt (run reason)
  "Cancel RUN's open turn => a line saying what happened."
  (let ((turn (active-turn-id run)))
    (if (null turn)
        (format nil "~a: nothing to stop, it is at rest" run)
        (progn (nlk:request-cancel-turn run turn
                                        (if (and (stringp reason) (plusp (length (squeeze reason))))
                                            (squeeze reason)
                                            "stopped with teammate_stop"))
               (format nil "~a: its turn is being cancelled" run)))))

(defun await (runs mode seconds)
  "Poll RUNS until MODE is met or SECONDS pass => (values INFOS WAITED SETTLED)."
  (let ((started (get-universal-time))
        (deadline (+ (get-universal-time) seconds)))
    (loop
      (let* ((infos (mapcar (lambda (run) (funcall *inspect* run)) runs))
             (settled (count-if (lambda (info) (at-rest-p (getf info :state))) infos))
             (met (if (and (stringp mode) (string-equal mode "any"))
                      (plusp settled)
                      (= settled (length infos)))))
        (when (or met (>= (get-universal-time) deadline))
          (return (values infos (- (get-universal-time) started) settled))))
      (sleep 1/4))))

;;; --- the seams a test stubs ---------------------------------------------------------
;;; Symbols rather than function values, so a test (or another cell) can
;;; substitute one without loading the store or the gateway.

(defparameter *launch* 'launch
  "What teammate_assign hands a run to: (MATE GOAL CONTEXT CONTINUE) => (values RUN-ID STATE).")

(defparameter *inspect* 'inspect-run
  "What teammate_check, teammate_wait and /teammates read a run through: (RUN) => a plist.")

(defparameter *send* 'send
  "What teammate_message hands its text to: (RUN TEXT) => a line.")

(defparameter *halt* 'halt
  "What teammate_stop hands the run to: (RUN REASON) => a line.")

;;; --- the tools ---------------------------------------------------------------------

(defun json-string (arguments key)
  (let ((value (nlk:json-value arguments :any key)))
    (and (stringp value) value)))

(defun json-strings (arguments key)
  (let ((raw (nlk:json-value arguments :any key)))
    (cond ((vectorp raw) (coerce raw 'list))
          ((listp raw) raw)
          (t nil))))

(defun report-runs (runs &optional (limit 10))
  (let ((shown (subseq runs 0 (min limit (length runs)))))
    (with-output-to-string (out)
      (dolist (run shown)
        (format out "~a~%" (run-lines run (funcall *inspect* run)))))))

(defun roster-text (session)
  "The roster, then what this conversation assigned: the answer to
teammates_roster and to /teammates typed alone."
  (with-output-to-string (out)
    (format out "Teammates (~d)~%~%" (length (teammates)))
    (if (teammates)
        (dolist (mate (teammates))
          (format out "  ~a  ~a~@[  (runs on ~a)~]~%" (teammate-name mate)
                  (teammate-description mate) (teammate-runs-on mate)))
        (format out "  (none yet: write them in the teammates section of the config)~%"))
    (when (setting :problems)
      (format out "~%Problems~%~{  ~a~%~}" (setting :problems)))
    (let ((runs (recent-runs session)))
      (format out "~%Runs from this session~%")
      (if runs
          (format out "~a" (report-runs runs))
          (format out "  (none yet)~%")))))

(defun tool-roster (arguments)
  (declare (ignore arguments))
  (roster-text nlk:*scribe-session-id*))

(defun tool-assign (arguments)
  (let* ((name (squeeze (json-string arguments "teammate")))
         (goal (squeeze (json-string arguments "goal")))
         (context (json-string arguments "context"))
         (continue (squeeze (json-string arguments "continue"))))
    (when (zerop (length goal))
      (fail "a teammate needs a goal: the words it works from"))
    (let ((mate (teammate-for name)))
      (unless mate
        (fail "~a" (cond ((and (plusp (length name)) (teammates))
                             (format nil "~a is not a teammate; the roster is ~{~a~^, ~}" name (teammate-names)))
                            ((plusp (length name))
                             (format nil "~a is not a teammate, and the roster is empty: write them in the teammates section" name))
                            (t "a goal needs the name of a teammate from the roster"))))
      (when (and (plusp (length continue)) (not (teammate-run-p continue)))
        (fail "~a is not a teammate run id: they read ~a<name>-XXXX, as teammates_roster gives them"
              continue +run-prefix+))
      (multiple-value-bind (run state)
          (funcall *launch* mate goal
                   (and (stringp context) (plusp (length (squeeze context))) context)
                   (and (plusp (length continue)) continue))
        (format nil "~a: ~a (~a), ~a~@[~%it runs on ~a~]~%teammate_check takes the run id when you want it, ~
                     or teammate_wait to block until it is done."
                (teammate-name mate) run (state-label state)
                (if (plusp (length continue))
                    "the same session, so its earlier work is there"
                    "a session of its own")
                (teammate-runs-on mate))))))

(defun tool-check (arguments)
  (let* ((run (squeeze (json-string arguments "run")))
         (seconds (nlk:json-value arguments :integer "wait_seconds")))
    (when (zerop (length run))
      (fail "teammate_check needs a run id, as teammates_roster gives them"))
    (unless (teammate-run-p run)
      (fail "~a is not a teammate run id: they read ~a<name>-XXXX, as teammates_roster gives them"
            run +run-prefix+))
    (when (and (integerp seconds) (plusp seconds))
      (let ((infos (multiple-value-bind (infos) (await (list run) "all" seconds)
                     infos)))
        (return-from tool-check (run-lines run (first infos)))))
    (run-lines run (funcall *inspect* run))))

(defun tool-wait (arguments)
  (let* ((runs (remove-if-not (lambda (run) (plusp (length run)))
                              (mapcar #'squeeze (json-strings arguments "runs"))))
         (mode (or (json-string arguments "mode") "all"))
         (seconds (or (nlk:json-value arguments :integer "seconds") (setting :wait-seconds))))
    (unless runs (fail "teammate_wait needs the run ids to wait on, as teammates_roster gives them"))
    (unless (member (string-downcase mode) '("any" "all") :test #'string=)
      (fail "mode is any or all, not ~a" mode))
    (multiple-value-bind (infos waited settled)
        (await runs mode (max 1 seconds))
      (format nil "waited ~ds: ~d of ~d ~a~%~{~a~}"
              waited settled (length runs)
              (if (= settled (length runs)) "settled" "settled so far")
              (mapcar (lambda (info) (run-lines (getf info :run) info)) infos)))))

(defun tool-message (arguments)
  (let ((run (squeeze (json-string arguments "run")))
        (text (json-string arguments "text")))
    (when (zerop (length run)) (fail "teammate_message needs a run id"))
    (when (zerop (length (squeeze (or text "")))) (fail "teammate_message needs some words: text"))
    (unless (teammate-run-p run)
      (fail "~a is not a teammate run id: they read ~a<name>-XXXX" run +run-prefix+))
    (funcall *send* run text)))

(defun tool-stop (arguments)
  (let ((run (squeeze (json-string arguments "run")))
        (reason (json-string arguments "reason")))
    (when (zerop (length run)) (fail "teammate_stop needs a run id"))
    (unless (teammate-run-p run)
      (fail "~a is not a teammate run id: they read ~a<name>-XXXX" run +run-prefix+))
    (funcall *halt* run reason)))

;;; --- the schemas ---------------------------------------------------------------------

(defun tool-schema (name description properties required)
  (list :description description
        :parameters (nlk:make-json-object "type" "object" "properties" properties
                                           "required" (coerce required 'vector))))

(defun roster-schema ()
  (tool-schema "teammates_roster"
                "The operator's teammate roster, and the runs this session started with their state."
                (nlk:make-json-object) '()))

(defun assign-schema ()
  (tool-schema "teammate_assign"
                "Give a task to one of the operator's named teammates; it works in a session of its own and answers there."
                (nlk:make-json-object
                 "teammate" (nlk:make-json-object
                              "type" "string"
                              "enum" (coerce (teammate-names) 'vector)
                              "description" (format nil "Which teammate works it.~%~a" (teammate-lines)))
                 "goal" (nlk:make-json-object
                         "type" "string"
                         "description" "The task, whole: the teammate sees this, its own instructions, and the context you pass -- never your conversation.")
                 "context" (nlk:make-json-object
                            "type" "string"
                            "description" "What it needs and cannot see: an earlier result, a path, a constraint.")
                 "continue" (nlk:make-json-object
                             "type" "string"
                             "description" "A run id from teammates_roster to put this goal to that same teammate session, so its earlier work is there."))
                '("teammate" "goal")))

(defun check-schema ()
  (tool-schema "teammate_check"
                "Where one teammate run stands: running or at rest, and its answer once it is done."
                (nlk:make-json-object
                 "run" (nlk:make-json-object "type" "string"
                                              "description" "The run id teammates_roster or teammate_assign gave.")
                 "wait_seconds" (nlk:make-json-object
                                 "type" "integer"
                                 "description" "Block this long for the run to finish; 0 checks now (the default)."))
                '("run")))

(defun wait-schema ()
  (tool-schema "teammate_wait"
                "Block until some or all teammate runs are at rest, or the seconds run out, and answer where each stands."
                (nlk:make-json-object
                 "runs" (nlk:make-json-object "type" "array" "items" (nlk:make-json-object "type" "string")
                                               "description" "The run ids to wait on.")
                 "mode" (nlk:make-json-object "type" "string" "enum" (vector "any" "all")
                                               "description" "any: return when one is at rest; all (the default): when every one is.")
                 "seconds" (nlk:make-json-object
                            "type" "integer"
                            "description" "How long to wait; defaults to the section's wait_seconds."))
                '("runs")))

(defun message-schema ()
  (tool-schema "teammate_message"
                "Send a teammate more words: a steer of the turn it is running, or its next turn when it is at rest."
                (nlk:make-json-object
                 "run" (nlk:make-json-object "type" "string" "description" "The run id.")
                 "text" (nlk:make-json-object "type" "string"
                                               "description" "What to tell it: a correction, a constraint, the next step."))
                '("run" "text")))

(defun stop-schema ()
  (tool-schema "teammate_stop"
                "Cancel the turn a teammate run is on; the session and everything it wrote stay."
                (nlk:make-json-object
                 "run" (nlk:make-json-object "type" "string" "description" "The run id.")
                 "reason" (nlk:make-json-object "type" "string" "description" "Why, in one line: it is recorded on the turn."))
                '("run")))

(defparameter +tools+
  '(("teammates_roster" roster-schema tool-roster)
    ("teammate_assign" assign-schema tool-assign)
    ("teammate_check" check-schema tool-check)
    ("teammate_wait" wait-schema tool-wait)
    ("teammate_message" message-schema tool-message)
    ("teammate_stop" stop-schema tool-stop))
  "What this cell offers the model, in the order it offers them.")

;;; --- /teammates -------------------------------------------------------------------------

(defun one-teammate-text (mate session)
  (with-output-to-string (out)
    (format out "~a  ~a~%~@[  runs on ~a~%~]~@[~%~a~%~]"
            (teammate-name mate) (teammate-description mate) (teammate-runs-on mate)
            (teammate-instructions mate))
    (let ((prefix (format nil "~a~a-" +run-prefix+ (teammate-name mate)))
          (runs (recent-runs session 20)))
      (setf runs (remove-if-not (lambda (run) (uiop:string-prefix-p prefix run)) runs))
      (if runs
          (progn (format out "~%Runs of ~a~%" (teammate-name mate))
                 (format out "~a" (report-runs runs 5)))
          (format out "~%No runs of ~a from this session yet.~%" (teammate-name mate))))))

(defun run-slash (args session-id)
  "Typing /teammates alone: the roster and this session's runs.
/teammates NAME: one teammate, its instructions and its runs."
  (let ((wanted (squeeze (or args ""))))
    (if (zerop (length wanted))
        (roster-text session-id)
        (let ((mate (teammate-for wanted)))
          (unless mate
            (fail "~a is not a teammate~@[; the roster is ~{~a~^, ~}~]" wanted (teammate-names)))
          (one-teammate-text mate session-id)))))

;;; --- the manual -------------------------------------------------------------------------

(defparameter +primer+
  "A teammate is one entry of the operator's roster in the teammates section: a
name the agent picks, a line it reads, the instructions the run works under, and
optionally the provider and model it runs on (never a choice the agent makes).
teammate_assign gives one a goal; it runs in a session of its own under this
one, which the agent does not see into, so the goal and the context are the
whole of what it knows beyond its instructions. teammate_check reads where a run
stands and its answer; teammate_wait blocks until runs are at rest; teammate_message
steers a running run or gives the next step to one at rest; teammate_stop cancels
its turn; teammates_roster lists the roster and this session's runs. Assigning
again with `continue' set to a run id puts the new goal to that same session, so
its earlier work is there. /teammates prints the same roster, /teammates NAME
one teammate, its instructions and its runs."
  "What (help :teammates) answers while the cell runs.")

;;; --- the cell -----------------------------------------------------------------------------

(nle:define-cell teammates
  (:section ("teammates")
    (:guide "one entry per role in the teammates object: {name: {description, instructions, provider, model}}; description is what the agent reads to pick it, instructions what the run works under, provider and model optional; seconds and max_live_runs bound a run")
    ("seconds" :integer :default 900 :min 30
               :doc "how long one teammate turn may run before it is stopped")
    ("wait_seconds" :integer :default 60 :min 1
                    :doc "how long teammate_wait blocks by default")
    ("max_live_runs" :integer :default 4 :min 1
                     :doc "how many teammate runs one session may have in flight"))
  (:settings (lambda (values table)
               (multiple-value-bind (mates problems)
                   (read-roster (ignore-errors (nlk:json-value table :any "teammates")))
                 (append values (list :teammates mates :problems problems)))))
  (:help :teammates
         "teammates: the operator's named roster, and the assign/check/wait/message/stop tools over it"
         +primer+)
  (:command "teammates" 'run-slash :catches 'teammates-error
            :description "The teammate roster and this session's runs; /teammates NAME for one")
  (:start (lambda ()
            (when (setting :problems)
              (nle:notice (format nil "teammates: ~{~a~^; ~}" (setting :problems))
                          :level :warning :key "nodecode-teammates")
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-teammates"))))
            (dolist (entry +tools+)
              (destructuring-bind (name schema handler) entry
                (nle:register-tool name (funcall schema) (coerce handler 'function))
                (nle:on-stop (lambda () (nle:unregister-tool name))))))))
