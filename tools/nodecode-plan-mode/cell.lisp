;;;; cell.lisp --- enforced plan mode: read-only planning, then the operator's approval.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of 100yenadmin/hermes-plan-mode (MIT), which spreads
;;;; the same mode over twelve Hermes hooks. Here it is four seams:
;;;;
;;;;   - one NLE:HOOK on the :TOOL point, which refuses a call that would
;;;;     change a file or run something while the mode is on -- the guard
;;;;     cell's shape, because Nodecode is a mono-tool organism whose EVAL form
;;;;     carries the writes and the commands alike, so a call is READ and not
;;;;     grepped;
;;;;   - one tool, plan_mode, which enters the mode, reports it, submits the
;;;;     plan for approval and leaves it;
;;;;   - one /plan command, for the operator's side of the same act;
;;;;   - one NLE:HOOK on NLE::READ-HARNESS-SECTIONS, which is how the note
;;;;     that tells the model what to do rides every request while the mode is
;;;;     on (the seam the context cell uses for its standing skill).
;;;;
;;;; Nothing leaves this machine. The plan is a Markdown file under
;;;; <cwd>/.nodecode/plans/ (or the section's plans_dir), and approval is the
;;;; operator's own question card -- the core's ASK-OPERATOR, answered or
;;;; timed out, never a network call.
;;;;
;;;; Config, a top-level key:
;;;;   "plan-mode": {"plans_dir": "", "ask_seconds": 180, "allow_commits": false}

(defpackage #:nodecode-plan-mode
  (:use #:cl))

(in-package #:nodecode-plan-mode)

(nlk:define-peripheral plan-mode :not-running t)

;;; --- what the mode reads, and what it lets through --------------------------

(defparameter +tool-name+ "plan_mode")

(defparameter +plan-writers+ '("write-file" "edit")
  "The operators whose own target may be a plan file under the plans directory.")

(defparameter +mutating-operators+
  '("write-file" "edit" "sh" "run-program" "launch-program" "run-shell-command"
    "delete-file" "rename-file" "copy-file" "ensure-directories-exist"
    "compile-file" "compile-system" "load" "load-system" "quickload")
  "Heads whose call changes a file or runs something, compared by name.")

(defparameter +read-only-tools+
  '("web_search" "websearch" "fetch" "search_files" "session_search"
    "browser_snapshot" "skills_list" "tool_search" "tool_describe" "vision_analyze")
  "Tool names plan mode lets run while planning. `eval' is read by its form instead.")

(defparameter +approve+ "Approve, implement")
(defparameter +keep-planning+ "Keep planning")
(defparameter +max-shown+ (* 64 1024)
  "How much of one plan file /plan show prints, and the approval card shows.")

(defparameter +manual+
  "Plan mode is enforced: while it is on, only read-only calls and writes under the session's plans directory run, so the agent reads, writes a plan, and changes nothing until the operator approves it. /plan <task> or /plan on <task> starts it; /plan status reports the phase and the plan files; /plan show [file] prints the plan; /plan approve takes it and the agent implements it on the next message; /plan reject <feedback> keeps planning with the note; /plan done ends an approved plan; /plan off leaves the mode without approving anything. The model enters the mode itself with the plan_mode tool (action on, status, submit, off) and submits the plan it wrote with action submit, which asks the operator on one card. A plan is a Markdown file under .nodecode/plans in the working directory unless the section's plans_dir names another.")

;;; --- the mode's state, one entry per session ---------------------------------

(defvar *sessions* (make-hash-table :test #'equal :synchronized t)
  "Session id -> the plan-mode state plist: :ACTIVE, :PHASE (:PLANNING,
:AWAITING, :EXECUTING), :REVISION, :DIR, :PLAN, :DIGEST, :SUMMARY,
:FEEDBACK, :BY. The empty-string key is the session a caller could not name.")

(defun session-key (&aux (turn (ignore-errors (nle:turn))))
  "The session this call belongs to, or \"\" when nothing names one."
  (or (and turn (getf turn :session-id)) ""))

(defun state-for (key)
  "KEY's state plist, or NIL. The unnamed session reads its own entry only,
and every entry answers a section read for it."
  (gethash (or key "") *sessions*))

(defun (setf state-for) (value key)
  (setf (gethash (or key "") *sessions*) value))

(defun current-state () (state-for (session-key)))

(defun live-state (&optional key)
  "KEY's (or the running one's) state when the mode is on, else NIL. A
session with no state of its own reads the unnamed one, which is where a
caller that could name no session filed it."
  (let ((state (if key (or (state-for key) (state-for "")) (current-state))))
    (and state (getf state :active) state)))

(defun blocking-state (&optional key)
  "The state while a change is refused: planning or awaiting approval."
  (let ((state (live-state key)))
    (and state (member (getf state :phase) '(:planning :awaiting)) state)))

;;; --- the plans directory ------------------------------------------------------

(defun plans-dir ()
  "The directory plans live in: the section's, or .nodecode/plans in the
working directory. A directory pathname."
  (let ((written (ignore-errors (setting :plans-dir))))
    (uiop:ensure-directory-pathname
     (uiop:merge-pathnames* (if (and written (plusp (length written)))
                                 written
                                 ".nodecode/plans/")
                            (uiop:getcwd)))))

(defun state-dir (state)
  (or (and state (getf state :dir)) (uiop:native-namestring (plans-dir))))

(defun dir-namestring (dir)
  (uiop:native-namestring (uiop:ensure-directory-pathname dir)))

(defun inside-plans-p (path dir)
  "Whether PATH, as the form spells it, lands inside DIR."
  (and path
       (stringp path)
       (plusp (length path))
       (uiop:string-prefix-p (dir-namestring dir)
                              (uiop:native-namestring
                               (uiop:merge-pathnames* path (uiop:getcwd))))))

(defun plan-files (&optional dir)
  "Every Markdown file in DIR (default: the plans directory), newest first."
  (let ((files (ignore-errors (uiop:directory-files (or dir (plans-dir)) "*.md"))))
    (sort (copy-list files) #'string>
          :key (lambda (file) (format nil "~10,'0d-~a" (or (ignore-errors (file-write-date file)) 0)
                                      (uiop:native-namestring file))))))

(defun plan-file (named state)
  "The plan file NAMED (a path or a file name), or STATE's, or the newest one.
=> a pathname, or NIL when there is none."
  (let* ((dir (if state (pathname (state-dir state)) (plans-dir)))
         (candidates (list (and named (plusp (length named))
                                (uiop:merge-pathnames* named (uiop:getcwd)))
                           (and named (plusp (length named))
                                (uiop:merge-pathnames* named dir))
                           (and state (getf state :plan) (pathname (getf state :plan)))
                           (first (plan-files dir)))))
    (find-if (lambda (candidate) (and candidate (probe-file candidate))) candidates)))

(defun plan-excerpt (file &optional (limit +max-shown+))
  "FILE's text, cut at LIMIT characters with a note when it is longer."
  (let ((text (or (ignore-errors (uiop:read-file-string file)) "")))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit)
                     (format nil "~%... (truncated at ~a characters)" limit))
        text)))

(defun digest (text)
  "A stable-enough fingerprint of one plan's text, for `changed since rev N'."
  (format nil "~a-~x" (length text) (sxhash text)))

(defun stamp ()
  "The timestamp a plan file's name carries; the model cannot read a clock."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0d_~2,'0d~2,'0d~2,'0d"
            year month day hour minute second)))

;;; --- reading an eval call, the way guard reads a command ---------------------

(defun operator-name (x)
  "X's symbol name, downcased, or NIL, so a form read in any package is read."
  (and (symbolp x) (string-downcase (symbol-name x))))

(defun literal-path (x strings)
  "The path X alone names, or NIL when the form hides it."
  (cond ((stringp x) x)
        ((and (consp x) (equal "$" (operator-name (car x))) (stringp (second x)) strings)
         (gethash (second x) strings))
        (t nil)))

(defun read-forms (text)
  "TEXT's forms, or NIL when it does not read."
  (handler-case
      (with-input-from-string (in text)
        (let ((*read-eval* nil)
              (*package* (find-package :nodecode-plan-mode))
              (forms '()))
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                do (push form forms))
          (nreverse forms)))
    (error () nil)))

(defun form-mutations (form strings &aux (found '()))
  "Every mutating call FORM makes, as (OP . TARGET), TARGET the path a write names."
  (labels ((walk (x)
             (cond ((consp x)
                    (let ((op (operator-name (car x))))
                      (when (member op +mutating-operators+ :test #'equal)
                        (push (cons op (literal-path (second x) strings)) found)))
                    (walk (car x))
                    (walk (cdr x)))
                   (t nil))))
    (walk form))
  (nreverse found))

(defun call-mutations (arguments)
  "The changes an EVAL call's own arguments show: its form, and the strings
object a heredoc reaches through. A form that will not read fails closed."
  (let* ((form (and (hash-table-p arguments) (gethash "form" arguments)))
         (strings (and (hash-table-p arguments) (gethash "strings" arguments)))
         (text (cond ((stringp form) form)
                     (form (format nil "~s" form))
                     (t ""))))
    (if (zerop (length text))
        nil
        (let ((forms (read-forms text)))
          (if (null forms)
              (list (cons "this call" nil))
              (loop for one in forms nconc (form-mutations one strings)))))))

(defun plan-write-p (mutation dir)
  "Whether MUTATION is one of the two writes plan mode still allows."
  (and (member (car mutation) +plan-writers+ :test #'equal)
       (inside-plans-p (cdr mutation) dir)))

(defun mutation-text (mutation dir)
  (let ((op (car mutation)) (target (cdr mutation)))
    (cond ((member op +plan-writers+ :test #'equal)
           (if target
               (format nil "~a writes ~a, outside ~a" op target (dir-namestring dir))
               (format nil "~a writes a file whose path the form does not show" op)))
          ((and (plusp (length op)) (alpha-char-p (char op 0)))
           (format nil "~a runs or changes something" op))
          (t (format nil "~a changes something" op)))))

(defun allowed-call-p (name arguments dir)
  "Whether a call of tool NAME with ARGUMENTS runs while planning.
=> (values ALLOWED WHY-NOT)."
  (cond ((equal name +tool-name+) (values t nil))
        ((member name +read-only-tools+ :test #'equal) (values t nil))
        ((not (equal name "eval"))
         (values nil (format nil "~a is not a read-only tool" name)))
        (t (let* ((mutations (call-mutations arguments))
                  (bad (find-if-not (lambda (one) (plan-write-p one dir)) mutations)))
             (if bad
                 (values nil (mutation-text bad dir))
                 (values t nil))))))

;;; --- what the model is told, in the harness section and in a refusal --------

(defun plan-style-note ()
  "The plan's own shape, the compact form of the original's lean profile."
  "Make it compact and decision-complete, so the operator can review it in a minute: Goal (one line); Decisions and assumptions (including every answer the operator gave); Changes (each file and what changes in it); Steps (numbered, each with how it is checked); Validation (the exact commands). Name exact files, functions and commands; include code only where a signature, format or pattern is itself the decision.")

(defun planning-note (state)
  "The note that rides every request while the mode is on."
  (let ((dir (state-dir state)))
    (format nil
            "Plan mode is ON (~a). Only read-only calls run, and files may be written only under ~a.~@[~%~%Plan this: ~a~]~%1. Explore with read-only calls first, and settle every fact the files can answer yourself. Before writing the plan, ask the operator about choices the files cannot settle: one short question with 2-3 choices, recommended first, and take the recommended one as an assumption when it goes unanswered.~%2. Write the plan as Markdown to ~a~a-<slug>.md (that timestamp is current; do not look up the time). ~a~:[~;~%~%No commits: do not plan or make one unless the operator asked.~]~%3. Show the complete plan in your reply, then call plan_mode(action=\"submit\") on its own to ask the operator to approve it.~%4. Do not implement before approval, and do not ask `should I proceed?' in prose: a rejection is review feedback, so revise the plan and submit a complete new revision.~@[~%~%The operator's note on the last revision: ~a~]"
            (phase-text state) dir
            (getf state :task) dir
            (stamp) (plan-style-note)
            (not (setting :allow-commits))
            (getf state :feedback))))

(defun execution-note (state)
  "The note once the operator has approved: implement it, in this turn."
  (format nil
          "The operator approved plan rev ~a (~a). Implement it now, in this turn: make each file's edits together, run its validation commands once, add or update the tests for the behaviour that changed, keep the existing test assertions unless the plan says otherwise, and reply in 2-4 lines with what changed and the test result.~@[ Do not commit: leave the changes for the operator to review.~]"
          (getf state :revision) (getf state :plan)
          (not (setting :allow-commits))))

(defun phase-text (state)
  (case (getf state :phase)
    (:planning (if (plusp (or (getf state :revision) 0))
                    (format nil "planning, rev ~a submitted and revised" (getf state :revision))
                    "planning"))
    (:awaiting (format nil "awaiting the operator's approval of rev ~a" (getf state :revision)))
    (:executing (format nil "executing rev ~a" (getf state :revision)))
    (t "off")))

(defun blocked-answer (why state)
  "The refusal a mutating call earns: the call's failure, and the way on."
  (nle:failure
   (format nil "ERROR: refused by nodecode-plan-mode: plan mode is on (~a), so ~a. Only read-only calls and writes under ~a run until the operator approves the plan: read what you need, put the commands and tests in the plan, write the plan there, then call plan_mode(action=\"submit\"). Do not retry this call or work around it.~@[~%The operator's note on the last revision: ~a~]"
           (phase-text state) why (state-dir state) (getf state :feedback))))

(defun clear-feedback (state)
  (setf (getf state :feedback) nil)
  state)

(defun note-feedback (state feedback)
  (setf (getf state :feedback) (and feedback (plusp (length feedback)) feedback))
  state)

;;; --- the two hooks -----------------------------------------------------------

(defun guard-call (op next)
  "The :TOOL point: while planning, a call that changes something is refused
with the call's failure, and NEXT never runs."
  (let ((state (blocking-state)))
    (if (null state)
        (funcall next op)
        (multiple-value-bind (allowed why)
            (allowed-call-p (getf op :name) (getf op :arguments) (state-dir state))
          (if allowed
              (funcall next op)
              (blocked-answer why state))))))

(defun plan-section (next session-id)
  "The standing harness section: the note that tells the model what to do,
while the mode is on for SESSION-ID. A section of this name replaces the
next reader's own, and is taken back when the mode ends."
  (let* ((state (live-state session-id))
         (text (cond ((null state) nil)
                     ((eq (getf state :phase) :executing) (execution-note state))
                     (t (planning-note state)))))
    (if text
        (acons "plan-mode" text
               (remove "plan-mode" (funcall next session-id) :key #'car :test #'equal))
        (funcall next session-id))))

;;; --- the mode's own verbs ----------------------------------------------------

(defun fresh-state ()
  "One session's state, every key present: a SETF on a member mutates the
plist the table holds, rather than a copy built to carry a key that was absent."
  (list :active nil :phase nil :revision 0 :dir nil :plan nil :digest nil
        :summary nil :feedback nil :task nil :by nil))

(defun enter (by task)
  "Turn the mode on for this session and answer the note it just installed."
  (let* ((key (session-key))
         (state (or (state-for key) (fresh-state))))
    (setf (getf state :active) t
          (getf state :phase) :planning
          (getf state :dir) (uiop:native-namestring (plans-dir))
          (getf state :by) by
          (getf state :task) (and task (plusp (length task)) task))
    (clear-feedback state)
    (setf (state-for key) state)
    (planning-note state)))

(defun leave (by)
  "Turn the mode off. The agent may leave before it submits; past that only
the operator ends it."
  (let ((state (current-state)))
    (cond ((null (live-state))
           "plan mode is not on.")
          ((and (eq by :agent) (member (getf state :phase) '(:awaiting :executing)))
           (format nil "plan mode stays on: the plan is ~a, so only the operator can end it (/plan off, or /plan done once it is implemented)."
                   (phase-text state)))
          (t (setf (getf state :active) nil
                   (getf state :phase) nil
                   (getf state :task) nil)
             (setf (state-for (session-key)) state)
             (format nil "plan mode is off (~a). Nothing was approved; everything may change again.~@[ The plan file stays at ~a.~]"
                     (if (eq by :agent) "you left it" "the operator left it")
                     (getf state :plan))))))

(defun status-text ()
  (let ((state (current-state)))
    (if (null (live-state))
        (format nil "plan mode: off. Plans directory: ~a~@[~%;the last plan: ~a~]."
                (dir-namestring (plans-dir)) (and state (getf state :plan)))
        (format nil "plan mode: ~a~%;plans directory: ~a~@[~%;the plan: ~a~]~@[~%;plan files: ~{~a~^, ~}~]~@[~%;the operator's note: ~a~]."
                (phase-text state) (state-dir state) (getf state :plan)
                (mapcar #'file-namestring (plan-files (pathname (state-dir state))))
                (getf state :feedback)))))

(defun show (named)
  (let* ((state (current-state))
         (file (plan-file named state)))
    (if (null file)
        (format nil "no plan file yet: write the plan as Markdown under ~a."
                (state-dir state))
        (format nil "~a~%~%~a" (uiop:native-namestring file) (plan-excerpt file)))))

(defun edit-path (named)
  (let* ((state (current-state))
         (file (plan-file named state)))
    (if (null file)
        (format nil "no plan file yet: write the plan as Markdown under ~a."
                (state-dir state))
        (format nil "Edit ~a and save it, then /plan show to read it back and /plan approve. Plan mode stays on until you approve or /plan off.~@[ The operator's note waits for the next revision: ~a~]"
                (uiop:native-namestring file) (and state (getf state :feedback))))))

(defun approve (named summary)
  "The operator's approval, typed rather than answered on a card."
  (let ((state (current-state)))
    (cond ((null (live-state))
           "plan mode is off: nothing to approve. /plan on <task> starts it.")
          ((eq (getf state :phase) :executing)
           (format nil "plan rev ~a is already approved; the agent implements it at ~a."
                   (getf state :revision) (getf state :plan)))
          (t (approve-file state named summary)))))

(defun approve-file (state named summary)
  "Take the plan NAMED (or STATE's, or the newest) as the one to implement."
  (let ((file (plan-file named state)))
    (if (null file)
        (format nil "no plan file to approve yet: the agent writes it under ~a." (state-dir state))
        (progn
          (setf (getf state :active) t
                (getf state :phase) :executing
                (getf state :revision) (max 1 (or (getf state :revision) 0))
                (getf state :plan) (uiop:native-namestring file)
                (getf state :digest) (digest (plan-excerpt file +max-shown+))
                (getf state :summary) (and summary (plusp (length summary)) summary))
          (clear-feedback state)
          (setf (state-for (session-key)) state)
          (nle:notice (format nil "plan-mode: plan rev ~a approved" (getf state :revision))
                      :key "nodecode-plan-mode")
          (format nil "Approved rev ~a (~a). Send the agent a message to start the implementation; /plan done ends it once it is implemented.~%~%~a"
                  (getf state :revision) (getf state :plan)
                  (execution-note state))))))

(defun reject (feedback)
  (let ((state (current-state)))
    (if (null (live-state))
        "plan mode is off: nothing to reject. /plan on <task> starts it."
        (progn
          (setf (getf state :phase) :planning
                (getf state :active) t)
          (note-feedback state feedback)
          (setf (state-for (session-key)) state)
          (format nil "Kept planning~@[ with the note: ~a~]. The agent revises the plan and submits a complete new revision."
                  (getf state :feedback))))))

(defun done ()
  (let ((state (current-state)))
    (if (null (live-state))
        "plan mode is off already."
        (progn
          (setf (getf state :active) nil (getf state :phase) nil (getf state :task) nil)
          (setf (state-for (session-key)) state)
          (nle:notice nil :key "nodecode-plan-mode")
          (format nil "plan mode is off; the approved plan's implementation is on its own now.~@[ The plan file stays at ~a.~]"
                  (getf state :plan))))))

;;; --- the agent's side: one tool ----------------------------------------------

(nlk:define-contract plan-action
  (:doc "Enforced plan mode, which the operator approves. action='on' enters it: only read-only calls and writes under the plans directory run until the plan is approved. Explore, write the plan as Markdown under the plans directory, show it in your reply, then call action='submit' on its own to ask the operator to approve it; approval starts implementation in this turn. action='status' reports the phase and plan files; action='off' leaves a mode you entered before you submitted anything.")
  ("action" :text :required t :options '("on" "status" "off" "submit")
            :doc "on enters plan mode, status reports it, submit asks for approval, off leaves it")
  ("reason" :text :default ""
            :doc "what the plan is for, one line; shown to the operator")
  ("path"   :text :default ""
            :doc "absolute path or file name of the plan to submit; default: the newest plan file")
  ("summary" :text :default ""
             :doc "optional one-line title for the approval card"))

(defun ask-operator-function ()
  "The core's ASK-OPERATOR, or NIL in a core that has none."
  (let ((symbol (find-symbol "ASK-OPERATOR" '#:nodecode.evolved)))
    (and symbol (fboundp symbol) (symbol-function symbol))))

(defun ask-approval (state)
  "Ask the operator to approve STATE's plan.
=> :APPROVED, :KEPT, or (values NIL WHY) when nobody answered."
  (let ((ask (ask-operator-function)))
    (if (null ask)
        (values nil :off)
        (multiple-value-bind (answer why)
            (funcall ask
                     (format nil "Plan rev ~a is ready to approve~@[ - ~a~]"
                             (getf state :revision) (getf state :summary))
                     :detail (plan-excerpt (getf state :plan) +max-shown+)
                     :choices (list +approve+ +keep-planning+)
                     :seconds (max 5 (or (setting :ask-seconds) 180)))
          (cond ((equal answer +approve+) :approved)
                ((equal answer +keep-planning+) :kept)
                (t (values nil (or why :unanswered))))))))

(defun submit (named summary)
  "Ask the operator to approve the plan, and answer the model with what next."
  (let ((state (current-state)))
    (cond
      ((null (live-state))
       "plan mode is off: call plan_mode(action=\"on\"), explore, write the plan, then submit it.")
      (t (submit-file state named summary)))))

(defun submit-file (state named summary)
  "File the new revision and ask the operator to approve it."
  (let ((file (plan-file named state)))
    (if (null file)
        (format nil "No plan file yet: write the plan as Markdown under ~a, show it in your reply, then call plan_mode(action=\"submit\")."
                (state-dir state))
        (progn
          (setf (getf state :active) t
                (getf state :phase) :awaiting
                (getf state :revision) (1+ (or (getf state :revision) 0))
                (getf state :plan) (uiop:native-namestring file)
                (getf state :digest) (digest (plan-excerpt file +max-shown+))
                (getf state :summary) (and summary (plusp (length summary)) summary))
          (clear-feedback state)
          (setf (state-for (session-key)) state)
          (let ((answer (ask-approval state)))
            (cond
              ((eq answer :approved)
               (setf (getf state :phase) :executing
                     (getf state :by) :operator)
               (setf (state-for (session-key)) state)
               (nle:notice (format nil "plan-mode: plan rev ~a approved" (getf state :revision))
                           :key "nodecode-plan-mode")
               (format nil "Approved. ~a" (execution-note state)))
              ((eq answer :kept)
               (setf (getf state :phase) :planning)
               (setf (state-for (session-key)) state)
               "The operator chose to keep planning: revise the plan and submit a complete new revision. Writing the plan file is still allowed; nothing else is.")
              (t
               (setf (state-for (session-key)) state)
               (format nil "Plan rev ~a is submitted and waiting for the operator's approval: ~a. They approve with /plan approve, or ask for changes with /plan reject <feedback>; changes are refused until one of those.~@[~%The note they will read: ~a~]"
                       (getf state :revision) (getf state :plan)
                       (getf state :summary)))))))))

(defun plan-mode-tool (action)
  "The plan_mode tool: the agent's side of the mode."
  (let ((verb (string-downcase (plan-action-action action))))
    (cond ((equal verb "on") (enter :agent (plan-action-reason action)))
          ((equal verb "status") (status-text))
          ((equal verb "off") (leave :agent))
          ((equal verb "submit") (submit (plan-action-path action) (plan-action-summary action)))
          (t (format nil "Unknown plan_mode action ~s: use on, status, off or submit." verb)))))

;;; --- the operator's side: one command -----------------------------------------

(defun run-plan (args session-id)
  "The /plan command: the operator's side of the mode."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (head (string-downcase (if space (subseq text 0 space) text)))
         (rest (nlk:trimmed (if space (subseq text (1+ space)) ""))))
    (cond ((equal head "") (status-text))
          ((equal head "on") (enter :operator (and (plusp (length rest)) rest)))
          ((equal head "off") (leave :operator))
          ((equal head "status") (status-text))
          ((equal head "show") (show rest))
          ((equal head "edit") (edit-path rest))
          ((equal head "approve") (approve rest ""))
          ((equal head "reject") (reject rest))
          ((equal head "done") (done))
          (t (leave :operator)
             (enter :operator text)))))

;;; --- the cell ---------------------------------------------------------------

(nle:define-cell plan-mode
  (:section ("plan-mode")
    (:guide "nothing here is required: plans go to .nodecode/plans in the working directory unless plans_dir names another directory; the agent writes the plan there, submits it, and nothing else changes until you approve it in /plan or on the approval card")
    ("plans_dir" :string :default ""
                 :doc "where plan files go; empty is .nodecode/plans in the working directory")
    ("ask_seconds" :integer :default 180 :min 5
                   :doc "how long a submitted plan waits for your answer before it stands waiting")
    ("allow_commits" :boolean :default nil
                      :doc "off tells the agent not to plan or make a git commit, so the changes stay uncommitted for your review"))
  (:tool +tool-name+ (nlk:contract 'plan-action) 'plan-mode-tool)
  (:command "plan" 'run-plan
            :description "Plan first: the agent reads and plans, and edits nothing until you approve"
            :argument-hint "on <task> | status | show | approve | reject <note> | done | off")
  (:help :plan-mode
         "plan-mode: plan first, edit after approval - /plan on <task>, /plan status, /plan show, /plan approve, /plan reject <note>"
         +manual+)
  (:hook :tool #'guard-call)
  (:hook 'nle::read-harness-sections #'plan-section)
  (:start (lambda ()
            (clrhash *sessions*)
            (ignore-errors (ensure-directories-exist (plans-dir)))
            (nle:on-stop (lambda ()
                           (clrhash *sessions*)
                           (nle:notice nil :key "nodecode-plan-mode"))))))
