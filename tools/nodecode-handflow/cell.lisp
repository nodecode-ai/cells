;;;; cell.lisp --- the plan store, the `handflow' tool, /handflow.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A plan is an ordered list of steps, each carrying a status
;;;; (not_started / in_progress / completed / blocked) and its notes, which
;;;; the agent rewrites while it works; the plan comes back in the same call
;;;; that changed it, so the model never spends a turn asking where it is.
;;;; One JSON file per plan under the home, one pointer to the active plan,
;;;; and every write atomic (a temp file and a rename).
;;;;
;;;; Rewritten from DECRUX9812/hermes-handflow, whose dockable desktop pane
;;;; this cell replaces with a harness section: the active plan rides every
;;;; request, so the plan is as visible to the model as the pane made it to
;;;; the person, and /handflow shows it to the person.

(in-package #:nodecode-handflow)

;;; The condition every refusal is, FAIL, and (with NOT-RUNNING) the settings
;;; variable, the running check and DEFINE-VERB the verbs below are written with.

(nlk:define-peripheral handflow :not-running t)

;;; --- the words -----------------------------------------------------------------

(defparameter *primer*
  "handflow keeps the plan for a multi-step task where both you and the model can
see it: an ordered list of steps, each not_started, in_progress, completed or
blocked, with a note on what each produced. The model writes it before it
works and marks it as it goes, through the handflow tool; the active plan is
carried in every request, so it also reads the plan without asking.
  (handflow:plan-text)    the active plan, as the model reads it
  (handflow:plans-text)   the recent plans, newest first
/handflow shows the active plan, /handflow list the recent plans. State is
plain JSON under the Nodecode home, one file per plan; nothing here calls a
model or a network."
  "What (help :handflow) answers while the cell runs.")

(defparameter +actions+
  '("start" "plan" "begin" "complete" "block" "reopen" "note" "add" "finish" "abort" "list" "use")
  "The tool's whole vocabulary, in the order its schema lists them.")

(defparameter +statuses+ '("not_started" "in_progress" "completed" "blocked")
  "A step's four states, as the files keep them.")

(defparameter +marks+
  '(("completed" . "[x]") ("in_progress" . "[>]") ("blocked" . "[!]") ("not_started" . "[ ]"))
  "One line's mark a status, the way the plan reads back.")

(defparameter +verbs+
  '(("in_progress" . "begin") ("completed" . "complete") ("blocked" . "block") ("not_started" . "reopen"))
  "Status to the act that produced it, so the event trail speaks the tool's
vocabulary (complete, not completed).")

(defparameter +closing-acts+
  '(("begin" . "in_progress") ("complete" . "completed") ("block" . "blocked") ("reopen" . "not_started"))
  "The four acts that move a step, and the status each moves it to.")

(defparameter *max-events* 200
  "How many events one plan's trail keeps, newest last.")

(defvar *last-error* nil
  "The last refusal the tool answered, so /handflow can show what went wrong.")

;;; --- where a plan is written ------------------------------------------------------

(defun now ()
  "The local time, second resolution, as the files keep it."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun state-root ()
  "The state directory: the section's state_dir, or handflow/ under the home."
  (let ((dir (setting :state-dir)))
    (if (and (stringp dir) (plusp (length dir)))
        (uiop:ensure-directory-pathname (nlk:expand-home dir))
        (nlk:home "handflow/"))))

(defun plans-dir () (merge-pathnames "plans/" (state-root)))
(defun active-path () (merge-pathnames "active.json" (state-root)))

(defun plan-file (plan-id)
  "Where the plan PLAN-ID is written: its id, kept to what a filename may hold."
  (let* ((id (princ-to-string plan-id))
         (safe (remove-if-not (lambda (c) (or (alphanumericp c) (find c "-_"))) id)))
    (when (zerop (length safe)) (fail "~s is no plan id" id))
    (merge-pathnames (format nil "~a.json" safe) (plans-dir))))

;;; --- reading and writing one file ------------------------------------------------

(defun write-json-file (path object)
  "OBJECT at PATH as JSON, atomically: a temp file beside it, then a rename."
  (ensure-directories-exist path)
  (let ((tmp (make-pathname :name (format nil "~a.tmp" (pathname-name path))
                             :type (pathname-type path)
                             :defaults path)))
    (with-open-file (out tmp :direction :output :if-exists :supersede
                              :if-does-not-exist :create :external-format :utf-8)
      (write-string (nlk:pretty-json object) out))
    (uiop:rename-file-overwriting-target tmp path))
  path)

(defun read-json-file (path)
  "PATH as a JSON object, or NIL when it is absent or unreadable."
  (when (probe-file path)
    (handler-case (nlk:decode-json (nlk:read-text path) :whole t)
      (error () nil))))

;;; --- the plan's own readers ------------------------------------------------------

(defun jstr (object key)
  "One string member of OBJECT, or NIL."
  (nlk:json-value object :string key))

(defun jsteps (plan)
  "PLAN's steps, a vector, empty when there are none."
  (nlk:json-array plan "steps"))

(defun jnotes (step)
  "STEP's notes, a vector, empty when there are none."
  (nlk:json-array step "notes"))

(defun clip (text limit)
  (let ((text (princ-to-string text)))
    (if (> (length text) limit) (subseq text 0 limit) text)))

(defun add-note (step text)
  "Append TEXT to STEP's notes."
  (setf (gethash "notes" step) (concatenate 'vector (jnotes step) (vector (princ-to-string text))))
  step)

(defun log-event (plan kind &optional (detail ""))
  "Append one event to PLAN's trail, keeping the newest MAX-EVENTS."
  (let* ((events (nlk:json-array plan "events"))
         (all (concatenate 'vector events
                            (vector (nlk:make-json-object "ts" (now) "kind" kind
                                                           "detail" (princ-to-string detail))))))
    (setf (gethash "events" plan)
          (if (> (length all) *max-events*)
              (subseq all (- (length all) *max-events*))
              all)))
  plan)

(defun counts (plan)
  "PLAN's tally => the completed, the total, the blocked, the in progress."
  (let ((done 0) (total 0) (blocked 0) (active 0))
    (loop for step across (jsteps plan)
          do (incf total)
             (let ((status (or (jstr step "status") "not_started")))
               (cond ((string= status "completed") (incf done))
                     ((string= status "blocked") (incf blocked))
                     ((string= status "in_progress") (incf active)))))
    (values done total blocked active)))

(defun open-work (plan)
  "PLAN's steps still to do, in order."
  (remove-if-not (lambda (step)
                   (member (or (jstr step "status") "") '("not_started" "in_progress")
                           :test #'string=))
                 (coerce (jsteps plan) 'list)))

;;; --- building and moving a plan ---------------------------------------------------

(defun random-tag ()
  (subseq (nlk:sha256-text (format nil "~a/~a/~a" (get-universal-time)
                                    (get-internal-real-time) (random 1000000)))
          0 4))

(defun new-plan-id ()
  (format nil "plan_~d_~a" (get-universal-time) (random-tag)))

(defun make-step (title position)
  (nlk:make-json-object "id" (format nil "s~2,'0d" position)
                         "title" title
                         "status" "not_started"
                         "notes" (vector)
                         "executor" ""
                         "created_at" (now)
                         "closed_at" ""))

(defun relabel (plan)
  "Renumber step ids to match position, so a plan with an insert still reads s01,
 s02, ... instead of ids that skip or collide."
  (loop for step across (jsteps plan)
        for i from 1
        do (setf (gethash "id" step) (format nil "s~2,'0d" i)))
  plan)

(defun new-plan (goal step-titles)
  "A plan whose first step is already in progress."
  (let ((plan (nlk:make-json-object "id" (new-plan-id) "goal" goal "status" "active"
                                     "executor" "" "steps" (vector) "events" (vector)
                                     "created_at" (now) "updated_at" (now) "closed_at" "")))
    (setf (gethash "steps" plan)
          (coerce (loop for title in step-titles
                         for i from 1
                         collect (make-step title i))
                  'vector))
    (relabel plan)
    (let ((first-step (aref (jsteps plan) 0)))
      (setf (gethash "status" first-step) "in_progress")
      (add-note first-step "started"))
    (log-event plan "start" (format nil "~d steps" (length (jsteps plan))))
    plan))

(defun active-id ()
  (or (nlk:json-value (read-json-file (active-path)) :string "plan_id") ""))

(defun set-active (plan-id)
  (write-json-file (active-path) (nlk:make-json-object "plan_id" plan-id "at" (now)))
  plan-id)

(defun save-plan (plan)
  (setf (gethash "updated_at" plan) (now))
  (write-json-file (plan-file (gethash "id" plan)) plan)
  plan)

(defun load-plan (plan-id)
  (let ((object (and plan-id (plusp (length (princ-to-string plan-id)))
                      (read-json-file (plan-file plan-id)))))
    (and (hash-table-p object) object)))

(defun recent-plans (&key (limit 10))
  "Plans newest-written first; a corrupt or half-written file is skipped."
  (let ((rows '()))
    (when (uiop:directory-exists-p (plans-dir))
      (dolist (path (uiop:directory-files (plans-dir) "*.json"))
        (let ((plan (read-json-file path)))
          (when (and (hash-table-p plan) (jstr plan "id"))
            (push (cons (or (ignore-errors (file-write-date path)) 0) plan) rows)))))
    (let* ((sorted (sort rows #'> :key #'car))
           (kept (if (> (length sorted) (max 1 limit)) (subseq sorted 0 (max 1 limit)) sorted)))
      (mapcar #'cdr kept))))

(defun active-plan ()
  "The pinned plan, or the most recently written one when nothing is pinned."
  (or (let ((id (active-id)))
        (and (plusp (length id)) (load-plan id)))
      (first (recent-plans :limit 1))))

(defun find-step (plan ref)
  "Resolve a step by 1-based position, by id, by title substring, or the
current one when REF is empty."
  (let* ((steps (jsteps plan))
         (text (and ref (princ-to-string ref))))
    (cond
      ((or (null text) (zerop (length text))
           (string-equal text "current") (string-equal text "next"))
       (or (find-if (lambda (step) (string= (or (jstr step "status") "") "in_progress")) steps)
           (find-if (lambda (step) (string= (or (jstr step "status") "") "not_started")) steps)))
      ((every #'digit-char-p text)
       (let ((index (1- (parse-integer text))))
         (and (<= 0 index) (< index (length steps)) (aref steps index))))
      (t (or (find-if (lambda (step) (string= (jstr step "id") text)) steps)
             (let ((needle (string-downcase text)))
               (find-if (lambda (step)
                          (search needle (string-downcase (or (jstr step "title") ""))))
                        steps)))))))

(defun auto-start-next (plan step)
  "A step marked in progress closes any other step left in progress."
  (loop for other across (jsteps plan)
        when (and (not (eq other step))
                  (string= (or (jstr other "status") "") "in_progress"))
          do (setf (gethash "status" other) "completed")
             (setf (gethash "closed_at" other) (now))
             (add-note other (format nil "auto-completed when ~a started" (jstr step "id")))
             (log-event plan "auto_complete" (jstr other "id"))))

(defun mark-step (plan ref status &optional (note ""))
  "Set a step's STATUS, with an optional note; refuses an unknown step or status."
  (unless (member status +statuses+ :test #'string=)
    (fail "status must be one of ~{~a~^, ~}" +statuses+))
  (let ((step (find-step plan ref)))
    (unless step (fail "no step matches ~s" ref))
    (setf (gethash "status" step) status)
    (when (member status '("completed" "blocked") :test #'string=)
      (setf (gethash "closed_at" step) (now)))
    (when (string= status "in_progress")
      (setf (gethash "closed_at" step) ""))
    (when (plusp (length note)) (add-note step note))
    (when (string= status "in_progress") (auto-start-next plan step))
    (log-event plan (or (cdr (assoc status +verbs+ :test #'string=)) status)
               (format nil "~a ~a" (jstr step "id") (clip (jstr step "title") 60)))
    step))

(defun add-step (plan title position)
  "Insert a step (replanning); POSITION is 1-based, absent appends."
  (let* ((steps (jsteps plan))
         (step (make-step title (1+ (length steps))))
         (index (if (and (integerp position) (plusp position))
                    (max 0 (min (length steps) (1- position)))
                    (length steps))))
    (setf (gethash "steps" plan)
          (concatenate 'vector (subseq steps 0 index) (vector step) (subseq steps index)))
    (relabel plan)
    (log-event plan "add_step" (clip title 60))
    (aref (jsteps plan) index)))

(defun reopen (plan)
  "Un-close a finished plan: a new step, or a step moving back to not_started."
  (unless (string= (or (jstr plan "status") "") "active")
    (setf (gethash "status" plan) "active")
    (setf (gethash "closed_at" plan) ""))
  plan)

(defun close-if-done (plan)
  "Every step complete closes the plan itself; add and reopen undo it."
  (multiple-value-bind (done total) (counts plan)
    (when (and (string= (or (jstr plan "status") "") "active") (plusp total) (= done total))
      (setf (gethash "status" plan) "done")
      (setf (gethash "closed_at" plan) (now))
      (log-event plan "auto_finish" "all steps complete")
      t)))

;;; --- the plan as text ------------------------------------------------------------

(defun mark-for (status)
  (or (cdr (assoc status +marks+ :test #'string=)) "[ ]"))

(defun render (plan)
  "PLAN as text, one line per step: what the model reads back and what
/handflow shows."
  (let ((lines (list (format nil "plan ~a  (~a)" (or (jstr plan "id") "?")
                              (or (jstr plan "status") "?"))
                      (format nil "goal: ~a" (or (jstr plan "goal") "")))))
    (loop for step across (jsteps plan)
          for i from 1
          do (push (format nil "~d. ~a ~a" i (mark-for (jstr step "status"))
                            (or (jstr step "title") ""))
                   lines)
             (loop for note across (jnotes step)
                   do (push (format nil "   note: ~a" (princ-to-string note)) lines)))
    (multiple-value-bind (done total blocked) (counts plan)
      (push (format nil "~d/~d complete, ~d blocked" done total blocked) lines))
    (format nil "~{~a~%~}" (nreverse lines))))

(defun answer (action plan &key extra)
  "One tool's answer: the act, the tally, and the plan as it now stands."
  (multiple-value-bind (done total blocked) (counts plan)
    (format nil "handflow ~a: ~d/~d complete, ~d blocked~@[~%~a~]~%~a"
            action done total blocked (and extra (plusp (length extra)) extra) (render plan))))

(defun list-text ()
  "The recent plans, newest first, the active one marked."
  (let ((plans (recent-plans :limit 10)))
    (if (null plans)
        "handflow: no plans yet -- call the handflow tool with action \"start\", a goal and the step titles."
        (format nil "handflow list: ~d plan~:p, newest first~%~{~a~}"
                (length plans)
                (loop for plan in plans
                      for active = (string= (or (jstr plan "id") "") (active-id))
                      collect (multiple-value-bind (done total) (counts plan)
                                (format nil "  ~a  ~a/~a  ~a~@[  (active)~]~%"
                                        (or (jstr plan "id") "?")
                                        done total (or (jstr plan "status") "?")
                                        active)))))))

;;; --- the tool -------------------------------------------------------------------

(nlk:define-contract handflow-call
  (:doc "Keep a visible plan for a multi-step task. Call action 'start' with the goal and the step list BEFORE you begin work, then 'begin', 'complete' or 'block' each step as you go, with a short note of what it produced. Every call returns the rendered plan, and the active plan also rides your context; 'add' replans mid-flight and 'finish' closes it. State is local JSON: no network, no model call.")
  ("action" :string :required t :options +actions+ :doc "what to do")
  ("goal" :string :doc "start: the outcome the user asked for")
  ("steps" :array :of :string :doc "start: ordered step titles (3-8 is the sweet spot)")
  ("step" :string :slot step-ref :doc "which step: 1-based number, id, title substring, or omit for the current one")
  ("note" :string :doc "short note: what the step produced, or why it is blocked")
  ("title" :string :doc "add: title of the new step")
  ("position" :integer :slot insert-at :doc "add: insert at this 1-based position; omit to append")
  ("text" :string :doc "note: text to append to a step's notes")
  ("summary" :string :doc "finish/abort: one line on the outcome")
  ("force" :boolean :doc "finish: close even while steps are still open")
  ("plan_id" :string :doc "use: the plan to make active")
  ("limit" :integer :doc "list: how many plans to show"))

(defun trimmed (value)
  (nlk:trimmed (princ-to-string (or value ""))))

(defun dispatch-start (call)
  (let* ((goal (trimmed (handflow-call-goal call)))
         (titles (remove-if (lambda (title) (zerop (length title)))
                            (map 'list #'trimmed (or (handflow-call-steps call) #())))))
    (when (zerop (length goal)) (fail "start needs a goal: the outcome the user asked for"))
    (when (null titles) (fail "start needs steps: an array of step titles for the plan"))
    (let ((plan (new-plan goal titles)))
      (save-plan plan)
      (set-active (gethash "id" plan))
      (answer "start" plan
              :extra (format nil "~d step~:p; step ~a is in progress"
                              (length titles) (jstr (aref (jsteps plan) 0) "id"))))))

(defun dispatch-use (call)
  (let* ((plan-id (trimmed (handflow-call-plan-id call)))
         (plan (and (plusp (length plan-id)) (load-plan plan-id))))
    (unless plan (fail "no plan ~s" plan-id))
    (set-active plan-id)
    (answer "use" plan :extra (format nil "plan ~a is the active one" plan-id))))

(defun dispatch-mark (act plan call)
  (let* ((status (cdr (assoc act +closing-acts+ :test #'string=)))
         (step (mark-step plan (handflow-call-step-ref call) status
                           (trimmed (handflow-call-note call)))))
    (close-if-done plan)
    (when (string= status "not_started") (reopen plan))
    (save-plan plan)
    (answer act plan :extra (format nil "step ~a: ~a" (jstr step "id") (jstr step "title")))))

(defun dispatch-note (plan call)
  (let ((text (trimmed (or (handflow-call-text call) (handflow-call-note call)))))
    (when (zerop (length text)) (fail "note needs text: what to record on the step"))
    (let ((step (find-step plan (handflow-call-step-ref call))))
      (unless step (fail "no step to attach the note to"))
      (add-note step text)
      (log-event plan "note" (clip text 60))
      (save-plan plan)
      (answer "note" plan :extra (format nil "step ~a" (jstr step "id"))))))

(defun dispatch-add (plan call)
  (let ((title (trimmed (handflow-call-title call))))
    (when (zerop (length title)) (fail "add needs a title: the step to insert"))
    (let ((step (add-step plan title (handflow-call-insert-at call))))
      (reopen plan)
      (save-plan plan)
      (answer "add" plan :extra (format nil "step ~a: ~a" (jstr step "id") title)))))

(defun dispatch-close (act plan call)
  (let ((summary (trimmed (or (handflow-call-summary call) (handflow-call-note call)))))
    (when (and (string= act "finish") (not (handflow-call-force call)))
      (let ((open (open-work plan)))
        (when open
          (fail "~d step~:p still open (~{~a~^; ~}) -- close them, or pass force=true to finish anyway"
                (length open)
                (loop for step in (subseq open 0 (min 4 (length open)))
                      collect (or (jstr step "title") "?"))))))
    (setf (gethash "status" plan) (if (string= act "finish") "done" "aborted"))
    (setf (gethash "closed_at" plan) (now))
    (when (plusp (length summary)) (log-event plan act summary))
    (save-plan plan)
    (answer act plan :extra summary)))

(defun dispatch (action call)
  (let ((act (string-downcase (princ-to-string action))))
    (cond
      ((string= act "start") (dispatch-start call))
      ((string= act "list") (list-text))
      ((string= act "use") (dispatch-use call))
      (t (let ((plan (active-plan)))
           (unless plan
             (fail "no active plan -- call the handflow tool with action \"start\", a goal and the step titles first"))
           (cond
             ((string= act "plan") (answer "plan" plan))
             ((assoc act +closing-acts+ :test #'string=) (dispatch-mark act plan call))
             ((string= act "note") (dispatch-note plan call))
             ((string= act "add") (dispatch-add plan call))
             (t (dispatch-close act plan call))))))))

(defun handflow-tool (call)
  "The handflow tool: one call, one plan, twelve actions. A refusal becomes
the tool answer ERROR: ..., so the model reads what went wrong and the
transcript draws the call failed."
  (handler-case
      (progn
        (setf *last-error* nil)
        (dispatch (handflow-call-action call) call))
    (handflow-error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: ~a" condition)))
    (error (condition)
      (setf *last-error* (format nil "~a" condition))
      (nle:failure (format nil "ERROR: HANDFLOW-ERROR: ~a" condition)))))

;;; --- /handflow, and the plan in every request ------------------------------------

(defun run-slash (args session-id)
  "/handflow: the plan the agent is working through; /handflow list, the recent
plans. One-line toggles belong to the tool -- a person reads the plan here."
  (declare (ignore session-id))
  (let ((argument (trimmed args)))
    (cond
      ((string-equal argument "list") (list-text))
      ((string-equal argument "help") *primer*)
      (t (let ((plan (active-plan)))
           (if plan
               (format nil "~a~@[~%last refusal: ~a~]" (render plan) *last-error*)
               "handflow: no plan yet -- ask for a task with a few steps and the agent plans before it works."))))))

(defun plan-brief (plan)
  (multiple-value-bind (done total) (counts plan)
    (format nil "handflow: the plan for the work in hand (~d/~d complete). Write it before you work with the handflow tool; mark each step as you go, with a note of what it produced.~%~a"
            done total (render plan))))

(defun plan-section (next session-id)
  "Every request's harness sections, with the active plan first: the pane the
original plugin drew on the desktop, in the one place a model always reads."
  (let ((sections (remove "handflow" (funcall next session-id) :key #'car :test #'equal)))
    (if (and (setting :show-plan) (not (eq nil (active-plan))))
        (let ((plan (active-plan)))
          (if plan (acons "handflow" (plan-brief plan) sections) sections))
        sections)))

;;; --- the verbs an eval calls -----------------------------------------------------

(define-verb plan-text ()
  "The active plan, one line per step, as the model reads it -- or a line
saying there is none yet."
  (let ((plan (active-plan)))
    (if plan (render plan) "handflow: no plan yet.")))

(define-verb plans-text ()
  "The recent plans, newest first, the active one marked."
  (list-text))

;;; --- the declaration ------------------------------------------------------------

(nle:define-cell handflow
  (:section ("handflow")
    (:guide "nothing is required: an empty state_dir keeps plans under the Nodecode home; show_plan off keeps the plan out of every request")
    ("state_dir" :string :default ""
                 :doc "where plan JSON lives; empty means handflow/ in the Nodecode home")
    ("show_plan" :boolean :default t
                 :doc "carry the active plan in every request, as the original's desktop pane showed it"))
  (:tool "handflow" (nlk:contract 'handflow-call) 'handflow-tool)
  (:command "handflow" 'run-slash
            :description "The plan the agent is working through"
            :argument-hint "LIST")
  (:help :handflow "handflow: a step plan the agent writes and marks as it works - (handflow:plan-text)" *primer*)
  (:hook 'nle::read-harness-sections #'plan-section)
  (:start (lambda ()
            (setf *last-error* nil)
            (nle:on-stop (lambda () (setf *last-error* nil))))))
