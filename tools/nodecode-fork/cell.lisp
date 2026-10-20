;;;; cell.lisp --- a turn forks itself: its independent tasks run at once, each
;;;; in a fork of the session that holds its whole context.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; (fork:run "task" ...) forks the calling session at the turn running it,
;;;; once per task, puts each task to its fork and waits for every fork's turn
;;;; to end. A fork is an anchored child (NLK:CREATE-SESSION :PARENT
;;;; :ANCHOR-TURN-ID): its history composes the parent's by reference through
;;;; the running turn, and the create carries the parent's model, so its first
;;;; request extends the parent's cached prefix and it pays for its own work
;;;; alone. That history ends in the eval call waiting on it, which the fold
;;;; answers `no result recorded (interrupted)': the task's words say why, and
;;;; that a fork does not fork.
;;;;
;;;; The wait rides the :FRAME point: a fork's turn ending pokes its waiter,
;;;; and the log (NLK:TURN-TERMINAL-KIND) says how it ended. A fork's turn
;;;; runs under the call's seconds (NLE:TURN-BUDGET): past them its tools are
;;;; refused and it answers where it stands, and one still running
;;;; *GRACE-SECONDS* later is cancelled. The caller unwinding -- an eval
;;;; interrupt, its own turn stopped -- cancels every fork still running.
;;;;
;;;; Forks are sessions like any other: /sessions draws them under the session
;;;; they came off, and their turns are in the log. They are never named (a
;;;; model call each) and never reflected on (their source is `fork').

(in-package #:nodecode-fork)

;;; --- the manual ---------------------------------------------------------------
;;; (help :fork) answers it while the cell runs; every request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter +primer+
  "Forking this turn, through the nodecode-fork cell: one verb, called through
eval. A refusal is ERROR: FORK-ERROR with the reason.
  (fork:run \"task\" ... &key seconds)   fork this session at the turn running
      it, once per task (at most 4), and wait for them all. Each fork holds
      this whole context, works its one task on this session's model, and
      answers. => one plist a task, in order: (:task :session :status :answer
      :ran) -- STATUS :completed, :failed, :cancelled or :timeout, ANSWER the
      fork's last words, RAN the first line of each eval it ran. SECONDS
      bounds each fork's turn (fork.seconds, 900, unless given): past it the
      fork's tools are refused and it answers where it stands.
When to fork: work that splits into branches that do not need each other --
a probe beside a measurement, one check over three folders -- or one probe
whose output would bury this context. When not to: a plain linear task, one
read, steps that each need the step before; do those here. Forks run at once
in the same folder, so give each a task that touches nothing another one does.
A fork cannot fork, and its answer comes back here, never to a person. A fork
pays for its own work, not this context again: it starts from this session's
cached prefix. A wait past the eval's yield goes on in the background, and its
value arrives when the last fork is done."
  "What (help :fork) answers while the cell runs.")

;;; --- the words -------------------------------------------------------------------
;;; Data, so a layer can reword them.

(defparameter *task-input*
  "[fork ~d of ~d] You are a fork of this session, made at the eval call above, which is your parent's and is still waiting on you -- that is why its result reads interrupted. Do not run it again: a fork cannot fork. Your one task:~%~a~%~:[~;Other forks are working on the other tasks at this moment, in the same folder: change nothing another task could touch. ~]Make no posts, messages or commits unless the task says to. Your answer goes back to that eval call, not to a person: lead with the result, then only the facts that back it."
  "A fork's input, over its number, how many forks the call made, its task,
and whether it has siblings.")

;;; --- the forks -------------------------------------------------------------------

(defparameter +most+ 4 "The forks one call may make.")

(defparameter *grace-seconds* 60
  "How long past its budget a fork's turn may still run before it is cancelled.")

(defparameter +turn-ends+ '("turn.completed" "turn.failed" "turn.cancelled"))

(defvar *waiting* (make-hash-table :test #'equal :synchronized t)
  "The forks a call is waiting on, by session id: (:SEMAPHORE S :SECONDS N).")

(defun fork-name (turn n)
  "The session id of TURN's Nth fork: fork-<turn>-<n>, numbered further when
taken."
  (nlk:unused-name (format nil "fork-~a-~d" turn n) #'nlk:session-exists-p))

(defun fork-p (session)
  "Whether SESSION is a fork this cell made."
  (and (stringp session) (uiop:string-prefix-p "fork-turn-" session) t))

(defun caller ()
  "(values SESSION TURN): the session whose eval is calling and the id of the
turn running it, or a refusal."
  (let ((session nlk:*scribe-session-id*)
        (turn nle::*current-durable-turn*))
    (cond ((fork-p session)
           (fail "a fork cannot fork: work your task here, and say in your answer what you would have forked"))
          ((not (and session turn (equal session (nlk:durable-turn-session-id turn))))
           (fail "fork:run forks the turn running it: call it from a session's eval, inside a turn"))
          (t (values session (nlk:durable-turn-turn-id turn))))))

(defun parse (arguments)
  "(values TASKS SECONDS) of fork:run's ARGUMENTS: the tasks, strings, then
:SECONDS N; or a refusal."
  (let* ((at (or (position-if-not #'stringp arguments) (length arguments)))
         (tasks (subseq arguments 0 at))
         (options (nthcdr at arguments)))
    (unless (and (evenp (length options))
                 (loop for (key) on options by #'cddr always (eq key :seconds)))
      (fail "fork:run takes the tasks, strings, then :seconds N alone; it was given ~s" options))
    (let ((seconds (getf options :seconds (setting :seconds))))
      (cond ((null tasks) (fail "fork:run needs a task: (fork:run \"task\" ...)"))
            ((> (length tasks) +most+)
             (fail "fork:run makes at most ~d forks a call; it was given ~d tasks" +most+ (length tasks)))
            ((some (lambda (task) (zerop (length (nlk:trimmed task)))) tasks)
             (fail "a task is words, and one was blank"))
            ((not (and (integerp seconds) (plusp seconds)))
             (fail ":seconds is a whole number of seconds above zero, not ~s" seconds))
            (t (values tasks seconds))))))

(defun start (session turn cwd task n count seconds)
  "Fork SESSION at TURN as its Nth of COUNT forks and put TASK to it. => the
fork: (:TASK :SESSION :TURN), or, when its turn could not start, already as
fork:run answers it, :STATUS :FAILED."
  (let ((child (fork-name turn n)))
    (nlk:create-session :id child :cwd cwd :parent session :anchor-turn-id turn)
    ;; Waited on before the submit: its turn's budget is read as it opens.
    (setf (gethash child *waiting*)
          (list :semaphore (bt2:make-semaphore :name child) :seconds seconds))
    (handler-case
        (list :task task :session child
              :turn (nlk:active-input-admission-turn-id
                     (nle:submit child (format nil *task-input* n count task (> count 1))
                                 :command-id (format nil "fork:~a" child)
                                 :source "fork" :source-id session)))
      (error (condition)
        (list :task task :session child :status :failed
              :answer (format nil "its turn could not start: ~a" condition) :ran '())))))

(defun await-fork (fork deadline seconds)
  "Wait for FORK's turn to end, or for DEADLINE (internal real time) to pass.
=> the turn's terminal kind, or :TIMEOUT once it has been asked to stop."
  (let ((child (getf fork :session))
        (turn (getf fork :turn)))
    (loop
      (let ((kind (nlk:turn-terminal-kind turn)))
        (when kind (return kind)))
      (let ((left (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
        (when (<= left 0)
          (nlk:request-cancel-turn child turn (format nil "the fork ran past its ~d seconds" seconds))
          ;; its last words land with its ending
          (nlk:wait-until (lambda () (nlk:turn-terminal-kind turn)) (nlk:deadline-after 10) 0.1)
          (return :timeout))
        ;; A frame pokes at the ending; the bound stands under a frame not seen.
        (bt2:wait-on-semaphore (getf (gethash child *waiting*) :semaphore)
                               :timeout (float (min left 30)))))))

(defun ran (child turn)
  "The first line of each eval CHILD's TURN ran -- its note -- in order, or the
name of any other tool it called."
  (loop for payload in (nlk:events :session-id child :turn-id turn
                                   :kind nlk::+kind-tool-call-started+ :as :payloads)
        for form = (nlk:json-value (ignore-errors
                                    (nlk:decode-json (nlk:json-value payload :string "arguments")))
                                   :string "form")
        collect (if (and form (plusp (length (nlk:trimmed form))))
                    (nlk:clip (nlk:trimmed (first (nlk:lines (nlk:trimmed form)))) 120)
                    (or (nlk:json-value payload :string "tool-name") "a tool"))))

(defun ending (child turn)
  "What CHILD's TURN said as it failed or was cancelled, or NIL."
  (nlk:json-value (nlk:events :session-id child :turn-id turn
                              :kind '("turn.failed" "turn.cancelled") :as :payload)
                  :string "detail"))

(defun result (fork kind)
  "FORK as fork:run answers it, its turn having ended as KIND."
  (let ((child (getf fork :session))
        (turn (getf fork :turn)))
    (list :task (getf fork :task) :session child
          :status (cond ((eq kind :timeout) :timeout)
                        ((equal kind "turn.completed") :completed)
                        ((equal kind "turn.failed") :failed)
                        (t :cancelled))
          :answer (or (nlk:turn-answer child turn) (ending child turn))
          :ran (ran child turn))))

(define-verb run (&rest arguments)
  "Fork the calling session at the turn running it, once per task, and wait
for every fork's turn to end. ARGUMENTS: the tasks, strings, then :SECONDS N.
=> one plist a task, in order: (:TASK :SESSION :STATUS :ANSWER :RAN)."
  (multiple-value-bind (tasks seconds) (parse arguments)
    (multiple-value-bind (session turn) (caller)
      (let ((cwd (nlk:find-session-cwd session))
            (count (length tasks))
            (forks '()))
        ;; However this call ends, no fork outlives the wait for it.
        (nlk:with-cleanup ((dolist (fork forks)
                             (let ((child (getf fork :session))
                                   (started (getf fork :turn)))
                               (remhash child *waiting*)
                               (when (and started (not (nlk:turn-terminal-kind started)))
                                 (ignore-errors
                                  (nlk:request-cancel-turn child started "the call waiting on it stopped"))))))
          (loop for task in tasks
                for n from 1
                do (push (start session turn cwd task n count seconds) forks))
          (setf forks (reverse forks))
          (let ((deadline (+ (get-internal-real-time)
                             (* (+ seconds *grace-seconds*) internal-time-units-per-second))))
            (loop for fork in forks
                  ;; one that never started is already as answered
                  collect (if (getf fork :status)
                              fork
                              (result fork (await-fork fork deadline seconds))))))))))

;;; --- the hooks -------------------------------------------------------------------

(defun observe (op)
  "A turn ended somewhere: wake whoever waits on it, when it was a fork's.
Publishing thread: a lookup and a signal."
  (when (member (nlk:frame-fact op) +turn-ends+ :test #'equal)
    (let ((waiting (gethash (getf op :session-id) *waiting*)))
      (when waiting
        (bt2:signal-semaphore (getf waiting :semaphore))))))

(defun budget-hook (next turn)
  "Advice on NLE:TURN-BUDGET: a fork's turn runs under the seconds its call
gave; every other turn is NEXT's."
  (let ((waiting (gethash (getf turn :session-id) *waiting*)))
    (if waiting
        (list :seconds (getf waiting :seconds))
        (funcall next turn))))

(defun naming-hook (next session-id text)
  "Advice on NLE:NAME-SESSION: a fork is never named, its name a model call.
Every other session is NEXT's."
  (unless (fork-p session-id)
    (funcall next session-id text)))

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell fork
  (:section ("fork")
    (:guide "fork:run hands a turn's independent tasks to forks of its session; seconds bounds each fork's turn")
    ("seconds" :integer :default 900 :min 30
               :doc "seconds a fork's turn may run before its tools are refused"))
  (:hook :frame (nle:observer #'observe "fork"))
  (:hook 'nle:turn-budget #'budget-hook)
  (:hook 'nle:name-session #'naming-hook)
  (:help :fork "fork: whenever an ask splits into two or more independent investigations that each take several reads, run them at once with (fork:run \"task 1\" \"task 2\" ...) instead of one after another: each of up to 4 forks of this session holds this whole context, and the call answers their results. Also for a probe whose output would bury this context; never for a linear task or a quick read" +primer+))
