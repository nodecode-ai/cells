;;;; cell.lisp --- the Duet queue: tasks that run in the background, on approval.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of benfirad's hermes-plugin-antigravity-duet (MIT).
;;;; What the plugin gives a Hermes user, this gives a Nodecode user: a durable
;;;; queue of long-running tasks, where every task a model adds waits in
;;;; "awaiting_approval" until the operator approves it, a worker drains what
;;;; is runnable through the operator's own CLI, and a task that ends by asking
;;;; for one more decision goes back to waiting instead of guessing.
;;;;
;;;; The operator's surface is /duet (also /duffy, as the original had both):
;;;; list, add, show, approve, reject, retry, run. The model's is the
;;;; duet_queue tool, which can add, list and show, and is told to the
;;;; operator's face when it tries to approve its own work. duet_swarm reads a
;;;; Ruflo swarm's state, when that CLI is installed.

(in-package #:nodecode-duet-queue)

;;; --- where the queue lives ------------------------------------------------------

(defun queue-home ()
  "The directory the queue and its task workspaces live in: the `dir' setting,
or duet-queue/ under the Nodecode home."
  (uiop:ensure-directory-pathname (or (setting :dir) (nlk:home "duet-queue/"))))

(defun queue-path ()
  "The one file the queue is: a JSON object of <next_id> and <tasks>."
  (merge-pathnames "queue.json" (queue-home)))

(defun work-directory (id)
  "Task ID's working directory, made, where its CLI runs."
  (let ((dir (merge-pathnames (format nil "work/task-~d/" id) (queue-home))))
    (ensure-directories-exist (merge-pathnames "keep" dir))
    dir))

(defvar *tasks* '()
  "The tasks, oldest first: JSON objects with the keys TASK-JSON writes.
Only this cell's verbs and the worker touch it, each under *LOCK*.")

(defvar *next-id* 1
  "The id the next task added takes. Kept with the queue, so ids are never
reused by a restart.")

(defvar *worker* nil
  "The thread that drains the queue while auto_run is on, or NIL.")

(defvar *busy* nil
  "Whether a task is running: one at a time, the worker and /duet run alike.")

;;; --- the file ------------------------------------------------------------------

(defun now-string ()
  "This moment as a sortable stamp: the machine's own clock."
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(defun clip (text limit)
  "TEXT, at most LIMIT characters, cut with an ellipsis when it was longer."
  (if (and (integerp limit) (plusp limit) (> (length text) limit))
      (concatenate 'string (subseq text 0 (max 0 (1- limit))) (string #\…))
      text))

(defun read-queue ()
  "The tasks and next id the file holds, or none: a file that is not there,
or is not the shape this cell writes, is an empty queue, never a failed start.
=> (VALUES TASKS NEXT-ID)."
  (let ((path (queue-path)))
    (if (not (probe-file path))
        (values '() 1)
        (handler-case
            (let* ((raw (nlk:decode-json (uiop:read-file-string path) :whole t))
                   (tasks (and (hash-table-p raw) (gethash "tasks" raw)))
                   (next (and (hash-table-p raw) (gethash "next_id" raw))))
              (values (if (or (listp tasks) (vectorp tasks))
                          (remove-if-not #'hash-table-p (coerce tasks 'list))
                          '())
                      (if (and (integerp next) (plusp next)) next 1)))
          (error () (values '() 1))))))

(defun write-queue ()
  "Write the queue where READ-QUEUE finds it: a temporary file beside it, then
one rename, so a reader sees the old queue or the new one, never half of one."
  (let* ((path (queue-path))
         (tmp (merge-pathnames "queue.json.tmp"
                                (uiop:pathname-directory-pathname path))))
    (ensure-directories-exist path)
    (with-open-file (out tmp :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string (nlk:encode-json-object
                     (nlk:make-json-object "next_id" *next-id*
                                            "tasks" (coerce *tasks* 'vector)))
                    out))
    (uiop:rename-file-overwriting-target tmp path)))

;;; --- one task ------------------------------------------------------------------

(defun find-task (id)
  "The task ID, or NIL."
  (and (integerp id)
       (find id *tasks* :key (lambda (task) (gethash "id" task)) :test #'=)))

(defun task-status (task)
  (or (gethash "status" task) "pending"))

(defun task-title (task)
  (or (gethash "title" task) ""))

(defun task-category (task)
  (or (gethash "category" task) "general"))

(defun task-result (task)
  (or (gethash "result" task) ""))

(defun task-note (task note)
  "NOTE appended to TASK's log, the last twenty kept."
  (let ((log (coerce (or (gethash "log" task) #()) 'list)))
    (setf (gethash "log" task) (coerce (last (append log (list note)) 20) 'vector))))

(defun task-lines (task)
  (coerce (or (gethash "log" task) #()) 'list))

(defun task-line (task)
  "One line for a task: its id, status, category and title."
  (format nil "#~d  ~a  [~a]  ~a"
          (gethash "id" task) (task-status task) (task-category task)
          (clip (task-title task) 90)))

(defun list-tasks (&optional (limit 20))
  "The newest LIMIT tasks, newest first, as lines."
  (let* ((tasks (reverse *tasks*))
         (shown (if (and (integerp limit) (< limit (length tasks)))
                    (subseq tasks 0 limit)
                    tasks)))
    (mapcar #'task-line shown)))

(defun add-task (title &key (category "general") (status "pending") (note ""))
  "Add TITLE to the queue as STATUS and answer its id. The oldest tasks beyond
the `max_tasks' cap are dropped."
  (with-duet-queue-lock
    (let* ((id *next-id*)
           (task (nlk:make-json-object
                  "id" id
                  "title" title
                  "category" (if (and (stringp category) (plusp (length category)))
                                 category
                                 "general")
                  "status" status
                  "created" (now-string)
                  "updated" (now-string)
                  "completed" ""
                  "approval_action" ""
                  "result" ""
                  "log" (if (plusp (length note)) (vector note) (vector)))))
      (setf *next-id* (1+ id)
            *tasks* (last (append *tasks* (list task)) (setting :max-tasks)))
      (write-queue)
      id)))

(defun reset-queue ()
  "Empty the queue in memory and on disk: what a test starts from."
  (with-duet-queue-lock
    (setf *tasks* '() *next-id* 1)
    (write-queue)))

(defun set-status (id status &optional note)
  "Move task ID to STATUS, with NOTE on its log. => T when the task is there."
  (with-duet-queue-lock
    (let ((task (find-task id)))
      (when task
        (setf (gethash "status" task) status
              (gethash "updated" task) (now-string))
        (when (and (stringp note) (plusp (length note)))
          (task-note task note))
        (write-queue)
        t))))

(defun queue-summary ()
  "The queue as the model reads it when it lists it: the waiting tasks first,
then what each status means."
  (with-duet-queue-lock
    (let* ((lines (list-tasks 30))
           (waiting (count "awaiting_approval" *tasks* :key #'task-status :test #'string=)))
      (format nil "~d task~:p in the queue~@[, ~d waiting for the operator~].~%~{~a~^~%~}~%~
~@[~%The operator approves a task with /duet approve ID, and the worker runs it through ~
~a.~]"
              (length *tasks*) (and (plusp waiting) waiting)
              (or lines (list "(nothing queued)"))
              (setting :command)))))

;;; --- the runner ------------------------------------------------------------------

(defun default-run-command (argv &key directory timeout)
  "Run ARGV in DIRECTORY, at most TIMEOUT seconds: (VALUES OUTPUT STATUS), the
CLI's output and its exit code, or -1 when it never finished. A runner is the
one seam that runs something; *RUN-COMMAND* is what a test replaces."
  (let ((process nil))
    (unwind-protect
         (handler-case
             (sb-ext:with-timeout (max 1 (or timeout 600))
               (setf process (uiop:launch-program argv :search t
                                                       :directory directory
                                                       :input nil
                                                       :output :stream
                                                       :error-output :output))
               (let ((output (with-output-to-string (out)
                               (loop for line = (read-line (uiop:process-info-output process)
                                                           nil nil)
                                     while line do (write-line line out))))
                     (status (uiop:wait-process process)))
                 (setf process nil)
                 (values output (or status -1))))
           (sb-ext:timeout ()
             (values (format nil "timed out after ~a seconds" timeout) -1))
           (error (condition)
             (values (format nil "could not run ~{~a~^ ~}: ~a" argv condition) -1)))
      (when (and process (ignore-errors (uiop:process-alive-p process)))
        (ignore-errors (uiop:terminate-process process))))))

(defvar *run-command* #'default-run-command
  "How a task and the swarm are run: (ARGV &KEY DIRECTORY TIMEOUT) => (VALUES
OUTPUT STATUS). Tests bind it; nothing else replaces it.")

;;; --- the gate and the worker ------------------------------------------------------

(defun runnable-task ()
  "The next task to run, approved before merely pending, or NIL."
  (or (find "approved" *tasks* :key #'task-status :test #'string=)
      (find "pending" *tasks* :key #'task-status :test #'string=)))

(defun claim-task ()
  "Take the next runnable task for this worker: it is running on the file
before its CLI starts, so a second worker cannot take it too. => the task."
  (with-duet-queue-lock
    (let ((task (runnable-task)))
      (when task
        (setf (gethash "status" task) "running"
              (gethash "updated" task) (now-string))
        (write-queue))
      task)))

(defun task-prompt (task directory)
  "What the CLI runs for TASK in DIRECTORY: the instruction, and what to do
when the work needs a decision the operator has not given yet."
  (format nil "You are an autonomous background worker on someone's machine.~%Task #~d (~a): ~a~%Working directory: ~a~%~%Do the work: research, drafts, analysis, and local files. When the task needs an external, financial or destructive action (sending something to another person, writing to a production system, deleting what you did not make), do not do it on your own: end your answer with [APPROVAL_REQUIRED: <the one action you would take>] and the operator decides.~%Finish with a short summary of what you did."
          (gethash "id" task) (task-category task) (task-title task) directory))

(defun approval-marker (output)
  "OUTPUT's [APPROVAL_REQUIRED: ...] as its action, and where it starts, or
NIL: (VALUES ACTION START)."
  (let ((start (search "[APPROVAL_REQUIRED:" output :test #'char-equal)))
    (when start
      (let* ((open (+ start (length "[APPROVAL_REQUIRED:")))
             (end (position #\] output :start open))
             (action (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (subseq output open (or end (length output))))))
        (values (if (plusp (length action)) action "the task asks for a decision")
                start)))))

(defun answer-text (output prompt)
  "OUTPUT without the prompt a CLI echoed back before its own answer: the
marker a task ends with is its answer's, never the instruction's own."
  (let ((start (and (plusp (length prompt)) (search prompt output))))
    (if start (subseq output (+ start (length prompt))) output)))

(defun settle-task (id output status)
  "Put what the CLI answered on task ID: done, failed, or back in
awaiting_approval when it ended by asking. Then say so where the operator is."
  (with-duet-queue-lock
    (let* ((task (find-task id))
           (output (or output ""))
           (text (clip (string-trim '(#\Space #\Tab #\Newline #\Return) output) 4000)))
      (when task
        (multiple-value-bind (action marker-start) (and (eql status 0) (approval-marker output))
          (cond
            ((and (eql status 0) action)
             (setf (gethash "status" task) "awaiting_approval"
                   (gethash "approval_action" task) action
                   (gethash "result" task) (string-trim '(#\Space #\Newline #\Tab #\Return)
                                                         (subseq output 0 (or marker-start 0))))
             (task-note task (format nil "paused at the gate: ~a" action))
             (nle:notice (format nil "duet: task #~d is waiting on you - ~a (/duet approve ~d)"
                                 id action id)
                         :level :warning :key "nodecode-duet-queue"))
            ((eql status 0)
             (setf (gethash "status" task) "done"
                   (gethash "result" task) text
                   (gethash "completed" task) (now-string)
                   (gethash "approval_action" task) "")
             (task-note task "finished")
             (nle:notice (format nil "duet: task #~d done - ~a" id (clip (task-title task) 60))
                         :level :info :key "nodecode-duet-queue"))
            (t
             (setf (gethash "status" task) "failed"
                   (gethash "result" task) text)
             (task-note task (format nil "the CLI answered ~a" status))
             (nle:notice (format nil "duet: task #~d failed (exit ~a) - /duet show ~d" id status id)
                         :level :error :key "nodecode-duet-queue")))
          (setf (gethash "updated" task) (now-string))
          (write-queue))))))

(defun execute-task (task)
  "Run TASK's CLI: its prompt as the last argument, in a workspace of its own,
under the timeout_seconds ceiling. => TASK's id."
  (let* ((id (gethash "id" task))
         (dir (work-directory id))
         (argv (append (list (setting :command))
                       (setting :args)
                       (list (task-prompt task dir)))))
    (multiple-value-bind (output status)
        (funcall *run-command* argv :directory dir :timeout (setting :timeout-seconds))
      (settle-task id (answer-text (or output "") (task-prompt task dir)) status))
    id))

(defun run-one ()
  "Claim and run the next runnable task: its id, or NIL when nothing is
runnable or one is already running."
  (if *busy*
      nil
      (let ((*busy* t))
        (let ((task (claim-task)))
          (when task (execute-task task))))))

(defun tick ()
  "The worker's step: run one runnable task, when auto_run is on."
  (when (setting :auto-run)
    (run-one)))

;;; --- the tool the model calls ------------------------------------------------------

(defparameter +queue-actions+ '("add" "list" "show" "approve" "reject" "retry")
  "What duet_queue accepts. The last three are the operator's; the model is told so.")

(nlk:define-contract duet-queue-call
  (:doc "The Duet background queue on this machine: hand it a long task to run later, in the background, without holding this conversation open. Tasks you add wait in awaiting_approval: the operator approves one with /duet approve ID and the worker runs it through their own CLI, so say plainly what the task is, as a complete instruction to someone who will see only that sentence. list shows the queue, show reads one task's result and log. approve, reject and retry are the operator's: the model is refused and told to ask.")
  ("action" :text :required t :options +queue-actions+
            :doc "add, list, show, or (for the operator) approve, reject, retry")
  ("title" :string
           :doc "the task, as one complete instruction (add)")
  ("category" :string :default "general"
              :doc "general, research, code, ops, swarm")
  ("task_id" :integer
             :doc "the task's id (show, approve, reject, retry)"))

(defun add-verb (call)
  "duet_queue add: TITLE into the queue, gated unless the section says not."
  (let ((title (string-trim '(#\Space #\Tab #\Newline #\Return)
                             (or (duet-queue-call-title call) ""))))
    (when (zerop (length title))
      (fail "add needs a title: the task, as one instruction"))
    (let* ((gated (setting :approval-required))
           (id (add-task title
                         :category (or (duet-queue-call-category call) "general")
                         :status (if gated "awaiting_approval" "pending")
                         :note (if gated
                                   "added by a model; waiting for the operator"
                                   "added by a model; runnable"))))
      (if gated
          (format nil "Task #~d queued as awaiting_approval. Tell the operator: it runs when they ~
run /duet approve ~d.~%~a" id id (task-line (find-task id)))
          (format nil "Task #~d queued and runnable; the worker takes it on its next pass.~%~a"
                  id (task-line (find-task id)))))))

(defun show-verb (id)
  "One task as the model or the operator reads it, or a refusal saying there is none."
  (unless id
    (return-from show-verb "show needs a task_id: /duet list shows the ids"))
  (let ((task (find-task id)))
    (unless task
      (return-from show-verb (format nil "there is no task #~d; /duet list shows the queue" id)))
    (with-output-to-string (out)
      (format out "~a~%created ~a, updated ~a~@[~%, completed ~a~]~@[~%, waiting for: ~a~]~%"
              (task-line task)
              (or (gethash "created" task) "-")
              (or (gethash "updated" task) "-")
              (let ((done (gethash "completed" task)))
                (and done (plusp (length done)) done))
              (let ((action (gethash "approval_action" task)))
                (and action (plusp (length action)) action)))
      (let ((result (task-result task)))
        (format out "~%result~%~a~%" (if (plusp (length result)) result "(nothing yet)")))
      (let ((lines (task-lines task)))
        (when lines
          (format out "~%log~%~{  ~a~%~}" lines))))))

(defun operator-only-answer (action id)
  "The answer a model's approve, reject or retry gets: whose verb it is."
  (format nil "~@[Task #~d is unchanged: ~]~a is the operator's verb, not the model's. Ask them to run ~
/duet ~a~@[ ~d~], and do not claim it ran until they have."
          (and id id) action action id))

(defun queue-tool (call)
  "The duet_queue tool: one verb of the queue, as its answer."
  (let ((action (string-downcase (or (duet-queue-call-action call) "")))
        (id (duet-queue-call-task-id call)))
    (cond
      ((string= action "add") (add-verb call))
      ((string= action "list") (queue-summary))
      ((string= action "show") (show-verb id))
      ((member action '("approve" "reject" "retry") :test #'string=)
       (operator-only-answer action id))
      (t (fail "~s is not one of the queue's actions: ~{~a~^, ~}"
               action +queue-actions+)))))

;;; --- the swarm tool, read-only --------------------------------------------------

(nlk:define-contract duet-swarm-call
  (:doc "Read the state of a Ruflo multi-agent swarm on this machine, when the Ruflo CLI is installed: whether a swarm is up and which agents it has. It starts, stops and spawns nothing.")
  ("action" :text :required t :options '("status" "agents")
            :doc "status of the swarm, or the agents it has"))

(defun swarm-argv (action)
  (let ((program (setting :swarm-command)))
    (cond ((string= action "status") (list program "swarm" "status"))
          ((string= action "agents") (list program "agent" "list"))
          (t (fail "~s is not one of the swarm's actions: status, agents" action)))))

(defun swarm-run (action)
  "ACTION read off the Ruflo CLI, as its answer."
  (let ((argv (swarm-argv action)))
    (multiple-value-bind (output status)
        (funcall *run-command* argv :timeout (min 120 (setting :timeout-seconds)))
      (let ((text (string-trim '(#\Space #\Newline #\Tab #\Return) (or output ""))))
        (cond ((and (eql status 0) (plusp (length text))) text)
              ((eql status 0) (format nil "~a answered nothing" (first argv)))
              (t (format nil "~a answered ~a~@[~% ~a~]"
                         (first argv) status (and (plusp (length text)) text))))))))

(defun swarm-tool (call)
  "The duet_swarm tool: the swarm's state, as its answer."
  (swarm-run (string-downcase (or (duet-swarm-call-action call) "status"))))

;;; --- the operator's /duet -------------------------------------------------------

(defparameter +usage+ "
/duet (or /duffy) - the background queue: tasks run through your own CLI

  list                    the queue, newest first, and what waits for you
  add <instruction>       add a task of your own, runnable at once
  show <id>               one task: its result, its log, what it is waiting for
  approve <id>            let a task the model added run
  reject <id>             drop a task
  retry <id>              put a failed or rejected task back in the queue
  run [id]                run that task now, or every runnable one
"
  "What /duet answers when it is asked for help.")

(defparameter +primer+
  "duet_queue adds a task to this machine's background queue; a task a model adds waits in awaiting_approval until the operator runs /duet approve ID, and then a worker runs it through the operator's own agent CLI, with the task's prompt as its last argument. Tasks are complete instructions to someone who sees only that sentence. /duet list, add, show, approve, reject, retry and run are the operator's; show reads a task's result and log."
  "(help :duet-queue)'s text, and the line every request carries.")

(defun move-task (action id)
  "The operator's approve, reject or retry of task ID, as its answer."
  (unless id
    (return-from move-task (format nil "usage: /duet ~a <task id>" action)))
  (let ((task (find-task id)))
    (unless task
      (return-from move-task (format nil "there is no task #~d" id)))
    (let ((status (cond ((string= action "approve") "approved")
                        ((string= action "reject") "rejected")
                        (t "pending"))))
      (set-status id status (format nil "the operator ran /duet ~a" action))
      (if (string= action "approve")
          (format nil "Task #~d approved; the worker takes it on its next pass.~%~a"
                  id (task-line task))
          (format nil "Task #~d is ~a.~%~a" id status (task-line task))))))

(defun run-verb (id)
  "Run task ID now, or every runnable task, as its answer."
  (cond
    (id
     (let ((task (find-task id)))
       (unless task
         (return-from run-verb (format nil "there is no task #~d" id)))
       (unless (member (task-status task) '("approved" "pending") :test #'string=)
         (return-from run-verb
           (format nil "Task #~d is ~a; /duet approve ~d runs it." id (task-status task) id)))
       (set-status id "approved" "the operator asked for it now")
       (let ((ran (run-one)))
         (cond ((eql ran id) (format nil "Task #~d ran.~%~a" id (show-verb id)))
               (ran (format nil "Task #~a ran first; #~d is approved and goes next." ran id))
               (t (format nil "Task #~d is approved; a task is already running, so it goes next."
                           id))))))
    (t
     (let ((ran '()))
       (loop for one = (run-one) while one do (push one ran))
       (if ran
           (format nil "Ran ~d task~:p: ~{~a~^, ~}"
                   (length ran) (mapcar (lambda (one) (format nil "#~d" one)) (reverse ran)))
           "Nothing is runnable: a task the model added waits for /duet approve ID.")))))

(defun run-slash (args session-id)
  "One /duet (or /duffy) invocation, as its answer: the operator's surface for
queue, the gate, and the Ruflo swarm the original bundled beside it."
  (declare (ignore session-id))
  (let* ((text (string-trim '(#\Space #\Tab) (or args "")))
         (head (string-downcase (or (first (nlk:split-words text)) "")))
         (tail (string-trim '(#\Space #\Tab)
                             (subseq text (min (length text) (length head)))))
         (id (ignore-errors (parse-integer tail :junk-allowed t))))
    (cond
      ((zerop (length head)) (queue-summary))
      ((member head '("help" "-h" "--help" "?") :test #'string=) (string-left-trim '(#\Newline) +usage+))
      ((member head '("list" "ls") :test #'string=) (queue-summary))
      ((string= head "add")
       (if (zerop (length tail))
           "usage: /duet add <the task, as one instruction>"
           (let ((id (add-task tail :status "pending" :note "added by the operator")))
             (format nil "Task #~d queued and runnable.~%~a" id (task-line (find-task id))))))
      ((string= head "show") (show-verb id))
      ((member head '("approve" "reject" "retry") :test #'string=) (move-task head id))
      ((string= head "run") (run-verb id))
      ((string= head "swarm") (swarm-run "status"))
      (t (format nil "~a is not one of its commands~%~a" head (string-left-trim '(#\Newline) +usage+))))))

;;; --- the entry ------------------------------------------------------------------

(defun install ()
  "The queue off its file, one worker when auto_run is on, and the line that
says how many tasks are waiting for the operator. A section with no command
refuses here: a queue that cannot run anything is not a queue."
  (unless (plusp (length (setting :command)))
    (error 'nlk:config-refusal
           :detail "the duet-queue section names no command: put the CLI that runs a task \nin \"command\" (agy, claude, codex ...) and (restart-cells \"nodecode-duet-queue\")"))
  (multiple-value-bind (tasks next) (read-queue)
    (setf *tasks* tasks
          *next-id* (max next 1 (1+ (reduce #'max tasks
                                            :key (lambda (task) (or (gethash "id" task) 0))
                                            :initial-value 0)))
          *busy* nil))
  (when (setting :auto-run)
    (setf *worker* (nlk:worker-start "duet-queue" #'tick
                                     :wake (setting :interval-seconds))))
  (nle:on-stop (lambda ()
                 (setf *worker* (nlk:worker-stop *worker*)
                       *tasks* '() *next-id* 1 *busy* nil)
                 (nle:notice nil :key "nodecode-duet-queue")))
  (let ((waiting (count "awaiting_approval" *tasks* :key #'task-status :test #'string=)))
    (when (plusp waiting)
      (nle:notice (format nil "duet: ~d task~:p waiting for your approval - /duet list" waiting)
                  :level :info :key "nodecode-duet-queue"))))

(nle:define-cell duet-queue
  (:section ("duet-queue")
    (:guide "command is the CLI that runs a task, with the prompt as its last argument; approval_required keeps what a model adds waiting for you; auto_run drains the queue in the background")
    ("command" :string :required t
               :doc "the CLI that runs a task: your own agent CLI, e.g. agy, claude or codex")
    ("args" :list
            :doc "flags the command takes before the prompt, e.g. [\"--print\"]")
    ("approval_required" :boolean :default t
                          :doc "what a model adds waits for your approval; off, it runs as soon as it is added")
    ("auto_run" :boolean :default t
                :doc "run runnable tasks in the background")
    ("interval_seconds" :integer :default 15 :min 1
                        :doc "how often the worker looks for a runnable task")
    ("timeout_seconds" :integer :default 600 :min 1
                       :doc "one task's ceiling")
    ("max_tasks" :integer :default 100 :min 1
                 :doc "how many tasks the queue keeps")
    ("dir" :path :doc "where the queue and its task workspaces live; empty keeps them under the Nodecode home")
    ("swarm_command" :string :default "ruflo"
                     :doc "the Ruflo CLI duet_swarm reads, when it is installed"))
  (:start #'install)
  (:help :duet-queue
         "duet_queue and /duet: a background task queue the operator approves before it runs"
         +primer+)
  (:tool "duet_queue" (nlk:contract 'duet-queue-call) 'queue-tool)
  (:tool "duet_swarm" (nlk:contract 'duet-swarm-call) 'swarm-tool)
  (:command "duet" #'run-slash
            :description "The background queue: list, add, show, approve, reject, retry, run"
            :argument-hint "list|add <task>|show <id>|approve <id>|run [id]")
  (:command "duffy" #'run-slash
            :description "Alias for /duet, as the original plugin spelled it"
            :argument-hint "list|add <task>|show <id>|approve <id>|run [id]"))
