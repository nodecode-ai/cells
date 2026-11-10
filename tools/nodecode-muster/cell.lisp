;;;; cell.lisp --- the board of flights, /muster, its tools, and START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of the original's __init__.py, fleet.py and ledger.py. Where
;;;; the Hermes plugin sat on subagent_start/subagent_stop hooks, a Nodecode
;;;; cell reads the one live fact that says how a running call's forks stand
;;;; - session_fork_progress, published while (fork "task" ...) runs - so the
;;;; board fills itself and nothing has to be started through muster. A
;;;; stalled row that hears from its fork again heals; a row still live under
;;;; another boot reads gone; a failed job is re-run in its own session, as
;;;; many times as the section allows and no more.

(in-package #:nodecode-muster)

(defparameter +primer+
  "The muster board says how every subagent flight stands: working, stuck
(silent too long), failing on repeat, gone (the image went down mid-flight),
done, or stopped. It fills itself: anything started with (fork \"task\" ...)
is on it a moment later, and nothing leaves this machine.

  /muster                      the board: what is running, stuck, gone
  /muster stop ID              stop one worker's turn
  /muster note ID MESSAGE       send a running worker a message
  /muster help                 this text
  /muster-digest               the report: the counts and the decisions

A worker silent for muster.stall_seconds is stuck; the next frame unsticks
it. The same job failing muster.spin_threshold times inside
muster.spin_window_seconds is failing on repeat. A job that ended badly is
re-run in its own session up to muster.max_requeues times (set auto_requeue
to false to watch only). Set show_goals to false to keep the jobs off a
shared screen.")
;; A note on the quote above: the original was one paragraph per verb; the
;; board, the ids and the four counts are what a model and a person both read.

(defvar *worker* nil
  "The sweeping thread while it runs, or NIL.")

(defvar *workers* t
  "Whether START-CELL runs the sweeping thread; a test calls TICK by hand.")

(defvar *running* nil
  "Whether the cell is started: the observer drops frames otherwise.")

(defvar *inbox* '()
  "Fork rows off the publishing thread, waiting for the sweeper: a list and a
poke, no I/O on a frame.")

(defvar *ledger-changed* nil
  "Whether anything reached the ledger since the last write.")

(defvar *inbox-lock* (sb-thread:make-mutex :name "muster inbox"))
(defvar *ledger-lock* (sb-thread:make-mutex :name "muster ledger"))

;;; --- what a frame says -------------------------------------------------------

(defun field (object key)
  (and (hash-table-p object) (gethash key object)))

(defun fork-row (fork parent)
  "One fork of a running call, as the frame draws it: whose it is, its task,
its own status, and what it last said."
  (list :id (field fork "session_id")
        :parent parent
        :turn (field fork "turn_id")
        :label (field fork "label")
        :status (field fork "status")
        :calls (field fork "calls")
        :note (field fork "note")
        :answer (field fork "answer")
        :ms (field fork "ms")))

(defun observe-frame (op)
  "A :FRAME observer: every fork progress frame is queued for the sweeper.
Publishing thread: a list, a poke, no I/O."
  (when (and *running* (equal "session_fork_progress" (getf op :kind)))
    (let* ((payload (getf op :payload))
           (parent (getf op :session-id))
           (forks (field payload "forks")))
      (when (and (vectorp forks) (plusp (length forks)))
        (let ((rows (loop for fork across forks
                          when (hash-table-p fork) collect (fork-row fork parent))))
          (when rows
            (sb-thread:with-mutex (*inbox-lock*)
              (setf *inbox* (nconc *inbox* rows)))
            (when *worker* (ignore-errors (nlk:worker-poke *worker*)))))))))

;;; --- the sweep ---------------------------------------------------------------

(defun drain-inbox (now)
  "Every queued fork row, folded into the ledger at NOW."
  (let ((rows (sb-thread:with-mutex (*inbox-lock*)
                (prog1 *inbox* (setf *inbox* '())))))
    (when rows
      (sb-thread:with-mutex (*ledger-lock*)
        (dolist (row rows)
          (when (getf row :id) (absorb row now))))
      (setf *ledger-changed* t))))

(defun resubmit (session text)
  "Run the job again in the flight's own session: the in-process ingress a
cell uses, never a new process. A test stubs this."
  (nle:submit session text :source "muster" :source-id session))

(defun retry (flight now)
  "Re-run FLIGHT's job once, in its session, and count the attempt."
  (let ((id (getf flight :id))
        (label (getf flight :label))
        (ended (getf flight :ended)))
    (when (and (stringp id) (stringp label) (plusp (length label)))
      (handler-case
          (progn
            (resubmit id (format nil "~a~%[muster] the last run ended badly; this is the retry." label))
            (sb-thread:with-mutex (*ledger-lock*)
              (setf flight (plist-set flight :requeues (1+ (or (getf flight :requeues) 0))))
              (setf flight (plist-set flight :retried ended))
              (setf flight (plist-set flight :ended nil))
              (setf flight (plist-set flight :last-seen now))
              (setf flight (plist-set flight :state "RUNNING"))
              (setf (gethash id *flights*) flight)
              (setf *ledger-changed* t)))
        (error () nil)))))

(defun sweep (now)
  "Read every row at NOW: a working row gone quiet is stuck, a stuck row that
spoke again is working, and a job that ended badly gets its re-run."
  (let ((stall (setting-int :stall-seconds 300))
        (max-requeues (setting-int :max-requeues 2))
        (auto (setting :auto-requeue))
        (due '()))
    (sb-thread:with-mutex (*ledger-lock*)
      (maphash
       (lambda (id flight)
         (declare (ignore id))
         (cond ((stalled-p flight now stall)
                (setf (getf flight :state) "STALLED")
                (setf *ledger-changed* t))
               ((stall-recovered-p flight now stall)
                (setf (getf flight :state) "RUNNING")
                (setf *ledger-changed* t)))
         (when (and auto
                    (failed-state-p (getf flight :state))
                    (< (or (getf flight :requeues) 0) max-requeues)
                    (not (equal (getf flight :retried) (getf flight :ended)))
                    (stringp (getf flight :label))
                    (plusp (length (getf flight :label))))
           (push flight due)))
       *flights*))
    (dolist (flight (nreverse due)) (retry flight now))
    due))

(defun tick (&aux (now (get-universal-time)))
  "One pass of the sweeper: the queued frames, the states, the re-runs, and
the document on disk when anything moved."
  (drain-inbox now)
  (sweep now)
  (when *ledger-changed* (ignore-errors (save-store)))
  nil)

;;; --- the tools ---------------------------------------------------------------

(nlk:define-contract muster-status-arguments
  (:doc "The muster board: every subagent flight this image has run - what it was asked, how it stands (working, stuck, failing on repeat, gone, done), how many tool calls it made, and the one command that fits it. Read it when the operator asks what the helpers are doing, or before starting more work while others run.")
  ("rows" :integer :default 40 :doc "how many of the newest flights to list"))

(nlk:define-contract muster-digest-arguments
  (:doc "The foreman's report on the fleet: the four counts, and only the flights that need a decision, one line each with the command that answers it. Cheaper than the board when all you want to know is whether anything needs you.")
  ("unused" :string :default "" :doc "nothing; pass nothing"))

(defun status-tool (args)
  "The muster_status tool."
  (let ((rows (muster-status-arguments-rows args)))
    (let ((*board-limit* (and (integerp rows) (>= rows 1) rows)))
      (table))))

(defun digest-tool (args)
  "The muster_digest tool."
  (declare (ignore args))
  (digest))

;;; --- the command -------------------------------------------------------------

(defun short-of (flight)
  (let ((id (getf flight :id))) (subseq id 0 (min 8 (length id)))))

(defun set-state (flight state)
  (sb-thread:with-mutex (*ledger-lock*)
    (setf (getf flight :state) state)
    (setf *ledger-changed* t)))

(defun stop-flight (prefix)
  "Stop one flight's turn, and never claim more than the kernel agreed to."
  (let ((flight (find-flight prefix)))
    (cond
      ((null flight)
       (format nil "no worker called \"~a\". /muster shows the ids." prefix))
      ((eq flight :many)
       (format nil "\"~a\" names more than one worker; give more of the id." prefix))
      ((not (live-state-p (getf flight :state)))
       (format nil "\"~a\" already finished on its own." (short-of flight)))
      ((not (getf flight :turn))
       (format nil "\"~a\" has no turn to stop yet." (short-of flight)))
      (t
       (handler-case
           (progn
             (nlk:request-cancel-turn (getf flight :session) (getf flight :turn)
                                       "stopped from the muster board")
             (set-state flight "CANCEL_REQUESTED")
             (format nil "stopping \"~a\" - it finishes its current step, then quits."
                     (short-of flight)))
         (error (condition)
           (format nil "couldn't stop \"~a\" (~a). its row is unchanged."
                   (short-of flight) condition)))))))

(defun note-flight (prefix text)
  "Send a running flight a message, through the same ingress a shell's steer uses."
  (let ((flight (find-flight prefix)))
    (cond
      ((null flight)
       (format nil "no worker called \"~a\". /muster shows the ids." prefix))
      ((eq flight :many)
       (format nil "\"~a\" names more than one worker; give more of the id." prefix))
      ((not (live-state-p (getf flight :state)))
       (format nil "\"~a\" is not running any more." (short-of flight)))
      ((not (getf flight :turn))
       (format nil "\"~a\" has no turn to take a message yet." (short-of flight)))
      (t
       (handler-case
           (progn
             (nle:submit (getf flight :session) text
                         :steer (getf flight :turn) :source "muster")
             (format nil "message sent to \"~a\" - it reads it at its next step."
                     (short-of flight)))
         (error (condition)
           (format nil "it can't take messages right now (~a)." condition)))))))

(defun run-slash (raw session-id)
  "The /muster command: the board, or one verb against one flight."
  (declare (ignore session-id))
  (let* ((parts (nlk:split-words (or raw "")))
         (verb (string-downcase (or (first parts) ""))))
    (values
     (cond
       ((member verb '("help" "h" "?") :test #'string=) +primer+)
       ((and (member verb '("stop" "cancel") :test #'string=) (second parts))
        (stop-flight (second parts)))
       ((and (equal verb "note") (second parts) (third parts))
        (note-flight (second parts) (format nil "~{~a~^ ~}" (cddr parts))))
       (t (table)))
     nil)))

(defun run-digest (raw session-id)
  (declare (ignore raw session-id))
  (values (digest) nil))

;;; --- the entry ---------------------------------------------------------------

(defun install ()
  "Read the document, mark what the last image cut off, and start the sweep."
  (setf *running* nil
        *ledger-changed* nil
        *boot* (format nil "~d-~d" (get-universal-time) (random 1000000)))
  (sb-thread:with-mutex (*inbox-lock*) (setf *inbox* '()))
  (load-store)
  (reconcile-lost)
  (setf *running* t)
  (when *workers*
    (setf *worker* (nlk:worker-start "nodecode-muster" #'tick :wake 5)))
  (nle:on-stop
   (lambda ()
     (setf *running* nil)
     (when *worker* (setf *worker* (nlk:worker-stop *worker*)))
     (ignore-errors (save-store))
     (sb-thread:with-mutex (*ledger-lock*) (clrhash *flights*))
     (sb-thread:with-mutex (*inbox-lock*) (setf *inbox* '()))
     (setf *ledger-changed* nil))))

(nle:define-cell muster
  (:section ("muster")
    (:guide "nothing is required: the board fills itself as subagents run; raise stall_seconds if a quiet worker is normal here")
    ("stall_seconds" :integer :default 300 :min 30
                     :doc "a worker with no frame for this long reads stuck")
    ("spin_threshold" :integer :default 3 :min 1
                      :doc "how many failures of the same job read as failing on repeat")
    ("spin_window_seconds" :integer :default 1800 :min 60
                           :doc "how far back those failures are counted")
    ("show_goals" :boolean :default t
                  :doc "show each worker's job on the board; off keeps it off a shared screen")
    ("board_rows" :integer :default 40 :min 1
                  :doc "how many of the newest flights the board lists")
    ("auto_requeue" :boolean :default t
                    :doc "re-run a job that ended badly, in its own session")
    ("max_requeues" :integer :default 2 :min 0
                    :doc "how many times one job may be re-run")
    ("dir" :path :doc "where the ledger lives; muster/ under the home unless answered"))
  (:start #'install)
  (:help :muster "the board of subagent flights: working, stuck, failing on repeat or gone, with stop and a message" +primer+)
  (:hook :frame (nle:observer #'observe-frame "nodecode-muster"))
  (:tool "muster_status" (nlk:contract 'muster-status-arguments) 'status-tool)
  (:tool "muster_digest" (nlk:contract 'muster-digest-arguments) 'digest-tool)
  (:command "muster" #'run-slash
            :description "Who's still running: the board of subagent flights"
            :argument-hint "stop ID|note ID TEXT|help")
  (:command "muster-digest" #'run-digest
            :description "The foreman's report: counts and the decisions you owe"))
