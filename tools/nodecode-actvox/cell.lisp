;;;; cell.lisp --- the ActVox inbox, read through the person's own CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of ActVox's Hermes plugin. It reads the inbox with the `actvox'
;;;; CLI the person installs and signs in themselves -- `actvox inbox --all
;;;; --json' for the items, `actvox auth status --json' for the login -- and
;;;; answers it three ways: one line at the start of a turn when tasks or
;;;; mentions wait (the :ROUND point), a tool the model calls, and
;;;; /actvox inbox|status. This cell holds no credential, writes nothing to
;;;; ActVox, and speaks to no host of its own.

(defpackage #:nodecode-actvox
  (:use #:cl)
  (:nicknames #:actvox)
  (:export #:inbox #:status #:inbox-text #:status-text #:pending-items #:*primer*
           #:actvox-error))

(in-package #:nodecode-actvox)

(nlk:define-peripheral actvox :not-running t :lock "actvox")

;;; --- the words -------------------------------------------------------------------

(defparameter +primer+
  "actvox: what waits in your ActVox inbox -- tasks assigned to you and
comments that @mention you in a workspace.
  /actvox inbox    what waits now, a workspace a block, each task and mention
      by id and state. (actvox:inbox) answers the same from eval.
  /actvox status   the CLI, the login it reads with, and how many workspaces
      that login reaches.
The actvox_inbox tool answers the inbox to the model, optionally for one
workspace_id, so a turn may work the items it names. A turn's first model
request carries one line when something waits (the actvox section's note).
Everything is read, never written: claim, comment and submit in ActVox.
The items come from the person's own `actvox' CLI (npm i -g @actvox/cli,
then actvox auth login), so the cell holds no token; cli_path and profile
in the actvox section pick the binary and the login. (help :actvox) has
this while the cell runs."
  "What (help :actvox) answers while the cell runs.")

(defparameter +owner-states+ '("assigned" "acknowledged" "changes_requested" "claimed")
  "The task states in which the owner has work to start or resume: the server's
NEXT_ACTIONS table, as the original counted it.")

(defparameter +install+ "npm i -g @actvox/cli"
  "How the CLI the inbox is read with is installed.")

(defparameter *note-seconds* 5
  "The ceiling on the read a turn's first model request makes: a slow inbox
loses the line, never the turn.")

(defparameter *usage* "Usage: /actvox [inbox|status]")

;;; --- the CLI ----------------------------------------------------------------------

(defun run-cli (argv timeout)
  "Run ARGV with no shell of ours, its output and its errors to one scratch
file => (VALUES OUTPUT EXIT). EXIT is 127 for a program that is not
installed and 124 for one that outlived TIMEOUT seconds, as the timeout
program would say."
  (let* ((path (merge-pathnames (format nil "nodecode-actvox-~36r.out" (random most-positive-fixnum))
                                 (uiop:temporary-directory)))
         (process (ignore-errors
                   (sb-ext:run-program (first argv) (rest argv)
                                       :search t :wait nil :input nil
                                       :output path :error :output))))
    (if (null process)
        (values "" 127)
        (let ((deadline (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second)))
              (killed nil))
          (loop
            (cond ((not (sb-ext:process-alive-p process)) (return))
                  ((> (get-internal-real-time) deadline)
                   (setf killed t)
                   (ignore-errors (sb-ext:process-kill process 9))
                   (return))
                  (t (sleep 1/20))))
          (ignore-errors (sb-ext:process-wait process))
          (unwind-protect
               (values (if (probe-file path) (or (uiop:read-file-string path) "") "")
                       (if killed 124 (or (sb-ext:process-exit-code process) 1)))
            (ignore-errors (delete-file path)))))))

(defparameter *run* #'run-cli
  "How the cell runs the CLI: (funcall *RUN* ARGV TIMEOUT) => (VALUES OUTPUT EXIT).
A test binds its own, so no test needs the CLI.")

(defun cli-program ()
  "The CLI the section names, or the one on PATH."
  (let ((path (nlk:trimmed (or (setting :cli-path) ""))))
    (if (plusp (length path)) path "actvox")))

(defun cli-profile ()
  "The login profile the section names, or \"\" for the CLI's default."
  (nlk:trimmed (or (setting :profile) "")))

(defun cli-argv (&rest args)
  "ACTVOX ARGS ... --profile PROFILE, as a program and its arguments."
  (let ((profile (cli-profile)))
    (append (list (cli-program)) args
            (when (plusp (length profile)) (list "--profile" profile)))))

(defun login-command ()
  "The command that signs the CLI in."
  (format nil "~a auth login~@[ --profile ~a~]" (cli-program) (cli-profile)))

(defun cli-error (payload)
  "PAYLOAD's error object as one sentence."
  (let ((failure (nlk:json-value payload :object "error")))
    (or (nlk:json-value failure :string "message")
        (nlk:json-value failure :string "code")
        "no answer")))

(defun read-json (args action &key (timeout (setting :timeout-seconds)))
  "Run the CLI with ARGS and read its one JSON object => (VALUES PAYLOAD PROBLEM):
the object, or a sentence that names the fix."
  (multiple-value-bind (output code) (funcall *run* (apply #'cli-argv args) timeout)
    (let ((payload (and (plusp (length (or output "")))
                        (ignore-errors
                         (nlk:decode-json
                          (string-trim '(#\Space #\Tab #\Newline #\Return) output))))))
      (cond ((= code 127)
             (values nil (format nil "the actvox CLI is not installed: run ~a, or set cli_path in the ~
actvox section" +install+)))
            ((= code 124)
             (values nil (format nil "~a did not answer in ~d s" action timeout)))
            ((not (hash-table-p payload))
             (values nil (format nil "~a printed no JSON (exit ~d)" action code)))
            ((not (nlk:json-value payload :boolean "ok"))
             (values nil (format nil "~a failed: ~a" action (cli-error payload))))
            (t (values payload nil))))))

(defun read-inbox (&key (timeout (setting :timeout-seconds)))
  "The inbox as the CLI answers it => (VALUES PAYLOAD PROBLEM)."
  (read-json '("inbox" "--all" "--json") "actvox inbox" :timeout timeout))

(defun read-status (&key (timeout (setting :timeout-seconds)))
  "The CLI's login as it answers it => (VALUES PAYLOAD PROBLEM)."
  (read-json '("auth" "status" "--json") "actvox auth status" :timeout timeout))

;;; --- reading what waits ---------------------------------------------------------------

(defun records (container key)
  "CONTAINER's KEY array as a list, keeping its objects."
  (let ((array (nlk:json-value container :array key)))
    (if (vectorp array)
        (loop for item across array when (hash-table-p item) collect item)
        '())))

(defun json-false-p (object key)
  "Whether KEY is on OBJECT and is JSON false -- the CLI's `assignable: false'."
  (multiple-value-bind (value present) (nlk:json-value object :boolean key)
    (and present (not value))))

(defun item-key (item)
  "ITEM's key: its kind, its workspace and its id."
  (format nil "~a:~a:~a" (getf item :kind) (getf item :workspace-id) (getf item :id)))

(defun scan-workspaces (workspaces now)
  "=> (VALUES PENDING IN-PROGRESS UNREAD) over the CLI's data.workspaces.
PENDING is what the owner acts on now: every unacknowledged mention, and every
task in an owner state with a next action. IN-PROGRESS holds the keys of
claimed tasks whose lease is still alive. UNREAD holds the workspaces that
answered with an error or were skipped this tick."
  (let ((pending '()) (in-progress '()) (unread '()))
    (dolist (workspace workspaces)
      (let ((id (or (nlk:json-value workspace :string "workspaceId") "")))
        (cond
          ((or (nlk:json-value workspace :boolean "skipped")
               (nlk:json-value workspace :object "error"))
           (push id unread))
          ((json-false-p workspace "assignable") nil)
          (t
           (dolist (mention (records workspace "mentions"))
             (let ((mention-id (nlk:json-value mention :string "id")))
               (when (and mention-id (null (nlk:json-value mention :string "acknowledgedAt")))
                 (push (list :kind "mention" :id mention-id :workspace-id id) pending))))
           (dolist (task (records workspace "assignments"))
             (let ((task-id (nlk:json-value task :string "id"))
                   (state (nlk:json-value task :string "state"))
                   (next (nlk:json-value task :array "nextActions")))
               (when (and task-id
                          (equal "owner" (nlk:json-value task :string "role"))
                          (member state +owner-states+ :test #'equal)
                          next
                          (plusp (length next)))
                 (let ((item (list :kind "assignment" :id task-id :workspace-id id :state state))
                       (expires (nlk:iso-universal
                                 (nlk:json-value task :string "lease" "expiresAt"))))
                   (if (and (equal state "claimed") expires (> expires now))
                       (push (item-key item) in-progress)
                       (push item pending))))))))))
    (values (nreverse pending) (nreverse in-progress) (nreverse unread))))

(defun data-workspaces (payload)
  "PAYLOAD's data.workspaces, as objects."
  (records (nlk:json-value payload :object "data") "workspaces"))

(defun pending-items (payload now)
  "The items PAYLOAD says the owner acts on now."
  (nth-value 0 (scan-workspaces (data-workspaces payload) now)))

(defun plural (count one many)
  "COUNT with its word."
  (format nil "~d ~a" count (if (= count 1) one many)))

(defun note-text (items)
  "One line for a turn's first model request, or NIL when nothing waits."
  (when items
    (let* ((tasks (count "assignment" items :key (lambda (item) (getf item :kind)) :test #'equal))
           (mentions (- (length items) tasks))
           (parts (remove nil (list (when (plusp tasks) (plural tasks "task" "tasks"))
                                    (when (plusp mentions) (plural mentions "mention" "mentions")))))
           (verb (if (= 1 (length items)) "waits" "wait")))
      (format nil "ActVox: ~{~a~^ and ~} ~a for you. The actvox_inbox tool lists them; /actvox inbox too."
              parts verb))))

(defun inbox-report (payload now)
  "PAYLOAD's inbox as /actvox inbox and the tool print it: a workspace a block,
its tasks by id and state, then its mentions."
  (let ((lines '()))
    (dolist (workspace (data-workspaces payload))
      (let ((name (or (nlk:json-value workspace :string "name")
                      (nlk:json-value workspace :string "workspaceId")
                      "a workspace"))
            (failure (nlk:json-value workspace :object "error")))
        (cond
          (failure
           (push (format nil "~a: error ~a" name (or (nlk:json-value failure :string "code") "?"))
                 lines))
          ((json-false-p workspace "assignable")
           (push (format nil "~a: not assignable here" name) lines))
          (t
           (let ((items (pending-items payload now)))
             (setf items (remove-if-not (lambda (item) (equal (getf item :workspace-id)
                                                             (or (nlk:json-value workspace :string "workspaceId") "")))
                                        items))
             (let ((tasks (remove-if-not (lambda (item) (equal "assignment" (getf item :kind))) items))
                   (mentions (remove-if-not (lambda (item) (equal "mention" (getf item :kind))) items)))
               (push (format nil "~a: ~a, ~a to act on" name
                             (plural (length tasks) "task" "tasks")
                             (plural (length mentions) "mention" "mentions"))
                     lines)
               (dolist (task tasks)
                 (push (format nil "  task ~a · ~a" (getf task :id) (getf task :state)) lines))
               (dolist (mention mentions)
                 (push (format nil "  mention ~a" (getf mention :id)) lines))))))))
    (if lines
        (format nil "~{~a~^~%~}~%Work them in ActVox; the cell only reads." (nreverse lines))
        "ActVox: no workspace answered.")))

(defun one-workspace (payload id)
  "PAYLOAD's data with only the workspaces ID names, by id or by name."
  (let ((kept (remove-if-not (lambda (workspace)
                                (or (equal id (nlk:json-value workspace :string "workspaceId"))
                                    (equal id (nlk:json-value workspace :string "name"))))
                              (data-workspaces payload))))
    (nlk:make-json-object
     "data" (nlk:make-json-object "workspaces" (coerce kept 'vector)))))

(defun status-lines (payload)
  "PAYLOAD of `actvox auth status' as /actvox status prints it."
  (let ((data (nlk:json-value payload :object "data")))
    (if (nlk:json-value data :boolean "authenticated")
        (let ((profile (nlk:json-value data :string "profile"))
              (ids (nlk:json-value data :array "workspaceIds")))
          (format nil "ActVox: signed in~@[ as ~a~], ~a.~%  CLI: ~a~@[ (profile ~a)~]~%  What waits: /actvox inbox"
                  profile (plural (if (vectorp ids) (length ids) 0) "workspace" "workspaces")
                  (cli-program) (cli-profile)))
        (format nil "ActVox: the CLI is there but not signed in. Run: ~a" (login-command)))))

;;; --- the answer ----------------------------------------------------------------------

(defun inbox-text ()
  "What waits, as the tool and /actvox inbox answer it."
  (multiple-value-bind (payload problem) (read-inbox)
    (if payload
        (inbox-report payload (get-universal-time))
        (format nil "ActVox: not ready. ~a." problem))))

(defun status-text ()
  "The connection, as /actvox status answers it."
  (multiple-value-bind (payload problem) (read-status)
    (if payload
        (status-lines payload)
        (format nil "ActVox: not ready. ~a." problem))))

;;; --- the turn's first line ------------------------------------------------------------

(defvar *cache* nil
  "The last healthy inbox read the note reuses: (SECONDS . ITEMS).")

(defun cached-items ()
  "The pending items, read afresh when the cache is older than cache_seconds;
NIL while ActVox cannot be read, so a turn is never blocked or cluttered."
  (with-actvox-lock
    (let ((now (get-universal-time))
          (ttl (max 1 (setting :cache-seconds))))
      (if (and *cache* (< (- now (car *cache*)) ttl))
          (cdr *cache*)
          (multiple-value-bind (payload problem) (read-inbox :timeout (min *note-seconds*
                                                                         (setting :timeout-seconds)))
            (declare (ignore problem))
            (when payload
              (let ((items (pending-items payload now)))
                (setf *cache* (cons now items))
                items)))))))

(defun note-for (op)
  "The line OP's turn carries at its first model request, or NIL."
  (when (and (setting :note) (eql 1 (getf op :round)))
    (let ((items (cached-items)))
      (and items (note-text items)))))

(defun note (op)
  "The :ROUND hook: on a turn's first model request, one line saying what
waits, recorded as the round's reminder. NIL on every later round."
  (nlk:when-let (text (note-for op))
    (nlk:when-let (turn (getf op :turn))
      (nlk:record-turn-context-prompt turn :text text))))

;;; --- the tool and the command ----------------------------------------------------------

(nlk:define-contract inbox-call
  (:doc "What waits in the ActVox inbox: tasks assigned to the owner and comments that @mention
them, a workspace a block, each item by id and state. Read-only; the items are read through
the person's own actvox CLI login.")
  ("workspace_id" :string
                   :doc "one workspace by id or name; leave it out for every assignable workspace"))

(defun inbox-tool (call)
  "The actvox_inbox tool: what waits, answered as the report's text."
  (handler-case
      (multiple-value-bind (payload problem) (read-inbox)
        (if payload
            (let ((workspace (inbox-call-workspace-id call)))
              (inbox-report (if workspace (one-workspace payload workspace) payload)
                            (get-universal-time)))
            (nle:failure (format nil "ERROR: actvox: ~a." problem))))
    (error (condition) (nle:failure (format nil "ERROR: actvox: ~a" condition)))))

(defun panel (summary)
  "The dialog bare /actvox answers: the inbox, or the connection."
  (nle:list-dialog
   :title "ActVox" :summary summary
   :rows (list (nle:run-row "What waits" "/actvox inbox"
                             :detail "tasks assigned to you and mentions nobody answered")
               (nle:run-row "The connection" "/actvox status"
                             :detail "the CLI, the login it reads with, and its workspaces"))))

(defun run-command (args session-id)
  "/actvox: no argument is the inbox and its panel, inbox lists it, status reads
the login, anything else says how it is used."
  (declare (ignore session-id))
  (let ((verb (string-downcase (nlk:trimmed (or args "")))))
    (cond ((string= verb "")
           (let ((text (inbox-text)))
             (values text (panel (nlk:one-line text :cap 120)))))
          ((string= verb "inbox") (inbox-text))
          ((string= verb "status") (status-text))
          (t (values *usage* (panel *usage*))))))

;;; --- the declaration --------------------------------------------------------------------

(nle:define-cell actvox
  (:section ("actvox")
    (:guide "install the ActVox CLI (npm i -g @actvox/cli) and sign in once with actvox auth login; cli_path names the binary when it is not on PATH, profile picks the login it reads, note says whether a turn opens with what waits, and cache_seconds is how long one read is reused")
    ("cli_path" :string :default ""
                :doc "the actvox binary; empty finds it on PATH")
    ("profile" :string :default ""
               :doc "the CLI login profile to read with; empty uses the CLI's default")
    ("timeout_seconds" :integer :default 15 :min 1
                       :doc "one CLI read's ceiling in seconds; the turn's first line uses 5")
    ("cache_seconds" :integer :default 60 :min 1
                     :doc "how long the turn's line reuses one inbox read")
    ("note" :boolean :default t
            :doc "open a turn with one line when tasks or mentions wait"))
  (:tool "actvox_inbox" (nlk:contract 'inbox-call) 'inbox-tool)
  (:command "actvox" 'run-command
            :catches 'actvox-error
            :description "ActVox: what waits in your inbox, and the connection"
            :argument-hint "[inbox|status]")
  (:help :actvox "actvox: what waits in your ActVox inbox - /actvox inbox, or (actvox:inbox)" +primer+)
  (:hook :round (nle:observer #'note "actvox"))
  (:start (lambda ()
            (with-actvox-lock (setf *cache* nil)))))
