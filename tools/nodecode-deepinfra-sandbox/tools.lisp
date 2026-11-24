;;;; tools.lisp --- the four tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original plugin gave no tool of its own: it swapped the agent's whole
;;;; terminal backend, so every shell and file call went to the sandbox. A cell
;;;; cannot take the core's shell away, so the sandbox gets verbs of its own --
;;;; one to run a command, one to read and one to write a file under
;;;; /workspace, and one for the sandbox's own lifecycle.
;;;;
;;;; A tool answers text: the model reads it, and the sandbox id, the paths
;;;; and the exit code in it are what the next call uses.

(in-package #:nodecode-deepinfra-sandbox)

;;; --- what an answer reads like -------------------------------------------------

(defun output-text (stdout stderr)
  "STDOUT and STDERR as one block the model reads, ending in a newline."
  (let ((output (concatenate 'string (or stdout "") (or stderr ""))))
    (cond ((zerop (length output)) "")
          ((member (char output (1- (length output))) '(#\Newline #\Return)) output)
          (t (concatenate 'string output (string #\Newline))))))

(defun exec-text (command &key (timeout 60))
  "Run COMMAND in the sandbox and answer what the model reads: its output,
its exit code, or the service's own word for why there is none."
  (multiple-value-bind (stdout stderr exit problem)
      (exec-stream command :timeout timeout)
    (cond ((and problem (null exit))
           (format nil "ERROR: deepinfra sandbox: ~a~@[~%~a~]" problem (output-text stdout stderr)))
          (t (format nil "~a[exit ~a]" (output-text stdout stderr) exit)))))

(defun status-text ()
  "What the sandbox is, without dialling anything: the id this home holds,
and the state the last call saw."
  (let ((id (remembered-id)))
    (cond ((null id)
           "deepinfra sandbox: none yet in this home; the next command makes one, and /workspace in it is the only place a file survives a stop")
          (t (format nil "deepinfra sandbox ~a: ~a; /workspace keeps its files across a stop~@[, ~
                          and costs nothing while stopped~]"
                     id (or *sandbox-state* "state not read since this launch")
                     (equal *sandbox-state* "stopped"))))))

;;; --- deepinfra_run --------------------------------------------------------------

(nlk:define-contract run-call
  (:doc "Run one shell command in the DeepInfra cloud sandbox, as bash -c, and answer what it
printed and its exit code. It runs in an isolated Linux microVM, never on the user's machine:
nothing of theirs is there except what deepinfra_write put under /workspace, and the sandbox's
environment carries no key. A stopped sandbox is woken first, which takes a few seconds.")
  ("command" :text :required t
   :doc "the shell command; it runs in the sandbox, never on the user's machine")
  ("timeout_seconds" :integer :default 60
   :doc "how long the command may run, up to 1800; the service kills it after that"))

(defun tool-run (call)
  "deepinfra_run: one command in the sandbox."
  (exec-text (run-call-command call) :timeout (run-call-timeout-seconds call)))

;;; --- deepinfra_read -------------------------------------------------------------

(nlk:define-contract read-call
  (:doc "Read a file out of /workspace in the DeepInfra sandbox and answer its text. Only
/workspace survives a stop, so every file this cell treats is under it. Use deepinfra_run with a
shell command for anything outside it (a log in /tmp, the output of a build).")
  ("path" :text :required t :doc "the file, under /workspace, for example /workspace/report.txt"))

(defun tool-read (call)
  "deepinfra_read: one file's text."
  (read-remote (read-call-path call)))

;;; --- deepinfra_write ------------------------------------------------------------

(nlk:define-contract write-call
  (:doc "Write a file into /workspace in the DeepInfra sandbox, making its folders, and answer
how many bytes landed. It replaces the whole file. Nothing is written on the user's machine.")
  ("path" :text :required t :doc "where to write it, under /workspace, for example /workspace/app.py")
  ("content" :string :default "" :doc "the whole file's text"))

(defun tool-write (call)
  "deepinfra_write: one file, whole."
  (let ((bytes (write-remote (write-call-path call) (write-call-content call))))
    (format nil "wrote ~d bytes to ~a in the sandbox" bytes (write-call-path call))))

;;; --- deepinfra_sandbox ----------------------------------------------------------

(nlk:define-contract lifecycle-call
  (:doc "The DeepInfra sandbox this home drives: its state, or make, start, stop and delete it.
status dials nothing. A stopped sandbox keeps /workspace and is not billed; delete removes the
sandbox and its files for good, so delete only when the user asks for it.")
  ("action" :text :required t :options '("status" "create" "start" "stop" "delete")
   :doc "status reads the state this cell last saw; create makes a sandbox now; start resumes a stopped one; stop keeps /workspace and stops the meter; delete removes the sandbox and its files for good"))

(defun tool-lifecycle (call)
  "deepinfra_sandbox: the sandbox's state, or one lifecycle action."
  (let ((action (lifecycle-call-action call)))
    (cond ((string= action "status") (status-text))
          ((string= action "create")
           (let ((id (ensure-running)))
             (format nil "sandbox ~a is running~@[ on the ~a plan~@]; /workspace in it is where files ~
                          last across a stop"
                     id (ignore-errors (plan-of id)))))
          ((string= action "start")
           (let* ((id (remembered-id))
                  (state (and id (state-of id))))
             (cond ((null id)
                    (format nil "sandbox ~a is running, made just now" (ensure-running)))
                   ((equal state "running") (format nil "sandbox ~a is already running" id))
                   (t (post-action id "start")
                      (if (wait-for-running id)
                          (format nil "sandbox ~a is running again, /workspace as it was" id)
                          (format nil "sandbox ~a did not come back up; try deepinfra_sandbox with ~
                                       status" id))))))
          ((string= action "stop")
           (let ((id (remembered-id)))
             (unless id (fail "deepinfra sandbox: there is no sandbox to stop"))
             (stop-remote id)
             (setf *sandbox-state* "stopped")
             (format nil "sandbox ~a is stopped; /workspace is kept and the next command wakes it" id)))
          (t (let ((id (remembered-id)))
               (unless id (fail "deepinfra sandbox: there is no sandbox to delete"))
               (delete-remote id)
               (format nil "sandbox ~a is deleted, /workspace with it" id))))))

;;; --- the eval verb --------------------------------------------------------------

(define-verb exec (command &key (timeout 60))
  "Run COMMAND in the DeepInfra sandbox and answer what it printed, then its
exit code: (exec \"ls /workspace\"). It makes or wakes the sandbox first."
  (exec-text command :timeout timeout))

(define-verb sandbox ()
  "What the sandbox this home drives is: (sandbox) answers its id and state."
  (status-text))
