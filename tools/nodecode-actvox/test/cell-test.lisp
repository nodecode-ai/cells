;;;; cell-test.lisp --- lifecycle; the tool, the command and the turn's line, through a stubbed CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test needs the actvox CLI, an ActVox account or the network: every run
;;;; goes through NODECODE-ACTVOX::*RUN*, a stub that records the argv it was
;;;; handed and answers the fixtures below, one per subcommand. What is under
;;;; test is what this cell owns -- which items count as waiting, how each
;;;; answer reads, and that a turn's line is asked for once and stays silent
;;;; when ActVox cannot be read.

(in-package #:nodecode.test)

(define-test-slice "actvox" "ACTVOX-CELL-" :start nodecode-actvox:start-cell)

(define-cell-lifecycle-tests "actvox"
  (:config "cli_path" "actvox")
  (:hooks :round)
  (:help :actvox)
  (:command "actvox")
  (:running
   (is (nle::find-tool "actvox_inbox") "the inbox tool is registered"))
  (:stopped
   (is (null (nle::find-tool "actvox_inbox")) "and stop takes it back"))
  (:refused ("timeout_seconds" 0)
            ("timeout_seconds" "soon"))
  (:idle nodecode-actvox:actvox-error (nodecode-actvox:inbox-text)))

;;; --- the fixtures -----------------------------------------------------------------

(defparameter *actvox-inbox-json*
  "{\"ok\": true, \"data\": {\"workspaces\": [
     {\"workspaceId\": \"w1\", \"name\": \"Acme\", \"assignable\": true,
      \"mentions\": [{\"id\": \"m1\"},
                     {\"id\": \"m2\", \"acknowledgedAt\": \"2026-01-01T00:00:00Z\"}],
      \"assignments\": [
        {\"id\": \"t1\", \"role\": \"owner\", \"state\": \"assigned\", \"nextActions\": [\"ack\"]},
        {\"id\": \"t2\", \"role\": \"owner\", \"state\": \"claimed\", \"nextActions\": [\"resume\"],
         \"lease\": {\"expiresAt\": \"2999-01-01T00:00:00Z\"}},
        {\"id\": \"t3\", \"role\": \"reviewer\", \"state\": \"assigned\", \"nextActions\": [\"review\"]},
        {\"id\": \"t4\", \"role\": \"owner\", \"state\": \"done\", \"nextActions\": [\"view\"]}]},
     {\"workspaceId\": \"w2\", \"name\": \"Garage\", \"assignable\": false,
      \"assignments\": [{\"id\": \"t5\", \"role\": \"owner\", \"state\": \"assigned\", \"nextActions\": [\"ack\"]}]},
     {\"workspaceId\": \"w3\", \"name\": \"Locked\", \"error\": {\"code\": \"forbidden\"}}]}}"
  "The CLI's `inbox --all --json': one workspace with work, one that takes none, one that refused.")

(defparameter *actvox-status-json*
  "{\"ok\": true, \"data\": {\"authenticated\": true, \"profile\": \"worker\",
    \"workspaceIds\": [\"w1\", \"w2\"]}}"
  "The CLI's `auth status --json' for a working login.")

(defvar *actvox-answer* nil
  "SUBCOMMAND -> (OUTPUT . EXIT) the stubbed CLI answers, or NIL for a CLI that is not installed.")

(defvar *actvox-calls* nil
  "Every stubbed call this test made, newest first: (ARGV TIMEOUT).")

(defmacro with-actvox ((&rest pairs) &body body)
  "BODY with the cell started on a stubbed CLI: *ACTVOX-ANSWER* holds its fixtures."
  `(let ((*actvox-answer* nil)
         (*actvox-calls* nil)
         (nodecode-actvox::*run*
           (lambda (argv timeout)
             (push (list argv timeout) *actvox-calls*)
             (let ((answer (cdr (assoc (second argv) *actvox-answer* :test #'equal))))
               (if answer (values (car answer) (cdr answer)) (values "" 127))))))
     (with-cell-stop ((actvox-start "cli_path" "actvox" ,@pairs))
       ,@body)))

(defun actvox-tool-text (arguments)
  "The actvox_inbox tool called as a provider sends it, answered as text."
  (princ-to-string (nle::message-content (execute-wire-call "call-actvox" "actvox_inbox" arguments))))

(defun actvox-asked ()
  "The stubbed calls, oldest first."
  (reverse *actvox-calls*))

;;; --- the tool --------------------------------------------------------------------

(deftest actvox-cell-the-tool-lists-what-waits (with-actvox ()
  (setf *actvox-answer* (list (cons "inbox" (cons *actvox-inbox-json* 0))))
  (let ((text (actvox-tool-text "{}"))
        (one (actvox-tool-text "{\"workspace_id\": \"Acme\"}")))
    (is (search "Acme: 1 task, 1 mention to act on" text) "the workspace's count")
    (is (search "task t1 · assigned" text) "a task to start, by id and state")
    (is (search "mention m1" text) "an unanswered mention")
    (is (null (search "m2" text)) "an acknowledged mention is not waiting")
    (is (null (search "t2" text)) "a claimed task whose lease is alive is in progress")
    (is (null (search "t3" text)) "a reviewer's task is not the owner's")
    (is (null (search "t4" text)) "a done task is not waiting")
    (is (null (search "t5" text)) "a workspace that takes no tasks is passed over")
    (is (search "Garage: not assignable here" text) "and it says so")
    (is (search "Locked: error forbidden" text) "a workspace that refused is named")
    (is (search "mention m1" one) "one workspace is answered by name")
    (is (null (search "Locked" one)) "and the others are left out"))))

;;; --- the runner -------------------------------------------------------------------

(deftest actvox-cell-the-cli-runner-reads-output-and-exit ()
  (multiple-value-bind (output code) (nodecode-actvox::run-cli (list "echo" "ok") 10)
    (is (search "ok" output) "a program's output comes back")
    (is (= 0 code) "with its exit code"))
  (multiple-value-bind (output code)
      (nodecode-actvox::run-cli (list "nodecode-actvox-no-such-program") 5)
    (declare (ignore output))
    (is (= 127 code) "a program that is not installed answers 127, as a shell would")))

;;; --- the command ------------------------------------------------------------------

(deftest actvox-cell-the-command-reads-the-login (with-actvox ("profile" "worker")
  (setf *actvox-answer* (list (cons "auth" (cons *actvox-status-json* 0))))
  (let ((text (nodecode-actvox::run-command "status" nil))
        (argv (first (first (actvox-asked)))))
    (is (search "signed in as worker" text) "the profile the login reads with")
    (is (search "2 workspaces" text) "and how many workspaces it reaches")
    (is (equal '("actvox" "auth" "status" "--json" "--profile" "worker") argv) "the profile rides the arguments"))
  (setf *actvox-answer* (list (cons "auth" (cons "{\"ok\":true,\"data\":{\"authenticated\":false}}" 0))))
  (let ((text (nodecode-actvox::run-command "status" nil)))
    (is (search "actvox auth login --profile worker" text) "a missing login names the fix"))))

(deftest actvox-cell-bare-slash-answers-a-panel (with-actvox ()
  (setf *actvox-answer* (list (cons "inbox" (cons *actvox-inbox-json* 0))))
  (multiple-value-bind (text dialog) (nodecode-actvox::run-command "" nil)
    (is (search "Acme: 1 task" text) "the bare command reads the inbox")
    (is-present dialog "and offers the panel"
      (let ((rows (format nil "~a" dialog)))
        (is (search "/actvox inbox" rows) "whose first row runs the inbox"))))
  (let ((inbox (nodecode-actvox::run-command "inbox" nil))
        (unknown (nodecode-actvox::run-command "later" nil)))
    (is (string= (nodecode-actvox::inbox-text) inbox) "a bare inbox is the report alone")
    (is (search "Usage: /actvox [inbox|status]" unknown) "an unknown verb says how the command is used"))))

;;; --- a CLI that is not there, or answers nothing ---------------------------------------

(deftest actvox-cell-a-missing-cli-says-how-to-install (with-actvox ()
  (setf *actvox-answer* nil)
  (let ((text (actvox-tool-text "{}")))
    (is (search "the actvox CLI is not installed" text) "the tool names the state")
    (is (search "npm i -g @actvox/cli" text) "and the command that fixes it"))
  (is (null (nodecode-actvox::note-for (list :round 1))) "and the turn's line stays silent")))

(deftest actvox-cell-an-answer-that-is-not-json-says-so (with-actvox ()
  (setf *actvox-answer* (list (cons "inbox" (cons "welcome to actvox" 0))))
  (is (search "printed no JSON" (actvox-tool-text "{}")) "the exit code is named")
  (setf *actvox-answer*
        (list (cons "inbox" (cons "{\"ok\": false, \"error\": {\"message\": \"token expired\"}}" 0))))
  (is (search "token expired" (actvox-tool-text "{}")) "a refused read carries the CLI's own message")))

;;; --- the turn's first line ----------------------------------------------------------

(deftest actvox-cell-the-note-opens-a-turn-once (with-actvox ()
  (setf *actvox-answer* (list (cons "inbox" (cons *actvox-inbox-json* 0))))
  (let ((first (nodecode-actvox::note-for (list :round 1)))
        (again (nodecode-actvox::note-for (list :round 1))))
    (is (search "ActVox: 1 task and 1 mention wait for you" first) "what waits, in one line")
    (is (search "1 task and 1 mention wait for you" again) "a second round of the same turn reads the cache")
    (is (= 1 (length (actvox-asked))) "one read for the turn, however often it is asked")
    (is (null (nodecode-actvox::note-for (list :round 2))) "a later round carries nothing"))
  (setf nodecode-actvox::*cache* nil)
  (is (search "1 task and 1 mention" (nodecode-actvox::note-for (list :round 1))) "a cleared cache reads the inbox again")
  (is (= 2 (length (actvox-asked))) "and asks the CLI once more")))

(deftest actvox-cell-the-note-can-be-turned-off (with-actvox ("note" nil)
  (setf *actvox-answer* (list (cons "inbox" (cons *actvox-inbox-json* 0))))
  (is (null (nodecode-actvox::note-for (list :round 1))) "note false opens no turn with a line")
  (is (null (actvox-asked)) "and the CLI is not read at all")))
