;;;; cell.lisp --- the ando section, the poll, the prompt, the reply.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One worker asks Ando for the agent's messages every poll_seconds and
;;;; runs each one as a turn in the section's session, one at a time: the
;;;; turn's answer is (ando:reply "..."), which posts back to the
;;;; conversation the message came from and lets the worker move on. A turn
;;;; that stops without answering still says so there, once, as the
;;;; original does, so a person is never left with silence where an error
;;;; happened.

(in-package #:nodecode-ando)

(nlk:define-peripheral ando :not-running t :lock "ando" :recursive t)

(defvar *connection* nil
  "The running cell's ANDO-CONNECTION, or NIL while it is not started.")

(defvar *worker* nil
  "The poll thread while the cell runs, or NIL.")

(defvar *handled* (make-hash-table :test #'equal)
  "Event ids already answered, so a repeated inbox row is never run twice.")

(defvar *active* nil
  "The message the sweep is answering, a plist of :EVENT :CONVERSATION
:THREAD :MESSAGE, or NIL.")

(defvar *replied* nil
  "True once the active message got its answer.")

(defparameter +interrupted+
  "I stopped before I could finish replying to this. Send it again if you still need it, and check anything I may have started."
  "What a person reads when a turn ended without answering; the original
posts the same sentence.")

(defparameter +message-input+
  "Ando message from ~a in conversation ~a~@[ (a reply in thread ~a)~].~
~%~%Reply with (ando:reply \"...\"), once, in markdown. It posts to that Ando conversation as this agent - keep the answer for that workspace, not for a terminal.~
~%~%--- the message ---~%~a"
  "The line every turn opens with: who wrote, where, and the words themselves.")

(defparameter +primer+
  "Ando is a workspace the operator's agent was invited into: people there message it, and it answers in the
same conversation, as itself. The nodecode-ando cell polls Ando every few seconds and runs each message as a
turn in a Nodecode session.
  (ando:reply \"text\" &key to)
      Post TEXT, markdown, to the Ando conversation being answered; TO names another conversation id.
      Call it once while a message is answered: it is the answer, and it is the only thing posted.
An incoming message names the person's membership id, its conversation and, when it is a reply, its thread
root. Answer for that conversation: no terminal chatter, no local paths, no ids nobody asked for. Attachments
and voice notes are not carried yet; ask for a pasted line when one matters.
ERROR: ANDO-ERROR names the problem, e.g. that nothing is being answered right now."
  "What (help :ando) answers while the cell runs.")

;;; --- the connection ------------------------------------------------------------

(defun running-connection ()
  "The connection the section named at start, or a refusal naming the section."
  (running-settings)
  *connection*)

(defun ensure-session (session)
  "SESSION standing, in the operator's working directory, when this image has a
store; a session the store does not know still takes a prompt."
  (unless (or (not (nlk:store-open-p)) (nlk:session-exists-p session))
    (ignore-errors
      (nlk:standby-session :id session :cwd (namestring (uiop:getcwd))))))

;;; --- the reply -----------------------------------------------------------------

(defun message-id-of (answer)
  "The id Ando answers a posted message with, whichever key carries it."
  (or (nlk:json-value answer :string "message_id")
      (nlk:json-value answer :string "id")
      (nlk:json-value answer :string "data" "id")))

(defun post-reply (connection conversation thread text)
  "Post TEXT to CONVERSATION, as a reply to THREAD when there is one."
  (multiple-value-bind (tool arguments)
      (if thread
          (values "reply_to_message"
                  (nlk:make-json-object "message_id" thread "markdown_content" text))
          (values "send_message"
                  (nlk:make-json-object "conversation_id" conversation
                                         "markdown_content" text)))
    (let ((id (message-id-of (call-tool connection tool arguments))))
      (unless (and id (plusp (length id)))
        (fail "Ando did not confirm the message"))
      id)))

(define-verb reply (text &key to)
  "Post TEXT (markdown) to the Ando conversation being answered, or to TO.
Answers a line saying what was posted. ANDO-ERROR when nothing is being
answered and TO names nothing."
  (let* ((connection (running-connection))
         (active *active*)
         (conversation (or to (getf active :conversation)))
         (elsewhere (and to active (not (equal to (getf active :conversation)))))
         (thread (unless elsewhere (getf active :thread))))
    (unless (and text (plusp (length (string-trim '(#\Space #\Tab #\Newline) text))))
      (fail "An Ando reply carries text; say what to post"))
    (unless conversation
      (fail "No Ando conversation is being answered: pass :to with one of its conversation ids"))
    (let ((id (post-reply connection conversation thread text)))
      (when (and (not elsewhere) active)
        (setf *replied* t))
      (format nil "posted to Ando (~a): ~a" conversation id))))

;;; --- one message ---------------------------------------------------------------

(defun allowed-p (connection message conversation)
  "Whether the section lets this message through: its author, its conversation."
  (let ((author (nlk:json-value message :string "authorWorkspaceMembershipId"))
        (members (setting :allowed-members))
        (conversations (setting :allowed-conversations)))
    (and (or (null members) (member author members :test #'string=))
         (or (null conversations) (member conversation conversations :test #'string=)))))

(defun note-handled (event)
  "Remember EVENT was answered."
  (when (and event (hash-table-count *handled*) (> (hash-table-count *handled*) 2000))
    (clrhash *handled*))
  (when event (setf (gethash event *handled*) t)))

(defun submit-ask (message)
  "Put MESSAGE, a row of the sweep, into the session as a turn."
  (let* ((active *active*)
         (session (setting :session))
         (author (or (nlk:json-value message :string "author" "display_name")
                     (nlk:json-value message :string "authorWorkspaceMembershipId")
                     "a member"))
         (text (or (nlk:json-value message :string "content") "[an attachment]"))
         (prompt (format nil +message-input+ author (getf active :conversation)
                          (getf active :thread) text)))
    (ensure-session session)
    (nle:submit session prompt
                :command-id (format nil "ando:~a" (getf active :event))
                :source "ando" :source-id (getf active :conversation))))

(defun wait-for-reply ()
  "Wait, at most reply_timeout_seconds, for the turn to answer."
  (let ((deadline (+ (get-universal-time) (setting :reply-timeout-seconds))))
    (loop until *replied*
          until (> (get-universal-time) deadline)
          do (sleep 0.25))
    *replied*))

(defun answer-item (connection item)
  "Answer ITEM, one inbox row: => T when a turn ran for it."
  (let* ((event (nlk:json-value item :string "event_id"))
         (message-id (nlk:json-value item :string "message_id"))
         (conversation (nlk:json-value item :string "conversation_id")))
    (when (and event message-id conversation)
      (let* ((message (message-of connection message-id))
             (thread (or (nlk:json-value item :string "thread_root_message_id")
                         (nlk:json-value message :string "thread_root_id"))))
        (cond
          ((gethash event *handled*)
           (acknowledge connection item "read")
           nil)
          ((not (allowed-p connection message conversation))
           (note-handled event)
           (acknowledge connection item "read")
           nil)
          (t
           (let ((*active* (list :event event :conversation conversation
                                  :thread (unless (equal thread message-id) thread)
                                  :message message-id))
                 (*replied* nil))
             (submit-ask message)
             (unless (wait-for-reply)
               (ignore-errors
                 (post-reply connection conversation (getf *active* :thread)
                              +interrupted+)))
             (note-handled event)
             (acknowledge connection item "handled")
             t)))))))

(defun poll-step ()
  "One sweep of the inbox, one message answered at a time."
  (with-ando-lock
    (when *connection*
      (handler-case
          (dolist (item (sweep *connection*))
            (answer-item *connection* item))
        (ando-error (condition)
          (nle:notice (format nil "ando: ~a" (ando-error-detail condition))
                      :level :warning :key "nodecode-ando"))
        (error (condition)
          (nle:notice (format nil "ando: ~a" condition)
                      :level :warning :key "nodecode-ando")))))
  nil)

;;; --- what /ando says and does ----------------------------------------------------

(defun status-text ()
  "One line about the running connection, or the refusal Ando answered."
  (let ((connection (running-connection)))
    (handler-case
        (let ((identity (call-tool connection "get_current_identity"))
              (workspace (call-tool connection "get_workspace_info")))
          (format nil "ando: ~a, workspace ~a; polling every ~ds into session ~a"
                  (or (nlk:json-value identity :string "identity" "display_name")
                      (ando-connection-display-name connection)
                      "the invited agent")
                  (or (nlk:json-value workspace :string "workspace" "name")
                      (ando-connection-workspace-id connection))
                  (setting :poll-seconds) (setting :session)))
      (ando-error (condition)
        (format nil "ando: ~a" (ando-error-detail condition))))))

(defun run-command (args session-id)
  "The /ando command: status (bare), sweep, or connect (redeem the invitation again)."
  (declare (ignore session-id))
  (let* ((words (uiop:split-string (string-downcase (nlk:trimmed (or args "")))
                                    :separator '(#\Space)))
         (action (or (first words) "")))
    (cond
      ((member action '("" "status") :test #'string=) (status-text))
      ((string= action "sweep")
       (let ((connection (running-connection)))
         (format nil "ando: ~d message~:p answered"
                 (count t (mapcar (lambda (item) (answer-item connection item))
                                  (sweep connection))))))
      ((string= action "connect")
       (let* ((invite (setting :invite))
              (path (or (setting :credential-file) (default-credential-path))))
         (unless invite
           (fail "The ando section holds no invitation: paste a fresh one from Settings > Members"))
         (let ((connection (write-credential (redeem invite (setting :agent-name)) path)))
           (setf *connection* connection)
           (format nil "ando: reconnected as ~a; the poll carries on"
                   (or (ando-connection-display-name connection) "the invited agent")))))
      (t (format nil "ando: status, sweep or connect - not ~a" action)))))

;;; --- the declaration --------------------------------------------------------------

(nle:define-cell ando
  (:section ("ando")
    (:guide "paste the invitation link from the workspace's Invite members sheet and name the agent; the cell redeems it once, keeps the key in a mode-600 file and starts answering; poll_seconds is how often it asks Ando for messages")
    (:any-of "invite" "credential_file")
    ("invite" :secret
     :doc "the Ando invitation link, used once; the cell redeems it and writes the credential file")
    ("credential_file" :path
     :doc "the mode-600 file the redemption wrote (defaults to a private file in the Nodecode home)")
    ("agent_name" :string :default "Nodecode"
     :doc "what a new invitation registers this agent as; a reconnected agent keeps its own name")
    ("session" :string :default "ando"
     :doc "the Nodecode session incoming Ando messages run in")
    ("allowed_members" :list
     :doc "workspace membership ids that may drive this agent; empty admits everyone Ando lets in")
    ("allowed_conversations" :list
     :doc "conversation ids this agent answers in; empty answers every conversation the invitation named")
    ("poll_seconds" :integer :default 5 :min 2
     :doc "how often Ando is asked for messages")
    ("reply_timeout_seconds" :integer :default 300 :min 10
     :doc "how long one message's turn may run before the agent says it stopped"))
  (:help :ando "ando:reply answers an Ando workspace message the cell is answering" +primer+)
  (:command "ando" 'run-command
            :description "Ando: status, a sweep now, or redeem the invitation again"
            :argument-hint "STATUS | SWEEP | CONNECT")
  (:start (lambda ()
            (setf *handled* (make-hash-table :test #'equal))
            (let ((connection (connect-from-settings)))
              (setf *connection* connection)
              (setf *worker* (nlk:worker-start "ando" #'poll-step
                                                :wake (lambda () (setting :poll-seconds))))
              (nle:on-stop (lambda ()
                             (when *worker* (nlk:worker-stop *worker*))
                             (setf *worker* nil *connection* nil
                                   *active* nil *replied* nil)))
              (nle:notice
               (format nil "ando: answering as ~a, polling every ~ds"
                       (or (ando-connection-display-name connection) "the invited agent")
                       (setting :poll-seconds))
               :key "nodecode-ando")
              (nle:on-stop (lambda () (nle:notice nil :key "nodecode-ando")))))))
