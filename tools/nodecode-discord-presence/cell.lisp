;;;; cell.lisp --- the card, the three tools, /presence, the tick.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; While the cell runs, the operator's Discord profile carries a card: the
;;;; session line (or the workspace, or a line the operator asked for), the
;;;; activity the agent is on (Thinking, Running Terminal Command), the model
;;;; and the turn's token total, with the elapsed time beside them. It is sent
;;;; to the Discord desktop client on this machine over its own IPC socket and
;;;; nowhere else; the client is what shows it to other Discord users, so
;;;; stealth_mode and every hide switch in the section decide what a card says.
;;;;
;;;; Config, a top-level key:
;;;;   "discord-presence": {"client_id": "123456789012345678"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Rewritten from DarRahman's Hermes Agent plugin (see NOTICE): the live
;;;; activity line and its tool names, the session/model/token card, the
;;;; privacy switches, the git branch line, the reconnect-per-tick loop and the
;;;; clear on the way out.

(in-package #:nodecode-discord-presence)

;;; --- what a tool is called on the card -------------------------------------------

(defparameter *tool-labels*
  '(("sh" . "Running Terminal Command")
    ("eval" . "Running Lisp")
    ("read-file" . "Reading File")
    ("write-file" . "Writing File")
    ("edit" . "Editing Code")
    ("patch" . "Editing Code")
    ("look" . "Viewing Image")
    ("websearch" . "Searching the Web")
    ("web-fetch" . "Reading a Page")
    ("chrome" . "Driving the Browser")
    ("prs" . "Reviewing Pull Requests")
    ("cron" . "Scheduling a Task")
    ("goal" . "Working a Goal")
    ("team" . "Coordinating Sessions")
    ("context" . "Recalling Context")
    ("board" . "Reading the Board"))
  "The verbs worth naming on a card, by the name the model calls them.")

(defun tool-label (name)
  "NAME as the card says it: a known verb in words, anything else by name."
  (let ((known (cdr (assoc (string-downcase (or name "")) *tool-labels* :test #'string=))))
    (or known
        (format nil "Running ~a"
                (string-capitalize (substitute #\Space #\- (string-downcase (or name ""))))))))

;;; --- the state one card is made of ------------------------------------------------

(defvar *worker* nil
  "The tick thread while the cell runs, or NIL.")

(defvar *lock* (bt2:make-lock :name "discord presence")
  "Held across one publish, so a tool's own card and the tick's never
interleave two frames on one pipe.")

(defvar *transport* nil
  "The open IPC pipe while the Discord client answers, or NIL.")

(defvar *socket-name* nil
  "The socket *TRANSPORT* was opened on.")

(defvar *last-key* nil
  "The card last sent: an unchanged one is not sent again.")

(defvar *last-error* nil
  "Why the last publish failed, or NIL.")

(defvar *paused* nil
  "T while the operator asked the card off: nothing is published until it is
asked back on.")

(defvar *override* nil
  "A plist of :details and :state the operator or the model set, which win
over the automatic lines until cleared.")

(defvar *activity* nil
  "The label of the tool running now, or NIL between turns.")

(defvar *activity-until* 0
  "Universal time the activity label stops standing without a newer one.")

(defvar *last-tokens* 0
  "The newest turn's billable tokens, as its own rounds reported them.")

(defvar *started-at* 0
  "The Unix second this cell started, the card's elapsed clock.")

;;; --- small readers -----------------------------------------------------------------

(defun nonblank (text)
  "TEXT trimmed, or NIL when it says nothing."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun unix-now ()
  "Now as a Unix second, which is the clock Discord's start timestamp reads."
  (- (get-universal-time) 2208988800))

(defmacro with-presence-lock (&body body)
  "BODY with the one pipe to itself: a tool's own frame and the tick's never
interleave."
  `(bt2:with-lock-held (*lock*) ,@body))

(defun format-tokens (count)
  "COUNT in the compact form the card has room for."
  (cond ((>= count 1000000) (format nil "~,1fM" (/ count 1000000.0)))
        ((>= count 1000) (format nil "~,1fk" (/ count 1000.0)))
        (t (princ-to-string count))))

(defun model-line ()
  "The model the session runs on, or the section's own label, or NIL."
  (or (ignore-errors (nonblank (setting :model-label)))
      (let ((symbol (find-symbol "*MODEL*" "NODECODE.EVOLVED")))
        (and symbol (boundp symbol) (ignore-errors (symbol-value symbol))))))

(defun workspace-name ()
  "The folder the agent works in, by name."
  (let* ((cwd (ignore-errors (uiop:getcwd)))
         (parts (and cwd (pathname-directory cwd)))
         (name (and parts (car (last parts)))))
    (and name (stringp name) (plusp (length name)) name)))

(defun git-branch ()
  "The branch of the workspace's own repository, read from .git/HEAD."
  (let ((head (ignore-errors (merge-pathnames ".git/HEAD" (uiop:getcwd)))))
    (when (and head (probe-file head))
      (let ((text (ignore-errors
                    (nlk:trimmed (uiop:read-file-string head)))))
        (when (nonblank text)
          (if (uiop:string-prefix-p "ref: refs/heads/" text)
              (subseq text (length "ref: refs/heads/"))
              (subseq text 0 (min 7 (length text)))))))))

(defun activity-label ()
  "The tool running now, or NIL once the hold has passed."
  (when (and *activity* (< (get-universal-time) *activity-until*))
    *activity*))

(defun note-activity (label)
  "LABEL as what the agent is on now, and wake the tick."
  (setf *activity* label
        *activity-until* (+ (get-universal-time) (setting :hold-seconds)))
  (nlk:worker-poke *worker*))

(defun note-tokens ()
  "Keep the running turn's token total for the state line."
  (let ((turn (ignore-errors (nle:turn))))
    (when turn
      (setf *last-tokens* (+ (or (getf turn :input-tokens) 0)
                              (or (getf turn :output-tokens) 0)
                              (or (getf turn :reasoning-tokens) 0))))))

;;; --- the card ----------------------------------------------------------------------

(defun state-line (stealth)
  "The line under the details: branch, model and token total, in that order."
  (unless stealth
    (let ((parts '()))
      (when (setting :show-git-branch)
        (let ((branch (git-branch)))
          (when branch (push (nlk:clip branch 24) parts))))
      (when (setting :show-model)
        (let ((model (model-line)))
          (when model (push (nlk:clip model 48) parts))))
      (when (and (setting :show-tokens) (plusp *last-tokens*))
        (push (format nil "~a tokens" (format-tokens *last-tokens*)) parts))
      (when parts (format nil "~{~a~^ | ~}" (nreverse parts))))))

(defun compose-card ()
  "The card as Discord's activity members, before anything is sent."
  (let* ((stealth (setting :stealth-mode))
         (activity (and (not stealth) (setting :show-activity) (activity-label)))
         (mode (setting :session-title-mode))
         (title (if (equal mode "full")
                    (or (workspace-name) "Active Session")
                    "Active Session"))
         (details (cond (stealth nil)
                        ((getf *override* :details))
                        ((equal mode "hidden") nil)
                        (activity (format nil "[~a] ~a" activity title))
                        (t title)))
         (state (or (and (not stealth) (getf *override* :state))
                    (state-line stealth)))
         (template (or (nonblank (setting :large-text)) "Nodecode"))
         (large-text (if activity (format nil "~a - ~a" template activity) template)))
    (list :details (and details (nlk:clip details 128))
          :state (and state (nlk:clip state 128))
          :large-image (ignore-errors (nonblank (setting :large-image)))
          :large-text (nlk:clip large-text 128)
          :started *started-at*)))

;;; --- the pipe ----------------------------------------------------------------------

(defun drop-transport ()
  "Forget the pipe, closing it if it is open."
  (when *transport*
    (ignore-errors (funcall (tp-close *transport*))))
  (setf *transport* nil *socket-name* nil *last-key* nil)
  nil)

(defun ensure-transport ()
  "The open pipe, opening the first socket the Discord client answers on."
  (or *transport*
      (let ((client-id (setting :client-id)))
        (loop for name in (socket-candidates)
              for candidate = (funcall *open-transport* name)
              when candidate
                do (handler-case
                       (progn
                         (send-handshake candidate client-id)
                         (receive-frame candidate 1)
                         (setf *transport* candidate
                               *socket-name* name
                               *last-key* nil)
                         (return candidate))
                     (error ()
                       (ignore-errors (funcall (tp-close candidate)))))
              finally (fail "no Discord client answered beside this process (tried ~d socket names): is the Discord desktop app running?"
                            (length (socket-candidates)))))))

(defun send-card (card)
  "CARD as one SET_ACTIVITY frame."
  (send-command *transport* "SET_ACTIVITY"
                (nlk:json-object "pid" (getpid)
                                  "activity" (activity-object card))))

(defun send-clear ()
  "Ask the client to take the card down, when there is a pipe to ask on."
  (when *transport*
    (handler-case
        (send-command *transport* "SET_ACTIVITY"
                      (nlk:json-object "pid" (getpid) "activity" :null))
      (error () (drop-transport))))
  (setf *last-key* nil)
  t)

(defun publish ()
  "Send the card when it changed. => the card, or a PRESENCE-ERROR."
  (with-presence-lock
    (let* ((card (compose-card))
           (key (format nil "~s" card)))
      (unless (equal key *last-key*)
        (ensure-transport)
        (handler-case
            (progn (send-card card)
                   (setf *last-key* key
                         *last-error* nil))
          (error (condition)
            (drop-transport)
            (fail "~a" (nlk:one-line (princ-to-string condition))))))
      card)))

(defun tick ()
  "One pass of the worker: let the activity expire, then publish."
  (when (and *activity* (>= (get-universal-time) *activity-until*))
    (setf *activity* nil
          *last-key* nil))
  (unless *paused*
    (handler-case (publish)
      (discord-presence-error (condition)
        (setf *last-error* (discord-presence-error-detail condition)))
      (error (condition)
        (setf *last-error* (nlk:one-line (princ-to-string condition))))))
  nil)

;;; --- the verbs ----------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's text; a refusal becomes the call's failure, never an unhandled
error."
  `(handler-case (progn ,@body)
     (discord-presence-error (condition)
       (nle:failure (format nil "ERROR: ~a" (discord-presence-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Discord presence: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun show-presence (details &key state)
  "Put DETAILS and STATE on the card, whatever the automatic lines say."
  (setf *override* (list :details (nlk:clip details 128)
                          :state (and (nonblank state) (nlk:clip state 128)))
        *paused* nil)
  (let ((card (publish)))
    (format nil "showing on Discord: ~a~@[ | ~a~]"
            (or (getf card :details) "(no details line)")
            (getf card :state))))

(defun clear-presence (&key (pause t))
  "Take the card down, and keep it down until something asks for it again."
  (ensure-transport)
  (send-clear)
  (setf *paused* pause
        *override* nil)
  (if pause
      "the Discord card is down, and stays down until /presence on or presence_show"
      "the Discord card is down; the next change puts it back"))

(defun presence-status (&key (detail t))
  "What a card would say now, and whether one can be sent; DETAIL is the
long answer."
  (let* ((card (compose-card))
         (details (or (getf card :details) "(no details line)")))
   (unless detail
     (return-from presence-status
       (format nil "discord-presence: ~:[off~;up~], showing ~s" (not *paused*) details)))
   (format nil "discord-presence: ~:[the card is off (/presence on puts it back)~;updating every ~d s~]~@[~%  socket: ~a~]~%  card: ~a~@[ | ~a~]~@[~%  large text: ~a~]~@[~%  last error: ~a~]"
          (not *paused*)
          (ignore-errors (setting :update-interval))
          *socket-name*
          details
          (getf card :state)
          (getf card :large-text)
          *last-error*)))

;;; --- the tool bodies -----------------------------------------------------------

(defun tool-show (call)
  "The presence_show tool: a card of the model's own."
  (answering (show-presence (presence-show-details call)
                             :state (presence-show-state call))))

(defun tool-clear (call)
  "The presence_clear tool: the card down."
  (answering (clear-presence :pause (presence-clear-pause call))))

(defun tool-status (call)
  "The presence_status tool: the card as it stands."
  (answering (presence-status :detail (presence-status-detail call))))

;;; --- /presence ----------------------------------------------------------------------

(defun status-dialog (text)
  "The sheet /presence answers alone, with the one row that sets a line."
  (nle:list-dialog
   :title "Discord presence"
   :summary text
   :rows (list (nle:ask-row "Show a line of your own" "Line" "/presence"
                             :placeholder "what to show" :verb "show"))))

(defun run-slash (args session-id)
  "One /presence invocation: nothing to read the status, off to take the card
down, on to put it back, anything else as the line to show."
  (declare (ignore session-id))
  (let* ((argument (nlk:trimmed (or args "")))
         (word (string-downcase argument)))
    (cond ((or (zerop (length word)) (string= word "status"))
           (let ((text (answering (presence-status))))
             (values text (and (not (search "ERROR" text)) (status-dialog text)))))
          ((member word '("off" "stop" "hide" "clear" "down") :test #'string=)
           (answering (clear-presence :pause t)))
          ((member word '("on" "start" "show" "resume" "up") :test #'string=)
           (answering
             (setf *paused* nil
                   *override* nil
                   *last-key* nil)
             (publish)
             "the Discord card is back on"))
          (t (answering (show-presence argument))))))

;;; --- the hook ---------------------------------------------------------------------

(defun observe-tool (op next)
  "A :TOOL point hook: name the tool on the card and keep the turn's spend."
  (when (getf op :name)
    (note-activity (tool-label (getf op :name)))
    (note-tokens))
  (funcall next op))

;;; --- the manual ---------------------------------------------------------------------

(defparameter *manual*
  "Discord presence is available through the nodecode-discord-presence cell: the operator's
Discord profile shows a card while the agent works - the session line, the tool it is
running, the model and the turn's token total - sent to the Discord desktop app on this
machine, which is what shows it to other Discord users. Nothing leaves this machine but
through that client, and the section's privacy switches decide what a card says.
  presence_show    {details, state}
      Put a line of your own on the card: details is the first line, state the line under
      it (128 characters each). It stands until presence_clear or /presence off, and wins
      over the automatic lines. Use it for what a person watching would want to know: the
      task, the branch, a build.
  presence_clear   {pause}
      Take the card down. With pause true (the default) it stays down until
      presence_show or /presence on; with pause false the next change puts it back.
  presence_status  {}
      What a card says now, which socket answered, and why the last send failed.
A person can do the same from the shell with /presence (status), /presence off,
/presence on, or /presence <a line to show>."
  "What (help :discord-presence) answers while the cell runs.")

;;; --- the cell ------------------------------------------------------------------------

(nlk:define-contract presence-show
  (:doc "Put a card of your own on the operator's Discord profile, sent to the Discord desktop app on this machine: details is its first line, state the line under it. It replaces the automatic session line until presence_clear or /presence off.")
  ("details" :text :required t
             :doc "the card's first line, at most 128 characters")
  ("state" :text :default ""
           :doc "the line under it, at most 128 characters; empty keeps the automatic model and token line"))

(nlk:define-contract presence-clear
  (:doc "Take the Discord card down: the profile shows nothing from this agent until presence_show or /presence on.")
  ("pause" :boolean :default t
           :doc "keep it down until something asks for it again; false clears it once and lets the next change put it back"))

(nlk:define-contract presence-status
  (:doc "What the Discord card says now, which local socket answered, and why the last send failed, if it did.")
  ("detail" :boolean :default t
            :doc "the card's lines, the socket that answered and the last error; false for one line"))

(defun check-client-id (values table)
  "The section's own id must be a Discord application id; there is no
default, because the card's name and art are the operator's application."
  (declare (ignore table))
  (let ((id (nonblank (getf values :client-id))))
    (unless id
      (nlk:config-error
       (format nil "no client_id: the card is shown under a Discord application of the operator's own - create one at discord.com/developers/applications, copy its Application ID, and set discord-presence.client_id to it")))
    (unless (every #'digit-char-p id)
      (nlk:config-error
       (format nil "client_id ~s is not a Discord application id (the long number under Application ID)" id)))
    (setf (getf values :client-id) id)
    values))

(nle:define-cell discord-presence
  (:section ("discord-presence")
    (:guide "a Discord application id of the operator's own (discord.com/developers/applications, Application ID), the Discord desktop app running on this machine, and the privacy switches below decide what other Discord users see; next step: /presence to read the card")
    ("client_id" :string :required t
                 :doc "the Application ID of a Discord application of your own; its name and art are the card's")
    ("update_interval" :integer :default 3 :min 1
                       :doc "how often the card is re-sent, in seconds")
    ("hold_seconds" :integer :default 15 :min 1
                    :doc "how long a tool's activity line stands before the session line comes back")
    ("session_title_mode" :choice :default "generic"
                          :options '("generic" "full" "hidden")
                          :doc "generic says Active Session, full says the workspace folder's name, hidden says neither")
    ("show_activity" :boolean :default t
                     :doc "name the tool the agent is running, e.g. [Reading File]")
    ("show_model" :boolean :default t
                  :doc "put the model's name on the card")
    ("show_tokens" :boolean :default t
                   :doc "put the turn's token total on the card")
    ("model_label" :string
                   :doc "the model name to show, when the section should not read the organism's own")
    ("show_git_branch" :boolean :default nil
                       :doc "put the workspace repository's branch on the card")
    ("stealth_mode" :boolean :default nil
                    :doc "a card with no lines but the large text: nothing about what is being worked on")
    ("large_image" :string
                   :doc "an art asset key uploaded to your Discord application; empty for none")
    ("large_text" :string :default "Nodecode"
                  :doc "the words on the large image, the activity appended after them")
    ("socket_path" :string
                   :doc "an explicit Discord IPC socket path, when the client's own is somewhere unusual"))
  (:settings #'check-client-id)
  (:help :discord-presence
         "discord-presence: presence_show, presence_clear and presence_status put the agent's work on your Discord profile"
         *manual*)
  (:tool "presence_show" (nlk:contract 'presence-show) #'tool-show)
  (:tool "presence_clear" (nlk:contract 'presence-clear) #'tool-clear)
  (:tool "presence_status" (nlk:contract 'presence-status) #'tool-status)
  (:command "presence" 'run-slash
            :description "The card on your Discord profile: status, off, on, or a line to show"
            :argument-hint "on | off | a line to show")
  (:hook :tool #'observe-tool)
  (:start (lambda ()
            (setf *override* nil
                  *activity* nil
                  *activity-until* 0
                  *paused* nil
                  *last-key* nil
                  *last-error* nil
                  *last-tokens* 0
                  *transport* nil
                  *socket-name* nil
                  *started-at* (unix-now))
            (setf *worker* (nlk:worker-start "discord-presence" #'tick
                                              :wake (setting :update-interval)
                                              :prime t))
            (nle:on-stop
             (lambda ()
               (when *worker*
                 (setf *worker* (nlk:worker-stop *worker*)))
               (send-clear)
               (drop-transport)
               (setf *paused* nil
                     *override* nil
                     *activity* nil
                     *activity-until* 0
                     *last-key* nil
                     *last-tokens* 0
                     *started-at* 0)))))
)