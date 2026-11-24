;;;; cell.lisp --- the primer, the section, the three tools, /beam, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :beam) answers the primer below, and every session's help section
;;;; carries one line naming it. The operator checks the key with /beam.

(in-package #:nodecode-beam)

;;; --- the manual -----------------------------------------------------------------

(defparameter +room-actions+
  '("agent_status" "agent_connect" "list" "create" "show" "members" "channels"
    "channel_create" "grant_list" "grant_put" "publish" "invite" "join")
  "What beam_rooms can do: the original's ACTIONS, its media_publish and its
listeners left to a later cell.")

(defparameter +primer+
  "Beam (b1m.ai) moves objects between S3, Cloudflare R2, S3-compatible stores,
Hippius and Hugging Face, and carries messages between agents in Beam Rooms.
The nodecode-beam cell gives three tools, and every one answers text:
  beam_account     check the Beam API key the section's variable holds: account status,
      user type, the key's role and its prefix (never the key).
  beam_transfers   action \"list\" (limit, offset, status, order_by), \"show\" (id) or
      \"cancel\" (id, confirmed true: cancelling is the user's call, so ask first).
  beam_rooms       the beam CLI's Room verbs: action agent_status, agent_connect,
      list, create (1 credit, confirmed true), show, members, channels, channel_create,
      grant_list, grant_put, publish, invite, join -- with room, channel, name, kind,
      message, subject_type, subject, actions, invitation_file and so on.
Transfers spend the organization's credits and a Room create costs one, so estimate
and confirm with the operator before one. Rooms need the beam CLI installed on this
machine and enrolled (beam_rooms action \"agent_connect\"); transfers read and
cancel go straight to Beam's API. The credentials a transfer needs are the storage
account's own and live outside this cell."
  "What (help :beam) answers while the cell runs.")

;;; --- arguments --------------------------------------------------------------------

(defun argument (args name)
  "ARGS' NAME, trimmed, or NIL when it is absent or blank."
  (let ((value (gethash name args)))
    (when (stringp value)
      (let ((value (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
        (and (plusp (length value)) value)))))

(defun ident-argument (args name &key required)
  "ARGS' NAME as a Room, channel or member id: never an option, never a control character."
  (let ((value (argument args name)))
    (when (and (null value) required)
      (fail "`~a` is required for this action." name))
    (when (and value (or (char= #\- (char value 0))
                         (some (lambda (character) (< (char-code character) 32)) value)
                         (> (length value) 256)))
      (fail "`~a` is not a valid id." name))
    value))

(defun integer-argument (args name default minimum maximum)
  "ARGS' NAME as an integer in MINIMUM..MAXIMUM, DEFAULT when it is absent."
  (let ((value (gethash name args)))
    (cond ((null value) default)
          ((not (and (numberp value) (= value (round value))))
           (fail "`~a` must be an integer." name))
          ((or (< value minimum) (> value maximum))
           (fail "`~a` must be between ~d and ~d." name minimum maximum))
          (t (round value)))))

(defun action-argument (args allowed &key default)
  "ARGS' action, one of ALLOWED, DEFAULT when it is absent."
  (let ((value (argument args "action")))
    (cond ((null value) (or default (fail "`action` is required.")))
          ((member value allowed :test #'string=) value)
          (t (fail "`action` must be one of ~{~a~^, ~}." allowed)))))

(defun confirmed-argument (args what)
  "Whether the operator approved WHAT: ARGS' confirmed, or a refusal asking for it."
  (or (gethash "confirmed" args)
      (fail "Ask the operator first, then call again with confirmed true: ~a." what)))

(defun member-of (value allowed what)
  "VALUE, when it is one of ALLOWED, else a refusal naming WHAT."
  (when (and value (not (member value allowed :test #'string=)))
    (fail "`~a` must be one of ~{~a~^, ~}." what allowed))
  value)

(defun action-list (text)
  "TEXT, comma-separated grant actions, checked against what a grant may allow."
  (let ((actions (remove "" (uiop:split-string text :separator ",")
                         :test #'string=)))
    (dolist (action actions)
      (member-of (string-trim '(#\Space) action) +grant-actions+ "actions"))
    (format nil "~{~a~^,~}" actions)))

(defun cli-flags (&rest pairs)
  "PAIRS, (NAME VALUE) ..., as --NAME=VALUE arguments; a NIL value is left out."
  (loop for (name value) in pairs
        when (and value (not (equal value "")))
          collect (format nil "--~a=~a" name value)))

;;; --- the account -------------------------------------------------------------------

(defun tool-account (arguments)
  "beam_account: the key the section names, checked against Beam."
  (declare (ignore arguments))
  (let ((account (call-api "/auth/me")))
    (format nil "Beam key ~a: ~a; user type ~a; key role ~a. Console: ~a"
            (or (text-field account "current_key_prefix") "(no prefix)")
            (or (text-field account "status") "unknown")
            (or (text-field account "user_type") "unknown")
            (or (text-field account "current_key_role") "unknown")
            (setting :console-url))))

;;; --- transfers ---------------------------------------------------------------------

(defun transfers-of (data)
  "DATA, an object carrying the transfers or the array itself, as a list."
  (let ((items (if (hash-table-p data) (field data "transfers") data)))
    (cond ((vectorp items) (coerce items 'list))
          ((listp items) items)
          (t '()))))

(defun transfers-text (data limit offset)
  "The list DATA as text, LIMIT and OFFSET said so a next page can be asked for."
  (let ((transfers (transfers-of data)))
    (if (null transfers)
        (format nil "No transfers~@[ at offset ~d~] in this organization." offset)
        (format nil "~d transfer~:p (newest first, offset ~d):~%~{  ~a~^~%~}"
                (length transfers) (or offset 0)
                (mapcar #'transfer-line transfers)))))

(defun transfer-text (transfer)
  "One transfer, its fields and how far it has gone."
  (format nil "~a~@[ (~a)~]~%~{  ~a: ~a~^~%~}"
          (transfer-line transfer) (percentage transfer)
          (loop for name in +transfer-fields+
                for value = (field transfer name)
                when value append (list name value))))

(defun tool-transfers (args)
  "beam_transfers: the organization's transfers, one of them, or a cancel."
  (let ((action (action-argument args '("list" "show" "cancel") :default "list")))
    (cond
      ((string= action "list")
       (let* ((limit (integer-argument args "limit" 20 1 100))
              (offset (integer-argument args "offset" 0 0 1000000))
              (status (member-of (argument args "status") +list-statuses+ "status"))
              (order (member-of (argument args "order_by") +order-by+ "order_by")))
         (transfers-text (call-api "/transfers"
                                   :parameters (list (cons "limit" limit)
                                                     (cons "offset" offset)
                                                     (cons "status" status)
                                                     (cons "order_by" order)))
                         limit offset)))
      ((string= action "show")
       (let ((id (ident-argument args "id" :required t)))
         (transfer-text (call-api (format nil "/transfers/~a" (url-encode id))))))
      (t
       (let ((id (ident-argument args "id" :required t)))
         (confirmed-argument args "a cancelled transfer cannot be resumed")
         (let ((answer (call-api (format nil (setting :cancel-path) (url-encode id)) :method :post)))
           (format nil "cancel requested for ~a: ~a"
                   id (or (text-field answer "message") (text-field answer "status")
                          "Beam took the request."))))))))

;;; --- Rooms through the beam CLI ------------------------------------------------------

(defun safe-name (text)
  "TEXT with anything but letters, digits, dot, dash and underscore replaced."
  (map 'string (lambda (character)
                 (if (or (alphanumericp character) (find character "-._")) character #\_))
       text))

(defun invitation-file (room)
  "Where an invitation token is written: under this home, owner-only by the CLI."
  (let ((path (nlk:home (format nil "beam/rooms/~a-~d.token" (safe-name room) (get-universal-time)))))
    (ensure-directories-exist path)
    path))

(defun expanded-path (text)
  "TEXT as a path a program is given: ~ expanded, separators as this system takes them."
  (let ((text (substitute #\/ #\\ text)))
    (if (and (plusp (length text)) (char= #\~ (char text 0)))
        (namestring (merge-pathnames (subseq text (min 2 (length text)))
                                      (uiop:ensure-directory-pathname (user-homedir-pathname))))
        text)))

(defun room-arguments (action args)
  "ACTION's beam CLI arguments, from ARGS. => (VALUES ARGUMENTS INPUT)."
  (flet ((need (name)
           (or (argument args name) (fail "`~a` is required for ~a." name action)))
         (room-id () (ident-argument args "room" :required t))
         (channel-id () (ident-argument args "channel" :required t)))
    (let ((base '("--json" "--no-interactive")))
      (cond
        ((string= action "agent_status") (values (append base (list "agent" "status")) nil))
        ((string= action "agent_connect")
         (values (append base (list "agent" "connect") (cli-flags (list "label" (argument args "label")))) nil))
        ((string= action "list")
         (values (append base (list "room" "list")
                         (cli-flags (list "state" (member-of (argument args "state") +room-states+ "state"))))
                 nil))
        ((string= action "create")
         (confirmed-argument args "creating a Room costs 1 credit")
         (values (append base (list "room" "create"
                                    (format nil "--idempotency-key=~a"
                                            (or (argument args "idempotency_key")
                                                (format nil "nodecode-~d" (get-universal-time))))))
                 nil))
        ((string= action "show") (values (append base (list "room" (room-id) "show")) nil))
        ((string= action "members") (values (append base (list "room" (room-id) "member" "list")) nil))
        ((string= action "channels") (values (append base (list "room" (room-id) "channel" "list")) nil))
        ((string= action "channel_create")
         (values (append base (list "room" (room-id) "channel" "create")
                         (cli-flags (list "name" (need "name"))
                                    (list "kind" (or (member-of (argument args "kind")
                                                                 +channel-kinds+ "kind")
                                                     "message"))))
                 nil))
        ((string= action "grant_list")
         (values (append base (list "room" (room-id) "channel" (channel-id) "grant" "list")) nil))
        ((string= action "grant_put")
         (values (append base (list "room" (room-id) "channel" (channel-id) "grant" "put")
                         (cli-flags (list "subject-type" (or (argument args "subject_type") "member"))
                                    (list "subject" (need "subject"))
                                    (list "actions" (action-list (need "actions")))))
                 nil))
        ((string= action "publish")
         (values (append base (list "room" (room-id) "channel" (channel-id) "publish" "--stdin")
                         (cli-flags (list "content-type" (argument args "content_type"))
                                    (list "retention" (member-of (argument args "retention")
                                                                 '("none" "sender_local")
                                                                 "retention"))
                                    (list "idempotency-key" (argument args "idempotency_key"))))
                 (need "message")))
        ((string= action "invite")
         (values (append base (list "room" (room-id) "invite")
                         (cli-flags (list "max-uses" (integer-argument args "max_uses" 1 1 100))
                                    (list "ttl" (integer-argument args "ttl_seconds" 900 60 (* 7 86400)))
                                    (list "agent" (ident-argument args "agent_id"))
                                    (list "principal" (ident-argument args "principal_id"))))
                 nil))
        ((string= action "join")
         (values (append base (list "room" (room-id) "join"
                                    "--invitation-file" (expanded-path (need "invitation_file"))))
                 nil))
        (t (fail "`action` must be one of ~{~a~^, ~}." +room-actions+))))))

(defun tool-rooms (args)
  "beam_rooms: one Beam Room action through the beam CLI."
  (let* ((action (action-argument args +room-actions+))
         (room (ident-argument args "room"))
         (path (and (string= action "invite") room (invitation-file room))))
    (multiple-value-bind (arguments input) (room-arguments action args)
      (when path
        (setf arguments (append arguments (list "--out" (namestring path)))))
      (multiple-value-bind (output err code) (funcall *run* arguments input)
        (let ((rendered (render-output output)))
          (when (and path (zerop code))
            (setf rendered (format nil "invitation written to ~a~%~a" (namestring path) rendered)))
          (if (zerop code)
              (format nil "beam_rooms ~a~@[: ~a~]~%~a" action room rendered)
              (format nil "beam_rooms ~a failed (exit ~d)~@[: ~a~]~%~a"
                      action code room
                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   (if (and err (plusp (length err))) err rendered)))))))))

;;; --- /beam ---------------------------------------------------------------------------

(defun run-command (arguments session-id)
  "One /beam invocation: no argument checks the key, `transfers' lists recent ones."
  (declare (ignore session-id))
  (let ((word (string-downcase (nlk:trimmed (or arguments "")))))
    (cond ((member word '("" "account" "key") :test #'string=)
           (tool-account (make-hash-table :test #'equal)))
          ((member word '("transfers" "list") :test #'string=)
           (let ((args (make-hash-table :test #'equal)))
             (setf (gethash "action" args) "list" (gethash "limit" args) 10)
             (tool-transfers args)))
          (t (format nil "~s is not an argument /beam takes: try none (the key's account) or transfers."
                     word)))))

;;; --- the declaration -------------------------------------------------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type
                                                                      "description" doc))))
         "required" (coerce (mapcar #'first properties) 'vector))))

(defparameter *account-schema*
  (list :description "Check the Beam API key this cell holds: the account's status, its user type and the key's role and prefix. Never the key itself. Call it first when another Beam tool reports an auth error."
        :parameters (nlk:json-object "type" "object"
                                      "properties" (nlk:json-object)
                                      "required" (vector)))
  "beam_account.")

(defparameter *transfers-schema*
  (tool-schema
   "Beam's transfers for this organization: action \"list\" (limit 1-100, offset, status pending/in_progress/completed/failed/cancelled, order_by created_at/completed_at), \"show\" one (id), or \"cancel\" one (id and confirmed true, after asking the operator: a cancel cannot be resumed)."
   '(("action" "string" "list (the default), show or cancel.")))
  "beam_transfers.")

(defparameter *rooms-schema*
  (tool-schema
   "Operate Beam Rooms through the beam CLI installed on this machine: action agent_status, agent_connect (enroll this machine), list, create (costs 1 credit, needs confirmed true), show, members, channels, channel_create (name, kind), grant_list, grant_put (subject_type member/role, subject, actions), publish (room, channel, message), invite (writes a one-use token file; give the path to the invitee, never its contents) or join (invitation_file). Rooms actions need the CLI; nodecode-beam says so when it is not installed."
   '(("action" "string" "agent_status, agent_connect, list, create, show, members, channels, channel_create, grant_list, grant_put, publish, invite or join.")
     ("room" "string" "the Room id, for every action but agent_status, agent_connect and list.")
     ("channel" "string" "the channel id, for grant_list, grant_put and publish.")
     ("name" "string" "a Room channel's name, for channel_create.")
     ("kind" "string" "a channel's kind: message (the default), stream, datagram, request-reply, object or media.")
     ("message" "string" "what publish sends, at most 1 MiB.")
     ("subject_type" "string" "member (the default) or role, for grant_put.")
     ("subject" "string" "the member or role a grant goes to.")
     ("actions" "string" "a grant's actions, comma-separated: discover, subscribe, publish, manage.")
     ("invitation_file" "string" "the token file an invite wrote, for join.")
     ("confirmed" "boolean" "true once the operator approved a Room create, which costs 1 credit.")))
  "beam_rooms.")

(nle:define-cell beam
  (:section ("beam")
    (:guide "make a key at console.b1m.ai and put it in the variable api_key_env names; next step: /beam; a Room action also needs the beam CLI installed and this machine enrolled with beam_rooms action agent_connect")
    ("api_key_env" :string :default "BEAM_API_KEY"
     :doc "the environment variable holding the Beam API key (b1m_...); transfers and Room creation spend the organization's credits")
    ("api_url" :string :default "https://beamcore.b1m.ai"
     :doc "Beam's API root")
    ("console_url" :string :default "https://console.b1m.ai"
     :doc "where the operator makes a key and reads the account")
    ("cancel_path" :string :default "/transfers/~a/cancel"
     :doc "the route a cancel posts to, ~a being the transfer id, relative to api_url")
    ("beam_cli" :path
     :doc "the beam CLI the Room actions run; empty means beam on PATH")
    ("timeout_seconds" :integer :default 20 :min 1
     :doc "seconds one Beam API call may take")
    ("cli_timeout_seconds" :integer :default 120 :min 1
     :doc "seconds one beam CLI call may take, enrolling a machine's included"))
  (:help :beam "beam: the Beam account, its transfers, and its Rooms through the beam CLI" +primer+)
  (:command "beam" 'run-command
            :description "Check the Beam key, or list the recent transfers"
            :argument-hint "TRANSFERS")
  (:tool "beam_account" *account-schema* #'tool-account)
  (:tool "beam_transfers" *transfers-schema* #'tool-transfers)
  (:tool "beam_rooms" *rooms-schema* #'tool-rooms))
