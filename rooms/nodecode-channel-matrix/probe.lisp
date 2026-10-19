;;;; probe.lisp --- what the token can see, and how the operator gets one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The read-only half of onboarding: PROBE-CHANNEL (START-CHANNEL's twin;
;;;; NCK:PROBE is the door) resolves the token the way START-CHANNEL does,
;;;; asks the homeserver who the account is, which rooms it has joined --
;;;; their names, whether they are encrypted, who is in them -- and answers a
;;;; text, never the token, so room and user ids are picked by name. The
;;;; section declaration at the end is the adapter's one statement of what
;;;; channels.matrix is made of (NLK:DEFINE-SECTION): the kit's refusal, the
;;;; setup panel, the model's setup primer and the addons report all read it.

(in-package #:nodecode-channel-matrix)

(nlk:access (result execution))

(defparameter +probe-shown+ 30
  "How many rooms or people one probe line names before it counts the rest.")

(defun section-homeserver (section)
  "The homeserver's base address SECTION names, without a trailing slash."
  (string-right-trim "/" (or (config-string section "homeserver") "")))

(defun probe-executor (section executor)
  "The executor the account's token speaks through; EXECUTOR, the scripted
test seam, stands for it."
  (if executor
      (wrap-matrix-executor executor)
      (make-matrix-executor :homeserver (section-homeserver section)
                            :token (resolve-channel-secret section "token"))))

(defun room-state-plan (room type)
  (rest-plan "GET" (format nil "~a/state/~a/" (room-path room) (encode type))
             (format nil "room_~a" type) nil 15))

(defun probe-rooms (executor me)
  "(values ROOMS PEOPLE): the joined rooms as (ID LABEL ENCRYPTED-P), and
the people in them but the account itself as (ID . NAME)."
  (let ((joined (execute-plan executor (read-plan "/joined_rooms" "joined_rooms")))
        (people '()))
    (values
     (loop for room across (nlk:json-array (and (execution-ok-p joined) (execution-body joined))
                                           "joined_rooms")
           for index from 0
           while (< index +probe-shown+)
           collect (let* ((name (execute-plan executor (room-state-plan room "m.room.name")))
                          (crypt (execute-plan executor (room-state-plan room "m.room.encryption")))
                          (members (execute-plan executor (rest-plan "GET" (room-path room "joined_members")
                                                                     "joined_members" nil 15)))
                          (table (and (execution-ok-p members)
                                      (nlk:json-value (execution-body members) :object "joined")))
                          (others '()))
                     (when table
                       (maphash (lambda (id member)
                                  (unless (equal id me)
                                    (let ((who (cons id (or (nlk:json-value member :text "display_name") id))))
                                      (push who others)
                                      (pushnew who people :key #'car :test #'equal))))
                                table))
                     (list room
                           (or (and (execution-ok-p name)
                                    (nlk:json-value (execution-body name) :text "name"))
                               (and (= 1 (length others))
                                    (format nil "DM with ~a" (cdr (first others))))
                               "unnamed room")
                           (execution-ok-p crypt))))
     (sort people #'string< :key #'car))))

(defun probe-channel (section &key executor (timeout 15) &aux (lines '()))
  "What the token in SECTION can see, as lines: the account, the rooms it
has joined -- the encrypted ones said so -- and the people in them."
  ;; EXECUTOR overrides the live one (the scripted test seam). Read only. The
  ;; answer never carries the token (the executor redacts every failure text).
  (declare (ignore timeout))
  (flet ((say (control &rest args)
           (push (apply #'format nil control args) lines)))
    (let* ((bot (probe-executor section executor))
           (me (execute-plan bot (read-plan "/account/whoami" "whoami")))
           (id (and (execution-ok-p me) (nlk:json-value (execution-body me) :string "user_id"))))
      (if (not id)
          (say "token: ~a -- make the bot an account of its own and give it an access token: a personal access token on a server that runs MAS (matrix.org), or one /login on one that does not"
               (probe-failure me))
          (multiple-value-bind (rooms people) (probe-rooms bot id)
            (say "token: ok -- ~a~@[ (device ~a)~] on ~a" id
                 (nlk:json-value (execution-body me) :string "device_id")
                 (section-homeserver section))
            (say "~a" (if rooms
                          (format nil "rooms: ~{~a~^, ~}"
                                  (loop for (room label encrypted) in rooms
                                        collect (format nil "~a ~a~:[~; (encrypted: not served)~]"
                                                        label room encrypted)))
                          "rooms: none yet -- invite the bot into a room, or start a direct message with it, from an allowed user"))
            (when people
              (say "people: ~{~a~^, ~}"
                   (mapcar (lambda (pair) (format nil "~a ~a" (cdr pair) (car pair)))
                           (subseq people 0 (min (length people) +probe-shown+))))))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun probe-choices (section executor reader)
  "READER's (ID . LABEL) conses over what the token sees, refused in the
probe's words when it serves nothing."
  (let* ((bot (probe-executor section executor))
         (me (execute-plan bot (read-plan "/account/whoami" "whoami")))
         (id (and (execution-ok-p me) (nlk:json-value (execution-body me) :string "user_id"))))
    (unless id
      (config-error "token: ~a" (probe-failure me)))
    (multiple-value-call reader (probe-rooms bot id))))

(defun probe-channel-choices (section &key executor (timeout 15))
  "The rooms the account has joined as ((id . label) ...) -- what a setup
panel offers for allowed_rooms."
  (declare (ignore timeout))
  (probe-choices section executor
                 (lambda (rooms people)
                   (declare (ignore people))
                   (loop for (room label) in rooms collect (cons room label)))))

(defun probe-user-choices (section &key executor (timeout 15))
  "The people in those rooms as ((id . name) ...) -- what a setup panel
offers for allowed_users and owner."
  (declare (ignore timeout))
  (probe-choices section executor (lambda (rooms people) (declare (ignore rooms)) people)))

;;; The declaration. Types and constraints are what START-CHANNEL reads and
;;; RESOLVE-CHANNEL-SECRET / REQUIRE-NON-EMPTY-ALLOWLIST enforce; the kit
;;; refuses on SECTION-PROBLEMS before either runs, so the refusal, the panel
;;; and the primer say the same thing in the same words.
(nlk:define-section ("channels" "matrix")
  (:guide "Give the bot a Matrix account of its own, never your own. On a server that runs Matrix Authentication Service (matrix.org, Element Server Suite) create the account and give it a personal access token; on one that does not, log it in once with POST /_matrix/client/v3/login and keep the access_token it answers. Keep the token in a file (mode 600) or an environment variable, never in the config. Invite the bot into a room, or start a direct message with it, from an allowed user: it joins on its own. Rooms with end-to-end encryption are not served yet: in Element, turn encryption off when you create the room (a direct message from Element is encrypted, so create a room instead).")
  (:check #'probe-channel)
  (:one-of "token_env" "token_file")
  (:any-of "allowed_rooms" "allowed_users")
  ("homeserver" :string :required t :doc "the homeserver's address, e.g. https://matrix.org")
  ("token_env" :env :doc "environment variable holding the bot account's access token")
  ("token_file" :path :doc "file holding the bot account's access token")
  ("allowed_rooms" :list :doc "room ids (!...:server) the bot answers in"
                   :choices #'probe-channel-choices)
  ("allowed_users" :list :doc "user ids (@name:server) allowed to drive the bot; their invites are joined"
                   :choices #'probe-user-choices)
  ("owner" :list :doc "the operator's user id, whose word is standing policy in the room"
           :choices #'probe-user-choices)
  ("require_mention" :boolean :default t
                     :doc "in rooms, answer when mentioned or in a thread the bot answers in; a direct message always answers")
  ("free_response_rooms" :list :choices #'probe-channel-choices
                         :doc "room ids where every allowed message may open a turn")
  ("ignored_rooms" :list :choices #'probe-channel-choices
                   :doc "room ids the bot reads neither as prompts nor as context")
  ("dm_policy" :choice :options '("allow" "disabled") :default "allow"
               :doc "direct messages")
  ("allow_bots" :choice :options '("none" "mentions" "all") :default "none"
                :doc "messages sent as m.notice, the way bots speak: none, those that mention the bot, or all")
  ("reactions" :boolean :default :false
               :doc "mark each ask on the message itself: 👀 from admission through the working turn, cleared when the turn ends")
  ("status_update_ms" :integer :default 6000
                      :doc "the fewest milliseconds between two edits of a turn's card; a homeserver allows a burst of edits, then about one every five seconds")
  ("stream" :boolean :default :false
            :doc "edit the answer into place as the model writes it; off by default, since every edit is an event in the room's history")
  ("turn_budget_minutes" :integer :default 0
                         :doc "minutes an ask's turn may run: past them its tool calls are refused and it answers with what it has; 0, the default, caps nothing")
  ("soul_file" :path :doc "a SOUL.md whose text is the channel's standing persona"))
