;;;; setup-card.lisp --- /channels setup: the owner's card of Discord's own pickers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Where the bot answers and who may use it, changed from Discord itself, on
;;;; any device: the bot's owner runs /channels setup in a channel of the
;;;; server and gets a card only they see, holding Discord's own channel,
;;;; user and role pickers (CONTROL-PICKER), an All channels button, and
;;;; Save and Cancel. A pick is held (*SETUP-DRAFTS*) and the card redrawn
;;;; with it; Save writes the members a pick changed into channels.discord
;;;; and takes the section into the running lane's admission at once
;;;; (SECTION-POLICY), no restart. The kit draws the command and gates it
;;;; (NCK::SETUP-COMMAND); a press says its line back as `/channels setup
;;;; VERB ID...' (PICKED-LINE), so typing one does what pressing does.

(in-package #:nodecode-channel-discord)

(defparameter +setup-draft-ms+ (* 30 60 1000)
  "How long a card's held picks outlive their last touch: Discord answers a
card's press for fifteen minutes after its interaction, and a card is opened
again with the command.")

(defvar *setup-drafts* (make-hash-table :test #'equal :synchronized t)
  "(user id . guild id) -> what that person's setup card holds until Save, a
plist: :CHANNELS :USERS :ROLES the picked ids, :TOUCHED the members a pick
changed, :ALL whether All channels was pressed last, :TEXT the server's text
channels as ((ID . NAME) ...), :ROLE-IDS its roles, :AT when it was touched.")

(defvar *application-owners* (make-hash-table :test #'eq :synchronized t :weakness :key)
  "Host -> the user ids its application names as owner: the owner, or every
member of the team that owns it (GET /applications/@me).")

(defun setup-get (host path label)
  "The decoded body of GET PATH through HOST's executor, or NIL when it failed."
  (let ((result (execute-plan (host-executor host) (rest-plan "GET" path label nil 15))))
    (and (execution-ok-p result) (execution-body result))))

(defun application-owners (host)
  "The user ids HOST's application names its owners, read once: the owner,
or the members of the team that owns it."
  (or (gethash host *application-owners*)
      (let* ((app (setup-get host "/applications/@me" "setup_application"))
             (team (and (hash-table-p app) (nlk:json-value app :object "team")))
             (ids (if team
                      (loop for member across (or (nlk:json-value team :array "members") #())
                            for id = (nlk:json-value member :string "user" "id")
                            when id collect id)
                      (nlk:when-let (id (and (hash-table-p app) (nlk:json-value app :string "owner" "id")))
                        (list id)))))
        (when ids (setf (gethash host *application-owners*) ids)))))

(defun application-owner-p (host user-id)
  "Whether USER-ID owns HOST's application (APPLICATION-OWNERS)."
  (and (member user-id (application-owners host) :test #'equal) t))

(defun discord-section ()
  "channels.discord as config.jsonc says it now, or NIL."
  (nlk:json-value (nle:read-shared-config) :object "channels" "discord"))

(defun section-ids (section name)
  "The ids the list member NAME of SECTION holds, in order."
  (let ((value (and (hash-table-p section) (gethash name section))))
    (and (vectorp value) (not (stringp value))
         (loop for id across value when (and (stringp id) (plusp (length id))) collect id))))

(defun setup-open (host key guild)
  "A fresh draft for KEY from the section as it is, over GUILD's text
channels and roles; NIL when the server could not be read."
  (let ((channels (setup-get host (format nil "/guilds/~a/channels" guild) "setup_channels"))
        (roles (setup-get host (format nil "/guilds/~a/roles" guild) "setup_roles"))
        (section (discord-section)))
    (when (and (vectorp channels) (vectorp roles))
      (let* ((text (loop for channel across channels
                         when (member (nlk:json-value channel :integer "type") +probed-channel-types+)
                           collect (cons (nlk:json-value channel :string "id")
                                         (nlk:json-value channel :string "name"))))
             (role-ids (loop for role across roles
                             for id = (nlk:json-value role :string "id")
                             ;; @everyone is the guild's own id, and a managed
                             ;; role is a bot's or a booster's, held by no one
                             ;; a pick could mean
                             ;; JSON false decodes to :FALSE, so only true is managed
                             unless (or (equal id guild) (eq t (nlk:json-value role :any "managed")))
                               collect id))
             (draft (list :channels (intersection-in-order (section-ids section "allowed_channels")
                                                           (mapcar #'car text))
                          :users (section-ids section "allowed_users")
                          :roles (intersection-in-order (section-ids section "allowed_roles") role-ids)
                          :touched '() :all nil :text text :role-ids role-ids
                          :at (get-universal-time))))
        (setf (gethash key *setup-drafts*) draft)))))

(defun intersection-in-order (ids among)
  "The members of IDS that AMONG holds, in IDS's order."
  (remove-if-not (lambda (id) (member id among :test #'equal)) ids))

(defun setup-draft (key)
  "KEY's held draft while it is fresh, else NIL (and forgotten)."
  (let ((draft (gethash key *setup-drafts*)))
    (cond ((null draft) nil)
          ((> (* 1000 (- (get-universal-time) (getf draft :at))) +setup-draft-ms+)
           (remhash key *setup-drafts*)
           nil)
          (t draft))))

(defun setup-pick (key member ids &key all)
  "Hold IDS as KEY's draft's MEMBER (:CHANNELS :USERS :ROLES), ALL saying the
All channels button picked them. => the draft, or NIL when it expired."
  (nlk:when-let (draft (setup-draft key))
    (setf (gethash key *setup-drafts*)
          (list* member (remove-duplicates ids :test #'equal :from-end t)
                 :touched (adjoin member (getf draft :touched))
                 ;; a channel pick says whether it was All; any other keeps it
                 :all (if (eq member :channels) all (getf draft :all))
                 :at (get-universal-time)
                 (alexandria:remove-from-plist draft member :touched :all :at)))))

(defun said-ids (ids form &key (none "none yet") (cap 10))
  "IDS as Discord draws mentions of them, through FORM (\"<#~a>\"), the
first CAP of them and how many more; NONE for none."
  (if ids
      (format nil "~{~a~^, ~}~:[~;, +~d more~]"
              (mapcar (lambda (id) (format nil form id)) (subseq ids 0 (min cap (length ids))))
              (> (length ids) cap) (- (length ids) cap))
      none))

(defun setup-fields (draft)
  "DRAFT's picks as the card's fields: where it answers, who may use it."
  (let ((channels (getf draft :channels)))
    (list (cons "Channels" (if (and (getf draft :all) channels)
                               (format nil "all ~d text channels" (length channels))
                               (said-ids channels "<#~a>" :none "none in this server")))
          (cons "People" (said-ids (getf draft :users) "<@~a>" :none "only the owner"))
          (cons "Roles" (said-ids (getf draft :roles) "<@&~a>" :none "none")))))

(defun setup-view (key &key notice)
  "The card for KEY's draft, offered beside its words (OFFER-CARD), NOTICE
said over it. => the words."
  (let* ((draft (setup-draft key))
         (text (getf draft :text)))
    (offer-card
     :panel (list :title "Where should I answer, and for whom?"
                  :context "Only you see this card. Nothing changes until Save."
                  :notice notice
                  :fields (setup-fields draft)
                  :tone :working)
     :controls (list (nck::picker :channel "Pick the channels I answer in" "/channels setup channels"
                                  :current (getf draft :channels))
                     (list (choice (format nil "All ~d channels" (length text)) "/channels setup all"))
                     (nck::picker :user "Who else may use me" "/channels setup users"
                                  :current (getf draft :users))
                     (nck::picker :role "Roles that may use me" "/channels setup roles"
                                  :current (getf draft :roles))
                     (list (choice "Save" "/channels setup save" :style :primary)
                           (choice "Cancel" "/channels setup cancel"))))
    (format nil "Setup: ~{~a: ~a~^; ~}. /channels setup save keeps it."
            (loop for (name . value) in (setup-fields draft) append (list name value)))))

(defun setup-closed (title text tone)
  "The card closed as TITLE over TEXT in TONE, its pickers gone. => TEXT."
  (offer-card :panel (list :title title :text text :tone tone))
  text)

(defun setup-save (host key)
  "Write the members KEY's draft changed into channels.discord and take the
section into HOST's admission live. => what the card says."
  (let* ((draft (setup-draft key))
         (touched (getf draft :touched))
         (section (discord-section))
         (text-ids (mapcar #'car (getf draft :text)))
         (channels (if (member :channels touched)
                       ;; this server's text channels are the card's; every
                       ;; other id -- another server's, a forum, a thread -- stays
                       (append (set-difference-in-order (section-ids section "allowed_channels") text-ids)
                               (getf draft :channels))
                       (section-ids section "allowed_channels")))
         (users (if (member :users touched) (getf draft :users) (section-ids section "allowed_users")))
         (roles (if (member :roles touched)
                    (append (set-difference-in-order (section-ids section "allowed_roles") (getf draft :role-ids))
                            (getf draft :roles))
                    (section-ids section "allowed_roles"))))
    (cond ((null draft)
           (setup-closed "This card has expired" "This card has expired: run /channels setup again to pick." :stopped))
          ((null touched)
           (remhash key *setup-drafts*)
           (setup-closed "Nothing changed" "You picked nothing new." :stopped))
          ;; the lane refuses to start on none (REQUIRE-NON-EMPTY-ALLOWLIST)
          ((not (or channels users roles))
           (setup-view key :notice "Pick at least one channel, person or role: with none, I would answer nobody."))
          (t
           (let ((refusal (handler-case
                              (progn (loop for (member name value) in `((:channels "allowed_channels" ,channels)
                                                                         (:users "allowed_users" ,users)
                                                                         (:roles "allowed_roles" ,roles))
                                           when (member member touched)
                                             do (nle:config-set (list "channels" "discord" name)
                                                                (coerce value 'vector)))
                                     (take-section-live host)
                                     nil)
                            (error (condition) (princ-to-string condition)))))
             (if refusal
                 (setup-view key :notice (format nil "Not saved: ~a" refusal))
                 (progn
                   (remhash key *setup-drafts*)
                   (setup-closed "Saved"
                                 (format nil "I answer in ~d channel~:p~@[ (~a)~], and ~a may use me. /channels setup changes it again."
                                         (length channels)
                                         (and (getf draft :all) "all of this server's")
                                         (who-may-use users roles))
                                 :done))))))))

(defun who-may-use (users roles)
  "Who USERS and ROLES let use the bot, in words."
  (cond ((and (null users) (null roles)) "its owner")
        (t (format nil "~@[~a~]~:[~; and ~]~@[~a~]"
                   (and users (format nil "~d ~:*~[people~;person~:;people~]" (length users)))
                   (and users roles)
                   (and roles (format nil "~d role~:p" (length roles)))))))

(defun set-difference-in-order (ids gone)
  "The members of IDS that GONE does not hold, in IDS's order."
  (remove-if (lambda (id) (member id gone :test #'equal)) ids))

(defun take-section-live (host)
  "Rebuild HOST's admission from channels.discord as config.jsonc says it now:
what START-CHANNEL would make of it, with the bot's own id and owners as
they are."
  (let* ((section (discord-section))
         (settings (nlk:section-settings (nlk:find-section '("channels" "discord")) section)))
    (setf (host-policy host)
          (section-policy section settings (nck::host-owners host)
                          (nck::inbound-policy-mention-target (host-policy host))))))

(defun discord-setup-card (host target words source)
  "/channels setup WORDS from the person SOURCE names, in the room at TARGET:
the card, or a pick, All channels, Save or Cancel on it. => its words, the
card beside them (OFFER-CARD)."
  (declare (ignore target))
  (let* ((user (gethash "user_id" source))
         (guild (gethash "workspace_id" source))
         (key (cons user guild))
         (verb (string-downcase (or (first words) "")))
         (ids (remove-if-not (lambda (word) (every #'digit-char-p word)) (rest words))))
    (cond ((not (and (stringp guild) (plusp (length guild))))
           "Run /channels setup in a channel of the server you want to set up: its pickers list that server's channels and roles.")
          ((string= verb "")
           (if (setup-open host key guild)
               (setup-view key)
               "I could not read this server's channels and roles; try again in a moment."))
          ((string= verb "cancel")
           (remhash key *setup-drafts*)
           (setup-closed "Nothing changed" "Cancelled: what I answer stays as it was." :stopped))
          ((string= verb "save") (setup-save host key))
          ((null (setup-draft key))
           (setup-closed "This card has expired" "This card has expired: run /channels setup again to pick." :stopped))
          ((string= verb "all")
           (setup-pick key :channels (mapcar #'car (getf (setup-draft key) :text)) :all t)
           (setup-view key))
          ((member verb '("channels" "users" "roles") :test #'string=)
           (setup-pick key (intern (string-upcase verb) :keyword) ids)
           (setup-view key))
          (t "usage: /channels setup [channels IDS | users IDS | roles IDS | all | save | cancel]"))))

;; The kit's /channels setup draws this card on Discord. Guarded: a kit from
;; before the card has no registry, and the lane runs as it did.
(when (boundp 'nck::*setup-cards*)
  (setf (gethash "discord" nck::*setup-cards*)
        (list :card 'discord-setup-card :owner-p 'application-owner-p)))
