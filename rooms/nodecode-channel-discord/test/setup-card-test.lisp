;;;; setup-card-test.lisp --- /channels setup: Discord's own pickers, held until Save.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun setup-guild-replies ()
  "The two reads a card opens on: guild g1's channels (text 201 and 202, an
announcement channel 203, a voice channel and a category it never lists),
then its roles (@everyone, a role 401, a bot's managed role 402)."
  (list (nck:make-scripted-response
         200 (vector (nlk:json-object "id" "201" "name" "general" "type" 0)
                     (nlk:json-object "id" "202" "name" "nodecode" "type" 0)
                     (nlk:json-object "id" "203" "name" "news" "type" 5)
                     (nlk:json-object "id" "204" "name" "voice" "type" 2)
                     (nlk:json-object "id" "205" "name" "lab" "type" 4)))
        (nck:make-scripted-response
         200 (vector (nlk:json-object "id" "g1" "name" "@everyone")
                     (nlk:json-object "id" "401" "name" "bot" "managed" :false)
                     (nlk:json-object "id" "402" "name" "wise" "managed" t)))))

(defun setup-config-member (name)
  "The member NAME of channels.discord as the config file says it now: its
ids, or :ABSENT."
  (let ((section (ncd::discord-section)))
    (if (nth-value 1 (gethash name section))
        (coerce (gethash name section) 'list)
        :absent)))

(defmacro with-setup-card ((host card &key (config "{\"channels\": {\"discord\": {\"allowed_channels\": [\"999\", \"201\"], \"allowed_users\": [\"301\"], \"owner\": [\"300\"]}}}")
                                       (opens 1) (user "300"))
                           &body body)
  "BODY with HOST a Discord-shaped host whose owner is 300 and whose executor
answers OPENS card openings, channels.discord as CONFIG says, and (CARD
&rest WORDS) running /channels setup WORDS as USER in guild g1 => (values
WORDS PANEL CONTROLS)."
  `(handler-bind ((warning #'muffle-warning))
     (with-temp-file (nodecode.evolved::*shared-config-path* :type "jsonc" :contents ,config :setf t)
       (let ((,host (test-host :executor (apply #'scripted-executor
                                                (loop repeat ,opens append (setup-guild-replies)))
                               :owners '("300")))
             (ncd::*setup-drafts* (make-hash-table :test #'equal :synchronized t)))
         (setf (nck:host-policy ,host) (channel-policy :mention-target "bot1"))
         (flet ((,card (&rest words)
                  (let ((nck:*command-source* (nlk:json-object "user_id" ,user "workspace_id" "g1"
                                                               "channel_id" "777"))
                        (nck::*command-card* nil))
                    (let ((text (ncd::discord-setup-card ,host nil words nck:*command-source*)))
                      (values text (getf nck::*command-card* :panel)
                              (getf nck::*command-card* :controls))))))
           ,@body)))))

(deftest channel-discord-setup-pickers-are-discords-own-selects ()
  ;; A kit picker is drawn as Discord's own select of its kind: a channel
  ;; select of text and announcement channels, a user select, a role select,
  ;; each opening on the ids it holds, a pick of none allowed.
  (let ((rows (ncd::controls-components
               (list (nck::picker :channel "Pick channels" "/channels setup channels" :current '("201" "202"))
                     (nck::picker :user "Who" "/channels setup users" :current '("301"))
                     (nck::picker :role "Roles" "/channels setup roles")))))
    (flet ((select (n) (aref (nlk:json-value (aref rows n) :array "components") 0)))
      (is (= 3 (length rows)) "one action row a picker")
      (is (equal '(8 5 6) (loop for n below 3 collect (nlk:json-value (select n) :integer "type"))))
      (is (equalp #(0 5) (nlk:json-value (select 0) :array "channel_types")))
      (is (equal '(0 25) (list (nlk:json-value (select 0) :integer "min_values")
                               (nlk:json-value (select 0) :integer "max_values"))))
      (is (equal '(("201" "channel") ("202" "channel"))
                 (loop for value across (nlk:json-value (select 0) :array "default_values")
                       collect (list (nlk:json-value value :string "id")
                                     (nlk:json-value value :string "type")))))
      (is (equal "user" (nlk:json-value (aref (nlk:json-value (select 1) :array "default_values") 0)
                                        :string "type")))
      (is (null (nlk:json-value (select 2) :array "default_values")) "none held, none shown")
      (let ((said (nck:said-line (nlk:json-value (select 0) :string "custom_id"))))
        (is (equal "/channels setup channels" said) "its custom_id is the press that says its line")))))

(deftest channel-discord-setup-a-pick-says-its-ids ()
  ;; A pick on a native select routes as its line with the picked ids after
  ;; it, a word each; a pick of none says the line alone.
  (let ((token (nck:press-token "/channels setup channels")))
    (flet ((pick (&rest values)
             (gethash "text"
                      (dgw-interaction
                       (nlk:encode-json-object
                        (nlk:json-object "type" 3 "id" "i1" "token" "tok" "application_id" "app"
                                         "channel_id" "777" "guild_id" "g1"
                                         "member" (nlk:json-object "user" (nlk:json-object "id" "300" "username" "peas"))
                                         "message" (nlk:json-object "id" "m9" "flags" 32832)
                                         "data" (nlk:json-object "component_type" 8 "custom_id" token
                                                                 "values" (coerce values 'vector))))))))
      (is (equal "/channels setup channels 201 203" (pick "201" "203")))
      (is (equal "/channels setup channels" (pick)) "none picked"))))

(deftest channel-discord-setup-card-holds-picks-until-save ()
  ;; The card opens on the section as it is, over this server alone; a pick
  ;; is held and drawn, never written; Save writes the members a pick changed
  ;; and nothing else, keeps another server's channels, and takes the section
  ;; into the running lane's admission at once.
  (with-setup-card (host card)
    (multiple-value-bind (text panel controls) (card)
      (is (search "Channels: <#201>" text) "999 is another server's, so the card holds 201 alone")
      (is (equal "Where should I answer, and for whom?" (getf panel :title)))
      (is (= 5 (length controls)) "channels, All, people, roles, Save and Cancel")
      (is (equal '("201") (fifth (first controls))) "the channel picker opens on what it holds")
      (is (equal "All 3 channels" (first (first (second controls)))) "text and announcement channels")
      (is (equal '("301") (fifth (third controls))))
      (is (null (fifth (fourth controls))) "no role held")
      (let ((json (nlk:encode-json-object (aref (ncd::panel-components panel controls) 0))))
        (is (ppcre:scan "\"type\":\\s*8\\b" json) "the card draws the channel select inside its panel")))
    (multiple-value-bind (text panel) (card "channels" "202" "203")
      (is (search "Channels: <#202>, <#203>" text))
      (is (equal "<#202>, <#203>" (cdr (assoc "Channels" (getf panel :fields) :test #'equal)))))
    (is (equal '("999" "201") (setup-config-member "allowed_channels")) "a pick writes nothing")
    (multiple-value-bind (text panel controls) (card "save")
      (is (equal "I answer in 3 channels, and 1 person may use me. /channels setup changes it again." text))
      (is (eq :done (getf panel :tone)))
      (is (null controls) "the saved card has no pickers left"))
    (is (equal '("999" "202" "203") (setup-config-member "allowed_channels")))
    (is (equal '("301") (setup-config-member "allowed_users")) "an untouched member stays")
    (is (eq :absent (setup-config-member "allowed_roles")) "and one never picked is never written")
    (is (equal '("300") (setup-config-member "owner")))
    (is (equal '("999" "202" "203") (nck::inbound-policy-allowed-channels (nck:host-policy host))) "the lane admits by it at once")
    (let ((users (nck::inbound-policy-allowed-users (nck:host-policy host))))
      (is (null (set-exclusive-or '("301" "300") users :test #'equal)) "its owner still always talks"))
    (is (equal "bot1" (nck::inbound-policy-mention-target (nck:host-policy host))))
    (is (search "expired" (card "save")) "a saved card is closed")))

(deftest channel-discord-setup-all-roles-cancel-and-the-empty-guard ()
  ;; All channels picks every text channel of this server; a role pick is
  ;; this server's alone; Cancel writes nothing; and a Save that would leave
  ;; the bot answering nobody is refused on the card.
  (with-setup-card (host card :opens 3)
    (card)
    (is (equal '("401") (getf (gethash '("300" . "g1") ncd::*setup-drafts*) :role-ids)) "@everyone and a bot's managed role are no role a pick could mean")
    (multiple-value-bind (text panel) (card "all")
      (declare (ignore text))
      (is (equal "all 3 text channels" (cdr (assoc "Channels" (getf panel :fields) :test #'equal)))))
    (card "roles" "401")
    (is (equal "I answer in 4 channels (all of this server's), and 1 person and 1 role may use me. /channels setup changes it again."
               (card "save")))
    (is (equal '("999" "201" "202" "203") (setup-config-member "allowed_channels")))
    (is (equal '("401") (setup-config-member "allowed_roles")))
    (card)
    (card "channels" "202")
    (is (equal "Cancelled: what I answer stays as it was." (card "cancel")))
    (is (equal '("999" "201" "202" "203") (setup-config-member "allowed_channels")) "Cancel wrote nothing")
    (is (search "expired" (card "channels" "201")) "and closed the card")
    (card)
    (card "users")
    (card "roles")
    (let ((said (card "save")))
      (is (equal "I answer in 4 channels, and its owner may use me. /channels setup changes it again." said) "the channels still answer, so emptying people and roles goes through"))
    (is (equal '() (setup-config-member "allowed_users")))
    (is (equal '() (setup-config-member "allowed_roles"))))
  (with-setup-card (host card :config "{\"channels\": {\"discord\": {\"allowed_channels\": [\"201\"]}}}")
    (card)
    (card "channels")
    (multiple-value-bind (text panel) (card "save")
      (is (search "/channels setup save keeps it" text) "the card stays open")
      (is (search "with none, I would answer nobody" (getf panel :notice))))
    (is (equal '("201") (setup-config-member "allowed_channels")) "and nothing was written")))

(deftest channel-discord-setup-card-is-the-owners ()
  ;; Only an owner opens the card: a declared operator, or -- in a room that
  ;; declares none -- whom the application names, read once; anywhere
  ;; outside a server the card says where to run it.
  (let ((host (test-host :owners '("300"))))
    (is (nck::setup-owner-p host "300"))
    (is (not (nck::setup-owner-p host "301")))
    (is (not (nck::setup-owner-p host nil))))
  (let* ((executor (scripted-executor (reply 200 "owner" (nlk:json-object "id" "300") "team" :null)))
         (host (test-host :owners '() :platform (ncd:discord-platform :bot-user-id "bot1"
                                                                      :application-id (constantly "app"))
                          :executor executor)))
    (is (nck::setup-owner-p host "300") "the application's owner")
    (is (not (nck::setup-owner-p host "301")))
    (is (= 1 (length (nck:recording-executor-plans executor))) "read once"))
  (let* ((executor (scripted-executor
                    (reply 200 "owner" (nlk:json-object "id" "999")
                           "team" (nlk:json-object "members" (vector (nlk:json-object "user" (nlk:json-object "id" "500"))
                                                                     (nlk:json-object "user" (nlk:json-object "id" "501")))))))
         (host (test-host :owners '() :platform (ncd:discord-platform :bot-user-id "bot1"
                                                                      :application-id (constantly "app"))
                          :executor executor)))
    (is (nck::setup-owner-p host "501") "a team's member")
    (is (not (nck::setup-owner-p host "999")) "a team owns the app, not its placeholder owner"))
  (let ((said (ncd::discord-setup-card (test-host) nil '() (nlk:json-object "user_id" "300"))))
    (is (search "Run /channels setup in a channel of the server" said) "a direct message has no server to pick from"))
  (is (search "runs in a room of the channel" (nck::channels-command "setup" nil)))
  (is (search "setup |" (nck::channels-command "nope" nil)) "the usage names it"))

(deftest channel-discord-setup-passes-the-gate-for-operators-alone ()
  ;; The card is where the channels are picked, so an operator's /channels
  ;; setup -- an interaction -- passes a channel the allowlist does not name;
  ;; anyone else's is refused there as any line is.
  (handler-bind ((warning #'muffle-warning))
    (with-digest-host (host nil :name "setup-gate"
                       :host-keys (:owners '("mike") :platform (test-platform :choices t)))
      (setf (nck:host-policy host) (channel-policy :allowed-channels '("123")))
      (flet ((setup (user)
               (let ((candidate (test-candidate :channel "777" :message nil :text "/channels setup"
                                                :user user :addressed t)))
                 (setf (gethash "interaction_id" (gethash "source" candidate)) "i1")
                 (nck:handle-candidate host candidate))))
        (is (eq :answer (setup "mike")) "an operator's opens past the gate")
        (is (eq :reject (setup "alice")) "anyone else's meets it")
        (let ((typed (test-candidate :channel "777" :text "/channels setup" :user "mike")))
          (is (eq :reject (nck:handle-candidate host typed)) "a typed line, no interaction, meets the gate too"))))))
