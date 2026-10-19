;;;; probe.lisp --- what the server can see, and how the operator gets one.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The read-only half of onboarding: PROBE-CHANNEL (START-CHANNEL's twin;
;;;; NCK:PROBE is the door) resolves the password the way START-CHANNEL does,
;;;; asks the BlueBubbles server what it is -- its version, the Mac's, the
;;;; account signed into Messages, whether its Private API helper is in --
;;;; which chats the account has and who is in them, and answers a text,
;;;; never the password, so chats and people are picked by name. The section
;;;; declaration at the end is the adapter's one statement of what
;;;; channels.imessage is made of (NLK:DEFINE-SECTION): the kit's refusal, the
;;;; setup panel, the model's setup primer and the addons report all read it.

(in-package #:nodecode-channel-imessage)

(nlk:access (result execution))

(defparameter +probe-shown+ 30
  "How many chats or people one probe line names before it counts the rest.")

(defun section-url (section)
  "The server's base address SECTION names, without a trailing slash."
  (string-right-trim "/" (or (config-string section "url") "")))

(defun section-executor (section executor &key book)
  "The executor the server's password speaks through; EXECUTOR, the scripted
test seam, stands for the HTTP under it."
  (let ((password (resolve-channel-secret section "password")))
    (if executor
        (wrap-imessage-executor executor :book book :password password)
        (make-imessage-executor :url (section-url section) :password password :book book))))

(defun server-info (executor)
  "(values INFO RESULT): the server's own account of itself, or NIL and the
failed RESULT."
  (let ((result (execute-plan executor (read-plan "/server/info" "server_info"))))
    (values (and result.ok-p (nlk:json-value result.body :object "data")) result)))

(defun private-api-p (info)
  "Whether the server INFO describes sends through its Private API: turned
on, and its helper inside Messages connected."
  (and (nlk:json-value info :boolean "private_api")
       (nlk:json-value info :boolean "helper_connected")
       t))

(defun contact-names (executor addresses)
  "A table of each of ADDRESSES, normalized, to its name in the Mac's
contacts; the ones it does not know are absent."
  (let ((names (make-hash-table :test #'equal))
        (result (and addresses (execute-plan executor (contacts-plan addresses)))))
    (when (and result result.ok-p)
      (loop for contact across (nlk:json-array result.body "data")
            for name = (nlk:json-value contact :text "displayName")
            when (plusp (length name))
              do (loop for key in '("phoneNumbers" "emails")
                       do (loop for entry across (nlk:json-array contact key)
                                for address = (if (stringp entry) entry (nlk:json-value entry :string "address"))
                                when address
                                  do (setf (gethash (normalize-handle address) names) name)))))
    names))

(defun chat-label (chat names)
  "What a person calls CHAT: its name, else who it is with."
  (let ((title (nlk:json-value chat :text "displayName"))
        (people (loop for handle across (nlk:json-array chat "participants")
                      for address = (normalize-handle (nlk:json-value handle :string "address"))
                      collect (or (gethash address names) address))))
    (cond ((plusp (length title)) title)
          ((direct-chat-p (nlk:json-value chat :string "guid"))
           (format nil "DM with ~a" (or (first people) "someone")))
          (people (format nil "group with ~{~a~^, ~}" people))
          (t "unnamed group"))))

(defun probe-chats (executor)
  "(values CHATS PEOPLE): the account's latest chats as (GUID LABEL), the
people in them as (HANDLE . NAME)."
  (let* ((result (execute-plan executor
                               (rest-plan "POST" (api-path "chat" "query") "chats" nil 15
                                          (nlk:json-object "with" (vector "participants")
                                                           "limit" +probe-shown+
                                                           "sort" "lastmessage"))))
         (chats (and result.ok-p (coerce (nlk:json-array result.body "data") 'list)))
         (addresses (remove-duplicates
                     (loop for chat in chats
                           append (loop for handle across (nlk:json-array chat "participants")
                                        for address = (nlk:json-value handle :string "address")
                                        when address collect (normalize-handle address)))
                     :test #'equal))
         (names (contact-names executor addresses)))
    (values (loop for chat in chats
                  collect (list (nlk:json-value chat :string "guid") (chat-label chat names)))
            (sort (mapcar (lambda (address) (cons address (or (gethash address names) address))) addresses)
                  #'string< :key #'car))))

(defun probe-channel (section &key executor (timeout 15) &aux (lines '()))
  "What the server in SECTION can see, as lines: the server and the account,
whether replies, tapbacks and typing are possible, the latest chats and the
people in them."
  ;; EXECUTOR overrides the live one (the scripted test seam). Read only. The
  ;; answer never carries the password (the executor redacts every failure).
  (declare (ignore timeout))
  (flet ((say (control &rest args)
           (push (apply #'format nil control args) lines)))
    (let ((bot (section-executor section executor)))
      (multiple-value-bind (info result) (server-info bot)
        (if (not info)
            (say "server: ~a -- run the BlueBubbles server on the Mac signed into the bot's Apple ID, set its password, and give url its address (a LAN, Tailscale or tunnel address this machine reaches)"
                 (probe-failure result))
            (multiple-value-bind (chats people) (probe-chats bot)
              (say "server: ok -- BlueBubbles ~a on macOS ~a~@[, signed in as ~a~]"
                   (or (nlk:json-value info :text "server_version") "?")
                   (or (nlk:json-value info :text "os_version") "?")
                   (nlk:json-value info :text "detected_imessage"))
              (say "private api: ~:[off -- answers, files and choices work; inline replies, tapbacks and the typing bubble need the server's Private API (its helper in Messages)~;on -- inline replies, tapbacks and the typing bubble~]"
                   (private-api-p info))
              (say "~a" (if chats
                            (format nil "chats: ~{~a~^, ~}"
                                    (loop for (guid label) in chats collect (format nil "~a ~a" label guid)))
                            "chats: none yet -- send the bot's Apple ID a message from an allowed person"))
              (when people
                (say "people: ~{~a~^, ~}"
                     (mapcar (lambda (pair) (if (equal (car pair) (cdr pair))
                                                (car pair)
                                                (format nil "~a ~a" (cdr pair) (car pair))))
                             (subseq people 0 (min (length people) +probe-shown+)))))))))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(defun probe-choices (section executor reader)
  "READER's (ID . LABEL) conses over what the server sees, refused in the
probe's words when it answers nothing."
  (let ((bot (section-executor section executor)))
    (multiple-value-bind (info result) (server-info bot)
      (unless info
        (config-error "server: ~a" (probe-failure result))))
    (multiple-value-call reader (probe-chats bot))))

(defun probe-chat-choices (section &key executor (timeout 15))
  "The account's latest chats as ((guid . label) ...) -- what a setup panel
offers for allowed_chats."
  (declare (ignore timeout))
  (probe-choices section executor
                 (lambda (chats people)
                   (declare (ignore people))
                   (loop for (guid label) in chats collect (cons guid label)))))

(defun probe-user-choices (section &key executor (timeout 15))
  "The people in those chats as ((handle . name) ...) -- what a setup panel
offers for allowed_users and owner."
  (declare (ignore timeout))
  (probe-choices section executor (lambda (chats people) (declare (ignore chats)) people)))

;;; The declaration. Types and constraints are what START-CHANNEL reads and
;;; RESOLVE-CHANNEL-SECRET / REQUIRE-NON-EMPTY-ALLOWLIST enforce; the kit
;;; refuses on SECTION-PROBLEMS before either runs.
(nlk:define-section ("channels" "imessage")
  (:guide "iMessage has no bot accounts: give the bot an Apple ID of its own, never your own, signed into Messages on a Mac that stays on, and run the BlueBubbles server there (https://bluebubbles.app, Apache-2.0; Full Disk Access and Accessibility granted, a server password set). Its Private API -- inline replies, tapbacks, the typing bubble -- needs SIP partly off on that Mac; without it the bot still answers, sends files and offers choices. Give url the address this machine reaches the server at (the LAN, Tailscale, or the server's own Cloudflare or ngrok tunnel). Keep the password in a file (mode 600) or an environment variable, never in the config. People are their handles: a phone number with its country code, or an email address.")
  (:check #'probe-channel)
  (:one-of "password_env" "password_file")
  (:any-of "allowed_chats" "allowed_users")
  ("url" :string :required t :doc "the BlueBubbles server's address, e.g. http://192.168.1.20:1234")
  ("password_env" :env :doc "environment variable holding the server password")
  ("password_file" :path :doc "file holding the server password")
  ("allowed_chats" :list :doc "chat guids (iMessage;-;+15551234567, iMessage;+;chat...) the bot answers in"
                   :choices #'probe-chat-choices)
  ("allowed_users" :list :doc "handles -- phone numbers with country code, or email addresses -- allowed to drive the bot"
                   :choices #'probe-user-choices)
  ("owner" :list :doc "the operator's handle, whose word is standing policy in the chat"
           :choices #'probe-user-choices)
  ("mention_names" :list :doc "names that call the bot in a group, typed with or without @ (iMessage has no bot mention)")
  ("require_mention" :boolean :default t
                     :doc "in groups, answer when called by a mention name or in a thread the bot answers in; a direct message always answers")
  ("free_response_chats" :list :choices #'probe-chat-choices
                         :doc "chat guids where every allowed message may open a turn")
  ("ignored_chats" :list :choices #'probe-chat-choices
                   :doc "chat guids the bot reads neither as prompts nor as context")
  ("dm_policy" :choice :options '("allow" "disabled") :default "allow" :doc "direct messages")
  ("group_policy" :choice :options '("allow" "disabled") :default "allow" :doc "group chats")
  ("reactions" :boolean :default :false
               :doc "tapback each ask: emphasis while its turn runs, thumbs up when it is done, down when it failed (needs the Private API)")
  ("poll_ms" :integer :default 1500
             :doc "milliseconds between two reads of the server's new messages")
  ("turn_budget_minutes" :integer :default 0
                         :doc "minutes an ask's turn may run: past them its tool calls are refused and it answers with what it has; 0, the default, caps nothing")
  ("soul_file" :path :doc "a SOUL.md whose text is the channel's standing persona"))
