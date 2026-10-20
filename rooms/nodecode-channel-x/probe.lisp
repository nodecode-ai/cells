;;;; probe.lisp --- what the session can see, and the section declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The read-only half of onboarding: PROBE-CHANNEL (START-CHANNEL's twin;
;;;; NCK:PROBE is the door) starts a bridge on the section's cookie, asks X
;;;; whom it is signed in as and whether that is the account handle names,
;;;; and reads the newest mentions, answering a text that never carries the
;;;; cookie. The declaration at the end is the adapter's one statement of what
;;;; channels.x is made of (NLK:DEFINE-SECTION).

(in-package #:nodecode-channel-x)

(nlk:access (result execution) (me execution) (named execution) (mentions execution))

(defun probe-channel (section &key executor (timeout 60) &aux (lines '()))
  "What the session in SECTION can see, as lines: the account it is signed
in as, whether that is handle's, and the newest mentions."
  (declare (ignore timeout))
  (flet ((say (control &rest args) (push (apply #'format nil control args) lines)))
    (let* ((handle (normalize-x-handle (config-string section "handle")))
           (bridge (unless executor
                     (handler-case
                         (start-bridge (or (config-string-list section "bridge_command") +default-bridge-command+)
                                       (resolve-channel-secret section "token"))
                       (bridge-failure (condition)
                         (say "session: ~a" condition)
                         nil)))))
      (unwind-protect
           (when (or executor bridge)
             (let* ((bot (wrap-x-executor (or executor (bridge-executor bridge)) :book (make-x-book)))
                    (me (execute-plan bot (bridge-plan "me" "me" 60)))
                    (self (and me.ok-p (nlk:json-value me.body :string "user_id")))
                    (named (and (plusp (length handle))
                                (execute-plan bot (bridge-plan "user_id" "user_id" 60 "handle" handle))))
                    (named-id (and named (execution-ok-p named)
                                   (nlk:json-value named.body :string "user_id"))))
               (say "session: ~:[refused -- ~a~;signed in as account ~a~]" self (or self me.error))
               (say "handle: ~:[not set -- the bot account's @handle~;@~a is account ~a~:[ -- NOT the signed-in account~;, the signed-in account~]~]"
                    (plusp (length handle)) handle (or named-id "unknown") (and self (equal self named-id)))
               (when self
                 (let ((mentions (execute-plan bot (mentions-plan 0 :limit 10))))
                   (if mentions.ok-p
                       (let ((tweets (nlk:json-array mentions.body "tweets")))
                         (say "mentions: ~d readable~@[, newest from @~a~]" (length tweets)
                              (and (plusp (length tweets))
                                   (nlk:json-value (aref tweets (1- (length tweets))) :string "author"))))
                       (say "mentions: ~a" mentions.error))))))
        (stop-bridge bridge)))
    (format nil "~{~a~^~%~}" (nreverse lines))))

(nlk:define-section ("channels" "x")
  (:guide "X has no bot API a session cookie opens, and X's rules allow suspending an account that scripts the website: give the bot an X account of its own, never yours. Sign that account in on x.com, copy its cookie header (auth_token and ct0 at least) into a file of mode 600, and name it token_file. The bridge runs tweetkit-x (https://github.com/nsozturk/tweetkit-x, MIT) under uvx. The bot answers only replies under a mention from allowed_users, never tweets on its own, and sends no direct messages (X's DMs are now encrypted XChat).")
  (:check #'probe-channel)
  (:one-of "token_env" "token_file")
  ("handle" :string :required t :doc "the bot account's @handle; the cookie must be signed in as it")
  ("token_env" :env :doc "environment variable holding the bot account's session cookie")
  ("token_file" :path :doc "file holding the bot account's session cookie (auth_token=...; ct0=...)")
  ("allowed_users" :list :required t :doc "@handles whose mentions the bot answers")
  ("owner" :list :doc "the operator's @handle, whose word is standing policy")
  ("ignored_conversations" :list :doc "conversation ids the bot reads neither as prompts nor as context")
  ("poll_seconds" :integer :default 60 :doc "seconds between two reads of the mentions (15 at least)")
  ("max_posts" :integer :default 4 :doc "posts one answer may take; the rest is cut")
  ("daily_posts" :integer :default 50 :doc "posts a UTC day allows; past them every post is refused")
  ("bridge_command" :list :doc "the interpreter bridge.py runs under; default uvx with tweetkit-x pinned")
  ("turn_budget_minutes" :integer :default 0
                         :doc "minutes an ask's turn may run: past them its tool calls are refused; 0 caps nothing")
  ("soul_file" :path :doc "a SOUL.md whose text is the channel's standing persona"))
