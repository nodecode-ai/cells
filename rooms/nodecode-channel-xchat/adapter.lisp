;;;; adapter.lisp --- the X Chat platform, its plans, and the reading lap.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; One X Chat conversation is one room: a 1:1 conversation (an id of the
;;;; form 123-456) or a group (g...). The lane runs as the account the
;;;; operator connected; a DM's body is ciphertext until OPEN-TEXT has it
;;;; (crypto.lisp), and an answer is sealed with the same conversation key
;;;; before it is POSTed.
;;;;
;;;; Thread topology (the kit rule: no network I/O on a thread that is not
;;;; ours): the lap reads and decrypts on its own supervised thread and hands
;;;; each candidate to HANDLE-CANDIDATE; every send, receipt and typing beat
;;;; is a plan the kit's delivery pool runs.
;;;;
;;;; The lap is a poll: X Chat has no stream. The conversation list carries
;;;; each conversation's newest event id, so a conversation with nothing
;;;; newer than its kept cursor costs no second call, and a message that
;;;; arrived while the organism was down is read when it comes back.

(in-package #:nodecode-channel-xchat)

(nlk:access (result execution) (plan request-plan) (host channel-host))

(defparameter +xchat-text-limit+ 4000
  "One X Chat message: the section may lower it, nothing raises it.")

(defvar *xchat-adapter* nil
  "The live adapter, or NIL when the lane is stopped.")

;;; --- the section ---------------------------------------------------------------

(nlk:define-section ("channels" "xchat")
  (:guide "connect the account the agent runs as: an X developer app with OAuth 2.0 user context, and a user token with dm.read, dm.write, users.read; put the token in an environment variable named by access_token_env; user_id is the account's numeric id; allowed_users names who may talk to it. Next step: DM that account, or open its self-chat.")
  ("access_token_env" :env :doc "environment variable holding the OAuth2 user access token")
  ("access_token_file" :path :doc "file holding the OAuth2 user access token")
  ("refresh_token_env" :env :doc "environment variable holding the refresh token (optional)")
  ("refresh_token_file" :path :doc "file holding the refresh token (optional)")
  ("client_id" :string :doc "X app OAuth2 client id, needed to refresh the token")
  ("client_secret_env" :env :doc "X app OAuth2 client secret, for confidential clients only")
  ("user_id" :string :doc "the account's numeric X user id")
  ("allowed_users" :list :doc "numeric X user ids allowed to talk to the agent")
  ("poll_seconds" :integer :default 10 :min 2 :doc "seconds between two reads of the conversations")
  ("send_read_receipts" :boolean :default nil :doc "mark read what the agent was given")
  ("enabled" :boolean :default t :doc "false stops the lane"))

;;; --- reading JSON without guessing ------------------------------------------------

(defun json-get (object key)
  "OBJECT's member KEY, or NIL."
  (when (hash-table-p object) (gethash key object)))

(defun json-list (object key)
  "OBJECT's member KEY as a list of its elements."
  (let ((value (json-get object key)))
    (if (vectorp value) (coerce value 'list) (list))))

(defun newer-p (a b)
  "Whether sequence id A is after B, numeric ids being numbers."
  (let ((x (ignore-errors (parse-integer (princ-to-string a) :junk-allowed t)))
        (y (ignore-errors (parse-integer (princ-to-string b) :junk-allowed t))))
    (if (and (integerp x) (integerp y))
        (> x y)
        (string> (princ-to-string a) (princ-to-string b)))))

(defun peer-of (self conversation-id)
  "The other account in CONVERSATION-ID from SELF's side: the part of a\n1:1 id (123-456) that is not SELF, else SELF."
  (let* ((id (princ-to-string conversation-id))
         (dash (position #\- id)))
    (if dash
        (let ((left (subseq id 0 dash)) (right (subseq id (1+ dash))))
          (cond ((equal left (princ-to-string self)) right)
                ((equal right (princ-to-string self)) left)
                (t right)))
        (princ-to-string self))))

;;; --- the plans ------------------------------------------------------------------

(defun xchat-adapter-book (adapter) (getf adapter :book))
(defun xchat-adapter-self (adapter) (getf adapter :self))
(defun xchat-adapter-allowed (adapter) (getf adapter :allowed))
(defun xchat-adapter-host-obj (adapter) (getf adapter :host))

(defun auth-headers (book)
  (list (cons "authorization" (format nil "Bearer ~a" (or (xchat-book-token book) "")))))

(defun xchat-conversations-plan (adapter)
  "GET /2/chat/conversations: every conversation and its newest event id."
  (rest-plan "GET" "/2/chat/conversations?max_results=100&chat_conversation.fields=id,newest_event_id"
             "conversations" t 30 nil (auth-headers (xchat-adapter-book adapter))))

(defun conversation-path (conversation-id)
  "A conversation's id in a URL path: the wire's colon form is a hyphen."
  (substitute #\- #\: (princ-to-string conversation-id)))

(defun xchat-events-plan (adapter conversation-id &key (max-results 50))
  (rest-plan "GET" (format nil "/2/chat/conversations/~a/events?max_results=~d"
                                   (conversation-path conversation-id) max-results)
             "events" t 30 nil (auth-headers (xchat-adapter-book adapter))))

(defun xchat-message-plan (adapter conversation-id text)
  "POST a sealed message. TEXT is already sealed."
  (rest-plan "POST" (format nil "/2/chat/conversations/~a/messages" (conversation-path conversation-id))
             "messages" t 60
             (nlk:make-json-object
              "message" (nlk:make-json-object
                         "body" (nlk:make-json-object "text" text)))
             (auth-headers (xchat-adapter-book adapter))))

(defun xchat-read-plan (adapter conversation-id sequence)
  (rest-plan "POST" (format nil "/2/chat/conversations/~a/read" (conversation-path conversation-id))
             "read" t 30
             (nlk:make-json-object "seen_until_sequence_id" (princ-to-string sequence))
             (auth-headers (xchat-adapter-book adapter))))

(defun xchat-typing-plan (adapter conversation-id)
  (rest-plan "POST" (format nil "/2/chat/conversations/~a/typing" (conversation-path conversation-id))
             "typing" t 15 nil (auth-headers (xchat-adapter-book adapter))))

(defun noop-plan (label &optional body)
  "A plan that reaches nothing, for what this surface cannot do."
  (rest-plan "NOOP" "none" label nil 5 body))

;;; --- the platform ----------------------------------------------------------------

(defun xchat-where-text (candidate)
  "Where the conversation is, for the lane contract."
  (format nil "an X Chat conversation ~a. The agent answers as the connected X account, ~
sealed with that conversation's key. X draws no Markdown and shows the answer as plain text."
          (or (source-field candidate "channel_id") "unknown")))

(defun xchat-platform (adapter)
  "The X Chat PLATFORM over ADAPTER: a sealed message, no edit, no delete, a
typing beat X Chat has, and no attachments."
  (let ((book (xchat-adapter-book adapter))
        (self (xchat-adapter-self adapter)))
    (make-platform
     :id "xchat"
     :name "X Chat"
     :noun "conversation"
     :owner-label "X account id"
     :session-prefix "xchat"
     :contract-section "xchat-channel"
     :text-limit +xchat-text-limit+
     :typing-refresh-ms 30000
     :plan-message (lambda (target chunk &rest keys)
                     (declare (ignore keys))
                     (xchat-message-plan adapter (getf target :channel-id)
                                          (seal-text (conversation-key self
                                                                        (peer-of self (getf target :channel-id))
                                                                        (getf target :channel-id))
                                                     chunk)))
     :plan-edit (lambda (target id text &rest keys)
                  (declare (ignore target id text keys))
                  (noop-plan "edit"))
     :plan-delete (lambda (target id &rest keys)
                    (declare (ignore target id keys))
                    (noop-plan "delete"))
     :plan-typing (lambda (target &rest keys)
                    (declare (ignore keys))
                    (xchat-typing-plan adapter (getf target :channel-id)))
     :plan-file (lambda (target path &rest keys)
                  (declare (ignore target path keys))
                  (noop-plan "file"))
     :message-id-of (lambda (body) (json-get (json-get body "data") "id"))
     :strip-mention (lambda (text) text)
     :answer-body (lambda (text footer) (declare (ignore footer)) text)
     :where-text #'xchat-where-text
     :choices nil
     :api-primer "X Chat is open from eval as the connected account: (ncxc:xchat-request \"conversations\") lists the conversations; (ncxc:xchat-request \"send\" :conversation \"<id>\" :text \"...\") sends a plain-text DM. A body is sealed with the conversation's key before it leaves."
     :seams-primer "  ncxc:xchat-request -- one X Chat call on the lane's own account, on the caller's thread.")))

;;; --- the lap --------------------------------------------------------------------

(defun lap-call (adapter plan)
  (execute-plan (getf adapter :executor) plan))

(defun cursor-path ()
  (nlk:home (format nil "channels/xchat/~a" +cursor-file+)))

(defun read-conversation (adapter conversation sink)
  "Read CONVERSATION's events newer than its kept cursor; hand each allowed
sender's decrypted message to SINK."
  (let* ((book (xchat-adapter-book adapter))
         (self (xchat-adapter-self adapter))
         (id (json-get conversation "id"))
         (newest (json-get conversation "newest_event_id"))
         (cursor (or (xchat-cursor book id) "0")))
    (when (and id newest (newer-p newest cursor))
      (let ((result (lap-call adapter (xchat-events-plan adapter id))))
        (when result.ok-p
          (dolist (event (json-list result.body "data"))
            (let ((sender (json-get event "sender_id"))
                  (sequence (json-get event "sequence_id"))
                  (sealed (json-get event "encoded_event")))
              (when (and sequence (or (null (xchat-cursor book id))
                                      (newer-p sequence (xchat-cursor book id))))
                (setf (xchat-cursor book id) sequence))
              (when (and sender sealed (not (equal sender self))
                         (member sender (xchat-adapter-allowed adapter) :test #'equal))
                (let ((text (open-text (conversation-key self sender id) sealed)))
                  (when (and text (plusp (length (nlk:trimmed text))))
                    (funcall sink
                             (nlk:json-object
                              "text" (nlk:trimmed text)
                              "source" (nlk:json-object
                                        "platform" "xchat"
                                        "chat_kind" "dm"
                                        "channel_id" id
                                        "user_id" sender
                                        "user_name" sender
                                        "message_id" (or (json-get event "id") sequence)
                                        "addressed" t
                                        "is_bot" nil)))))))))))))

(defun xchat-poll-lap (adapter stop-p &key sink)
  "One read of the conversations. Answers the seconds to wait before the
next lap, or :STOP once STOP-P says so."
  (when (funcall stop-p) (return-from xchat-poll-lap :stop))
  (let ((result (lap-call adapter (xchat-conversations-plan adapter))))
    (when (funcall stop-p) (return-from xchat-poll-lap :stop))
    (cond
      ((member result.status '(401 403))
       (set-channel-status "xchat" :state :degraded :connected nil
                                    :detail "X refused the account's token: give the lane a fresh user access token")
       60.0)
      (result.ok-p
       (set-channel-status "xchat" :state :running :connected t)
       (dolist (conversation (json-list result.body "data"))
         (unless (funcall stop-p)
           (handler-case (read-conversation adapter conversation (or sink #'handle-candidate))
             (error (condition) (warn "xchat: a conversation was not read: ~a" condition)))))
       (keep-cursors (xchat-adapter-book adapter))
       1.0)
      (t
       (set-channel-status "xchat" :connected nil :detail result.error)
       (warn "xchat read failed: ~a" result.error)
       30.0))))

;;; --- the door -------------------------------------------------------------------

(defun xchat-request (op &rest arguments)
  "One X Chat call from eval, as the connected account.
OP is \"conversations\", \"events\" (:conversation), \"send\" (:conversation
:text) or \"read\" (:conversation :sequence). Answers (:status N :body VALUE)."
  (let ((adapter (or *xchat-adapter* (error "the xchat channel is not running"))))
    (labels ((plan ()
               (cond
                 ((string= op "conversations") (xchat-conversations-plan adapter))
                 ((string= op "events") (xchat-events-plan adapter (getf arguments :conversation)))
                 ((string= op "send")
                  (let* ((conversation (getf arguments :conversation))
                         (text (getf arguments :text))
                         (peer (or (getf arguments :peer) conversation))
                         (sealed (seal-text (conversation-key (xchat-adapter-self adapter)
                                                              (princ-to-string peer)
                                                              conversation)
                                            text)))
                    (xchat-message-plan adapter conversation sealed)))
                 ((string= op "read") (xchat-read-plan adapter (getf arguments :conversation)
                                                       (getf arguments :sequence)))
                 (t (error "xchat: no such op ~a" op)))))
      (let ((result (lap-call adapter (plan))))
        (list :status result.status :body result.body
              :error result.error)))))

;;; --- start and stop ---------------------------------------------------------------

(defun token-of (section base)
  (handler-case (resolve-channel-secret section base) (error () nil)))

(defun start-channel (section &key executor)
  "Start the X Chat lane from its channels.xchat SECTION; answers a stop thunk."
  (let* ((settings (nlk:section-settings (nlk:find-section (list "channels" "xchat")) section))
         (token (resolve-channel-secret section "access_token"))
         (refresh (token-of section "refresh_token"))
         (self (or (and (plusp (length (or (getf settings :user-id) ""))) (getf settings :user-id))
                   (config-error "channels.xchat: user_id is not set -- the account's numeric X id")))
         (allowed (getf settings :allowed-users))
         (book (make-xchat-book :path (cursor-path) :token token :refresh-token refresh))
         (executor (or executor (make-dexador-executor :base-url +api-base+
                                                       :redact-prefixes (list token refresh))))
         (adapter (list :book book :self self
                        :allowed (or allowed '())
                        :executor executor
                        :host nil)))
    (require-non-empty-allowlist "xchat" "allowed_users" (getf settings :allowed-users))
    (let* ((host (make-host-from-section
                  section
                  (xchat-platform adapter)
                  :executor executor
                  :owners (getf settings :allowed-users)
                  :soul-path (soul-path section)
                  :request-timeout 30
                  :policy (make-inbound-policy
                           :allowed-users (getf settings :allowed-users)
                           :dm-policy :enabled
                           :group-policy :enabled
                           :require-mention nil
                           :allow-bots :none
                           :channel-match :exact)))
           (stop (run-host host "xchat"
                           (lambda (stop-p) (xchat-poll-lap adapter stop-p))
                           :on-start (lambda () (setf *xchat-adapter* adapter))
                           :on-stop (lambda ()
                                      (when (eq *xchat-adapter* adapter)
                                        (setf *xchat-adapter* nil))))))
      (lambda ()
        (funcall stop)
        (keep-cursors book)))))
