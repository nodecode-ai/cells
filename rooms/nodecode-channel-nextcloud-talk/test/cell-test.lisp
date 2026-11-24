;;;; cell-test.lisp --- the Nextcloud Talk adapter's own tests. Offline:
;;;; every call is answered by a scripted executor, never a socket.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun talk-answer (body &key (status 200))
  (nck::make-execution :ok-p (< status 400) :status status :attempts 1 :body body))

(defun talk-scripted (&rest answers)
  "An executor answering ANSWERS in order, then a bare object."
  (let ((queue (copy-list answers)))
    (nck::make-plan-executor
     :run (lambda (plan)
            (declare (ignore plan))
            (or (pop queue) (talk-answer (nlk:json-object)))))))

(defun talk-ocs (data)
  "DATA wrapped as the OCS envelope Talk answers in."
  (nlk:json-object "ocs" (nlk:json-object "meta" (nlk:json-object "statuscode" 200)
                                           "data" data)))

(defun talk-me (&key (id "nodebot") (name "Node Bot"))
  (talk-ocs (nlk:json-object "id" id "displayname" name)))

(defun talk-room (&rest pairs)
  "One row of the room list: a group room `tok1' unless PAIRS say else."
  (let ((row (nlk:json-object "token" "tok1" "displayName" "Ops" "type" 3)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key row) value))
    row))

(defun talk-message (&rest pairs)
  "One chat message: kim (user `kim') saying HI in room tok1, id 11."
  (let ((message (nlk:json-object "id" 11 "token" "tok1"
                                   "actorType" "users" "actorId" "kim"
                                   "actorDisplayName" "Kim"
                                   "message" "hi" "messageType" "comment"
                                   "systemMessage" nil)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key message) value))
    message))

(defun talk-section (&rest pairs)
  (let ((table (nlk:json-object)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key table) value))
    table))

(deftest channel-nextcloud-talk-a-mentioned-message-is-a-candidate ()
  (let ((candidate (channel-nextcloud-talk:talk-candidate
                     (talk-message "message" "@nodebot what is up?")
                     :user-id "nodebot" :display-name "Node Bot")))
    (is (equal "what is up?" (gethash "text" candidate)))
    (is-source candidate "platform" "nextcloud-talk" "chat_kind" "group"
                "channel_id" "tok1" "user_id" "kim" "user_name" "Kim"
                "message_id" "11")
    (is (nck:candidate-addressed-p candidate) "a mention addresses the account")))

(deftest channel-nextcloud-talk-a-group-message-must-name-the-account ()
  (let ((plain (channel-nextcloud-talk:talk-candidate
                (talk-message) :user-id "nodebot" :display-name "Node Bot"))
        (named (channel-nextcloud-talk:talk-candidate
                (talk-message "message" "@\"Node Bot\" and then?")
                :user-id "nodebot" :display-name "Node Bot")))
    (is (not (nck:candidate-addressed-p plain)) "no mention, no address")
    (is (nck:candidate-addressed-p named) "Talk's quoted @\"Display Name\" addresses it")
    (is (equal "and then?" (gethash "text" named)) "the name is not handed to the model")))

(deftest channel-nextcloud-talk-a-one-to-one-room-and-a-free-room-answer ()
  (let ((dm (channel-nextcloud-talk:talk-candidate
             (talk-message) :user-id "nodebot" :room-type 1))
        (free (channel-nextcloud-talk:talk-candidate
               (talk-message) :user-id "nodebot" :room-type 3 :free-room-p t)))
    (is (equal "dm" (nck:source-field dm "chat_kind")) "type 1 is a one-to-one room")
    (is (nck:candidate-addressed-p dm) "a one-to-one room is said to the account by being one")
    (is (nck:candidate-addressed-p free) "a room listed as free answers every message")))

(deftest channel-nextcloud-talk-what-is-not-a-person-s-words ()
  (let ((own (channel-nextcloud-talk:talk-candidate
              (talk-message "actorId" "nodebot" "message" "@nodebot hi")
              :user-id "nodebot"))
        (system (channel-nextcloud-talk:talk-candidate
                 (talk-message "messageType" "system" "systemMessage" t)
                 :user-id "nodebot"))
        (empty (channel-nextcloud-talk:talk-candidate
                (talk-message "message" "  ") :user-id "nodebot")))
    (is (null own) "the account's own message")
    (is (null system) "a join or a call event is not an ask")
    (is (null empty) "no words, nothing to answer")))

(deftest channel-nextcloud-talk-a-mention-is-not-taken-from-code ()
  (is (channel-nextcloud-talk:talk-mentioned-p "hey @nodebot" :user-id "nodebot"))
  (is (not (channel-nextcloud-talk:talk-mentioned-p "use `@nodebot /approve` in that room"
                                                       :user-id "nodebot")) "a mention quoted in a code span is an example"))

(deftest channel-nextcloud-talk-a-message-plan-names-the-room ()
  (let ((plan (channel-nextcloud-talk:message-plan
               (list :channel-id "tok1" :message-id "11") "the answer" :reply-to "11")))
    (is (string= "POST" (nck::request-plan-method plan)))
    (is (search "/chat/tok1" (nck::request-plan-path plan)) "the room is in the path")
    (is (search "format=json" (nck::request-plan-path plan)) "and the JSON marker")
    (is (equal "the answer" (gethash "message" (nck::request-plan-body plan))))
    (is (eql 11 (gethash "replyTo" (nck::request-plan-body plan))) "the answer replies under the ask")))

(deftest channel-nextcloud-talk-a-send-is-read-for-its-id ()
  (is (equal "42" (channel-nextcloud-talk:message-id-of (talk-ocs (nlk:json-object "id" 42)))) "the id a send answered with")
  (is (null (channel-nextcloud-talk:message-id-of (nlk:json-object))) "no id, no message"))

(deftest channel-nextcloud-talk-the-app-password-is-required ()
  (handler-case
      (progn (channel-nextcloud-talk:probe-channel
              (talk-section "server_url" "https://cloud.example.com" "username" "nodebot")
              :executor (talk-scripted (talk-answer (talk-me))))
             (is nil "an unnamed app password must refuse"))
    (nlk:config-refusal (condition)
      (is (search "app_password" (princ-to-string condition)) "the refusal names the key"))))

(deftest channel-nextcloud-talk-a-probe-reads-the-account-and-its-rooms ()
  (let* ((path (nlk:home "channels/nextcloud-talk/probe-password"))
         (_ (progn (ensure-directories-exist path)
                   (nlk:write-file-atomically path "app-pass-9")))
         (text (channel-nextcloud-talk:probe-channel
                (talk-section "server_url" "https://cloud.example.com"
                              "username" "nodebot"
                              "app_password_file" (uiop:native-namestring path))
                :executor (talk-scripted (talk-answer (talk-me))
                                         (talk-answer (talk-ocs (vector (talk-room))))))))
    (is (search "Node Bot" text) "the account it is")
    (is (search "rooms: 1" text) "the rooms it is in")
    (is (search "Ops (group)" text) "a room is named with its kind")
    (is (not (search "app-pass-9" text)) "the probe never says the password")))

(deftest channel-nextcloud-talk-starts-and-stops-cleanly ()
  (let* ((path (nlk:home "channels/nextcloud-talk/start-password"))
         (_ (progn (ensure-directories-exist path)
                   (nlk:write-file-atomically path "app-pass-9")))
         (section (talk-section "server_url" "https://cloud.example.com"
                                "username" "nodebot"
                                "app_password_file" (uiop:native-namestring path)
                                "allowed_users" (vector "kim")))
         (stop (channel-nextcloud-talk:start-channel
                section
                :executor (talk-scripted (talk-answer (talk-me))
                                         (talk-answer (talk-ocs (vector (talk-room))))))))
    (is (functionp stop) "start answers its stop thunk")
    (funcall stop)
    (is t "and stopping is quiet")))
