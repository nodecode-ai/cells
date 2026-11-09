;;;; cell-test.lisp --- the MAX adapter's own tests. Offline: every call is
;;;; answered by a scripted executor, never a socket.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)



(defun max-answer (body &key (status 200))
  (nck::make-execution :ok-p (< status 400) :status status :attempts 1 :body body))

(defun max-gone (error)
  (nck::make-execution :ok-p nil :status 0 :attempts 1 :error error))

(defun max-scripted (&rest answers)
  "An executor answering ANSWERS in order, then a bare object."
  (let ((queue (copy-list answers)))
    (nck::make-plan-executor
     :run (lambda (plan)
            (declare (ignore plan))
            (or (pop queue) (max-answer (nlk:json-object)))))))

(defun max-me (&rest pairs)
  (let ((body (nlk:json-object "user_id" 900 "name" "Node Bot" "username" "nodebot")))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key body) value))
    body))

(defun max-update (&rest pairs)
  "One message_created update: kim (501) saying \"hi\" in chat 700."
  (let* ((message (nlk:json-object
                   "body" (nlk:json-object "mid" "mid.1" "text" "hi")
                   "sender" (nlk:json-object "user_id" 501 "name" "Kim" "is_bot" nil)
                   "recipient" (nlk:json-object "chat_id" 700 "chat_type" "dialog")))
         (update (nlk:json-object "update_type" "message_created" "message" message)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key update) value))
    update))

(defun max-rest (update key value)
  (setf (gethash key (gethash "message" update)) value)
  update)

(defun max-section (&rest pairs)
  (let ((table (nlk:json-object)))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key table) value))
    table))

(deftest channel-max-a-dialog-message-is-a-candidate ()
  (let ((candidate (ncm:max-candidate (max-update) :username "nodebot")))
    (is (equal "hi" (gethash "text" candidate)))
    (is-source candidate "platform" "max" "chat_kind" "dm" "channel_id" "700"
               "user_id" "501" "user_name" "Kim" "message_id" "mid.1")
    (is (nck:candidate-addressed-p candidate) "a dialog is said to the bot by being one")))

(deftest channel-max-a-group-message-must-name-the-bot ()
  (let* ((update (max-rest (max-update) "recipient"
                            (nlk:json-object "chat_id" 700 "chat_type" "chat")))
         (plain (ncm:max-candidate update :username "nodebot"))
         (named (ncm:max-candidate (max-rest update "body"
                                              (nlk:json-object "mid" "mid.2"
                                                               "text" "@nodebot and then?"))
                                    :username "nodebot")))
    (is (equal "group" (gethash "chat_kind" (gethash "source" plain))))
    (is (not (nck:candidate-addressed-p plain)) "no mention, no address")
    (is (nck:candidate-addressed-p named) "a mention addresses it")
    (is (equal "and then?" (gethash "text" named)))))

(deftest channel-max-what-is-not-a-person-s-words ()
  (let ((bot (max-rest (max-update) "sender"
                        (nlk:json-object "user_id" 900 "name" "Node Bot" "is_bot" t)))
        (edited (let ((each (max-update))) (setf (gethash "update_type" each) "message_edited") each))
        (empty (max-rest (max-update) "body" (nlk:json-object "mid" "mid.3" "text" ""))))
    (is (null (ncm:max-candidate bot :username "nodebot")) "a bot's own message")
    (is (null (ncm:max-candidate edited :username "nodebot")) "an edit is not a new ask")
    (is (null (ncm:max-candidate empty :username "nodebot")) "no words, nothing to answer")))

(deftest channel-max-a-message-plan-names-the-chat ()
  (let ((plan (ncm:message-plan (list :channel-id "700" :message-id "mid.1")
                                "the answer"
                                :reply-to "mid.1")))
    (is (string= "POST" (nck::request-plan-method plan)))
    (is (search "chat_id=700" (nck::request-plan-path plan)) "the chat is in the path")
    (is (search "access_token=" (nck::request-plan-path plan)) "so is the token's slot")
    (is (equal "the answer" (gethash "text" (nck::request-plan-body plan))))
    (is (equal "mid.1" (gethash "mid" (gethash "link" (nck::request-plan-body plan))))
        "the answer replies under the ask")))

(deftest channel-max-a-send-is-read-for-its-mid ()
  (is (equal "mid.9" (ncm:message-id-of
                       (nlk:json-object "message"
                                        (nlk:json-object "body" (nlk:json-object "mid" "mid.9"))))))
  (is (equal "8" (ncm:message-id-of (nlk:json-object "message_id" 8)))
      "a numeric id reads as its digits"))

(deftest channel-max-the-token-is-required ()
  (let ((section (max-section)))
    (handler-case (progn (ncm:probe-channel section :executor (max-scripted (max-answer (max-me))))
                         (is nil "an unnamed token must refuse"))
      (nlk:config-refusal (condition)
        (is (search "bot_token" (princ-to-string condition)) "the refusal names the key")))))

(deftest channel-max-a-probe-reads-the-bot ()
  (let ((text (ncm:probe-channel
               (max-section "bot_token_file"
                            (let ((path (nlk:home "channels/max/probe-token")))
                              (ensure-directories-exist path)
                              (nlk:write-file-atomically path "t0ken")
                              (uiop:native-namestring path)))
               :executor (max-scripted (max-answer (max-me))
                                       (max-answer (nlk:json-object
                                                     "updates" (vector (max-update))
                                                     "marker" 5))))))
    (is (search "Node Bot" text))
    (is (search "updates: 1 readable" text))
    (is (not (search "t0ken" text)) "the probe never says the token")))

(deftest channel-max-starts-and-stops-cleanly ()
  (let* ((path (nlk:home "channels/max/start-token"))
         (_ (progn (ensure-directories-exist path)
                   (nlk:write-file-atomically path "t0ken")))
         (section (max-section "bot_token_file" (uiop:native-namestring path)
                               "allowed_users" (vector "501")
                               "poll_seconds" 1))
         (stop (ncm:start-channel
                section
                :executor (max-scripted (max-answer (max-me))
                                         (max-answer (nlk:json-object
                                                       "updates" (vector (max-update))
                                                       "marker" 5))))))
    (is (functionp stop) "start answers its stop thunk")
    (funcall stop)
    (is t "and stopping is quiet")))
