;;;; rest-test.lisp --- Slack Web API plans, the thread book, the envelope.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun slack-target (channel &optional message-id)
  (list :channel-id channel :thread-id nil :message-id message-id))

(deftest channel-slack-replies-land-in-the-ask-thread ()
  (let ((book (ncs:make-thread-book)))
    ;; In a channel an answer hangs off its ask; in a DM it is the next line.
    (is (equal "1.1" (ncs:reply-thread book (slack-target "C1") "1.1")))
    (is (null (ncs:reply-thread book (slack-target "D1") "1.1")))
    (is (null (ncs:reply-thread book (slack-target "C1") nil)))
    ;; A reply to a message inside a thread goes in that thread, DM or not.
    (ncs:note-thread-message book "C1" "2.2" "1.1")
    (ncs:note-thread-message book "D1" "2.3" "1.2")
    (ncs:note-thread-message book "C1" "1.1" "1.1")
    (is (equal "1.1" (ncs:reply-thread book (slack-target "C1") "2.2")))
    (is (equal "1.2" (ncs:reply-thread book (slack-target "D1") "2.3")))
    (is (equal "1.1" (ncs:reply-thread book (slack-target "C1") "1.1")))
    ;; Posting in a thread is what makes the bot a speaker there.
    (is (not (ncs:bot-thread-p book "C1" "1.1")))
    (ncs:message-plan book (slack-target "C1") (first-chunk "hi") :reply-to "2.2")
    (is (ncs:bot-thread-p book "C1" "1.1"))
    (is (not (ncs:bot-thread-p book "C2" "1.1")))))

(deftest channel-slack-message-is-a-markdown-block ()
  (let* ((book (ncs:make-thread-book))
         (chunks (nck:split-text-chunks (format nil "# Title~%~%**bold** and `code`") 12000))
         (plan (ncs:message-plan book (slack-target "C1") (first chunks) :reply-to "1.1"
                                 :mentions '("U7")
                                 :controls '(("Stop" "nck:stop:s" :danger)
                                             ("Tools" "nck:tools:s" :secondary)))))
    (is-plan plan :method "POST" :path "/chat.postMessage" :label "send_message" :retry t
                  "channel" "C1" "thread_ts" "1.1")
    (let ((blocks (plan-field plan "blocks")))
      (is (equal "markdown" (nlk:json-value (aref blocks 0) :string "type")))
      (is (equal (format nil "<@U7> # Title~%~%**bold** and `code`")
                 (nlk:json-value (aref blocks 0) :string "text")))
      (is-present (buttons (nlk:json-array (aref blocks 1) "elements")) "the controls ride"
        (is (= 2 (length buttons)))
        (is (equal "danger" (nlk:json-value (aref buttons 0) :string "style")))
        (is (equal "nck:stop:s" (nlk:json-value (aref buttons 0) :string "value")))
        (is (null (nlk:json-value (aref buttons 1) :string "style")))
        (is (not (equal (nlk:json-value (aref buttons 0) :string "action_id")
                        (nlk:json-value (aref buttons 1) :string "action_id"))))))
    ;; The notification's words are the text on one line.
    (is (equal (format nil "<@U7> # Title **bold** and `code`") (plan-field plan "text"))))
  ;; A later chunk carries no controls and no mention, in the same thread.
  (let* ((chunks (nck:split-text-chunks (format nil "one~%two") 4))
         (plan (ncs:message-plan (ncs:make-thread-book) (slack-target "C1") (second chunks)
                                 :reply-to "1.1" :mentions '("U7") :controls '(("Stop" "x" :danger)))))
    (is-plan plan :label "send_message_chunk_2_of_2" "thread_ts" "1.1")
    (is (= 1 (length (plan-field plan "blocks"))))
    (is (equal "two" (nlk:json-value (aref (plan-field plan "blocks") 0) :string "text")))))

(deftest channel-slack-card-controls-are-buttons-of-words ()
  ;; The kit's running card sends Stop beside Details as one row; Slack
  ;; refuses the whole post (invalid_blocks) unless every button's text is
  ;; words and its value the data.
  (flet ((buttons (controls)
           (let ((blocks (ncs::message-blocks "card" controls)))
             (if (> (length blocks) 1)
                 (coerce (nlk:json-array (aref blocks 1) "elements") 'list)
                 '())))
         (words (button) (nlk:json-value (nlk:json-value button :object "text") :string "text"))
         (value (button) (nlk:json-value button :string "value")))
    (let ((row (buttons '((("Stop" "nck:stop:s" :danger) ("Details" "nck:details:s" :secondary))))))
      (is (equal '("Stop" "Details") (mapcar #'words row)))
      (is (equal '("nck:stop:s" "nck:details:s") (mapcar #'value row)))
      (is (equal "danger" (nlk:json-value (first row) :string "style"))))
    (is (equal '("Details") (mapcar #'words (buttons '(("Details" "nck:details:s" :secondary))))))
    (is (equal '("Go") (mapcar #'words (buttons '((("Go" "g" :primary) ("Off" "o" :secondary t)))))))
    (is (equal '("✓ One · first" "Two")
               (mapcar #'words (buttons '((:menu "pick" (("One" "m:1" "first" t) ("Two" "m:2" nil nil))))))))
    (is (= 75 (length (words (first (buttons (list (list (make-string 90 :initial-element #\a) "x" nil))))))))
    (is (null (buttons :clear)))))

(deftest channel-slack-press-answer-reaches-the-presser-alone ()
  ;; A card's Details press answers in words only its presser sees, in the
  ;; thread the card lives in; a press the kit answers without words plans
  ;; nothing.
  (let ((plans (ncs::ack-plan '(:id "6.6" :user-id "U1" :channel-id "C1" :thread-id "4.4")
                              :text (format nil "Ran 2 commands~%Waited 30s"))))
    (is (= 1 (length plans)))
    (is-plan (first plans) :method "POST" :path "/chat.postEphemeral" :label "control_ack"
                           "channel" "C1" "user" "U1" "thread_ts" "4.4"
                           "text" "Ran 2 commands Waited 30s")
    (is (equal (format nil "Ran 2 commands~%Waited 30s")
               (nlk:json-value (aref (plan-field (first plans) "blocks") 0) :string "text"))))
  (is (null (ncs::ack-plan '(:id "6.6" :user-id "U1" :channel-id "D1")))))

(deftest channel-slack-edit-delete-status-reaction ()
  (let ((edit (ncs:edit-plan (slack-target "C1") "5.5" "done" :controls :clear)))
    (is-plan edit :path "/chat.update" :retry nil "channel" "C1" "ts" "5.5")
    (is (= 1 (length (plan-field edit "blocks"))) "an edit sends the blocks whole: no buttons"))
  (is-plan (ncs:edit-plan (slack-target "C1") "5.5" "done" :retry t) :retry t)
  (is-plan (ncs:delete-plan (slack-target "C1") "5.5") :path "/chat.delete" :retry t
           "channel" "C1" "ts" "5.5")
  (let ((book (ncs:make-thread-book)))
    (is-plan (ncs:typing-plan book (slack-target "C1" "1.1"))
             :path "/assistant.threads.setStatus" :label "typing_indicator"
             "channel_id" "C1" "thread_ts" "1.1" "status" "is working...")
    (is (null (ncs:typing-plan book (slack-target "D1" "1.1")))
        "a DM answers in the channel, where Slack shows no status"))
  (let ((plans (ncs:reaction-plans (slack-target "C1") "5.5" "👀")))
    (is (= 1 (length plans)))
    (is-plan (first plans) :path "/reactions.add" "channel" "C1" "timestamp" "5.5" "name" "eyes"))
  (let ((plans (ncs:reaction-plans (slack-target "C1") "5.5" nil :previous "👀")))
    (is-plan (first plans) :path "/reactions.remove" "name" "eyes"))
  (is (null (ncs:reaction-plans (slack-target "C1") "5.5" "👀" :previous "👀"))))

(deftest channel-slack-slash-command-answers-through-its-url ()
  (let ((command (ncs:slack-command-candidate
                  (nlk:json-object "text" "help" "channel_id" "C1" "user_id" "U1"
                                   "response_url" "https://hooks.slack.com/commands/1"))))
    (is-plan (ncs:respond-plan command "the answer")
             :method "POST" :path "https://hooks.slack.com/commands/1"
             "response_type" "in_channel" "text" "the answer"))
  (is (null (ncs:respond-plan (ncs:slack-message-candidate (slack-message "C1" "1.1" "hi"))
                              "the answer"))
      "a message is answered as a reply"))

(deftest channel-slack-envelope-says-ok ()
  ;; Slack refuses with status 200: the envelope's ok decides.
  (let ((executor (ncs:wrap-slack-executor
                   (scripted-executor (reply 200 "ok" nil "error" "channel_not_found")
                                      (reply 200 "ok" nil "error" "already_reacted")
                                      (reply 200 "ok" t "ts" "9.9")))))
    (let ((refused (nck:execute-plan executor (nck:rest-plan "POST" "/chat.postMessage"
                                                             "send_message" nil 5))))
      (is (not (nck:execution-ok-p refused)))
      (is (search "channel_not_found" (nck:execution-error refused))))
    (let ((settled (nck:execute-plan executor (nck:rest-plan "POST" "/reactions.add"
                                                             "add_reaction" nil 5))))
      (is (nck:execution-ok-p settled) "already on is what was asked"))
    (let ((posted (nck:execute-plan executor (nck:rest-plan "POST" "/chat.postMessage"
                                                            "send_message" nil 5))))
      (is (equal "9.9" (ncs:slack-message-id (nck:execution-body posted)))))
    (is (nck:execution-ok-p (nck:execute-plan executor nil))
        "a status beat Slack cannot show is done")))

(deftest channel-slack-address-carries-the-channel ()
  (is (equal "C1:5.5" (ncs:slack-address (slack-target "C1") "5.5")))
  (is (not (equal (ncs:slack-address (slack-target "C1") "5.5")
                  (ncs:slack-address (slack-target "C2") "5.5")))))
