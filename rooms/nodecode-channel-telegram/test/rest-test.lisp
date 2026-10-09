;;;; rest-test.lisp --- Bot API plans, failure classifier, body retry_after.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(nlk:access (document nck::request-plan) (photo nck::request-plan) (second-plan nck::request-plan))

(defun telegram-wrapped (plan body &optional (status 200))
  "PLAN executed through WRAP-TELEGRAM-EXECUTOR over a recording executor
that answers STATUS with BODY."
  (nck:execute-plan (nct:wrap-telegram-executor
                     (scripted-executor (nck:make-scripted-response status body)))
                    plan))

(deftest channel-telegram-message-plans ()
  (let ((plan (nct:send-message-plan '(:channel-id "555" :thread-id "33")
                                     (first-chunk "hello") :ping t)))
    (is-plan plan :path "/sendRichMessage" :retry t :label "send_rich_message" "chat_id" "555"
                  "message_thread_id" 33 (:string "rich_message" "markdown") "hello" "text" nil
                  "disable_notification" nil)
    (is (null (gethash "reply_parameters" (nck:request-plan-body plan))) "no ask, no reply"))
  (is-plan (nct:send-message-plan '(:channel-id "555") (first-chunk "hi"))
           "message_thread_id" nil "disable_notification" t)
  ;; The answer replies to the ask on chunk 1 only.
  (let* ((chunks (nck:split-text-chunks (make-string 5000 :initial-element #\y)
                                        4096))
         (first-plan (nct:send-message-plan '(:channel-id "555") (first chunks)
                                            :reply-to "12" :ping t))
         (second-plan (nct:send-message-plan '(:channel-id "555") (second chunks)
                                             :reply-to "12" :ping t)))
    (is (= 2 (length chunks)))
    (is-plan first-plan (:any "reply_parameters" "message_id") 12
             (:boolean "reply_parameters" "allow_sending_without_reply") t)
    (is-plan second-plan "reply_parameters" nil :label "send_rich_message_chunk_2_of_2"))
  ;; An edit carrying text would make the rich message plain.
  (is-plan (nct:edit-message-plan '(:channel-id "555" :thread-id "33") "42" "updated")
           :path "/editMessageText" :retry nil
           "message_id" 42 (:string "rich_message" "markdown") "updated" "text" nil "message_thread_id" nil)
  (is-plan (nct:edit-message-plan '(:channel-id "555") "42" "final" :retry t) :retry t)
  (is-plan (nct:delete-message-plan '(:channel-id "555" :thread-id "33") "42")
           :path "/deleteMessage" :retry t
           :label "delete_message" "chat_id" "555" "message_id" 42 "message_thread_id" nil)
  (is-plan (nct:typing-plan '(:channel-id "555" :thread-id "33"))
           :path "/sendChatAction" :retry nil
           :label "typing_indicator" "action" "typing" "chat_id" "555" "message_thread_id" 33))

(deftest channel-telegram-controls-are-an-inline-keyboard ()
  ;; A running card's Stop and Details share one keyboard row (2026-10-04:
  ;; read as one button, the row put a list in `text' and Telegram refused
  ;; every post of a running card, 400). A lone button is a row of its own; a
  ;; menu is one row per option, the current one checked; a disabled button,
  ;; which Telegram cannot draw, and one whose data passes 64 bytes, which
  ;; would refuse the whole message, are left out.
  (flet ((keyboard (controls)
           (nlk:json-value (nct::controls-markup controls) :array "inline_keyboard"))
         (button (row index &optional (column 0))
           (aref (aref row index) column)))
    (let ((rows (keyboard (list (list (list "Stop" "nck:stop:s1" :danger)
                                      (list "Details" "nck:details:s1" :secondary))))))
      (is (= 1 (length rows)) "Stop and Details on one row")
      (is (= 2 (length (aref rows 0))))
      (is-shape (button rows 0) ((:string "text") "Stop") ((:string "callback_data") "nck:stop:s1")
        ((:string "style") "danger" "Stop is red"))
      (is-shape (button rows 0 1) ((:string "text") "Details") ((:string "callback_data") "nck:details:s1")
        ((:string "style") null "Details wears Telegram's own face")))
    (let ((rows (keyboard (list (list "Details" "nck:details:s1" :secondary)))))
      (is (= 1 (length rows)))
      (is-shape (button rows 0) ((:string "text") "Details")))
    (let ((rows (keyboard (list (list :menu "Select provider"
                                      (list (nck:menu-choice "p1" "/models p1" :description "2 models" :current t)
                                            (nck:menu-choice "p2" "/models p2")
                                            (nck:menu-choice "long" (make-string 60 :initial-element #\m))))
                                (list (nck:choice "Prev" "/models page 0" :disabled t)
                                      (nck:choice "Next" "/models page 2"))
                                (list (nck:choice "Off" "/models page 9" :disabled t))))))
      (is (= 3 (length rows)) "two options, the pager's live half; a row with nothing left is none")
      (is-shape (button rows 0) ((:string "text") "✓ p1 · 2 models") ((:string "callback_data") "nck:say:/models p1"))
      (is-shape (button rows 1) ((:string "text") "p2"))
      (is (= 1 (length (aref rows 2))) "the disabled Prev is left out")
      (is-shape (button rows 2) ((:string "text") "Next"))))
  (is (zerop (length (nlk:json-value (nct::controls-markup :clear) :array "inline_keyboard"))))
  (let ((rows (nlk:json-value (nck:request-plan-body
                               (nct:send-message-plan '(:channel-id "555") (first-chunk "Working · 3s")
                                                      :controls (list (list (list "Stop" "nck:stop:s1" :danger)
                                                                            (list "Details" "nck:details:s1" :secondary)))))
                              :array "reply_markup" "inline_keyboard")))
    (is-shape (aref (aref rows 0) 1) ((:string "text") "Details" "the card's post carries the row"))))

(deftest channel-telegram-a-card-is-a-rich-message ()
  ;; A card is Telegram's own blocks, laid out as Discord's container: the
  ;; phase a pill (a disabled button, blue working, red failed, grey stopped),
  ;; the task a heading, what the turn does now beside its thought, the words
  ;; it said, its steps a checklist (one a failure cut short struck through),
  ;; a rule over the footer's earlier steps and numbers, and its buttons inside
  ;; it, Stop red; every word escaped. Posted, it is a sendRichMessage taking
  ;; its words as written; edited, an editMessageText carrying it; a card's
  ;; buttons never ride a keyboard.
  (let* ((working (list :state :working :elapsed "1m 46s" :task "any news <today>" :headline "Thinking"
                        :thought "Also the adapted headline" :said (format nil "a & b~%c") :earlier 2
                        :steps '((:done "Ran find . | head" "0s" nil) (:done "Searched 2 patterns" "7s" nil))
                        :meta (format nil "deepseek/deepseek-flash (max) · 4 steps~%↑37.2k · $0.013")))
         (running (list (list (list "Stop" "nck:stop:s1" :danger) (list "Details" "nck:details:s1" :secondary))))
         (post (nct:send-message-plan '(:channel-id "555") (first-chunk "plain") :card working :controls running
                                                                                 :reply-to "12")))
    (is (equal (concatenate 'string
                            "<tg-button-row align=\"left\"><tg-button type=\"disabled\" style=\"primary\">"
                            "Working · 1m 46s</tg-button></tg-button-row>"
                            "<h3>any news &lt;today&gt;</h3>"
                            "<p><b>Thinking</b> · <i>Also the adapted headline</i></p>"
                            "<p>a &amp; b<br>c</p>"
                            "<ul><li><input type=\"checkbox\" checked>Ran find . | head · 0s</li>"
                            "<li><input type=\"checkbox\" checked>Searched 2 patterns · 7s</li></ul>"
                            "<hr/><footer>+2 earlier · deepseek/deepseek-flash (max) · 4 steps<br>↑37.2k · $0.013</footer>"
                            "<tg-button-row align=\"left\">"
                            "<tg-button type=\"callback_data\" style=\"danger\" data=\"nck:stop:s1\">Stop</tg-button>"
                            "<tg-button type=\"callback_data\" data=\"nck:details:s1\">Details</tg-button>"
                            "</tg-button-row>")
               (nct::card-html working running)))
    (is-plan post :path "/sendRichMessage" :label "send_rich_message"
                  (:string "rich_message" "html") (nct::card-html working running)
                  (:boolean "rich_message" "skip_entity_detection") t
                  (:any "reply_parameters" "message_id") 12
                  "disable_notification" t "text" nil "reply_markup" nil "link_preview_options" nil)
    (is (search "target chat is unavailable" (nct::telegram-failure-message post 400 "Bad Request: chat not found")))
    (is-plan (nct:edit-message-plan '(:channel-id "555") "42" "plain" :card working :controls running)
             :path "/editMessageText" (:string "rich_message" "html") (nct::card-html working running)
             "text" nil "reply_markup" nil))
  (is (equal (concatenate 'string
                          "<tg-button-row align=\"left\"><tg-button type=\"disabled\" style=\"danger\">"
                          "Failed at 9s</tg-button></tg-button-row>"
                          "<p><i>provider 502</i></p>"
                          "<ul><li><input type=\"checkbox\" checked>Read a file · 1s</li>"
                          "<li><input type=\"checkbox\"><s>Ran just test</s> · 8s</li></ul>"
                          "<tg-button-row align=\"left\">"
                          "<tg-button type=\"callback_data\" data=\"nck:details:s1\">Details</tg-button>"
                          "</tg-button-row>")
             (nct::card-html (list :state :failed :elapsed "9s" :headline "Failed at 9s" :note "provider 502"
                                   :steps '((:done "Read a file" "1s" nil) (:stopped "Ran just test" "8s" nil)))
                             (list (list "Details" "nck:details:s1" :secondary)))))
  ;; A stopped card's pill wears Telegram's own face; while a step runs, the
  ;; thought stands alone and the step waits unticked.
  (is (search "<tg-button type=\"disabled\">Stopped at 40s</tg-button>"
              (nct::card-html (list :state :stopped :elapsed "40s" :headline "Stopped at 40s") :clear)))
  (let ((html (nct::card-html (list :state :working :elapsed "4s" :task "lint" :headline "Running just lint"
                                    :thought "Checking" :steps '((:running "Running just lint" "3s" nil))))))
    (is (search "<p><i>Checking</i></p>" html))
    (is (search "<li><input type=\"checkbox\">Running just lint · 3s</li>" html))))

(deftest channel-telegram-words-are-rich-markdown ()
  ;; A message's words are Markdown Telegram parses itself, kept saying what
  ;; Discord draws them saying (each habit probed against the Bot API,
  ;; 2026-10-04). A rich message unfurls no link, so a link stays a link
  ;; with no preview options to ask it.
  (is-each (nct::rich-markdown)
    ("Vec<String> and /agent <name>" "Vec&lt;String> and /agent &lt;name>"
     "a tag Telegram does not know vanishes, so a < outside code is an entity")
    ("`<b>` and ``a ` <b>`` stay, a lone ` <i> does not"
     "`<b>` and ``a ` <b>`` stay, a lone ` &lt;i> does not"
     "a code span passes as written; an unclosed tick opens none")
    ("Tom & Jerry, [q](https://x.test/?a=1&b=2)" "Tom & Jerry, [q](https://x.test/?a=1&b=2)"
     "an & stays, a link's address decoding no entity")
    ("See ![chart](https://x.test/c.png)" "See \\![chart](https://x.test/c.png)"
     "a picture by a link that holds none would refuse the whole message")
    ((format nil "#hashtag~%## Heading~%#") (format nil "\\#hashtag  ~%## Heading  ~%#")
     "a leading # that opens no heading is escaped")
    ((format nil "one~%two~%~%three") (format nil "one  ~%two~%~%three")
     "a lone line break holds; a blank line parts paragraphs")
    ((format nil "Here:~%| a | b |~%|---|---|~%| <x> | 2 |")
     (format nil "Here:  ~%~%| a | b |  ~%|---|---|  ~%| &lt;x> | 2 |")
     "a table gets the blank line over it Telegram needs")
    ((format nil "```html~%<p>a~%b</p>~%```~%after <x>") (format nil "```html~%<p>a~%b</p>~%```~%after &lt;x>")
     "a fence passes as written, its lines as they are"))
  (is-plan (nct:send-message-plan '(:channel-id "555") (first-chunk "see https://example.com <b>"))
           :path "/sendRichMessage" (:string "rich_message" "markdown") "see https://example.com &lt;b>"
           "text" nil "link_preview_options" nil))

(deftest channel-telegram-an-addressed-post-notifies ()
  ;; Telegram spells a bare user id no mention, so the addressed post simply
  ;; notifies: the note that needs the operator reaches them.
  (is-plan (nct:send-message-plan '(:channel-id "555")
                                  (first-chunk "⚠ needs your attention")
                                  :mentions '("8071918233"))
           :path "/sendRichMessage" :retry t "disable_notification" nil
           (:string "rich_message" "markdown") "⚠ needs your attention"))

(deftest channel-telegram-reaction-plans ()
  ;; One setMessageReaction per transition: Telegram replaces the bot's
  ;; whole reaction set, so the previous emoji is never needed and clearing
  ;; is the empty set.
  (let ((plans (nct:reaction-plans '(:channel-id "555" :thread-id "33") "42"
                                   "👀" :previous nil)))
    (is (= 1 (length plans)) "one call sets the reaction")
    (let ((plan (first plans)))
      (is-plan plan :path "/setMessageReaction" :label "set_message_reaction" :retry t
                    "chat_id" "555" "message_id" 42 "message_thread_id" nil)
      (is-shape (aref (plan-field plan "reaction") 0) ("emoji" "👀") ("type" "emoji"))))
  (let ((plans (nct:reaction-plans '(:channel-id "555") "42" "✍"
                                   :previous "👀")))
    (is (= 1 (length plans)) "a change is still one call — the set is replaced")
    (is (equal "✍" (gethash "emoji" (aref (plan-field (first plans) "reaction") 0)))))
  (let ((plans (nct:reaction-plans '(:channel-id "555") "42" nil
                                   :previous "✍")))
    (is (= 1 (length plans)))
    (is (equalp #() (gethash "reaction" (nck:request-plan-body (first plans))))))
  ;; The kit's outcome marks are not in Telegram's fixed set: each is said
  ;; with the one of its own that means the same.
  (flet ((glyph (mark)
           (let ((plan (first (nct:reaction-plans '(:channel-id "555") "42" mark :previous "👀"))))
             (gethash "emoji" (aref (plan-field plan "reaction") 0)))))
    (is (equal "👍" (glyph nck:+reaction-done+)))
    (is (equal "👎" (glyph nck:+reaction-failed+)))))

(deftest channel-telegram-body-retry-after ()
  (is-table (expected json) (eql expected (nct:telegram-retry-after-ms 429 (cell-json json) nil))
    (7000 "{\"ok\": false, \"parameters\": {\"retry_after\": 7}}") (nil "{}"))
  (is (null (nct:telegram-retry-after-ms 429 nil nil)))
  ;; End to end through the retry loop: the body value drives the delay.
  (let ((result (scripted-execution (list (reply 429 "parameters" (nlk:json-object "retry_after" 2))
                                          (nck:make-scripted-response 200))
                                    :retry t :retry-after-fn #'nct:telegram-retry-after-ms)))
    (is-shape result (nck:execution-ok-p is) (nck:execution-retry-delays '(2000)))))

(deftest channel-telegram-failure-classifier ()
  (let ((send (nct:send-message-plan '(:channel-id "5") (first-chunk "x")))
        (poll (nct:get-updates-plan nil)))
    (is-table (needle plan status detail)
      (search needle (nct:telegram-failure-message plan status detail))
      ("forum topic" send 400 "Bad Request: message thread not found")
      ("allowed_chats" send 403 "Forbidden: bot was blocked by the user")
      ("rate limited" send 429 "Too Many Requests")
      ("duplicate Telegram poller" poll 409 "Conflict: terminated by other getUpdates request")
      ("status 500" send 500 "oops"))))

(deftest channel-telegram-acknowledgement-is-not-state ()
  ;; Telegram answers a pin, a reaction, a deletion and the command menu
  ;; with {"ok":true,"result":true} — a boolean that says the request was
  ;; accepted and nothing about what is there. The wrapper answers it as no
  ;; body at all, which is what makes NCK:CALL's read-back see a Telegram
  ;; write for what it is (T-002).
  (is (nct:telegram-acknowledgement-p
       (cell-json "{\"ok\":true,\"result\":true}")))
  (is-table (json) (not (nct:telegram-acknowledgement-p (cell-json json)))
    ("{\"ok\":true,\"result\":{\"message_id\":90}}")
    ("{\"ok\":true,\"result\":[]}"))
  (is (not (nct:telegram-acknowledgement-p nil)))
  (let ((result (telegram-wrapped (first (nct:reaction-plans '(:channel-id "5") "9" "x"))
                                  (cell-json "{\"ok\":true,\"result\":true}"))))
    (is-shape result (nck:execution-ok-p is "still a success")
      (nck:execution-status = 200 "still its own status") (nck:execution-body null)))
  (let* ((created (cell-json "{\"ok\":true,\"result\":{\"message_id\":90}}"))
         (result (telegram-wrapped (nct:send-message-plan '(:channel-id "5") (first-chunk "hi"))
                                   created)))
    (is (eq created (nck:execution-body result)))))

(deftest channel-telegram-read-back-path ()
  ;; Which object a write names. Telegram addresses objects in the BODY, so
  ;; this is a lookup where Discord gets to truncate a path, and the read
  ;; carries its ids in a query string.
  (is-each (nct:read-back-path)
    ("/pinChatMessage" '(:chat_id -100123 :message_id 77) "/getChat?chat_id=-100123"
     "a pin reads back the chat, whose pinned_message answers for it")
    ("/unpinChatMessage" '(:chat_id -100123) "/getChat?chat_id=-100123" "and so does unpinning")
    ("/setChatTitle" '(:chat_id -100123 :title "peas") "/getChat?chat_id=-100123"
     "a title change reads back the chat that carries it")
    ("/promoteChatMember" '(:chat_id -100123 :user_id 42 :can_pin_messages t)
     "/getChatMember?chat_id=-100123&user_id=42" "a right reads back the member")
    ("/setMyCommands" '(:commands #()) "/getMyCommands" "the menu reads back as the menu")
    ("/pinChatMessage" (nlk:json-object "chat_id" -100123 "message_id" 77)
     "/getChat?chat_id=-100123" "a body the model built as an object reads the same")
    ("/setMessageReaction" '(:chat_id -100123 :message_id 77 :reaction #()) nil
     "nothing here reads a message, so a reaction cannot be confirmed")
    ("/deleteMessage" '(:chat_id -100123 :message_id 77) nil "nor a deletion")
    ("/sendMessage" '(:chat_id -100123 :text "hi") nil "and a send is already its own evidence")
    ("/pinChatMessage" nil nil "a write with no chat in it names no chat")
    ("/getMe" nil nil nil))
  (is (equal "pinChatMessage" (nct::telegram-method-name "/pinChatMessage")))
  (is (equal "getChat" (nct::telegram-method-name "/getChat?chat_id=5"))))

(deftest channel-telegram-not-modified-is-success ()
  (is (nct:telegram-not-modified-p
       "Bad Request: message is not modified: specified new message content"))
  (is (nct:telegram-not-modified-p
       (cell-json "{\"ok\":false,\"error_code\":400,\"description\":\"Bad Request: message is not modified\"}")))
  (is (not (nct:telegram-not-modified-p
            "Bad Request: message thread not found")))
  (is (nck:execution-ok-p
       (telegram-wrapped (nct:edit-message-plan '(:channel-id "5") "9" "same")
                         (cell-json "{\"ok\":false,\"error_code\":400,\"description\":\"Bad Request: message is not modified\"}")
                         400)))
  (is (not (nck:execution-ok-p
            (telegram-wrapped (nct:send-message-plan '(:channel-id "5") (first-chunk "x"))
                              (cell-json "{\"ok\":false,\"error_code\":400,\"description\":\"Bad Request: message thread not found\"}")
                              400)))))

;;; --- the probe ----------------------------------------------------------------

(defun telegram-probe-responses ()
  "getMe, then getUpdates with two chats and two writers — Mike's private chat
written twice, so a probe that names each once is seen to."
  (flet ((update (id chat from)
           (nlk:json-object "update_id" id
                            "message" (nlk:json-object "chat" chat "from" from))))
    (list (reply 200 "ok" t "result" (nlk:json-object "id" 123 "username" "nodecode_bot"))
          (reply 200 "ok" t
                 "result" (vector (update 1 (nlk:json-object "id" 4589 "type" "private"
                                                             "first_name" "Mike")
                                          (nlk:json-object "id" 4589 "username" "mike"))
                                  (update 2 (nlk:json-object "id" 4589 "type" "private"
                                                             "first_name" "Mike")
                                          (nlk:json-object "id" 4589 "username" "mike"))
                                  (update 3 (nlk:json-object "id" -100 "type" "supergroup"
                                                             "title" "peas")
                                          (nlk:json-object "id" 77 "first_name" "Ann")))))))

(deftest channel-telegram-probe-lists-the-chats-that-wrote ()
  (with-temp-file (token-path :contents "123:tok-secret" :type "txt")
    (let ((section (nlk:json-object "token_file" token-path)))
      (let* ((executor (nck:make-recording-executor :responses (telegram-probe-responses)))
             (text (nct:probe-channel section :executor executor)))
        (is (search "token: ok — bot @nodecode_bot (id 123)" text))
        (is (search "chat \"Mike\" (private, chat id 4589, from mike user id 4589)" text))
        (is (search "chat \"peas\" (supergroup, chat id -100, from Ann user id 77)" text))
        (is (not (search "tok-secret" text)) "the token is never in the answer")
        (is (equal '("/getMe" "/getUpdates")
                   (mapcar #'nck:request-plan-path (nck:recording-executor-plans executor))))
        (is-plan (second (nck:recording-executor-plans executor)) "timeout" 0)
        (is (null (nth-value 1 (gethash "offset" (nck:request-plan-body
                                                  (second (nck:recording-executor-plans executor))))))))
      (let* ((executor (scripted-executor
                        (reply 200 "ok" t "result" (nlk:json-object "id" 123 "username" "b"))
                        (reply 409 "ok" nil "description" "Conflict: terminated by other getUpdates request")))
             (text (nct:probe-channel section :executor executor)))
        (is (search "the telegram lane is running" text)))
      (let* ((executor (scripted-executor (reply 401 "ok" nil "description" "Unauthorized")))
             (text (nct:probe-channel section :executor executor)))
        (is (search "token: unauthorized (401)" text))
        (is (not (search "tok-secret" text)))))))

(deftest channel-telegram-declares-its-section-and-offers-chats-and-users-by-name ()
  (let ((section (nlk:find-section '("channels" "telegram"))))
    (is-present section "the adapter declares channels.telegram"
      (is (equal "nodecode-channel-telegram" (nlk:section-owner section)))
      (is (equal '(("token_env" "token_file")) (nlk:section-one-of section)))
      (is (equal '(("allowed_chats" "allowed_users")) (nlk:section-any-of section)))
      (is (functionp (nlk:section-check section)))
      (is (search "@BotFather" (nlk:section-text section)) "the guide is the walk")))
  (with-temp-file (token-path :contents "123:tok-secret" :type "txt")
    (let ((section (nlk:json-object "token_file" token-path)))
      (is (equal '(("4589" . "Mike  private") ("-100" . "peas  supergroup"))
                 (nct:probe-chat-choices
                  section :executor (nck:make-recording-executor
                                     :responses (telegram-probe-responses)))))
      (is (equal '(("4589" . "mike") ("77" . "Ann"))
                 (nct:probe-user-choices
                  section :executor (nck:make-recording-executor
                                     :responses (telegram-probe-responses)))))
      (let ((refusal (signals-error nlk:config-refusal
                       (nct:probe-chat-choices
                        section :executor (scripted-executor
                                           (reply 200 "ok" t "result"
                                                  (nlk:json-object "id" 1 "username" "b"))
                                           (reply 200 "ok" t "result" #()))))))
        (is (and refusal (search "none have written to the bot yet" (princ-to-string refusal))))))))

(deftest channel-telegram-set-my-commands-plan ()
  ;; The catalog as Telegram's menu: names outside its grammar are left out
  ;; rather than failing the whole call, descriptions are cut to 256 and
  ;; never empty, and the plan is retried — nothing else sets the menu.
  (let* ((plan (nct:set-my-commands-plan
                (list (list :name "help" :description "Show commands" :usage "")
                      (list :name "models"
                            :description (make-string 300 :initial-element #\d)
                            :usage "/models [provider]")
                      (list :name "Bad-Name" :description "x" :usage "")
                      (list :name "bare" :description "" :usage ""))
                :timeout-seconds 7))
         (commands (gethash "commands" (nck:request-plan-body plan))))
    (is-plan plan :path "/setMyCommands" :retry t :label "set_my_commands" :timeout 7)
    (is (= 3 (length commands)))
    (is-shape (aref commands 0) ("command" "help") ("description" "Show commands"))
    (is (= 256 (length (gethash "description" (aref commands 1)))))
    (is (equal "bare" (gethash "description" (aref commands 2))))
    (is (nct:telegram-command-name-p "memory_index"))
    (is (not (nct:telegram-command-name-p "memory-index")))
    (is (not (nct:telegram-command-name-p "")))))

(deftest channel-telegram-file-plans ()
  ;; getFile names the path; the file root — the token's, set at start —
  ;; joins it into the one URL a download uses, and a stopped lane has no
  ;; root, so the URL is refused rather than guessed.
  (is-plan (nct:get-file-plan "FILE123")
           :path "/getFile" :label "get_file" :retry t "file_id" "FILE123")
  (let ((nct::*telegram-file-root* "https://api.telegram.org/file/botTOKEN"))
    (is (equal "https://api.telegram.org/file/botTOKEN/photos/x.jpg"
               (nct:telegram-file-url "photos/x.jpg"))))
  (let ((nct::*telegram-file-root* nil))
    (is (signals-error error (nct:telegram-file-url "photos/x.jpg")))))

(deftest channel-telegram-answer-carries-its-files ()
  ;; The answer's own picture: one file as the caption-carrying file
  ;; message, several as one media group.
  (let ((plan (nct:send-message-plan '(:channel-id "1") (first-chunk "the answer")
                                    :files (list #p"/tmp/notes.pdf"))))
    (is-plan plan :path "/sendDocument" "caption" "the answer" "document" #P"/tmp/notes.pdf"))
  (let ((plan (nct:send-message-plan '(:channel-id "1") (first-chunk "two pictures")
                                    :files (list #p"/tmp/a.pdf" #p"/tmp/b.pdf"))))
    (is-plan plan :path "/sendMediaGroup" :label "send_media_group"
                  "file0" #P"/tmp/a.pdf" "file1" #P"/tmp/b.pdf")
    (is (search "attach://file0" (plan-field plan "media")))))

(deftest channel-telegram-file-message-plans ()
  ;; A photo renders inline and a document files: the plan picks sendPhoto
  ;; or sendDocument, the caption rides the body, and the topic the target
  ;; names follows the file into it. The bytes decide photo-or-document —
  ;; TELEGRAM-IMAGE-FILE-P's call — so the plan asks.
  (let* ((photo (nct:telegram-file-plan '(:channel-id "1" :thread-id "5")
                                        #P"/tmp/pea.png"
                                        :content "look" :reply-to "7"
                                        :photo-p t))
         (body photo.body))
    (is-plan photo :path "/sendPhoto" :label "send_photo" "chat_id" "1" "message_thread_id" "5"
                   "caption" "look" "photo" #P"/tmp/pea.png")
    (is (nck:multipart-body-p body) "files ride as form-data")
    (is (search "message_id" (plan-field photo "reply_parameters"))))
  (let* ((document (nct:telegram-file-plan '(:channel-id "1")
                                           #P"/tmp/notes.pdf"
                                           :photo-p nil))
         (body document.body))
    (is-plan document :path "/sendDocument" :label "send_document" "document" #P"/tmp/notes.pdf")
    (is (null (assoc "caption" body :test #'equal)))
    (is (null (assoc "message_thread_id" body :test #'equal))))
  ;; A voice message plays as one, its length rounded up to the second.
  (is-plan (nct:telegram-file-plan '(:channel-id "1") #P"/tmp/voice-message.ogg"
                                   :reply-to "7" :voice '(:seconds 2.2 :waveform #()))
           :path "/sendVoice" :label "send_voice" "chat_id" "1" "duration" "3"
           "voice" #P"/tmp/voice-message.ogg"))

(deftest channel-telegram-details-are-an-alert ()
  ;; A press is answered so its spinner stops; a card's Details press is
  ;; answered with an alert only the presser sees, the details' head read
  ;; plainly — Telegram's alert holds 200 characters and no markdown.
  (is-plan (first (nct:answer-callback-plans '(:id "q1")))
           :label "answer_callback_query" "callback_query_id" "q1" "text" nil "show_alert" nil)
  (let ((plan (first (nct:answer-callback-plans
                      '(:id "q2")
                      :text (format nil "**Steps** · 1~%1. ✓ Ran just lint · 3s~%```~%clean~%```~%**Thinking**~%> All clean.")))))
    (is-plan plan "show_alert" t
             "text" (format nil "Steps · 1~%1. ✓ Ran just lint · 3s~%clean~%Thinking~%All clean."))))
