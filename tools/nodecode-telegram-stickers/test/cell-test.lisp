;;;; cell-test.lisp --- the telegram-stickers cell against a stubbed Bot API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Bot API call is a stubbed NLK:HTTP and the bot token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Telegram, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it -
;;;; through the real dispatch, contract and all - so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "telegram-stickers" "TELEGRAM-STICKERS-CELL-")

(defun telegram-stickers-config (&rest pairs)
  "A whole shared config holding the `telegramstickers' section PAIRS make."
  (let ((section (apply #'nlk:make-json-object pairs)))
    (alexandria:ensure-gethash "token_env" section "TELEGRAM_BOT_TOKEN")
    (nlk:json-object "telegramstickers" section)))

(defun telegram-stickers-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-telegram-stickers:start-cell (apply #'telegram-stickers-config pairs)))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-sticker-token (&body body)
  "BODY with the token environment answering one fixed bot token."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "TELEGRAM_BOT_TOKEN") "123:test-token"))
     ,@body))

(defmacro with-telegram ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Bot API request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +cats+
  (concatenate
   'string
   "{\"ok\":true,\"result\":{\"name\":\"cats\",\"title\":\"Cat Life\",\"stickers\":["
   "{\"file_id\":\"FILE-LAUGH\",\"emoji\":\"\😂\",\"is_animated\":false},"
   "{\"file_id\":\"FILE-EYEROLL\",\"emoji\":\"\🙄\",\"is_animated\":true},"
   "{\"file_id\":\"FILE-SMUG\",\"emoji\":\"\😏\",\"is_video\":true}]}}")
  "What getStickerSet answers for one pack: a laughing, an eye-rolling and a smug sticker.")

(defparameter +dogs+
  "{\"ok\":true,\"result\":{\"name\":\"dogs\",\"title\":\"Good Dogs\",\"stickers\":[{\"file_id\":\"FILE-DOG\",\"emoji\":\"\🐶\"}]}}"
  "A second pack, with a single sticker.")

(defparameter +sent+
  "{\"ok\":true,\"result\":{\"message_id\":77,\"chat\":{\"id\":42}}}"
  "What sendSticker answers once the sticker is in the chat.")

(defun sticker-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun sticker-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (sticker-answer id tool arguments)))

(defun sent-payload (asked)
  "The JSON body of the sendSticker exchange in ASKED."
  (let ((entry (find "sendSticker" asked :key (lambda (row) (second row)) :test #'search)))
    (and entry (nlk:decode-json (third entry)))))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "telegram-stickers"
  (:config "token_env" "TELEGRAM_BOT_TOKEN")
  (:help :telegram-stickers)
  (:command "stickers")
  (:fixture with-sticker-token)
  (:refused ("token_env" 5)))

(deftest telegram-stickers-cell-without-a-token-refuses ()
  ;; The one setting no verb can work without: a section that names no token is
  ;; refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (telegram-stickers-start "token_env" ""))))
    (is (search "no token" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "telegram_sticker_find")) "and installs nothing")))

;;; --- telegram_sticker_find ------------------------------------------------------------

(deftest telegram-stickers-cell-lists-its-packs (with-sticker-token)
  (with-cell-stop ((telegram-stickers-start "packs" (vector "cats" "dogs")))
    (with-telegram (asked ("getStickerSet"
                            (values (if (search "cats" (or (getf options :content) ""))
                                        +cats+ +dogs+)
                                    200)))
      (let* ((object (sticker-object "ts-find-1" "telegram_sticker_find" "{}"))
             (packs (nlk:json-array object "packs")))
        (is (= 2 (length packs)) "one entry per pack in use")
        (is (equal "Cat Life" (nlk:json-value (aref packs 0) :string "title")) "with the pack's own title")
        (is (= 3 (nlk:json-value (aref packs 0) :integer "count")) "and how many stickers it holds")
        (is (equal (format nil "~{~a~^ ~}" (list "😂" "🙄" "😏"))
                   (nlk:json-value (aref packs 0) :string "emojis"))
            (format nil "what the pack offers: ~s" (nlk:json-value (aref packs 0) :string "emojis")))
        (is (= 2 (length asked)) "one getStickerSet per pack"))
      (let ((again (sticker-object "ts-find-2" "telegram_sticker_find" "{}")))
        (is (= 2 (length (nlk:json-array again "packs"))) "the second find answers the same packs")
        (is (= 2 (length asked)) "and reads the cached packs, not Telegram")))))

(deftest telegram-stickers-cell-finds-a-sticker-by-emoji (with-sticker-token)
  (with-cell-stop ((telegram-stickers-start "packs" (vector "cats")))
    (with-telegram (asked ("getStickerSet" (values +cats+ 200)))
      (let* ((object (sticker-object "ts-find-3" "telegram_sticker_find"
                                      "{\"emoji\":\"\🙄\"}"))
             (hit (aref (nlk:json-array object "stickers") 0)))
        (is (= 2 (nlk:json-value object :integer "count"))
            (nlk:encode-json-object object))
        (is (equal "cats:1" (nlk:json-value hit :string "id")) "the exact tag comes first")
        (is (equal "animated" (nlk:json-value hit :string "kind")) "and what kind of sticker it is")
        (is (equal "cats:2" (nlk:json-value (aref (nlk:json-array object "stickers") 1) :string "id")) "then the smile of the same feeling, as the plugin's nearest-emoji fallback did")))))

(deftest telegram-stickers-cell-finds-a-sticker-by-the-feeling (with-sticker-token)
  (with-cell-stop ((telegram-stickers-start "packs" (vector "cats")))
    (with-telegram (asked ("getStickerSet" (values +cats+ 200)))
      (let* ((object (sticker-object "ts-find-4" "telegram_sticker_find"
                                      "{\"query\":\"facepalm\"}"))
             (hit (aref (nlk:json-array object "stickers") 0)))
        (is (= 1 (nlk:json-value object :integer "count")) "a word finds the emoji of its feeling")
        (is (equal "cats:1" (nlk:json-value hit :string "id")) "an eye roll answers a facepalm"))
      (let* ((object (sticker-object "ts-find-5" "telegram_sticker_find"
                                      "{\"emoji\":\"\🤦\"}"))
             (hit (aref (nlk:json-array object "stickers") 0)))
        (is (equal "cats:1" (nlk:json-value hit :string "id")) "an emoji no pack has falls back to the nearest feeling")))))

(deftest telegram-stickers-cell-says-what-the-packs-do-have (with-sticker-token)
  (with-cell-stop ((telegram-stickers-start "packs" (vector "cats")))
    (with-telegram (asked ("getStickerSet" (values +cats+ 200)))
      (let ((object (sticker-object "ts-find-6" "telegram_sticker_find"
                                    "{\"query\":\"basketball\"}")))
        (is (= 0 (nlk:json-value object :integer "count")))
        (is (search "\😂" (nlk:json-value object :string "hint")) "the hint lists the emojis the packs have")))))

;;; --- telegram_sticker_send -------------------------------------------------------------

(deftest telegram-stickers-cell-sends-a-sticker (with-sticker-token)
  (with-cell-stop ((telegram-stickers-start "packs" (vector "cats") "default_chat_id" "42"))
    (with-telegram (asked ("sendSticker" (values +sent+ 200))
                          ("getStickerSet" (values +cats+ 200)))
      (let* ((object (sticker-object "ts-send-1" "telegram_sticker_send"
                                      "{\"sticker\":\"cats:0\",\"reply_to_message_id\":9,\"thread_id\":5}"))
             (payload (sent-payload asked)))
        (is (eq t (nlk:json-value object :boolean "success")))
        (is (= 77 (nlk:json-value object :integer "message_id")) "the answer carries the new message id")
        (is (equal "42" (nlk:json-value payload :string "chat_id")) "the section's chat when the call names none")
        (is (equal "FILE-LAUGH" (nlk:json-value payload :string "sticker")) "the sticker's file id goes out")
        (is (= 9 (nlk:json-value payload :integer "reply_parameters" "message_id")) "as the reply asked for")
        (is (= 5 (nlk:json-value payload :integer "message_thread_id")) "in the forum topic")))))

(deftest telegram-stickers-cell-paces-each-chat (with-sticker-token)
  (with-cell-stop ((telegram-stickers-start "packs" (vector "cats")))
    (with-telegram (asked ("sendSticker" (values +sent+ 200))
                          ("getStickerSet" (values +cats+ 200)))
      (is (eq t (nlk:json-value (sticker-object "ts-send-2" "telegram_sticker_send"
                                                "{\"sticker\":\"cats:0\",\"chat_id\":\"42\"}")
                                 :boolean "success")))
      (is (search "a sticker already went to this chat"
                  (sticker-answer "ts-send-3" "telegram_sticker_send"
                                  "{\"sticker\":\"cats:1\",\"chat_id\":\"42\"}")) "a second sticker inside the cooldown is refused")
      (is (eq t (nlk:json-value (sticker-object "ts-send-4" "telegram_sticker_send"
                                                "{\"sticker\":\"cats:1\",\"chat_id\":\"42\",\"force\":true}")
                                 :boolean "success")) "force sends anyway"))))

(deftest telegram-stickers-cell-mutes-a-chat (with-sticker-token)
  (with-cell-stop ((telegram-stickers-start "packs" (vector "cats")))
    (with-telegram (asked ("sendSticker" (values +sent+ 200))
                          ("getStickerSet" (values +cats+ 200)))
      (let ((object (sticker-object "ts-set-1" "telegram_sticker_settings"
                                    "{\"action\":\"mute\",\"chat_id\":\"42\"}")))
        (is (eq t (nlk:json-value object :boolean "muted")) "the chat is switched off"))
      (is (search "switched off"
                  (sticker-answer "ts-send-5" "telegram_sticker_send"
                                  "{\"sticker\":\"cats:0\",\"chat_id\":\"42\"}")) "and a send there is refused")
      (let ((object (sticker-object "ts-set-2" "telegram_sticker_settings"
                                    "{\"action\":\"unmute\",\"chat_id\":\"42\"}")))
        (is (eq nil (nlk:json-value object :boolean "muted")) "unmute switches it back on")))))

(deftest telegram-stickers-cell-refuses-a-send-without-a-chat (with-sticker-token)
  (with-cell-stop ((telegram-stickers-start "packs" (vector "cats")))
    (with-telegram (asked ("getStickerSet" (values +cats+ 200)))
      (let ((answer (sticker-answer "ts-send-6" "telegram_sticker_send" "{\"sticker\":\"cats:0\"}")))
        (is (search "no chat" answer) answer)))))

(deftest telegram-stickers-cell-refuses-an-id-no-pack-has (with-sticker-token)
  (with-cell-stop ((telegram-stickers-start "packs" (vector "cats")))
    (with-telegram (asked ("getStickerSet" (values +cats+ 200)))
      (is (search "no sticker cats:9"
                  (sticker-answer "ts-send-7" "telegram_sticker_send"
                                  "{\"sticker\":\"cats:9\",\"chat_id\":\"42\"}")) "the refusal says which id and what the pack holds"))))

;;; --- /stickers ------------------------------------------------------------------------

(deftest telegram-stickers-cell-the-slash-command-answers (with-sticker-token)
  (with-cell-stop ((telegram-stickers-start "packs" (vector "cats")))
    (with-telegram (asked ("getStickerSet" (values +cats+ 200)))
      (let ((text (nodecode-telegram-stickers::run-slash "list" nil)))
        (is (search "1 pack(s) in use" text))
        (is (search "cats (3 stickers)" text) "the pack and what it holds")
        (is (search "Cooldown" text) "and the pacing in force"))
      (is (search "switched off" (nodecode-telegram-stickers::run-slash "mute 42" nil)) "the mute switch is the command's too")
      (is (search "switched back on" (nodecode-telegram-stickers::run-slash "unmute 42" nil))))))
