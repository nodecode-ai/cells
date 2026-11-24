;;;; cell-test.lisp --- the rocketchat cell against a stubbed server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Rocket.Chat endpoint is a stubbed NLK:HTTP and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches a server, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it --
;;;; through the real dispatch, contract and all -- so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "rocketchat" "ROCKETCHAT-CELL-"
  :start nodecode-rocketchat:start-cell
  :defaults (("url" "https://rc.test") ("token_env" "RC_TOKEN") ("user_id" "bot-id")))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-rc-token (&body body)
  "BODY with the token environment answering one fixed token."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "RC_TOKEN") "test-token"))
     ,@body))

(defmacro with-rc-wire ((asked routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM) pairs,
tried in order; FORM runs with METHOD, URL and OPTIONS bound and answers
(values TEXT STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT),
oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url (getf options :content)) ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Rocket.Chat request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +channels+
  (concatenate
   'string
   "{\"success\":true,\"channels\":["
   "{\"_id\":\"ROOM1\",\"name\":\"general\",\"t\":\"c\",\"usersCount\":12,\"topic\":\"all hands\"},"
   "{\"_id\":\"ROOMP\",\"name\":\"ops\",\"t\":\"c\",\"usersCount\":3}]}")
  "What GET /api/v1/channels.list answers.")

(defparameter +groups+
  "{\"success\":true,\"groups\":[{\"_id\":\"ROOMG\",\"name\":\"secret\",\"t\":\"p\",\"usersCount\":2}]}"
  "What GET /api/v1/groups.list answers.")

(defparameter +general-room+
  "{\"success\":true,\"room\":{\"_id\":\"ROOM1\",\"name\":\"general\",\"t\":\"c\"}}"
  "What GET /api/v1/rooms.info?roomId=ROOM1 answers.")

(defparameter +history+
  (concatenate
   'string
   "{\"success\":true,\"total\":2,\"messages\":["
   "{\"_id\":\"M1\",\"rid\":\"ROOM1\",\"msg\":\"standup in ten\",\"ts\":\"2026-01-31T10:00:00.000Z\","
   "\"u\":{\"username\":\"ada\",\"name\":\"Ada\"}},"
   "{\"_id\":\"M2\",\"rid\":\"ROOM1\",\"msg\":\"shipping today\",\"ts\":\"2026-01-31T10:05:00.000Z\","
   "\"u\":{\"username\":\"grace\"}}]}")
  "What GET /api/v1/channels.history answers.")

(defparameter +posted+
  "{\"success\":true,\"message\":{\"_id\":\"M3\",\"rid\":\"ROOM1\",\"msg\":\"on it\",\"u\":{\"username\":\"bot\"}}}"
  "What POST /api/v1/chat.postMessage answers.")

(defparameter +dm-room+
  "{\"success\":true,\"room\":{\"_id\":\"D1\",\"t\":\"d\",\"usernames\":[\"bot\",\"ada\"],\"uids\":[\"bot-id\",\"ADA1\"]}}"
  "What POST /api/v1/im.create answers.")

(defparameter +dm-posted+
  "{\"success\":true,\"message\":{\"_id\":\"M4\",\"rid\":\"D1\",\"msg\":\"task\",\"u\":{\"username\":\"bot\"}}}"
  "What POST /api/v1/chat.postMessage answers inside the DM room.")

(defun rc-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun rc-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (rc-answer id tool arguments)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "rocketchat"
  (:config)
  (:help :rocketchat)
  (:fixture with-rc-token)
  (:refused ("url" 5) ("url" "chat.example.com") ("timeout" 0)))

(deftest rocketchat-cell-without-a-token-refuses (with-rc-token)
  ;; The one setting no verb can work without: the section names a token, and a
  ;; section that names none is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (rocketchat-start "token_env" ""))))
    (is (search "no token" (princ-to-string condition)) "the refusal says what is missing")
    (is (null (nle::find-tool "rocketchat_list_channels")) "and installs nothing")))

(deftest rocketchat-cell-without-a-user-id-refuses (with-rc-token)
  (let ((condition (signals-error nlk:config-refusal (rocketchat-start "user_id" ""))))
    (is (search "user_id" (princ-to-string condition)) "the refusal names the id")))

;;; --- rocketchat_list_channels -------------------------------------------------------

(deftest rocketchat-cell-lists-channels (with-rc-token)
  (with-cell-stop ((rocketchat-start))
    (with-rc-wire (asked (("/api/v1/channels.list" (values +channels+ 200))
                          ("/api/v1/groups.list" (values +groups+ 200))))
      (let* ((object (rc-object "rc-1" "rocketchat_list_channels" "{}"))
             (rooms (nlk:json-array object "channels")))
        (is (= 3 (nlk:json-value object :integer "count")) "channels and groups together")
        (is (equal "general" (nlk:json-value (aref rooms 0) :string "name")))
        (is (equal "ROOM1" (nlk:json-value (aref rooms 0) :string "room_id")))
        (is (equal "group" (nlk:json-value (aref rooms 2) :string "type")))
        (is (equal :get (first (first asked))) "a plain list is two REST calls")))))

(deftest rocketchat-cell-filters-and-honours-allowed-rooms (with-rc-token)
  (with-cell-stop ((rocketchat-start "allowed_rooms" (vector "ROOMG")))
    (with-rc-wire (asked (("/api/v1/channels.list" (values +channels+ 200))
                          ("/api/v1/groups.list" (values +groups+ 200))))
      (let* ((object (rc-object "rc-2" "rocketchat_list_channels" "{\"filter\":\"sec\"}"))
             (rooms (nlk:json-array object "channels")))
        (is (= 1 (nlk:json-value object :integer "count")) "one room, the allowlisted group")
        (is (equal "secret" (nlk:json-value (aref rooms 0) :string "name")))))))

;;; --- rocketchat_get_history ---------------------------------------------------------

(deftest rocketchat-cell-reads-history (with-rc-token)
  (with-cell-stop ((rocketchat-start))
    (with-rc-wire (asked (("/api/v1/rooms.info" (values +general-room+ 200))
                          ("/api/v1/channels.history" (values +history+ 200))))
      (let* ((object (rc-object "rc-3" "rocketchat_get_history" "{\"room_id\":\"ROOM1\",\"count\":2}"))
             (messages (nlk:json-array object "messages"))
             (first-message (aref messages 0)))
        (is (= 2 (nlk:json-value object :integer "count")))
        (is (equal "standup in ten" (nlk:json-value first-message :string "text")))
        (is (equal "ada" (nlk:json-value (nlk:json-value first-message :object "sender") :string "username")))
        (is (equal "untrusted" (nlk:json-value first-message :string "content_trust")) "the room's words are data")
        (is (= 2 (length asked)) "one rooms.info and one history call")))))

(deftest rocketchat-cell-refuses-a-room-outside-the-allowlist (with-rc-token)
  (with-cell-stop ((rocketchat-start "allowed_rooms" (vector "ROOMG")))
    (with-rc-wire (asked ())
      (let ((answer (rc-answer "rc-4" "rocketchat_get_history" "{\"room_id\":\"ROOM1\"}")))
        (is (search "allowed_rooms" answer) "the refusal names the setting, without a request"))
      (is (null asked) "and nothing was dialled"))))

;;; --- rocketchat_search_messages -----------------------------------------------------

(deftest rocketchat-cell-searches-a-room (with-rc-token)
  (with-cell-stop ((rocketchat-start))
    (with-rc-wire (asked (("/api/v1/chat.search" (values +history+ 200))))
      (let ((object (rc-object "rc-5" "rocketchat_search_messages"
                                "{\"room_id\":\"ROOM1\",\"query\":\"shipping\"}")))
        (is (equal "shipping" (nlk:json-value object :string "query")))
        (is (= 2 (nlk:json-value object :integer "count")))
        (is (search "searchText=shipping" (second (first asked))) "the phrase rides the query string")))))

;;; --- rocketchat_post, and the write opt-in -------------------------------------------

(deftest rocketchat-cell-posts-when-write-tools-are-on (with-rc-token)
  (with-cell-stop ((rocketchat-start "write_tools" t))
    (with-rc-wire (asked (("/api/v1/chat.postMessage" (values +posted+ 200))))
      (let ((object (rc-object "rc-6" "rocketchat_post"
                                "{\"room_id\":\"ROOM1\",\"message\":\"on it\"}")))
        (is (eq t (nlk:json-value object :boolean "sent")))
        (is (equal "M3" (nlk:json-value object :string "message_id")))
        (is (search "\"roomId\":\"ROOM1\"" (princ-to-string (third (first asked)))) "posted to its room")))))

(deftest rocketchat-cell-refuses-a-write-with-write-tools-off (with-rc-token)
  (with-cell-stop ((rocketchat-start))
    (with-rc-wire (asked ())
      (let ((answer (rc-answer "rc-7" "rocketchat_post"
                                "{\"room_id\":\"ROOM1\",\"message\":\"on it\"}")))
        (is (search "write_tools" answer) "the refusal names the setting that turns it on")
        (is (null asked) "and nothing was dialled")))))

;;; --- rocketchat_dm and rocketchat_delegate -------------------------------------------

(deftest rocketchat-cell-delegates-in-a-marked-envelope (with-rc-token)
  (with-cell-stop ((rocketchat-start "write_tools" t))
    (with-rc-wire (asked (("/api/v1/im.create" (values +dm-room+ 200))
                          ("/api/v1/chat.postMessage" (values +dm-posted+ 200))))
      (let ((object (rc-object "rc-8" "rocketchat_delegate"
                                "{\"username\":\"ada\",\"message\":\"count the deploys\"}")))
        (is (eq t (nlk:json-value object :boolean "terminal_reply")))
        (is (= 32 (length (nlk:json-value object :string "delegation_id"))))
        (is (search "[hermes-delegation:v1:task:" (princ-to-string (third (second asked)))) "the envelope")))))
