;;;; cell-test.lisp --- lifecycle, the five tools, /fluxer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a stubbed Fluxer:
;;;; that a start installs the five tools, /fluxer and the manual, and a
;;;; stop takes every one of them back; that a section with no bot token
;;;; refuses and installs nothing; that a send posts the section's token,
;;;; the channel and the text, and cannot ping @everyone; that a read
;;;; renders oldest first with the ids a reaction or a pin takes; that the
;;;; directory lists one guild or every guild; that a reaction and a pin
;;;; reach their own routes, and an unpin its DELETE; and that /fluxer
;;;; prints the endpoint and asks by hand.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "fluxer"
  (:config "bot_token" "app.secret" "base_url" "https://fluxer.test/v1")
  (:help :fluxer)
  (:command "fluxer")
  (:running (is (nle::find-tool "fluxer_send_message") "fluxer_send_message is registered")
            (is (nle::find-tool "fluxer_read_messages"))
            (is (nle::find-tool "fluxer_list_channels"))
            (is (nle::find-tool "fluxer_react"))
            (is (nle::find-tool "fluxer_pin"))
            (is (equal "https://fluxer.test/v1" (nodecode-fluxer::base)) "the section's base")
            (is (equal "Bot app.secret" (cdr (assoc "authorization" (nodecode-fluxer::headers)
                                                     :test #'equal)))
                "the section's token, as a bot token")
            (is (= 20 (nodecode-fluxer::setting :read-limit)) "the default read limit"))
  (:stopped (is (null (nle::find-tool "fluxer_send_message")) "stop unregisters the tools")
            (is (null (nle::find-tool "fluxer_pin")))
            (is (null nodecode-fluxer::*last-error*)))
  (:refused ("timeout" 0) ("read_limit" 0))
  (:idle nodecode-fluxer:fluxer-error (nodecode-fluxer::run-slash "" nil)))

(deftest fluxer-cell-a-missing-token-refuses-and-installs-nothing ()
  (is (signals-error nlk:config-refusal
        (nodecode-fluxer:start-cell
         (nlk:json-object "fluxer" (nlk:make-json-object "base_url" "https://fluxer.test/v1"))))
      "no bot token is a refusal naming the member")
  (is (null (nle::find-tool "fluxer_send_message")) "a refused start installed nothing"))

;;; --- the send --------------------------------------------------------------------

(defun fluxer-sent (id &optional (content ""))
  "An ANSWER: a POST minted message ID whose text is CONTENT."
  (declare (ignore content))
  (values (nlk:encode-json-object (nlk:json-object "id" id)) 200))

(deftest fluxer-cell-a-send-posts-the-token-and-breaks-a-ping ()
  (with-fluxer ("home_channel" "555")
    (fluxer-answering "/channels/987/messages" (lambda (method url body)
                                                 (declare (ignore url body))
                                                 (if (equal method "POST")
                                                     (fluxer-sent "msg9")
                                                     (values "{}" 200))))
    (is (equal "Sent message msg9 to channel 987."
               (fluxer-tool "fluxer_send_message"
                            (nlk:json-object "channel" "987" "content" "release is up @everyone"))))
    (let* ((request (first (fluxer-requests)))
           (body (fluxer-body request))
           (sent (nlk:json-value body :string "content")))
      (is (equal "POST" (first request)))
      (is (equal "https://fluxer.test/v1/channels/987/messages" (second request)))
      (is (equal "Bot app.secret" (fluxer-header request "authorization")) "the section's token")
      (is (equal "application/json" (fluxer-header request "content-type")))
      (is (search "release is up @" sent) "the words are the operator's")
      (is (search (format nil "@~aeveryone" (string (code-char #x200B))) sent)
          "but the ping is broken by a zero-width space")
      (is (not (search "@everyone" sent)) "so the literal ping is gone")
      (is (= 0 (length (nlk:json-value body :array "allowed_mentions" "parse")))
          "and nothing may be parsed as a mention")
      (is (null (nlk:json-value body :object "message_reference")) "a plain message carries no reply"))))

(deftest fluxer-cell-a-reply-names-the-message-it-answers ()
  (with-fluxer ()
    (fluxer-answering "/channels/987/messages" (lambda (method url body)
                                                 (declare (ignore method url body))
                                                 (fluxer-sent "msg10")))
    (is (equal "Sent message msg10 to channel 987 as a reply to msg1."
               (fluxer-tool "fluxer_send_message"
                            (nlk:json-object "channel" "987" "content" "on it"
                                             "reply_to" "msg1"))))
    (let ((body (fluxer-body (first (fluxer-requests)))))
      (is (equal "msg1" (nlk:json-value body :string "message_reference" "message_id"))))))

(deftest fluxer-cell-mentions-are-allowed-when-the-section-says-so ()
  (with-fluxer ("allow_mentions" t)
    (fluxer-answering "/channels/987/messages" (lambda (method url body)
                                                 (declare (ignore method url body))
                                                 (fluxer-sent "msg11")))
    (fluxer-tool "fluxer_send_message"
                 (nlk:json-object "channel" "987" "content" "hi @everyone"))
    (let* ((body (fluxer-body (first (fluxer-requests))))
           (sent (nlk:json-value body :string "content")))
      (is (equal "hi @everyone" sent) "an allowed mention goes as written")
      (is (= 3 (length (nlk:json-value body :array "allowed_mentions" "parse")))
          "and every kind may be parsed"))))

;;; --- the read --------------------------------------------------------------------

(defun fluxer-message (id author content &optional attachments)
  "One message object, as a deployment answers it."
  (let ((message (nlk:json-object "id" id
                                   "content" content
                                   "author" (nlk:json-object "username" author))))
    (when attachments
      (setf (gethash "attachments" message)
            (coerce (mapcar (lambda (name) (nlk:json-object "filename" name)) attachments)
                    'vector)))
    message))

(deftest fluxer-cell-a-read-renders-oldest-first-with-the-ids ()
  (with-fluxer ()
    (fluxer-answering "/channels/987/messages"
                       (lambda (method url body)
                         (declare (ignore method url body))
                         (values (nlk:encode-json-object
                                  (vector (fluxer-message "m2" "ada" "second line" '("notes.txt"))
                                          (fluxer-message "m1" "ada" "first line")))
                                 200)))
    (is (equal (format nil "m1 ada: first line~%m2 ada: second line [attached: notes.txt]")
               (fluxer-tool "fluxer_read_messages" (nlk:json-object "channel" "987" "limit" 5))))
    (let ((request (first (fluxer-requests))))
      (is (equal "GET" (first request)))
      (is (equal "https://fluxer.test/v1/channels/987/messages?limit=5" (second request))
          "the call's own limit"))))

(deftest fluxer-cell-an-empty-channel-says-so ()
  (with-fluxer ()
    (fluxer-answer "[]")
    (is (equal "No messages in channel 404."
               (fluxer-tool "fluxer_read_messages" (nlk:json-object "channel" "404"))))))

(deftest fluxer-cell-a-refused-call-is-a-repair-not-a-crash ()
  (with-fluxer ()
    (fluxer-answer "{\"message\":\"Unknown Channel\"}" 404)
    (is-carrying (answer (fluxer-tool "fluxer_read_messages" (nlk:json-object "channel" "9")))
                 ("ERROR: FLUXER-ERROR:" "the refusal is the model's repair")
                 ("HTTP 404") ("Unknown Channel"))
    (is (search "HTTP 404" nodecode-fluxer::*last-error*) "/fluxer can show the last failure")))

;;; --- the directory ----------------------------------------------------------------

(defun fluxer-channel (id name)
  (nlk:json-object "id" id "name" name "type" "text"))

(deftest fluxer-cell-the-directory-lists-a-guild-or-every-guild ()
  (with-fluxer ()
    (fluxer-answering "/users/@me/guilds"
                       (lambda (method url body)
                         (declare (ignore method url body))
                         (values (nlk:encode-json-object
                                  (vector (nlk:json-object "id" "42" "name" "Lab")))
                                 200)))
    (fluxer-answering "/guilds/42/channels"
                       (lambda (method url body)
                         (declare (ignore method url body))
                         (values (nlk:encode-json-object
                                  (vector (fluxer-channel "7" "general")
                                          (fluxer-channel "8" "builds")))
                                 200)))
    (is (equal (format nil "7  #general~%8  #builds")
               (fluxer-tool "fluxer_list_channels" (nlk:json-object "guild" "42")))
        "one guild's channels, with the id a send takes")
    (is (equal (format nil "7  #general  (Lab)~%8  #builds  (Lab)")
               (fluxer-tool "fluxer_list_channels" (nlk:json-object)))
        "every guild's, each naming its guild")))

;;; --- reaction and pin -------------------------------------------------------------

(deftest fluxer-cell-a-reaction-and-a-pin-reach-their-own-routes ()
  (with-fluxer ()
    (fluxer-answer "" 204)
    (is (equal "Reacted with 👍 to message m1 in channel 987."
               (fluxer-tool "fluxer_react"
                            (nlk:json-object "channel" "987" "message" "m1" "emoji" "👍"))))
    (is (equal "https://fluxer.test/v1/channels/987/messages/m1/reactions/%F0%9F%91%8D/@me"
               (second (fluxer-latest)))
        "the emoji as the UTF-8 octets it is")
    (is (equal "Pinned message m1 in channel 987."
               (fluxer-tool "fluxer_pin" (nlk:json-object "channel" "987" "message" "m1"))))
    (is (equal "PUT" (first (fluxer-latest))))
    (is (equal "https://fluxer.test/v1/channels/987/pins/m1" (second (fluxer-latest)))
        "a pin is the PUT on the channel's pins")
    (is (equal "Unpinned message m1 in channel 987."
               (fluxer-tool "fluxer_pin"
                            (nlk:json-object "channel" "987" "message" "m1" "remove" t))))
    (is (equal "DELETE" (first (fluxer-latest))) "an unpin is the DELETE")
    (is (equal "https://fluxer.test/v1/channels/987/pins/m1" (second (fluxer-latest))))))

;;; --- /fluxer ---------------------------------------------------------------------

(deftest fluxer-cell-the-command-shows-the-endpoint-or-posts-by-hand ()
  (with-fluxer ("home_channel" "555")
    (fluxer-answering "/channels/555/messages"
                       (lambda (method url body)
                         (declare (ignore method url body))
                         (fluxer-sent "msg12")))
    (let ((status (nodecode-fluxer::run-slash "" nil)))
      (is (search "fluxer.test/v1" status) "the endpoint")
      (is (search "home channel 555" status) "the home channel")
      (is (search "mentions neutralized" status) "the mention switch"))
    (is (equal "Sent message msg12 to channel 555."
               (nodecode-fluxer::run-slash "send release is up" nil))
        "a bare send goes to home_channel")
    (is (equal "Sent message msg12 to channel 555."
               (nodecode-fluxer::run-slash "send #555 hello" nil))
        "and #CHANNEL names one")
    (is (search "not one of its choices"
                (nodecode-fluxer::run-slash "fly" nil)))))
