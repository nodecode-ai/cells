;;;; events-test.lisp --- X mentions as candidates; the policy; plain text.
;;;;
;;;; SPDX-License-Identifier: MIT

(in-package #:nodecode.test)

(defun x-tweet (&rest pairs)
  "A bridge mention: id 200 by @kim (account 501) in conversation 200, PAIRS over it."
  (let ((tweet (nlk:make-json-object "id" "200" "text" "@nodebot hi" "author_id" "501" "author" "kim"
                                     "author_name" "Kim" "in_reply_to" nil "conversation_id" "200")))
    (loop for (key value) on pairs by #'cddr do (setf (gethash key tweet) value))
    tweet))

(deftest channel-x-mention-is-a-candidate ()
  (let ((candidate (ncx:x-mention-candidate (x-tweet "text" "@nodebot what &amp; why &lt;3")
                                            :bot-id "900" :bot-handle "NodeBot")))
    (is (equal "what & why <3" (gethash "text" candidate)))
    (is-source candidate "platform" "x" "chat_kind" "group" "channel_id" "200" "user_id" "501"
               "user_name" "@kim" "message_id" "200")
    (is (nck:candidate-addressed-p candidate) "X itself said it to the bot")))

(deftest channel-x-reply-prefix-is-not-the-words ()
  (let ((candidate (ncx:x-mention-candidate (x-tweet "text" "@nodebot @ann and then?" "in_reply_to" "150"
                                                     "conversation_id" "100")
                                            :bot-id "900" :bot-handle "nodebot")))
    (is (equal "and then?" (gethash "text" candidate)))
    (is-source candidate "channel_id" "100" "reply_to_message_id" "150")))

(deftest channel-x-what-is-not-said-to-the-bot ()
  (is (null (ncx:x-mention-candidate (x-tweet "author_id" "900") :bot-id "900" :bot-handle "nodebot")))
  (is (null (ncx:x-mention-candidate (x-tweet "text" "@ann hi") :bot-id "900" :bot-handle "nodebot")))
  (is (null (ncx:x-mention-candidate (x-tweet "text" "@nodebot") :bot-id "900" :bot-handle "nodebot")))
  (is (null (ncx:x-mention-candidate (x-tweet "text" "@nodebotx hi") :bot-id "900" :bot-handle "nodebot")))
  (is (ncx:x-mention-candidate (x-tweet "text" "@ann and then?" "in_reply_to" "777")
                               :bot-id "900" :bot-handle "nodebot"
                               :bot-tweet-p (lambda (id) (equal id "777")))))

(deftest channel-x-policy-admits-allowed-people-only ()
  (let ((policy (ncx:x-inbound-policy :allowed-users '("501") :bot-handle "nodebot"))
        (said (ncx:x-mention-candidate (x-tweet) :bot-id "900" :bot-handle "nodebot"))
        (stranger (ncx:x-mention-candidate (x-tweet "author_id" "502" "author" "eve")
                                           :bot-id "900" :bot-handle "nodebot")))
    (is (eq :answer (nck:decide-inbound policy said)) "an allowed person's mention opens a turn")
    (is (not (eq :answer (nck:decide-inbound policy stranger))) "a stranger's does not")))

(deftest channel-x-markdown-is-plain ()
  (is (equal "Title" (ncx:markdown-plain "## Title")))
  (is (equal "bold and code" (ncx:markdown-plain "**bold** and `code`")))
  (is (equal "docs https://x.y" (ncx:markdown-plain "[docs](https://x.y)")))
  (is (equal (format nil "- a~%- b") (ncx:markdown-plain (format nil "* a~%* b"))))
  (is (equal "snake_case_name" (ncx:markdown-plain "snake_case_name")) "single underscores stay"))
