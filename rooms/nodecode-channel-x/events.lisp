;;;; events.lisp --- X mentions as the kit's candidates. Pure.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A tweet the notifications timeline holds becomes the shared candidate
;;;; shape when it is something a person said to the bot: it names the bot's
;;;; handle, or it replies to one of the bot's own tweets. Nothing here
;;;; touches the network: which tweets are the bot's is the adapter's to know,
;;;; passed in.
;;;;
;;;; Rooms. An X conversation -- a first tweet and every reply under it -- is
;;;; one room: its conversation id is the candidate's channel. A reply names
;;;; the tweet it answers, so a lane goes on down a reply chain the way a
;;;; Discord thread does. There are no direct messages here: X's DMs moved to
;;;; encrypted XChat, which a web session cannot read or send in.
;;;;
;;;; Text. X stores a tweet with &amp; &lt; &gt; escaped and a reply's words
;;;; after the handles of everyone in the chain ("@a @b actual words"). The
;;;; model reads the words; the handles say only where the tweet sits.

(in-package #:nodecode-channel-x)

(defparameter +x-text-limit+ 270
  "Characters one post carries: X's 280 less room for the mark that says an
 answer goes on.")

(defun normalize-x-handle (handle)
  "HANDLE without its @, lowercased: the one way handles are compared."
  (string-downcase (string-left-trim "@" (nlk:trimmed (or handle "")))))

(defun x-unescape (text)
  "TEXT with the three entities X escapes in a stored tweet read back."
  (let ((out (or text "")))
    (loop for (from to) in '(("&lt;" "<") ("&gt;" ">") ("&amp;" "&"))
          do (setf out (ppcre:regex-replace-all from out to)))
    out))

(defun x-strip-reply-prefix (text)
  "TEXT without the run of @handles X puts ahead of a reply's words."
  (nlk:trimmed (ppcre:regex-replace "^(\\s*@\\w+)+\\s*" (or text "") "")))

(defun x-mention-p (text handle)
  "Whether TEXT names @HANDLE."
  (let ((handle (normalize-x-handle handle)))
    (and (stringp text) (plusp (length handle))
         (ppcre:scan (format nil "(?i)(^|[^\\w@])@~a(?!\\w)" (ppcre:quote-meta-chars handle)) text)
         t)))

(defun markdown-plain (markdown)
  "MARKDOWN as the plain text a post shows: X draws none of it. Fences and
backticks go, emphasis marks go, a heading keeps its words, a link becomes
its words then its address."
  (let ((text (or markdown "")))
    (loop for (pattern replacement) in
          '(("(?m)^```[^\\n]*\\n?" "")
            ("`([^`]*)`" "\\1")
            ("!\\[([^\\]]*)\\]\\(([^)]*)\\)" "\\1 \\2")
            ("\\[([^\\]]+)\\]\\(([^)]+)\\)" "\\1 \\2")
            ("\\*\\*([^*]+)\\*\\*" "\\1")
            ("__([^_]+)__" "\\1")
            ("(?m)^#{1,6}\\s+" "")
            ("(?m)^\\s*[-*]\\s+" "- ")
            ("\\n{3,}" (format nil "~%~%")))
          do (setf text (ppcre:regex-replace-all pattern text replacement)))
    (nlk:trimmed text)))

(defun x-mention-candidate (tweet &key bot-id bot-handle bot-tweet-p)
  "One TWEET -- a bridge mention: id, text, author_id, author, author_name,
in_reply_to, conversation_id -- as the shared candidate, or NIL for one that
is not said to the bot: the bot's own, a tweet that neither names BOT-HANDLE
nor replies to a tweet BOT-TWEET-P says is the bot's, or one with no words."
  (let* ((id (nlk:json-value tweet :string "id"))
         (author-id (nlk:json-value tweet :string "author_id"))
         (raw (x-unescape (nlk:json-value tweet :string "text")))
         (parent (nlk:json-value tweet :string "in_reply_to"))
         (words (x-strip-reply-prefix raw))
         (to-bot (and parent bot-tweet-p (funcall bot-tweet-p parent) t))
         (named (x-mention-p raw bot-handle)))
    (when (and id author-id (not (equal author-id bot-id))
               (or named to-bot)
               (plusp (length words)))
      (nlk:json-object
       "text" words
       "source"
       (nlk:json-object
        "platform" "x"
        "chat_kind" "group"
        "channel_id" (or (nlk:json-value tweet :string "conversation_id") id)
        "user_id" author-id
        "user_name" (let ((handle (nlk:json-value tweet :string "author")))
                      (if handle (format nil "@~a" handle) author-id))
        "message_id" id
        "reply_to_message_id" parent
        ;; Said to the bot by X itself: a mention or a reply under its own
        ;; tweet, so no name in the words is needed.
        "addressed" t
        "is_bot" nil)))))

(defun x-inbound-policy (&key allowed-users bot-handle ignored-conversations &allow-other-keys)
  "The kit policy over X: people by account id, every room public (a
conversation is a group), no direct messages, a mention the one way in."
  (make-inbound-policy
   :allowed-users allowed-users
   :dm-policy :disabled
   :group-policy :enabled
   :require-mention t
   :ignored-channels ignored-conversations
   :mention-target bot-handle
   :mention-test (lambda (text target)
                   (declare (ignore target))
                   (x-mention-p text bot-handle))
   :allow-bots :none
   :channel-match :exact))
