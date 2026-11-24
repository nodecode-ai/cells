;;;; cell.lisp --- Telegram stickers: three tools over the Bot API.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :telegram-stickers) answers the primer below, and every request's
;;;; help section carries one line naming it. Nothing hooks the provider path:
;;;; Telegram speaks HTTP, and NLK:HTTP is the one seam.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "telegramstickers": {"token_env": "TELEGRAM_BOT_TOKEN",
;;;;                         "packs": ["MyPacks"], "default_chat_id": "1234"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from churnast's Hermes Agent plugin (see NOTICE): the tools, the
;;;; reaction table that lets words find an emoji, the pack reading, the
;;;; per-chat cooldown, the mute switch and the ids a find answers with.

(in-package #:nodecode-telegram-stickers)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Telegram stickers are available through the nodecode-telegram-stickers cell: three tools, which a
person can also call from eval in the telegram-stickers package. They send as the bot of the token
the section names, so a sticker lands in the chat exactly as the bot's own message.
  telegram_sticker_find  {query, emoji, pack, limit, refresh}
      No arguments: the packs in use, their titles and the emojis they cover. query: a few English
      words about the reaction ('facepalm', 'eye roll', 'thumbs up', 'sleepy'); it matches the
      emojis pack authors tag stickers with, by feeling, and the pack names. emoji: the stickers
      tagged with it, or with the nearest emoji of the same feeling. An answer carries ids like
      'pack_name:12' and, when nothing fits, the emojis the packs do have.
  telegram_sticker_send  {sticker, chat_id, reply_to_message_id, thread_id, force}
      One sticker, as a reply to the message being answered, in the same forum topic. sticker is an
      id from find, an emoji, or the same few English words. chat_id is the chat; empty, the
      section's default_chat_id. A chat's cooldown refuses a second sticker too soon: answer in
      words then, or pass force when the owner clearly asked for this one. After sending, do not
      describe the sticker in text - one short line that carries the conversation on.
  telegram_sticker_settings  {action, chat_id, seconds}
      list: what is set and what the packs hold. mute / unmute: switch stickers off or back on in a
      chat, as soon as anyone asks you to stop. cooldown: the least seconds between two stickers in
      one chat (0 turns pacing off).
Use a sticker the way a person would: in place of a short emotional line ('haha', 'oh no', 'nice try'),
on a joke or good news, never instead of a real answer, not twice in a row, and never to someone who is
upset. /stickers in the chat lists what is set, 'refresh' reloads the packs, 'mute <chat id>' and
'unmute <chat id>' switch one chat."
  "What (help :telegram-stickers) answers while the cell runs.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (telegram-stickers-error (condition)
       (nle:failure (format nil "ERROR: ~a" (telegram-stickers-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Telegram stickers: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

;;; --- small text helpers ----------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun words-of (text)
  "TEXT's words, lowercased: runs of letters and digits."
  (when (stringp text)
    (remove "" (mapcar #'string-downcase
                       (ppcre:split "[^A-Za-z0-9]+" text))
            :test #'string=)))

(defun plain-emoji (emoji)
  "EMOJI without a variation selector, so the same picture compares equal."
  (remove (code-char #xFE0F) (or emoji "")))

(defun emoji-p (text)
  "Whether TEXT names no word at all: an emoji, or a run of them."
  (let ((text (nlk:trimmed (or text ""))))
    (and (plusp (length text))
         (null (words-of text)))))

;;; --- the feelings a model names, and the emojis pack authors tag ---------------
;;;
;;; REACTION-GROUPS, ported from the plugin: the words a model tends to use for a
;;; feeling (two-word ones written together, 'thumbsup'), the emojis pack authors
;;; tag such stickers with (closest first). They let 'facepalm' find a sticker
;;; tagged with an eye roll in packs without a palm, and let an emoji no pack has
;;; fall back to the nearest feeling.

(defparameter +feelings+
  '((("facepalm" "smh" "ugh") ("🤦" "🙄" "😑" "😩" "😒" "🤷"))
   (("laugh" "laughing" "lol" "lmao" "haha" "hahaha" "funny" "hilarious" "rofl")
    ("😂" "🤣" "😆" "😹" "😄" "😁"))
   (("cry" "crying" "sad" "tears" "sob" "sobbing" "upset") ("😭" "😢" "😿" "🥺" "😞" "☹"))
   (("love" "heart" "adore") ("❤" "😍" "🥰" "😘" "💕" "💖" "💘"))
   (("kiss" "kisses") ("😘" "😗" "😚" "💋"))
   (("hug" "hugs" "cuddle") ("🤗" "🫂" "🥰"))
   (("angry" "mad" "rage" "furious" "annoyed") ("😡" "😠" "🤬" "👿" "😤"))
   (("shock" "shocked" "surprise" "surprised" "wow" "omg" "ohno") ("😱" "😮" "😲" "😳" "🤯" "😯"))
   (("scared" "afraid" "fear" "frightened") ("😨" "😱" "😰" "😧"))
   (("cool" "confident" "chill") ("😎" "🆒" "🤙"))
   (("think" "thinking" "hmm" "wonder") ("🤔" "🧐" "🤨"))
   (("sleep" "sleepy" "tired" "goodnight" "night" "bed") ("😴" "💤" "🥱" "😪" "🛌"))
   (("party" "celebrate" "celebration" "congrats" "congratulations" "birthday" "yay")
    ("🥳" "🎉" "🎊" "🍾" "🎂"))
   (("yes" "agree" "okay" "ok" "approve" "thumbsup" "nice") ("👍" "👌" "✅" "🆗" "💯"))
   (("no" "nope" "disagree" "dislike" "thumbsdown") ("👎" "🙅" "❌" "🚫"))
   (("hi" "hello" "hey" "wave" "greeting" "bye" "goodbye") ("👋" "🙋" "🤚"))
   (("thanks" "thank" "please" "pray" "hope") ("🙏" "🤲"))
   (("clap" "applause" "bravo") ("👏" "🙌"))
   (("fire" "lit" "amazing" "awesome") ("🔥" "💯" "🤩"))
   (("shrug" "whatever" "dunno" "idk") ("🤷" "🙄" "😐"))
   (("eyeroll" "eyerolling" "rollingeyes") ("🙄" "😒" "😑"))
   (("smug" "sly" "mischievous" "evil") ("😏" "😈" "🤭"))
   (("sarcasm" "sarcastic" "irony" "ironic") ("🙃" "😏" "🙄"))
   (("nervous" "awkward" "sweat" "oops") ("😅" "😬" "🫠" "😓" "🙃"))
   (("confused" "puzzled") ("😕" "🤨" "😵" "🤔"))
   (("money" "rich" "cash") ("🤑" "💰" "💸" "💵"))
   (("strong" "muscle" "power") ("💪" "🏋"))
   (("sick" "ill" "nausea") ("🤢" "🤮" "🤒" "😷" "🦠"))
   (("cold" "freezing") ("🥶" "❄"))
   (("hungry" "food" "yum" "delicious") ("😋" "🤤" "🍕" "🍔"))
   (("coffee") ("☕"))
   (("popcorn" "drama") ("🍿" "👀"))
   (("look" "watching" "eyes") ("👀" "🧐"))
   (("work" "working" "laptop" "computer" "coding") ("💻" "⌨"))
   (("quiet" "secret" "shh") ("🤫" "🤐"))
   (("pleading" "puppy" "beg") ("🥺" "🙏"))
   (("happy" "joy" "glad" "smile") ("😊" "😄" "😁" "🙂" "😃"))
   (("bored" "boring" "meh") ("😑" "😐" "🥱"))
   (("travel" "flight" "airport" "plane" "trip" "vacation") ("✈" "🧳" "🏖"))
   (("dance" "dancing") ("💃" "🕺" "👯"))
   (("mindblown" "crazy") ("🤯" "🤪")))
  "Words a model names a feeling with, and the emojis pack authors tag it with, closest first.")

(defparameter *feelings-by-word* (make-hash-table :test #'equal)
  "A feeling's words, each to its emoji list.")

(defparameter *feelings-by-emoji* (make-hash-table :test #'equal)
  "An emoji, to the emoji lists of every feeling it belongs to.")

(loop for (words emojis) in +feelings+
      do (dolist (word words)
           (setf (gethash word *feelings-by-word*) emojis))
         (dolist (emoji emojis)
           (pushnew emojis (gethash (plain-emoji emoji) *feelings-by-emoji*) :test #'equal)))

(defun feeling-of-word (word)
  "The emoji list WORD names, also as a plural ('hugs', 'kisses')."
  (or (gethash word *feelings-by-word*)
      (and (uiop:string-suffix-p "s" word)
           (> (length word) 1)
           (gethash (subseq word 0 (1- (length word))) *feelings-by-word*))))

(defun feeling-of-emoji (emoji)
  "The first emoji list EMOJI belongs to, or NIL."
  (first (gethash (plain-emoji emoji) *feelings-by-emoji*)))

;;; --- the state a running cell keeps -----------------------------------------------

(defvar *catalog* nil
  "Pack short name -> (:title TITLE :words TITLE-WORDS :stickers #(...)), or NIL.")
(defvar *pack-order* nil
  "The packs the catalog holds, in the order the section names them.")
(defvar *unavailable* nil
  "Packs Telegram refused, as (NAME . WHY), newest first.")
(defvar *last-sent* nil
  "Chat id -> universal time of the last sticker sent there.")
(defvar *cooldowns* nil
  "Chat id -> the least seconds between two stickers there, when not the section's.")
(defvar *muted* nil
  "Chat ids stickers are switched off in.")

(defun reset-state ()
  "What a start begins with: no catalog, no pacing, nothing muted."
  (setf *catalog* nil
        *pack-order* nil
        *unavailable* nil
        *last-sent* (make-hash-table :test #'equal)
        *cooldowns* (make-hash-table :test #'equal)
        *muted* (make-hash-table :test #'equal)))

;;; --- reading the section ---------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, plus the bot token the section names: read once, here."
  (declare (ignore table))
  (let* ((env (nonblank (getf values :token-env)))
         (file (nonblank (getf values :token-file)))
         (base (string-right-trim "/" (or (getf values :api-base) "https://api.telegram.org")))
         (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of token_env and token_file, not both"))
    (unless (or (uiop:string-prefix-p "https://" base) (uiop:string-prefix-p "http://" base))
      (nlk:config-error "api_base ~s has no http:// or https:// scheme: set it to the Bot API base, e.g. \"https://api.telegram.org\"" base))
    (setf (getf plist :api-base) base
          (getf plist :token)
          (cond (env (or (nonblank (nle::credential-env env))
                         (nlk:config-error "token_env names ~a, which is unset or empty" env)))
                (file (or (nonblank (ignore-errors
                                     (uiop:read-file-string (nlk:expand-home file))))
                          (nlk:config-error "token_file ~a cannot be read, or is empty" file)))
                (t (nlk:config-error "no token: set token_env to the environment variable holding the bot token, or token_file to a file holding one"))))
    plist))

(defun scrub (text)
  "TEXT without the bot token in it: the last line of defence for a URL that carries it."
  (let ((token (ignore-errors (setting :token))))
    (if (and (stringp token) (plusp (length token)) (search token text))
        (ppcre:regex-replace-all (ppcre:quote-meta-chars token) text "<token>")
        text)))

;;; --- one Bot API call ---------------------------------------------------------------

(defun api-exchange (method payload)
  "One POST to the bot's METHOD => (values BODY STATUS). A dial that fails is a refusal."
  (let ((url (format nil "~a/bot~a/~a" (setting :api-base) (setting :token) method)))
    (handler-case
        (multiple-value-bind (body status)
            (nlk:http :post url
                      :headers (list (cons "content-type" "application/json")
                                     (cons "accept" "application/json"))
                      :content (nlk:encode-json-object (or payload (nlk:json-object)))
                      :timeout (setting :timeout)
                      :connect-timeout (setting :timeout))
          (values (if (stringp body) body (nlk:body-text body)) status))
      (error (condition)
        (fail "~a did not answer: ~a" method
              (scrub (nlk:one-line (princ-to-string condition))))))))

(defun api-call (method &optional payload)
  "METHOD's decoded result, or a refusal carrying Telegram's own words."
  (multiple-value-bind (body status) (api-exchange method payload)
    (let ((object (ignore-errors (nlk:decode-json (or body "")))))
      (cond ((and (hash-table-p object)
                  (eq t (nlk:json-value object :boolean "ok")))
             (or (nlk:json-value object :object "result") (nlk:json-object)))
            (t (fail "~a answered ~@[HTTP ~a ~]~a" method status
                     (scrub (or (and (hash-table-p object)
                                     (nlk:json-value object :string "description"))
                                (nlk:clip (or body "") 200)))))))))

;;; --- the packs ---------------------------------------------------------------------

(defparameter +default-packs+ '("TheFoods" "MelieTheCavy" "OfficeTurkey" "Animals")
  "Four packs made by Telegram, used while the section names none, as the plugin did.")

(defun pack-name-of (entry)
  "A pack as the section names it: the short name of a t.me/addstickers/ link, or the name."
  (let* ((entry (string-right-trim "/" (nlk:trimmed entry)))
         (marker "addstickers/")
         (position (search marker entry)))
    (if position (subseq entry (+ position (length marker))) entry)))

(defun packs-in-use ()
  "The section's packs, or the four Telegram-made ones when it names none."
  (or (remove-if-not #'nonblank (mapcar #'pack-name-of (or (setting :packs) '())))
      (copy-list +default-packs+)))

(defun load-pack (name)
  "One getStickerSet, kept in the catalog: its title and its stickers."
  (let* ((set (api-call "getStickerSet" (nlk:json-object "name" name)))
         (title (or (nlk:json-value set :string "title") name))
         (stickers (or (nlk:json-array set "stickers") #()))
         (rows (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for sticker across stickers
          for index from 0
          do (vector-push-extend
              (list :index index
                    :file-id (or (nlk:json-value sticker :string "file_id") "")
                    :emoji (or (nlk:json-value sticker :string "emoji") "")
                    :kind (cond ((eq t (nlk:json-value sticker :boolean "is_video")) "video")
                                ((eq t (nlk:json-value sticker :boolean "is_animated")) "animated")
                                (t "static")))
              rows))
    (setf (gethash name *catalog*) (list :title title
                                          :words (words-of title)
                                          :stickers (copy-seq rows)))))

(defun load-catalog (&key refresh)
  "Read every pack in use once; REFRESH reads them again. A pack Telegram refuses
is kept as unavailable and the others still load."
  (when (or refresh (null *catalog*))
    (setf *catalog* (make-hash-table :test #'equal)
          *pack-order* '()
          *unavailable* '())
    (dolist (name (packs-in-use))
      (handler-case
          (progn (load-pack name)
                 (push name *pack-order*))
        (telegram-stickers-error (condition)
          (push (cons name (telegram-stickers-error-detail condition)) *unavailable*))))
    (setf *pack-order* (nreverse *pack-order*)))
  *catalog*)

(defun catalog-pack (name)
  "The catalog's entry for NAME, or a refusal the model can act on."
  (or (gethash name *catalog*)
      (fail "pack ~a is not loaded; telegram_sticker_find with refresh true lists the packs in use, which are: ~{~a~^, ~}"
            name (packs-in-use))))

;;; --- picking a sticker ----------------------------------------------------------------

(defun sticker-tag (sticker) (getf sticker :emoji))

(defun sticker-score (sticker pack-title-words pack-words tokens emoji)
  "How well STICKER answers TOKENS and EMOJI: an exact emoji tag first, then the
feeling the words name, then the pack's own name."
  (let ((score 0)
        (tag (plain-emoji (sticker-tag sticker))))
    (when (and emoji (plusp (length emoji)))
      (cond ((string= tag (plain-emoji emoji)) (incf score 10))
            (t (let ((group (feeling-of-emoji emoji)))
                 (when (and group (member tag (mapcar #'plain-emoji group) :test #'string=))
                   (incf score 4))))))
    (dolist (token tokens)
      (let ((group (feeling-of-word token)))
        (when (and group (member tag (mapcar #'plain-emoji group) :test #'string=))
          (incf score 3)))
      (when (member token pack-title-words :test #'string=) (incf score 2))
      (when (member token pack-words :test #'string=) (incf score 2)))
    score))

(defun search-stickers (tokens emoji &key pack (limit 10))
  "The best LIMIT stickers for TOKENS and EMOJI, best first => a list of
(:pack NAME :sticker STICKER :score N)."
  (let ((hits '()))
    (dolist (name *pack-order*)
      (when (or (null pack) (string= name pack))
        (let* ((entry (gethash name *catalog*))
               (title-words (getf entry :words))
               (pack-words (words-of name)))
          (loop for sticker across (getf entry :stickers)
                for score = (sticker-score sticker title-words pack-words tokens emoji)
                do (when (plusp score)
                     (push (list :pack name :sticker sticker :score score) hits))))))
    (subseq (stable-sort hits #'> :key (lambda (hit) (getf hit :score)))
            0 (min limit (length hits)))))

(defun sticker-id (pack sticker)
  (format nil "~a:~a" pack (getf sticker :index)))

(defun split-id (wanted)
  "WANTED as (values PACK INDEX) when it is an id, else (values NIL NIL)."
  (multiple-value-bind (match registers)
      (ppcre:scan-to-strings "\\A([A-Za-z0-9_]+):([0-9]+)\\z" wanted)
    (if match
        (values (aref registers 0) (parse-integer (aref registers 1)))
        (values nil nil))))

(defun emoji-list-text (reversed limit)
  "REVERSED, a newest-first list of emojis, as one line: each emoji once, at most LIMIT of them."
  (let ((all (nreverse reversed)))
    (format nil "~{~a~^ ~}" (subseq all 0 (min limit (length all))))))

(defun emoji-hint ()
  "The emojis the packs in use have, for a find that matched nothing."
  (let ((seen '()))
    (dolist (name *pack-order*)
      (loop for sticker across (getf (gethash name *catalog*) :stickers)
            do (let ((tag (sticker-tag sticker)))
                 (when (and (plusp (length tag)) (not (member tag seen :test #'string=)))
                   (push tag seen)))))
    (if seen
        (format nil "no sticker fits; the packs in use are tagged with: ~a"
                (emoji-list-text seen 80))
        "no sticker fits, and no pack is loaded: check the packs setting and the bot token")))

(defun resolve-sticker (wanted)
  "WANTED - an id, an emoji, or a few words - as (values PACK STICKER)."
  (multiple-value-bind (pack index) (split-id wanted)
    (if pack
        (let* ((entry (catalog-pack pack))
               (stickers (getf entry :stickers)))
          (unless (< -1 index (length stickers))
            (fail "no sticker ~a: pack ~a has ~a sticker(s), numbered 0 to ~a"
                  wanted pack (length stickers) (1- (length stickers))))
          (values pack (aref stickers index)))
        (let ((hits (search-stickers (words-of wanted)
                                     (and (emoji-p wanted) (nlk:trimmed wanted))
                                     :limit 1)))
          (unless hits (fail "~a" (emoji-hint)))
          (values (getf (first hits) :pack) (getf (first hits) :sticker))))))

;;; --- pacing and the mute switch -------------------------------------------------------

(defun chat-cooldown (chat)
  "CHAT's own cooldown, or the section's."
  (let ((own (gethash chat *cooldowns*)))
    (if (integerp own) own (setting :cooldown-seconds))))

(defun pacing-wait (chat force)
  "Why a sticker cannot go to CHAT yet, or NIL."
  (let* ((cooldown (chat-cooldown chat))
         (last (gethash chat *last-sent*))
         (now (get-universal-time)))
    (when (and (not force) last (plusp cooldown) last
               (>= (- now last) 0) (< (- now last) cooldown))
      (format nil "a sticker already went to this chat ~a s ago: wait ~a s or answer in words (force sends anyway)"
              (- now last) (1+ (- cooldown (- now last)))))))

;;; --- telegram_sticker_find -------------------------------------------------------------

(defun pack-summary (name)
  (let ((entry (gethash name *catalog*)))
    (nlk:json-object
     "name" name
     "title" (or (getf entry :title) name)
     "count" (length (getf entry :stickers))
     "emojis" (let ((seen '()))
                (loop for sticker across (getf entry :stickers)
                      for tag = (sticker-tag sticker)
                      do (when (and (plusp (length tag)) (not (member tag seen :test #'string=)))
                           (push tag seen)))
                (emoji-list-text seen 40)))))

(defun find-stickers (call)
  "The telegram_sticker_find tool: what the packs hold, and the stickers that fit."
  (answering
    (running-settings)
    (let* ((query (or (sticker-find-query call) ""))
           (emoji (or (sticker-find-emoji call) ""))
           (pack (nonblank (sticker-find-pack call)))
           (limit (let ((asked (sticker-find-limit call)))
                    (max 1 (min 50 (if (integerp asked) asked 10)))))
           (refresh (and (sticker-find-refresh call) t)))
      (load-catalog :refresh refresh)
      (let ((answer (nlk:json-object
                     "packs" (map 'vector #'pack-summary *pack-order*))))
        (setf (gethash "in_use" answer) (map 'vector #'identity (packs-in-use)))
        (when *unavailable*
          (setf (gethash "unavailable" answer)
                (map 'vector (lambda (pair)
                               (nlk:json-object "pack" (car pair) "why" (cdr pair)))
                     (reverse *unavailable*))))
        (let ((hits (if (or (plusp (length query)) (plusp (length emoji)))
                        (search-stickers (words-of query)
                                         (or (nonblank (plain-emoji emoji))
                                             (and (emoji-p query) (nlk:trimmed query)))
                                         :pack (and pack (pack-name-of pack))
                                         :limit limit)
                        '())))
          (setf (gethash "count" answer) (length hits)
                (gethash "stickers" answer)
                (map 'vector (lambda (hit)
                               (let ((sticker (getf hit :sticker)))
                                 (nlk:json-object
                                  "id" (sticker-id (getf hit :pack) sticker)
                                  "emoji" (sticker-tag sticker)
                                  "kind" (getf sticker :kind)
                                  "pack" (getf hit :pack)
                                  "pack_title" (getf (gethash (getf hit :pack) *catalog*) :title))))
                             hits))
          (if (plusp (length hits))
              (setf (gethash "next" answer)
                    "send one by id with telegram_sticker_send, or pass the same words or emoji")
              (setf (gethash "hint" answer)
                    (if (or (plusp (length query)) (plusp (length emoji)))
                        (emoji-hint)
                        "no query: this is the list of packs; ask with words or an emoji for the stickers themselves"))))
        (nlk:encode-json-object answer)))))

;;; --- telegram_sticker_send --------------------------------------------------------------

(defun send-sticker (call)
  "The telegram_sticker_send tool: one sticker, paced per chat, as the bot."
  (answering
    (running-settings)
    (let* ((wanted (nonblank (sticker-send-sticker call)))
           (chat (or (nonblank (sticker-send-chat-id call))
                     (nonblank (setting :default-chat-id))))
           (thread (sticker-send-thread-id call))
           (reply (sticker-send-reply-to call))
           (force (and (sticker-send-force call) t)))
      (when (null wanted) (return-from send-sticker (refuse "no sticker named: pass words, an emoji, or an id from telegram_sticker_find")))
      (unless chat
        (return-from send-sticker (refuse "no chat: pass chat_id, or set default_chat_id in the telegramstickers section")))
      (when (gethash chat *muted*)
        (return-from send-sticker (refuse "stickers are switched off in chat ~a: /stickers unmute ~a switches them back on, or answer in words" chat chat)))
      (let ((wait (pacing-wait chat force)))
        (when wait (return-from send-sticker (refuse "~a" wait))))
      (load-catalog)
      (multiple-value-bind (pack sticker) (resolve-sticker wanted)
        (let ((payload (nlk:json-object "chat_id" chat
                                         "sticker" (getf sticker :file-id))))
          (when (and (integerp thread) (plusp thread))
            (setf (gethash "message_thread_id" payload) thread))
          (when (and (integerp reply) (plusp reply))
            (setf (gethash "reply_parameters" payload)
                  (nlk:json-object "message_id" reply "allow_sending_without_reply" t)))
          (let* ((result (api-call "sendSticker" payload))
                 (sent (nlk:json-value result :integer "message_id")))
            (setf (gethash chat *last-sent*) (get-universal-time))
            (let ((answer (nlk:json-object "success" t
                                            "chat_id" chat
                                            "sticker" (sticker-id pack sticker)
                                            "emoji" (sticker-tag sticker)
                                            "kind" (getf sticker :kind))))
              (when (integerp sent) (setf (gethash "message_id" answer) sent))
              (when (and (integerp reply) (plusp reply))
                (setf (gethash "replied_to" answer) reply))
              (setf (gethash "next" answer)
                    "one short line that carries the conversation on, with no description of the sticker")
              (nlk:encode-json-object answer))))))))

;;; --- telegram_sticker_settings ------------------------------------------------------------

(defun string-vector (list)
  (map 'vector #'identity (or list '())))

(defun muted-chats ()
  (let ((chats '()))
    (maphash (lambda (chat muted) (when muted (push chat chats))) (or *muted* (make-hash-table :test #'equal)))
    (sort chats #'string<)))

(defun loaded-counts ()
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (name *pack-order*)
      (setf (gethash name counts) (length (getf (gethash name *catalog*) :stickers))))
    counts))

(defun settings-list-answer ()
  "Everything the section says and everything the catalog holds, as one object."
  (let ((answer (nlk:json-object
                 "ok" t
                 "action" "list"
                 "packs" (string-vector (setting :packs))
                 "in_use" (string-vector (packs-in-use))
                 "default_packs" (and (setting :default-packs) t)
                 "default_chat_id" (or (nonblank (setting :default-chat-id)) "")
                 "cooldown_seconds" (setting :cooldown-seconds)
                 "muted" (string-vector (muted-chats))
                 "available" (string-vector (mapcar (lambda (pair) (car pair)) (reverse *unavailable*))))))
    (let ((counts (loaded-counts)))
      (when (plusp (hash-table-count counts))
        (setf (gethash "loaded" answer) counts)))
    (setf (gethash "next" answer)
          (if (plusp (length (packs-in-use)))
              "ask for a sticker with telegram_sticker_find, or fold one into a reply with telegram_sticker_send"
              "add pack short names to the packs setting (the part after t.me/addstickers/)"))
    answer))

(defun sticker-settings (call)
  "The telegram_sticker_settings tool: list, mute, unmute, or set a chat's cooldown."
  (answering
    (running-settings)
    (let* ((action (string-downcase (nlk:trimmed (or (sticker-settings-action call) ""))))
           (chat (or (nonblank (sticker-settings-chat-id call))
                     (nonblank (setting :default-chat-id))))
           (seconds (sticker-settings-seconds call)))
      (cond
        ((string= action "list")
         (nlk:encode-json-object (settings-list-answer)))
        ((member action '("mute" "unmute") :test #'string=)
         (unless chat (return-from sticker-settings (refuse "which chat? pass chat_id, or set default_chat_id in the section")))
         (if (string= action "mute")
             (setf (gethash chat *muted*) t)
             (remhash chat *muted*))
         (nlk:encode-json-object
          (nlk:json-object "ok" t "action" action "chat_id" chat
                           "muted" (if (string= action "mute") t nil)
                           "next" (if (string= action "mute")
                                      "say it in their language, briefly: no more stickers here, and answer in words from now on"
                                      "stickers may go to this chat again, still paced"))))
        ((string= action "cooldown")
         (unless chat (return-from sticker-settings (refuse "which chat? pass chat_id, or set default_chat_id in the section")))
         (unless (and (integerp seconds) (>= seconds 0))
           (return-from sticker-settings (refuse "cooldown needs seconds, a whole number 0 or more (0 turns pacing off)")))
         (setf (gethash chat *cooldowns*) seconds)
         (nlk:encode-json-object
          (nlk:json-object "ok" t "action" "cooldown" "chat_id" chat "cooldown_seconds" seconds
                           "next" (if (plusp seconds)
                                      "say how often stickers may come now, in their language"
                                      "say pacing is off in this chat"))))
        (t (return-from sticker-settings (refuse "unknown action ~s: use list, mute, unmute or cooldown" action)))))))

;;; --- /stickers -------------------------------------------------------------------------

(defun slash-summary ()
  "The packs in use, what the catalog holds, and the pacing, in one paragraph."
  (let* ((in-use (packs-in-use))
         (counts (loaded-counts))
         (loaded (if (plusp (hash-table-count counts))
                     (format nil "~{~a~^, ~}" (loop for name in *pack-order*
                                                    for count = (gethash name counts)
                                                    collect (format nil "~a (~a sticker~:p)" name count)))
                     "not read yet: /stickers refresh reads them")))
    (format nil "Stickers: ~a pack(s) in use (~{~a~^, ~}); ~a. Cooldown ~a s per chat~@[; default chat ~a~].~@[ Unavailable: ~{~a~^, ~}.~]"
            (length in-use) in-use loaded (setting :cooldown-seconds)
            (nonblank (setting :default-chat-id))
            (mapcar #'car (reverse *unavailable*)))))

(defun slash-text (args)
  "What /stickers says for ARGS."
  (running-settings)
  (let* ((parts (remove "" (ppcre:split "\\s+" (nlk:trimmed (or args ""))) :test #'string=))
         (head (string-downcase (or (first parts) "")))
         (rest (or (second parts) "")))
    (cond
      ((member head '("" "list" "status") :test #'string=)
       (let ((catalog (handler-case (load-catalog) (telegram-stickers-error () nil))))
         (declare (ignore catalog))
         (slash-summary)))
      ((string= head "refresh")
       (load-catalog :refresh t)
       (format nil "Stickers: reloaded. ~a" (slash-summary)))
      ((member head '("mute" "unmute") :test #'string=)
       (let ((chat (or (nonblank rest) (nonblank (setting :default-chat-id)))))
         (unless chat (fail "which chat? say /stickers ~a <chat id>" head))
         (if (string= head "mute")
             (setf (gethash chat *muted*) t)
             (remhash chat *muted*))
         (format nil "Stickers: ~a in chat ~a.~@[ Stickers may go there again, still paced.~]"
                 (if (string= head "mute") "switched off" "switched back on") chat
                 (string= head "unmute"))))
      (t (format nil "Stickers: /stickers [list | refresh | mute <chat id> | unmute <chat id>]~%  ~a"
                 (handler-case (slash-summary) (telegram-stickers-error () "")))))))

(defun run-slash (args session-id)
  "/stickers: what is set, a reload of the packs, and the mute switch of one chat."
  (declare (ignore session-id))
  (handler-case (values (slash-text args) nil)
    (telegram-stickers-error (condition)
      (values (format nil "Stickers: ~a" (telegram-stickers-error-detail condition)) nil))
    (error (condition)
      (values (format nil "Stickers: /stickers failed: ~a"
                      (nlk:one-line (princ-to-string condition)))
              nil))))

;;; --- the cell -------------------------------------------------------------------------

(nle:define-cell telegram-stickers
  (:section ("telegramstickers")
    (:guide "a bot token from BotFather named in token_env, or kept in token_file; packs are sticker set short names (the part after t.me/addstickers/), and four Telegram-made packs come when none is set; next step: ask for a sticker")
    (:one-of "token_env" "token_file")
    ("token_env" :env
     :doc "the environment variable holding the bot token the chat bot already uses (TELEGRAM_BOT_TOKEN)")
    ("token_file" :path
     :doc "a file holding the bot token, which the file's own permissions guard")
    ("api_base" :string :default "https://api.telegram.org"
     :doc "the Bot API base URL; change only for a self-hosted Bot API server")
    ("packs" :list
     :doc "sticker sets to use, by short name or t.me/addstickers/ link, favourite first")
    ("default_chat_id" :string
     :doc "the chat the tools send to when a call names none; groups and channels are negative numbers")
    ("cooldown_seconds" :integer :default 20 :min 0
     :doc "the least seconds between two stickers in one chat; 0 turns pacing off")
    ("timeout" :integer :default 15 :min 1
     :doc "how long one Bot API call may take, in seconds"))
  (:settings #'read-settings)
  (:help :telegram-stickers
         "telegram-stickers: three tools that find and send stickers from your own Telegram packs, as the chat bot"
         *manual*)
  (:tool "telegram_sticker_find" (nlk:contract 'sticker-find) #'find-stickers)
  (:tool "telegram_sticker_send" (nlk:contract 'sticker-send) #'send-sticker)
  (:tool "telegram_sticker_settings" (nlk:contract 'sticker-settings) #'sticker-settings)
  (:command "stickers" 'run-slash
            :description "Sticker packs: what is set, reload the packs, mute or unmute one chat"
            :argument-hint "[list | refresh | mute <chat id> | unmute <chat id>]")
  (:start (lambda () (reset-state))))
