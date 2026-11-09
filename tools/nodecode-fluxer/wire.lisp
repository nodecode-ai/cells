;;;; wire.lisp --- Fluxer's bot REST API, one function per operation.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The same calls the original adapter makes, with the bot token as
;;;; `Authorization: Bot <token>' against api.fluxer.app/v1 (or a
;;;; self-hosted base the section names):
;;;;
;;;;   POST   /channels/{channel}/messages                            post one message
;;;;   GET    /channels/{channel}/messages?limit=N                    recent messages
;;;;   GET    /users/@me/guilds?limit=200                             the bot's guilds
;;;;   GET    /guilds/{guild}/channels                               one guild's channels
;;;;   PUT    /channels/{channel}/messages/{message}/reactions/{e}/@me      react
;;;;   PUT    /channels/{channel}/pins/{message}                      pin
;;;;   DELETE /channels/{channel}/pins/{message}                      unpin
;;;;
;;;; A message this cell posts carries `allowed_mentions' and has dangerous
;;;; ping syntax broken with a zero-width space, the adapter's own
;;;; fail-closed rule: an @everyone a model writes pings nobody unless the
;;;; section allows mentions. *HTTP* is the one seam a test stubs.

(in-package #:nodecode-fluxer)

(defparameter *http* #'nlk:http
  "The exchange this cell dials through: NLE's HTTP, stubbed by a test.")

(defparameter +default-base+ "https://api.fluxer.app/v1"
  "The hosted Fluxer API; a self-hosted deployment names its own base URL.")

(defparameter +zero-width-space+ (string (code-char #x200B))
  "What neutralizes a mention: a client does not read one across it.")

(defvar *last-error* nil
  "Why the last verb that failed failed, for /fluxer; NIL once one answers.")

;;; --- where and as whom ---------------------------------------------------------

(defun bot-token ()
  "The section's bot token, trimmed."
  (nlk:trimmed (or (setting :bot-token) "")))

(defun base ()
  "The API root this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-base+)))

(defun allow-mentions-p ()
  (eq t (setting :allow-mentions)))

(defun headers ()
  `(("authorization" . ,(format nil "Bot ~a" (bot-token)))
    ("content-type" . "application/json")
    ("accept" . "application/json")
    ("user-agent" . "nodecode-fluxer")))

(defun check-config ()
  "Refuse a section that could never reach Fluxer, naming the member."
  (when (zerop (length (bot-token)))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "fluxer: no bot token: put the Fluxer bot token in the fluxer "
                                 "section's \"bot_token\" (in /settings, or in config.jsonc), "
                                 "then run (restart-cells \"nodecode-fluxer\")")))
  (when (zerop (length (base)))
    (error 'nlk:config-refusal
           :detail "fluxer: \"base_url\" is empty: name the API root, or leave it out for the hosted one")))

;;; --- one exchange --------------------------------------------------------------

(defun utf-8-octets (code)
  "The UTF-8 octets of one character code."
  (cond ((< code #x80) (list code))
        ((< code #x800) (list (logior #xC0 (ash code -6))
                              (logior #x80 (logand code #x3F))))
        ((< code #x10000) (list (logior #xE0 (ash code -12))
                                (logior #x80 (logand (ash code -6) #x3F))
                                (logior #x80 (logand code #x3F))))
        (t (list (logior #xF0 (ash code -18))
                 (logior #x80 (logand (ash code -12) #x3F))
                 (logior #x80 (logand (ash code -6) #x3F))
                 (logior #x80 (logand code #x3F))))))

(defun percent-encode (string)
  "STRING as one URL path or query segment: unreserved characters as they
are, everything else as the UTF-8 octets it is."
  (with-output-to-string (out)
    (loop for ch across string
          do (if (or (alphanumericp ch) (find ch "-._~" :test #'char=))
                 (write-char ch out)
                 (dolist (octet (utf-8-octets (char-code ch)))
                   (format out "%~2,'0X" octet))))))

(defun query-string (pairs)
  "PAIRS, an alist of name and value, as a query string."
  (format nil "~{~a=~a~^&~}"
          (loop for (name . value) in pairs
                collect (percent-encode name) collect (percent-encode value))))

(defun decode-body (answer)
  "ANSWER, an HTTP body, as a decoded value; an empty one is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (nlk:trimmed answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        (t answer)))

(defun sentence (body)
  "What a refused response said, one line, at most 300 characters, or NIL."
  (let ((text (typecase body
                (string body)
                (t (ignore-errors (nlk:encode-json-object body))))))
    (when (and text (plusp (length text)))
      (let ((flat (substitute #\Space #\Newline text)))
        (subseq flat 0 (min 300 (length flat)))))))

(defun refused (what status body)
  "Refuse: WHAT was answered STATUS, saying BODY where it said anything."
  (fail "~a answered HTTP ~d~@[: ~a~]" what status (sentence body)))

(defun exchange (method path &key query content)
  "METHOD PATH on the configured base. => (values DECODED STATUS)."
  (let ((timeout (setting :timeout))
        (url (format nil "~a~a~@[?~a~]" (base) path (and query (query-string query)))))
    (multiple-value-bind (body status)
        (funcall *http* method url
                 :headers (headers)
                 :content content
                 :timeout timeout
                 :connect-timeout (min 10 timeout))
      (values (decode-body body) status))))

(defun answering (what method path &key query content)
  "METHOD PATH, its 2xx answer decoded, else a refusal naming WHAT."
  (multiple-value-bind (value status) (exchange method path :query query :content content)
    (if (<= 200 status 299)
        value
        (refused what status value))))

;;; --- mentions ----------------------------------------------------------------

(defun replace-all (text needle replacement)
  "TEXT with every NEEDLE replaced by REPLACEMENT."
  (let ((out (make-string-output-stream))
        (start 0)
        (step (length needle)))
    (loop for at = (search needle text :start2 start)
          do (if at
                 (progn (write-string text out :start start :end at)
                        (write-string replacement out)
                        (setf start (+ at step)))
                 (return)))
    (write-string text out :start start)
    (get-output-stream-string out)))

(defun neutralized (text)
  "TEXT with every global, role and user mention broken by a zero-width
space, so a model cannot ping a channel by writing one."
  (reduce (lambda (string pair) (replace-all string (first pair) (second pair)))
          (list (list "@everyone" (format nil "@~aeveryone" +zero-width-space+))
                (list "@here" (format nil "@~ahere" +zero-width-space+))
                (list "<@" (format nil "<@~a" +zero-width-space+)))
          :initial-value (or text "")))

(defun message-content (text)
  "TEXT as it goes on the wire: neutralized unless the section allows mentions."
  (if (allow-mentions-p) (or text "") (neutralized text)))

(defun allowed-mentions ()
  "The `allowed_mentions' object: nothing is parsed unless the section
allows it, which is what keeps a neutralized mention neutralized."
  (if (allow-mentions-p)
      (nlk:json-object "parse" (vector "everyone" "roles" "users")
                        "replied_user" t)
      (nlk:json-object "parse" (vector)
                        "replied_user" :false)))

;;; --- the operations ------------------------------------------------------------

(defun post-message (channel content &key reply-to)
  "Post CONTENT in CHANNEL, as a reply to REPLY-TO when given. => the message."
  (let ((payload (nlk:json-object "content" (message-content content)
                                   "allowed_mentions" (allowed-mentions))))
    (when (and reply-to (plusp (length reply-to)))
      (setf (gethash "message_reference" payload)
            (nlk:json-object "message_id" reply-to)))
    (answering "posting to the channel"
               "POST" (format nil "/channels/~a/messages" (percent-encode channel))
               :content (nlk:encode-json-object payload))))

(defun fetch-messages (channel limit)
  "The last LIMIT messages of CHANNEL, oldest first. => a list."
  (let* ((value (answering "reading the channel"
                            "GET" (format nil "/channels/~a/messages" (percent-encode channel))
                            :query (list (cons "limit" (princ-to-string limit)))))
         (items (cond ((vectorp value) value)
                      ((hash-table-p value)
                       (or (nlk:json-value value :array "items")
                           (nlk:json-value value :array "messages")
                           #()))
                      (t #()))))
    (coerce (reverse items) 'list)))

(defun guild-list ()
  "Every guild the bot is in. => a list."
  (let ((value (answering "listing the guilds" "GET" "/users/@me/guilds"
                           :query (list (cons "limit" "200")))))
    (cond ((vectorp value) (coerce value 'list))
          ((hash-table-p value)
           (coerce (or (nlk:json-value value :array "guilds")
                       (nlk:json-value value :array "items")
                       #())
                   'list))
          (t '()))))

(defun guild-channels (guild)
  "GUILD's channels. => a list."
  (let ((value (answering "listing the guild's channels" "GET"
                           (format nil "/guilds/~a/channels" (percent-encode guild)))))
    (if (vectorp value) (coerce value 'list) '())))

(defun put-reaction (channel message emoji)
  "React to MESSAGE in CHANNEL with EMOJI, as the bot."
  (answering "reacting to the message" "PUT"
             (format nil "/channels/~a/messages/~a/reactions/~a/@me"
                     (percent-encode channel) (percent-encode message) (percent-encode emoji))))

(defun set-pin (channel message &key remove)
  "Pin MESSAGE in CHANNEL, or unpin it with REMOVE."
  (answering (if remove "unpinning the message" "pinning the message")
             (if remove "DELETE" "PUT")
             (format nil "/channels/~a/pins/~a"
                     (percent-encode channel) (percent-encode message))))

;;; --- what a message and a channel read as ----------------------------------------

(defun field (object name)
  "NAME's value in OBJECT, whichever of the shapes a deployment answered."
  (typecase object
    (hash-table (gethash name object))
    (list (cdr (assoc name object :test #'equal)))
    (t nil)))

(defun text-field (object name)
  (let ((value (field object name)))
    (and value (princ-to-string value))))

(defun author-name (message)
  "Who wrote MESSAGE, as the adapter reads it: the global name, else the
display name, else the username, else the name."
  (let ((author (field message "author")))
    (or (text-field author "global_name")
        (text-field author "display_name")
        (text-field author "username")
        (text-field author "name")
        (text-field message "author_id")
        "unknown")))

(defun attachments-of (message)
  "MESSAGE's attachments as a list of names."
  (let ((items (field message "attachments")))
    (loop for item across (if (vectorp items) items #())
          for name = (or (text-field item "filename")
                         (text-field item "title")
                         (text-field item "name"))
          when name collect name)))

(defun one-line (text)
  "TEXT with its newlines flattened, so one message is one line."
  (let ((flat (substitute #\Space #\Newline (substitute #\Space #\Return (or text "")))))
    (if (> (length flat) 500) (concatenate 'string (subseq flat 0 500) "…") flat)))

(defun message-line (message)
  "One message as a line: its id, who wrote it, what it says."
  (let ((attached (attachments-of message)))
    (format nil "~a ~a: ~a~@[ [attached: ~{~a~^, ~}]~]"
            (or (text-field message "id") "?")
            (author-name message)
            (one-line (text-field message "content"))
            attached)))
