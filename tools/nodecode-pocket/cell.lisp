;;;; cell.lisp --- the operator's phone, through Notify by belweave.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; An update is a notification the person's phone shows. A question is an
;;;; interaction Notify keeps until it is answered or expires, and this cell
;;;; reads the answer back by its id. Nothing polls in the background: a tool
;;;; call is one exchange, and a question not yet answered is read again by
;;;; the next notify_poll.
;;;;
;;;; Everything this cell sends leaves the machine for Notify's own service,
;;;; so the tool descriptions and the manual say so, and the runtime refuses
;;;; to put the configured key itself on the wire as content.

(in-package #:nodecode-pocket)

;;; --- the wire -------------------------------------------------------------

(defparameter +api-base+ "https://notify.belweave.ai/api/agent"
  "Notify by belweave's agent API: the one origin this cell talks to, and the
third party every update and question goes to.")

(defparameter +user-agent+ "nodecode-pocket/0.1.0 (+https://github.com/nodecode-ai/cells)"
  "Names this client on every exchange.")

(defparameter +max-wait+ 25
  "Notify's own ceiling on one wait, in seconds; a longer ask is polled again.")

(defparameter +kinds+ '("approval" "yes_no" "reply")
  "The three questions Notify can put on a phone: a yes/no decision to act,
a yes or a no, and a written reply.")

(defparameter +title-limit+ 80
  "Notify's ceiling on an update's title, in characters.")

(defparameter +body-limit+ 8000
  "Notify's ceiling on an update's body, in characters.")

(defparameter +prompt-limit+ 2000
  "Notify's ceiling on a question's prompt, in characters.")

(defparameter +key-limit+ 200
  "Notify's ceiling on an idempotency key, in characters.")

(defparameter *kept-receipts* 8
  "How many receipts /pocket lists.")

(defparameter *sent* nil
  "The receipts this image made, newest first. Not durable: a restart forgets
them, and the phone's own inbox is the record.")

(defun http-exchange (method url body headers timeout)
  "One HTTP exchange, the shape NLK:HTTP answers: (values BODY STATUS HEADERS)."
  (nlk:http method url :content body :headers headers :timeout timeout
            :connect-timeout (min 10 timeout)))

(defvar *request* #'http-exchange
  "The exchange every tool goes through: a function of METHOD URL BODY HEADERS
and TIMEOUT answering (values BODY STATUS ...). A test binds this instead of
dialling; nothing else in the cell knows HTTP.")

;;; --- words ----------------------------------------------------------------

(defun trimmed (text)
  "TEXT without the whitespace a person or a model pads a line with."
  (string-trim '(#\Space #\Tab #\Newline #\Return) (or text "")))

(defun non-empty (text)
  "TEXT trimmed, or NIL when nothing is left."
  (let ((text (trimmed text)))
    (and (plusp (length text)) text)))

(defun limited (text limit what)
  "TEXT, refused when it is longer than LIMIT characters."
  (when (and text (> (length text) limit))
    (fail "the ~a is ~d characters, and Notify takes at most ~d: shorten it"
          what (length text) limit))
  text)

(defun origin ()
  "The agent API origin this cell was configured with, without its trailing slash."
  (let ((base (non-empty (setting :base-url))))
    (string-right-trim "/" (or base +api-base+))))

(defun api-token ()
  "The Notify agent key, read from the environment variable the section names."
  (let* ((name (or (non-empty (setting :api-token-env)) "NOTIFY_API_TOKEN"))
         (value (uiop:getenv name)))
    (unless (and value (plusp (length (trimmed value))))
      (fail "no Notify key: the environment variable ~a is ~a. Create an agent key with \
notifications:send, interactions:create and interactions:read at https://notify.belweave.ai, \
export it under that name, then (restart-cells \"nodecode-pocket\")"
            name (if value "empty" "unset")))
    (trimmed value)))

(defun project (named)
  "The project NAMED, or the section's own when the call names none."
  (or (non-empty named) (non-empty (setting :project)) "Nodecode"))

(defun identifier (value)
  "VALUE, refused unless it is a Notify id this cell can put in a path."
  (let ((value (trimmed value)))
    (unless (and (plusp (length value))
                 (<= (length value) 128)
                 (every (lambda (character)
                          (or (alphanumericp character)
                              (member character '(#\- #\_))))
                        value))
      (fail "~s is not a Notify interaction id: they are letters, digits, - and _"
            (if (plusp (length value)) value "an empty string")))
    value))

(defun idempotency-key (value)
  "VALUE as the Idempotency-Key header, or NIL when the call names none."
  (let ((value (non-empty value)))
    (when (and value (some (lambda (character) (< (char-code character) 32)) value))
      (fail "the idempotency_key carries a control character; use 1-~d plain characters"
            +key-limit+))
    (limited value +key-limit+ "idempotency_key")))

(defun remember (receipt)
  "Keep RECEIPT for /pocket, newest first, at most *KEPT-RECEIPTS* of them."
  (push receipt *sent*)
  (when (> (length *sent*) *kept-receipts*)
    (setf *sent* (subseq *sent* 0 *kept-receipts*)))
  receipt)

(defun credential-free (text)
  "TEXT, refused when it carries the configured key: the key rides the header,
and a body that repeats it puts a credential on the service's own record."
  (when (and (stringp text) (search (api-token) text))
    (fail "the text repeats the Notify key itself; send the key in api_token_env only"))
  text)

;;; --- one exchange ---------------------------------------------------------

(defun refuse (status object)
  "The refusal a failed exchange is, in words that say what to do next."
  (let ((code (nlk:json-value object :string "code"))
        (required (nlk:json-value object :array "required")))
    (case status
      (401 (fail "Notify refused the key (HTTP 401): put a current agent key in the \
variable api_token_env names, then (restart-cells \"nodecode-pocket\")"))
      (403 (fail "the key may not do that (HTTP 403~@[ ~a~])~@[: it is missing ~{~a~^, ~}~]. \
Grant the scope at https://notify.belweave.ai" code (coerce required 'list)))
      (429 (fail "Notify is rate limiting (HTTP 429): retry the same write with the same \
idempotency_key once it clears"))
      (t (fail "Notify answered HTTP ~d~@[ (~a)~]" status code)))))

(defun call (method path &key body idempotency (timeout nil))
  "One authenticated call to the agent API at PATH, with BODY the JSON text.
=> the decoded response object, or a refusal naming what to do."
  (let* ((url (concatenate 'string (origin) path))
         (headers (remove nil
                          (list (cons "accept" "application/json")
                                (cons "authorization" (format nil "Bearer ~a" (api-token)))
                                (cons "user-agent" +user-agent+)
                                (if body (cons "content-type" "application/json"))
                                (if idempotency (cons "idempotency-key" idempotency)))))
         (limit (or timeout (setting :timeout-seconds))))
    (multiple-value-bind (answer status) (funcall *request* method url body headers limit)
      (let ((object (ignore-errors
                     (nlk:decode-json (nlk:body-text answer) :whole t))))
        (unless (and (integerp status) (< status 300) (hash-table-p object))
          (refuse (or status 0) object))
        object))))

(defun interaction-of (object)
  "OBJECT's interaction, refused when the answer carries none."
  (or (nlk:json-value object :object "interaction")
      (fail "Notify answered without an interaction")))

;;; --- the answers ----------------------------------------------------------

(defun answer-text (interaction)
  "INTERACTION as the one line a question's answer is: what the person said,
or that nothing has come back yet."
  (let ((status (or (non-empty (nlk:json-value interaction :string "status")) "pending"))
        (response (non-empty (nlk:json-value interaction :string "response")))
        (id (nlk:json-value interaction :string "id")))
    (cond ((member status '("approved" "yes") :test #'string=)
           (format nil "~a~@[: ~a~]~@[ (interaction ~a)~]" status response id))
          ((string= status "replied")
           (format nil "replied~@[ (interaction ~a)~]: ~a" id (or response "")))
          ((member status '("denied" "no" "expired" "canceled") :test #'string=)
           (format nil "~a~@[ (interaction ~a)~]: no permission~@[ - ~a~]"
                   status id response))
          (t (format nil "still pending (interaction ~a): read it again with notify_poll, \
or wait inside notify_ask" (or id "?"))))))

(defun receipt-line (object)
  "The line a send answers: what the phone's service took, by id."
  (let* ((notification (nlk:json-value object :object "notification"))
         (id (nlk:json-value notification :string "id"))
         (accepted (nlk:json-value object :integer "accepted")))
    (format nil "sent to the phone~@[: ~d destination~:p accepted~]~@[, notification ~a~]"
            accepted id)))

;;; --- what a caller sends --------------------------------------------------

(nlk:define-contract pocket-update
  (:doc "One update for the operator's phone: what to tell them, and where to read more. \
The text leaves this machine for Notify by belweave (https://notify.belweave.ai).")
  ("body" :text :required t
   :doc "the update itself, as the phone shows it: 1-8000 characters, no secret")
  ("title" :string :default ""
   :doc "the headline, at most 80 characters; the project's name when empty")
  ("summary" :string :default ""
   :doc "one line the phone shows under the title, at most 500 characters")
  ("url" :string :default ""
   :doc "an https link the update opens, when the person taps it")
  ("project" :string :default ""
   :doc "the Notify project to group it under; the section's project when empty")
  ("idempotency_key" :string :default ""
   :doc "reuse the same key to retry one intended update instead of sending a second"))

(nlk:define-contract pocket-question
  (:doc "A question for the operator's phone, answered there as approve/deny, yes/no, or \
in words. The answer is what the person said: only an approved approval decides the one \
action the prompt names, and the caller still runs and verifies that action itself.")
  ("title" :text :required t
   :doc "the question's headline on the phone, at most 80 characters")
  ("prompt" :text :required t
   :doc "what is being asked, in one or two sentences: at most 2000 characters")
  ("kind" :text :default "yes_no" :options +kinds+
   :doc "approval (act or not), yes_no, or reply (a written answer)")
  ("expires_in_seconds" :integer :default 900
   :doc "how long the question stays answerable, 30 to 86400 seconds")
  ("wait_seconds" :integer :default 0
   :doc "wait up to this long inside the call for the answer; Notify holds 25, and \
notify_poll reads a longer wait")
  ("project" :string :default ""
   :doc "the Notify project to group it under; the section's project when empty")
  ("idempotency_key" :string :default ""
   :doc "reuse the same key to retry one intended question, not to ask twice"))

(nlk:define-contract pocket-answer
  (:doc "Read back the answer to a question already asked on the phone.")
  ("interaction_id" :text :required t
   :doc "the interaction id the ask answered: letters, digits, - and _")
  ("wait_seconds" :integer :default 0
   :doc "wait up to this long for an answer, at most 25 seconds"))

(defun update-object (update)
  "UPDATE as the wire object POST /notifications takes."
  (let ((body (credential-free (limited (pocket-update-body update) +body-limit+ "body"))))
    (nlk:json-object
     :opt "title" (limited (non-empty (pocket-update-title update)) +title-limit+ "title")
     "body" body
     :opt "summary" (non-empty (pocket-update-summary update))
     :opt "url" (non-empty (pocket-update-url update))
     "project" (project (pocket-update-project update)))))

(defun question-object (question)
  "QUESTION as the wire object POST /interactions takes."
  (let ((expires (or (pocket-question-expires-in-seconds question) 900)))
    (unless (<= 30 expires 86400)
      (fail "expires_in_seconds is ~d: Notify takes 30 to 86400" expires))
    (nlk:json-object
     "title" (limited (pocket-question-title question) +title-limit+ "title")
     "prompt" (credential-free (limited (pocket-question-prompt question) +prompt-limit+
                                        "prompt"))
     "kind" (pocket-question-kind question)
     "presentation" "notification"
     "expiresInSeconds" expires
     :opt "project" (non-empty (pocket-question-project question)))))

;;; --- the three tools ------------------------------------------------------

(defun send-update (update)
  "UPDATE, a POCKET-UPDATE, to the operator's phone => the receipt line."
  (let* ((object (call :post "/notifications"
                        :body (nlk:encode-json-object (update-object update))
                        :idempotency (idempotency-key (pocket-update-idempotency-key update))))
         (receipt (receipt-line object)))
    (remember receipt)))

(defun ask-on-phone (question)
  "QUESTION, a POCKET-QUESTION, on the operator's phone => the answer, or that
it is pending and the id notify_poll reads."
  (let* ((wait (max 0 (min +max-wait+ (or (pocket-question-wait-seconds question) 0))))
         (object (call :post "/interactions"
                       :body (nlk:encode-json-object (question-object question))
                       :idempotency (idempotency-key
                                     (pocket-question-idempotency-key question))))
         (interaction (interaction-of object))
         (id (identifier (or (nlk:json-value interaction :string "id")
                             (fail "Notify accepted the question but named no interaction")))))
    (remember (format nil "asked ~a: ~a" id (pocket-question-title question)))
    (if (plusp wait)
        (answer-text (interaction-of
                      (call :get (format nil "/interactions/~a/wait?timeout=~d" id wait)
                            :timeout (max (setting :timeout-seconds) (+ wait 5)))))
        (answer-text interaction))))

(defun poll-answer (lookup)
  "LOOKUP, a POCKET-ANSWER, => the answer to that interaction, or that it is
still pending."
  (let* ((id (identifier (pocket-answer-interaction-id lookup)))
         (wait (max 0 (min +max-wait+ (or (pocket-answer-wait-seconds lookup) 0)))))
    (answer-text
     (interaction-of
      (if (plusp wait)
          (call :get (format nil "/interactions/~a/wait?timeout=~d" id wait)
                :timeout (max (setting :timeout-seconds) (+ wait 5)))
          (call :get (format nil "/interactions/~a" id)))))))

;;; --- the verbs an eval form calls -----------------------------------------

(defun notify (text &key title summary url project idempotency-key)
  "Send TEXT to the operator's phone through Notify => the receipt line.
A layer rewording what the cell says rebinds this, not the tool."
  (send-update (make-pocket-update :body text :title (or title "") :summary (or summary "")
                                   :url (or url "") :project (or project "")
                                   :idempotency-key (or idempotency-key ""))))

(defun ask (prompt &key (title "Nodecode needs an answer") (kind "yes_no") (wait 0)
                     (expires 900) project idempotency-key)
  "Put PROMPT on the operator's phone and read what comes back. WAIT is how
long to wait inside the call (Notify holds 25); EXPIRES how long the question
stays answerable; KIND is approval, yes_no or reply."
  (ask-on-phone (make-pocket-question :title title :prompt prompt :kind kind
                                      :expires-in-seconds expires :wait-seconds wait
                                      :project (or project "")
                                      :idempotency-key (or idempotency-key ""))))

(defun answer (interaction-id &key (wait 0))
  "The answer to INTERACTION-ID, or that it is still pending."
  (poll-answer (make-pocket-answer :interaction-id interaction-id :wait-seconds wait)))

;;; --- /pocket --------------------------------------------------------------

(defun key-state ()
  "One line saying whether the key the section names is there."
  (if (ignore-errors (api-token))
      (format nil "key: found in ~a" (setting :api-token-env))
      (format nil "key: not set yet - export ~a with a Notify agent key"
              (setting :api-token-env))))

(defun status-text ()
  "What /pocket says with nothing typed: where it sends, and what it sent."
  (format nil "pocket: ~a~%~a~%project: ~a~@[~%~{~a~^~%~}~]"
          (origin) (key-state) (setting :project) *sent*))

(defun run-command (args session-id)
  "One /pocket invocation => the answer's text, and a panel when nothing was
typed: the status, and a row that sends what the operator types."
  (declare (ignore session-id))
  (let ((args (trimmed args)))
    (handler-case
        (cond ((member args '("" "status") :test #'string=)
               (let ((text (status-text)))
                 (values text
                         (nle:list-dialog
                          :title "Pocket" :summary text
                          :rows (list (nle:ask-row "Send an update" "Update" "/pocket"
                                                    :placeholder "what the phone should say"
                                                    :verb "send"))))))
              ((string= args "test")
               (values (notify "Pocket is connected: this is a test update from Nodecode."
                               :title "Nodecode test")
                       nil))
              (t (values (notify args :title "Nodecode") nil)))
      (pocket-error (condition) (values (princ-to-string condition) nil)))))

;;; --- the manual ----------------------------------------------------------

(defparameter +manual+
  "pocket: the operator's phone, through Notify by belweave (https://notify.belweave.ai), which is a third party every update and question goes to. Tools: notify_send (body, title, summary, url, project, idempotency_key) sends an update and answers a receipt; notify_ask (title, prompt, kind approval|yes_no|reply, expires_in_seconds, wait_seconds) puts a question on the phone and answers the decision, the words, or that it is pending with an interaction id; notify_poll (interaction_id, wait_seconds) reads that id again. From eval: (pocket:notify \"the build is green\" :title \"Release\") answers the receipt line; (pocket:ask \"Deploy 2.4.1 now?\" :title \"Deploy\" :kind \"yes_no\" :wait 20) answers yes, no, approved, denied, replied: TEXT, or still pending; (pocket:answer \"int_abc\") reads one id. One wait holds 25 seconds, so a longer decision is asked once and polled. An answer is what the person said, not permission to run anything: only an approved approval decides the single action its prompt named, and the caller still runs and verifies that action itself. The key is the name of an environment variable in the `pocket' section, never a value in the config; /pocket alone lists where it sends and what it sent. Send selected text only: the body, title, summary, prompt and project leave this machine, so no tokens, no file contents and nothing the operator did not ask to be told."
  "The (help :pocket) text: the tools' shape, the eval verbs, and what an answer is.")

(nle:define-cell pocket
  (:section ("pocket")
    (:guide "Notify by belweave puts an update or a question on the operator's phone. Create an agent key at https://notify.belweave.ai with notifications:send, interactions:create and interactions:read; export it, and set api_token_env to the variable's name. Then /pocket test says whether the phone took a notification, and notify_ask asks the phone a question")
    ("api_token_env" :env :required t :default "NOTIFY_API_TOKEN"
     :doc "the environment variable holding the Notify agent key")
    ("project" :string :default "Nodecode"
     :doc "the Notify project updates and questions are grouped under")
    ("base_url" :string :default "https://notify.belweave.ai/api/agent"
     :doc "the agent API origin; leave it unless you proxy Notify yourself")
    ("timeout_seconds" :integer :default 15 :min 1
     :doc "one exchange's ceiling, in seconds"))
  (:tool "notify_send" (nlk:contract 'pocket-update) 'send-update)
  (:tool "notify_ask" (nlk:contract 'pocket-question) 'ask-on-phone)
  (:tool "notify_poll" (nlk:contract 'pocket-answer) 'poll-answer)
  (:help :pocket "pocket: an update or a question on the operator's phone, through Notify by belweave"
         +manual+)
  (:command "pocket" 'run-command
            :description "The operator's phone: status, a test update, or an update saying what you type"))
