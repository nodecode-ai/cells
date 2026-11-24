;;;; cell-test.lisp --- the cell's lifecycle, and one turn's message, offline.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Nothing here dials Telegram: NlK:HTTP is replaced by a stub that answers
;;;; a Bot API body and keeps every call, so "one message was sent and then
;;;; rewritten" is a fact a test reads rather than a chat a person looks at.

(in-package #:nodecode.test)

(define-test-slice "kanban-stream" "KANBAN-STREAM-CELL-" :start nodecode-kanban-stream:start-cell)

(define-cell-lifecycle-tests "kanban-stream"
  (:config "bot_token" "123:abc" "chat_id" "42")
  (:hooks :frame)
  (:help :kanban-stream)
  (:command "kanban-stream")
  (:refused ("throttle_seconds" 0)))

;;; --- the stub ------------------------------------------------------------------

(defvar *telegram-calls* '()
  "The Bot API calls the stub answered this test, newest first: (:name :body).")

(defvar *telegram-answer* nil
  "A function of the method name answering a body to send instead of the
default success, or NIL for the default.")

(defun telegram-method (url)
  "The Bot API method URL names: the last path segment."
  (let ((slash (position #\/ url :from-end t)))
    (if slash (subseq url (1+ slash)) url)))

(defun telegram-success (name)
  "The body Telegram answers NAME with when it worked."
  (if (equal name "sendMessage")
      (nlk:json-object "ok" t "result" (nlk:json-object "message_id" 7))
      (nlk:json-object "ok" t "result" t)))

(defmacro with-telegram (&body body)
  "Run BODY with every Bot API call stubbed and kept in *TELEGRAM-CALLS*."
  `(let ((*telegram-calls* '())
         (*telegram-answer* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                  (declare (ignore method))
                                  (let* ((name (telegram-method url))
                                         (body (nlk:decode-json (getf args :content)))
                                         (said (and *telegram-answer*
                                                    (funcall *telegram-answer* name))))
                                    (push (list :name name :body body) *telegram-calls*)
                                    (values (nlk:encode-json-object
                                             (or said (telegram-success name)))
                                            200)))
       ,@body)))

(defun telegram-called (&optional name)
  "The calls the stub answered, newest first, less those not of NAME."
  (if name
      (remove-if-not (lambda (call) (equal name (getf call :name))) *telegram-calls*)
      *telegram-calls*))

(defun telegram-text (call)
  "The text a stub call carried."
  (nlk:json-value (getf call :body) :string "text"))

;;; --- facts, published the way the gateway publishes them --------------------------

(defun stream-fact (type &key (session "s1") (turn "t1") payload)
  "One durable fact of TYPE, through the cell's :frame hook."
  (cell-publish "nodecode-kanban-stream"
                (cell-frame-op type :session session :turn turn :payload payload)
                nil))

(defun stream-call (tool &key (session "s1") (turn "t1") (arguments "{}"))
  "A tool call of TOOL, as the engine records one."
  (stream-fact "turn.tool_call_started" :session session :turn turn
               :payload (nlk:json-object "tool-name" tool "arguments" arguments
                                          "call-id" "c1")))

(defmacro with-stream ((&rest pairs) &body body)
  "Run BODY with the cell started on PAIRS and Telegram stubbed."
  `(with-telegram
     (with-cell-stop ((kanban-stream-start ,@pairs))
       ,@body)))

;;; --- one turn, one message -------------------------------------------------------

(deftest kanban-stream-cell-a-tool-call-opens-the-message ()
  (with-stream ("bot_token" "123:abc" "chat_id" "42" "history" 3)
    (stream-call "eval" :arguments "{\"form\":\"(sh \\\"ls\\\")\"}")
    (is (= 1 (length (telegram-called))) "one call: the message is opened")
    (is (equal "sendMessage" (getf (first *telegram-calls*) :name)))
    (let ((text (telegram-text (first *telegram-calls*))))
      (is-carrying text ("s1") ("eval") ("(sh")))))

(deftest kanban-stream-cell-the-end-rewrites-and-pins-the-one-message ()
  (with-stream ("bot_token" "123:abc" "chat_id" "42")
    (stream-call "eval")
    (stream-call "sh" :arguments "{\"command\":\"make\"}")
    (is (= 1 (length (telegram-called))) "the second call is throttled into the same message")
    (stream-fact "turn.completed" :payload (nlk:json-object "detail" "the build is green"))
    (is (= 3 (length (telegram-called))) "one edit and one pin at the end")
    (is (equal "pinChatMessage" (getf (first *telegram-calls*) :name)) "the pin goes last")
    (let ((edit (second *telegram-calls*)))
      (is (equal "editMessageText" (getf edit :name)))
      (is (= 7 (nlk:json-value (getf edit :body) :integer "message_id")) "the same message")
      (is-carrying (text (telegram-text edit)) ("Done.") ("the build is green")))))

(deftest kanban-stream-cell-a-failed-turn-says-so ()
  (with-stream ("bot_token" "123:abc" "chat_id" "42" "pin_final" nil)
    (stream-call "eval")
    (stream-fact "turn.failed" :payload (nlk:json-object "detail" "the provider refused"))
    (let ((edit (first (telegram-called "editMessageText"))))
      (is-carrying (text (telegram-text edit)) ("Blocked.") ("the provider refused")))
    (is (null (telegram-called "pinChatMessage")) "pin_final false leaves the message loose")))

(deftest kanban-stream-cell-tokens-ride-the-footer ()
  (with-stream ("bot_token" "123:abc" "chat_id" "42" "throttle_seconds" 1)
    (stream-call "eval")
    (sleep 1.1)
    (stream-fact "turn.usage" :payload (nlk:json-object "output-tokens" 400))
    (is-present (edit (first (telegram-called "editMessageText")))
      "the rate is shown once a round reports tokens"
      (is-carrying (text (telegram-text edit)) ("tok/s")))))

;;; --- what is watched --------------------------------------------------------------

(deftest kanban-stream-cell-only-the-listed-sessions-stream ()
  (with-stream ("bot_token" "123:abc" "chat_id" "42" "sessions" (vector "s2"))
    (stream-call "eval" :session "s1")
    (is (null (telegram-called)) "a session off the list is silent")
    (stream-call "eval" :session "s2")
    (is (= 1 (length (telegram-called))) "a listed one streams")))

(deftest kanban-stream-cell-mute-stops-posting ()
  (with-stream ("bot_token" "123:abc" "chat_id" "42")
    (cell-entry "nodecode-kanban-stream" "kanban-stream" "off")
    (stream-call "eval")
    (is (null (telegram-called)) "muted means nothing is sent")
    (is-carrying (answer (cell-entry "nodecode-kanban-stream" "kanban-stream" "on")) ("streaming again"))
    (stream-call "eval")
    (is (= 1 (length (telegram-called))) "and it streams again")))

(deftest kanban-stream-cell-a-new-turn-opens-a-new-message ()
  (with-stream ("bot_token" "123:abc" "chat_id" "42" "pin_final" nil)
    (stream-call "eval" :turn "t1")
    (stream-fact "turn.completed" :turn "t1")
    (stream-call "eval" :turn "t2")
    (is (= 2 (length (telegram-called "sendMessage"))) "one message per turn")
    (stream-call "eval" :turn "t1")
    (is (= 2 (length (telegram-called "sendMessage"))) "a finished turn's replay opens nothing")))

;;; --- refusing, and answering --------------------------------------------------------

(deftest kanban-stream-cell-a-chat-is-required ()
  (with-telegram
    (signals-error nlk:config-refusal (kanban-stream-start "bot_token" "123:abc"))))

(deftest kanban-stream-cell-a-token-is-required ()
  (with-telegram
    (let ((nlk-var "KANBAN_STREAM_TEST_TOKEN"))
      (signals-error nlk:config-refusal
        (kanban-stream-start "chat_id" "42" "bot_token_env" nlk-var)))))

(deftest kanban-stream-cell-status-names-the-chat-and-test-posts ()
  (with-stream ("bot_token" "123:abc" "chat_id" "42")
    (is-carrying (answer (cell-entry "nodecode-kanban-stream" "kanban-stream" nil))
                 ("chat 42") ("nothing streaming right now"))
    (is-carrying (answer (cell-entry "nodecode-kanban-stream" "kanban-stream" "test"))
                 ("Sent message 7") ("chat 42"))
    (is (= 1 (length (telegram-called "sendMessage"))))
    (stream-call "eval")
    (is-carrying (answer (cell-entry "nodecode-kanban-stream" "kanban-stream" "status"))
                 ("s1") ("message 7"))))

(deftest kanban-stream-cell-the-tool-posts-a-line ()
  (with-stream ("bot_token" "123:abc" "chat_id" "42")
    (is-present (entry (nle::find-tool "kanban_stream")) "the tool is registered"
      (let ((answer (funcall (third entry)
                              (nle::tool-body-arguments entry (nlk:json-object "text" "the build is green")))))
        (is-carrying (said answer) ("Sent to chat 42 as message 7"))
        (is (equal "the build is green" (telegram-text (first *telegram-calls*)))))
      (let ((answer (funcall (third entry)
                              (nle::tool-body-arguments entry (nlk:json-object "text" "")))))
        (is-carrying (said answer) ("chat 42"))))))

(deftest kanban-stream-cell-a-deleted-message-is-posted-afresh ()
  (with-stream ("bot_token" "123:abc" "chat_id" "42" "throttle_seconds" 1)
    (stream-call "eval")
    (setf *telegram-answer*
          (lambda (name)
            (and (equal name "editMessageText")
                 (nlk:json-object "ok" nil
                                  "description" "Bad Request: message to edit not found"))))
    (sleep 1.1)
    (stream-fact "turn.usage" :payload (nlk:json-object "output-tokens" 50))
    (is (= 2 (length (telegram-called "sendMessage"))) "a message the chat no longer has is posted again, never edited twice")))
