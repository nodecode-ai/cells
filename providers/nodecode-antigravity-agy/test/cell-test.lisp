;;;; cell-test.lisp --- the antigravity-agy cell against a stand-in CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test here starts the real agy or reaches Google. A round is dialled
;;;; on the relay's loopback port by a raw client, and the CLI behind it is
;;;; test/fake-agy.sh, a stand-in that prints the stream-json agy prints.

(in-package #:nodecode.test)

(define-test-slice "antigravity-agy" "ANTIGRAVITY-AGY-CELL-" :start nodecode-antigravity-agy:start-cell)

(define-cell-lifecycle-tests "antigravity-agy"
  (:hooks 'nle::models-catalog-table :credential)
  (:running (is (search "127.0.0.1" (nodecode-antigravity-agy::relay-base)) "a round has a loopback address"))
  (:stopped (is (null nodecode-antigravity-agy::*relay*) "and the listener is closed"))
  (:refused ("timeout_seconds" 0)))

;;; --- fixtures -----------------------------------------------------------------

(defun agy-fake ()
  "The stand-in CLI, as a command line the section can carry."
  (format nil "sh ~a" (uiop:native-namestring
                        (asdf:system-relative-pathname "nodecode-antigravity-agy" "test/fake-agy.sh"))))

(defmacro with-fake-agy ((&key (answer "text") (command '(agy-fake))) &body body)
  "BODY with the cell started on the stand-in CLI and FAKE_AGY_ANSWER the
answer it gives."
  (let ((saved (gensym)))
    `(let ((,saved (uiop:getenv "FAKE_AGY_ANSWER")))
       (unwind-protect
            (progn
              (sb-posix:setenv "FAKE_AGY_ANSWER" ,answer 1)
              (with-cell-stop ((antigravity-agy-start "command" ,command "timeout_seconds" 30))
                ,@body))
         (if ,saved (sb-posix:setenv "FAKE_AGY_ANSWER" ,saved 1) (sb-posix:unsetenv "FAKE_AGY_ANSWER"))))))

(defparameter +agy-round+
  (concatenate 'string
               "{\"model\":\"gemini-3.8-flash-low\",\"stream\":true,"
               "\"messages\":[{\"role\":\"system\",\"content\":\"You are Nodecode.\"},"
               "{\"role\":\"user\",\"content\":\"say hello\"}],"
               "\"tools\":[{\"type\":\"function\",\"function\":{\"name\":\"read\","
               "\"description\":\"Read a file\",\"parameters\":{\"type\":\"object\","
               "\"properties\":{\"path\":{\"type\":\"string\"}}}}}]}")
  "A chat body the lane builds for a round that offers one tool.")

(defun agy-exchange (method target &optional body)
  "One exchange with the relay: the whole response as text."
  (let* ((relay nodecode-antigravity-agy::*relay*)
         (socket (usocket:socket-connect "127.0.0.1" (nodecode-antigravity-agy::relay-port relay)
                                         :element-type '(unsigned-byte 8)))
         (crlf (format nil "~c~c" #\Return #\Linefeed))
         (octets (and body (sb-ext:string-to-octets body :external-format :utf-8))))
    (unwind-protect
         (with-open-stream (stream (usocket:socket-stream socket))
           (write-sequence
            (sb-ext:string-to-octets
             (format nil "~a ~a HTTP/1.1~aHost: 127.0.0.1~aContent-Type: application/json~aContent-Length: ~d~aConnection: close~a~a"
                     method target crlf crlf crlf (if octets (length octets) 0) crlf crlf crlf)
             :external-format :utf-8)
            stream)
           (when octets (write-sequence octets stream))
           (finish-output stream)
           (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)
                                             :adjustable t :fill-pointer 0)))
             (loop for byte = (read-byte stream nil nil)
                   do (unless byte
                        (return (sb-ext:octets-to-string buffer :external-format :utf-8)))
                      (vector-push-extend byte buffer))))
      (ignore-errors (usocket:socket-close socket)))))

;;; --- the round -----------------------------------------------------------------

(deftest antigravity-agy-cell-round-answers-with-the-cli-text ()
  (with-fake-agy ()
    (let ((response (agy-exchange "POST" "/chat/completions" +agy-round+)))
      (is (search "HTTP/1.1 200" response) "the relay answers 200")
      (is (search "Hello from agy" response) "the CLI's answer is streamed back")
      (is (search "data: [DONE]" response) "and the stream is closed the chat lane's way"))))

(deftest antigravity-agy-cell-round-hands-a-tool-call-back ()
  (with-fake-agy (:answer "tool")
    (let ((response (agy-exchange "POST" "/chat/completions" +agy-round+)))
      (is (search "tool_calls" response) "a <call> line becomes the round's tool_calls")
      (is (search "\\\"path\\\":\\\"notes\\\"" response) "with the arguments the model wrote")
      (is (search "finish_reason\":\"tool_calls" response) "and the round finishes on the call"))))

(deftest antigravity-agy-cell-lists-its-models ()
  (with-fake-agy ()
    (let ((response (agy-exchange "GET" "/models")))
      (is (search "gemini-3.8-flash-low" response) "the listing names the slug the section offers")
      (is (search "\"object\":\"list\"" response) "in the listing shape the core reads"))))

(deftest antigravity-agy-cell-refuses-without-a-cli ()
  (with-fake-agy (:command "no-such-agy-cli-xyz")
    (let ((response (agy-exchange "POST" "/chat/completions" +agy-round+)))
      (is (search "502" response) "a round with no CLI is a bad gateway")
      (is (search "no-such-agy-cli-xyz" response) "and says which command is missing"))))

(deftest antigravity-agy-cell-refuses-an-agent-fallback ()
  (with-fake-agy (:answer "fallback")
    (let ((response (agy-exchange "POST" "/chat/completions" +agy-round+)))
      (is (search "502" response) "a fallback to agy's own agent is refused, not run")
      (is (search "refusing the round" response) "and the answer says why"))))

;;; --- the wire ------------------------------------------------------------------

(deftest antigravity-agy-cell-names-the-slug-the-cli-knows ()
  (is (equal "gemini-3.8-flash-medium" (nodecode-antigravity-agy::native-model "flash")) "an alias resolves")
  (is (equal "gemini-3.1-pro-high" (nodecode-antigravity-agy::native-model "pro")) "and so does pro")
  (is (equal "gpt-oss-120b-medium" (nodecode-antigravity-agy::native-model "gpt-oss-120b-medium")) "a slug passes through")
  (is (= 1000000 (nodecode-antigravity-agy::context-window "gemini-3.8-flash-low")) "a Gemini window")
  (is (= 200000 (nodecode-antigravity-agy::context-window "claude-sonnet-4-6")) "a Claude window"))

(deftest antigravity-agy-cell-splits-a-call-out-of-the-answer ()
  (multiple-value-bind (content calls)
      (nodecode-antigravity-agy::split-calls "Let me look.<call name=\"read\">{\"path\":\"a\"}</call>")
    (is (equal "Let me look." content) "the prose before the call is the content")
    (is (equal '("read") (mapcar #'car calls)) "and the call is named"))
  (multiple-value-bind (content calls)
      (nodecode-antigravity-agy::split-calls "plain answer")
    (is (equal "plain answer" content) "an answer with no call is all content")
    (is (null calls) "and carries no calls")))

(deftest antigravity-agy-cell-prompts-the-tool-protocol ()
  (let* ((request (nlk:decode-json +agy-round+))
         (messages (nlk:json-value request :array "messages"))
         (with-tools (nodecode-antigravity-agy::system-prompt messages
                                                                (nlk:json-value request :array "tools")))
         (without (nodecode-antigravity-agy::system-prompt messages nil)))
    (is (search "You are Nodecode." with-tools) "the round's system text is kept")
    (is (search "<call name=" with-tools) "a round with tools is told the call line")
    (is (search "### read" with-tools) "and given the tool's schema")
    (is (search "No tools are available" without) "a round without tools is told so")
    (is (search "say hello" (nodecode-antigravity-agy::conversation-text messages)) "the history is flattened")))



