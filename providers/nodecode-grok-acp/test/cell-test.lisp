;;;; cell-test.lisp --- the grok-acp cell against stubbed I/O.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test here starts the real grok CLI. A round's transport is a session
;;;; over two strings (OPEN-ACP-SESSION stubbed), so the JSON-RPC framing,
;;;; the streamed text, the CLI's own requests and its errors are all spoken
;;;; by a script; a whole round's fold stubs RUN-ACP-ROUND.

(in-package #:nodecode.test)

(define-test-slice "grok-acp" "GROK-ACP-CELL-" :start nodecode-grok-acp:start-cell)

(define-cell-lifecycle-tests "grok-acp"
  (:config "command" "grok")
  (:hooks :credential 'nle::configured-provider-inventory)
  (:running (is (nle::find-lane-by-name "grok-acp" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "grok-acp" nil)) "and taken back out"))
  (:refused () ("command" 5) ("models" 5)))

;;; --- the script a stand-in CLI writes -----------------------------------------

(defun acp-line (object)
  "OBJECT as one line, the way the CLI writes one."
  (format nil "~a~%" (nlk:encode-json-object object)))

(defun acp-reply (id result)
  "The reply a request numbered ID takes."
  (acp-line (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun acp-failure (id code text)
  "An error where ID's reply would be."
  (acp-line (nlk:json-object "jsonrpc" "2.0" "id" id
                              "error" (nlk:json-object "code" code "message" text))))

(defun acp-chunk (text)
  "One streamed piece of the answer."
  (acp-line (nlk:json-object "jsonrpc" "2.0" "method" "session/update"
                              "params" (nlk:json-object
                                        "sessionId" "s1"
                                        "update" (nlk:json-object
                                                  "sessionUpdate" "agent_message_chunk"
                                                  "content" (nlk:json-object
                                                             "type" "text" "text" text))))))

(defun acp-ask (id method params)
  "A request of the CLI's own, numbered ID."
  (acp-line (nlk:json-object "jsonrpc" "2.0" "id" id "method" method "params" params)))

(defun acp-handshake ()
  "The three replies a round's first three requests take."
  (concatenate 'string (acp-reply 1 (nlk:json-object "protocolVersion" 1))
               (acp-reply 2 (nlk:json-object "sessionId" "s1"))
               (acp-reply 3 (nlk:json-object))))

(defmacro with-scripted-cli ((session script) &body body)
  "BODY with the cell's child stubbed: a session over SCRIPT, the lines the
CLI writes, and SESSION that session, so a test reads what was sent."
  `(with-stubbed-fdefinition (nodecode-grok-acp::open-acp-session (argv directory)
                              (declare (ignore argv directory))
                              (setf ,session
                                    (nodecode-grok-acp::make-acp-session
                                     (make-string-output-stream)
                                     (make-string-input-stream ,script))))
     ,@body))

(defun sent-text (session)
  "What the cell wrote down SESSION's pipe."
  (get-output-stream-string (nodecode-grok-acp::acp-session-to-agent session)))

;;; --- the protocol --------------------------------------------------------------

(deftest grok-acp-cell-speaks-the-protocol-and-streams-the-answer ()
  (let ((session nil)
        (script (concatenate 'string (acp-handshake)
                             (acp-chunk "po") (acp-chunk "ng")
                             (acp-reply 4 (nlk:json-object "stopReason" "end_turn"))))
        (seen '()))
    (with-scripted-cli (session script)
      (is (equal "end_turn"
                 (nodecode-grok-acp::run-acp-round
                  "grok" #("agent" "--no-leader" "stdio") nil "hello"
                  :model "grok-4.7"
                  :on-text (lambda (text) (push text seen))))))
    (is (equal '("po" "ng") (nreverse seen)) "the answer streams as it comes")
    (let ((sent (sent-text session)))
      (is (search "\"initialize\"" sent) "the round opens the protocol")
      (is (search "\"session/new\"" sent) "and opens a session")
      (is (search "grok-4.7" sent) "the picked model reaches the session")
      (is (search "hello" sent) "the round's prompt is the session's prompt"))))

(deftest grok-acp-cell-answers-the-clis-own-requests ()
  (uiop:with-temporary-file (:pathname path :type "txt")
    (with-open-file (out path :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
      (write-string "notes" out))
    (let ((session nil)
          (script (concatenate
                   'string (acp-handshake)
                   (acp-ask "srv-1" "fs/read_text_file"
                            (nlk:json-object "sessionId" "s1" "path" (namestring path)))
                   (acp-ask "srv-2" "session/request_permission"
                            (nlk:json-object
                             "sessionId" "s1"
                             "toolCall" (nlk:json-object "toolCallId" "t1")
                             "options" (vector (nlk:json-object "optionId" "no" "name" "Reject"
                                                                 "kind" "reject_once")
                                               (nlk:json-object "optionId" "yes" "name" "Allow"
                                                                "kind" "allow_once"))))
                   (acp-reply 4 (nlk:json-object "stopReason" "end_turn")))))
      (with-scripted-cli (session script)
        (is (equal "end_turn"
                   (nodecode-grok-acp::run-acp-round "grok" #("agent") nil "hello"
                                                     :model "grok-4.7")) "the round ends where the CLI says it did"))
      (let ((sent (sent-text session)))
        (is (search "notes" sent) "the file the CLI asked for is read back to it")
        (is (search "selected" sent) "its permission ask is answered")
        (is (search "yes" sent) "with the first option that allows")))))

(deftest grok-acp-cell-says-when-the-cli-is-not-logged-in ()
  (let ((session nil)
        (script (concatenate 'string (acp-reply 1 (nlk:json-object "protocolVersion" 1))
                             (acp-failure 2 -32000 "Authentication required"))))
    (with-scripted-cli (session script)
      (let ((condition (signals-error nodecode-grok-acp::acp-error
                                      (nodecode-grok-acp::run-acp-round
                                       "grok" #("agent") nil "hello"))))
        (is (search "logged in" (princ-to-string condition)) "the refusal says what to do about it")))))

;;; --- the round -----------------------------------------------------------------

(deftest grok-acp-cell-folds-the-answer-and-its-tool-blocks ()
  (let ((parts '()) (asked nil))
    (with-cell-stop ((grok-acp-start "command" "grok"))
      (let ((nle::*provider* "grok-acp") (nle::*model* "grok-4.7")
            (nle::*api-key* "x") (nle::*endpoint* nil))
        (with-stubbed-fdefinition
            (nodecode-grok-acp::run-acp-round
             (command argv directory prompt &key model effort on-text on-thought)
             (declare (ignore command argv directory model effort on-thought))
             (setf asked prompt)
             (funcall on-text "Reading ")
             (funcall on-text "<tool_")
             (funcall on-text "call>{\"id\":\"c1\",\"type\":\"function\",\"function\":{\"name\":\"read\",\"arguments\":\"{}\"}}</tool_call>")
             (funcall on-text " done")
             "end_turn")
          (multiple-value-bind (message usage finish request)
              (nodecode-grok-acp:call-grok-streaming
               (user-context "hello") :on-part (lambda (part) (push part parts)))
            (declare (ignore usage request))
            (is (equal "Reading  done" (nlk:json-value message :string "content")) "no block, and no half marker, is the text")
            (is (equal "tool_calls" finish) "the round ends in its calls")
            (let ((call (find :tool-call parts :key #'nle::stream-part-kind)))
              (is call "the block becomes a tool call")
              (is (equal "read" (nle::stream-part-tool-name call))))
            (is (find :text-delta parts :key #'nle::stream-part-kind) "the answer streams as text deltas")
            (is (find :finish parts :key #'nle::stream-part-kind) "the stream is closed once")
            (is (search "<tool_call>" asked) "the prompt states the block contract")
            (is (search "hello" asked) "and carries the conversation")))))))

(deftest grok-acp-cell-lists-its-models ()
  (with-cell-stop ((grok-acp-start "command" "grok" "models" (vector "grok-4.7")))
    (multiple-value-bind (ids models)
        (nodecode-grok-acp::inventory
         (lambda (&rest arguments) (declare (ignore arguments)) (values '() '())))
      (is (member "grok-acp" ids :test #'equal) "the provider is listed")
      (is (equal "Grok (grok CLI)" (getf (first models) :provider-display)))
      (is (equal "grok-4.7" (getf (first models) :model-id)) "and the section's models are what /models offers"))))
