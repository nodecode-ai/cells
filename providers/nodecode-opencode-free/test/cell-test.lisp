;;;; cell-test.lisp --- the opencode-free cell against a stand-in ACP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test here starts the real OpenCode CLI or reaches a model: a round
;;;; runs against test/fake-acp.sh under /bin/sh, and the prompt and parsing
;;;; halves are tested on literal text. Nothing here needs the network, a key
;;;; or the vendor's binary.

(in-package #:nodecode.test)

(define-test-slice "opencode-free" "OPENCODE-FREE-CELL-" :start nodecode-opencode-free:start-cell)

(define-cell-lifecycle-tests "opencode-free"
  (:hooks :credential 'nle::configured-provider-inventory)
  (:running (is (nle::find-lane-by-name "opencode-free" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "opencode-free" nil)) "and taken back out"))
  (:refused ("timeout_seconds" 0)))

;;; --- fixtures -------------------------------------------------------------

(defun of-fake ()
  "The stand-in ACP server's path."
  (uiop:native-namestring
   (asdf:system-relative-pathname "nodecode-opencode-free" "test/fake-acp.sh")))

(defun of-tools ()
  "The round's tool schemas, as a turn hands them to a lane."
  (nlk:decode-json
   "[{\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"description\":\"Read a file\",\"parameters\":{\"type\":\"object\"}}}]"))

(defparameter +of-answer+
  (concatenate 'string
               "Let me read it."
               "<hermes_tool_call>{\"id\":\"call_1\",\"type\":\"function\","
               "\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"notes.txt\\\"}\"}}"
               "</hermes_tool_call>")
  "What a round's model says when it asks for one Nodecode tool.")

(defmacro with-of-cli (&body body)
  "BODY with the cell started on the stand-in ACP server."
  `(with-cell-stop ((opencode-free-start "command" "/bin/sh"
                                         "args" (vector (of-fake))
                                         "timeout_seconds" 30))
     ,@body))

;;; --- the prompt and the parsing -------------------------------------------

(deftest opencode-free-cell-renders-the-round-as-one-prompt ()
  (let* ((messages (nlk:decode-json
                    (concatenate 'string
                     "[{\"role\":\"user\",\"content\":\"read notes\"},"
                     "{\"role\":\"assistant\",\"content\":\"reading\",\"tool_calls\":[{\"id\":\"call_0\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{}\"}}]},"
                     "{\"role\":\"tool\",\"tool_call_id\":\"call_0\",\"content\":\"hello\"}]")))
         (prompt (nodecode-opencode-free::render-prompt "You are Nodecode." messages (of-tools) "auto")))
    (is (search "You are Nodecode." prompt) "the system voice rides the prompt")
    (is (search "read_file" prompt) "and the round's tool schemas")
    (is (search "Tool result for call_0" prompt) "a tool result is labelled by its call")
    (is (search "REMOTE TOOL PROTOCOL" prompt) "the remote tool contract opens and closes it")
    (is (search "hello" prompt) "the result itself is in the transcript")))

(deftest opencode-free-cell-parses-a-remote-tool-call ()
  (multiple-value-bind (calls prose)
      (nodecode-opencode-free::parse-tool-calls +of-answer+ (of-tools) "auto")
    (is (= 1 (length calls)) "one call")
    (is (equal "read_file" (nlk:json-value (aref calls 0) :string "function" "name")) "named as the round advertised it")
    (is (equal "{\"path\":\"notes.txt\"}"
               (nlk:json-value (aref calls 0) :string "function" "arguments")) "its arguments as the chat wire carries them")
    (is (equal "Let me read it." prose) "and the prose around it is the answer")))

(deftest opencode-free-cell-refuses-an-unadvertised-call ()
  (let ((answer (concatenate 'string
                             "<hermes_tool_call>{\"id\":\"call_9\",\"type\":\"function\","
                             "\"function\":{\"name\":\"shell\",\"arguments\":\"{}\"}}"
                             "</hermes_tool_call>")))
    (is (signals-error nodecode-opencode-free::opencode-free-error
          (nodecode-opencode-free::parse-tool-calls answer (of-tools) "auto")) "a tool the round did not advertise dispatches nothing")))

(deftest opencode-free-cell-refuses-an-uncurated-model ()
  (is (signals-error nodecode-opencode-free::opencode-free-error
        (nodecode-opencode-free::exchange "gpt-5-paid" "hello")) "a model outside the curated list never reaches the CLI"))

;;; --- a whole round --------------------------------------------------------

(deftest opencode-free-cell-runs-a-round-against-the-stand-in-cli ()
  (with-of-cli
    (multiple-value-bind (text reasoning)
        (nodecode-opencode-free::exchange "glm-5.3-flash-free"
                                          (nodecode-opencode-free::render-prompt
                                           "You are Nodecode." (nlk:decode-json
                                                                  "[{\"role\":\"user\",\"content\":\"read notes\"}]")
                                           
                                           (of-tools) "auto"))
      (is (search "Let me read it." text) "the CLI's words come back")
      (is (search "<hermes_tool_call>" text) "with the call the model asked for")
      (is (stringp reasoning) "and the reasoning stream is drained")
      (multiple-value-bind (calls prose)
          (nodecode-opencode-free::parse-tool-calls text (of-tools) "auto")
        (is (= 1 (length calls)) "which parses into one Nodecode tool call")
        (is (equal "read_file" (nlk:json-value (aref calls 0) :string "function" "name")))
        (is (equal "Let me read it." prose) "the answer beside it is the prose")))))
