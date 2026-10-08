;;;; addon-test.lisp --- the claude-code add-on against a stand-in CLI.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; No test here starts the real claude CLI or reaches Anthropic. The wire
;;;; transforms are pure and tested on literal bodies; the relay is dialled by
;;;; a raw loopback client; a whole round runs against test/fake-claude.py,
;;;; which speaks the CLI's stream-json and POSTs to the relay as the CLI
;;;; does, with dex:post stubbed for the send (the core suite's seam,
;;;; WITH-STUBBED-FDEFINITION).

(in-package #:nodecode.test)

(define-test-slice "claude-code" "CLAUDE-CODE-ADDON-" :start nodecode-claude-code:start-addon)

(define-addon-lifecycle-tests "claude-code"
  (:hooks :credential 'nle::walk-provider-stream 'nle::configured-provider-inventory
          'nle::resolve-model-capability 'nle::model-price)
  (:running (is (nle::find-lane-by-name "claude-code" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "claude-code" nil)) "and taken back out"))
  (:refused ("timeout_seconds" 0)))

;;; --- fixtures -----------------------------------------------------------------

(defun cc-json (text)
  "TEXT decoded the way the add-on decodes a body."
  (nlk:decode-json text))

(defun cc-fake ()
  "The stand-in CLI's path."
  (uiop:native-namestring (asdf:system-relative-pathname "nodecode-claude-code" "test/fake-claude.py")))

(defun cc-python-p ()
  "Whether the stand-in CLI can run here."
  (nlk::executable-on-path "python3"))

(defparameter +cc-round+
  (concatenate
   'string
   "{\"model\":\"claude-haiku-4-5\",\"max_tokens\":4000,"
   "\"system\":[{\"type\":\"text\",\"text\":\"You are Nodecode.\",\"cache_control\":{\"type\":\"ephemeral\"}}],"
   "\"tools\":[{\"name\":\"read\",\"description\":\"Read a file\",\"input_schema\":{\"type\":\"object\"},"
   "\"cache_control\":{\"type\":\"ephemeral\"}}],"
   "\"messages\":["
   "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"read notes\"}]},"
   "{\"role\":\"assistant\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"t\",\"signature\":\"s\"},"
   "{\"type\":\"text\",\"text\":\"reading\"},"
   "{\"type\":\"tool_use\",\"id\":\"toolu_1\",\"name\":\"read\",\"input\":{\"path\":\"notes\"},"
   "\"cache_control\":{\"type\":\"ephemeral\"}}]},"
   "{\"role\":\"user\",\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_1\",\"content\":\"hello\","
   "\"cache_control\":{\"type\":\"ephemeral\"}}]}]}")
  "A Messages body the Anthropic lane builds for a round after one tool call.")

(defun cc-marked (body)
  "(MESSAGE-INDEX BLOCK-INDEX) of every block in BODY carrying a cache marker."
  (loop for message across (nlk:json-value body :array "messages")
        for index from 0
        nconc (loop for block across (or (nlk:json-value message :array "content") #())
                    for position from 0
                    when (gethash "cache_control" block) collect (list index position))))

(defun cc-sse (&rest frames)
  "A canned Anthropic stream: FRAMES as data lines."
  (apply #'make-truncated-sse-stream frames))

(defparameter +cc-tool-stream+
  (list "{\"type\":\"message_start\",\"message\":{\"id\":\"msg_1\",\"model\":\"claude-haiku-4-5\",\"usage\":{\"input_tokens\":10,\"output_tokens\":1}}}"
        "{\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"tool_use\",\"id\":\"toolu_2\",\"name\":\"mcp__nodecode__read\",\"input\":{}}}"
        "{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"path\\\":\\\"b\\\"}\"}}"
        "{\"type\":\"content_block_stop\",\"index\":0}"
        "{\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":7}}"
        "{\"type\":\"message_stop\"}")
  "One streamed call of the read tool, under its request name.")

(defmacro with-cc-round ((&key (mode "") record) &body body)
  "BODY with the add-on started on the stand-in CLI, MODE its behavior and
RECORD the file it writes what it saw to; the environment it inherits
carries an API key and a Claude Code entrypoint the round must not pass on."
  `(let ((saved (mapcar (lambda (name) (cons name (uiop:getenv name)))
                        '("FAKE_CLAUDE_MODE" "FAKE_CLAUDE_RECORD" "ANTHROPIC_API_KEY" "CLAUDE_CODE_ENTRYPOINT"))))
     (unwind-protect
          (progn
            (sb-posix:setenv "FAKE_CLAUDE_MODE" ,mode 1)
            (sb-posix:setenv "FAKE_CLAUDE_RECORD" (or ,record "") 1)
            (sb-posix:setenv "ANTHROPIC_API_KEY" "sk-must-not-reach-the-cli" 1)
            (sb-posix:setenv "CLAUDE_CODE_ENTRYPOINT" "claude-vscode" 1)
            ;; the stand-in is a python3 script: without one there is no round to run
            (when (cc-python-p)
              (with-addon-stop ((claude-code-start "command" (cc-fake) "timeout_seconds" 20))
                ,@body)))
       (loop for (name . value) in saved
             do (if value (sb-posix:setenv name value 1) (sb-posix:unsetenv name))))))

(defun cc-round (&optional (text "read notes"))
  "One claude-code round of a user TEXT, dex:post stubbed to stream a read
call: (values MESSAGE URL HEADERS CONTENT)."
  (let ((url nil) (headers nil) (content nil))
    (with-stubbed-fdefinition
        (dex:post (asked &rest args)
         (setf url asked headers (getf args :headers) content (getf args :content))
         (values (apply #'cc-sse +cc-tool-stream+) 200))
      (let ((nle::*provider* "claude-code")
            (nle::*model* "claude-haiku-4-5")
            (nle::*api-key* nil))
        (values (nle::call-anthropic-streaming (user-context text)) url headers content)))))

;;; --- the wire -----------------------------------------------------------------

(deftest claude-code-addon-replays-a-round-as-frames ()
  (let* ((frames (nodecode-claude-code::replay-frames (cc-json +cc-round+) "claude-haiku-4-5"))
         (assistant (second frames)))
    (is (= 3 (length frames)) "one frame a message")
    (is (equal '("user" "assistant" "user")
               (mapcar (lambda (frame) (nlk:json-value frame :string "type")) frames)))
    (is (nth-value 1 (gethash "shouldQuery" (first frames))) "an earlier user turn does not ask")
    (is (null (nth-value 1 (gethash "shouldQuery" (third frames)))) "the last one does")
    (is (equal "claude-haiku-4-5" (nlk:json-value assistant :string "message" "model")) "the assistant frame names the model, or the CLI drops its thinking")
    (is (equal "mcp__nodecode__read"
               (nlk:json-value (aref (nlk:json-value assistant :array "message" "content") 2) :string "name")) "a tool call under its request name")
    (is (equal "read" (nlk:json-value (aref (nlk:json-value (aref (nlk:json-value (cc-json +cc-round+) :array "messages") 1)
                                                            :array "content")
                                            2)
                                      :string "name")) "and the round's own body is untouched")
    (is (notany (lambda (frame)
                  (some (lambda (block) (gethash "cache_control" block))
                        (nlk:json-value frame :array "message" "content")))
                frames) "no marker is replayed")))

(deftest claude-code-addon-refuses-an-assistant-prefill ()
  (let ((body (cc-json "{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"},{\"role\":\"assistant\",\"content\":\"par\"}]}")))
    (is (signals-error nle::provider-config-error
          (nodecode-claude-code::replay-frames body "claude-haiku-4-5")))))

(deftest claude-code-addon-lays-its-own-members-over-the-cli ()
  (let ((extra (nodecode-claude-code::extra-body (cc-json +cc-round+))))
    (is (equal "mcp__nodecode__read" (nlk:json-value (aref (gethash "tools" extra) 0) :string "name")))
    (is (null (gethash "cache_control" (aref (gethash "tools" extra) 0))) "tools carry no marker")
    (is (= 4000 (gethash "max_tokens" extra)))
    (is (equal "disabled" (nlk:json-value extra :string "thinking" "type")) "no thinking asked is thinking off")
    (is (equalp #() (nlk:json-value extra :array "context_management" "edits")) "and the CLI's thinking edit goes with it"))
  (let* ((body (cc-json "{\"messages\":[],\"max_tokens\":9000,\"thinking\":{\"type\":\"adaptive\"},\"output_config\":{\"effort\":\"high\"}}"))
         (extra (nodecode-claude-code::extra-body body)))
    (is (equal "adaptive" (nlk:json-value extra :string "thinking" "type")))
    (is (null (gethash "context_management" extra)))
    (is (equal "high" (nodecode-claude-code::effort body)))))

(deftest claude-code-addon-routes-a-long-window-to-its-1m-name ()
  (is (equal "claude-sonnet-5[1m]" (nodecode-claude-code::model-argument "claude-sonnet-5" 1000000)))
  (is (equal "claude-haiku-4-5" (nodecode-claude-code::model-argument "claude-haiku-4-5" 200000)))
  (is (equal "claude-opus-5-5" (nodecode-claude-code::model-argument "claude-opus-5-5" nil)) "unknown stays plain"))

(deftest claude-code-addon-pins-the-marker-behind-a-changed-tool-result ()
  ;; The CLI appended its date to the last tool result and marked it: the
  ;; next round replays that result without the date, so the marker goes
  ;; back to the assistant message.
  (let* ((body (cc-json "{\"messages\":[{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"env\"},{\"type\":\"text\",\"text\":\"q\"}]},{\"role\":\"assistant\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"t\"},{\"type\":\"tool_use\",\"id\":\"a\",\"name\":\"x\",\"input\":{}}]},{\"role\":\"user\",\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"a\",\"content\":\"r + date\",\"cache_control\":{\"type\":\"ephemeral\"}}]}]}"))
         (queried (cc-json "[{\"type\":\"tool_result\",\"tool_use_id\":\"a\",\"content\":\"r\"}]")))
    (is (equal '((1 1)) (cc-marked (nodecode-claude-code::pin-breakpoint body queried))))))

(deftest claude-code-addon-pins-the-marker-before-a-trailing-system-turn ()
  ;; The CLI's context rode a trailing role:system message, marked; the
  ;; newest user turn is the queried frame unchanged, so it takes the marker.
  (let* ((body (cc-json "{\"messages\":[{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"q\"}]},{\"role\":\"system\",\"content\":\"env\"},{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"a\",\"name\":\"x\",\"input\":{}}]},{\"role\":\"user\",\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"a\",\"content\":\"r\"}]},{\"role\":\"system\",\"content\":[{\"type\":\"text\",\"text\":\"date\",\"cache_control\":{\"type\":\"ephemeral\"}}]}]}"))
         (queried (cc-json "[{\"type\":\"tool_result\",\"tool_use_id\":\"a\",\"content\":\"r\"}]")))
    (is (equal '((3 0)) (cc-marked (nodecode-claude-code::pin-breakpoint body queried))))))

(deftest claude-code-addon-leaves-a-request-with-two-markers ()
  (let ((body (cc-json "{\"messages\":[{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"q\",\"cache_control\":{}}]},{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"a\"}]},{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"b\",\"cache_control\":{}}]}]}")))
    (is (equal '((0 0) (2 0)) (cc-marked (nodecode-claude-code::pin-breakpoint body #()))))))

(deftest claude-code-addon-folds-a-call-back-to-its-tool ()
  (let ((seen nil)
        (frame (cc-json (second +cc-tool-stream+))))
    (funcall (nodecode-claude-code::unprefixed-fold (lambda (frame finish record)
                                                      (declare (ignore finish record))
                                                      (setf seen (nlk:json-value frame :string "content_block" "name"))))
             frame nil nil)
    (is (equal "read" seen))))

;;; --- the relay ------------------------------------------------------------------

(deftest claude-code-addon-relay-keeps-the-messages-post ()
  (let ((relay (nodecode-claude-code::open-relay)))
    (unwind-protect
         (let* ((body "{\"model\":\"m\"}")
                (connection (usocket:socket-connect "127.0.0.1" (nodecode-claude-code::relay-port relay)
                                                    :element-type '(unsigned-byte 8)))
                (stream (usocket:socket-stream connection)))
           (unwind-protect
                (progn
                  (write-sequence (sb-ext:string-to-octets
                                   (format nil "POST ~a/v1/messages?beta=true HTTP/1.1~c~cHost: x~c~cAuthorization: Bearer tok~c~cAccept-Encoding: br~c~cContent-Type: application/json~c~cContent-Length: ~d~c~c~c~c~a"
                                           (nodecode-claude-code::relay-prefix relay)
                                           #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed
                                           #\Return #\Linefeed #\Return #\Linefeed (length body)
                                           #\Return #\Linefeed #\Return #\Linefeed body)
                                   :external-format :latin-1)
                                  stream)
                  (finish-output stream)
                  (is (await (:timeout 5) (nodecode-claude-code::relay-capture relay)) "captured")
                  (let ((capture (nodecode-claude-code::relay-capture relay)))
                    (is (equal body (sb-ext:octets-to-string (getf capture :body))))
                    (is (equal "https://api.anthropic.com/v1/messages?beta=true"
                               (nodecode-claude-code::captured-endpoint capture)))
                    (is (equal '(("Authorization" . "Bearer tok"))
                               (nodecode-claude-code::captured-headers capture)) "the hop's headers stay behind")))
             (usocket:socket-close connection)))
      (nodecode-claude-code::close-relay relay))))

;;; --- a whole round against the stand-in CLI -------------------------------------

(deftest claude-code-addon-sends-the-request-the-cli-wrote ()
  (with-temp-file (record :contents "")
    (with-cc-round (:record (uiop:native-namestring record))
      (nlk:bind (((message url headers content) (cc-round)))
        (is (equal "https://api.anthropic.com/v1/messages?beta=true" url) "the path the CLI asked for")
        (is (equal "Bearer fake-oauth-token" (nlk:header-value headers "authorization")) "the CLI's login")
        (is (equal "cli" (nlk:header-value headers "x-app")))
        (is (equal "claude-cli/0.0.0-fake (external, sdk-cli)" (nlk:header-value headers "user-agent")) "the CLI's own client name, not Nodecode's")
        (is (null (nlk:header-value headers "x-api-key")) "no key of Nodecode's rides along")
        (is (null (nlk:header-value headers "accept-encoding")))
        (is (= 1 (count "content-type" headers :key #'car :test #'string-equal)))
        (let ((sent (nlk:decode-json content)))
          (is (every (lambda (tool) (uiop:string-prefix-p "mcp__nodecode__" (nlk:json-value tool :string "name")))
                     (nlk:json-value sent :array "tools")) "the request names every tool as the CLI does")
          (is (search "cc_entrypoint" (nlk:json-value (aref (nlk:json-value sent :array "system") 0) :string "text")) "the CLI's system blocks"))
        (is (equal "read" (nlk:json-value (aref (nlk:json-value message :array "tool_calls") 0)
                                          :string "function" "name")) "the call comes back under Nodecode's name")
        (let ((seen (nlk:decode-json (uiop:read-file-string record))))
          (is (search "/admit/" (nlk:json-value seen :string "env" "ANTHROPIC_BASE_URL")) "the relay is its API")
          (is (null (nlk:json-value seen :string "env" "ANTHROPIC_API_KEY")) "the inherited key is withheld")
          (is (null (nlk:json-value seen :string "env" "CLAUDE_CODE_ENTRYPOINT")) "and so is a session's marker")
          (is (equal "1" (nlk:json-value seen :string "env" "CLAUDE_CODE_DISABLE_AUTO_MEMORY")))
          (is (find "--strict-mcp-config" (nlk:json-value seen :array "argv") :test #'equal))
          (is (equal "claude-haiku-4-5" (let ((argv (coerce (nlk:json-value seen :array "argv") 'list)))
                                          (second (member "--model" argv :test #'equal))))))))))

(deftest claude-code-addon-says-when-the-cli-is-logged-out ()
  (with-cc-round (:mode "logged-out")
    (let ((refusal (signals-error nle::provider-config-error (cc-round))))
      (when (typep refusal 'nle::provider-error)
        (is (eql 403 (nle::provider-error-status refusal)) "decisive: never retried")
        (is (search "/login" (nle::provider-error-detail refusal)))))))

(deftest claude-code-addon-says-when-there-is-no-cli ()
  (with-addon-stop ((claude-code-start "command" "/nonexistent/claude"))
    (let ((refusal (signals-error nle::provider-config-error (cc-round))))
      (when (typep refusal 'nle::provider-error)
        (is (eql 404 (nle::provider-error-status refusal)))
        (is (search "not installed" (nle::provider-error-detail refusal)))))))

;;; --- what the rest of the organism reads ----------------------------------------

(deftest claude-code-addon-is-a-provider-with-models ()
  (with-temp-auth (auth "{}")
    (with-addon-stop ((claude-code-start "command" "/nonexistent/claude" "models" (vector "claude-haiku-4-5")))
      (nlk:bind (((ids models) (nle::configured-provider-inventory :auth-path auth)))
        (is (member "claude-code" ids :test #'equal) "listed")
        (is (find "claude-haiku-4-5" (remove "claude-code" models :key (lambda (row) (getf row :provider-id))
                                                                  :test-not #'equal)
                  :key (lambda (row) (getf row :model-id)) :test #'equal) "with its models"))
      (is (eq :oauth (nle::provider-auth-state "claude-code" :auth-path auth)) "credentialed by the CLI")
      (is (null (nle::model-price "claude-haiku-4-5" "claude-code")) "and never priced"))
    (nlk:bind (((ids) (nle::configured-provider-inventory :auth-path auth)))
      (is (not (member "claude-code" ids :test #'equal)) "stopped, it is gone"))))
