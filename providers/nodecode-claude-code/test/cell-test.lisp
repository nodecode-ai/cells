;;;; cell-test.lisp --- the claude-code cell against a stand-in CLI.
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

(define-test-slice "claude-code" "CLAUDE-CODE-CELL-" :start nodecode-claude-code:start-cell)

(define-cell-lifecycle-tests "claude-code"
  (:hooks :credential 'nle::walk-provider-stream 'nle::configured-provider-inventory
          'nle::listable-provider-ids 'nle::list-provider-models
          'nle::resolve-model-capability 'nle::model-price)
  (:running (is (nle::find-lane-by-name "claude-code" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "claude-code" nil)) "and taken back out"))
  (:refused ("timeout_seconds" 0)))

;;; --- fixtures -----------------------------------------------------------------

(defun cc-json (text)
  "TEXT decoded the way the cell decodes a body."
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

(defun cc-header (headers name)
  "The value of header NAME in the alist HEADERS, any case."
  (cdr (assoc name headers :test #'string-equal)))

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

(defmacro with-cc-round ((&key (mode "") record (reuse 600)) &body body)
  "BODY with the cell started on the stand-in CLI, MODE its behavior and
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
              (nodecode-claude-code::forget-authored)
              (with-cell-stop ((claude-code-start "command" (cc-fake) "timeout_seconds" 20
                                                  "reuse_seconds" ,reuse))
                ,@body)))
       (nodecode-claude-code::forget-authored)
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

(deftest claude-code-cell-replays-a-round-as-frames ()
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

(deftest claude-code-cell-refuses-an-assistant-prefill ()
  (let ((body (cc-json "{\"messages\":[{\"role\":\"user\",\"content\":\"hi\"},{\"role\":\"assistant\",\"content\":\"par\"}]}")))
    (is (signals-error nle::provider-config-error
          (nodecode-claude-code::replay-frames body "claude-haiku-4-5")))))

(deftest claude-code-cell-lays-its-own-members-over-the-cli ()
  (let ((extra (nodecode-claude-code::extra-body (cc-json +cc-round+))))
    (is (equal "mcp__nodecode__read" (nlk:json-value (aref (gethash "tools" extra) 0) :string "name")))
    (is (null (gethash "cache_control" (aref (gethash "tools" extra) 0))) "tools carry no marker")
    (is (= 4000 (gethash "max_tokens" extra)))
    (is (equal "disabled" (nlk:json-value extra :string "thinking" "type")) "no thinking asked is thinking off")
    (is (equalp #() (nlk:json-value extra :array "context_management" "edits")) "and the CLI's thinking edit goes with it"))
  ;; Sonnet 5.5 turns thinking off as between_tools, and the API refuses the
  ;; CLI's clear-thinking edit there too: `clear_thinking_20251015 strategy
  ;; requires thinking to be enabled or adaptive'.
  (let ((extra (nodecode-claude-code::extra-body
                (cc-json "{\"messages\":[],\"max_tokens\":9000,\"thinking\":{\"type\":\"between_tools\"}}"))))
    (is (equal "between_tools" (nlk:json-value extra :string "thinking" "type")))
    (is (equalp #() (nlk:json-value extra :array "context_management" "edits")) "between_tools is thinking off too"))
  (let* ((body (cc-json "{\"messages\":[],\"max_tokens\":9000,\"thinking\":{\"type\":\"adaptive\"},\"output_config\":{\"effort\":\"high\"}}"))
         (extra (nodecode-claude-code::extra-body body)))
    (is (equal "adaptive" (nlk:json-value extra :string "thinking" "type")))
    (is (null (gethash "context_management" extra)))
    (is (equal "high" (nodecode-claude-code::effort body)))))

(deftest claude-code-cell-routes-a-long-window-to-its-1m-name ()
  (is (equal "claude-sonnet-5[1m]" (nodecode-claude-code::model-argument "claude-sonnet-5" 1000000)))
  (is (equal "claude-haiku-4-5" (nodecode-claude-code::model-argument "claude-haiku-4-5" 200000)))
  (is (equal "claude-opus-5-5" (nodecode-claude-code::model-argument "claude-opus-5-5" nil)) "unknown stays plain"))

(deftest claude-code-cell-sends-the-rounds-own-messages ()
  ;; The CLI's per-request context rides its newest turn and the next round
  ;; replays that turn without it: a thinking block signed behind it would
  ;; fail the API's prefix check, so the request carries the round's own
  ;; messages, thinking, markers and all, under the request's tool names.
  (let* ((body (cc-json +cc-round+))
         (sent (nodecode-claude-code::request-messages body))
         (assistant (nlk:json-value (aref sent 1) :array "content")))
    (is (= 3 (length sent)) "no message of the CLI's")
    (is (equal '("thinking" "text" "tool_use") (map 'list #'nodecode-claude-code::block-type assistant)))
    (is (equal "s" (nlk:json-value (aref assistant 0) :string "signature")) "the thinking as it was signed")
    (is (equal "mcp__nodecode__read" (nlk:json-value (aref assistant 2) :string "name")))
    (is (equal '((1 2) (2 0)) (cc-marked (nlk:json-object "messages" sent))) "Nodecode's markers")
    (is (equal "read" (nlk:json-value (aref (nlk:json-value (aref (nlk:json-value body :array "messages") 1)
                                                            :array "content")
                                            2)
                                      :string "name")) "and the round's own body is untouched")))

(deftest claude-code-cell-folds-a-call-back-to-its-tool ()
  (let ((seen nil)
        (frame (cc-json (second +cc-tool-stream+))))
    (funcall (nodecode-claude-code::unprefixed-fold (lambda (frame finish record)
                                                      (declare (ignore finish record))
                                                      (setf seen (nlk:json-value frame :string "content_block" "name"))))
             frame nil nil)
    (is (equal "read" seen))))

;;; --- the relay ------------------------------------------------------------------

(deftest claude-code-cell-relay-keeps-the-messages-post ()
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

(deftest claude-code-cell-sends-the-request-the-cli-wrote ()
  (with-temp-file (record :contents "")
    (with-cc-round (:record (uiop:native-namestring record))
      (nlk:bind (((message url headers content) (cc-round)))
        (is (equal "https://api.anthropic.com/v1/messages?beta=true" url) "the path the CLI asked for")
        (is (equal "Bearer fake-oauth-token" (cc-header headers "authorization")) "the CLI's login")
        (is (equal "cli" (cc-header headers "x-app")))
        (is (equal "claude-cli/0.0.0-fake (external, sdk-cli)" (cc-header headers "user-agent")) "the CLI's own client name, not Nodecode's")
        (is (null (cc-header headers "x-api-key")) "no key of Nodecode's rides along")
        (is (null (cc-header headers "accept-encoding")))
        (is (= 1 (count "content-type" headers :key #'car :test #'string-equal)))
        (let ((sent (nlk:decode-json content)))
          (is (every (lambda (tool) (uiop:string-prefix-p "mcp__nodecode__" (nlk:json-value tool :string "name")))
                     (nlk:json-value sent :array "tools")) "the request names every tool as the CLI does")
          (is (search "cc_entrypoint" (nlk:json-value (aref (nlk:json-value sent :array "system") 0) :string "text")) "the CLI's system blocks")
          (is (notany (lambda (message) (equal "system" (nlk:json-value message :string "role")))
                      (nlk:json-value sent :array "messages")) "and none of its per-request messages"))
        (is (equal "read" (nlk:json-value (aref (nlk:json-value message :array "tool_calls") 0)
                                          :string "function" "name")) "the call comes back under Nodecode's name")
        (let ((seen (nlk:decode-json (uiop:read-file-string record))))
          (is (search "/admit/" (nlk:json-value seen :string "env" "ANTHROPIC_BASE_URL")) "the relay is its API")
          (is (null (nlk:json-value seen :string "env" "ANTHROPIC_API_KEY")) "the inherited key is withheld")
          (is (null (nlk:json-value seen :string "env" "CLAUDE_CODE_ENTRYPOINT")) "and so is a session's marker")
          (is (equal "1" (nlk:json-value seen :string "env" "CLAUDE_CODE_DISABLE_AUTO_MEMORY")))
          (is (find "--strict-mcp-config" (nlk:json-value seen :array "argv") :test #'equal))
          (is (find "--tools=" (nlk:json-value seen :array "argv") :test #'equal) "no tools of the CLI's own")
          (is (notany (lambda (argument) (equal "" argument)) (nlk:json-value seen :array "argv")) "no argument is empty: nothing rests on how a Windows command line passes one")
          (is (equal "claude-haiku-4-5" (let ((argv (coerce (nlk:json-value seen :array "argv") 'list)))
                                          (second (member "--model" argv :test #'equal))))))))))

;;; --- what the CLI wrote, sent again --------------------------------------------

(defun cc-later (&optional (body (cc-json +cc-round+)))
  "BODY one tool call further on: another call and its result after its
messages, the first user message as it was."
  (nlk:copy-json-object
   body "messages"
   (concatenate 'vector (nlk:json-value body :array "messages")
                (vector (cc-json "{\"role\":\"assistant\",\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_2\",\"name\":\"read\",\"input\":{\"path\":\"b\"}}]}")
                        (cc-json "{\"role\":\"user\",\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"toolu_2\",\"content\":\"bee\"}]}")))))

(defun cc-author (record body &rest keys)
  "Author BODY's request the way a round does, RECORD emptied first:
(values HEADERS ENDPOINT SENT KEY RAN), SENT decoded, RAN whether the CLI
started."
  (with-open-file (out record :direction :output :if-exists :supersede) (declare (ignore out)))
  (multiple-value-bind (headers endpoint octets key)
      (apply #'nodecode-claude-code::author-request body
             (append keys (list :model "claude-haiku-4-5" :window 200000
                                :directory (uiop:temporary-directory) :session "s-cc")))
    (values headers endpoint
            (nlk:decode-json (sb-ext:octets-to-string octets :external-format :utf-8))
            key
            (plusp (length (uiop:read-file-string record))))))

(defun cc-text (value)
  "VALUE as JSON text, to compare."
  (sb-ext:octets-to-string (nlk:encode-json-octets value) :external-format :utf-8))

(deftest claude-code-cell-sends-again-what-the-cli-wrote ()
  (with-temp-file (record :contents "")
    (with-cc-round (:record (uiop:native-namestring record))
      (nlk:bind (((headers endpoint sent key ran) (cc-author record (cc-json +cc-round+))))
        (is ran "the first round's request is the CLI's")
        (is (null key))
        (let ((later (cc-later)))
          (nlk:bind (((headers2 endpoint2 sent2 key2 ran2) (cc-author record later)))
            (is (not ran2) "a round whose inputs did not move starts no CLI")
            (is key2 "and says it sent a kept request")
            (is (equal headers headers2) "the CLI's headers, its login among them")
            (is (equal endpoint endpoint2))
            (dolist (member '("system" "tools" "metadata" "model" "max_tokens"))
              (is (equal (cc-text (gethash member sent)) (cc-text (gethash member sent2))) "what the CLI wrote around the messages is sent again"))
            (is (equal (cc-text (nodecode-claude-code::request-messages later))
                       (cc-text (gethash "messages" sent2))) "around this round's own messages")
            (is (equal (loop for k being the hash-keys of sent collect k)
                       (loop for k being the hash-keys of sent2 collect k)) "member for member in the CLI's order")))))))

(deftest claude-code-cell-writes-afresh-when-an-input-moves ()
  (with-temp-file (record :contents "")
    (with-cc-round (:record (uiop:native-namestring record))
      (let ((body (cc-json +cc-round+)))
        (cc-author record body)
        (flet ((ran (body &rest keys)
                 (nth-value 4 (apply #'cc-author record body keys))))
          (is (ran (nlk:copy-json-object body "system" "You are Nodecode, changed.")) "another system text")
          (is (ran (nlk:copy-json-object body "output_config" (cc-json "{\"effort\":\"low\"}"))) "another effort")
          (is (ran (nlk:copy-json-object body "max_tokens" 8000)) "another output ceiling")
          (let ((messages (copy-seq (nlk:json-value body :array "messages"))))
            (setf (aref messages 0) (cc-json "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"read other notes\"}]}"))
            (is (ran (nlk:copy-json-object body "messages" messages)) "another first message: the billing fingerprint is taken from it"))
          (is (ran (nlk:copy-json-object
                    body "messages"
                    (concatenate 'vector (nlk:json-value body :array "messages")
                                 (vector (cc-json "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"look\"}]}")
                                         (cc-json "{\"role\":\"user\",\"content\":[{\"type\":\"image\",\"source\":{\"type\":\"base64\",\"media_type\":\"image/png\",\"data\":\"AA==\"}}]}"))))) "a block of a type the history did not carry")
          (is (ran body :session "s-other") "another session"))))))

(deftest claude-code-cell-starts-the-cli-every-round-when-reuse-is-off ()
  (with-temp-file (record :contents "")
    (with-cc-round (:record (uiop:native-namestring record) :reuse 0)
      (cc-author record (cc-json +cc-round+))
      (nlk:bind (((_ _ _ key ran) (cc-author record (cc-later))))
        (is ran "reuse_seconds 0: the CLI writes every round")
        (is (null key))))))

(deftest claude-code-cell-writes-afresh-after-a-login-refusal ()
  (with-temp-file (record :contents "")
    (with-cc-round (:record (uiop:native-namestring record))
      (cc-round)
      (with-open-file (out record :direction :output :if-exists :supersede) (declare (ignore out)))
      (let ((posts 0))
        (with-stubbed-fdefinition
            (dex:post (asked &rest args)
             (declare (ignore asked args))
             (if (= 1 (incf posts))
                 (error 'dex:http-request-failed :status 401 :body "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"OAuth token has expired\"}}"
                                                 :headers (make-hash-table :test #'equal) :uri "https://api.anthropic.com/v1/messages" :method :post)
                 (values (apply #'cc-sse +cc-tool-stream+) 200)))
          (let ((message (let ((nle::*provider* "claude-code") (nle::*model* "claude-haiku-4-5") (nle::*api-key* nil))
                           (nle::call-anthropic-streaming (user-context "read notes")))))
            (is (= 2 posts) "the refused request, then one the CLI wrote afresh")
            (is (plusp (length (uiop:read-file-string record))) "the CLI ran for the second")
            (is (equal "read" (nlk:json-value (aref (nlk:json-value message :array "tool_calls") 0) :string "function" "name")) "and the round's answer came back")))))))

(deftest claude-code-cell-forgets-a-request-that-failed ()
  (with-temp-file (record :contents "")
    (with-cc-round (:record (uiop:native-namestring record))
      (cc-round)
      (is (= 1 (hash-table-count nodecode-claude-code::*authored*)) "the round's request is kept")
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (declare (ignore asked args))
           (error 'dex:http-request-failed :status 400 :body "{\"type\":\"error\",\"error\":{\"type\":\"invalid_request_error\",\"message\":\"bad\"}}"
                                           :headers (make-hash-table :test #'equal) :uri "https://api.anthropic.com/v1/messages" :method :post))
        (is (handler-case (let ((nle::*provider* "claude-code") (nle::*model* "claude-haiku-4-5") (nle::*api-key* nil))
                            (nle::call-anthropic-streaming (user-context "read notes"))
                            nil)
              (nle::provider-error () t)) "the failure stands: only a login refusal is sent again"))
      (is (zerop (hash-table-count nodecode-claude-code::*authored*)) "and the next round asks the CLI again"))))

(deftest claude-code-cell-relay-keeps-a-header-value-whole ()
  (nlk:bind (((method target headers)
              (nodecode-claude-code::parse-head
               (format nil "POST /v1/messages HTTP/1.1~c~cX-One: true~c~cX-Two: ~ctight~c ~c~c~c~c"
                       #\Return #\Linefeed #\Return #\Linefeed #\Tab #\Tab #\Return #\Linefeed #\Return #\Linefeed))))
    (is (equal "POST" method))
    (is (equal "/v1/messages" target))
    (is (equal "true" (cc-header headers "x-one")) "a value that starts or ends in t keeps it")
    (is (equal "tight" (cc-header headers "x-two")) "only spaces and tabs are trimmed")))

(deftest claude-code-cell-looks-for-a-windows-cli-by-its-type ()
  (is (equal '("claude.exe" "claude.cmd" "claude.bat")
             (nodecode-claude-code::program-names "claude" :windows t)) "the native installer's .exe first, then npm's .cmd, never the sh script beside it")
  (is (equal '("C:\\tools\\claude.CMD")
             (nodecode-claude-code::program-names "C:\\tools\\claude.CMD" :windows t)) "a command that names its type is taken as it is")
  (is (equal '("claude") (nodecode-claude-code::program-names "claude" :windows nil)) "elsewhere the name is the name"))

(deftest claude-code-cell-says-when-the-cli-is-logged-out ()
  (with-cc-round (:mode "logged-out")
    (let ((refusal (signals-error nle::provider-config-error (cc-round))))
      (when (typep refusal 'nle::provider-error)
        (is (eql 403 (nle::provider-error-status refusal)) "decisive: never retried")
        (is (search "/login" (nle::provider-error-detail refusal)))))))

(deftest claude-code-cell-says-when-there-is-no-cli ()
  (with-cell-stop ((claude-code-start "command" "/nonexistent/claude"))
    (let* ((nodecode-claude-code::*settle-seconds* 1/2)
           (refusal (signals-error nle::provider-config-error (cc-round))))
      (when (typep refusal 'nle::provider-error)
        (is (eql 404 (nle::provider-error-status refusal)))
        (is (search "not installed" (nle::provider-error-detail refusal)))))))

(deftest claude-code-cell-waits-out-a-reinstall ()
  ;; A reinstall leaves the CLI's path missing, then present but not yet
  ;; executable, for seconds: the round waits for it rather than failing.
  (uiop:with-temporary-file (:pathname script :keep nil)
    (let ((path (uiop:native-namestring script))
          (nodecode-claude-code::*settle-seconds* 5))
      (with-open-file (out path :direction :output :if-exists :supersede)
        (write-line "#!/bin/sh" out))
      (sb-posix:chmod path #o644)
      (let* ((started (get-internal-real-time))
             (installer (sb-thread:make-thread (lambda () (sleep 1/2) (sb-posix:chmod path #o755))))
             (found (nodecode-claude-code::settled-program path)))
        (sb-thread:join-thread installer)
        (is (equal (truename path) found) "the CLI, once it may run")
        (is (<= 2/5 (/ (- (get-internal-real-time) started) internal-time-units-per-second) 4) "found after the reinstall, not before and not at the deadline"))
      (sb-posix:chmod path #o644)
      (let ((nodecode-claude-code::*settle-seconds* 1/2))
        (is (equal (truename path) (nodecode-claude-code::settled-program path)) "a CLI that never becomes executable is still named, for the start to say why")))))

(deftest claude-code-cell-retries-a-start-that-fails-in-the-window ()
  (let ((fake (cc-fake))
        (tries 0))
    (flet ((start (program)
             (declare (ignore program))
             (if (= 1 (incf tries))
                 (error "spawning claude: Permission denied")
                 (values :started tries))))
      (let ((nodecode-claude-code::*settle-seconds* 5))
        (is (equal '(:started 2) (multiple-value-list (nodecode-claude-code::start-cli fake #'start))) "a start that fails inside the window is tried again"))
      (setf tries 0)
      (let ((nodecode-claude-code::*settle-seconds* 0))
        (is (signals-error error (nodecode-claude-code::start-cli fake #'start)) "past the window the failure stands")))))

;;; --- what the rest of the organism reads ----------------------------------------

(deftest claude-code-cell-is-a-provider-with-models ()
  (with-temp-auth (auth "{}")
    (with-cell-stop ((claude-code-start "command" "/nonexistent/claude" "models" (vector "claude-haiku-4-5")))
      (nlk:bind (((ids models) (nle::configured-provider-inventory :auth-path auth)))
        (is (member "claude-code" ids :test #'equal) "listed")
        (is (find "claude-haiku-4-5" (remove "claude-code" models :key (lambda (row) (getf row :provider-id))
                                                                  :test-not #'equal)
                  :key (lambda (row) (getf row :model-id)) :test #'equal) "with its models"))
      (is (eq :oauth (nle::provider-auth-state "claude-code" :auth-path auth)) "credentialed by the CLI")
      ;; Two resellers outvote Anthropic's own row, so only the hook's
      ;; mapping to anthropic answers 1/5 rather than their 9/9.
      (with-catalog-fixture (catalog "{\"anthropic\":{\"models\":{\"claude-haiku-4-5\":{\"cost\":{\"input\":1,\"output\":5,\"cache_read\":0.1,\"cache_write\":1.25}}}},
                                      \"aaa\":{\"models\":{\"claude-haiku-4-5\":{\"cost\":{\"input\":9,\"output\":9}}}},
                                      \"bbb\":{\"models\":{\"claude-haiku-4-5\":{\"cost\":{\"input\":9,\"output\":9}}}}}")
        (is (equal '(1 5 0.1d0 1.25d0) (nle::model-price "claude-haiku-4-5" "claude-code")) "priced at Anthropic's API list price")))
    (nlk:bind (((ids) (nle::configured-provider-inventory :auth-path auth)))
      (is (not (member "claude-code" ids :test #'equal)) "stopped, it is gone"))))

(defparameter +cc-listing+
  "{\"data\":[{\"type\":\"model\",\"id\":\"claude-fable-5-2\",\"display_name\":\"Claude Fable 5.2\",\"max_input_tokens\":1000000,\"max_tokens\":128000},
            {\"type\":\"model\",\"id\":\"claude-haiku-4-5-20251001\",\"display_name\":\"Claude Haiku 4.5\",\"max_input_tokens\":200000}],
    \"has_more\":false}"
  "Anthropic's /v1/models as a login may be answered it.")

(deftest claude-code-cell-lists-what-its-login-may-call ()
  (with-cc-round ()
    (let ((asked nil) (sent nil))
      (with-stubbed-fdefinition
          (nle::http-fetch (url &rest args)
           (setf asked url sent (getf args :headers))
           (values +cc-listing+ 200))
        (nlk:bind (((rows error) (nle::list-provider-models "claude-code")))
          (is (null error) "the listing answered")
          (is (equal "https://api.anthropic.com/v1/models?limit=1000" asked) "Anthropic's own listing")
          (is (equal "Bearer fake-oauth-token" (cc-header sent "authorization")) "asked with the CLI's own login")
          (is (eql 1000000 (getf (find "claude-fable-5-2" rows :key (lambda (row) (getf row :id)) :test #'equal) :context-window)) "a model the day it ships, with its window")
          (is (equal '("claude-fable-5-2" "claude-haiku-4-5-20251001") (mapcar (lambda (row) (getf row :id)) rows)) "what the login may call, nothing else"))))
    (is (member "claude-code" (nle::listable-provider-ids) :test #'equal) "/models fetches it")))

(deftest claude-code-cell-lists-its-own-models-beside-the-login ()
  (with-cell-stop ((claude-code-start "command" "/nonexistent/claude" "models" (vector "claude-haiku-4-5" "claude-fable-5-2")))
    (let ((ids (mapcar (lambda (row) (getf row :id))
                       (nodecode-claude-code::listing-rows +cc-listing+ (nle::parse-provider-listing +cc-listing+)))))
      (is (equal '("claude-fable-5-2" "claude-haiku-4-5-20251001" "claude-haiku-4-5") ids) "an alias the listing leaves out joins it, once"))
    (nlk:bind (((rows error) (nle::list-provider-models "claude-code")))
      (is (null rows) "no CLI, no listing")
      (is (search "no /nonexistent/claude on PATH" error) "and its row says why"))))
