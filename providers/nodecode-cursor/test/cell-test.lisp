;;;; cell-test.lisp --- the cursor cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every exchange with Cursor a stubbed
;;;; dex:post or dex:get, every run a fake service: a RunSSE body this file
;;;; feeds as the cell's appends arrive. The expected protobuf bytes are
;;;; written out here from agent.proto's field numbers (a tag byte is
;;;; (NUMBER << 3) | WIRE-TYPE: #x0a field 1, #x12 field 2, #x1a field 3, ...),
;;;; never produced by the cell's own encoder. Nothing touches the network,
;;;; the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "cursor" "CURSOR-CELL-" :start nodecode-cursor:start-cell)

(define-cell-lifecycle-tests "cursor"
  (:hooks 'nle::models-catalog-table :credential 'nle::list-provider-models)
  (:command "cursor")
  (:running (is (nle::find-lane-by-name "cursor" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "cursor" nil)) "and taken back out"))
  (:refused ("base_url" 5)))

;;; --- bytes, written out -----------------------------------------------------------

(defun cu-bytes (&rest parts)
  "PARTS as one octet vector: an integer is an octet, a string its UTF-8, a
vector its octets, a list its parts."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (labels ((add (part)
               (etypecase part
                 (null)
                 (integer (vector-push-extend part out))
                 (string (loop for octet across (sb-ext:string-to-octets part :external-format :utf-8)
                               do (vector-push-extend octet out)))
                 (cons (mapc #'add part))
                 (vector (loop for octet across part do (vector-push-extend octet out))))))
      (mapc #'add parts))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun cu-len (tag &rest parts)
  "A length-delimited field: the TAG octet(s), the length, PARTS."
  (let* ((body (apply #'cu-bytes parts))
         (length (length body)))
    (cu-bytes tag
              (loop for value = length then (ash value -7)
                    collect (if (< value #x80) value (logior #x80 (logand value #x7f)))
                    while (>= value #x80))
              body)))

(defun cu-frame (payload &optional (flags 0))
  "One Connect frame: the flag octet, four octets of big-endian length, PAYLOAD."
  (let ((length (length payload)))
    (cu-bytes flags (ldb (byte 8 24) length) (ldb (byte 8 16) length) (ldb (byte 8 8) length)
              (ldb (byte 8 0) length) payload)))

(defun cu-octets (text)
  (coerce (sb-ext:string-to-octets text :external-format :utf-8) '(simple-array (unsigned-byte 8) (*))))

(defun cu-digest (text)
  "The raw SHA-256 of TEXT's UTF-8, independent of the cell."
  (let ((hex (subseq (nlk::sha256-text text) 7)))
    (cu-bytes (loop for i below 32 collect (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun cu-b64url (octets)
  (string-right-trim "=" (substitute #\_ #\/ (substitute #\- #\+ (cl-base64:usb8-array-to-base64-string octets)))))

(defun cu-jwt (&key (sub "auth0|user_01") (exp 2000000000))
  "An unsigned JWT naming SUB, expiring at EXP."
  (format nil "eyJhbGciOiJIUzI1NiJ9.~a.c2ln"
          (cu-b64url (cu-octets (format nil "{\"sub\":\"~a\",\"exp\":~d}" sub exp)))))

(defun cu-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun cu-said (fragment)
  "Whether a notice said lately carries FRAGMENT."
  (some (lambda (entry) (search fragment (first entry))) (nlk:notice-log :limit 50)))

(defun cu-signed-in (&key (access (cu-jwt)) (refresh "rt-1") (expires 4000000000) email)
  "An auth.json text holding a Cursor sign-in."
  (shasht:write-json
   (nlk:json-object "oauth_tokens"
                    (nlk:json-object "cursor" (nlk:json-object "access_token" access "refresh_token" refresh
                                                               "expires_at" expires :opt "email" email)))
   nil))

(defparameter +cu-uuid+ "00000000-0000-4000-8000-0000000000aa")

;;; --- the catalog and the credential ------------------------------------------------

(deftest cursor-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((cursor-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "cursor"))
           (models (nlk:json-value row :object "models")))
      (is (equal "Cursor (Claude, GPT, etc.)" (nlk:json-value row :string "name")))
      (is (equal "https://api2.cursor.sh" (nlk:json-value row :string "api")))
      (is (equal "nodecode-cursor" (nlk:json-value row :string "npm")))
      (is (gethash "claude-opus-5-5" models) "the bundled models are listed")
      (is (equal "cursor" (nle::configured-provider-lane "cursor")) "the cell's own lane drives it")
      (is (equal "https://api2.cursor.sh" (nle::lane-endpoint "cursor" "cursor"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "cursor"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest cursor-cell-answers-the-kept-sign-in ()
  (with-cell-stop ((cursor-start))
    (with-temp-auth (auth (cu-signed-in :access "tok-signed" :email "me@example.com"))
      (let ((credential (nle::resolve-provider-credential "cursor" :auth-path auth :probe t)))
        (is (equal "tok-signed" (nle:credential-key credential)))
        (is (eq :oauth (nle:credential-source credential)))
        (is (equal auth (getf (nle:credential-attributes credential) :auth-path)))))))

(deftest cursor-cell-reads-the-token-variables ()
  (with-cell-stop ((cursor-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "CURSOR_ACCESS_TOKEN") "tok-env"))
        (let ((credential (nle::resolve-provider-credential "cursor" :auth-path auth :probe t)))
          (is (equal "tok-env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "CURSOR_API_KEY") "tok-key"))
        (is (equal "tok-key" (nle:credential-key (nle::resolve-provider-credential "cursor" :auth-path auth :probe t)))
            "CURSOR_API_KEY, the discovery's variable, answers too")
        (is (not (equal "tok-key" (nle:credential-key (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t))))
            "another provider's ladder never reads it")))))

(deftest cursor-cell-never-sends-another-familys-key ()
  (with-cell-stop ((cursor-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (member name '("OPENAI_API_KEY" "ANTHROPIC_API_KEY" "GOOGLE_API_KEY") :test #'equal)
                                      "sk-other"))
        (let ((credential (nle::resolve-provider-credential "cursor" :auth-path auth :probe t)))
          (is (not (equal "sk-other" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest cursor-cell-saved-key-outranks-the-sign-in ()
  (with-cell-stop ((cursor-start))
    (with-temp-auth (auth "{\"api_keys\":{\"cursor\":{\"provider\":\"cursor\",\"key\":\"tok-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "CURSOR_ACCESS_TOKEN") "tok-env"))
        (is (equal "tok-saved" (nle:credential-key (nle::resolve-provider-credential "cursor" :auth-path auth :probe t))))))))

;;; --- protobuf, by hand ------------------------------------------------------------------

(deftest cursor-cell-writes-varints-and-values-as-protobuf-does ()
  (is (equalp (cu-bytes #xac #x02) (nodecode-cursor::varint 300)))
  (is (equalp (cu-bytes #xff #xff #xff #xff #xff #xff #xff #xff #xff #x01) (nodecode-cursor::varint -1))
      "a negative int32 is its ten-octet two's complement")
  ;; google.protobuf.Value: struct_value is field 5, its entries field 1 of
  ;; {key 1, value 2}; string_value 3, bool_value 4, null_value 1,
  ;; number_value 2 (a little-endian double), list_value 6 of values at 1
  (let ((object (nlk:json-object "a" "x" "b" t)))
    (is (equalp (cu-len #x2a (cu-len #x0a (cu-len #x0a "a") (cu-len #x12 (cu-len #x1a "x")))
                        (cu-len #x0a (cu-len #x0a "b") (cu-len #x12 #x20 #x01)))
                (nodecode-cursor::json-value-octets object))))
  (is (equalp (cu-bytes #x11 #x00 #x00 #x00 #x00 #x00 #x00 #xf8 #x3f)
              (nodecode-cursor::json-value-octets 1.5d0))
      "1.5 is the double 0x3ff8000000000000")
  (let ((value (nodecode-cursor::octets-json-value
                (cu-len #x2a (cu-len #x0a (cu-len #x0a "n")
                                     (cu-len #x12 (cu-len #x32 (cu-len #x0a #x11 0 0 0 0 0 0 #xf0 #x3f)
                                                          (cu-len #x0a #x08 #x00)
                                                          (cu-len #x0a #x20 #x01))))))))
    (is (equalp (vector 1 :null t) (gethash "n" value))
        "a list of the number 1, null and true, the integral double read as an integer")))

(deftest cursor-cell-reads-protobuf-fields-in-wire-order ()
  (let ((fields (nodecode-cursor::pb-decode (cu-bytes #x08 #x96 #x01 (cu-len #x12 "hi") #x08 #x05))))
    (is (equal '(1 2 1) (mapcar #'first fields)))
    (is (= 5 (nodecode-cursor::pb-get fields 1)) "the last of a repeated scalar wins")
    (is (equal "hi" (nodecode-cursor::pb-text fields 2))))
  (is (signals-error nodecode-cursor::malformed-proto
        (nodecode-cursor::pb-decode (cu-bytes #x12 #x05 "hi")))
      "a field cut short is refused"))

(deftest cursor-cell-frames-connect-messages ()
  (is (equalp (cu-bytes 0 0 0 0 3 1 2 3) (nodecode-cursor::connect-frame (cu-bytes 1 2 3))))
  (multiple-value-bind (frames rest)
      (nodecode-cursor::connect-frames (cu-bytes (cu-frame (cu-bytes 9)) (cu-frame (cu-octets "{}") 2) 0 0 0))
    (is (equal '(0 2) (mapcar #'car frames)))
    (is (equalp (cu-octets "{}") (cdr (second frames))))
    (is (equalp (cu-bytes 0 0 0) rest) "a frame cut short waits for the rest"))
  (is (equalp (cu-bytes 7 7)
              (nodecode-cursor::connect-unary-body (cu-bytes (cu-frame (cu-octets "{}") 2) (cu-frame (cu-bytes 7 7)))))
      "a unary answer's message, its trailer stepped over")
  (is (null (nodecode-cursor::connect-unary-body (cu-frame (cu-bytes 7) 1))) "a compressed frame is not read"))

;;; --- naming the model on the wire --------------------------------------------------------

(deftest cursor-cell-routes-a-model-the-way-omp-does ()
  (flet ((resolved (model wire &optional (mode :normalized))
           (multiple-value-list (nodecode-cursor::resolve-wire-model model wire mode))))
    (is (equal "gpt-5.4-high" (nodecode-cursor::wire-model-id "gpt-5.4" "high")) "effort routing")
    (is (equal "claude-opus-5-5-low" (nodecode-cursor::wire-model-id "claude-opus-5-5" nil))
        "thinking off rides the row's request id")
    (is (equal '("gpt-5.4" "gpt-5.4-high" (("reasoning" . "high")) nil) (resolved "gpt-5.4" "gpt-5.4-high"))
        "an OpenAI effort sibling goes as its base id and a reasoning parameter")
    (is (equal '("gpt-5.5" "gpt-5.5-none" () nil) (resolved "gpt-5.5" "gpt-5.5-none")) "-none is off")
    (is (equal '("gpt-5.2-fast" "gpt-5.2-high-fast" (("reasoning" . "high")) nil)
               (resolved "gpt-5.2-high-fast" "gpt-5.2-high-fast"))
        "the -fast lane stays on the base id")
    (is (equal '("claude-opus-5-5-xhigh" "claude-opus-5-5-xhigh" () nil) (resolved "claude-opus-5-5" "claude-opus-5-5-xhigh"))
        "another class keeps its sibling slug; its own max-mode marker says no")
    (is (equal '("composer-2.5" "composer-2.5" (("fast" . "false")) nil) (resolved "composer-2.5" "composer-2.5"))
        "a bare composer-2.5 pins the standard tier")
    (is (equal '("gpt-5.4-high" "gpt-5.4-high" () nil) (resolved "gpt-5.4" "gpt-5.4-high" :discovered))
        "discovered mode sends the id as it is")
    (is (eq t (fourth (resolved "claude-opus-4-7-high-fast" "claude-opus-4-7-thinking-high-fast")))
        "a fast Opus member is max mode by its own marker")
    (is (nodecode-cursor::max-mode-wire-id-p "claude-opus-4-7-max"))
    (is (null (nodecode-cursor::round-effort "composer-2.5" "high")) "a model that does not think asks for no effort")
    (is (signals-error nle::provider-config-error (nodecode-cursor::round-effort "gpt-5.4" "max"))
        "an effort the model does not take is refused")))

;;; --- the run request, byte for byte ------------------------------------------------------

(deftest cursor-cell-writes-the-run-request-from-agent-proto ()
  (with-cell-stop ((cursor-start))
    (let ((nle::*provider* "cursor") (nle::*model* "gpt-5.4") (nle::*api-key* "tok") (nle::*endpoint* nil))
      (let* ((context (user-context "hi"))
             (system (nle::compiled-turn-context-system-prompt context))
             (text (nle::message-content (car (last (coerce (nle::request-messages context) 'list)))))
             (system-id (cu-digest (format nil "{\"role\":\"system\",\"content\":~a}"
                                           (nlk:encode-json-object
                                            (if (plusp (length (nlk:trimmed system))) system "You are a helpful assistant."))))))
        (with-stubbed-fdefinition (nodecode-cursor::uuid () +cu-uuid+)
          (let ((request (nodecode-cursor::build-request context "gpt-5.4" "gpt-5.4-high" :normalized "conv-1"
                                                         (nodecode-cursor::make-conversation))))
            (is (equalp
                 ;; AgentClientMessage.run_request = 1
                 (cu-len #x0a
                         ;; conversation_state = 1: root_prompt_messages_json = 1, the system blob
                         (cu-len #x0a (cu-len #x0a system-id))
                         ;; action = 2: user_message_action = 1 { user_message = 1 { text 1, message_id 2 } }
                         (cu-len #x12 (cu-len #x0a (cu-len #x0a (cu-len #x0a text) (cu-len #x12 +cu-uuid+))))
                         ;; model_details = 3: model_id 1, display_model_id 3, display_name 4
                         (cu-len #x1a (cu-len #x0a "gpt-5.4-high") (cu-len #x1a "gpt-5.4") (cu-len #x22 "GPT-5.4"))
                         ;; requested_model = 9: model_id 1, parameters 3 { id 1, value 2 }
                         (cu-len #x4a (cu-len #x0a "gpt-5.4") (cu-len #x1a (cu-len #x0a "reasoning") (cu-len #x12 "high")))
                         ;; conversation_id = 5
                         (cu-len #x2a "conv-1"))
                 (nodecode-cursor::request-octets request)))
            (is (equal "gpt-5.4-high" (nodecode-cursor::request-fallback request))
                "the discovery id a not-found may retry with")))))))

(deftest cursor-cell-replays-a-tool-round-as-history ()
  (with-cell-stop ((cursor-start))
    (let ((nle::*provider* "cursor") (nle::*model* "gpt-5.4") (nle::*api-key* "tok") (nle::*endpoint* nil))
      (let* ((call (nle::chat-tool-call-object "tc|1" "eval" "{\"form\":\"(+ 1 2)\"}"))
             (context (compiled-context
                       (list (nle::message "user" "add")
                             (nlk:json-object "role" "assistant" "content" "Adding." "tool_calls" (vector call))
                             (nle::message "tool" "3" :tool-call-id "tc|1"))))
             (conversation (nodecode-cursor::make-conversation))
             (request (nodecode-cursor::build-request context "gpt-5.4" "gpt-5.4-high" :normalized "conv-2" conversation))
             (blobs (nodecode-cursor::conv-blobs conversation))
             (run (nodecode-cursor::pb-sub (nodecode-cursor::pb-decode (nodecode-cursor::request-octets request)) 1))
             (state (nodecode-cursor::pb-sub run 1)))
        (flet ((blob (id) (gethash (nodecode-cursor::hex id) blobs))
               (json (id) (nlk:decode-json (sb-ext:octets-to-string (gethash (nodecode-cursor::hex id) blobs)
                                                                    :external-format :utf-8))))
          (is (equalp (cu-len #x12) (nodecode-cursor::pb-get run 2))
              "a history ending on a tool result resumes: action = { resume_action 2 {} }")
          (let ((root (nodecode-cursor::pb-all state 1)))
            (is (= 4 (length root)) "system, user, the assistant round, its result")
            (is (equal "user" (nlk:json-value (json (second root)) :string "role")))
            (let ((assistant (json (third root))))
              (is (equal "Adding." (nlk:json-value (aref (nlk:json-value assistant :array "content") 0) :string "text")))
              (let ((part (aref (nlk:json-value assistant :array "content") 1)))
                (is (equal "tool-call" (nlk:json-value part :string "type")))
                (is (equal "tc_1" (nlk:json-value part :string "toolCallId")) "the id within Cursor's charset")
                (is (equal "(+ 1 2)" (nlk:json-value part :string "args" "form")))))
            (let ((result (json (fourth root))))
              (is (equal "tool" (nlk:json-value result :string "role")))
              (is (equal "3" (nlk:json-value (aref (nlk:json-value result :array "content") 0) :string "result")))
              (is (equal "eval" (nlk:json-value (aref (nlk:json-value result :array "content") 0) :string "toolName")))))
          (let* ((turns (nodecode-cursor::pb-all state 8))
                 (turn (nodecode-cursor::pb-sub (nodecode-cursor::pb-decode (blob (first turns))) 1))
                 (steps (nodecode-cursor::pb-all turn 2)))
            (is (= 1 (length turns)) "one agent turn")
            (is (search "add" (nodecode-cursor::pb-text (nodecode-cursor::pb-decode (blob (nodecode-cursor::pb-get turn 1))) 1)))
            (is (= 2 (length steps)) "its text and its call")
            ;; the call: ConversationStep.tool_call = 2 { tool_call_id 57, mcp_tool_call 15 { args 1, result 2 } }
            (let* ((step (nodecode-cursor::pb-sub (nodecode-cursor::pb-decode (blob (second steps))) 2))
                   (mcp (nodecode-cursor::pb-sub step 15)))
              (is (equal "tc_1" (nodecode-cursor::pb-text step 57)))
              (is (equal "eval" (nodecode-cursor::pb-text (nodecode-cursor::pb-sub mcp 1) 1)))
              (is (equalp (cu-len #x0a (cu-len #x0a (cu-len #x0a (cu-len #x0a "3"))))
                          (nodecode-cursor::pb-get mcp 2))
                  "its result: success 1 { content 1 { text 1 { text 1 } } }"))))))))

;;; --- the history a Kimi K3 round replays --------------------------------------------------

(deftest cursor-cell-replays-k3-thinking-only-to-its-own-model ()
  (with-cell-stop ((cursor-start))
    (let ((nle::*provider* "cursor") (nle::*model* "kimi-k3-high") (nle::*api-key* "tok") (nle::*endpoint* nil))
      (flet ((history (writer)
               ;; the round as the store keeps it: a fresh object, the cell's
               ;; knowledge of its writer kept apart from it
               (let ((round (nlk:json-object "role" "assistant" "content" "hello" "reasoning_content" "greet back")))
                 (when writer
                   (nodecode-cursor::note-round-model
                    (nlk:json-object "role" "assistant" "content" "hello" "reasoning_content" "greet back")
                    writer))
                 (compiled-context (list (nle::message "user" "hi") round (nle::message "user" "again")))))
             (assistant-part (context)
               (let* ((conversation (nodecode-cursor::make-conversation))
                      (request (nodecode-cursor::build-request context "kimi-k3-high" "kimi-k3-high"
                                                               :normalized "conv-3" conversation))
                      (state (nodecode-cursor::pb-sub (nodecode-cursor::pb-sub (nodecode-cursor::pb-decode (nodecode-cursor::request-octets request)) 1) 1))
                      (assistant (nlk:decode-json
                                  (sb-ext:octets-to-string
                                   (gethash (nodecode-cursor::hex (third (nodecode-cursor::pb-all state 1)))
                                            (nodecode-cursor::conv-blobs conversation))
                                   :external-format :utf-8))))
                 (aref (nlk:json-value assistant :array "content") 0))))
        (let ((part (assistant-part (history "kimi-k3-high"))))
          (is (equal "reasoning" (nlk:json-value part :string "type")) "the model's own thinking replays first")
          (is (equal "greet back" (nlk:json-value part :string "text")))
          (is (equal "kimi-k3-high" (nlk:json-value part :string "providerOptions" "cursor" "modelName"))))
        (clrhash nodecode-cursor::*round-models*)
        (is (equal "text" (nlk:json-value (assistant-part (history nil)) :string "type"))
            "a round whose writer is unknown replays no thinking, and is let through")
        (is (signals-error nle::provider-config-error
              (nodecode-cursor::build-request (history "claude-opus-5-5") "kimi-k3-high" "kimi-k3-high" :normalized "conv-4"
                                              (nodecode-cursor::make-conversation)))
            "K3 refuses to continue history another Cursor model wrote")))))

(deftest cursor-cell-projects-tool-schemas-for-the-models-that-need-it ()
  (let ((projected (nodecode-cursor::project-schema
                    (nlk:decode-json "{\"type\":\"object\",\"properties\":{\"a\":{\"anyOf\":[{\"type\":\"string\"},{\"type\":\"number\"}]}},\"oneOf\":[{\"properties\":{\"x\":{\"type\":\"string\"}},\"required\":[\"x\"]},{\"properties\":{\"y\":{\"type\":\"number\"}},\"required\":[\"x\",\"y\"]}],\"not\":{\"anyOf\":[{\"required\":[\"z\"]}]}}"))))
    (is (not (nodecode-cursor::combiner-p projected)) "no composition keyword is left")
    (is (null (nth-value 1 (gethash "not" projected))) "a negation of one is dropped")
    (is (equal '("a" "x" "y") (sort (alexandria:hash-table-keys (nlk:json-value projected :object "properties")) #'string<))
        "object branches merge their properties")
    (is (equalp #("x") (nlk:json-value projected :array "required")) "what every branch requires stays required")
    (is (null (nth-value 1 (gethash "anyOf" (nlk:json-value projected :object "properties" "a"))))
        "a scalar union widens to accept-all"))
  (is (nodecode-cursor::tool-schema-projection-p "claude-fable-5-high"))
  (is (not (nodecode-cursor::tool-schema-projection-p "gpt-5.4"))))

(deftest cursor-cell-tells-a-rewritten-snapshot-from-a-cut-one ()
  (is (nodecode-cursor::rewritten-snapshot-p "{\"a\":1}{\"a\":2}"))
  (is (not (nodecode-cursor::rewritten-snapshot-p "{\"a\":\"(+ 1")))
  (is (not (nodecode-cursor::rewritten-snapshot-p "{\"a\":1}"))))

;;; --- the answers to the service's asks ---------------------------------------------------

(defun cu-exec (&rest parts)
  (nodecode-cursor::pb-decode (apply #'cu-bytes parts)))

(deftest cursor-cell-refuses-the-native-tools-in-their-own-results ()
  ;; ExecServerMessage { id 1 = 7, read_args 7 { path 1, tool_call_id 2 } }
  (is (equalp (list (cu-len #x12 #x08 #x07 (cu-len #x3a (cu-len #x1a (cu-len #x0a "/etc/x") (cu-len #x12 "Tool not available")))))
              (nodecode-cursor::exec-answer (cu-exec #x08 #x07 (cu-len #x3a (cu-len #x0a "/etc/x") (cu-len #x12 "t9")))))
      "execClientMessage 2 { id 1, read_result 7 { rejected 3 { path, reason } } }")
  (is (equalp (list (cu-len #x12 #x08 #x03 (cu-len #x12 (cu-len #x22 (cu-len #x0a "ls") (cu-len #x12 "/w") (cu-len #x1a "Tool not available")))))
              (nodecode-cursor::exec-answer (cu-exec #x08 #x03 (cu-len #x12 (cu-len #x0a "ls") (cu-len #x12 "/w")))))
      "shell_result 2 { rejected 4 { command, working_directory, reason } }")
  (is (equalp (list (cu-len #x2a (cu-len #x12 #x08 #x09 (cu-len #x12 "Unknown exec message variant")
                                         (cu-len #x22 "unknown_exec_variant")))
                    (cu-len #x2a (cu-len #x0a #x08 #x09)))
              (nodecode-cursor::exec-answer (cu-exec #x08 #x09)))
      "an ask this build cannot name: exec_client_control_message 5 { throw 2 }, then { stream_close 1 }")
  ;; pi_read_args 45 is answered as pi_read_result 46
  (is (equalp (list (cu-len #x12 #x08 #x04 (cu-len '(#xf2 #x02) (cu-len #x12 (cu-len #x0a "Tool not available")))))
              (nodecode-cursor::exec-answer (cu-exec #x08 #x04 (cu-len '(#xea #x02) (cu-len #x0a "/x"))))))
  (is (equalp (list (cu-len #x12 #x08 #x05 (cu-len #x2a (cu-len #x12 (cu-len #x0a "grep pattern is required (received an empty pattern).")))))
              (nodecode-cursor::exec-answer (cu-exec #x08 #x05 (cu-len #x2a (cu-len #x12 "src")))))
      "a grep with no pattern is refused in words the model can act on"))

(deftest cursor-cell-hands-an-mcp-call-to-the-turn-loop ()
  (let ((handed nil))
    ;; { id 2, exec_id 15 = "e1", mcp_args 11 { name 1, args 2 { form: "(+ 1 2)" }, tool_call_id 3 } }
    (is (equalp (list (cu-len #x12 #x08 #x02 (cu-len #x7a "e1")
                              ;; mcp_result 11 { success 1 { content 1 { text 1 { text 1 } } } }
                              (cu-len #x5a (cu-len #x0a (cu-len #x0a (cu-len #x0a (cu-len #x0a nodecode-cursor::+handoff+)))))))
                (nodecode-cursor::exec-answer
                 (cu-exec #x08 #x02 (cu-len #x7a "e1")
                          (cu-len #x5a (cu-len #x0a "eval")
                                  (cu-len #x12 (cu-len #x0a "form") (cu-len #x12 (cu-len #x1a "(+ 1 2)")))
                                  (cu-len #x1a "tc1")))
                 :handoff (lambda (id name arguments) (setf handed (list id name (gethash "form" arguments)))))))
    (is (search "end the turn" nodecode-cursor::+handoff+) "the handoff text is omp's")
    (is (equal '("tc1" "eval" "(+ 1 2)") handed) "the call reaches the round's message"))
  (is (equalp (list (cu-len #x12 #x08 #x02 (cu-len #x5a (cu-len #x1a (cu-len #x0a "Tool \"eval\" is not approved to run without asking.")))))
              (nodecode-cursor::exec-answer
               (cu-exec #x08 #x02 (cu-len #x5a (cu-len #x0a "eval") (cu-len #x1a "tc1") #x38 #x01))))
      "an approval probe (smart_mode_approval_only 7) runs nothing and is refused"))

(deftest cursor-cell-tells-the-service-its-tools-and-rules ()
  (let ((rule (nodecode-cursor::cursor-rule "/omp/system-prompt/0.mdc" "Be brief."))
        (tool (nodecode-cursor::mcp-tool-definition "eval" "Run Lisp" (nlk:json-object "type" "object"))))
    ;; CursorRule { full_path 1, content 2, type 3 { global 1 {} }, source 4 = USER (2) }
    (is (equalp (cu-bytes (cu-len #x0a "/omp/system-prompt/0.mdc") (cu-len #x12 "Be brief.")
                          (cu-len #x1a (cu-len #x0a)) #x20 #x02)
                rule))
    ;; McpToolDefinition { name 1, provider_identifier 4, tool_name 5, description 2, input_schema 3 }
    (is (equalp (cu-bytes (cu-len #x0a "eval") (cu-len #x22 "pi-agent") (cu-len #x2a "eval") (cu-len #x12 "Run Lisp")
                          (cu-len #x1a (cu-len #x2a (cu-len #x0a (cu-len #x0a "type") (cu-len #x12 (cu-len #x1a "object"))))))
                tool))
    ;; { id 1, request_context_args 10 {} } -> request_context_result 10 { success 1 { request_context 1 { rules 2, tools 7 } } }
    (is (equalp (list (cu-len #x12 #x08 #x01 (cu-len #x52 (cu-len #x0a (cu-len #x0a (cu-len #x12 rule) (cu-len #x3a tool))))))
                (nodecode-cursor::exec-answer (cu-exec #x08 #x01 (cu-len #x52)) :rules (list rule) :tools (list (cons "eval" tool)))))
    ;; mcp_state_exec_args 36 -> mcp_state_exec_result 36 { success 1 { servers 1 { name, identifier, tools 5, status 7 } } }
    (is (equalp (list (cu-len #x12 #x08 #x06 (cu-len '(#xa2 #x02) (cu-len #x0a (cu-len #x0a (cu-len #x0a "pi-agent") (cu-len #x12 "pi-agent")
                                                                             (cu-len #x2a tool) (cu-len #x3a "connected"))))))
                (nodecode-cursor::exec-answer (cu-exec #x08 #x06 (cu-len '(#xa2 #x02))) :tools (list (cons "eval" tool)))))))

(deftest cursor-cell-serves-blobs-and-answers-queries ()
  (let ((blobs (make-hash-table :test #'equal))
        (id (cu-digest "{}")))
    (setf (gethash (nodecode-cursor::hex id) blobs) (cu-octets "{}"))
    ;; KvServerMessage { id 1 = 5, get_blob_args 2 { blob_id 1 } } -> kv_client_message 3 { id, get_blob_result 2 { blob_data 1 } }
    (is (equalp (cu-len #x1a #x08 #x05 (cu-len #x12 (cu-len #x0a "{}")))
                (nodecode-cursor::kv-answer (cu-exec #x08 #x05 (cu-len #x12 (cu-len #x0a id))) blobs)))
    (is (equalp (cu-len #x1a #x08 #x06 (cu-len #x12))
                (nodecode-cursor::kv-answer (cu-exec #x08 #x06 (cu-len #x12 (cu-len #x0a (cu-bytes 1 2 3)))) blobs))
        "a blob not held answers an empty result")
    (is (equalp (cu-len #x1a #x08 #x07 (cu-len #x1a))
                (nodecode-cursor::kv-answer (cu-exec #x08 #x07 (cu-len #x1a (cu-len #x0a (cu-bytes 4)) (cu-len #x12 "z"))) blobs)))
    (is (equalp (cu-octets "z") (gethash "04" blobs)) "a set keeps the blob"))
  ;; InteractionQuery { id 1, web_search_request_query 2 {} } -> interaction_response 6 { id, web_search 2 { approved 1 {} } }
  (is (equalp (cu-len #x32 #x08 #x04 (cu-len #x12 (cu-len #x0a)))
              (nodecode-cursor::interaction-answer (cu-exec #x08 #x04 (cu-len #x12)))))
  (is (equalp (cu-len #x32 #x08 #x04 (cu-len #x22 (cu-len #x12 (cu-len #x0a "Mode switches are not implemented by this client"))))
              (nodecode-cursor::interaction-answer (cu-exec #x08 #x04 (cu-len #x22)))))
  (is (null (nodecode-cursor::interaction-answer (cu-exec #x08 #x04 (cu-len #x42)))) "VM setup is left alone")
  (is (equalp (cu-len #x32 #x08 #x04 (cu-len #x62 (cu-len #x0a)))
              (nodecode-cursor::interaction-answer (cu-exec #x08 #x04 (cu-len #x62))))
      "an unmodelled permission gate is approved on its own member"))

;;; --- the error trailer --------------------------------------------------------------------

(deftest cursor-cell-reads-the-end-of-stream-trailer ()
  (is (null (nodecode-cursor::end-stream-error (cu-octets "{}"))) "a clean end")
  (let ((error (nodecode-cursor::end-stream-error
                (cu-octets "{\"error\":{\"code\":\"resource_exhausted\",\"message\":\"Error\"}}"))))
    (is (eql 429 (nle::provider-error-status error)))
    (is (equal "Connect error resource_exhausted: Error" (nle::provider-error-detail error))))
  ;; aiserver.v1.ErrorDetails { error 1 = BAD_MODEL_NAME (5), details 2 { title 1, detail 2 } }
  (let* ((detail (cl-base64:usb8-array-to-base64-string
                  (cu-bytes #x08 #x05 (cu-len #x12 (cu-len #x0a "Bad model") (cu-len #x12 "nope")))))
         (error (nodecode-cursor::end-stream-error
                 (cu-octets (format nil "{\"error\":{\"code\":\"not_found\",\"message\":\"x\",\"details\":[{\"type\":\"aiserver.v1.ErrorDetails\",\"value\":\"~a\"}]}}" detail)))))
    (is (eql 404 (nle::provider-error-status error)))
    (is (equal "Cursor BAD_MODEL_NAME: Bad model: nope" (nle::provider-error-detail error)))
    (is (nodecode-cursor::model-not-found-p error)))
  (let ((error (nodecode-cursor::end-stream-error
                (cu-octets "{\"error\":{\"code\":\"internal\",\"message\":\"Error\",\"details\":[{\"type\":\"aiserver.v1.ErrorDetails\",\"debug\":{\"details\":{\"isRetryable\":true}}}]}}"))))
    (is (eql 503 (nle::provider-error-status error)) "a retryable verdict in the debug form")
    (is (search "[details: aiserver.v1.ErrorDetails: " (nle::provider-error-detail error))))
  (is (equal "Failed to parse Connect end stream"
             (nle::provider-error-detail (nodecode-cursor::end-stream-error (cu-octets "{nope"))))))

;;; --- a fake Cursor service --------------------------------------------------------------
;;; RunSSE answers a body this file feeds; each BidiAppend hands the client's
;;; message to SCRIPT, which feeds the body of the run it names.

(defclass cu-body (sb-gray:fundamental-binary-input-stream)
  ((lock :initform (bt2:make-lock) :reader cu-lock)
   (ready :initform (bt2:make-condition-variable) :reader cu-ready)
   (octets :initform (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0) :reader cu-buffer)
   (position :initform 0 :accessor cu-position)
   (ended :initform nil :accessor cu-ended)))

(defmethod stream-element-type ((stream cu-body)) '(unsigned-byte 8))

(defmethod sb-gray:stream-read-byte ((stream cu-body))
  (bt2:with-lock-held ((cu-lock stream))
    (loop with deadline = (+ (get-internal-real-time) (* 10 internal-time-units-per-second))
          until (or (< (cu-position stream) (fill-pointer (cu-buffer stream)))
                    (cu-ended stream)
                    (> (get-internal-real-time) deadline))
          do (bt2:condition-wait (cu-ready stream) (cu-lock stream) :timeout 0.2))
    (if (< (cu-position stream) (fill-pointer (cu-buffer stream)))
        (prog1 (aref (cu-buffer stream) (cu-position stream)) (incf (cu-position stream)))
        :eof)))

(defmethod close ((stream cu-body) &key abort)
  (declare (ignore abort))
  (bt2:with-lock-held ((cu-lock stream))
    (setf (cu-ended stream) t)
    (bt2:condition-notify (cu-ready stream)))
  t)

(defun cu-feed (body &rest frames)
  "Append the server FRAMES (AgentServerMessage payloads, or (:end JSON)) to BODY."
  (bt2:with-lock-held ((cu-lock body))
    (dolist (frame frames)
      (let ((octets (if (and (consp frame) (eq (first frame) :end))
                        (cu-frame (cu-octets (second frame)) 2)
                        (cu-frame frame))))
        (loop for octet across octets do (vector-push-extend octet (cu-buffer body)))
        (when (and (consp frame) (eq (first frame) :end))
          (setf (cu-ended body) t))))
    (bt2:condition-notify (cu-ready body))))

(defstruct cu-service
  (lock (bt2:make-lock))
  (bodies (make-hash-table :test #'equal))
  (runs '())          ; (URL HEADERS BODY-OCTETS), newest first
  (appends '())       ; (REQUEST-ID SEQNO DATA HEADERS), newest first
  script)

(defun cu-body-of (service request-id)
  (bt2:with-lock-held ((cu-service-lock service))
    (or (gethash request-id (cu-service-bodies service))
        (setf (gethash request-id (cu-service-bodies service)) (make-instance 'cu-body)))))

(defmacro with-cu-service ((service script) &body forms)
  "FORMS with dex:post answering as a Cursor service whose appends SCRIPT,
a function of (BODY SEQNO MESSAGE-FIELDS RUN-INDEX), answers."
  `(let ((,service (make-cu-service :script ,script)))
     (with-stubbed-fdefinition
         (dex:post (url &rest args)
          (let ((headers (getf args :headers)) (content (getf args :content)))
            (cond
              ((search "/agent.v1.AgentService/RunSSE" url)
               (let* ((id (nodecode-cursor::pb-text (nodecode-cursor::pb-decode (subseq content 5)) 1)))
                 (bt2:with-lock-held ((cu-service-lock ,service))
                   (push (list url headers content id) (cu-service-runs ,service)))
                 (values (cu-body-of ,service id) 200)))
              ((search "/aiserver.v1.BidiService/BidiAppend" url)
               (let* ((fields (nodecode-cursor::pb-decode content))
                      (id (nodecode-cursor::pb-text (nodecode-cursor::pb-sub fields 2) 1))
                      (seqno (or (nodecode-cursor::pb-get fields 3) 0))
                      (data (nodecode-cursor::pb-get fields 4))
                      (run (bt2:with-lock-held ((cu-service-lock ,service))
                             (push (list id seqno data headers) (cu-service-appends ,service))
                             (position id (remove-duplicates (mapcar #'first (reverse (cu-service-appends ,service)))
                                                             :test #'equal :from-end t)
                                       :test #'equal))))
                 (funcall (cu-service-script ,service) (cu-body-of ,service id) seqno
                          (nodecode-cursor::pb-decode data) (or run 0))
                 (values (make-array 0 :element-type '(unsigned-byte 8)) 200)))
              (t (error "unexpected POST to ~a" url)))))
       ,@forms)))

(defun cu-appends (service)
  "The appends SERVICE saw, oldest first."
  (reverse (cu-service-appends service)))

;;; The service's messages, written out: AgentServerMessage { interaction_update 1 { ... } }

(defun cu-update (member-tag &rest parts)
  (cu-len #x0a (apply #'cu-len member-tag parts)))

(defun cu-text-delta (text) (cu-update #x0a (cu-len #x0a text)))
(defun cu-thinking-delta (text) (cu-update #x22 (cu-len #x0a text)))
(defun cu-mcp-tool-call (name id &optional arguments)
  ;; ToolCall { mcp_tool_call 15 { args 1 { name 1, args 2 { key 1, value 2 }, tool_call_id 3 } } }
  (cu-len #x7a (cu-len #x0a (cu-len #x0a name)
                       (loop for (key value) on arguments by #'cddr
                             collect (cu-len #x12 (cu-len #x0a key) (cu-len #x12 (cu-len #x1a value))))
                       (cu-len #x1a id))))
(defun cu-call-started (call-id tool-call) (cu-update #x12 (cu-len #x0a call-id) (cu-len #x12 tool-call)))
(defun cu-partial-call (call-id args-text) (cu-update #x3a (cu-len #x0a call-id) (cu-len #x1a args-text)))
(defun cu-call-completed (call-id tool-call) (cu-update #x1a (cu-len #x0a call-id) (cu-len #x12 tool-call)))
(defun cu-turn-ended (input output cache-read) (cu-update #x72 #x08 input #x10 output #x18 cache-read))
(defun cu-exec-frame (&rest parts) (apply #'cu-len #x12 parts))

(defun cu-member (fields)
  "The AgentClientMessage member FIELDS (the client's message) carries."
  (nodecode-cursor::pb-oneof fields '(1 2 3 4 5 6 7 8)))

(defun cu-lane-round (context)
  "One round of CONTEXT on the lane its frozen config names, the way the turn loop runs it."
  (let ((config (nle::compiled-turn-context-provider-config context)))
    (funcall (nle::provider-lane-stream-symbol
              (nle::find-lane-by-name (nle::effective-provider-config-lane config)))
             context)))

(defun cu-tool-round-script (system-json)
  "A service that asks for the request context and a blob, streams text,
thinking and one MCP call, asks for the call over the exec channel, and ends
the turn once the call is answered."
  (lambda (body seqno fields run)
    (declare (ignore seqno run))
    (case (cu-member fields)
      (1 (cu-feed body
                  (cu-exec-frame #x08 #x01 (cu-len #x52))
                  ;; kv_server_message 4 { id 1 = 5, get_blob_args 2 { blob_id 1 } }
                  (cu-len #x22 #x08 #x05 (cu-len #x12 (cu-len #x0a (cu-digest system-json))))
                  (cu-thinking-delta "I should add.")
                  (cu-text-delta "Adding ")
                  (cu-text-delta "now.")
                  (cu-call-started "c1" (cu-mcp-tool-call "eval" "tc1"))
                  (cu-partial-call "c1" "{\"form\":\"(+ 1")
                  (cu-partial-call "c1" "{\"form\":\"(+ 1 2)\"}")
                  (cu-call-completed "c1" (cu-mcp-tool-call "eval" "tc1" '("form" "(+ 1 2)")))
                  (cu-exec-frame #x08 #x02 (cu-len #x5a (cu-len #x0a "eval") (cu-len #x1a "tc1")))))
      (2 (when (eql 2 (nodecode-cursor::pb-get (nodecode-cursor::pb-sub fields 2) 1))
           (cu-feed body
                    (cu-turn-ended 100 20 10)
                    ;; conversation_checkpoint_update 3 { todos 3, token_details 5 { used_tokens 1 } }
                    (cu-len #x1a (cu-len #x1a "todo-blob") (cu-len #x2a #x08 #x78))
                    '(:end "{}")))))))

(deftest cursor-cell-runs-a-round-and-hands-its-call-to-the-turn-loop ()
  (with-cell-stop ((cursor-start))
    (let ((nle::*provider* "cursor") (nle::*model* "claude-opus-5-5") (nle::*api-key* "tok-1")
          (nle::*reasoning-effort* "high") (nle::*endpoint* nil))
      (let* ((context (user-context "add one and two"))
             (system (nle::compiled-turn-context-system-prompt context))
             (system-json (format nil "{\"role\":\"system\",\"content\":~a}"
                                  (nlk:encode-json-object (if (plusp (length (nlk:trimmed system))) system "You are a helpful assistant.")))))
        (with-cu-service (service (cu-tool-round-script system-json))
          (multiple-value-bind (message usage finish request) (cu-lane-round context)
            (is (equal "Adding now." (nlk:json-value message :string "content")))
            (is (equal "I should add." (nlk:json-value message :string "reasoning_content")))
            (is (equal "tool_calls" finish) "a round that ends on a call hands it on")
            (is-present (call (and (= 1 (length (nlk:json-value message :array "tool_calls")))
                                   (aref (nlk:json-value message :array "tool_calls") 0)))
                "one tool call"
              (is (equal "tc1" (nlk:json-value call :string "id")))
              (is (equal "eval" (nlk:json-value call :string "function" "name")))
              (is (equal "(+ 1 2)" (nlk:json-value (nlk:decode-json (nlk:json-value call :string "function" "arguments"))
                                                   :string "form"))))
            (is (subsetp (alexandria:hash-table-keys message)
                         '("role" "content" "reasoning_content" "tool_calls") :test #'equal)
                "the round carries no field of the cell's own")
            (is (equal "claude-opus-5-5" (nodecode-cursor::round-model message))
                "the cell knows which model wrote it")
            (is (eql 90 (nle::provider-usage-input-tokens usage)) "input less what the cache served")
            (is (eql 20 (nle::provider-usage-output-tokens usage)))
            (is (eql 10 (nle::provider-usage-cached-input-tokens usage)))
            (destructuring-bind (url headers body id) (first (cu-service-runs service))
              (is (equal "https://api2.cursor.sh/agent.v1.AgentService/RunSSE" url))
              (is (equal "Bearer tok-1" (cu-header headers "authorization")))
              (is (equal "application/connect+proto" (cu-header headers "content-type")))
              (is (equal "cli" (cu-header headers "x-cursor-client-type")))
              (is (equal "cli-2026.09.02-c22c1a3" (cu-header headers "x-cursor-client-version")))
              (is (equal "true" (cu-header headers "x-cursor-streaming")))
              (is (equal "1" (cu-header headers "connect-protocol-version")))
              (is (equalp (cu-frame (cu-len #x0a id)) body) "the body is one frame of the BidiRequestId"))
            (let ((appends (cu-appends service)))
              (destructuring-bind (id seqno data headers) (first appends)
                (declare (ignore id))
                (is (eql 0 seqno) "the run request goes first")
                (is (equalp request data) "and is the request the round keeps")
                (is (equal "application/proto" (cu-header headers "content-type")))
                (is (equal "true" (cu-header headers "x-cursor-streaming")) "with the run's own headers"))
              (is (equal (loop for i below (length appends) collect i) (mapcar #'second appends))
                  "the appends ride in sequence")
              (let ((sent (mapcar (lambda (append) (third append)) appends)))
                (is (find (cu-len #x1a #x08 #x05 (cu-len #x12 (cu-len #x0a system-json))) sent :test #'equalp)
                    "the blob the service asked for is served")
                (is (find (cu-len #x12 #x08 #x02 (cu-len #x5a (cu-len #x0a (cu-len #x0a (cu-len #x0a (cu-len #x0a nodecode-cursor::+handoff+))))))
                          sent :test #'equalp)
                    "the call is answered with the handoff")
                (is (find-if (lambda (octets)
                               (let ((exec (nodecode-cursor::pb-sub (nodecode-cursor::pb-decode octets) 2)))
                                 (and (eql 1 (nodecode-cursor::pb-get exec 1))
                                      (nodecode-cursor::pb-sub exec 10))))
                             sent)
                    "the request context is answered")))
            ;; requested_model = 9 { model_id 1 }: a Claude sibling goes as itself
            (let ((run (nodecode-cursor::pb-sub (nodecode-cursor::pb-decode request) 1)))
              (is (equal "claude-opus-5-5-high" (nodecode-cursor::pb-text (nodecode-cursor::pb-sub run 9) 1))))))))))

(deftest cursor-cell-retries-the-discovery-id-when-the-pair-is-unknown ()
  (with-cell-stop ((cursor-start))
    (let ((nle::*provider* "cursor") (nle::*model* "gpt-5.4") (nle::*api-key* "tok-1")
          (nle::*reasoning-effort* "high") (nle::*endpoint* nil))
      (let ((detail (cl-base64:usb8-array-to-base64-string (cu-bytes #x08 #x05 (cu-len #x12 (cu-len #x0a "Unknown model"))))))
        (with-cu-service (service (lambda (body seqno fields run)
                                    (declare (ignore seqno))
                                    (when (eql 1 (cu-member fields))
                                      (if (zerop run)
                                          (cu-feed body (list :end (format nil "{\"error\":{\"code\":\"not_found\",\"message\":\"x\",\"details\":[{\"type\":\"aiserver.v1.ErrorDetails\",\"value\":\"~a\"}]}}" detail)))
                                          (cu-feed body (cu-text-delta "4") (cu-turn-ended 5 1 0) '(:end "{}"))))))
          (multiple-value-bind (message usage finish) (cu-lane-round (user-context "two plus two"))
            (declare (ignore usage))
            (is (equal "4" (nlk:json-value message :string "content")))
            (is (equal "stop" finish))
            (let ((models (loop for (nil seqno data) in (cu-appends service)
                                when (eql 0 seqno)
                                  collect (let ((requested (nodecode-cursor::pb-sub
                                                            (nodecode-cursor::pb-sub (nodecode-cursor::pb-decode data) 1) 9)))
                                            (list (nodecode-cursor::pb-text requested 1)
                                                  (length (nodecode-cursor::pb-all requested 3)))))))
              (is (equal '(("gpt-5.4" 1) ("gpt-5.4-high" 0)) models)
                  "the normalized pair, then the discovery id with no parameter"))))))))

(deftest cursor-cell-fails-a-stream-cut-before-the-turn-ended ()
  (with-cell-stop ((cursor-start))
    (let ((nle::*provider* "cursor") (nle::*model* "gpt-5.4") (nle::*api-key* "tok-1") (nle::*endpoint* nil))
      (with-cu-service (service (lambda (body seqno fields run)
                                  (declare (ignore seqno run))
                                  (when (eql 1 (cu-member fields))
                                    (cu-feed body (cu-call-started "c1" (cu-mcp-tool-call "eval" "tc1")))
                                    (close body))))
        (is (signals-error nle::provider-stream-incomplete (cu-lane-round (user-context "go")))
            "a stream that ends on an open call is incomplete, which the turn retries"))
      (with-cu-service (service (lambda (body seqno fields run)
                                  (declare (ignore seqno run))
                                  (when (eql 1 (cu-member fields))
                                    ;; step_completed 17 { step_id 1 } after the answer, then the line drops
                                    (cu-feed body (cu-text-delta "done") (cu-update (quote (#x8a #x01)) #x08 #x01))
                                    (close body))))
        (is (equal "done" (nlk:json-value (cu-lane-round (user-context "go")) :string "content"))
            "a stream cut after the final step's answer text lost only its usage"))
      (with-cu-service (service (lambda (body seqno fields run)
                                  (declare (ignore seqno run))
                                  (when (eql 1 (cu-member fields))
                                    (cu-feed body (list :end "{\"error\":{\"code\":\"resource_exhausted\",\"message\":\"Error\"}}")))))
        (let ((refusal (signals-error nle::provider-error (cu-lane-round (user-context "go")))))
          (is (eql 429 (and refusal (nle::provider-error-status refusal)))))))))

(deftest cursor-cell-rotates-a-poisoned-conversation-once-per-streak ()
  (with-saved-globals ((nodecode-cursor::*rotated* (make-hash-table :test #'equal))
                       (nodecode-cursor::*rotated-good* (make-hash-table :test #'equal))
                       (nodecode-cursor::*rotated-fresh* (make-hash-table :test #'equal)))
    (nodecode-cursor::rotate "base" "base")
    (let ((first (gethash "base" nodecode-cursor::*rotated*)))
      (is (and first (not (equal first "base"))) "a fresh wire id")
      (is (gethash first nodecode-cursor::*rotated-fresh*) "rebuilt from the history alone")
      (nodecode-cursor::rotate "base" first)
      (is (equal first (gethash "base" nodecode-cursor::*rotated*)) "a failed rotation is not repeated")
      (setf (gethash first nodecode-cursor::*rotated-good*) t)
      (nodecode-cursor::rotate "base" first)
      (is (not (equal first (gethash "base" nodecode-cursor::*rotated*))) "a rotated id that completed may rotate again"))))

;;; --- after a model switch ----------------------------------------------------------------

(deftest cursor-cell-round-replays-clean-on-another-lane ()
  (with-cell-stop ((cursor-start))
    (let ((round nil))
      (let ((nle::*provider* "cursor") (nle::*model* "kimi-k3-high") (nle::*api-key* "tok-1")
            (nle::*reasoning-effort* "high") (nle::*endpoint* nil))
        (with-cu-service (service (lambda (body seqno fields run)
                                    (declare (ignore seqno run))
                                    (when (eql 1 (cu-member fields))
                                      (cu-feed body (cu-thinking-delta "think") (cu-text-delta "4")
                                               (cu-turn-ended 5 1 0) '(:end "{}")))))
          (setf round (cu-lane-round (user-context "two plus two")))))
      (is (equal "kimi-k3-high" (nodecode-cursor::round-model round)) "a K3 round's writer is kept by the cell")
      ;; the session moves to another provider: the chat lane replays the round
      (let ((nle::*provider* "openai-completions") (nle::*model* "gpt-5.4") (nle::*api-key* "k")
            (nle::*reasoning-effort* nil) (nle::*endpoint* nil) (body nil))
        (with-stubbed-fdefinition
            (dex:post (asked &rest args)
             (setf body (nlk:decode-json (getf args :content)))
             (values (make-truncated-sse-stream
                      "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                      "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                     200))
          (nle::call-provider-streaming
           (compiled-context (list (nle::message "user" "two plus two") round (nle::message "user" "next")))))
        (is-present (replayed (find "assistant" (coerce (nlk:json-value body :array "messages") 'list)
                                    :key (lambda (message) (nlk:json-value message :string "role")) :test #'equal))
            "the chat request replays the cursor round"
          (is (equal "4" (nlk:json-value replayed :string "content")))
          (is (subsetp (alexandria:hash-table-keys replayed)
                       '("role" "content" "reasoning_content" "reasoning_signature" "tool_calls") :test #'equal)
              (format nil "no field the cell added rides the other lane: ~s" (alexandria:hash-table-keys replayed))))))))

;;; --- the sign-in ----------------------------------------------------------------------

(deftest cursor-cell-signs-in-by-polling-and-keeps-the-token ()
  (with-cell-stop ((cursor-start))
    (with-temp-auth (auth "{\"api_keys\":{\"other\":{\"provider\":\"other\",\"key\":\"k\"}}}")
      (with-saved-globals ((nodecode-cursor::*poll-base-delay* 0.01))
        (let ((polls '()) (profile-cookie nil) (access (cu-jwt :sub "auth0|user_42" :exp 2000000000)))
          (with-stubbed-fdefinition
              (dex:get (url &rest args)
               (cond ((search "/auth/poll" url)
                      (push url polls)
                      (if (< (length polls) 2)
                          (error 'dex:http-request-not-found :status 404 :body "" :uri url :method :get :headers nil)
                          (values (format nil "{\"accessToken\":\"~a\",\"refreshToken\":\"rt-9\"}" access) 200)))
                     ((search "/api/auth/me" url)
                      (setf profile-cookie (cu-header (getf args :headers) "Cookie"))
                      (values "{\"sub\":\"user_42\",\"email\":\"me@example.com\"}" 200))
                     (t (error "unexpected GET ~a" url))))
            (let* ((answer (nodecode-cursor::login auth))
                   (url (ppcre:scan-to-strings "https://cursor\\.com/loginDeepControl\\?\\S+" answer)))
              (is url "the answer names the page to open")
              (is (await (:timeout 5)
                    (nodecode-cursor::stored-entry (nle::read-auth-file auth)))
                  "the sign-in finishes in the background")
              (let* ((params (quri:url-decode-params (subseq url (1+ (position #\? url)))))
                     (challenge (cdr (assoc "challenge" params :test #'equal)))
                     (uuid (cdr (assoc "uuid" params :test #'equal)))
                     (poll (first polls))
                     (verifier (ppcre:register-groups-bind (v) ("verifier=([^&]+)" poll) v)))
                (is (equal "login" (cdr (assoc "mode" params :test #'equal))))
                (is (equal "cli" (cdr (assoc "redirectTarget" params :test #'equal))))
                (is (search (format nil "uuid=~a" uuid) poll) "the poll names the same uuid")
                (is (= 128 (length verifier)) "96 random octets as base64url")
                (is (equal challenge (cu-b64url (cu-digest verifier))) "PKCE S256"))
              (is (equal (format nil "WorkosCursorSessionToken=~a" (quri:url-encode (format nil "user_42::~a" access)))
                         profile-cookie)
                  "the profile is read with the token as the web session")
              (let* ((stored (nle::read-auth-file auth))
                     (entry (nodecode-cursor::stored-entry stored)))
                (is (equal access (nlk:json-value entry :string "access_token")))
                (is (equal "rt-9" (nlk:json-value entry :string "refresh_token")))
                (is (eql (- 2000000000 300) (nlk:json-value entry :integer "expires_at")) "exp less five minutes")
                (is (equal "me@example.com" (nlk:json-value entry :string "email")))
                (is (equal "k" (nlk:json-value stored :string "api_keys" "other" "key")) "every other field is kept")
                (is (equal "600" (format nil "~o" (logand #o777 (sb-posix:stat-mode (sb-posix:stat auth)))))
                    "the store is 0600")))))))))

(deftest cursor-cell-gives-up-polling-after-three-errors-in-a-row ()
  (with-cell-stop ((cursor-start))
    (with-temp-auth (auth "{}")
      (with-saved-globals ((nodecode-cursor::*poll-base-delay* 0.01))
        (with-stubbed-fdefinition (dex:get (url &rest args) (values "oops" 500))
          (nodecode-cursor::login auth)
          (is (await (:timeout 5) (cu-said "Too many consecutive errors during Cursor auth polling")))
          (is (null (nodecode-cursor::stored-entry (nle::read-auth-file auth)))))))))

(deftest cursor-cell-refreshes-a-due-token-on-a-round-only ()
  (with-cell-stop ((cursor-start))
    (with-temp-auth (auth (cu-signed-in :access "old" :refresh "rt-1" :expires 10 :email "me@example.com"))
      (let ((posts '()) (new (cu-jwt :exp 2100000000)))
        (with-stubbed-fdefinition
            (dex:post (url &rest args)
             (push (list url (getf args :content)) posts)
             (values (format nil "{\"access_token\":\"~a\"}" new) 200))
          (is (equal "old" (nle:credential-key (nle::resolve-provider-credential "cursor" :auth-path auth :probe t)))
              "a probe never refreshes")
          (is (null posts) "nor dials")
          (is (equal new (nle:credential-key (nle::resolve-provider-credential "cursor" :auth-path auth
                                                                                :endpoint "https://api2.cursor.sh")))
              "a round refreshes the due token first")
          (destructuring-bind (url body) (first posts)
            (is (equal "https://api2.cursor.sh/oauth/token" url))
            (let ((sent (nlk:decode-json body)))
              (is (equal "refresh_token" (nlk:json-value sent :string "grant_type")))
              (is (equal "KbZUR41cY7W6zRSdpSUJ7I7mLYBKOCmB" (nlk:json-value sent :string "client_id")))
              (is (equal "rt-1" (nlk:json-value sent :string "refresh_token")))))
          (let ((entry (nodecode-cursor::stored-entry (nle::read-auth-file auth))))
            (is (equal new (nlk:json-value entry :string "access_token")))
            (is (equal "rt-1" (nlk:json-value entry :string "refresh_token")) "Cursor's renewal keeps the refresh token")
            (is (eql (- 2100000000 300) (nlk:json-value entry :integer "expires_at")))
            (is (equal "me@example.com" (nlk:json-value entry :string "email")))))))))

(deftest cursor-cell-says-a-session-cursor-ended ()
  (with-cell-stop ((cursor-start))
    (with-temp-auth (auth (cu-signed-in :access "old" :expires 10))
      (with-stubbed-fdefinition (dex:post (url &rest args) (values "{\"access_token\":\"\",\"shouldLogout\":true}" 200))
        (is (signals-error nle::credential-error
              (nle::resolve-provider-credential "cursor" :auth-path auth :endpoint "https://api2.cursor.sh")))
        (is (cu-said "Cursor ended this session") "the refusal stands as a notice")
        (is (equal "old" (nlk:json-value (nodecode-cursor::stored-entry (nle::read-auth-file auth)) :string "access_token"))
            "the kept sign-in is left as it was")))))

(deftest cursor-cell-says-its-status-and-signs-out ()
  (with-cell-stop ((cursor-start))
    (with-temp-auth (auth (cu-signed-in :email "me@example.com"))
      (with-saved-globals ((nle::*auth-file-path* auth))
        (is (search "signed in as me@example.com" (cell-entry "nodecode-cursor" "cursor" "status")))
        (is (search "signed out" (cell-entry "nodecode-cursor" "cursor" "logout")))
        (is (null (nodecode-cursor::stored-entry (nle::read-auth-file auth))))
        (is (search "usage: /cursor" (cell-entry "nodecode-cursor" "cursor" "bogus")))))))

;;; --- the listing and the neighbours ---------------------------------------------------------

(deftest cursor-cell-lists-the-accounts-roster ()
  (with-cell-stop ((cursor-start))
    (let ((asked nil))
      (with-stubbed-fdefinition
          (dex:post (url &rest args)
           (setf asked (list url (getf args :headers) (getf args :content)))
           ;; GetUsableModelsResponse { models 1 { model_id 1, display_name 4, max_mode 7 } ... }
           (values (cu-bytes (cu-len #x0a (cu-len #x0a "gpt-5.4-high") (cu-len #x22 "GPT-5.4 High") #x38 #x01)
                             (cu-len #x0a (cu-len #x0a "kimi-k3") (cu-len #x1a "Kimi K3 1M"))
                             (cu-len #x0a (cu-len #x0a "claude-opus-5-5")))
                   200))
        (multiple-value-bind (rows error) (nle::list-provider-models "cursor" :key "tok-l")
          (is (null error))
          (is (equal '("claude-opus-5-5" "gpt-5.4-high" "kimi-k3") (mapcar (lambda (row) (getf row :id)) rows)))
          (is (equal "GPT-5.4 High" (getf (second rows) :display)))
          (is (eql 1000000 (getf (third rows) :context-window)) "a 1M label is a 1M window")
          (is (eql 1000000 (getf (first rows) :context-window)) "a bundled model keeps its window")
          (is (eq t (nodecode-cursor::resolve-max-mode "gpt-5.4-high" "gpt-5.4-high"))
              "the roster's max-mode marker rides the model's rounds"))
        (destructuring-bind (url headers content) asked
          (is (equal "https://api2.cursor.sh/agent.v1.AgentService/GetUsableModels" url))
          (is (equal "Bearer tok-l" (cu-header headers "authorization")))
          (is (equal "application/proto" (cu-header headers "content-type")))
          (is (zerop (length content)) "an empty GetUsableModelsRequest"))))))

(deftest cursor-cell-checks-a-key-by-asking-with-it ()
  (with-cell-stop ((cursor-start))
    (with-saved-globals ((nle::*provider-models-cache-path* (format nil "/tmp/cursor-cell-models-~a.json" (random 1000000))))
      (let ((asked '()))
        (with-stubbed-fdefinition
            (dex:post (url &rest args)
             (let ((token (cu-header (getf args :headers) "authorization")))
               (push token asked)
               (if (equal token "Bearer tok-good")
                   (values (cu-bytes (cu-len #x0a (cu-len #x0a "gpt-5.4"))) 200)
                   (error 'dex:http-request-unauthorized
                          :status 401 :uri url :method :post :headers nil
                          :body (cu-octets "{\"code\":\"unauthenticated\",\"message\":\"bad key\"}")))))
          (multiple-value-bind (rows reason) (nle::list-provider-models "cursor" :key "tok-bad")
            (is (null rows))
            (is (equal "HTTP 401 unauthenticated" reason) "a refusal in the core's reason shape"))
          (is (equal "Bearer tok-bad" (first asked)) "the key under check is the one asked with")
          (is (eq :refused (nle::provider-key-check "cursor" "tok-bad")) "so /connect says refused")
          (is (eq :works (nle::provider-key-check "cursor" "tok-good")) "and a key the roster answers works"))))))

(deftest cursor-cell-leaves-other-providers-alone ()
  (with-cell-stop ((cursor-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "gpt-5.4") (nle::*api-key* "k")
          (nle::*endpoint* nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (is (search "/chat/completions" asked))
           (setf headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (equal "gpt-5.4" (nlk:json-value body :string "model")))
      (is (null (cu-header headers "x-cursor-client-type"))))))
