;;;; wire.lisp --- the Cascade exchanges: GetUserJwt, AssignModel, GetChatMessage, GetCliModelConfigs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): ai/src/providers/devin.ts (the auth
;;;; exchange, the router assignment, the chat request, the stream fold, the
;;;; Connect trailer's error, the HTTP error's detail), catalog/src/wire/
;;;; devin-proto.ts (a unary answer, bare or gzipped), catalog/src/discovery/
;;;; devin.ts (fetchDevinModels), ai/src/utils/deterministic-id.ts, and a
;;;; compact form of ai/src/utils/schema/normalize.ts's Google normalizer.
;;;;
;;;; A round is three calls to the Cascade host, all Connect over HTTP/1.1:
;;;;
;;;;   GetUserJwt       unary, application/proto: the Metadata carries the
;;;;                    session token (devin-session-token$ in front; on a 401
;;;;                    the bare key is tried once), the answer is the user
;;;;                    JWT and, for some accounts, a chat host of their own
;;;;   AssignModel      unary, only for a router model (`adaptive'): the
;;;;                    router names the concrete uid and an assignment JWT
;;;;   GetChatMessage   server streaming, application/connect+proto: one
;;;;                    gzipped request frame; the answer is frames of
;;;;                    GetChatMessageResponse deltas (text, thinking, its
;;;;                    signature, tool calls, usage, a stop reason) and an
;;;;                    end-of-stream frame whose JSON may carry an error
;;;;
;;;; The organism's history is chat-shaped; it reaches Cascade as
;;;; ChatMessagePrompts: user turns as USER, the assistant's as SYSTEM (its
;;;; text, its thinking and signature when this model made them, its tool
;;;; calls), tool results as TOOL. Message ids are derived from the
;;;; conversation and the position, as omp derives them, so a history rebuilt
;;;; the same way names its turns the same way.

(in-package #:nodecode-devin)

(defparameter +chat-path+ "/exa.api_server_pb.ApiServerService/GetChatMessage")
(defparameter +assign-path+ "/exa.api_server_pb.ApiServerService/AssignModel")
(defparameter +auth-path+ "/exa.auth_pb.AuthService/GetUserJwt")
(defparameter +models-path+ "/exa.api_server_pb.ApiServerService/GetCliModelConfigs")

(defparameter +stop-patterns+ '("<|user|>" "<|bot|>" "<|context_request|>" "<|endoftext|>" "<|end_of_turn|>")
  "The stop patterns every chat request carries (DEVIN_DEFAULT_STOP_PATTERNS).")

(defparameter +large-history-bytes+ (* 512 1024)
  "The history size past which an opaque invalid_argument trailer before any
output is read as a context overflow (LARGE_HISTORY_RECOVERY_BYTES).")

(defparameter +discovery-seconds+ 5
  "How long one GetCliModelConfigs exchange may take (omp's timeoutMs).")

(defparameter +stream-headers+
  '(("content-type" . "application/connect+proto")
    ("connect-protocol-version" . "1")
    ("connect-content-encoding" . "gzip")
    ("accept-encoding" . "identity")
    ("user-agent" . "connect-go/1.18.1 (go1.26.3)")
    ("connect-accept-encoding" . "gzip"))
  "The headers of the streaming chat request, as omp sends them.")

(defparameter +unary-headers+
  '(("content-type" . "application/proto")
    ("connect-protocol-version" . "1")
    ("accept" . "*/*"))
  "The headers of a unary Connect call.")

;;; --- small helpers ---------------------------------------------------------------------

(defun deterministic-uuid (seed)
  "The leading 128 bits of SEED's SHA-256 in the 8-4-4-4-12 layout (deterministicUuid)."
  (let ((hex (subseq (nlk:sha256-text seed) 7)))
    (format nil "~a-~a-~a-~a-~a" (subseq hex 0 8) (subseq hex 8 12) (subseq hex 12 16)
            (subseq hex 16 20) (subseq hex 20 32))))

(defun seed (&rest parts)
  "PARTS joined by NUL, the way omp seeds a message id."
  (format nil (format nil "~~{~~a~~^~c~~}" (code-char 0)) parts))

(defun body-octets (body)
  "A response BODY (octets, a binary stream, a string or NIL) as octets."
  (typecase body
    (null (octets #()))
    (string (utf8 body))
    (stream (let ((out (make-writer)) (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
              (ignore-errors
               (loop for end = (read-sequence buffer body)
                     while (plusp end)
                     do (loop for index below end do (put-byte out (aref buffer index)))))
              (octets out)))
    (t (octets body))))

(defun header-value (headers name)
  "NAME's value in dexador's response HEADERS (a hash table or an alist), or NIL."
  (typecase headers
    (hash-table (gethash (string-downcase name) headers))
    (list (cdr (assoc name headers :test #'string-equal)))))

;;; --- errors ------------------------------------------------------------------------------

(defun error-detail (octets content-type)
  "The human sentence an error body carries, or NIL: never proxy HTML, never
binary protobuf, never control characters (devinErrorDetail)."
  (let ((text (ignore-errors (sb-ext:octets-to-string (octets octets) :external-format :utf-8))))
    (when (and text (not (search "text/html" (string-downcase (or content-type "")))))
      (let* ((text (nlk:trimmed text))
             (parsed (ignore-errors (nlk:decode-json text)))
             (text (or (and (hash-table-p parsed)
                            (or (nlk:json-value parsed :string "error" "message")
                                (nlk:json-value parsed :string "error")
                                (nlk:json-value parsed :string "message")))
                       text))
             (normalized (nlk:trimmed (ppcre:regex-replace-all "\\s+" text " "))))
        (when (and (plusp (length normalized))
                   (not (ppcre:scan "(?i)^\\s*(?:<!doctype\\s+html\\b|<html\\b)" normalized))
                   (notany (lambda (char) (let ((code (char-code char)))
                                            (or (< code 32) (<= 127 code 159))))
                           normalized))
          (subseq normalized 0 (min 4096 (length normalized))))))))

(defun http-error (operation status octets content-type)
  "The PROVIDER-ERROR an HTTP refusal of OPERATION is (createDevinHttpError)."
  (let ((detail (error-detail octets content-type)))
    (make-condition 'nle::provider-error
                    :status status
                    :scope :request
                    :detail (format nil "Devin ~a error ~a~@[: ~a~]" operation status detail)
                    :evidence-body (nle::bounded-evidence (or detail ""))
                    :evidence-content-type content-type)))

(defparameter +connect-statuses+
  '(("canceled" . 499) ("unknown" . 500) ("invalid_argument" . 400) ("deadline_exceeded" . 504)
    ("not_found" . 404) ("already_exists" . 409) ("permission_denied" . 403)
    ("resource_exhausted" . 429) ("failed_precondition" . 400) ("aborted" . 409)
    ("out_of_range" . 400) ("unimplemented" . 501) ("internal" . 500) ("unavailable" . 503)
    ("data_loss" . 500) ("unauthenticated" . 401))
  "The HTTP status the Connect protocol pairs with each error code.")

(defun clip-evidence (text)
  "TEXT bounded to two thousand characters (MAX_TRAILER_EVIDENCE_CHARS)."
  (if (> (length text) 2000) (format nil "~a…" (subseq text 0 2000)) text))

(defun trailer-details (details)
  "The trailer's error details as one line, or NIL (summarizeTrailerDetails)."
  (let ((summary ""))
    (loop for entry across (if (and (vectorp details) (not (stringp details))) details #())
          do (when (hash-table-p entry)
               (let* ((type (let ((value (nlk:json-value entry :string "type")))
                              (and (plusp (length (or value ""))) value)))
                      (value (let ((value (nlk:json-value entry :string "value")))
                               (and (plusp (length (or value ""))) value)))
                      (debug (multiple-value-bind (debug present) (gethash "debug" entry)
                               (and present (if (stringp debug) debug (ignore-errors (nlk:encode-json-object debug))))))
                      (evidence (or debug value))
                      (part (cond ((and type evidence) (format nil "~a: ~a" (clip-evidence type) (clip-evidence evidence)))
                                  (type (clip-evidence type))
                                  (evidence (clip-evidence evidence)))))
                 (when part
                   (let ((next (if (plusp (length summary)) (format nil "~a; ~a" summary part) part)))
                     (when (> (length next) 2000)
                       (return-from trailer-details (clip-evidence next)))
                     (setf summary next))))))
    (and (plusp (length summary)) summary)))

(defun trailer-error (text)
  "The error an end-of-stream trailer TEXT carries, as (:code :message
:formatted :detail :raw), or NIL (readConnectTrailerError)."
  (let* ((text (nlk:trimmed (or text "")))
         (parsed (and (plusp (length text)) (ignore-errors (nlk:decode-json text))))
         (error (and (hash-table-p parsed) (nlk:json-value parsed :object "error"))))
    (when error
      (let ((code (or (nlk:json-value error :string "code") ""))
            (message (or (nlk:json-value error :string "message") "")))
        (unless (and (zerop (length code)) (zerop (length message)))
          (let ((detail (trailer-details (gethash "details" error))))
            (list :code code :message message :detail detail :raw (clip-evidence text)
                  :formatted (format nil "Devin stream error~@[ ~a~]: ~a~@[ [details: ~a]~]"
                                     (and (plusp (length code)) code) message detail))))))))

;;; --- one unary call ---------------------------------------------------------------------

(defun unary (base path body &key (timeout 30))
  "(values OCTETS STATUS CONTENT-TYPE) of one unary Connect call to BASE+PATH:
a refusal answers its status and body; a transport failure is a request-scope
PROVIDER-ERROR."
  (let ((url (concatenate 'string base path)))
    (handler-case
        (nlk:with-cancellable-wait (nle::*current-durable-turn*)
          (sb-sys:with-deadline (:seconds timeout)
            (multiple-value-bind (answer status headers)
                (dex:post url :headers +unary-headers+ :content body :force-binary t
                              :connect-timeout timeout :read-timeout timeout)
              (values (body-octets answer) status (header-value headers "content-type")))))
      (nlk:turn-cancelled-condition (condition) (error condition))
      (dex:http-request-failed (e)
        (values (body-octets (ignore-errors (dex:response-body e)))
                (dex:response-status e)
                (header-value (ignore-errors (dex:response-headers e)) "content-type")))
      (sb-sys:deadline-timeout (condition)
        (error 'nle::provider-error :scope :request :detail (nle::transport-failure-label condition url)))
      (error (e)
        (error 'nle::provider-error :scope :request :detail (nle::transport-failure-label e url))))))

(defun ok-status-p (status)
  (and (integerp status) (<= 200 status 299)))

(defun unary-message (decoder octets)
  "OCTETS decoded by DECODER, bare or gzipped (decodeDevinUnaryMessage); NIL when neither decodes."
  (or (ignore-errors (multiple-value-list (funcall decoder octets)))
      (ignore-errors (multiple-value-list (funcall decoder (gunzip octets))))))

;;; --- GetUserJwt -------------------------------------------------------------------------

(defun user-jwt (token base)
  "(values USER-JWT CHAT-BASE WIRE-KEY) for the session TOKEN at BASE
(fetchDevinAuthMetadata): the session-token form first, the bare key once
on a 401; CHAT-BASE the account's own host when the answer names one."
  (let* ((session (session-token token))
         (wire-key session))
    (multiple-value-bind (octets status content-type)
        (unary base +auth-path+ (encode-metadata-request (cli-metadata token)))
      (when (and (eql status 401) (plusp (length token)) (not (equal token session)))
        (setf wire-key token)
        (multiple-value-setq (octets status content-type)
          (unary base +auth-path+ (encode-metadata-request (wire-metadata token)))))
      (unless (ok-status-p status)
        (error (http-error "auth" status octets content-type)))
      (destructuring-bind (&optional jwt url) (unary-message #'decode-user-jwt-response octets)
        (unless (plusp (length (or jwt "")))
          (error 'nle::provider-error :scope :request
                                      :detail "Devin auth error: GetUserJwt returned an empty user JWT"))
        (let ((url (nlk:trimmed (or url ""))))
          (values jwt (if (plusp (length url)) (string-right-trim "/" url) base) wire-key))))))

;;; --- the history as ChatMessagePrompts -----------------------------------------------------

(defun message-images (content)
  "The images CONTENT's parts carry, as encoded ImageData: base64 data: URIs only."
  (loop for part in (nle::message-content-parts content)
        when (equal "image_url" (nle::content-part-type part))
          append (multiple-value-bind (media-type data)
                     (nle::parse-data-uri (nlk:json-value part :string "image_url" "url"))
                   (and media-type (list (encode-image data media-type))))))

(defun message-text (content)
  "CONTENT's text parts, joined with nothing between them."
  (nle::content-text content))

(defun user-prompt (message message-id)
  "A user (or developer) MESSAGE as a USER prompt with its images (buildUserPrompt)."
  (let ((content (gethash "content" message)))
    (encode-chat-prompt :message-id message-id :source +chat-source-user+
                        :prompt (message-text content) :images (message-images content))))

(defun tool-call-prompts (message)
  "MESSAGE's tool calls as encoded ChatToolCalls, their arguments re-serialized."
  (loop for call across (or (nlk:json-value message :array "tool_calls") #())
        collect (multiple-value-bind (name input) (nle::tool-call-function-input call)
                  (encode-tool-call (or (nlk:json-value call :string "id") "") name
                                    (nlk:encode-json-object input)))))

;;; --- which model wrote a message ---------------------------------------------------------
;;; omp replays a message's thinking, its signature and the id Cascade gave it
;;; only to the model that wrote it (isNativeDevinMessage). The organism's
;;; history replays an assistant message verbatim to whatever lane a later
;;; round rides, so the cell adds no field of its own to it: what it needs is
;;; kept here, keyed by what the stored message carries and replays
;;; unchanged. In this process only: after a restart a message's author is
;;; unknown (README, Gaps).

(defvar *authors* (make-hash-table :test #'equal :synchronized t)
  "A message key (MESSAGE-KEY) -> (MODEL-ID . CASCADE-MESSAGE-ID).")

(defparameter +authors-limit+ 4096
  "The most messages whose author is kept; past it the table starts over.")

(defun message-key (message)
  "The digest of what an assistant MESSAGE carries and replays unchanged: its
text, its reasoning and its tool-call ids."
  (nlk:sha256-text
   (format nil "~a~c~a~c~{~a~^,~}"
           (nle::message-content message) (code-char 0)
           (or (nlk:json-value message :string "reasoning_content") "") (code-char 0)
           (loop for call across (or (nlk:json-value message :array "tool_calls") #())
                 collect (or (nlk:json-value call :string "id") "")))))

(defun remember-author (message model-id message-id)
  "Keep that MODEL-ID wrote MESSAGE, and the id Cascade gave it."
  (when (>= (hash-table-count *authors*) +authors-limit+)
    (clrhash *authors*))
  (setf (gethash (message-key message) *authors*) (cons model-id message-id)))

(defun message-author (message)
  "(values MODEL-ID CASCADE-MESSAGE-ID) of the round that wrote MESSAGE in
this process, or NIL."
  (let ((author (gethash (message-key message) *authors*)))
    (values (car author) (cdr author))))

(defun forget-authors ()
  "Forget every message's author."
  (clrhash *authors*))

(defun assistant-prompt (message index cascade-id model-id)
  "An assistant MESSAGE as a SYSTEM prompt, or NIL when it carries nothing:
its thinking and signature replay only to the model that made them; another
model's thinking is demoted to text ahead of the answer."
  (multiple-value-bind (author cascade-message-id) (message-author message)
    (assistant-prompt-of message index cascade-id (equal author model-id) cascade-message-id)))

(defun assistant-prompt-of (message index cascade-id native cascade-message-id)
  "ASSISTANT-PROMPT's body: NATIVE whether this round's model wrote MESSAGE,
CASCADE-MESSAGE-ID the id Cascade gave it."
  (let* ((thinking (or (nlk:json-value message :string "reasoning_content") ""))
         (text (message-text (gethash "content" message)))
         (prompt (if (and (not native) (plusp (length (string-trim '(#\Space #\Tab #\Newline) thinking))))
                     (format nil "~a~%~a" thinking text)
                     text))
         (thinking (if native thinking ""))
         (signature (or (and native (nlk:json-value message :string "reasoning_signature")) ""))
         (calls (tool-call-prompts message)))
    (unless (and (zerop (length prompt)) (zerop (length thinking)) (zerop (length signature)) (null calls))
      (encode-chat-prompt
       :message-id (let ((id (and native cascade-message-id)))
                     (if (plusp (length (or id "")))
                         id
                         (format nil "bot-~a" (deterministic-uuid (seed cascade-id index "assistant")))))
       :source +chat-source-system+ :prompt prompt :thinking thinking :signature signature
       :tool-calls calls))))

(defun tool-prompt (message index cascade-id)
  "A tool result MESSAGE as a TOOL prompt."
  (let ((content (gethash "content" message))
        (call-id (or (nlk:json-value message :string "tool_call_id") "")))
    (encode-chat-prompt :message-id (deterministic-uuid (seed cascade-id index "tool" call-id))
                        :source +chat-source-tool+ :tool-call-id call-id
                        :prompt (message-text content) :images (message-images content))))

(defun chat-prompts (messages cascade-id model-id)
  "MESSAGES, the round's chat-shaped history, as encoded ChatMessagePrompts
(buildChatMessagePrompts). A history system message (an eviction stub) is
omp's developer message: a USER prompt."
  (loop for message across messages
        for index from 0
        for role = (nlk:json-value message :string "role")
        for prompt = (cond ((equal role "user")
                            (user-prompt message (deterministic-uuid (seed cascade-id index "user"))))
                           ((equal role "system")
                            (user-prompt message (deterministic-uuid (seed cascade-id index "developer"))))
                           ((equal role "assistant") (assistant-prompt message index cascade-id model-id))
                           ((equal role "tool") (tool-prompt message index cascade-id)))
        when prompt collect prompt))

(defun active-tail (messages)
  "How many prompts at the end of MESSAGES are the turn's own input, which no
history maintenance can shrink."
  (let ((last (and (plusp (length messages)) (nlk:json-value (aref messages (1- (length messages))) :string "role"))))
    (cond ((equal last "user") 1)
          ((equal last "system")
           (1+ (loop for index from (- (length messages) 2) downto 0
                     while (member (nlk:json-value (aref messages index) :string "role") '("user" "system")
                                   :test #'equal)
                     count t)))
          (t 0))))

;;; --- tool schemas -------------------------------------------------------------------
;;; A compact form of normalizeSchemaForGoogle, for the Gemini backend Devin
;;; routes some models to: it refuses a JSON Schema type array as an opaque
;;; invalid_argument.

(defparameter +unsupported-schema-fields+
  '("$schema" "$ref" "$defs" "definitions" "$dynamicRef" "$dynamicAnchor" "examples" "prefixItems"
    "unevaluatedProperties" "unevaluatedItems" "patternProperties" "additionalProperties"
    "propertyNames" "minItems" "maxItems" "minLength" "maxLength" "minimum" "maximum"
    "exclusiveMinimum" "exclusiveMaximum" "multipleOf" "pattern" "format" "dependencies"
    "dependentSchemas" "dependentRequired" "x-mcp-header" "deprecated" "readOnly" "writeOnly"
    "$comment")
  "The schema keywords Google's wire schemas have no field for (UNSUPPORTED_SCHEMA_FIELDS).")

(defparameter +liftable-fields+
  '("pattern" "format" "minLength" "maxLength" "minimum" "maximum" "exclusiveMinimum"
    "exclusiveMaximum" "multipleOf" "minItems" "maxItems" "examples")
  "The stripped keywords whose constraint is said in the description instead.")

(defun schema-reference (root reference)
  "The subschema REFERENCE (#/$defs/X or #/definitions/X) names in ROOT, or NIL."
  (ppcre:register-groups-bind (section name) ("^#/(\\$defs|definitions)/(.+)$" (or reference ""))
    (nlk:json-value root :object section name)))

(defun merge-under (node branch)
  "NODE with BRANCH's members it lacks laid under it."
  (when (hash-table-p branch)
    (maphash (lambda (key value)
               (unless (nth-value 1 (gethash key node))
                 (setf (gethash key node) value)))
             branch))
  node)

(defun google-schema (schema &optional (root schema) (depth 0))
  "SCHEMA as Google's JSON-Schema field takes it: references resolved, a type
list its one non-null type (null making it nullable), constants and enums as
strings, unsupported keywords dropped with their constraints said in the
description, every object naming its properties."
  (cond
    ((eq schema t) (make-hash-table :test #'equal))
    ((not (hash-table-p schema)) schema)
    ((> depth 32) (nlk:json-object "type" "object" "properties" (make-hash-table :test #'equal)))
    (t
     (let ((node (nlk:copy-json-object schema))
           (spilled '()))
       (alexandria:when-let (target (schema-reference root (nlk:json-value node :string "$ref")))
         (remhash "$ref" node)
         (merge-under node target))
       (let ((type (gethash "type" node)))
         (when (and (vectorp type) (not (stringp type)))
           (let ((kinds (remove "null" (coerce type 'list) :test #'equal)))
             (setf (gethash "type" node) (or (first kinds) "string"))
             (when (find "null" type :test #'equal)
               (setf (gethash "nullable" node) t)))))
       (multiple-value-bind (constant present) (gethash "const" node)
         (when present
           (remhash "const" node)
           (when (stringp constant) (setf (gethash "enum" node) (vector constant)))))
       (alexandria:when-let (enum (nlk:json-value node :array "enum"))
         (let ((strings (remove-if-not #'stringp enum)))
           (if (plusp (length strings))
               (setf (gethash "enum" node) (coerce strings 'vector))
               (remhash "enum" node))))
       (when (and (gethash "enum" node) (not (gethash "type" node)))
         (setf (gethash "type" node) "string"))
       (dolist (key +unsupported-schema-fields+)
         (multiple-value-bind (value present) (gethash key node)
           (when present
             (when (member key +liftable-fields+ :test #'equal)
               (push (cons key value) spilled))
             (remhash key node))))
       (when spilled
         (let ((said (format nil "{~{~a~^, ~}}"
                             (mapcar (lambda (pair) (format nil "~a: ~a" (car pair) (nlk:encode-json-object (cdr pair))))
                                     (nreverse spilled))))
               (existing (nlk:json-value node :text "description")))
           (setf (gethash "description" node)
                 (if existing (format nil "~a~%~%~a" existing said) said))))
       (alexandria:when-let (properties (nlk:json-value node :object "properties"))
         (let ((normalized (make-hash-table :test #'equal)))
           (maphash (lambda (name property)
                      (setf (gethash name normalized) (google-schema property root (1+ depth))))
                    properties)
           (setf (gethash "properties" node) normalized)))
       (let ((items (gethash "items" node)))
         (cond ((hash-table-p items) (setf (gethash "items" node) (google-schema items root (1+ depth))))
               ((and (vectorp items) (not (stringp items)))
                (setf (gethash "items" node)
                      (if (plusp (length items))
                          (google-schema (aref items 0) root (1+ depth))
                          (make-hash-table :test #'equal))))))
       (dolist (key '("anyOf" "oneOf" "allOf"))
         (alexandria:when-let (branches (nlk:json-value node :array key))
           (setf (gethash key node)
                 (map 'vector (lambda (branch) (google-schema branch root (1+ depth))) branches))))
       (when (equal "object" (gethash "type" node))
         (unless (nlk:json-value node :object "properties")
           (setf (gethash "properties" node) (make-hash-table :test #'equal)))
         (alexandria:when-let (required (nlk:json-value node :array "required"))
           (let ((named (remove-if-not (lambda (name) (nth-value 1 (gethash name (gethash "properties" node))))
                                       required)))
             (if (plusp (length named))
                 (setf (gethash "required" node) (coerce named 'vector))
                 (remhash "required" node)))))
       node))))

(defun gemini-p (model-id uid)
  "Whether the round lands on Devin's Gemini backend: the model or the uid is
a Gemini, or the uid is in the server's own MODEL_GOOGLE_GEMINI_ namespace."
  (or (search "gemini" (string-downcase (or model-id "")))
      (search "gemini" (string-downcase (or uid "")))
      (uiop:string-prefix-p "MODEL_GOOGLE_GEMINI_" (or uid ""))))

(defun tool-definitions (tools google)
  "The round's TOOLS (chat-shaped wrappers) as encoded ChatToolDefinitions;
GOOGLE normalizes each schema for the Gemini backend first."
  (map 'list (lambda (wrapper)
               (let* ((function (gethash "function" wrapper))
                      (schema (or (nlk:json-value function :object "parameters") (nlk:json-object "type" "object"))))
                 (encode-tool-definition (nlk:json-value function :string "name")
                                         (or (nlk:json-value function :string "description") "")
                                         (nlk:encode-json-object (if google (google-schema schema) schema)))))
       (or tools #())))

;;; --- AssignModel ---------------------------------------------------------------------------

(defun router-prompt (messages)
  "The prompt a router scores: the newest user (or developer) turn alone, with no message id."
  (loop for index from (1- (length messages)) downto 0
        for message = (aref messages index)
        when (member (nlk:json-value message :string "role") '("user" "system") :test #'equal)
          return (user-prompt message "")))

(defun assign-model (base wire-key router-uid cascade-id messages)
  "(values ASSIGNMENT-JWT MODEL-UID) the router ROUTER-UID resolves to for this
cascade (assignDevinModel); a failed assignment fails the round, never
sending the router's own uid to GetChatMessage."
  (multiple-value-bind (octets status content-type)
      (unary base +assign-path+
             (encode-assign-model-request :metadata (wire-metadata wire-key) :router-uid router-uid
                                          :cascade-id cascade-id :prompt (router-prompt messages)))
    (unless (ok-status-p status)
      (error (http-error "AssignModel" status octets content-type)))
    (destructuring-bind (&optional jwt uid) (unary-message #'decode-assign-model-response octets)
      (unless (and (plusp (length (or jwt ""))) (plusp (length (or uid ""))))
        (error 'nle::provider-error :scope :request
                                    :detail "Devin AssignModel error: response carried no assignment JWT and model uid"))
      (values jwt uid))))

;;; --- GetChatMessage ------------------------------------------------------------------------

(defun cascade-id (context)
  "The Cascade thread this round belongs to: a UUID derived from the session,
so every round of a session shares it, else a fresh one."
  (declare (ignore context))
  (let ((session (getf (nle:turn) :session-id)))
    (if session (deterministic-uuid (format nil "nodecode-devin~c~a" (code-char 0) session)) (uuid))))

(defun chat-request (context &key spec model-id uid wire-key jwt cascade-id assignment-jwt
                                  (execution-id (uuid)) (redact nil))
  "(values REQUEST PROMPTS) of the round's GetChatMessageRequest
(buildDevinChatRequest): the encoded bytes and the encoded history prompts.
REDACT leaves the key and the JWT out, the form a round keeps."
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (messages (coerce (nle::request-messages context) 'vector))
         (prompts (chat-prompts messages cascade-id model-id)))
    (values
     (encode-chat-request
      :metadata (if redact (wire-metadata "" "") (wire-metadata wire-key jwt))
      :prompt (nle::compiled-turn-context-system-prompt context)
      :prompts prompts
      :chat-model-uid uid
      :request-type +request-type-cascade+
      :configuration (encode-completion-configuration
                      :max-tokens (or (nle::effective-max-output-tokens context) (getf spec :output) 64000)
                      :max-newlines 200
                      :temperature (or (nle::effective-provider-config-temperature config) 0.4d0)
                      :first-temperature (or (nle::effective-provider-config-temperature config) 0.4d0)
                      :top-k 50
                      :top-p (or (nle::effective-provider-config-top-p config) 1)
                      :stop-patterns +stop-patterns+
                      :fim-eot-threshold 1)
      :tools (tool-definitions (nle::compiled-turn-context-tools context) (gemini-p model-id uid))
      :disable-parallel-tool-calls (not (getf spec :parallel))
      :tool-choice (encode-tool-choice "auto")
      :system-cache-options (encode-cache-options +cache-control-ephemeral+)
      :cascade-id cascade-id
      :planner-mode +planner-mode-default+
      :execution-id execution-id
      :model-assignment-jwt assignment-jwt)
     prompts)))

(defun history-bytes (prompts tail)
  "The bytes a request carrying PROMPTS but their last TAIL encodes to."
  (length (encode-chat-request :prompts (butlast prompts tail))))

(defun post-stream (url body seconds evidence)
  "(values STREAM STATUS) of the streaming POST of BODY to URL, its response
an octet stream; a refusal is a request-scope PROVIDER-ERROR."
  (handler-case
      (multiple-value-bind (stream status headers)
          (nlk:with-cancellable-wait (nle::*current-durable-turn*)
            (sb-sys:with-deadline (:seconds seconds)
              (dex:post url :headers +stream-headers+ :content body
                            :want-stream t :force-binary t :use-connection-pool nil
                            :connect-timeout seconds :read-timeout (* 4 seconds))))
        (unless (ok-status-p status)
          ;; a refusal dexador answered rather than signalled
          (let ((condition (http-error "API" status (body-octets stream) (header-value headers "content-type"))))
            (setf (nle::provider-error-evidence-request-body condition) (nle::bounded-evidence evidence))
            (ignore-errors (close stream))
            (error condition)))
        (values stream status))
    (nlk:turn-cancelled-condition (condition) (error condition))
    (nle::provider-error (condition) (error condition))
    (dex:http-request-failed (e)
      (let ((condition (http-error "API" (dex:response-status e)
                                   (body-octets (ignore-errors (dex:response-body e)))
                                   (header-value (ignore-errors (dex:response-headers e)) "content-type"))))
        (setf (nle::provider-error-evidence-request-body condition) (nle::bounded-evidence evidence))
        (error condition)))
    (sb-sys:deadline-timeout ()
      (error 'nle::provider-error :scope :request :detail (format nil "no response for ~a s" seconds)
                                  :evidence-request-body (nle::bounded-evidence evidence)))
    (usocket:connection-refused-error (e)
      (error 'nle::provider-connection-refused
             :detail (format nil "~a — is the server running?" (nle::transport-failure-label e url))))
    (error (e)
      (error 'nle::provider-error :scope :request :detail (nle::transport-failure-label e url)))))

(defun next-frame (stream seconds)
  "(values FLAGS PAYLOAD) of the next frame on STREAM within SECONDS, or NIL
at its end; a cut, a silence or a broken frame is PROVIDER-STREAM-INCOMPLETE."
  (handler-case
      (nlk:with-cancellable-wait (nle::*current-durable-turn*)
        (sb-sys:with-deadline (:seconds seconds)
          (read-connect-frame stream)))
    (nlk:turn-cancelled-condition (condition) (error condition))
    (sb-sys:deadline-timeout ()
      (error 'nle::provider-stream-incomplete :detail (format nil "no byte for ~a s" seconds)))
    (proto-error (e)
      (error 'nle::provider-stream-incomplete :detail (proto-error-text e)))
    (error (e)
      (error 'nle::provider-stream-incomplete :detail (nle::stream-cut-detail e)))))

(defstruct (fold (:copier nil))
  "What one round's stream has said so far."
  asm
  (calls '())          ; (ID INDEX . ARGUMENTS) per tool call, newest first
  (active nil)         ; the id a delta without one continues
  (signature nil)      ; the thinking signature, the last one sent
  (message-id nil)
  (stop 0)
  (usage nil)
  (output-p nil))

(defun fold-call (fold call)
  "Fold one streamed tool CALL delta into FOLD: a new id opens a call, a
name names it, arguments arrive whole-so-far or as a suffix."
  (let* ((asm (fold-asm fold))
         (id (if (plusp (length (getf call :id))) (getf call :id) (fold-active fold))))
    (when id
      (let ((entry (assoc id (fold-calls fold) :test #'equal)))
        (unless entry
          (let ((index (length (fold-calls fold))))
            (nle::open-tool-buffer asm index :id id :name (getf call :name) :arguments "")
            (push (setf entry (list* id index "")) (fold-calls fold))))
        (let* ((index (second entry))
               (buffer (cdr (assoc index (nle::lane-assembly-tool-buffers asm)))))
          (when (plusp (length (getf call :name)))
            (setf (getf buffer :name) (getf call :name)))
          (setf (fold-active fold) id)
          (let* ((delta (getf call :arguments))
                 (previous (cddr entry)))
            (if (plusp (length delta))
                (let* ((accumulated (if (uiop:string-prefix-p previous delta)
                                        delta
                                        (concatenate 'string previous delta)))
                       (fragment (subseq accumulated (length previous))))
                  (setf (cddr entry) accumulated
                        (getf buffer :arguments) accumulated)
                  (nle::lifecycle-tool-progress asm index buffer fragment))
                ;; a name that arrived after the call opened starts its bracket
                (nle::lifecycle-tool-progress asm index buffer nil))))))))

(defun fold-response (fold response on-part)
  "Fold one GetChatMessageResponse into FOLD, as omp's stream walk does."
  (let ((asm (fold-asm fold)))
    (when (and (plusp (length (getf response :message-id))) (null (fold-message-id fold)))
      (setf (fold-message-id fold) (getf response :message-id))
      (nle::emit-stream-part on-part :response-metadata :id (getf response :message-id)
                                                        :model (getf response :actual-model-uid)))
    (let ((thinking (getf response :delta-thinking)))
      (when (plusp (length thinking))
        (setf (fold-output-p fold) t)
        (nle::assembly-reasoning-delta asm "reasoning-0" thinking :close-text t)
        ;; only a frame that thinks says the signature, the last one standing
        (when (plusp (length (getf response :delta-signature)))
          (setf (fold-signature fold) (getf response :delta-signature)))))
    (let ((text (getf response :delta-text)))
      (when (plusp (length text))
        (setf (fold-output-p fold) t)
        (nle::assembly-text-delta asm "text-0" text :close-reasoning t)))
    (when (getf response :tool-calls)
      (setf (fold-output-p fold) t)
      (nle::lifecycle-close-text asm)
      (nle::lifecycle-close-reasoning asm)
      (dolist (call (getf response :tool-calls))
        (fold-call fold call)))
    (unless (zerop (getf response :stop-reason))
      (setf (fold-stop fold) (getf response :stop-reason)))
    (alexandria:when-let (usage (getf response :usage))
      (setf (fold-usage fold) usage))))

(defun fold-usage-struct (usage)
  "Cascade's usage as the organism counts it: input apart from the cache
reads, the total every count summed (omp's totalTokens)."
  (when usage
    (destructuring-bind (&key input output cache-read cache-write) usage
      (nle::make-provider-usage :input-tokens input :output-tokens output
                                :cached-input-tokens cache-read :cache-write-tokens cache-write
                                :total-tokens (+ input output cache-read cache-write)))))

(defun trailer-condition (trailer fold prompts tail)
  "The PROVIDER-ERROR an end-of-stream TRAILER is. An opaque invalid_argument
before any output over a history past +LARGE-HISTORY-BYTES+ is said as a
context overflow, so the turn's eviction runs (omp flags ContextOverflow)."
  (let* ((code (string-downcase (getf trailer :code)))
         (overflow (and (not (fold-output-p fold))
                        (equal code "invalid_argument")
                        (ppcre:scan "(?i)\\binternal error\\b" (getf trailer :message))
                        (>= (history-bytes prompts tail) +large-history-bytes+))))
    (make-condition 'nle::provider-error
                    :status (cdr (assoc code +connect-statuses+ :test #'equal))
                    :scope :stream
                    :detail (format nil "~a~:[~; (a large history: treated as a context overflow)~]"
                                    (getf trailer :formatted) overflow)
                    :evidence-body (getf trailer :raw))))

(defun stream-round (context &key (on-part nle::*turn-part-fn*))
  "The lane's stream: one Cascade round over CONTEXT, its answer folded into
the organism's assistant message. => (values MESSAGE USAGE FINISH-REASON REQUEST)"
  (let* ((config (nle::compiled-turn-context-provider-config context))
         (model-id (nle::effective-provider-config-model config))
         (token (nle::effective-provider-config-api-key config))
         (base (string-right-trim "/" (nle::effective-provider-config-endpoint config)))
         (seconds (nle::effective-provider-config-request-timeout config)))
    (when (or (zerop (length token)) (equal token "public"))
      (error 'nle::provider-config-error
             :status 401
             :detail (format nil "Devin is not signed in: run /~a login, or set DEVIN_API_KEY" +provider+)))
    ;; a model the seeds do not name is looked up in the account's roster first
    (unless (or (model-spec model-id) *discovery-tried*)
      (handler-case (discover token base)
        (nlk:turn-cancelled-condition (condition) (error condition))
        (error () nil)))
    (let* ((spec (or (model-spec model-id) (list :id model-id)))
           (uid (wire-uid spec model-id (nle::effective-provider-config-reasoning-effort config)))
           (cascade (cascade-id context))
           (messages (coerce (nle::request-messages context) 'vector)))
      (multiple-value-bind (jwt chat-base wire-key) (user-jwt token base)
        (multiple-value-bind (assignment assigned)
            (when (getf spec :router)
              (assign-model chat-base wire-key (or (getf spec :wire-id) model-id) cascade messages))
          (let* ((uid (or assigned uid))
                 (execution (uuid))
                 (arguments (list :spec spec :model-id model-id :uid uid :cascade-id cascade
                                  :assignment-jwt assignment :execution-id execution))
                 (kept (apply #'chat-request context :redact t arguments)))
            (multiple-value-bind (request prompts)
                (apply #'chat-request context :wire-key wire-key :jwt jwt arguments)
              (let ((stream (post-stream (concatenate 'string chat-base +chat-path+)
                                         (connect-frame (gzip request) +compressed-flag+)
                                         seconds kept))
                    (fold (make-fold :asm (nle::make-lane-assembly :on-part on-part))))
                (nle::emit-stream-part on-part :stream-start)
                (unwind-protect
                     (handler-bind ((nle::provider-error
                                      (lambda (condition)
                                        (nle::attach-provider-evidence condition "" kept))))
                       (loop
                         (when nle::*current-durable-turn*
                           (nlk:ensure-turn-not-cancelled nle::*current-durable-turn*))
                         (multiple-value-bind (flags payload) (next-frame stream seconds)
                           (unless flags (return))
                           (let ((payload (if (logtest flags +compressed-flag+)
                                              (handler-case (gunzip payload)
                                                (proto-error (e)
                                                  (error 'nle::provider-stream-incomplete
                                                         :detail (proto-error-text e))))
                                              payload)))
                             (if (logtest flags +end-stream-flag+)
                                 (progn
                                   (alexandria:when-let (trailer (trailer-error (ignore-errors (utf8-text payload))))
                                     (nle::emit-stream-part on-part :error :payload (getf trailer :formatted))
                                     (error (trailer-condition trailer fold prompts (active-tail messages))))
                                   (return))
                                 (fold-response fold
                                                (handler-case (decode-chat-response payload)
                                                  (proto-error (e)
                                                    (error 'nle::provider-stream-incomplete
                                                           :detail (format nil "a GetChatMessageResponse did not decode: ~a"
                                                                           (proto-error-text e)))))
                                                on-part))))))
                  (nle::close-provider-stream-body stream))
                (finish-round fold model-id on-part kept)))))))))

(defun finish-round (fold model-id on-part kept)
  "Close FOLD's spans and answer the round's values, as DEFINE-PROVIDER-LANE does."
  (let ((asm (fold-asm fold)))
    ;; a call whose arguments never arrived is a call with none
    (loop for (nil index . arguments) in (fold-calls fold)
          when (zerop (length (string-trim '(#\Space #\Tab #\Newline) arguments)))
            do (setf (getf (cdr (assoc index (nle::lane-assembly-tool-buffers asm))) :arguments) "{}"))
    (nle::assembly-close-spans asm :order '(:text :reasoning :tools))
    (nle::flush-thinking-tag asm)
    (alexandria:when-let (signature (fold-signature fold))
      (nle::assembly-signature-delta asm signature))
    (let* ((tool-buffers (sort (copy-list (nle::lane-assembly-tool-buffers asm)) #'< :key #'car))
           (message
             (nlk:json-object
              "role" "assistant"
              "content" (let ((full (get-output-stream-string (nle::lane-assembly-content asm))))
                          (if (string= full "") :null full))
              :when (nle::lane-assembly-reasoning-seen-p asm) "reasoning_content"
              (get-output-stream-string (nle::lane-assembly-reasoning asm))
              :when (fold-signature fold) "reasoning_signature" (fold-signature fold)
              :when tool-buffers "tool_calls"
              (map 'vector (lambda (pair &aux (buffer (cdr pair)))
                             (nle::chat-tool-call-object (or (getf buffer :id) "")
                                                         (getf buffer :name) (getf buffer :arguments)))
                   tool-buffers)))
           (finish (cond (tool-buffers "tool_calls")
                         ((= (fold-stop fold) +stop-reason-max-tokens+) "length")
                         (t "stop"))))
      ;; no field of the cell's own rides the message: what replays it
      ;; natively next round is kept beside it
      (remember-author message model-id (fold-message-id fold))
      (nle::emit-stream-part on-part :finish)
      (values message (fold-usage-struct (fold-usage fold)) finish kept))))

;;; --- GetCliModelConfigs ----------------------------------------------------------------------

(defun refusal-code (octets)
  "The machine-readable reason a refusal's body OCTETS names, or NIL: the
core's reading of a JSON error body, else a Connect error's own `code'."
  (let ((text (ignore-errors (sb-ext:octets-to-string (octets octets) :external-format :utf-8))))
    (and text
         (or (nle::error-body-code text)
             (let ((code (nlk:json-value (ignore-errors (nlk:decode-json text)) :string "code")))
               (and code (not (find #\Space code)) (plusp (length code)) code))))))

(defun fetch-configs (base metadata)
  "The configs GetCliModelConfigs answers the identity METADATA at BASE, or
(values NIL REASON) when it does not answer: a refusal as `HTTP <status>
<code>', the reason /connect's key check reads, a transport failure as its
label."
  (multiple-value-bind (octets status)
      (handler-case (unary base +models-path+ (encode-metadata-request metadata) :timeout +discovery-seconds+)
        (nle::provider-error (e) (values nil (nle::provider-error-detail e))))
    (cond ((not (integerp status)) (values nil (or status "no answer")))
          ((not (ok-status-p status))
           (values nil (format nil "HTTP ~d~@[ ~a~]" status (refusal-code octets))))
          (t (let ((decoded (unary-message #'decode-model-configs-response octets)))
               (if decoded
                   (values (normalize-configs (first decoded)) nil)
                   (values nil "the model configs did not decode")))))))

(defun seed-only-p (specs)
  "Whether SPECS are the two-row fallback seed a legacy seat is answered with."
  (and specs (every (lambda (spec) (member (getf spec :id) '("swe-1-6" "swe-1-6-fast") :test #'equal)) specs)))

(defun fetch-models (key base)
  "The account's roster for KEY at BASE, as specs, or (values NIL REASON)
(fetchDevinModels): the native chisel identity first; when it answers
nothing but the seeds, the legacy Windsurf identity, the longer roster kept."
  (multiple-value-bind (native reason) (fetch-configs base (discovery-metadata key))
    (if (and native (not (seed-only-p native)))
        native
        (multiple-value-bind (legacy legacy-reason) (fetch-configs base (legacy-metadata key))
          (let ((models (if (and legacy (or (null native) (> (length legacy) (length native)))) legacy native)))
            (if models
                models
                (values nil (or reason legacy-reason "Devin returned an empty model catalog"))))))))

(defun discover (key base)
  "Ask the account's roster for KEY at BASE and keep it for the catalog:
the specs, or (values NIL REASON)."
  (setf *discovery-tried* t)
  (multiple-value-bind (specs reason) (fetch-models key base)
    (when specs
      (setf *discovered* specs)
      (forget-catalog))
    (values specs reason)))
