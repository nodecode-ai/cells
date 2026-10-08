;;;; wire.lisp --- the Cloud Code Assist request, and its stream as Gemini frames.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Ported from oh-my-pi (see NOTICE): ai/src/providers/google-gemini-cli.ts
;;;; (buildRequest, the stream walk, the planning-leak buffer), the message
;;;; and tool conversion of ai/src/providers/google-shared.ts, the effort
;;;; mapping of ai/src/stream.ts (mapOptionsForApi, google-gemini-cli) and
;;;; catalog/src/model-thinking.ts, and a compact form of ai/src/utils/schema/
;;;; normalize.ts's two Google normalizers. Pure transforms, no I/O.
;;;;
;;;; A request is the Gemini generateContent body wrapped with the account's
;;;; project and the model the request names:
;;;;   {"project": P, "model": M, "request": {contents, systemInstruction,
;;;;    tools, toolConfig, generationConfig}}
;;;; and every streamed event is {"response": <a Gemini chunk>, "traceId": T},
;;;; or {"error": {code, message, status}} when the stream fails in band. The
;;;; organism's own Gemini fold (the google lane) folds the unwrapped chunks;
;;;; this file builds the request from the organism's history the way omp
;;;; converts its own, and readies each chunk first: the error and the
;;;; blocked prompt said, the planning text Flash models leak held back, the
;;;; signatures each block arrived with kept for the next round.
;;;;
;;;; The next round replays what the last one signed. The organism's message
;;;; keeps one reasoning text and one answer text, and the google fold keeps
;;;; each call's signature on it (`thought_signatures'). The thought's and
;;;; the answer's signatures, and the model that made them (a signature is
;;;; only replayed to its own model), are kept here, in a table keyed by the
;;;; message's session and contents, and never on the message: another lane
;;;; sends a message's fields as they are, and a strict API refuses one it
;;;; does not know.

(in-package #:nodecode-google-gemini-cli)

(defparameter +skip-signature+ "skip_thought_signature_validator"
  "The sentinel an unsigned function call carries where Cloud Code Assist
requires a signature.")

(defparameter +non-vision-placeholder+ "[image omitted: model does not support vision]")

(defparameter +google-thinking+
  '(("minimal" . 1024) ("low" . 4096) ("medium" . 8192) ("high" . 16384) ("xhigh" . 24575) ("max" . 32768))
  "omp's default thinking budget per effort, for a model that names none.")

(defparameter +min-output-tokens+ 1024
  "The answer room a thinking budget leaves inside the output ceiling.")

(defparameter +output-cap-when-unknown+ 64000
  "The output ceiling when neither the round nor the model names one.")

(defstruct (cca-round (:conc-name round-) (:constructor make-round) (:copier nil))
  "What one Cloud Code Assist round knows and what its stream has said."
  provider model facts token project email (tool-names '())
  ;; the planning-leak buffer
  (buffering nil) (buffer "") (buffered-signature nil)
  ;; the block the stream is in, and the signature each kind of block carried
  (block nil) (thinking-signature nil) (text-signature nil)
  (finished nil) (response-id nil))

(defvar *round* nil
  "The Cloud Code Assist round running on this thread, or NIL.")

(defun whitespace-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return #\Page #\No-break_space)))

(defun trim-start (text)
  (string-left-trim '(#\Space #\Tab #\Newline #\Return #\Page #\No-break_space) text))

(defun blank-p (text)
  "Whether TEXT is NIL or only whitespace."
  (or (null text) (every #'whitespace-p text)))

;;; --- tool schemas -------------------------------------------------------------------

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

(defun merge-branches (node branches)
  "NODE with BRANCHES' members laid under it: properties and required joined,
every other member the first one that names it."
  (dolist (branch branches node)
    (when (hash-table-p branch)
      (maphash (lambda (key value)
                 (cond ((and (equal key "properties") (hash-table-p value))
                        (let ((properties (or (nlk:json-value node :object "properties")
                                              (setf (gethash "properties" node) (make-hash-table :test #'equal)))))
                          (maphash (lambda (name schema)
                                     (unless (nth-value 1 (gethash name properties))
                                       (setf (gethash name properties) schema)))
                                   value)))
                       ((and (equal key "required") (vectorp value))
                        (setf (gethash "required" node)
                              (coerce (remove-duplicates (concatenate 'list (nlk:json-array node "required") value)
                                                         :test #'equal :from-end t)
                                      'vector)))
                       ((not (nth-value 1 (gethash key node)))
                        (setf (gethash key node) value))))
               branch))))

(defun normalize-schema (schema mode &optional (root schema) (depth 0))
  "SCHEMA as Google's wire takes a tool's parameters: MODE :GOOGLE for the
JSON-Schema field (parametersJsonSchema), :CCA for the legacy `parameters'
field Claude on Cloud Code Assist reads, which takes no combiner, no type
list and no nullable."
  (cond
    ((eq schema t) (make-hash-table :test #'equal))
    ((not (hash-table-p schema)) schema)
    ((> depth 32) (nlk:json-object "type" "object" "properties" (make-hash-table :test #'equal)))
    (t
     (let ((node (nlk:copy-json-object schema))
           (spilled '()))
       ;; a reference is the schema it names, the node's own members over it
       (alexandria:when-let (target (schema-reference root (nlk:json-value node :string "$ref")))
         (remhash "$ref" node)
         (merge-branches node (list target)))
       ;; combiners: kept for the JSON-Schema field, collapsed for the legacy one
       (when (eq mode :cca)
         (alexandria:when-let (all (nlk:json-value node :array "allOf"))
           (remhash "allOf" node)
           (merge-branches node (map 'list (lambda (branch) (normalize-schema branch mode root (1+ depth))) all)))
         (dolist (key '("anyOf" "oneOf"))
           (alexandria:when-let (branches (nlk:json-value node :array key))
             (remhash key node)
             (let ((kept (remove-if (lambda (branch) (equal "null" (nlk:json-value branch :string "type")))
                                    (map 'list (lambda (branch) (normalize-schema branch mode root (1+ depth)))
                                         branches))))
               (cond ((every (lambda (branch) (equal "object" (nlk:json-value branch :string "type"))) kept)
                      (merge-branches node kept))
                     (t (merge-branches node (list (first kept))))))))
         (remhash "not" node))
       ;; a type list is its one non-null type; null makes it nullable
       (let ((type (gethash "type" node)))
         (when (and (vectorp type) (not (stringp type)))
           (let ((kinds (remove "null" (coerce type 'list) :test #'equal)))
             (setf (gethash "type" node) (or (first kinds) "string"))
             (when (and (find "null" type :test #'equal) (eq mode :google))
               (setf (gethash "nullable" node) t)))))
       (when (eq mode :cca) (remhash "nullable" node))
       ;; a constant is a one-member enum; Google's enums are strings
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
       ;; what Google has no field for goes; a constraint goes into the description
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
       ;; the subschemas
       (alexandria:when-let (properties (nlk:json-value node :object "properties"))
         (let ((normalized (make-hash-table :test #'equal)))
           (maphash (lambda (name property)
                      (setf (gethash name normalized) (normalize-schema property mode root (1+ depth))))
                    properties)
           (setf (gethash "properties" node) normalized)))
       (let ((items (gethash "items" node)))
         (cond ((hash-table-p items) (setf (gethash "items" node) (normalize-schema items mode root (1+ depth))))
               ((and (vectorp items) (not (stringp items)))
                (setf (gethash "items" node)
                      (if (plusp (length items))
                          (normalize-schema (aref items 0) mode root (1+ depth))
                          (make-hash-table :test #'equal))))))
       (dolist (key '("anyOf" "oneOf" "allOf"))
         (alexandria:when-let (branches (nlk:json-value node :array key))
           (setf (gethash key node)
                 (map 'vector (lambda (branch) (normalize-schema branch mode root (1+ depth))) branches))))
       ;; an object always names its properties, and requires only those
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

(defun cca-tools (tools facts)
  "The round's TOOLS (chat-shaped wrappers) as Gemini function declarations:
the JSON-Schema field, or the legacy one where FACTS say the model reads it."
  (let ((legacy (compat facts "legacy_parameters")))
    (vector
     (nlk:json-object
      "functionDeclarations"
      (map 'vector
           (lambda (wrapper)
             (let* ((function (gethash "function" wrapper))
                    (schema (or (nlk:json-value function :object "parameters")
                                (nlk:json-object "type" "object"))))
               (if legacy
                   (nlk:json-object "name" (gethash "name" function)
                                    "description" (or (nlk:json-value function :string "description") "")
                                    "parameters" (normalize-schema schema :cca))
                   (nlk:json-object "name" (gethash "name" function)
                                    "description" (or (nlk:json-value function :string "description") "")
                                    "parametersJsonSchema" (normalize-schema schema :google)))))
           tools)))))

;;; --- thinking --------------------------------------------------------------------------

(defun model-efforts (facts)
  "The efforts FACTS's model takes, weakest first."
  (sort (remove-if-not #'nle::effort-rank (coerce (nlk:json-array facts "thinking" "efforts") 'list))
        #'< :key #'nle::effort-rank))

(defun supported-effort (asked efforts)
  "ASKED as one of EFFORTS: itself, else the strongest below it, else the weakest."
  (cond ((member asked efforts :test #'string-equal) (find asked efforts :test #'string-equal))
        ((null efforts) asked)
        (t (or (find-if (lambda (effort) (< (nle::effort-rank effort) (or (nle::effort-rank asked) 0)))
                        efforts :from-end t)
               (first efforts)))))

(defun routed-model (facts effort)
  "The model id a request at EFFORT names (NIL for thinking off): the
effort's routed id, else the requested id, else the model's own."
  (or (nlk:json-value facts :text "thinking" "routing" (or effort "off"))
      (nlk:json-value facts :text "request_model_id")
      (nlk:json-value facts :string "id")))

(defun default-effort (facts)
  "The effort a model that cannot think off runs at when none is asked: the
one whose route is its requested id, else its weakest."
  (let ((efforts (model-efforts facts))
        (requested (nlk:json-value facts :text "request_model_id")))
    (or (and requested
             (find-if (lambda (effort) (equal requested (nlk:json-value facts :text "thinking" "routing" effort)))
                      efforts))
        (first efforts))))

(defun thinking-level (effort facts)
  "EFFORT as Google's thinkingLevel; a minimal that routes to the low id says LOW."
  (cond ((equal effort "minimal")
         (let ((minimal (nlk:json-value facts :text "thinking" "routing" "minimal")))
           (if (and minimal (equal minimal (nlk:json-value facts :text "thinking" "routing" "low")))
               "LOW"
               "MINIMAL")))
        ((equal effort "low") "LOW")
        ((equal effort "medium") "MEDIUM")
        (t "HIGH")))

(defun thinking-plan (facts effort base-tokens)
  "(values MODEL THINKING-CONFIG MAX-TOKENS) of a round at EFFORT (NIL or
`off' for none) whose answer may run to BASE-TOKENS."
  ;; mapOptionsForApi's google-gemini-cli arm and the mandatory-reasoning clamp
  (let* ((reasoning (nlk:json-value facts :boolean "reasoning"))
         (mode (nlk:json-value facts :string "thinking" "mode"))
         (suppress (nlk:json-value facts :boolean "thinking" "suppress_when_off"))
         (asked (and (stringp effort) (not (string-equal effort "off")) (string-downcase effort)))
         (asked (or asked
                    (and reasoning (nlk:json-value facts :boolean "thinking" "requires_effort") (not suppress)
                         (default-effort facts)))))
    (when (and asked reasoning)
      (let ((effort (supported-effort asked (model-efforts facts))))
        (if (equal mode "google-level")
            (return-from thinking-plan
              (values (routed-model facts effort)
                      (nlk:json-object "includeThoughts" t "thinkingLevel" (thinking-level effort facts))
                      base-tokens))
            (let* ((budget (or (nlk:json-value facts :integer "thinking" "budgets" effort)
                               (cdr (assoc effort +google-thinking+ :test #'equal))
                               (cdr (assoc "high" +google-thinking+ :test #'equal))))
                   (ceiling (nlk:json-value facts :integer "output"))
                   (total (min (if base-tokens (+ base-tokens budget) +output-cap-when-unknown+)
                               (or ceiling most-positive-fixnum))))
              (when (<= total budget)
                (setf budget (max 0 (- total +min-output-tokens+))))
              (when (plusp budget)
                (return-from thinking-plan
                  (values (routed-model facts effort)
                          (nlk:json-object "includeThoughts" t "thinkingBudget" budget)
                          total)))))))
    ;; off: Cloud Code Assist re-applies the model's own default when the
    ;; config is absent, so a model that must be told says so
    (values (routed-model facts nil)
            (and reasoning suppress
                 (if (equal mode "google-level")
                     (nlk:json-object "includeThoughts" :false "thinkingLevel" "MINIMAL")
                     (nlk:json-object "includeThoughts" :false "thinkingBudget" 0)))
            base-tokens)))

;;; --- the history ---------------------------------------------------------------------

(defun signature-p (text)
  "Whether TEXT is a thought signature Google takes: base64."
  (and (stringp text) (plusp (length text)) (zerop (mod (length text) 4))
       (ppcre:scan "^[A-Za-z0-9+/]+={0,2}$" text) t))

(defun demoted-thinking (facts text)
  "Reasoning a model cannot take back as a thought, as text it reads
(renderDemotedThinking): bare for Claude, a thinking fence for Gemini, a
think tag for the rest."
  (let ((class (nlk:json-value facts :string "class")))
    (cond ((equal class "anthropic") text)
          ((equal class "gemini") (format nil "```thinking~%~a~%```" text))
          (t (format nil "<think>~%~a~%</think>" text)))))

(defun image-part (url)
  "One Gemini image part: a data: URI inline, anything else by reference."
  (multiple-value-bind (media-type data) (nle::parse-data-uri url)
    (if media-type
        (nlk:json-object "inlineData" (nlk:json-object "mimeType" (if (equal media-type "image/*") "image/jpeg" media-type)
                                                       "data" data))
        (nlk:json-object "fileData" (nlk:json-object "fileUri" url "mimeType" "image/jpeg")))))

(defun content-parts (content images)
  "(values PARTS OMITTED) of a message's CONTENT: its non-blank texts and,
when IMAGES, its images; OMITTED when an image was left out."
  (let ((parts '()) (omitted nil))
    (if (stringp content)
        (unless (blank-p content) (push (nlk:json-object "text" content) parts))
        (dolist (part (nle::message-content-parts content))
          (let ((type (nle::content-part-type part)))
            (cond ((equal type "text")
                   (let ((text (nlk:json-value part :string "text")))
                     (unless (blank-p text) (push (nlk:json-object "text" text) parts))))
                  ((equal type "image_url")
                   (alexandria:when-let (url (nlk:json-value part :string "image_url" "url"))
                     (if images (push (image-part url) parts) (setf omitted t))))))))
    (values (nreverse parts) omitted)))

(defun wire-call-id (id used)
  "ID as a function part's id, unique among USED (a table it joins): omp's
character set and length, and a suffix where the history reused an id."
  (let* ((clean (ppcre:regex-replace-all "[^a-zA-Z0-9_-]" (or id "") "_"))
         (clean (subseq clean 0 (min 64 (length clean))))
         (clean (if (plusp (length clean)) clean "call"))
         (wire clean))
    (loop for n from 2 while (gethash wire used)
          do (setf wire (format nil "~a_~d" (subseq clean 0 (min 58 (length clean))) n)))
    (setf (gethash wire used) t)
    wire))

(defun cca-contents (messages facts provider model)
  "The chat-shaped MESSAGES as Gemini contents for MODEL at PROVIDER
(convertMessages): user text and images, the model's own turns with the
signatures it made, and every tool result of a step in one user turn."
  (let ((contents (make-array 0 :adjustable t :fill-pointer t))
        (images (nlk:json-value facts :boolean "images"))
        (part-ids (compat facts "function_part_id"))
        (multimodal (compat facts "multimodal_function_response"))
        (used (make-hash-table :test #'equal))
        (wire-ids (make-hash-table :test #'equal))
        (names (make-hash-table :test #'equal))
        (pending-images '())
        (self (format nil "~a/~a" provider model)))
    (flet ((push-content (role parts)
             (vector-push-extend (nlk:json-object "role" role "parts" (coerce parts 'vector)) contents))
           (flush-images ()
             (when pending-images
               (vector-push-extend (nlk:json-object "role" "user" "parts" (coerce (reverse pending-images) 'vector))
                                   contents)
               (setf pending-images '()))))
      (loop for message across messages
            for role = (nlk:json-value message :string "role")
            when (hash-table-p message)
              do (unless (equal role "tool") (flush-images))
                 (cond
                   ((equal role "assistant")
                    (let* ((signed (recall-signatures message))
                           (same (equal self (getf signed :model)))
                           (parts '())
                           (first-call t)
                           (reasoning (nlk:json-value message :string "reasoning_content"))
                           (text (nle::content-text (gethash "content" message))))
                      (unless (blank-p reasoning)
                        (let ((signature (and same (signature-p (getf signed :thinking)) (getf signed :thinking))))
                          (cond (signature
                                 (push (nlk:json-object "thought" t "text" reasoning "thoughtSignature" signature) parts))
                                ((compat facts "drop_unsigned_thinking"))
                                (t (push (nlk:json-object "text" (demoted-thinking facts reasoning)) parts)))))
                      (unless (blank-p text)
                        (let ((signature (getf signed :text)))
                          (push (nlk:json-object "text" text
                                                 :when (and same (signature-p signature)) "thoughtSignature" signature)
                                parts)))
                      (loop for call across (nlk:json-array message "tool_calls")
                            for id = (nlk:json-value call :string "id")
                            do (multiple-value-bind (name arguments) (nle::tool-call-function-input call)
                                 (let* ((wire (wire-call-id id used))
                                        (signature (nlk:json-value message :string "thought_signatures" (or id "")))
                                        (signature (and same (signature-p signature) signature))
                                        (fallback (or (compat facts "skip_signature")
                                                      (and first-call (compat facts "skip_signature_first_call")))))
                                   (setf (gethash (or id "") wire-ids) wire
                                         (gethash (or id "") names) name
                                         first-call nil)
                                   (push (nlk:json-object
                                          "functionCall" (nlk:json-object "name" name "args" arguments
                                                                          :when part-ids "id" wire)
                                          :opt "thoughtSignature" (or signature (and fallback +skip-signature+)))
                                         parts))))
                      (when parts (push-content "model" (nreverse parts)))))
                   ((equal role "tool")
                    (let* ((id (or (nlk:json-value message :string "tool_call_id") ""))
                           (content (gethash "content" message))
                           (text (nle::content-text content))
                           (image-parts (and images
                                             (loop for part in (nle::message-content-parts content)
                                                   when (equal (nle::content-part-type part) "image_url")
                                                     collect (image-part (nlk:json-value part :string "image_url" "url")))))
                           (omitted (and (not images)
                                         (some (lambda (part) (equal (nle::content-part-type part) "image_url"))
                                               (nle::message-content-parts content))))
                           (value (cond (omitted (format nil "~@[~a~%~]~a" (and (plusp (length text)) text)
                                                         +non-vision-placeholder+))
                                        ((plusp (length text)) text)
                                        (image-parts "(see attached image)")
                                        (t "")))
                           (part (nlk:json-object
                                  "functionResponse"
                                  (nlk:json-object "name" (or (gethash id names)
                                                              (nlk:json-value message :text "name") "")
                                                   "response" (nlk:json-object "output" value)
                                                   :when (and image-parts multimodal) "parts" (coerce image-parts 'vector)
                                                   :when part-ids "id" (or (gethash id wire-ids)
                                                                           (wire-call-id id used)))))
                           (last (and (plusp (length contents)) (aref contents (1- (length contents))))))
                      ;; every function response of a step rides one user turn
                      (if (and last (equal "user" (gethash "role" last))
                               (some (lambda (existing) (gethash "functionResponse" existing))
                                     (gethash "parts" last)))
                          (setf (gethash "parts" last) (concatenate 'vector (gethash "parts" last) (vector part)))
                          (push-content "user" (list part)))
                      (when (and image-parts (not multimodal))
                        (setf pending-images (append (reverse image-parts)
                                                     (list (nlk:json-object "text" "Tool result image:"))
                                                     pending-images)))))
                   (t
                    ;; user, and a system message inside the history (an
                    ;; eviction stub) as the user's text, as the organism's
                    ;; own Gemini lane carries it
                    (let ((content (gethash "content" message)))
                      (multiple-value-bind (parts omitted)
                          (content-parts (if (equal role "system")
                                             (concatenate 'string "[system] " (nle::content-text content))
                                             content)
                                         images)
                        (when omitted (setf parts (append parts (list (nlk:json-object "text" +non-vision-placeholder+)))))
                        (when parts (push-content "user" parts)))))))
      (flush-images))
    (coerce contents 'vector)))

;;; --- the stream ---------------------------------------------------------------------------

(defun planning-leak-prefix-p (text)
  "Whether TEXT opens the way a leaked planning object does: {\"thought\": ..."
  (let ((trimmed (trim-start text)))
    (when (and (plusp (length trimmed)) (char= #\{ (char trimmed 0)))
      (let ((after-brace (trim-start (subseq trimmed 1))))
        (cond ((zerop (length after-brace)) (<= (length trimmed) 100))
              ((char/= #\" (char after-brace 0)) nil)
              (t (let ((quote (position #\" after-brace :start 1)))
                   (if (null quote)
                       (and (uiop:string-prefix-p (subseq after-brace 1) "thought") (<= (length trimmed) 100))
                       (and (equal "thought" (subseq after-brace 1 quote))
                            (let ((after-key (trim-start (subseq after-brace (1+ quote)))))
                              (cond ((zerop (length after-key)) (<= (length trimmed) 100))
                                    (t (char= #\: (char after-key 0))))))))))))))

(defun leading-json-object (text quote-aware)
  "(values JSON REST) of the object TEXT opens with, braces balanced, strings
skipped when QUOTE-AWARE; NIL when it never closes."
  (let* ((offset (- (length text) (length (trim-start text))))
         (trimmed (subseq text offset)))
    (when (and (plusp (length trimmed)) (char= #\{ (char trimmed 0)))
      (let ((depth 0) (in-string nil) (escaped nil))
        (loop for index from 0 below (length trimmed)
              for char = (char trimmed index)
              do (cond ((and quote-aware in-string)
                        (cond (escaped (setf escaped nil))
                              ((char= char #\\) (setf escaped t))
                              ((char= char #\") (setf in-string nil))))
                       ((and quote-aware (char= char #\")) (setf in-string t))
                       ((char= char #\{) (incf depth))
                       ((char= char #\})
                        (decf depth)
                        (when (zerop depth)
                          (return-from leading-json-object
                            (values (subseq trimmed 0 (1+ index)) (subseq trimmed (1+ index))))))))))))

(defun leak-signature-p (text tool-names)
  "Whether TEXT carries a planning object's keys."
  (or (search "\"thought\"" text)
      (some (lambda (name) (search (format nil "\"~a\"" name) text)) tool-names)
      (search "\"_i\"" text) (search "\"paths\"" text) (search "\"command\"" text)
      (and (search "\"path\"" text) (search "\"content\"" text))))

(defun leak-object-p (parsed tool-names)
  "Whether the decoded PARSED is a planning object: a thought, a call of a
tool, or a tool's arguments."
  (and (hash-table-p parsed)
       (or (stringp (gethash "thought" parsed))
           (let ((call (gethash "call" parsed))) (and (stringp call) (member call tool-names :test #'equal)))
           (nth-value 1 (gethash "_i" parsed)) (nth-value 1 (gethash "paths" parsed))
           (nth-value 1 (gethash "command" parsed))
           (and (nth-value 1 (gethash "path" parsed)) (nth-value 1 (gethash "content" parsed))))))

(defun consume-planning (text tool-names &optional final)
  "(values KIND VISIBLE) of the buffered TEXT (consumePlanningBuffer): KIND
:INCOMPLETE while it may still become a leak, :LEAK when its leading object
was one (VISIBLE what follows it), :PLAIN when it is text (VISIBLE all of it)."
  (if (not (planning-leak-prefix-p text))
      (values :plain text)
      (multiple-value-bind (json rest)
          (multiple-value-bind (json rest) (leading-json-object text t)
            (if json (values json rest) (leading-json-object text nil)))
        (cond ((null json)
               (cond ((not final) (values :incomplete nil))
                     ((leak-signature-p (string-trim " " text) tool-names) (values :leak ""))
                     (t (values :plain text))))
              (t (let ((parsed (handler-case (nlk:decode-json json :whole t) (error () :unparsed))))
                   (cond ((eq parsed :unparsed)
                          (if (leak-signature-p json tool-names) (values :leak rest) (values :plain text)))
                         ((leak-object-p parsed tool-names) (values :leak rest))
                         (t (values :plain text)))))))))

(defun ready-parts (candidate round)
  "Ready CANDIDATE's parts for the Gemini fold: planning text a Flash model
leaked held back and stripped, and the signature each block carries noted."
  (let ((leaks (compat (round-facts round) "flash_leak"))
        (names (round-tool-names round)))
    (loop for part across (nlk:json-array candidate "content" "parts")
          when (hash-table-p part)
            do (let ((text (nlk:json-value part :string "text"))
                     (signature (nlk:json-value part :text "thoughtSignature"))
                     (call (nlk:json-value part :object "functionCall")))
                 (cond
                   ((and text (plusp (length text)) (eq (gethash "thought" part) t))
                    (setf (round-block round) :thinking)
                    (when signature (setf (round-thinking-signature round) signature)))
                   ((and text (plusp (length text)))
                    (cond ((round-buffering round)
                           (setf (round-buffer round) (concatenate 'string (round-buffer round) text)
                                 (gethash "text" part) "")
                           (when signature (setf (round-buffered-signature round) signature)))
                          ((and leaks (uiop:string-prefix-p "{" (trim-start text)))
                           (setf (round-buffering round) t
                                 (round-buffer round) text
                                 (round-buffered-signature round) signature
                                 (gethash "text" part) "")))
                    (when (round-buffering round)
                      (multiple-value-bind (kind visible) (consume-planning (round-buffer round) names)
                        (unless (eq kind :incomplete)
                          (setf (gethash "text" part) visible
                                signature (round-buffered-signature round)
                                (round-buffering round) nil
                                (round-buffer round) ""
                                (round-buffered-signature round) nil))))
                    (when (plusp (length (gethash "text" part)))
                      (setf (round-block round) :text)
                      (when signature (setf (round-text-signature round) signature))))
                   ((and (equal text "") signature (not call))
                    (case (round-block round)
                      (:thinking (setf (round-thinking-signature round) signature))
                      (:text (setf (round-text-signature round) signature)))))
                 (when call
                   ;; a call ends the block, and what was held back with it
                   (setf (round-block round) nil
                         (round-buffering round) nil
                         (round-buffer round) ""))))
    (when (and (nlk:json-value candidate :string "finishReason") (round-buffering round))
      (multiple-value-bind (kind visible) (consume-planning (round-buffer round) names t)
        (declare (ignore kind))
        (setf (round-buffering round) nil (round-buffer round) "")
        (when (plusp (length visible))
          (let ((content (or (nlk:json-value candidate :object "content")
                             (setf (gethash "content" candidate) (nlk:json-object "role" "model")))))
            (setf (gethash "parts" content)
                  (concatenate 'vector (nlk:json-array content "parts")
                               (vector (nlk:json-object "text" visible))))
            (setf (round-block round) :text)
            (alexandria:when-let (signature (round-buffered-signature round))
              (setf (round-text-signature round) signature))))))))

(defun ready-usage (response)
  "RESPONSE's usage with the prompt count it omits filled in, and the cached
count clamped to it (mapGoogleUsage): an Antigravity chunk can do either."
  (alexandria:when-let (usage (nlk:json-value response :object "usageMetadata"))
    (let* ((candidates (or (nlk:json-value usage :integer "candidatesTokenCount") 0))
           (thoughts (or (nlk:json-value usage :integer "thoughtsTokenCount") 0))
           (total (or (nlk:json-value usage :integer "totalTokenCount") 0))
           (prompt (let ((reported (nlk:json-value usage :integer "promptTokenCount")))
                     (if (and reported (plusp reported)) reported (max 0 (- total candidates thoughts)))))
           (cached (nlk:json-value usage :integer "cachedContentTokenCount")))
      (setf (gethash "promptTokenCount" usage) prompt)
      (when cached (setf (gethash "cachedContentTokenCount" usage) (min cached prompt))))))

(defun ready-frame (frame round)
  "One Cloud Code Assist event FRAME as the Gemini chunk the fold takes, or
NIL for an event without one; an in-band error or a blocked prompt signals."
  (alexandria:when-let (error (nlk:json-value frame :object "error"))
    (let ((code (nlk:json-value error :integer "code")))
      (error 'nle::provider-error
             :status (and code (>= code 400) code)
             :detail (format nil "Cloud Code Assist stream error: ~a"
                             (or (nlk:json-value error :text "message") (nlk:json-value error :text "status")
                                 "unknown error")))))
  (alexandria:when-let (response (nlk:json-value frame :object "response"))
    (alexandria:when-let (id (nlk:json-value response :text "responseId"))
      (setf (round-response-id round) id))
    (let ((candidates (nlk:json-array response "candidates"))
          (blocked (nlk:json-value response :text "promptFeedback" "blockReason")))
      (when (and (zerop (length candidates)) blocked)
        (error 'nle::provider-error
               :status 400
               :detail (format nil "Request blocked by Google (~a)~@[: ~a~]"
                               blocked (nlk:json-value response :text "promptFeedback" "blockReasonMessage"))))
      ;; the first candidate is the answer, as omp reads it
      (when (plusp (length candidates))
        (let ((candidate (aref candidates 0)))
          (setf (gethash "candidates" response) (vector candidate))
          (when (hash-table-p candidate)
            (ready-parts candidate round)
            (when (nlk:json-value candidate :string "finishReason")
              (setf (round-finished round) t)))))
      (ready-usage response)
      response)))

(defun ready-fold (fold round)
  "FOLD, the Gemini lane's, handed each event's chunk."
  (lambda (frame finish record)
    (alexandria:when-let (response (ready-frame frame round))
      (funcall fold response finish record))))

;;; --- what a round signed --------------------------------------------------------

(defvar *signatures* (make-hash-table :test #'equal :synchronized t)
  "A message key (MESSAGE-KEY) -> (:model PROVIDER/MODEL :thinking SIGNATURE
:text SIGNATURE): what a round of this lane signed.")

(defvar *signature-keys* '()
  "The keys of *SIGNATURES*, newest first, for its bound.")

(defvar *signatures-lock* (bt2:make-lock :name "signatures")
  "Held across one change to *SIGNATURES* and its keys.")

(defparameter +signatures-limit+ 4096
  "The most messages whose signatures are kept; past it the oldest half goes.")

(defun message-key (message)
  "What identifies the assistant MESSAGE across rounds and its reload from
the store: the running turn's session, and the message's reasoning, answer
and calls."
  (nlk:sha256-text
   (with-output-to-string (out)
     (format out "~a~c~a~c~a" (or (getf (nle:turn) :session-id) "")
             #\Nul (or (nlk:json-value message :string "reasoning_content") "")
             #\Nul (nle::content-text (gethash "content" message)))
     (loop for call across (nlk:json-array message "tool_calls")
           do (format out "~c~a~c~a~c~a" #\Nul (or (nlk:json-value call :string "id") "")
                      #\Nul (or (nlk:json-value call :string "function" "name") "")
                      #\Nul (or (nlk:json-value call :string "function" "arguments") ""))))))

(defun remember-signatures (message round)
  "Keep what ROUND signed, under its assembled MESSAGE's key. => MESSAGE, untouched."
  (when (and (hash-table-p message)
             (or (round-thinking-signature round) (round-text-signature round)
                 (gethash "thought_signatures" message)))
    (let ((key (message-key message)))
      (bt2:with-lock-held (*signatures-lock*)
        (unless (nth-value 1 (gethash key *signatures*))
          (push key *signature-keys*))
        (setf (gethash key *signatures*)
              (list :model (format nil "~a/~a" (round-provider round) (round-model round))
                    :thinking (round-thinking-signature round)
                    :text (round-text-signature round)))
        (when (> (hash-table-count *signatures*) +signatures-limit+)
          (let ((kept (subseq *signature-keys* 0 (floor +signatures-limit+ 2))))
            (dolist (old (nthcdr (length kept) *signature-keys*)) (remhash old *signatures*))
            (setf *signature-keys* kept))))))
  message)

(defun recall-signatures (message)
  "What a round of this lane signed for the assistant MESSAGE, or NIL: a
message another lane made, or one older than this process."
  (and (hash-table-p message) (gethash (message-key message) *signatures*)))
