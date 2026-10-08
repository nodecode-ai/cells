;;;; cell-test.lisp --- the apple cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests run on Linux. The platform gate is NODECODE-APPLE::MAC-P,
;;;; stubbed true where a test plays a Mac; the Swift bridge helper is a
;;;; /bin/sh script in a scratch directory that keeps the request it was sent
;;;; and prints canned bridge events, so the request bytes, the event fold and
;;;; the process plumbing are the real ones. Nothing is compiled and no model
;;;; runs.

(in-package #:nodecode.test)

(define-test-slice "apple" "APPLE-CELL-" :start nodecode-apple:start-cell)

(define-cell-lifecycle-tests "apple"
  (:hooks 'nle::models-catalog-table :credential)
  (:refused ("helper" 5)))

;;; --- fixtures -------------------------------------------------------------------

(defparameter +apple-available+
  "{\"type\":\"availability\",\"available\":true,\"contextSize\":4096,\"variant\":\"AFM 3 Core\",\"vision\":true,\"toolCalling\":true,\"reasoningCapable\":true}")

(defparameter +apple-unavailable+
  "{\"type\":\"availability\",\"available\":false,\"reason\":\"apple_intelligence_not_enabled\",\"contextSize\":4096}")

(defparameter +apple-tool-turn+
  '("{\"type\":\"reasoning\",\"text\":\"Add them.\"}"
    "{\"type\":\"text\",\"text\":\"Adding.\"}"
    "{\"type\":\"toolCall\",\"callId\":\"call-1\",\"name\":\"eval\",\"arguments\":\"{\\\"form\\\":\"}"
    "{\"type\":\"toolCall\",\"callId\":\"call-1\",\"name\":\"eval\",\"arguments\":\"\\\"(+ 1 2)\\\"}\"}"
    "{\"type\":\"usage\",\"input\":120,\"cachedInput\":100,\"output\":9,\"reasoning\":3}"
    "{\"type\":\"done\"}")
  "A turn that reasons, says a word, then calls eval in two argument fragments.")

(defun apple-stub-helper (dir events &optional (availability +apple-available+))
  "A stand-in bridge helper in DIR: `availability' prints AVAILABILITY,
`generate' keeps its stdin in DIR/request.json and prints EVENTS. => its path."
  (let ((path (merge-pathnames "nodecode-apple-bridge" dir)))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "#!/bin/sh~%if [ \"$1\" = availability ]; then~%  printf '%s\\n' '~a'~%  exit 0~%fi~%cat > '~a'~%~{printf '%s\\n' '~a'~%~}"
              availability (uiop:native-namestring (merge-pathnames "request.json" dir)) events))
    (sb-posix:chmod (uiop:native-namestring path) #o755)
    (uiop:native-namestring path)))

(defun apple-scratch-dir ()
  (let ((dir (uiop:ensure-directory-pathname
              (format nil "/tmp/nc-apple-~36r/" (random (expt 36 8) (make-random-state t))))))
    (ensure-directories-exist dir)
    dir))

(defun apple-lane-round (context)
  "One round of CONTEXT on the lane its frozen config names."
  (let ((config (nle::compiled-turn-context-provider-config context)))
    (funcall (nle::provider-lane-stream-symbol
              (nle::find-lane-by-name (nle::effective-provider-config-lane config)))
             context)))

(defun apple-await-probe ()
  "Wait for the availability the start asked for, so no test races it."
  (alexandria:when-let (thread nodecode-apple::*probe-thread*)
    (ignore-errors (bt2:join-thread thread))))

(defmacro with-apple-mac ((dir &key (events '+apple-tool-turn+) (availability '+apple-available+)) &body forms)
  "FORMS on a Mac (MAC-P stubbed true) with the cell started on a stand-in
helper in DIR printing EVENTS, its availability asked."
  `(let ((,dir (apple-scratch-dir)))
     (unwind-protect
          (with-stubbed-fdefinition (nodecode-apple::mac-p () t)
            (with-cell-stop ((apple-start "helper" (apple-stub-helper ,dir ,events ,availability)))
              (apple-await-probe)
              ,@forms))
       (uiop:delete-directory-tree ,dir :validate t :if-does-not-exist :ignore))))

(defmacro with-apple-round ((values request) (&key (events '+apple-tool-turn+) (context '(user-context "add one and two"))
                                                   effort temperature)
                            &body forms)
  "FORMS after one round on the stand-in: VALUES the lane's answer or the
provider error it ended in, REQUEST what the helper was sent (decoded)."
  (alexandria:with-gensyms (dir)
    `(with-apple-mac (,dir :events ,events)
       (let* ((nle::*provider* "apple") (nle::*model* "on-device") (nle::*api-key* nil) (nle::*endpoint* nil)
              (nle::*reasoning-effort* ,effort) (nle::*temperature* ,temperature)
              (,values (handler-case (multiple-value-list (apple-lane-round ,context))
                         (nle::provider-error (condition) condition)))
              (,request (let ((file (merge-pathnames "request.json" ,dir)))
                          (and (probe-file file) (nlk:decode-json (uiop:read-file-string file))))))
         (declare (ignorable ,values ,request))
         ,@forms))))

(defun apple-said (fragment)
  "Whether a notice said lately carries FRAGMENT."
  (some (lambda (entry) (search fragment (first entry))) (nlk:notice-log :limit 50)))

;;; --- the platform gate --------------------------------------------------------------------

(deftest apple-cell-does-nothing-off-a-mac ()
  (with-stubbed-fdefinition (nodecode-apple::mac-p () nil)
    (with-cell-stop ((apple-start))
      (is (apple-said "run only on a Mac with Apple silicon") "the start says so")
      (is (null (nle::find-lane-by-name "apple" nil)) "and registers no lane")
      (is (null (nlk:json-value (nle::models-catalog-table) :object "apple")) "nor a catalog row")
      (let ((condition (handler-case (progn (nodecode-apple::stream-round (user-context)) nil)
                         (nle::provider-error (c) c))))
        (is (typep condition 'nle::provider-config-error) "a round refuses before any process starts")))))

(deftest apple-cell-on-a-mac-offers-the-model-the-bridge-reports ()
  (with-apple-mac (dir)
    (is (nle::find-lane-by-name "apple" nil) "the lane is registered")
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "apple"))
           (model (gethash "on-device" (nlk:json-value row :object "models"))))
      (is (equal "Apple Foundation Models (on-device)" (nlk:json-value row :string "name")))
      (is (equal "local://apple-foundation-models" (nlk:json-value row :string "api")))
      (is (equal "Apple AFM 3 Core" (nle::catalog-model-name model)))
      (is (= 4096 (nle::catalog-model-context model)) "the window the bridge reports")
      (is (= 4096 (nle::catalog-model-output model)) "the output ceiling under it")
      (is (equalp #("text" "image") (nle::catalog-model-inputs model)))
      (is (nle::catalog-model-reasoning-p model))
      (is (nle::catalog-model-tool-call-p model))
      (is (equal "apple" (nle::configured-provider-lane "apple"))))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (is (eq :public (nle:credential-source (nle::resolve-provider-credential "apple" :auth-path auth :probe t)))
            "no key, and never another family's"))))
  (with-apple-mac (dir :availability +apple-unavailable+)
    (is (zerop (hash-table-count (nlk:json-value (nle::models-catalog-table) :object "apple" "models")))
        "no model while the bridge says it cannot generate")
    (is (search "apple_intelligence_not_enabled" (second (cell-notice "nodecode-apple"))) "and says why")))

;;; --- a round ------------------------------------------------------------------------------

(deftest apple-cell-sends-the-transcript-request ()
  (with-apple-round (values request) (:effort "high" :temperature 0)
    (is (consp values) "the round answered")
    (is (stringp (nlk:json-value request :string "instructions")) "the system prompt is the instructions")
    (let ((last (let ((entries (nlk:json-array request "entries"))) (aref entries (1- (length entries))))))
      (is (equal "prompt" (nlk:json-value last :string "kind")))
      (is (search "add one and two" (nlk:json-value (aref (nlk:json-array last "parts") 0) :string "text"))))
    (is (equal "deep" (nlk:json-value request :string "reasoningLevel")) "high is deep for a reasoning model")
    (is (eq t (gethash "greedy" request)) "temperature 0 with no bounds is greedy")
    (is (eql 0 (gethash "temperature" request)))
    (loop for tool across (nlk:json-array request "tools")
          for schema = (nlk:decode-json (nlk:json-value tool :string "parameters"))
          do (is (equal "object" (nlk:json-value schema :string "type")) "parameters ride as a JSON text")
             (is (nth-value 1 (gethash "x-order" schema)) "in GenerationSchema's dialect"))))

(deftest apple-cell-folds-reasoning-text-and-a-call ()
  (with-apple-round (values request) ()
    (destructuring-bind (message usage finish request-json) values
      (is (equal "Adding." (nlk:json-value message :string "content")))
      (is (equal "Add them." (nlk:json-value message :string "reasoning_content")))
      (let ((call (aref (nlk:json-value message :array "tool_calls") 0)))
        (is (equal "call-1" (nlk:json-value call :string "id")) "the bridge's call id")
        (is (equal "eval" (nlk:json-value call :string "function" "name")))
        (is (equal "(+ 1 2)" (nlk:json-value (nlk:decode-json (nlk:json-value call :string "function" "arguments"))
                                             :string "form"))
            "the fragments make one argument object"))
      (is (equal "tool_calls" finish))
      (is (= 20 (nle::provider-usage-input-tokens usage)) "the uncached part of the prompt")
      (is (= 100 (nle::provider-usage-cached-input-tokens usage)))
      (is (= 9 (nle::provider-usage-output-tokens usage)))
      (is (= 3 (nle::provider-usage-reasoning-tokens usage)))
      (is (equal (nlk:json-value request :string "instructions")
                 (nlk:json-value (nlk:decode-json request-json) :string "instructions"))
          "the request kept is the one the helper read"))))

(deftest apple-cell-lowers-the-history ()
  (with-apple-round (values request)
      (:events '("{\"type\":\"text\",\"text\":\"ok\"}" "{\"type\":\"done\"}")
       :context (compiled-context
                 (list (nle::message "user" (vector (nle::make-text-content-part "what is this?")
                                                    (nle::make-image-content-part "data:image/png;base64,iVBORw0KGgo=")))
                       (nlk:json-object "role" "assistant" "content" "Let me run it."
                                        "tool_calls" (vector (nle::chat-tool-call-object "call-9" "eval" "{\"form\":\"(+ 1 2)\"}")))
                       (nle::message "tool" "3" :name "eval" :tool-call-id "call-9")
                       (nle::message "user" "thanks"))))
    (is (equal "stop" (third values)))
    (let ((entries (coerce (nlk:json-array request "entries") 'list)))
      (is (equal '("prompt" "response" "toolCalls" "toolOutput" "prompt")
                 (mapcar (lambda (entry) (gethash "kind" entry)) entries)))
      (let ((image (find "image" (nlk:json-array (first entries) "parts")
                         :key (lambda (part) (gethash "type" part)) :test #'equal)))
        (is (equal "iVBORw0KGgo=" (nlk:json-value image :string "data")))
        (is (equal "image-1" (nlk:json-value image :string "label")) "labelled across the conversation"))
      (let ((call (aref (nlk:json-array (third entries) "calls") 0)))
        (is (equal "call-9" (nlk:json-value call :string "id")))
        (is (equal "(+ 1 2)" (nlk:json-value (nlk:decode-json (nlk:json-value call :string "arguments")) :string "form"))
            "arguments go as a JSON text"))
      (is (equal "call-9" (nlk:json-value (fourth entries) :string "id")) "an output answers its call")
      (is (equal "eval" (nlk:json-value (fourth entries) :string "name"))))))

(deftest apple-cell-says-what-the-bridge-refused ()
  (with-apple-round (values request)
      (:events '("{\"type\":\"error\",\"code\":\"guardrail_violation\",\"message\":\"May contain sensitive content\"}"))
    (is (typep values 'nle::provider-error))
    (is (search "(guardrail_violation)" (nle::provider-error-detail values)))
    (is (eql 400 (nle::provider-error-status values)) "a safety refusal is final"))
  (with-apple-round (values request)
      (:events '("{\"type\":\"error\",\"code\":\"context_size_exceeded\",\"message\":\"Prompt is too long: 5000 tokens exceed the 4096 token context window\"}"))
    (is (nle::provider-overflow-error-p values) "an overflow the core evicts on"))
  (with-apple-round (values request) (:events '("{\"type\":\"text\",\"text\":\"half\"}"))
    (is (typep values 'nle::provider-stream-incomplete) "no terminal event is a cut turn")))

;;; --- the schema dialect ---------------------------------------------------------------------

(deftest apple-cell-lowers-tool-schemas-to-generation-schema ()
  (multiple-value-bind (schema paths)
      (nodecode-apple::foundation-schema
       (nlk:decode-json "{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\",\"description\":\"a file\"},\"mode\":{\"anyOf\":[{\"const\":\"read\"},{\"const\":\"write\"}]},\"limit\":{\"type\":[\"integer\",\"null\"],\"minimum\":1},\"options\":{\"type\":\"object\",\"description\":\"extra flags\"},\"tags\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/$defs/Tag\"}}},\"required\":[\"path\",\"nope\"],\"$defs\":{\"Tag\":{\"type\":\"object\",\"additionalProperties\":{\"type\":\"string\"}}}}")
       "fs.open")
    (let ((properties (nlk:json-value schema :object "properties")))
      (is (equal "fs_open" (nlk:json-value schema :string "title")) "a title the decoder takes")
      (is (equalp #("path") (nlk:json-value schema :array "required")) "only required names that exist")
      (is (equalp #("path" "mode" "limit" "options" "tags") (nlk:json-value schema :array "x-order")))
      (is (and (nth-value 1 (gethash "additionalProperties" schema)) (null (gethash "additionalProperties" schema)))
          "closed")
      (is (equalp #("read" "write") (nlk:json-value properties :array "mode" "enum")) "string literals are an enum")
      (is (equal "integer" (nlk:json-value properties :string "limit" "type")) "a nullable type is its one type")
      (is (= 1 (nlk:json-value properties :integer "limit" "minimum")))
      (is (equal "string" (nlk:json-value properties :string "options" "type")) "a free-form map is encoded")
      (is (equal "extra flags (JSON-encoded value)" (nlk:json-value properties :string "options" "description")))
      (is (equal "string" (nlk:json-value properties :string "tags" "items" "type")) "so is a map behind a $ref"))
    (is (equal '(("options") ("tags" "*")) paths))
    (let ((arguments (nlk:decode-json "{\"path\":\"a\",\"options\":\"{\\\"force\\\":true}\",\"tags\":[\"{\\\"k\\\":\\\"v\\\"}\",\"not json\"]}")))
      (nodecode-apple::decode-arguments arguments paths)
      (is (eq t (nlk:json-value arguments :any "options" "force")) "an encoded value is parsed back")
      (is (equal "v" (nlk:json-value (aref (nlk:json-value arguments :array "tags") 0) :string "k")))
      (is (equal "not json" (aref (nlk:json-value arguments :array "tags") 1)) "what does not parse stays"))))
