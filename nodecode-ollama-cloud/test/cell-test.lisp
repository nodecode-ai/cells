;;;; cell-test.lisp --- the ollama-cloud cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every round a stubbed dex:post
;;;; answering canned NDJSON, every listing a stubbed dex:request: nothing
;;;; touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "ollama-cloud" "OLLAMA-CLOUD-CELL-" :start nodecode-ollama-cloud:start-cell)

(define-cell-lifecycle-tests "ollama-cloud"
  (:hooks 'nle::models-catalog-table :credential 'nle::list-provider-models)
  (:running (is (nle::find-lane-by-name "ollama-cloud" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "ollama-cloud" nil)) "and taken back out"))
  (:refused ("base_url" 5)))

;;; --- fixtures -------------------------------------------------------------------

(defun oc-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun oc-lines (&rest objects)
  "A character stream of OBJECTS (JSON texts), one per line: Ollama's NDJSON."
  (make-string-input-stream (format nil "~{~a~%~}" objects)))

(defparameter +oc-tool-round+
  (list "{\"model\":\"gpt-oss:120b\",\"message\":{\"role\":\"assistant\",\"content\":\"\",\"thinking\":\"Add them.\"},\"done\":false}"
        "{\"model\":\"gpt-oss:120b\",\"message\":{\"role\":\"assistant\",\"content\":\"Adding.\"},\"done\":false}"
        "{\"model\":\"gpt-oss:120b\",\"message\":{\"role\":\"assistant\",\"content\":\"\",\"tool_calls\":[{\"function\":{\"name\":\"eval\",\"arguments\":{\"form\":\"(+ 1 2)\"}}}]},\"done\":false}"
        "{\"model\":\"gpt-oss:120b\",\"message\":{\"role\":\"assistant\",\"content\":\"\"},\"done\":true,\"done_reason\":\"stop\",\"prompt_eval_count\":120,\"prompt_eval_cached_count\":100,\"eval_count\":9}")
  "A round that thinks, says a word, then calls eval: done says stop.")

(defparameter +oc-text-round+
  (list "{\"message\":{\"role\":\"assistant\",\"content\":\"ok\"},\"done\":false}"
        "{\"message\":{\"role\":\"assistant\",\"content\":\"\"},\"done\":true,\"done_reason\":\"stop\",\"prompt_eval_count\":5,\"eval_count\":1}")
  "A round that says ok.")

(defun oc-lane-round (context)
  "One round of CONTEXT on the lane its frozen config names, the way the turn loop runs it."
  (let ((config (nle::compiled-turn-context-provider-config context)))
    (funcall (nle::provider-lane-stream-symbol
              (nle::find-lane-by-name (nle::effective-provider-config-lane config)))
             context)))

(defmacro with-oc-round ((values url headers body) (&key (model "gpt-oss:120b") effort choice temperature
                                                        (lines '+oc-text-round+) (context '(user-context "add one and two"))
                                                        config)
                         &body forms)
  "FORMS with the cell started on CONFIG and one round of MODEL streaming
LINES: VALUES the lane's (MESSAGE USAGE FINISH REQUEST-JSON), URL HEADERS BODY
the request as dex:post saw it (BODY decoded)."
  `(with-cell-stop ((ollama-cloud-start ,@config))
     (let ((nle::*provider* "ollama-cloud") (nle::*model* ,model) (nle::*api-key* "ok-test")
           (nle::*reasoning-effort* ,effort) (nle::*tool-choice* ,choice) (nle::*temperature* ,temperature)
           (nle::*endpoint* nil) (,url nil) (,headers nil) (,body nil) (,values nil))
       (declare (ignorable ,url ,headers ,body ,values))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (apply #'oc-lines ,lines) 200))
         (setf ,values (multiple-value-list (oc-lane-round ,context))))
       ,@forms)))

(defun oc-messages (body)
  "The messages of a decoded request BODY, as a list."
  (coerce (nlk:json-array body "messages") 'list))

;;; --- the catalog and the lane ---------------------------------------------------------

(deftest ollama-cloud-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((ollama-cloud-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "ollama-cloud"))
           (models (nlk:json-value row :object "models")))
      (is (equal "Ollama Cloud" (nlk:json-value row :string "name")))
      (is (equal "https://ollama.com" (nlk:json-value row :string "api")) "the bare host, not /v1")
      (is (equalp #("OLLAMA_CLOUD_API_KEY") (nlk:json-value row :array "env")))
      (is (gethash "gpt-oss:120b" models) "omp's default model is listed")
      (is (equal '("high" "max") (nle::catalog-model-efforts (gethash "deepseek-v3.2" models)))
          "a DeepSeek ladder reaches max")
      (is (equal "ollama-cloud" (nle::configured-provider-lane "ollama-cloud")) "the cell's own lane drives it")
      (is (equal "https://ollama.com" (nle::lane-endpoint "ollama-cloud" "ollama-cloud"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "ollama-cloud"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest ollama-cloud-cell-base-follows-the-section ()
  (with-cell-stop ((ollama-cloud-start "base_url" "https://relay.example/api/"))
    (is (equal "https://relay.example"
               (nlk:json-value (nle::models-catalog-table) :string "ollama-cloud" "api"))
        "a trailing /api and slash are dropped, as omp normalizes the base")))

;;; --- the credential --------------------------------------------------------------------

(deftest ollama-cloud-cell-reads-its-key-variable ()
  (with-cell-stop ((ollama-cloud-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OLLAMA_CLOUD_API_KEY") "ok-env"))
        (let ((credential (nle::resolve-provider-credential "ollama-cloud" :auth-path auth :probe t)))
          (is (equal "ok-env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))
        (is (not (equal "ok-env"
                        (nle:credential-key (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t))))
            "another provider's ladder never reads OLLAMA_CLOUD_API_KEY")))))

(deftest ollama-cloud-cell-never-sends-another-familys-key ()
  (with-cell-stop ((ollama-cloud-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "ollama-cloud" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential)))
              "the chat family's default variable never reaches Ollama")
          (is (eq :public (nle:credential-source credential))))))))

(deftest ollama-cloud-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((ollama-cloud-start))
    (with-temp-auth (auth "{\"api_keys\":{\"ollama-cloud\":{\"provider\":\"ollama-cloud\",\"key\":\"ok-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OLLAMA_CLOUD_API_KEY") "ok-env"))
        (is (equal "ok-saved"
                   (nle:credential-key (nle::resolve-provider-credential "ollama-cloud" :auth-path auth :probe t))))))))

(deftest ollama-cloud-cell-refuses-a-round-without-a-key ()
  (with-cell-stop ((ollama-cloud-start))
    (let ((nle::*provider* "ollama-cloud") (nle::*model* "gpt-oss:120b") (nle::*api-key* nil) (nle::*endpoint* nil))
      (with-temp-auth (auth "{}")
        (let ((nle::*auth-file-path* auth) (posted nil))
          (with-stubbed-fdefinitions ((nle::credential-env (name) nil)
                                      (dex:post (&rest args) (setf posted t) (values (oc-lines) 200)))
            (let ((condition (handler-case (progn (oc-lane-round (user-context)) nil)
                               (nle::provider-error (c) c))))
              (is (typep condition 'nle::provider-config-error) "no key is a config refusal")
              (is (search "ollama.com/settings/keys" (nle::provider-error-detail condition)))
              (is (null posted) "and nothing is posted"))))))))

;;; --- the request ------------------------------------------------------------------------

(deftest ollama-cloud-cell-posts-the-native-chat-request ()
  (with-oc-round (values url headers body) (:effort "max" :model "deepseek-v3.2")
    (is (equal "https://ollama.com/api/chat" url))
    (is (equal "Bearer ok-test" (oc-header headers "authorization")))
    (is (equal "application/json" (oc-header headers "content-type")))
    (is (equal "deepseek-v3.2" (nlk:json-value body :string "model")))
    (is (eq t (gethash "stream" body)))
    (is (equal "max" (nlk:json-value body :string "think")) "max rides Ollama's own think value")
    (is (null (nth-value 1 (gethash "options" body))) "no sampling configured, no options")
    (let ((messages (oc-messages body)))
      (is (equal "system" (nlk:json-value (first messages) :string "role")) "the system prompt leads")
      (is (equal '("user" "add one and two")
                 (let ((last (car (last messages))))
                   (list (nlk:json-value last :string "role") (nlk:json-value last :string "content"))))))
    (is (null (nlk:json-value body :any "options" "num_predict"))
        "no output ceiling: every Ollama Cloud model omits it")))

(deftest ollama-cloud-cell-maps-the-effort-to-think ()
  (flet ((think (effort model)
           (let ((body nil))
             (with-oc-round (values url headers sent) (:effort effort :model model)
               (setf body sent))
             (multiple-value-list (gethash "think" body)))))
    (is (equal '(nil t) (think "off" "gpt-oss:120b")) "off is think: false for a thinking model")
    (is (equal '(nil nil) (think "off" "gemma3:4b")) "and no key for a model that does not think")
    (is (equal '("low" t) (think "minimal" "gpt-oss:120b")))
    (is (equal '("high" t) (think "xhigh" "gpt-oss:120b")) "xhigh is Ollama's high")
    (is (equal '(nil nil) (think nil "gpt-oss:120b")) "no effort leaves the model's default")))

(deftest ollama-cloud-cell-carries-tool-choice-and-sampling ()
  (with-oc-round (values url headers body) (:choice "required" :temperature 0.5)
    (is (equal "required" (nlk:json-value body :string "tool_choice")))
    (is (= 0.5 (nlk:json-value body :number "options" "temperature"))))
  (with-oc-round (values url headers body) (:choice "auto")
    (is (null (nth-value 1 (gethash "tool_choice" body))) "auto is Ollama's default, left off")))

(defun oc-history-context ()
  "A compiled context of a turn with an image, a call and its result: built
inside a round, so its frozen config is that round's."
  (compiled-context
   (list (nle::message "user" (vector (nle::make-text-content-part "what is this?")
                                      (nle::make-image-content-part "data:image/png;base64,iVBORw0KGgo=")))
         (nlk:json-object "role" "assistant" "content" "Let me look." "reasoning_content" "hmm"
                          "tool_calls" (vector (nle::chat-tool-call-object "ollama:1:eval" "eval" "{\"form\":\"(+ 1 2)\"}")))
         (nle::message "tool" "3" :name "eval" :tool-call-id "ollama:1:eval")
         (nle::message "user" "thanks"))))

(defun oc-role (role messages)
  "The first of MESSAGES whose role is ROLE."
  (find role messages :key (lambda (m) (gethash "role" m)) :test #'equal))

(deftest ollama-cloud-cell-converts-the-history ()
  (with-oc-round (values url headers body) (:model "kimi-k3" :context (oc-history-context))
    (let* ((messages (oc-messages body))
           (user (oc-role "user" messages))
           (assistant (oc-role "assistant" messages))
           (tool (oc-role "tool" messages))
           (call (aref (nlk:json-array assistant "tool_calls") 0)))
      (is (equalp #("iVBORw0KGgo=") (nlk:json-value user :array "images")) "a data: image rides as base64")
      (is (search "what is this?" (nlk:json-value user :string "content")))
      (is (null (nth-value 1 (gethash "thinking" assistant))) "Ollama Cloud refuses thinking in history")
      (is (equal "eval" (nlk:json-value call :string "function" "name")))
      (is (equal "(+ 1 2)" (nlk:json-value call :string "function" "arguments" "form"))
          "arguments go back as an object")
      (is (equal "eval" (nlk:json-value tool :string "tool_name")) "a result names its tool")
      (is (search "3" (nlk:json-value tool :string "content")))))
  (with-oc-round (values url headers body) (:model "gpt-oss:120b" :context (oc-history-context))
    (let ((user (oc-role "user" (oc-messages body))))
      (is (null (nth-value 1 (gethash "images" user))) "a model without vision gets no image")
      (is (search "[image omitted: model does not support vision]" (nlk:json-value user :string "content"))))))

(deftest ollama-cloud-cell-gives-a-userless-request-a-user-turn ()
  (with-oc-round (values url headers body)
      (:context (compiled-context (list (nle::message "system" "[evicted] carry on"))))
    (let ((messages (oc-messages body)))
      (is (equal "system" (gethash "role" (first messages))) "the prompt stays system")
      (is (null (oc-role "user" (butlast messages))) "no user turn was sent but the one made")
      (is (equal "user" (gethash "role" (car (last messages))))
          "the last system turn past it becomes the user turn Ollama needs"))))

(deftest ollama-cloud-cell-sanitizes-tool-schemas ()
  (let* ((schema (nlk:decode-json "{\"type\":\"object\",\"additionalProperties\":false,\"properties\":{\"any\":true,\"maybe\":{\"type\":[\"string\",\"null\"]},\"either\":{\"type\":[\"string\",\"number\"]},\"list\":{\"type\":\"array\",\"items\":true}}}"))
         (clean (nodecode-ollama-cloud::sanitize-schema schema))
         (properties (nlk:json-value clean :object "properties")))
    (is (null (nth-value 1 (gethash "additionalProperties" clean))) "a boolean additionalProperties is dropped")
    (is (= 6 (length (nlk:json-value properties :array "any" "anyOf"))) "true widens to every primitive")
    (is (equal "string" (nlk:json-value properties :string "maybe" "type")) "a nullable type is its one type")
    (is (= 2 (length (nlk:json-value (aref (nlk:json-value properties :array "either" "allOf") 0) :array "anyOf")))
        "two types are a union under allOf")
    (is (= 6 (length (nlk:json-value properties :array "list" "items" "anyOf"))))
    (let ((tools (nodecode-ollama-cloud::wire-tools
                  (vector (nlk:json-object "type" "function"
                                           "function" (nlk:json-object "name" "eval" "description" "run lisp"
                                                                       "parameters" schema))))))
      (is (equal "eval" (nlk:json-value (aref tools 0) :string "function" "name")))
      (is (hash-table-p (nlk:json-value (aref tools 0) :object "function" "parameters" "properties" "any"))))))

;;; --- the stream -------------------------------------------------------------------------

(deftest ollama-cloud-cell-folds-thinking-text-and-a-tool-call ()
  (with-oc-round (values url headers body) (:lines +oc-tool-round+)
    (destructuring-bind (message usage finish request-json) values
      (is (equal "Adding." (nlk:json-value message :string "content")))
      (is (equal "Add them." (nlk:json-value message :string "reasoning_content")))
      (let ((call (aref (nlk:json-value message :array "tool_calls") 0)))
        (is (equal "ollama:2:eval" (nlk:json-value call :string "id")) "omp's id: the block index and the name")
        (is (equal "eval" (nlk:json-value call :string "function" "name")))
        (is (equal "(+ 1 2)" (nlk:json-value (nlk:decode-json (nlk:json-value call :string "function" "arguments"))
                                             :string "form"))))
      (is (equal "tool_calls" finish) "a call is a tool round whatever done_reason says")
      (is (= 20 (nle::provider-usage-input-tokens usage)) "the uncached part of the prompt")
      (is (= 100 (nle::provider-usage-cached-input-tokens usage)))
      (is (= 9 (nle::provider-usage-output-tokens usage)))
      (is (typep request-json 'nlk:octets)))))

(deftest ollama-cloud-cell-folds-a-plain-answer ()
  (with-oc-round (values url headers body) ()
    (destructuring-bind (message usage finish request-json) values
      (declare (ignore request-json))
      (is (equal "ok" (nlk:json-value message :string "content")))
      (is (null (nth-value 1 (gethash "reasoning_content" message))))
      (is (equal "stop" finish))
      (is (null (nle::provider-usage-cached-input-tokens usage)) "no cache count, none claimed"))))

(defun oc-failure (lines)
  "The provider error a round streaming LINES ends in, or NIL."
  (handler-case (progn (with-oc-round (values url headers body) (:lines lines)) nil)
    (nle::provider-error (condition) condition)))

(deftest ollama-cloud-cell-says-what-a-failed-round-was ()
  (let ((load (oc-failure (list "{\"message\":{\"role\":\"assistant\",\"content\":\"\"},\"done\":true,\"done_reason\":\"load\"}"))))
    (is (search "done_reason: load" (nle::provider-error-detail load)))
    (is (eql 400 (nle::provider-error-status load))))
  (let ((full (oc-failure (list "{\"message\":{\"role\":\"assistant\",\"content\":\"\"},\"done\":true,\"done_reason\":\"length\"}"))))
    (is (search "context window" (nle::provider-error-detail full)))
    (is (nle::provider-overflow-error-p full) "an empty length finish is an overflow the core evicts on"))
  (let ((cut (oc-failure (list "{\"message\":{\"role\":\"assistant\",\"content\":\"half\"},\"done\":false}"))))
    (is (typep cut 'nle::provider-stream-incomplete) "no done line is a truncated stream"))
  (let ((said (oc-failure (list "{\"error\":\"model runner has unexpectedly stopped\"}"))))
    (is (search "unexpectedly stopped" (nle::provider-error-detail said)) "an in-stream error is said")))

;;; --- the listing --------------------------------------------------------------------------

(deftest ollama-cloud-cell-lists-api-tags ()
  (with-cell-stop ((ollama-cloud-start))
    (let ((asked '()))
      (with-stubbed-fdefinition (dex:request (url &rest args)
                                 (push (list url (getf args :headers) (getf args :content)) asked)
                                 (cond ((search "/api/tags" url)
                                        (values "{\"models\":[{\"name\":\"gpt-oss:120b\",\"model\":\"gpt-oss:120b\"},{\"name\":\"brand-new:1t\",\"model\":\"brand-new:1t\"},{\"name\":\"mystery:7b\",\"model\":\"mystery:7b\"}]}" 200 (make-hash-table :test 'equal)))
                                       ((search "brand-new" (or (getf args :content) ""))
                                        (values "{\"capabilities\":[\"completion\",\"thinking\"],\"model_info\":{\"newarch.context_length\":262144}}" 200 (make-hash-table :test 'equal)))
                                       (t (values "{}" 404 (make-hash-table :test 'equal)))))
        (multiple-value-bind (rows error) (nle::list-provider-models "ollama-cloud" :key "ok-list")
          (is (null error))
          (is (equal '("brand-new:1t" "gpt-oss:120b" "mystery:7b") (mapcar (lambda (row) (getf row :id)) rows)))
          (is (= 131072 (getf (find "gpt-oss:120b" rows :key (lambda (row) (getf row :id)) :test #'equal) :context-window))
              "a bundled model keeps its row's window")
          (is (= 262144 (getf (first rows) :context-window)) "a new one asks /api/show")
          (is (= 128000 (getf (third rows) :context-window)) "and one /api/show cannot place gets omp's default")))
      (let ((tags (find-if (lambda (entry) (search "/api/tags" (first entry))) asked)))
        (is (equal "https://ollama.com/api/tags" (first tags)))
        (is (equal "Bearer ok-list" (oc-header (second tags) "authorization")))))))

;; /connect's key check calls the listing with :key and reads a NIL second
;; value as "the key works": the hook asks /api/tags with that key, and a
;; refusal is the core's own "HTTP <status> <code>" reason.
(deftest ollama-cloud-cell-key-check-asks-with-the-key ()
  (with-cell-stop ((ollama-cloud-start))
    (let ((asked nil))
      (with-stubbed-fdefinition (dex:request (url &rest args)
                                 (setf asked (list url (getf args :headers)))
                                 (values "{\"error\":\"unauthorized\"}" 401 (make-hash-table :test 'equal)))
        (multiple-value-bind (rows reason) (nle::list-provider-models "ollama-cloud" :key "ok-typed")
          (is (null rows))
          (is (and (stringp reason) (uiop:string-prefix-p "HTTP 401" reason)) reason)
          (is (ppcre:register-groups-bind ((#'parse-integer status) code) ("^HTTP (\\d+)(?: (\\S+))?" reason)
                (nle::refused-key-answer-p status code))
              "a reason the key check reads as a refused key")))
      (is (equal "https://ollama.com/api/tags" (first asked)))
      (is (equal "Bearer ok-typed" (oc-header (second asked) "authorization")) "the key typed, not a stored one"))
    (with-stubbed-fdefinition (dex:request (url &rest args)
                               (values "{\"models\":[]}" 200 (make-hash-table :test 'equal)))
      (is (null (nth-value 1 (nle::list-provider-models "ollama-cloud" :key "ok-good")))
          "an answer to the key is the key working"))))

;;; --- the core's own path ------------------------------------------------------------------

(deftest ollama-cloud-cell-keeps-the-core-chat-lane-when-pinned ()
  (with-temp-shared-config ("{\"providers\": {\"ollama-cloud\": {\"sdk\": \"openai-completions\", \"base_url\": \"https://ollama.com/v1\"}}}")
    (with-cell-stop ((ollama-cloud-start))
      (is (equal "openai-completions" (nle::configured-provider-lane "ollama-cloud"))
          "an sdk pin outranks the catalog's package")
      (let ((nle::*provider* "ollama-cloud") (nle::*model* "gpt-oss:120b") (nle::*api-key* nil) (nle::*endpoint* nil)
            (url nil) (headers nil))
        (with-temp-auth (auth "{}")
          (let ((nle::*auth-file-path* auth))
            (with-stubbed-fdefinitions ((nle::credential-env (name) (and (equal name "OLLAMA_CLOUD_API_KEY") "ok-env"))
                                        (dex:post (asked &rest args)
                                                  (setf url asked headers (getf args :headers))
                                                  (values (make-truncated-sse-stream
                                                           "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                                                           "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                                                           "[DONE]")
                                                          200)))
              (oc-lane-round (user-context)))))
        (is (equal "https://ollama.com/v1/chat/completions" url) "the chat lane serves it, as without the cell")
        (is (equal "Bearer ok-env" (oc-header headers "authorization")) "with the Ollama key")))))
