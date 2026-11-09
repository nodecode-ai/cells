;;;; cell-test.lisp --- the anyrouter cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post or
;;;; NLE:HTTP: nothing touches the network, the environment or the operator's
;;;; files.

(in-package #:nodecode.test)

(define-test-slice "anyrouter" "ANYROUTER-CELL-" :start nodecode-anyrouter:start-cell)

(define-cell-lifecycle-tests "anyrouter"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body 'nle::walk-provider-stream)
  (:command "anyrouter")
  (:refused ("base_url" 5) ("sort" "cheapest")))

(defun anyrouter-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun anyrouter-stream ()
  "One whole chat answer, `ok'."
  (make-truncated-sse-stream
   "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
   "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
   "[DONE]"))

(defun anyrouter-context (&optional (session "s1"))
  "A compiled turn context for SESSION, holding one user message, built under
whatever provider specials the caller bound."
  (nle::compile-turn-context
   (nle::snapshot-context-request session :messages (list (nle::message "user" "x")))))

(defmacro with-anyrouter-round ((url headers body) (model &key effort)
                                (&rest section) &body forms)
  "FORMS after one chat round of MODEL at EFFORT, the cell started with
SECTION: URL, HEADERS and BODY (decoded) as dex:post saw them."
  `(with-cell-stop ((anyrouter-start ,@section))
     (let ((nle::*provider* "anyrouter") (nle::*model* ,model) (nle::*api-key* "sk-ar-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* ,effort)
           (nle::*model-capability-memo* nil)
           (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (anyrouter-stream) 200))
         (nle::call-provider-streaming (anyrouter-context)))
       ,@forms)))

;;; --- the catalog, the lane, the address ----------------------------------------------

(deftest anyrouter-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((anyrouter-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "anyrouter")))
      (is (equal "AnyRouter" (nlk:json-value row :string "name")))
      (is (equal "https://anyrouter.dev/api/v1" (nlk:json-value row :string "api")))
      (is (gethash "anyrouter/hermes" (nlk:json-value row :object "models")) "the original's fallback routes are listed")
      (is (find "image" (nle::catalog-model-inputs (gethash "anyrouter/hermes" (nlk:json-value row :object "models"))) :test #'equal) "the original's provider-wide vision is kept")
      (is (equal "openai-completions" (nle::configured-provider-lane "anyrouter")) "the chat lane drives it")
      (is (equal "https://anyrouter.dev/api/v1/chat/completions"
                 (nle::lane-endpoint "anyrouter" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "anyrouter")))))

(deftest anyrouter-cell-base-follows-the-section ()
  (with-cell-stop ((anyrouter-start "base_url" "https://router.example/api/v1"))
    (is (equal "https://router.example/api/v1"
               (nlk:json-value (nle::models-catalog-table) :string "anyrouter" "api")))))

;;; --- the key -------------------------------------------------------------------------

(deftest anyrouter-cell-reads-anyrouter-api-key ()
  (with-cell-stop ((anyrouter-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "ANYROUTER_API_KEY") "sk-ar"))
        (let ((credential (nle::resolve-provider-credential "anyrouter" :auth-path auth :probe t)))
          (is (equal "sk-ar" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest anyrouter-cell-never-sends-another-familys-key ()
  (with-cell-stop ((anyrouter-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "anyrouter" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest anyrouter-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((anyrouter-start))
    (with-temp-auth (auth "{\"api_keys\":{\"anyrouter\":{\"provider\":\"anyrouter\",\"key\":\"sk-ar-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "ANYROUTER_API_KEY") "sk-ar-env"))
        (is (equal "sk-ar-saved"
                   (nle:credential-key (nle::resolve-provider-credential "anyrouter" :auth-path auth :probe t))))))))

;;; --- one round ---------------------------------------------------------------------------

(deftest anyrouter-cell-sends-session-and-preferences ()
  (with-anyrouter-round (url headers body) ("anyrouter/hermes" :effort "high")
      ("only" #("anthropic" "openai") "ignore" #("deepinfra") "order" #("anthropic")
       "sort" "price" "allow_fallbacks" "off" "max_price" 5)
    (is (equal "https://anyrouter.dev/api/v1/chat/completions" url))
    (is (equal "Bearer sk-ar-test" (anyrouter-header headers "authorization")))
    (is (equal "anyrouter/hermes" (nlk:json-value body :string "model")))
    (is (equal "s1" (nlk:json-value body :string "session_id")) "the session id rides for the dashboard")
    (is (equalp #("anthropic" "openai") (nlk:json-value body :array "provider" "only")))
    (is (equalp #("deepinfra") (nlk:json-value body :array "provider" "ignore")))
    (is (equalp #("anthropic") (nlk:json-value body :array "provider" "order")))
    (is (equal "price" (nlk:json-value body :string "provider" "sort")))
    (multiple-value-bind (value present) (nlk:json-value body :any "provider" "allow_fallbacks")
      (is (and present (null value)) "fallbacks off is said outright"))
    (is (= 5 (nlk:json-value body :integer "provider" "max_price")))))

(deftest anyrouter-cell-sends-reasoning-as-its-own-object ()
  (with-anyrouter-round (url headers body) ("anyrouter/hermes" :effort "high") ()
    (is (equal "https://anyrouter.dev/api/v1/chat/completions" url))
    (is (equal "high" (nlk:json-value body :string "reasoning" "effort")) "AnyRouter's own reasoning object")
    (is (null (nlk:json-value body :string "reasoning_effort")) "never the OpenAI spelling")
    (is (null (gethash "provider" body)) "no routing preferences, no provider field")))

(deftest anyrouter-cell-sends-off-as-disabled ()
  (with-anyrouter-round (url headers body) ("x-ai/grok-4.7" :effort "off") ()
    (multiple-value-bind (value present) (nlk:json-value body :any "reasoning" "enabled")
      (is (and present (null value)) "an off selection is {\"enabled\": false}"))))

(deftest anyrouter-cell-leaves-other-providers-alone ()
  (with-cell-stop ((anyrouter-start "sort" "price"))
    (let ((nle::*provider* "openai-completions") (nle::*model* "anyrouter/hermes")
          (nle::*api-key* "sk-openai") (nle::*endpoint* nil)
          (nle::*reasoning-effort* "high") (nle::*model-capability-memo* nil)
          (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (declare (ignore asked))
           (setf body (nlk:decode-json (getf args :content)))
           (values (anyrouter-stream) 200))
        (nle::call-provider-streaming (anyrouter-context)))
      (is (equal "high" (nlk:json-value body :string "reasoning_effort")) "another provider keeps the OpenAI spelling")
      (is (null (nlk:json-value body :string "reasoning")))
      (is (null (nlk:json-value body :string "session_id")))
      (is (null (gethash "provider" body))))))

;;; --- attribution ------------------------------------------------------------------------

(deftest anyrouter-cell-asks-for-attribution-only-when-the-section-does ()
  (with-anyrouter-round (url headers body) ("anyrouter/hermes" :effort "high") ()
    (is (equal "https://anyrouter.dev/api/v1/chat/completions" url))
    (is (null (anyrouter-header headers "X-AnyRouter-Title")) "off by default"))
  (with-anyrouter-round (url headers body) ("anyrouter/hermes" :effort "high") ("attribution" t)
    (is (equal "high" (nlk:json-value body :string "reasoning" "effort")))
    (is (equal "Nodecode" (anyrouter-header headers "X-AnyRouter-Title")))
    (is (equal "cli-agent" (anyrouter-header headers "X-AnyRouter-Source")))
    (is (equal "https://nodecode.ai" (anyrouter-header headers "HTTP-Referer")))))

;;; --- the catalog command --------------------------------------------------------------------

(defparameter *anyrouter-catalog*
  "{\"data\":[{\"id\":\"x-ai/grok-4.7\",\"name\":\"Grok 4.7\",\"capabilities\":[\"function-calling\",\"reasoning\",\"vision\"],\"pricing\":{\"input_per_1m\":3,\"output_per_1m\":15},\"context_length\":256000},{\"model_id\":\"moonshotai/kimi-k3\",\"capabilities\":[\"tools\"],\"supported_parameters\":[\"tools\"],\"pricing\":{\"input_per_1m\":0.6,\"output_per_1m\":2.5}},{\"id\":\"embed-only\",\"capabilities\":[\"embeddings\"],\"pricing\":{\"input_per_1m\":0.02,\"output_per_1m\":0}}]}"
  "A catalog as GET /v1/models answers it: two routes a turn can take, one it
cannot.")

(deftest anyrouter-cell-command-lists-the-tool-capable-routes ()
  (with-cell-stop ((anyrouter-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore options))
                                (is (equal :get method))
                                (is (search "/api/v1/models" url))
                                (values *anyrouter-catalog* 200 nil))
      (let ((answer (nodecode-anyrouter::run-command "" "s1")))
        (is (search "x-ai/grok-4.7" answer))
        (is (search "moonshotai/kimi-k3" answer))
        (is (null (search "embed-only" answer)) "a route that takes no tool calls is left out")
        (is (search "per 1M" answer) "the catalog's rates are shown")
        (let* ((row (nlk:json-value (nle::models-catalog-table) :object "anyrouter"))
               (models (nlk:json-value row :object "models"))
               (price (nle::catalog-model-price (gethash "x-ai/grok-4.7" models))))
          (is (gethash "x-ai/grok-4.7" models) "/models offers it")
          (is (find "image" (nle::catalog-model-inputs (gethash "x-ai/grok-4.7" models)) :test #'equal) "AnyRouter relays an image to whichever upstream serves the route")
          (is (null (gethash "embed-only" models)) "a route that takes no tool calls is not listed")
          (is (and price (= 3 (first price)) (= 15 (second price))) "the catalog's per-million rate is what a round is costed by"))))))

(deftest anyrouter-cell-command-narrows-by-text ()
  (with-cell-stop ((anyrouter-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (values *anyrouter-catalog* 200 nil))
      (let ((answer (nodecode-anyrouter::run-command "grok" "s1")))
        (is (search "x-ai/grok-4.7" answer))
        (is (null (search "moonshotai/kimi-k3" answer)))))))

(deftest anyrouter-cell-command-says-why-the-catalog-did-not-answer ()
  (with-cell-stop ((anyrouter-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (values "nope" 503 nil))
      (is (search "HTTP 503" (nodecode-anyrouter::run-command "" "s1"))))))
