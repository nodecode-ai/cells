;;;; cell-test.lisp --- the opper cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every round a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "opper" "OPPER-CELL-" :start nodecode-opper:start-cell)

(define-cell-lifecycle-tests "opper"
  (:hooks 'nle::models-catalog-table :credential 'nle::walk-provider-stream)
  (:refused ("base_url" 5)))

(defun opper-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun opper-round (model &key (key "sk-test"))
  "One chat round for opper MODEL on KEY with dex:post stubbed:
(values URL HEADERS BODY) as dex:post saw them."
  (let ((nle::*provider* "opper") (nle::*model* model) (nle::*api-key* key)
        (nle::*endpoint* nil) (url nil) (headers nil) (body nil))
    (with-stubbed-fdefinition
        (dex:post (asked &rest args)
         (setf url asked headers (getf args :headers) body (nlk:decode-json (getf args :content)))
         (values (make-truncated-sse-stream
                  "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                  "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                  "[DONE]")
                 200))
      (nle::call-provider-streaming (user-context)))
    (values url headers body)))

(defun opper-refusal (model key)
  "What a round on KEY refuses with, or NIL when it ran."
  (handler-case
      (progn (let ((nle::*provider* "opper") (nle::*model* model) (nle::*api-key* key)
                    (nle::*endpoint* nil))
               (with-stubbed-fdefinition
                   (dex:post (&rest args)
                    (declare (ignore args))
                    (error "the round reached the socket"))
                 (nle::call-provider-streaming (user-context))))
             nil)
    (nle::provider-config-error (condition) (nle::provider-error-detail condition))))

;;; --- the catalog -----------------------------------------------------------------

(deftest opper-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((opper-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "opper")))
      (is (equal "Opper" (nlk:json-value row :string "name")))
      (is (equal "https://api.opper.ai/v3/compat" (nlk:json-value row :string "api")))
      (is (equal "@ai-sdk/openai-compatible" (nlk:json-value row :string "npm")) "the chat lane's package")
      (is (equal '("OPPER_API_KEY") (coerce (nlk:json-value row :array "env") 'list)) "the key variable the row names")
      (let ((models (nlk:json-value row :object "models")))
        (is (gethash "claude-sonnet-4-6" models) "the plugin's fallback roster is listed")
        (is (gethash "gpt-5.4-mini" models))
        (is (gethash "mistral-large-2512" models))
        (is (null (nle::catalog-model-context (gethash "claude-sonnet-4-6" models))) "no window is published, so none is invented")
        (is (nle::catalog-model-turn-p (gethash "claude-sonnet-4-6" models)) "a turn can run on it"))
      (is (equal "openai-completions" (nle::configured-provider-lane "opper")) "the chat lane drives it")
      (is (equal "https://api.opper.ai/v3/compat/chat/completions"
                 (nle::lane-endpoint "opper" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "opper")))))

(deftest opper-cell-base-follows-the-section ()
  (with-cell-stop ((opper-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "opper" "api")))))

(deftest opper-cell-base-env-stands-in-for-the-section ()
  (with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "OPPER_BASE_URL") "https://eu.example/v3/compat/"))
    (with-cell-stop ((opper-start "base_url" "https://api.opper.ai/v3/compat"))
      (is (equal "https://eu.example/v3/compat"
                 (nlk:json-value (nle::models-catalog-table) :string "opper" "api")) "the variable wins, and its trailing slash is trimmed"))))

(deftest opper-cell-unset-base-env-leaves-the-section-alone ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (is (equal "https://relay.example/v1"
               (getf (nodecode-opper::read-settings
                      (list :base-url "https://relay.example/v1") nil)
                     :base-url)))))

(deftest opper-cell-asks-the-live-catalogue ()
  (with-cell-stop ((opper-start))
    (let ((asked '()))
      (with-stubbed-fdefinition (nle::http-fetch (url &key headers timeout proxy content binary)
                                 (declare (ignore headers timeout proxy content binary))
                                 (push url asked)
                                 (values "{\"data\":[{\"id\":\"claude-opus-5\"},{\"id\":\"azure/gpt-5.5\"}]}" 200))
        (let ((rows (nle::list-provider-models "opper")))
          (is (equal "https://api.opper.ai/v3/compat/models" (car asked)) "the catalogue address the plugin names")
          (is (equal '("azure/gpt-5.5" "claude-opus-5")
                     (sort (mapcar (lambda (row) (getf row :id)) rows) #'string<)) "the ids the gateway publishes, a pinned route included"))))))

;;; --- the key ------------------------------------------------------------------------

(deftest opper-cell-reads-opper-api-key ()
  (with-cell-stop ((opper-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPPER_API_KEY") "op-test"))
        (let ((credential (nle::resolve-provider-credential "opper" :auth-path auth :probe t)))
          (is (equal "op-test" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest opper-cell-never-sends-another-familys-key ()
  (with-cell-stop ((opper-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "opper" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest opper-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((opper-start))
    (with-temp-auth (auth "{\"api_keys\":{\"opper\":{\"provider\":\"opper\",\"key\":\"op-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPPER_API_KEY") "op-test"))
        (is (equal "op-saved"
                   (nle:credential-key (nle::resolve-provider-credential "opper" :auth-path auth :probe t))))))))

;;; --- one round ---------------------------------------------------------------------

(deftest opper-cell-sends-a-chat-round ()
  (with-cell-stop ((opper-start))
    (multiple-value-bind (url headers body) (opper-round "claude-sonnet-4-6")
      (is (equal "https://api.opper.ai/v3/compat/chat/completions" url) "the chat lane's address")
      (is (equal "Bearer sk-test" (opper-header headers "authorization")))
      (is (equal "claude-sonnet-4-6" (nlk:json-value body :string "model")) "a pool name rides the round as it was picked"))))

(deftest opper-cell-sends-a-pinned-route-as-it-is ()
  (with-cell-stop ((opper-start))
    (multiple-value-bind (url headers body) (opper-round "anthropic/claude-sonnet-4-6")
      (declare (ignore headers))
      (is (equal "https://api.opper.ai/v3/compat/chat/completions" url))
      (is (equal "anthropic/claude-sonnet-4-6" (nlk:json-value body :string "model")) "a vendor prefix pins one route and is not rewritten"))))

(deftest opper-cell-refuses-a-round-with-no-key ()
  (with-cell-stop ((opper-start))
    (let ((refusal (opper-refusal "claude-sonnet-4-6" "public")))
      (is (stringp refusal) "a round with no key refuses instead of earning a 401")
      (is (search "OPPER_API_KEY" refusal) "in words that name the variable to set"))))

(deftest opper-cell-leaves-other-providers-alone ()
  (with-cell-stop ((opper-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "gpt-5.5")
          (nle::*api-key* "sk-openai") (nle::*endpoint* nil) (url nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (stringp url) "the other provider dials its own address, not Opper's")
      (is (not (search "opper.ai" url)) "never the gateway Opper's row names")
      (is (equal "gpt-5.5" (nlk:json-value body :string "model")) "its model is not rewritten"))))