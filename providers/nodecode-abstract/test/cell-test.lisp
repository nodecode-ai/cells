;;;; cell-test.lisp --- the abstract cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every round a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "abstract" "ABSTRACT-CELL-" :start nodecode-abstract:start-cell)

(define-cell-lifecycle-tests "abstract"
  (:hooks 'nle::models-catalog-table 'nle::list-provider-models :credential
          'nle::walk-provider-stream)
  (:refused ("base_url" 5)))

(defun abstract-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun abstract-round (model &key (key "sk-test"))
  "One Responses round for abstract MODEL on KEY with dex:post stubbed:
(values URL HEADERS BODY) as dex:post saw them."
  (let ((nle::*provider* "abstract") (nle::*model* model) (nle::*api-key* key)
        (nle::*endpoint* nil) (url nil) (headers nil) (body nil))
    (with-stubbed-fdefinition
        (dex:post (asked &rest args)
         (setf url asked headers (getf args :headers) body (nlk:decode-json (getf args :content)))
         (values (make-truncated-sse-stream
                  "{\"type\":\"response.output_text.delta\",\"item_id\":\"m\",\"delta\":\"ok\"}"
                  "{\"type\":\"response.completed\",\"response\":{}}")
                 200))
      (nle::call-responses-streaming (user-context)))
    (values url headers body)))

(defun abstract-refusal (model key)
  "What a round on KEY refuses with, or NIL when it ran."
  (handler-case
      (progn (let ((nle::*provider* "abstract") (nle::*model* model) (nle::*api-key* key)
                    (nle::*endpoint* nil))
               (with-stubbed-fdefinition
                   (dex:post (&rest args)
                    (declare (ignore args))
                    (error "the round reached the socket"))
                 (nle::call-responses-streaming (user-context))))
             nil)
    (nle::provider-config-error (condition) (nle::provider-error-detail condition))))

;;; --- the catalog -----------------------------------------------------------------

(deftest abstract-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((abstract-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "abstract")))
      (is (equal "Abstract" (nlk:json-value row :string "name")))
      (is (equal "https://api.abstract.inc/v1" (nlk:json-value row :string "api")))
      (is (equal "@ai-sdk/openai" (nlk:json-value row :string "npm")) "the Responses lane's package")
      (let ((models (nlk:json-value row :object "models")))
        (is (gethash "glm-5.3:superfast" models) "the published roster is listed")
        (is (gethash "deepseek-v4-flash:superfast" models))
        (is (gethash "kimi-k3:superfast" models))
        (is (= 65536 (nle::catalog-model-output (gethash "kimi-k3:superfast" models))) "the output cap the API documents")
        (is (null (nle::catalog-model-context (gethash "kimi-k3:superfast" models))) "no window is published, so none is invented")
        (is (equal '(3.0 12.75 0.3 nil) (nle::catalog-model-price (gethash "kimi-k3:superfast" models))))
        (is (nle::catalog-model-turn-p (gethash "glm-5.3:superfast" models)) "a turn can run on it"))
      (is (equal "openai-responses" (nle::configured-provider-lane "abstract")) "the Responses lane drives it")
      (is (equal "https://api.abstract.inc/v1/responses"
                 (nle::lane-endpoint "abstract" "openai-responses"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "abstract")))))

(deftest abstract-cell-a-relay-base-names-the-endpoint ()
  (with-cell-stop ((abstract-start "base_url" "https://relay.example/v1"))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "abstract")))
      (is (equal "https://relay.example/v1" (nlk:json-value row :string "api")))
      (is (equal "https://relay.example/v1/responses"
                 (nle::lane-endpoint "abstract" "openai-responses"))))))

(deftest abstract-cell-lists-its-roster-without-a-request ()
  (with-cell-stop ((abstract-start))
    (let ((asked '()))
      (with-stubbed-fdefinition (nle::http-fetch (url &key headers timeout proxy content binary)
                                 (push url asked)
                                 (values "{\"data\":[]}" 200))
        (multiple-value-bind (rows why) (nle::list-provider-models "abstract" :key "ab-test")
          (is (equal '("glm-5.3:superfast" "deepseek-v4-flash:superfast" "kimi-k3:superfast")
                     (mapcar (lambda (row) (getf row :id)) rows)) "the static roster, in the plugin's order")
          (is (equal "GLM-5.3" (getf (first rows) :display)))
          (is (stringp why) "a key check is told the key was not checked"))
        (is (equal '("glm-5.3:superfast" "deepseek-v4-flash:superfast" "kimi-k3:superfast")
                   (mapcar (lambda (row) (getf row :id)) (nle::list-provider-models "abstract"))) "no key asks the same roster")
        (is (null asked) "the roster is answered without a request")))))

;;; --- the key ------------------------------------------------------------------------

(deftest abstract-cell-reads-abstract-api-key ()
  (with-cell-stop ((abstract-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "ABSTRACT_API_KEY") "ab-test"))
        (let ((credential (nle::resolve-provider-credential "abstract" :auth-path auth :probe t)))
          (is (equal "ab-test" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest abstract-cell-never-sends-another-familys-key ()
  (with-cell-stop ((abstract-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "abstract" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest abstract-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((abstract-start))
    (with-temp-auth (auth "{\"api_keys\":{\"abstract\":{\"provider\":\"abstract\",\"key\":\"ab-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "ABSTRACT_API_KEY") "ab-test"))
        (is (equal "ab-saved"
                   (nle:credential-key (nle::resolve-provider-credential "abstract" :auth-path auth :probe t))))))))

;;; --- one round -------------------------------------------------------------------

(deftest abstract-cell-sends-a-responses-round ()
  (with-cell-stop ((abstract-start))
    (multiple-value-bind (url headers body) (abstract-round "glm-5.3:superfast")
      (is (equal "https://api.abstract.inc/v1/responses" url) "the Responses lane's address")
      (is (equal "Bearer sk-test" (abstract-header headers "authorization")))
      (is (equal "glm-5.3:superfast" (nlk:json-value body :string "model")) "the wire id is the id the picker shows"))))

(deftest abstract-cell-refuses-a-round-with-no-key ()
  (with-cell-stop ((abstract-start))
    (let ((refusal (abstract-refusal "kimi-k3:superfast" "public")))
      (is (stringp refusal) "a round with no key refuses instead of earning a 401")
      (is (search "ABSTRACT_API_KEY" refusal) "in words that name the variable to set"))))

(deftest abstract-cell-leaves-other-providers-alone ()
  (with-cell-stop ((abstract-start))
    (let ((nle::*provider* "openai-responses") (nle::*model* "glm-5.3:superfast")
          (nle::*api-key* "public") (nle::*endpoint* nil) (url nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"type\":\"response.completed\",\"response\":{}}")
                   200))
        (nle::call-responses-streaming (user-context)))
      (is (equal "https://api.openai.com/v1/responses" url) "the other lane keeps its own address")
      (is (equal "glm-5.3:superfast" (nlk:json-value body :string "model"))))))
