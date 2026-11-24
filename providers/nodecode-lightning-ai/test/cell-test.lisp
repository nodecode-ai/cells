;;;; cell-test.lisp --- the lightning-ai cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, the gateway's /models a stubbed
;;;; NLE:HTTP, every chat round a stubbed dex:post: nothing touches the
;;;; network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "lightning-ai" "LIGHTNING-AI-CELL-" :start nodecode-lightning-ai:start-cell)

(define-cell-lifecycle-tests "lightning-ai"
  (:hooks 'nle::models-catalog-table :credential)
  (:help :lightning-ai)
  (:command "lightning-ai")
  (:refused ("base_url" 5)))

(defparameter *lightning-ai-models*
  "{\"object\":\"list\",\"data\":[{\"id\":\"lightning-a\",\"context_window\":131072},{\"id\":\"lightning-b\"},{\"id\":\"\"},{\"object\":\"model\"}]}"
  "A model list as GET /models answers it: two models, a row with an empty
id, and a row that names nothing.")

(defun lightning-ai-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun lightning-ai-context (&optional (session "s1"))
  "A compiled turn context for SESSION, holding one user message, built under
whatever provider specials the caller bound."
  (nle::compile-turn-context
   (nle::snapshot-context-request session :messages (list (nle::message "user" "x")))))

(defmacro with-lightning-ai-round ((url headers body) (model &rest section) &body forms)
  "FORMS after one chat round of MODEL, the cell started with SECTION: URL,
HEADERS and BODY (decoded) as dex:post saw them."
  `(with-cell-stop ((lightning-ai-start ,@section))
     (let ((nle::*provider* "lightningai") (nle::*model* ,model) (nle::*api-key* "ln-sk-test")
           (nle::*endpoint* nil) (nle::*model-capability-memo* nil)
           (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (make-truncated-sse-stream
                     "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                     "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                     "[DONE]")
                    200))
         (nle::call-provider-streaming (lightning-ai-context)))
       ,@forms)))

;;; --- the catalog, the lane, the address ----------------------------------------------

(deftest lightning-ai-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((lightning-ai-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "lightningai")))
      (is (equal "Lightning AI" (nlk:json-value row :string "name")))
      (is (equal "https://platform.lightning.ai/api/v1" (nlk:json-value row :string "api")))
      (is (equal "@ai-sdk/openai-compatible" (nlk:json-value row :string "npm")) "the chat lane's package")
      (is (equal '("LIGHTNING_API_KEY" "LIGHTNING_AI_API_KEY")
                 (coerce (nlk:json-value row :array "env") 'list)) "the key variables the original's env_vars name")
      (is (equal "openai-completions" (nle::configured-provider-lane "lightningai")) "the chat lane drives it")
      (is (equal "https://platform.lightning.ai/api/v1/chat/completions"
                 (nle::lane-endpoint "lightningai" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "lightningai")))))

(deftest lightning-ai-cell-base-follows-the-section ()
  (with-cell-stop ((lightning-ai-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "lightningai" "api")))))

(deftest lightning-ai-cell-base-env-stands-in-for-the-section ()
  (with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "LIGHTNING_BASE_URL") "https://mirror.example/v1/"))
    (with-cell-stop ((lightning-ai-start "base_url" "https://platform.lightning.ai/api/v1"))
      (is (equal "https://mirror.example/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "lightningai" "api")) "the variable wins, and its trailing slash is trimmed"))))

(deftest lightning-ai-cell-unset-base-env-leaves-the-section-alone ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (is (equal "https://relay.example/v1"
               (getf (nodecode-lightning-ai::read-settings
                      (list :base-url "https://relay.example/v1") nil)
                     :base-url)))))

;;; --- the key ------------------------------------------------------------------------

(deftest lightning-ai-cell-reads-lightning-api-key ()
  (with-cell-stop ((lightning-ai-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "LIGHTNING_API_KEY") "ln-sk-env"))
        (let ((credential (nle::resolve-provider-credential "lightningai" :auth-path auth :probe t)))
          (is (equal "ln-sk-env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest lightning-ai-cell-reads-the-other-spelling-too ()
  (with-cell-stop ((lightning-ai-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "LIGHTNING_AI_API_KEY") "ln-sk-alt"))
        (let ((credential (nle::resolve-provider-credential "lightningai" :auth-path auth :probe t)))
          (is (equal "ln-sk-alt" (nle:credential-key credential)) "the profile's second variable")
          (is (eq :env (nle:credential-source credential))))))))

(deftest lightning-ai-cell-never-sends-another-familys-key ()
  (with-cell-stop ((lightning-ai-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "lightningai" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest lightning-ai-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((lightning-ai-start))
    (with-temp-auth (auth "{\"api_keys\":{\"lightningai\":{\"provider\":\"lightningai\",\"key\":\"ln-sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "LIGHTNING_API_KEY") "ln-sk-env"))
        (is (equal "ln-sk-saved"
                   (nle:credential-key
                    (nle::resolve-provider-credential "lightningai" :auth-path auth :probe t))))))))

;;; --- one round ----------------------------------------------------------------------

(deftest lightning-ai-cell-sends-a-round-to-lightning-ai ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (with-lightning-ai-round (url headers body) ("lightning-a")
      (is (equal "https://platform.lightning.ai/api/v1/chat/completions" url))
      (is (equal "Bearer ln-sk-test" (lightning-ai-header headers "authorization")))
      (is (equal "lightning-a" (nlk:json-value body :string "model"))))))

(deftest lightning-ai-cell-round-follows-a-base-the-section-names ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (with-lightning-ai-round (url headers body) ("lightning-b" "base_url" "https://relay.example/v1")
      (is (equal "https://relay.example/v1/chat/completions" url))
      (is (equal "lightning-b" (nlk:json-value body :string "model"))))))

;;; --- the model list the gateway publishes --------------------------------------------

(deftest lightning-ai-cell-start-reads-the-model-list ()
  (with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "LIGHTNING_API_KEY") "ln-sk-env"))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore options))
                                (is (equal :get method))
                                (is (equal "https://platform.lightning.ai/api/v1/models" url))
                                (values *lightning-ai-models* 200 nil))
      (with-cell-stop ((lightning-ai-start))
        (let ((models (nlk:json-value (nle::models-catalog-table) :object "lightningai" "models")))
          (is (gethash "lightning-a" models) "the models the gateway listed are in the picker")
          (is (= 131072 (nle::catalog-model-context (gethash "lightning-a" models))) "the window the list published is kept"))))))

(deftest lightning-ai-cell-start-with-no-key-makes-no-request ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (error "no request belongs on a keyless start"))
      (with-cell-stop ((lightning-ai-start))
        (is (null (nlk:json-value (nle::models-catalog-table)
                                   :object "lightningai" "models" "lightning-a")))))))

(deftest lightning-ai-cell-command-lists-what-the-gateway-publishes ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (with-cell-stop ((lightning-ai-start))
      (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                  (declare (ignore options))
                                  (is (equal :get method))
                                  (is (equal "https://platform.lightning.ai/api/v1/models" url))
                                  (values *lightning-ai-models* 200 nil))
        (let ((answer (nodecode-lightning-ai::run-command "" "s1")))
          (is (search "lightning-a" answer))
          (is (search "lightning-b" answer))
          (is (gethash "lightning-b" (nlk:json-value (nle::models-catalog-table)
                                                    :object "lightningai" "models")) "/models offers it"))))))

(deftest lightning-ai-cell-command-narrows-by-text ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (with-cell-stop ((lightning-ai-start))
      (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                  (declare (ignore method url options))
                                  (values *lightning-ai-models* 200 nil))
        (let ((answer (nodecode-lightning-ai::run-command "lightning-b" "s1")))
          (is (search "lightning-b" answer))
          (is (null (search "lightning-a" answer))))))))

(deftest lightning-ai-cell-command-reads-a-list-with-nothing-in-it ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (with-cell-stop ((lightning-ai-start))
      (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                  (declare (ignore method url options))
                                  (values "{\"object\":\"list\",\"data\":[]}" 200 nil))
        (let ((answer (nodecode-lightning-ai::run-command "" "s1")))
          (is (search "0 models" answer))
          (is (null (search "did not answer" answer)) "an empty list is a read that found nothing"))))))

(deftest lightning-ai-cell-command-says-why-a-read-failed ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (with-cell-stop ((lightning-ai-start))
      (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                  (declare (ignore method url options))
                                  (values "nope" 503 nil))
        (let ((answer (nodecode-lightning-ai::run-command "" "s1")))
          (is (search "HTTP 503" answer))
          (is (search "lightning.ai" answer) "a keyless start is told where a key is made"))))))
