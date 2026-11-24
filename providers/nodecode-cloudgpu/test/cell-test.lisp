;;;; cell-test.lisp --- the cloudgpu cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post or
;;;; NLE:HTTP: nothing touches the network, the environment or the operator's
;;;; files.

(in-package #:nodecode.test)

(define-test-slice "cloudgpu" "CLOUDGPU-CELL-" :start nodecode-cloudgpu:start-cell)

(define-cell-lifecycle-tests "cloudgpu"
  (:hooks 'nle::models-catalog-table :credential)
  (:command "cloudgpu")
  (:refused ("base_url" 5)))

(defun cloudgpu-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun cloudgpu-context (&optional (session "s1"))
  "A compiled turn context for SESSION, holding one user message, built under
whatever provider specials the caller bound."
  (nle::compile-turn-context
   (nle::snapshot-context-request session :messages (list (nle::message "user" "x")))))

(defmacro with-cloudgpu-round ((url headers body) (model &rest section) &body forms)
  "FORMS after one chat round of MODEL, the cell started with SECTION: URL,
HEADERS and BODY (decoded) as dex:post saw them."
  `(with-cell-stop ((cloudgpu-start ,@section))
     (let ((nle::*provider* "cloudgpu") (nle::*model* ,model) (nle::*api-key* "cgw-sk-test")
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
         (nle::call-provider-streaming (cloudgpu-context)))
       ,@forms)))

;;; --- the catalog, the lane, the address ----------------------------------------------

(deftest cloudgpu-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((cloudgpu-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "cloudgpu")))
      (is (equal "CloudGPU" (nlk:json-value row :string "name")))
      (is (equal "https://cloudgpu.app/v1" (nlk:json-value row :string "api")))
      (is (equal '("CLOUDGPU_API_KEY") (coerce (nlk:json-value row :array "env") 'list)) "the key variable the row names")
      (is (gethash "deepseek-v4.1-flash" (nlk:json-value row :object "models")) "the bundled models are listed")
      (is (gethash "minimax-m3" (nlk:json-value row :object "models")))
      (is (equal "openai-completions" (nle::configured-provider-lane "cloudgpu")) "the chat lane drives it")
      (is (equal "https://cloudgpu.app/v1/chat/completions"
                 (nle::lane-endpoint "cloudgpu" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "cloudgpu")))))

(deftest cloudgpu-cell-base-follows-the-section ()
  (with-cell-stop ((cloudgpu-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "cloudgpu" "api")))))

(deftest cloudgpu-cell-base-env-stands-in-for-the-section ()
  (with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "CLOUDGPU_BASE_URL") "https://mirror.example/v1/"))
    (with-cell-stop ((cloudgpu-start "base_url" "https://cloudgpu.app/v1"))
      (is (equal "https://mirror.example/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "cloudgpu" "api")) "the variable wins, and its trailing slash is trimmed"))))

(deftest cloudgpu-cell-unset-base-env-leaves-the-section-alone ()
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (is (equal "https://relay.example/v1"
               (getf (nodecode-cloudgpu::read-settings
                      (list :base-url "https://relay.example/v1") nil)
                     :base-url)))))

;;; --- the key -------------------------------------------------------------------------

(deftest cloudgpu-cell-reads-cloudgpu-api-key ()
  (with-cell-stop ((cloudgpu-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "CLOUDGPU_API_KEY") "cgw-sk-env"))
        (let ((credential (nle::resolve-provider-credential "cloudgpu" :auth-path auth :probe t)))
          (is (equal "cgw-sk-env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest cloudgpu-cell-never-sends-another-familys-key ()
  (with-cell-stop ((cloudgpu-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "cloudgpu" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest cloudgpu-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((cloudgpu-start))
    (with-temp-auth (auth "{\"api_keys\":{\"cloudgpu\":{\"provider\":\"cloudgpu\",\"key\":\"cgw-sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "CLOUDGPU_API_KEY") "cgw-sk-env"))
        (is (equal "cgw-sk-saved"
                   (nle:credential-key
                    (nle::resolve-provider-credential "cloudgpu" :auth-path auth :probe t))))))))

;;; --- one round -----------------------------------------------------------------------

(deftest cloudgpu-cell-sends-a-round-to-cloudgpu ()
  (with-cloudgpu-round (url headers body) ("kimi-k3")
    (is (equal "https://cloudgpu.app/v1/chat/completions" url))
    (is (equal "Bearer cgw-sk-test" (cloudgpu-header headers "authorization")))
    (is (equal "kimi-k3" (nlk:json-value body :string "model")))))

(deftest cloudgpu-cell-round-follows-a-base-the-section-names ()
  (with-cloudgpu-round (url headers body) ("deepseek-v4.1-flash" "base_url" "https://relay.example/v1")
    (is (equal "https://relay.example/v1/chat/completions" url))
    (is (equal "deepseek-v4.1-flash" (nlk:json-value body :string "model")))))

;;; --- the public model list -----------------------------------------------------------

(defparameter *cloudgpu-models*
  "{\"object\":\"list\",\"data\":[{\"id\":\"deepseek-v4.1-flash\",\"context_window\":131072},{\"id\":\"glm-5.2\"},{\"id\":\"\"},{\"object\":\"model\"}]}"
  "A model list as GET /models answers it: two models, a row with an empty
id, and a row that names nothing.")

(deftest cloudgpu-cell-command-lists-what-the-api-publishes ()
  (with-cell-stop ((cloudgpu-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore options))
                                (is (equal :get method))
                                (is (equal "https://cloudgpu.app/v1/models" url))
                                (values *cloudgpu-models* 200 nil))
      (let ((answer (nodecode-cloudgpu::run-command "" "s1")))
        (is (search "deepseek-v4.1-flash" answer))
        (is (search "glm-5.2" answer))
        (is (gethash "glm-5.2" (nlk:json-value (nle::models-catalog-table)
                                               :object "cloudgpu" "models")) "/models offers it")
        (is (= 131072 (nle::catalog-model-context
                       (gethash "deepseek-v4.1-flash"
                                (nlk:json-value (nle::models-catalog-table)
                                                :object "cloudgpu" "models")))) "the window the list published is kept")))))

(deftest cloudgpu-cell-command-narrows-by-text ()
  (with-cell-stop ((cloudgpu-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (values *cloudgpu-models* 200 nil))
      (let ((answer (nodecode-cloudgpu::run-command "glm" "s1")))
        (is (search "glm-5.2" answer))
        (is (null (search "deepseek" answer)))))))

(deftest cloudgpu-cell-command-keeps-the-bundled-models-when-it-fails ()
  (with-cell-stop ((cloudgpu-start))
    (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
      (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                  (declare (ignore method url options))
                                  (values "nope" 503 nil))
        (let ((answer (nodecode-cloudgpu::run-command "" "s1")))
          (is (search "HTTP 503" answer))
          (is (search "cloudgpu.app/api" answer) "a keyless start is told where a key is made"))
        (is (gethash "deepseek-v4.1-flash" (nlk:json-value (nle::models-catalog-table)
                                                         :object "cloudgpu" "models")))))))
