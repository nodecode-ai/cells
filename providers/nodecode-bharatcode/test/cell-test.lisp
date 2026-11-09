;;;; cell-test.lisp --- the bharatcode cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, the provider's model list a
;;;; stubbed read, every chat round a stubbed dex:post: nothing touches the
;;;; network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "bharatcode" "BHARATCODE-CELL-" :start nodecode-bharatcode:start-cell)

(defmacro with-stubbed-catalog (&body body)
  "BODY with the provider's /models read stubbed to name nothing, so a start
never dials."
  `(with-stubbed-fdefinition (nodecode-bharatcode::fetch-live-items (base key timeout)
                             (declare (ignore base key timeout))
                             nil)
     ,@body))

(define-cell-lifecycle-tests "bharatcode"
  (:fixture with-stubbed-catalog)
  (:hooks 'nle::models-catalog-table :credential)
  (:refused ("base_url" 5)))

(defun header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-bharatcode-round ((url headers body) model &body forms)
  "FORMS with the cell started and one chat round for bharatcode MODEL
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-stubbed-catalog
     (with-cell-stop ((bharatcode-start))
       (let ((nle::*provider* "bharatcode") (nle::*model* ,model) (nle::*api-key* "sk-test")
             (nle::*endpoint* nil) (,url nil) (,headers nil) (,body nil))
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
           (nle::call-provider-streaming (user-context)))
         ,@forms))))

(deftest bharatcode-cell-puts-its-row-in-the-catalog ()
  (with-stubbed-catalog
    (with-cell-stop ((bharatcode-start))
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "bharatcode")))
        (is (equal "BharatCode" (nlk:json-value row :string "name")))
        (is (equal "https://bharatcode.ai/api/model/v1" (nlk:json-value row :string "api")))
        (is (equalp #("BHARATCODE_API_KEY") (nlk:json-value row :array "env")) "the key variable the original reads")
        (is (gethash "deepseek-v4.1-flash" (nlk:json-value row :object "models")) "the fallback pair is offered while the provider has named nothing")
        (is (equal "openai-completions" (nle::configured-provider-lane "bharatcode")) "the chat lane drives it")
        (is (equal "https://bharatcode.ai/api/model/v1/chat/completions"
                   (nle::lane-endpoint "bharatcode" "openai-completions"))))
      (funcall stop)
      (setf stop nil)
      (is (null (nlk:json-value (nle::models-catalog-table) :object "bharatcode"))))))

(deftest bharatcode-cell-lists-what-the-provider-names ()
  (with-stubbed-catalog
    (with-cell-stop ((bharatcode-start))
      (with-stubbed-fdefinition (nodecode-bharatcode::fetch-live-items (base key timeout)
                                 (declare (ignore base key timeout))
                                 (vector (nlk:json-object "id" "deepseek-v4.1-flash")
                                         (nlk:json-object "id" "sarvam-2")))
        (nodecode-bharatcode::refresh-live))
      (let* ((row (nlk:json-value (nle::models-catalog-table) :object "bharatcode"))
             (models (nlk:json-value row :object "models")))
        (is (gethash "sarvam-2" models) "a model the provider names is listed")
        (is (null (gethash "qwen-3.8-27b" models)) "the fallback pair gives way to the live list")
        (is (equal '("text" "image")
                   (coerce (nle::catalog-model-inputs (gethash "sarvam-2" models)) 'list)) "every row takes images, as the original declares")))))

(deftest bharatcode-cell-reads-the-model-list-from-its-own-base ()
  (let ((url nil) (headers nil))
    (with-stubbed-fdefinition
        (nlk:http (method asked &rest args)
         (declare (ignore method))
         (setf url asked headers (getf args :headers))
         (values "{\"data\":[{\"id\":\"sarvam-2\"}]}" 200))
      (with-stubbed-fdefinition (nodecode-bharatcode::env-key () "bc-key")
        (with-cell-stop ((bharatcode-start))
          (is (equal "https://bharatcode.ai/api/model/v1/models" url))
          (is (equal "Bearer bc-key" (header headers "authorization")) "the account's own key is sent")
          (is (equal '("sarvam-2") (nodecode-bharatcode::live-ids)) "and the start's read keeps it"))))))

(deftest bharatcode-cell-base-follows-the-section ()
  (with-stubbed-catalog
    (with-cell-stop ((bharatcode-start "base_url" "https://relay.example/v1"))
      (is (equal "https://relay.example/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "bharatcode" "api"))))))

(deftest bharatcode-cell-reads-its-own-key ()
  (with-stubbed-catalog
    (with-cell-stop ((bharatcode-start))
      (with-temp-auth (auth "{}")
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "BHARATCODE_API_KEY") "bc-key"))
          (let ((credential (nle::resolve-provider-credential "bharatcode" :auth-path auth :probe t)))
            (is (equal "bc-key" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))))))

(deftest bharatcode-cell-never-sends-another-familys-key ()
  (with-stubbed-catalog
    (with-cell-stop ((bharatcode-start))
      (with-temp-auth (auth "{}")
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (let ((credential (nle::resolve-provider-credential "bharatcode" :auth-path auth :probe t)))
            (is (not (equal "sk-openai" (nle:credential-key credential))))
            (is (eq :public (nle:credential-source credential)))))))))

(deftest bharatcode-cell-sends-a-round-to-its-own-endpoint ()
  (with-bharatcode-round (url headers body) "deepseek-v4.1-flash"
    (is (equal "https://bharatcode.ai/api/model/v1/chat/completions" url))
    (is (equal "Bearer sk-test" (header headers "authorization")))
    (is (equal "deepseek-v4.1-flash" (nlk:json-value body :string "model")) "the model id goes out as the provider spells it")))
