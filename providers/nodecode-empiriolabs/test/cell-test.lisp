;;;; cell-test.lisp --- the empiriolabs cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every catalog GET is a stubbed NLK:HTTP, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every round a stubbed DEX:POST: nothing touches the
;;;; network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "empiriolabs" "EMPIRIOLABS-CELL-" :start nodecode-empiriolabs:start-cell)

(define-cell-lifecycle-tests "empiriolabs"
  (:hooks 'nle::models-catalog-table 'nle::list-provider-models :credential
          'nle::request-body)
  (:refused ("base_url" 5)))

(defparameter +catalog+
  "{\"object\":\"list\",\"data\":[
     {\"id\":\"glm-5-3\",\"display_name\":\"GLM 5.3\",
      \"supported_endpoints\":[\"POST /v1/chat/completions\"],
      \"features\":[\"function_calling\",\"reasoning\"],
      \"context_length\":200000,\"input_modalities\":[\"text\"],
      \"supported_parameters\":[{\"name\":\"reasoning_effort\",\"options\":[\"low\",\"Medium\",\"high\"]}],
      \"pricing\":{\"prompt\":0.0000002,\"completion\":0.0000008}},
     {\"id\":\"qwen3-7-flash\",\"display_name\":\"Qwen3 7 Flash\",
      \"supported_endpoints\":[\"POST /v1/chat/completions\"],
      \"features\":[\"function_calling\"],\"context_length\":262144,
      \"pricing\":{\"prompt\":0.0000001,\"completion\":0.0000002}},
     {\"id\":\"vision-1\",\"supported_endpoints\":[\"POST /v1/chat/completions\"],
      \"features\":[\"function_calling\"],\"context_length\":131072,
      \"input_modalities\":[\"text\",\"image\"],
      \"capabilities\":{\"reasoning\":true},
      \"supported_parameters\":[{\"name\":\"temperature\",\"options\":[]}]},
     {\"id\":\"old-flash\",\"supported_endpoints\":[\"POST /v1/chat/completions\"],
      \"features\":[\"function_calling\"],\"deprecated\":true,\"context_length\":8000},
     {\"id\":\"embed-1\",\"supported_endpoints\":[\"POST /v1/embeddings\"],
      \"features\":[],\"context_length\":8192}
   ]}"
  "The service's catalog shape, with one row of each kind a filter meets.")

(defun header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun list-rows ()
  "The cell's listing, asked the way /connect's key check asks it."
  (nle::list-provider-models "empiriolabs" :key "sk-test"))

(defmacro with-catalog ((&key (status 200) (text '+catalog+) (key "sk-test")) &body body)
  "BODY with every NLK:HTTP stubbed to BODY at STATUS, and the cell's
listing run once with KEY so its catalog is in hand."
  `(with-stubbed-fdefinition (nlk:http (method url &rest args)
                                                              (values ,text ,status))
     (nle::list-provider-models "empiriolabs" :key ,key)
     ,@body))

(defmacro with-empiriolabs-round ((url headers body) (model &key effort catalog) &body forms)
  "FORMS with the cell started and one chat round for empiriolabs MODEL at
EFFORT captured: URL, HEADERS and BODY (the decoded request) as dex:post saw
them. With CATALOG, the cell's listing runs first, so the model's own details
(a start forgets them) are in hand when the round is shaped."
  `(with-cell-stop ((empiriolabs-start))
     (let ((nle::*provider* "empiriolabs") (nle::*model* ,model) (nle::*api-key* "sk-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* ,effort)
           (,url nil) (,headers nil) (,body nil))
       (flet ((send ()
                (with-stubbed-fdefinition
                    (dex:post (asked &rest args)
                     (setf ,url asked ,headers (getf args :headers)
                           ,body (nlk:decode-json (getf args :content)))
                     (values (make-truncated-sse-stream
                              "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                              "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                              "[DONE]")
                             200))
                  (nle::call-provider-streaming (user-context)))))
         (if ,catalog (with-catalog () (send)) (send)))
       ,@forms)))

;;; --- the row, the base, the key -------------------------------------------------

(deftest empiriolabs-cell-puts-its-row-in-the-catalog ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((empiriolabs-start))
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "empiriolabs")))
        (is (equal "EmpirioLabs AI" (nlk:json-value row :string "name")))
        (is (equal "https://api.empiriolabs.ai/v1" (nlk:json-value row :string "api")))
        (is (gethash "glm-5-3" (nlk:json-value row :object "models")) "the fallback ids are listed")
        (is (gethash "qwen3-7-flash" (nlk:json-value row :object "models")))
        (is (equal '("EMPIRIOLABS_API_KEY") (coerce (nlk:json-value row :array "env") 'list)))
        (is (equal "openai-completions" (nle::configured-provider-lane "empiriolabs")) "the chat lane drives it")
        (is (equal "https://api.empiriolabs.ai/v1/chat/completions"
                   (nle::lane-endpoint "empiriolabs" "openai-completions"))))
      (funcall stop)
      (setf stop nil)
      (is (null (nlk:json-value (nle::models-catalog-table) :object "empiriolabs"))))))

(deftest empiriolabs-cell-base-follows-the-section ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((empiriolabs-start "base_url" "https://relay.example/v1"))
      (is (equal "https://relay.example/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "empiriolabs" "api")))
      (is (equal "https://relay.example/v1/models"
                 (nodecode-empiriolabs::catalog-url))))))

(deftest empiriolabs-cell-reads-its-own-key-variable ()
  (with-cell-stop ((empiriolabs-start))
    (with-stubbed-fdefinition (nle::credential-env (name)
                               (and (equal name "EMPIRIOLABS_API_KEY") "sk-empirio"))
      (with-temp-auth (auth "{}")
        (let ((credential (nle::resolve-provider-credential "empiriolabs" :auth-path auth :probe t)))
          (is (equal "sk-empirio" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (let ((credential (nle::resolve-provider-credential "empiriolabs" :auth-path auth :probe t)))
            (is (not (equal "sk-openai" (nle:credential-key credential))) "another family's key is not sent")
            (is (eq :public (nle:credential-source credential)))))))))

;;; --- the listing ----------------------------------------------------------------

(deftest empiriolabs-cell-lists-chat-models-that-take-tools ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((empiriolabs-start))
      (with-catalog ()
        (let ((rows (list-rows)))
          (is (equal '("glm-5-3" "qwen3-7-flash" "vision-1")
                     (mapcar (lambda (row) (getf row :id)) rows)) "the deprecated row and the embedding row are left out")
          (is (equal 200000 (getf (first rows) :context-window)) "its context window travels with it"))))))

(deftest empiriolabs-cell-catalog-details-reach-the-row ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((empiriolabs-start))
      (with-catalog ()
        (let* ((models (nlk:json-value (nle::models-catalog-table) :object "empiriolabs" "models"))
               (glm (gethash "glm-5-3" models))
               (vision (gethash "vision-1" models)))
          (is (equal 200000 (nle::catalog-model-context glm)))
          (is (equal '("low" "medium" "high") (coerce (nle::catalog-model-efforts glm) 'list)) "the model's own rungs, lower-cased")
          (is (nle::catalog-model-reasoning-p glm))
          (is (equal '(0.0000002 0.0000008 nil nil) (nle::catalog-model-price glm)))
          (is (find "image" (nle::catalog-model-inputs vision) :test #'equal) "image input is declared")
          (is (nle::catalog-model-reasoning-p vision)))))))

(deftest empiriolabs-cell-a-plain-listing-is-taken-whole ()
  ;; A base that serves a plain OpenAI-style list has none of the service's
  ;; fields, so nothing is filtered out of it.
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((empiriolabs-start "base_url" "https://relay.example/v1"))
      (with-catalog (:text "{\"data\":[{\"id\":\"m1\"},{\"id\":\"m2\"}]}")
        (is (equal '("m1" "m2") (mapcar (lambda (row) (getf row :id)) (list-rows))))))))

(deftest empiriolabs-cell-a-refused-catalog-says-so ()
  ;; /connect's key check reads a NIL second value as a key the service took,
  ;; so a refusal answers its words.
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-cell-stop ((empiriolabs-start))
      (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                                                      (values "{\"error\":\"invalid key\"}" 401))
        (multiple-value-bind (rows reason) (list-rows)
          (is (null rows))
          (is (equal "HTTP 401" reason)))))))

;;; --- the round ------------------------------------------------------------------

(deftest empiriolabs-cell-sends-the-model-and-the-key ()
  (with-empiriolabs-round (url headers body) ("glm-5-3")
    (is (equal "https://api.empiriolabs.ai/v1/chat/completions" url))
    (is (equal "glm-5-3" (nlk:json-value body :string "model")))
    (is (equal "Bearer sk-test" (header headers "authorization")))
    (is (null (nth-value 1 (gethash "reasoning_effort" body))) "no effort chosen, none sent")))

(deftest empiriolabs-cell-clamps-the-effort-to-the-models-own ()
  (with-stubbed-fdefinition (nle::credential-env (name) nil)
    (with-empiriolabs-round (url headers body) ("glm-5-3" :effort "high" :catalog t)
      (is (equal "high" (nlk:json-value body :string "reasoning_effort"))))
    (with-empiriolabs-round (url headers body) ("glm-5-3" :effort "max" :catalog t)
      (is (equal "high" (nlk:json-value body :string "reasoning_effort")) "the nearest rung the model offers, never a dearer one"))
    (with-empiriolabs-round (url headers body) ("qwen3-7-flash" :effort "high" :catalog t)
      (is (null (nth-value 1 (gethash "reasoning_effort" body))) "a model that declares none is asked for none"))
    (with-empiriolabs-round (url headers body) ("glm-5-3" :effort "minimal" :catalog t)
      (is (equal "low" (nlk:json-value body :string "reasoning_effort")) "a rung below every one it offers takes its weakest"))))
