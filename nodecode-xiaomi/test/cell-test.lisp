;;;; cell-test.lisp --- the xiaomi cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every round a stubbed dex:post,
;;;; every cluster probe a stubbed dex:get, every listing a stubbed
;;;; NLE::HTTP-FETCH: nothing touches the network, the environment or the
;;;; operator's files.

(in-package #:nodecode.test)

(define-test-slice "xiaomi" "XIAOMI-CELL-" :start nodecode-xiaomi:start-cell)

(define-cell-lifecycle-tests "xiaomi"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body 'nle::walk-provider-stream
          'nle::list-provider-models)
  (:refused ("base_url" 5) ("token_plan_region" "mars")))

(defun xiaomi-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun xiaomi-round (model &key (key "sk-test") effort)
  "One chat round for xiaomi MODEL on KEY at EFFORT, with dex:post stubbed:
(values URL HEADERS BODY) as dex:post saw them."
  (let ((nle::*provider* "xiaomi") (nle::*model* model) (nle::*api-key* key)
        (nle::*endpoint* nil) (nle::*reasoning-effort* effort)
        (url nil) (headers nil) (body nil))
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

(defmacro with-xiaomi-clusters ((probes &rest answers) &body body)
  "BODY with dex:get answering a cluster probe: ANSWERS alternate a cluster
host fragment and the status it answers (any other host answers 401), and
PROBES collects the (URL BEARER) asked, oldest last."
  `(let ((,probes '()))
     (with-stubbed-fdefinition
         (dex:get (url &rest args)
          (push (list url (xiaomi-header (getf args :headers) "authorization")) ,probes)
          (let ((status (or (loop for (host status) on (list ,@answers) by #'cddr
                                  when (search host url) return status)
                            401)))
            (if (= status 200)
                (values "{\"data\":[]}" 200)
                (error 'dex:http-request-failed :status status :body "" :uri url :method :get
                                                :headers (make-hash-table)))))
       ,@body)))

;;; --- the catalog -----------------------------------------------------------------

(deftest xiaomi-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((xiaomi-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "xiaomi")))
      (is (equal "Xiaomi MiMo" (nlk:json-value row :string "name")))
      (is (equal "https://api.xiaomimimo.com/v1" (nlk:json-value row :string "api")))
      (is (gethash "mimo-v2.5" (nlk:json-value row :object "models")) "the bundled models are listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "xiaomi")) "the chat lane drives it")
      (is (equal "https://api.xiaomimimo.com/v1/chat/completions"
                 (nle::lane-endpoint "xiaomi" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "xiaomi"))
        "a stopped cell leaves the catalog as models.dev made it")))

;;; --- the key ------------------------------------------------------------------------

(deftest xiaomi-cell-reads-xiaomi-api-key ()
  (with-cell-stop ((xiaomi-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "XIAOMI_API_KEY") "sk-mimo"))
        (let ((credential (nle::resolve-provider-credential "xiaomi" :auth-path auth :probe t)))
          (is (equal "sk-mimo" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest xiaomi-cell-never-sends-another-familys-key ()
  (with-cell-stop ((xiaomi-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "xiaomi" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential)))
              "the chat family's default variable never reaches Xiaomi")
          (is (eq :public (nle:credential-source credential))))))))

(deftest xiaomi-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((xiaomi-start))
    (with-temp-auth (auth "{\"api_keys\":{\"xiaomi\":{\"provider\":\"xiaomi\",\"key\":\"tp-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "XIAOMI_API_KEY") "sk-mimo"))
        (is (equal "tp-saved"
                   (nle:credential-key (nle::resolve-provider-credential "xiaomi" :auth-path auth :probe t))))))))

;;; --- one round -------------------------------------------------------------------

(deftest xiaomi-cell-sends-a-pay-as-you-go-round ()
  (with-cell-stop ((xiaomi-start))
    (with-xiaomi-clusters (probes)
      (multiple-value-bind (url headers body) (xiaomi-round "mimo-v2.5" :effort "high")
        (is (equal "https://api.xiaomimimo.com/v1/chat/completions" url))
        (is (equal "Bearer sk-test" (xiaomi-header headers "authorization")))
        (is (equal "mimo-v2.5" (nlk:json-value body :string "model")))
        (is (equal "enabled" (nlk:json-value body :string "thinking" "type")) "the zai switch, on")
        (is (null (nth-value 1 (gethash "reasoning_effort" body))) "MiMo takes no reasoning_effort"))
      (is (null probes) "a pay-as-you-go key asks no cluster"))))

(deftest xiaomi-cell-says-thinking-off-the-zai-way ()
  (with-cell-stop ((xiaomi-start))
    (let ((body (nth-value 2 (xiaomi-round "mimo-v2.5-pro" :effort "off"))))
      (is (equal "disabled" (nlk:json-value body :string "thinking" "type"))))
    (let ((body (nth-value 2 (xiaomi-round "mimo-v2.5-pro"))))
      (is (null (nth-value 1 (gethash "thinking" body))) "no effort: the provider's own default"))))

(deftest xiaomi-cell-finds-a-token-plan-keys-cluster ()
  (with-cell-stop ((xiaomi-start))
    (with-xiaomi-clusters (probes "token-plan-ams" 200)
      (let ((url (xiaomi-round "mimo-v2.5" :key "tp-abc")))
        (is (equal "https://token-plan-ams.xiaomimimo.com/v1/chat/completions" url)
            "Singapore refused the key, Amsterdam took it")
        (is (equal '("https://token-plan-sgp.xiaomimimo.com/v1/models"
                     "https://token-plan-ams.xiaomimimo.com/v1/models")
                   (mapcar #'first (reverse probes)))
            "in omp's order, stopping at the first that answers")
        (is (equal "Bearer tp-abc" (second (first probes)))))
      (setf probes '())
      (is (equal "https://token-plan-ams.xiaomimimo.com/v1/chat/completions"
                 (xiaomi-round "mimo-v2.5" :key "tp-abc")))
      (is (null probes) "the cluster found is kept"))))

(deftest xiaomi-cell-sends-a-token-plan-key-to-the-pinned-cluster ()
  (with-cell-stop ((xiaomi-start "token_plan_region" "cn"))
    (with-xiaomi-clusters (probes)
      (is (equal "https://token-plan-cn.xiaomimimo.com/v1/chat/completions"
                 (xiaomi-round "mimo-v2.5" :key "tp-abc")))
      (is (null probes) "a pinned cluster is not asked"))))

(deftest xiaomi-cell-lists-a-token-plan-key-from-its-cluster ()
  (with-cell-stop ((xiaomi-start))
    (let ((asked '()))
      (with-stubbed-fdefinition (nle::http-fetch (url &key headers timeout proxy content binary)
                                 (push (cons url (xiaomi-header headers "authorization")) asked)
                                 (if (search "token-plan-ams" url)
                                     (values "{\"data\":[{\"id\":\"mimo-v2.6-pro\"}]}" 200)
                                     (values nil "HTTP 401")))
        (let ((rows (nle::list-provider-models "xiaomi" :key "tp-xyz")))
          (is (equal '("mimo-v2.6-pro") (mapcar (lambda (row) (getf row :id)) rows)))
          (is (equal '("https://token-plan-sgp.xiaomimimo.com/v1/models"
                       "https://token-plan-ams.xiaomimimo.com/v1/models")
                     (mapcar #'car (reverse asked))))
          (is (equal "Bearer tp-xyz" (cdr (first asked)))))
        (setf asked '())
        (nle::list-provider-models "xiaomi" :key "sk-plain")
        (is (equal '("https://api.xiaomimimo.com/v1/models") (mapcar #'car asked))
            "a pay-as-you-go key lists from the section's base, once"))
      (with-xiaomi-clusters (probes)
        (is (equal "https://token-plan-ams.xiaomimimo.com/v1/chat/completions"
                   (xiaomi-round "mimo-v2.5" :key "tp-xyz"))
            "a round goes where the listing found the key")
        (is (null probes))))))

(deftest xiaomi-cell-leaves-other-providers-alone ()
  (with-cell-stop ((xiaomi-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "mimo-v2.5") (nle::*api-key* "tp-k")
          (nle::*endpoint* nil) (nle::*reasoning-effort* "high") (url nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf url asked body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (not (search "xiaomimimo" url)))
      (is (null (nth-value 1 (gethash "thinking" body)))))))
