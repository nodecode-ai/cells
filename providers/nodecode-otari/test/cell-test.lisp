;;;; cell-test.lisp --- the otari cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "otari" "OTARI-CELL-" :start nodecode-otari:start-cell)

(define-cell-lifecycle-tests "otari"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body)
  (:refused ("base_url" 5)))

(defun otari-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-otari-round ((url headers body) (model &key effort config) &body forms)
  "FORMS with the cell started on CONFIG (section pairs) and one chat round
for otari MODEL at EFFORT captured: URL, HEADERS and BODY (the decoded
request) as dex:post saw them."
  `(with-cell-stop ((otari-start ,@config))
     (let ((nle::*provider* "otari") (nle::*model* ,model) (nle::*api-key* "sk-otari-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* ,effort)
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
         (nle::call-provider-streaming (user-context)))
       ,@forms)))

;;; --- the catalog -------------------------------------------------------------------

(deftest otari-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((otari-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "otari")))
      (is (equal "Otari" (nlk:json-value row :string "name")))
      (is (equal "https://api.otari.ai/api/v1" (nlk:json-value row :string "api")) "the hosted gateway unless the section says otherwise")
      (is (hash-table-p (nlk:json-value row :object "models")) "no model rows of its own: the gateway's own listing fills them")
      (is (equal "openai-completions" (nle::configured-provider-lane "otari")) "the chat lane drives it")
      (is (equal "https://api.otari.ai/api/v1/chat/completions"
                 (nle::lane-endpoint "otari" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "otari")) "a stop takes the row back out")))

(deftest otari-cell-serves-the-base-the-section-names ()
  (with-cell-stop ((otari-start "base_url" "https://eu.api.otari.ai/api/v1"))
    (is (equal "https://eu.api.otari.ai/api/v1"
               (nlk:json-value (nle::models-catalog-table) :string "otari" "api")) "the EU host is the section's business")
    (is (equal "https://eu.api.otari.ai/api/v1/chat/completions"
               (nle::lane-endpoint "otari" "openai-completions")))))

(deftest otari-cell-refuses-a-base-url-of-the-wrong-shape ()
  (is (signals-error nlk:config-refusal (otari-start "base_url" 5)) "a base_url that is not a string refuses at start")
  (is (null (nlk:json-value (nle::models-catalog-table) :object "otari")) "and installs no row"))

;;; --- the key -----------------------------------------------------------------------

(deftest otari-cell-reads-otari-api-key ()
  (with-cell-stop ((otari-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OTARI_API_KEY") "sk-otari"))
        (let ((credential (nle::resolve-provider-credential "otari" :auth-path auth :probe t)))
          (is (equal "sk-otari" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest otari-cell-never-sends-another-familys-key ()
  (with-cell-stop ((otari-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "otari" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest otari-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((otari-start))
    (with-temp-auth (auth "{\"api_keys\":{\"otari\":{\"provider\":\"otari\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OTARI_API_KEY") "sk-otari"))
        (is (equal "sk-saved"
                   (nle:credential-key (nle::resolve-provider-credential "otari"
                                                                        :auth-path auth :probe t))))))))

;;; --- one round -----------------------------------------------------------------------

(deftest otari-cell-sends-a-round-to-the-gateway ()
  (with-otari-round (url headers body) ("openai/gpt-4o" :config ("base_url" "https://relay.example/api/v1"))
    (is (equal "https://relay.example/api/v1/chat/completions" url) "the section's base")
    (is (equal "Bearer sk-otari-test" (otari-header headers "authorization")))
    (is (equal "openai/gpt-4o" (nlk:json-value body :string "model")) "a routed id goes out as the gateway's catalog spells it")))

(deftest otari-cell-spells-max-the-way-the-gateway-hears-it ()
  (with-otari-round (url headers body) ("openai/gpt-4o" :effort "max")
    (is (equal "xhigh" (nlk:json-value body :string "reasoning_effort")) "Otari's top rung is xhigh"))
  (with-otari-round (url headers body) ("openai/gpt-4o" :effort "medium")
    (is (equal "medium" (nlk:json-value body :string "reasoning_effort")) "another rung goes as it is"))
  (with-otari-round (url headers body) ("openai/gpt-4o" :effort "high")
    (is (equal "high" (nlk:json-value body :string "reasoning_effort")))))

(deftest otari-cell-leaves-reasoning-out-when-there-is-none ()
  (with-otari-round (url headers body) ("openai/gpt-4o")
    (is (null (nth-value 1 (gethash "reasoning_effort" body))) "no effort picked: the gateway's own default, not `none'"))
  (with-otari-round (url headers body) ("openai/gpt-4o" :effort "off")
    (is (null (nth-value 1 (gethash "reasoning_effort" body))) "off is left out too: Otari accepts no `none'")))

(deftest otari-cell-leaves-other-providers-alone ()
  (with-cell-stop ((otari-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "openai/gpt-4o")
          (nle::*api-key* "k") (nle::*endpoint* nil) (nle::*reasoning-effort* "max")
          (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (is (stringp asked))
           (setf body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (equal "max" (nlk:json-value body :string "reasoning_effort")) "another provider keeps the rung it was given"))))
