;;;; cell-test.lisp --- the apinex cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "apinex" "APINEX-CELL-" :start nodecode-apinex:start-cell)

(define-cell-lifecycle-tests "apinex"
  (:hooks 'nle::models-catalog-table :credential)
  (:refused ("base_url" 5)))

(defun apinex-row (provider)
  "The catalog row PROVIDER names, as a JSON object, or NIL."
  (nlk:json-value (nle::models-catalog-table) :object provider))

(defun apinex-lists-p (provider model-id)
  "Whether PROVIDER's row lists MODEL-ID."
  (and (gethash model-id (nlk:json-value (apinex-row provider) :object "models")) t))

(deftest apinex-cell-puts-both-rows-in-the-catalog ()
  (with-cell-stop ((apinex-start))
    (is (equal "APInex" (nlk:json-value (apinex-row "apinex") :string "name")))
    (is (equal "APInex (OpenAI-compatible)"
               (nlk:json-value (apinex-row "apinex-openai") :string "name")))
    (is (equal "https://api.apinex.bond/v1" (nlk:json-value (apinex-row "apinex") :string "api")))
    (is (apinex-lists-p "apinex" "claude-sonnet-5.5") "the Claude family is listed")
    (is (apinex-lists-p "apinex-openai" "free/glm-5.3-flash") "the free tier is listed")
    (is (not (apinex-lists-p "apinex" "free/glm-5.3-flash")) "the chat models are not on the Messages wire")
    (is (not (apinex-lists-p "apinex-openai" "claude-sonnet-5.5")) "the Claude models are not on the chat wire")
    (is (equal "anthropic" (nle::configured-provider-lane "apinex")) "the Messages lane drives apinex")
    (is (equal "openai-completions" (nle::configured-provider-lane "apinex-openai")) "the chat lane drives apinex-openai")
    (is (equal "https://api.apinex.bond/v1/messages"
               (nle::lane-endpoint "apinex" "anthropic")))
    (is (equal "https://api.apinex.bond/v1/chat/completions"
               (nle::lane-endpoint "apinex-openai" "openai-completions")))
    (funcall stop)
    (setf stop nil)
    (is (null (apinex-row "apinex")))
    (is (null (apinex-row "apinex-openai")))))

(deftest apinex-cell-leaves-media-and-non-tool-models-out ()
  (with-cell-stop ((apinex-start))
    (loop for model-id in '("free/image-banana-2" "whisper-large-v3" "drex-v2.0" "free/jev-1.13")
          do (is (not (apinex-lists-p "apinex" model-id)) model-id)
             (is (not (apinex-lists-p "apinex-openai" model-id)) model-id))))

(deftest apinex-cell-base-follows-the-section ()
  (with-cell-stop ((apinex-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1" (nlk:json-value (apinex-row "apinex") :string "api")))
    (is (equal "https://relay.example/v1" (nlk:json-value (apinex-row "apinex-openai") :string "api")))
    (is (equal "https://relay.example/v1/messages" (nle::lane-endpoint "apinex" "anthropic")))))

(deftest apinex-cell-reads-apinex-api-key ()
  (with-cell-stop ((apinex-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "APINEX_API_KEY") "sk-apx"))
        (dolist (provider '("apinex" "apinex-openai"))
          (let ((credential (nle::resolve-provider-credential provider :auth-path auth :probe t)))
            (is (equal "sk-apx" (nle:credential-key credential)) provider)
            (is (eq :env (nle:credential-source credential)) provider)))))))

(deftest apinex-cell-never-sends-another-familys-key ()
  (with-cell-stop ((apinex-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                  (cond ((equal name "OPENAI_API_KEY") "sk-openai")
                                        ((equal name "ANTHROPIC_API_KEY") "sk-anthropic")))
        (dolist (provider '("apinex" "apinex-openai"))
          (is (eq :public
                  (nle:credential-source (nle::resolve-provider-credential provider :auth-path auth :probe t)))
              provider))))))

(deftest apinex-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((apinex-start))
    (with-temp-auth (auth "{\"api_keys\":{\"apinex\":{\"provider\":\"apinex\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "APINEX_API_KEY") "sk-apx"))
        (is (equal "sk-saved"
                   (nle:credential-key (nle::resolve-provider-credential "apinex" :auth-path auth :probe t))))))))

(defmacro with-apinex-round ((url headers body) model &body forms)
  "FORMS with the cell started and one chat round for apinex-openai MODEL
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-cell-stop ((apinex-start))
     (let ((nle::*provider* "apinex-openai") (nle::*model* ,model) (nle::*api-key* "sk-test")
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
       ,@forms)))

(deftest apinex-cell-sends-a-chat-round-to-the-gateway ()
  (with-apinex-round (url headers body) "free/glm-5.3-flash"
    (is (equal "https://api.apinex.bond/v1/chat/completions" url))
    (is (equal "free/glm-5.3-flash" (nlk:json-value body :string "model")))
    (is (equal "Bearer sk-test" (cdr (assoc "authorization" headers :test #'string-equal))))))
