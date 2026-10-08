;;;; cell-test.lisp --- the cline-pass cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "cline-pass" "CLINE-PASS-CELL-" :start nodecode-cline-pass:start-cell)

(define-cell-lifecycle-tests "cline-pass"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body 'nle::walk-provider-stream)
  (:refused ("base_url" 5)))

(defun header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-cline-pass-round ((url headers body) model &body forms)
  "FORMS with the cell started and one chat round for cline-pass MODEL
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-cell-stop ((cline-pass-start))
     (let ((nle::*provider* "cline-pass") (nle::*model* ,model) (nle::*api-key* "sk-test")
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

(deftest cline-pass-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((cline-pass-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "cline-pass")))
      (is (equal "ClinePass" (nlk:json-value row :string "name")))
      (is (equal "https://api.cline.bot/api/v1" (nlk:json-value row :string "api")))
      (is (gethash "kimi-k3" (nlk:json-value row :object "models")) "the bundled models are listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "cline-pass"))
          "the chat lane drives it")
      (is (equal "https://api.cline.bot/api/v1/chat/completions"
                 (nle::lane-endpoint "cline-pass" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "cline-pass"))
        "a stopped cell leaves the catalog as models.dev made it")))

(deftest cline-pass-cell-base-follows-the-section ()
  (with-cell-stop ((cline-pass-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "cline-pass" "api")))))

(deftest cline-pass-cell-reads-cline-api-key ()
  (with-cell-stop ((cline-pass-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "CLINE_API_KEY") "sk-cline"))
        (let ((credential (nle::resolve-provider-credential "cline-pass" :auth-path auth :probe t)))
          (is (equal "sk-cline" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential)))))
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "CLINE_API_KEY") "sk-cline"))
        (is (not (equal "sk-cline"
                        (nle:credential-key (nle::resolve-provider-credential "anthropic" :auth-path auth :probe t))))
            "another provider's ladder never reads CLINE_API_KEY")))))

(deftest cline-pass-cell-never-sends-another-familys-key ()
  (with-cell-stop ((cline-pass-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "cline-pass" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential)))
              "the chat family's default variable never reaches Cline")
          (is (eq :public (nle:credential-source credential))))))))

(deftest cline-pass-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((cline-pass-start))
    (with-temp-auth (auth "{\"api_keys\":{\"cline-pass\":{\"provider\":\"cline-pass\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "CLINE_API_KEY") "sk-cline"))
        (is (equal "sk-saved"
                   (nle:credential-key (nle::resolve-provider-credential "cline-pass" :auth-path auth :probe t))))))))

(deftest cline-pass-cell-sends-the-wire-id-and-client-headers ()
  (with-cline-pass-round (url headers body) "kimi-k3"
    (is (equal "https://api.cline.bot/api/v1/chat/completions" url))
    (is (equal "cline-pass/kimi-k3" (nlk:json-value body :string "model"))
        "a subscription model goes out under its cline-pass/ id")
    (is (equal "cline-sdk" (header headers "X-CLIENT-TYPE")))
    (is (equal "Cline/3.0.58" (header headers "User-Agent")))
    (is (equal "Bearer sk-test" (header headers "authorization")))))

(deftest cline-pass-cell-sends-a-free-model-as-it-is ()
  (with-cline-pass-round (url headers body) "cline-free/solar-mini4"
    (is (equal "cline-free/solar-mini4" (nlk:json-value body :string "model")))))

(deftest cline-pass-cell-leaves-other-providers-alone ()
  (with-cell-stop ((cline-pass-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "kimi-k3") (nle::*api-key* "k")
          (nle::*endpoint* nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (is (stringp asked))
           (setf headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (equal "kimi-k3" (nlk:json-value body :string "model")))
      (is (null (header headers "X-CLIENT-TYPE"))))))
