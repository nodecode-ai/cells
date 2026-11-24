;;;; cell-test.lisp --- the liquid cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every wire to the exchange a stubbed dex:post: the
;;;; test run has no network, and nothing here dials one.

(in-package #:nodecode.test)

(define-test-slice "liquid" "LIQUID-CELL-" :start nodecode-liquid:start-cell)

(define-cell-lifecycle-tests "liquid"
  (:hooks 'nle::models-catalog-table :credential 'nle::walk-provider-stream)
  (:command "liquid")
  (:help :liquid)
  ;; a cap the exchange would not read as money leaves every offer ineligible,
  ;; or the bill uncapped: the cell refuses rather than send either
  (:refused ("cap_usd" "abc") ("cap_usd" "0.00")))

(defun liquid-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun liquid-stream ()
  "One whole chat answer, `ok'."
  (make-truncated-sse-stream
   "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
   "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
   "[DONE]"))

(defmacro with-liquid-round ((url headers body) (model &key effort (provider "liquid"))
                             (&rest section) &body forms)
  "FORMS after one chat round of PROVIDER's MODEL at EFFORT, the cell started
with SECTION: URL, HEADERS and BODY (decoded) as dex:post saw them."
  `(with-cell-stop ((liquid-start ,@section))
     (let ((nle::*provider* ,provider) (nle::*model* ,model) (nle::*api-key* "sk-liquid-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* ,effort)
           (nle::*max-completion-tokens* nil) (nle::*model-capability-memo* nil)
           (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition (dex:post (asked &rest args)
                                 (setf ,url asked ,headers (getf args :headers)
                                       ,body (nlk:decode-json (getf args :content)))
                                 (values (liquid-stream) 200))
         (nle::call-provider-streaming (user-context)))
       ,@forms)))

(defparameter *liquid-models-dev*
  "{\"liquid\":{\"id\":\"liquid\",\"name\":\"Liquid Inference\",\"npm\":\"@ai-sdk/openai-compatible\",\"api\":\"https://router.inference.ai.exchange/v1\",\"env\":[\"LIQUID_API_KEY\"],\"models\":{\"liquid.auto\":{\"id\":\"liquid.auto\",\"name\":\"Liquid Auto\",\"reasoning\":true,\"tool_call\":true,\"limit\":{\"context\":10000000,\"output\":32000}}}}}"
  "A models.dev document holding Liquid Inference as models.dev publishes it.")

;;; --- the catalog, the lane, the address ----------------------------------------------

(deftest liquid-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((liquid-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "liquid")))
      (is (equal "Liquid Inference" (nlk:json-value row :string "name")))
      (is (equal "https://router.inference.ai.exchange/v1" (nlk:json-value row :string "api")))
      (is (equalp #("LIQUID_API_KEY") (nlk:json-value row :array "env")))
      (is (gethash "liquid.auto" (nlk:json-value row :object "models")) "the exchange's model is listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "liquid")) "the chat lane drives it")
      (is (equal "https://router.inference.ai.exchange/v1/chat/completions"
                 (nle::lane-endpoint "liquid" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "liquid")))))

(deftest liquid-cell-keeps-what-models-dev-makes-of-it ()
  (with-catalog-fixture (catalog *liquid-models-dev*)
    (with-cell-stop ((liquid-start "base_url" "https://relay.example/v1"))
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "liquid")))
        (is (gethash "liquid.auto" (nlk:json-value row :object "models")) "models.dev's model row is kept")
        (is (equal "@ai-sdk/openai-compatible" (nlk:json-value row :string "npm")) "and its lane")
        (is (equal "https://relay.example/v1" (nlk:json-value row :string "api")) "the section's base reaches the row"))
      (let ((nle::*provider* "liquid") (nle::*model* "liquid.auto") (nle::*api-key* nil)
            (nle::*endpoint* nil) (nle::*model-capability-memo* nil)
            (nle::*auth-file-path* (merge-pathnames "no-such-auth.json" (uiop:temporary-directory)))
            (seen-url nil) (seen-headers nil) (seen-body nil))
        (with-stubbed-fdefinitions
            ((nle::credential-env (name) (and (equal name "LIQUID_API_KEY") "sk-liquid-env"))
             (dex:post (asked &rest args)
               (setf seen-url asked seen-headers (getf args :headers)
                     seen-body (nlk:decode-json (getf args :content)))
               (values (liquid-stream) 200)))
          (nle::call-provider-streaming (user-context)))
        (is (equal "https://relay.example/v1/chat/completions" seen-url))
        (is (equal "Bearer sk-liquid-env" (liquid-header seen-headers "authorization")))
        (is (equal "liquid.auto" (nlk:json-value seen-body :string "model")))))))

;;; --- the key -----------------------------------------------------------------------------

(deftest liquid-cell-reads-liquid-api-key ()
  (with-cell-stop ((liquid-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "LIQUID_API_KEY") "sk-liquid-env"))
        (let ((credential (nle::resolve-provider-credential "liquid" :auth-path auth :probe t)))
          (is (equal "sk-liquid-env" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest liquid-cell-never-sends-another-familys-key ()
  (with-cell-stop ((liquid-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "liquid" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest liquid-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((liquid-start))
    (with-temp-auth (auth "{\"api_keys\":{\"liquid\":{\"provider\":\"liquid\",\"key\":\"sk-liquid-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "LIQUID_API_KEY") "sk-liquid-env"))
        (is (equal "sk-liquid-saved"
                   (nle:credential-key (nle::resolve-provider-credential "liquid" :auth-path auth :probe t))))))))

;;; --- one round --------------------------------------------------------------------------

(deftest liquid-cell-a-round-names-its-client-and-its-cap ()
  (with-liquid-round (url headers body) ("liquid.auto") ()
    (is (equal "https://router.inference.ai.exchange/v1/chat/completions" url))
    (is (equal "Bearer sk-liquid-test" (liquid-header headers "authorization")))
    (is (equal "Nodecode" (liquid-header headers "X-Title")) "the client the request comes from")
    (is (equal "1.00" (liquid-header headers "x-liquid-cap-usd")) "one USD a request, by default")
    (is (equal "liquid.auto" (nlk:json-value body :string "model")))
    (is (null (nth-value 1 (gethash "reasoning_effort" body))) "no level picked, no reasoning_effort")))

(deftest liquid-cell-the-section-names-the-cap-and-a-level ()
  (with-liquid-round (url headers body) ("liquid.auto" :effort "high") ("cap_usd" "0.25")
    (is (equal "0.25" (liquid-header headers "x-liquid-cap-usd")) "the section's cap rides the request")
    (is (equal "high" (nlk:json-value body :string "reasoning_effort")) "a picked level rides as a top-level effort")))

(deftest liquid-cell-leaves-other-providers-alone ()
  (with-liquid-round (url headers body) ("gpt-5.5" :effort "high" :provider "openai-completions") ()
    (is (equal "high" (nlk:json-value body :string "reasoning_effort")))
    (is (null (liquid-header headers "X-Title")))
    (is (null (liquid-header headers "x-liquid-cap-usd")))))

;;; --- /liquid ----------------------------------------------------------------------------

(deftest liquid-cell-answers-its-command ()
  (with-cell-stop ((liquid-start))
    (with-stubbed-fdefinition (nle::credential-env (name)
                               (and (equal name "LIQUID_API_KEY") "sk-liquid-env"))
      (let ((answer (cell-entry "nodecode-liquid" "liquid" "")))
        (is (search "https://router.inference.ai.exchange/v1" answer) "the base, as it stands")
        (is (search "1.00" answer) "the cap, as it stands")
        (is (search "LIQUID_API_KEY" answer) "and where the key came from"))))
  (with-cell-stop ((liquid-start))
    (with-stubbed-fdefinition (nle::credential-env (name) nil)
      (is (search "No key yet" (cell-entry "nodecode-liquid" "liquid" "")) "a missing key is said"))))
