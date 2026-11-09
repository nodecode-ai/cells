;;;; cell-test.lisp --- the telnyx cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "telnyx" "TELNYX-CELL-" :start nodecode-telnyx:start-cell)

(define-cell-lifecycle-tests "telnyx"
  (:hooks 'nle::models-catalog-table :credential)
  (:refused ("base_url" 5)))

(defun telnyx-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-telnyx-round ((url headers body) model &body forms)
  "FORMS with the cell started and one chat round for telnyx MODEL captured:
URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-cell-stop ((telnyx-start))
     (let ((nle::*provider* "telnyx") (nle::*model* ,model) (nle::*api-key* "sk-test")
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

(deftest telnyx-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((telnyx-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "telnyx")))
      (is (equal "Telnyx" (nlk:json-value row :string "name")))
      (is (equal "https://api.telnyx.com/v2/ai/openai" (nlk:json-value row :string "api")))
      (is (gethash "moonshotai/Kimi-K3" (nlk:json-value row :object "models")) "curated models listed")
      (is (equal '("TELNYX_API_KEY") (coerce (nlk:json-value row :array "env") 'list)))
      (is (equal "openai-completions" (nle::configured-provider-lane "telnyx")) "chat lane")
      (is (equal "https://api.telnyx.com/v2/ai/openai/chat/completions"
                 (nle::lane-endpoint "telnyx" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "telnyx")))))

(deftest telnyx-cell-base-follows-the-section ()
  (with-cell-stop ((telnyx-start "base_url" "https://relay.example/v2/ai/openai"))
    (is (equal "https://relay.example/v2/ai/openai"
               (nlk:json-value (nle::models-catalog-table) :string "telnyx" "api")))))

(deftest telnyx-cell-reads-telnyx-api-key ()
  (with-cell-stop ((telnyx-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "TELNYX_API_KEY") "KEY-test"))
        (let ((credential (nle::resolve-provider-credential "telnyx" :auth-path auth :probe t)))
          (is (equal "KEY-test" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest telnyx-cell-never-sends-another-familys-key ()
  (with-cell-stop ((telnyx-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "telnyx" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest telnyx-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((telnyx-start))
    (with-temp-auth (auth "{\"api_keys\":{\"telnyx\":{\"provider\":\"telnyx\",\"key\":\"KEY-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "TELNYX_API_KEY") "KEY-env"))
        (is (equal "KEY-saved"
                   (nle:credential-key (nle::resolve-provider-credential "telnyx"
                                                                         :auth-path auth :probe t))))))))

(deftest telnyx-cell-sends-its-model-with-the-bearer-and-no-cap ()
  (with-telnyx-round (url headers body) "moonshotai/Kimi-K3"
    (is (equal "https://api.telnyx.com/v2/ai/openai/chat/completions" url))
    (is (equal "moonshotai/Kimi-K3" (nlk:json-value body :string "model")))
    (is (equal "Bearer sk-test" (telnyx-header headers "authorization")))
    ;; the original's default_max_tokens of None: no cap is volunteered, and
    ;; only an operator ceiling ever sends one
    (is (null (nlk:json-value body :any "max_tokens")) "no output cap by default")))

(deftest telnyx-cell-leaves-other-providers-alone ()
  (with-cell-stop ((telnyx-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "moonshotai/Kimi-K3")
          (nle::*api-key* "k") (nle::*endpoint* nil) (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (is (stringp asked))
           (setf headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (equal "moonshotai/Kimi-K3" (nlk:json-value body :string "model")))
      (is (null (telnyx-header headers "x-title"))))))
