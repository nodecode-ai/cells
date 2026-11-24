;;;; cell-test.lisp --- the crowbot cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post or
;;;; NLE:HTTP: nothing touches the network, the environment or the operator's
;;;; files.

(in-package #:nodecode.test)

(define-test-slice "crowbot" "CROWBOT-CELL-" :start nodecode-crowbot:start-cell)

(define-cell-lifecycle-tests "crowbot"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body)
  (:command "crowbot")
  (:refused ("base_url" 5) ("context_tokens" 0) ("default_effort" "extreme")))

(defmacro with-crowbot-round ((url body) (effort &rest section) &body forms)
  "FORMS after one chat round of kimi-k3 at EFFORT, the cell started with
SECTION: URL and BODY (decoded) as dex:post saw them."
  `(with-cell-stop ((crowbot-start ,@section))
     (let ((nle::*provider* "crowbot") (nle::*model* "kimi-k3")
           (nle::*api-key* "1234567890123456") (nle::*endpoint* nil)
           (nle::*reasoning-effort* ,effort) (nle::*model-capability-memo* nil)
           (,url nil) (,body nil))
       (declare (ignorable ,url ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,body (nlk:decode-json (getf args :content)))
            (values (make-truncated-sse-stream
                     "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
                     "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                     "[DONE]")
                    200))
         (nle::call-provider-streaming
          (nle::compile-turn-context
           (nle::snapshot-context-request "s1" :messages (list (nle::message "user" "x"))))))
       ,@forms)))

;;; --- the catalog, the lane, the address ----------------------------------------------

(deftest crowbot-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((crowbot-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "crowbot")))
      (is (equal "Crowbot" (nlk:json-value row :string "name")))
      (is (equal "https://api.crowbot.sh/v1" (nlk:json-value row :string "api")))
      (is (gethash "kimi-k3" (nlk:json-value row :object "models")) "the original's model is listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "crowbot")) "the chat lane drives it")
      (is (equal "https://api.crowbot.sh/v1/chat/completions"
                 (nle::lane-endpoint "crowbot" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "crowbot")))))

(deftest crowbot-cell-base-follows-the-section ()
  (with-cell-stop ((crowbot-start "base_url" "https://relay.example/api/v1"))
    (is (equal "https://relay.example/api/v1"
               (nlk:json-value (nle::models-catalog-table) :string "crowbot" "api")))))

(deftest crowbot-cell-offers-the-whole-reasoning-ladder ()
  (with-cell-stop ((crowbot-start))
    (let ((ladder (nle::effort-ladder "crowbot" "kimi-k3")))
      (is (member "max" ladder :test #'equal) "the relay's top rung is offered, not clamped away")
      (is (member "xhigh" ladder :test #'equal))
      (is (member "off" ladder :test #'equal) "and off, so a round can spend nothing")
      (is (not (member "none" ladder :test #'equal)) "the wire's `none' is the core's `off'"))))

;;; --- the key -------------------------------------------------------------------------

(deftest crowbot-cell-reads-the-account-number-variable ()
  (with-cell-stop ((crowbot-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "CROWBOT_API_KEY") "1234567890123456"))
        (let ((credential (nle::resolve-provider-credential "crowbot" :auth-path auth :probe t)))
          (is (equal "1234567890123456" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest crowbot-cell-reads-the-legacy-variable ()
  (with-cell-stop ((crowbot-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "HERMES_CUSTOM_API_CROWBOT_SH_API_KEY")
                                      "1234567890123456"))
        (is (equal "1234567890123456"
                   (nle:credential-key
                    (nle::resolve-provider-credential "crowbot" :auth-path auth :probe t))))))))

(deftest crowbot-cell-never-sends-another-familys-key ()
  (with-cell-stop ((crowbot-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "crowbot" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

;;; --- the round's effort ------------------------------------------------------------------

(deftest crowbot-cell-sends-medium-when-nothing-was-picked ()
  (with-crowbot-round (url body) (nil)
    (is (equal "https://api.crowbot.sh/v1/chat/completions" url))
    (is (equal "kimi-k3" (nlk:json-value body :string "model")))
    (is (equal "medium" (nlk:json-value body :string "reasoning_effort")) "the relay's own ceiling is not inherited")))


(deftest crowbot-cell-default-follows-the-section ()
  (with-crowbot-round (url body) (nil "default_effort" "low")
    (is (equal "low" (nlk:json-value body :string "reasoning_effort"))))
  (with-crowbot-round (url body) (nil "default_effort" "endpoint")
    (multiple-value-bind (value present) (nlk:json-value body :any "reasoning_effort")
      (is (and (null value) (not present)) "endpoint leaves the field off the wire"))))

(deftest crowbot-cell-keeps-the-operators-pick ()
  (with-crowbot-round (url body) ("max")
    (is (equal "max" (nlk:json-value body :string "reasoning_effort")) "a pick of the top rung rides as it is"))

  (with-crowbot-round (url body) ("off")
    (multiple-value-bind (value present) (nlk:json-value body :any "reasoning_effort")
      (is (and (null value) (not present)) "off is not turned back into medium"))))

(deftest crowbot-cell-leaves-other-providers-alone ()
  (with-cell-stop ((crowbot-start))
   (let ((nle::*provider* "openai-completions") (nle::*model* "gpt-x") (nle::*api-key* "k")
        (nle::*endpoint* nil) (nle::*reasoning-effort* nil) (nle::*model-capability-memo* nil)
        (body nil))
    (with-stubbed-fdefinition
        (dex:post (asked &rest args)
         (declare (ignore asked))
         (setf body (nlk:decode-json (getf args :content)))
         (values (make-truncated-sse-stream
                  "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                 200))
      (nle::call-provider-streaming
       (nle::compile-turn-context
        (nle::snapshot-context-request "s1" :messages (list (nle::message "user" "x"))))))
    (multiple-value-bind (value present) (nlk:json-value body :any "reasoning_effort")
      (is (and (null value) (not present)) "another provider's round gains no crowbot default")))))

;;; --- the model list -----------------------------------------------------------------------

(defparameter *crowbot-catalog*
  "{\"data\":[{\"id\":\"kimi-k3\"},{\"id\":\"glm-5\"},{\"id\":\"grok-4.7\"}]}"
  "A model list as GET /models answers it.")

(deftest crowbot-cell-command-lists-and-offers-the-models ()
  (with-cell-stop ((crowbot-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore options))
                                (is (eq :get method))
                                (is (search "/v1/models" url))
                                (values *crowbot-catalog* 200 nil))
      (let ((answer (nodecode-crowbot::run-command "" "s1")))
        (is (search "glm-5" answer))
        (is (search "3 models listed" answer))
        (let ((models (nlk:json-value (nle::models-catalog-table) :object "crowbot" "models")))
          (is (gethash "glm-5" models) "/models offers what the relay listed")
          (is (gethash "kimi-k3" models)))))))

(deftest crowbot-cell-command-narrows-by-text ()
  (with-cell-stop ((crowbot-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (values *crowbot-catalog* 200 nil))
      (let ((answer (nodecode-crowbot::run-command "grok" "s1")))
        (is (search "grok-4.7" answer))
        (is (null (search "glm-5" answer)))))))

(deftest crowbot-cell-command-says-why-the-list-did-not-answer ()
  (with-cell-stop ((crowbot-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (values "nope" 503 nil))
      (is (search "HTTP 503" (nodecode-crowbot::run-command "" "s1"))))))
