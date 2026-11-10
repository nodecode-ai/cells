;;;; cell-test.lisp --- the openpaths cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, OpenPaths' /models a stubbed
;;;; read, every chat round a stubbed dex:post: nothing touches the network,
;;;; the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "openpaths" "OPENPATHS-CELL-" :start nodecode-openpaths:start-cell)

(define-cell-lifecycle-tests "openpaths"
  (:hooks 'nle::models-catalog-table 'nle::request-body)
  (:refused ("base_url" 5)))

(defvar *round-effort* nil
  "The reasoning effort a round binds, as a session pick or the default would.")

(defmacro with-openpaths-round ((url body) model &body forms)
  "FORMS with the cell started and one chat round for openpaths MODEL
captured: URL and BODY (the decoded request) as dex:post saw them, with
*ROUND-EFFORT* what the round binds as its reasoning effort."
  `(with-cell-stop ((openpaths-start))
     (let ((nle::*provider* "openpaths") (nle::*model* ,model) (nle::*api-key* "sk-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* *round-effort*)
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
         (nle::call-provider-streaming (user-context)))
       ,@forms)))

(deftest openpaths-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((openpaths-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "openpaths")))
      (is (equal "OpenPaths" (nlk:json-value row :string "name")))
      (is (equal "https://openpaths.io/v1" (nlk:json-value row :string "api")))
      (is (equal "@ai-sdk/openai-compatible" (nlk:json-value row :string "npm")) "the chat lane's package")
      (is (equal '("OPENPATHS_API_KEY") (coerce (nlk:json-value row :array "env") 'list)) "its own key variable only")
      (is (gethash "auto-medium-task" (nlk:json-value row :object "models")) "the fallback list, while OpenPaths has said nothing")
      (is (gethash "auto-think" (nlk:json-value row :object "models")) "an auto-think route is offered")
      (is (equal "openai-completions" (nle::configured-provider-lane "openpaths")) "the chat lane drives it")
      (is (equal "https://openpaths.io/v1/chat/completions"
                 (nle::lane-endpoint "openpaths" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "openpaths")))))

(deftest openpaths-cell-base-follows-the-section ()
  (with-cell-stop ((openpaths-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "openpaths" "api")))))

(deftest openpaths-cell-lists-what-openpaths-names ()
  (with-cell-stop ((openpaths-start))
    (with-stubbed-fdefinitions
        ((nodecode-openpaths::credential-key () "sk-test")
         (nodecode-openpaths::fetch-model-ids (base key timeout)
           (vector (nlk:json-object "id" "auto-hard-task")
                   (nlk:json-object "id" "gpt-5.5"))))
      (nodecode-openpaths::refresh-live))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "openpaths"))
           (models (nlk:json-value row :object "models")))
      (is (gethash "auto-hard-task" models) "a route OpenPaths names is listed")
      (is (gethash "gpt-5.5" models) "and a vendor id it names")
      (is (null (gethash "auto-easy-task" models)) "the fallback gives way to the live list")
      (is (equal '("off" "low" "medium" "high")
                 (nle::catalog-model-efforts (gethash "gpt-5.5" models))) "with the rungs it takes"))))

(deftest openpaths-cell-sends-a-level-the-route-takes ()
  (let ((*round-effort* "medium"))
    (with-openpaths-round (url body) "auto-medium-task"
      (is (equal "https://openpaths.io/v1/chat/completions" url))
      (is (equal "medium" (nlk:json-value body :string "reasoning_effort"))))))

(deftest openpaths-cell-lowers-a-level-the-route-does-not-take ()
  (let ((*round-effort* "minimal"))
    (with-openpaths-round (url body) "gpt-5.5"
      (is (equal "low" (nlk:json-value body :string "reasoning_effort")) "below the floor is the floor, never above")))
  (let ((*round-effort* "xhigh"))
    (with-openpaths-round (url body) "gpt-5.5"
      (is (equal "high" (nlk:json-value body :string "reasoning_effort")) "xhigh is sent as high, the original's own mapping")))
  (let ((*round-effort* "max"))
    (with-openpaths-round (url body) "gpt-5.5"
      (is (equal "high" (nlk:json-value body :string "reasoning_effort")) "and max is its top"))))

(deftest openpaths-cell-turns-reasoning-off-as-none ()
  (let ((*round-effort* "off"))
    (with-openpaths-round (url body) "claude-sonnet-4-6"
      (is (equal "none" (nlk:json-value body :string "reasoning_effort")) "the wire's own spelling of disabled"))))

(deftest openpaths-cell-sends-auto-for-an-auto-think-route-with-no-pick ()
  (let ((*round-effort* nil))
    (with-openpaths-round (url body) "auto-think"
      (is (equal "auto" (nlk:json-value body :string "reasoning_effort")) "an auto-think route thinks by default"))))

(deftest openpaths-cell-omits-reasoning-when-none-was-picked ()
  (let ((*round-effort* nil))
    (with-openpaths-round (url body) "gpt-5.5"
      (is (null (gethash "reasoning_effort" body)) "nothing is sent, so OpenPaths' default stands"))))

(deftest openpaths-cell-leaves-other-providers-alone ()
  (with-cell-stop ((openpaths-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "auto-think") (nle::*api-key* "k")
          (nle::*endpoint* nil) (nle::*reasoning-effort* nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (setf body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (null (gethash "reasoning_effort" body)) "an auto-think id on another provider is not sent auto"))))

(deftest openpaths-cell-reads-openpaths-api-key ()
  (with-cell-stop ((openpaths-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENPATHS_API_KEY") "sk-openpaths"))
        (let ((credential (nle::resolve-provider-credential "openpaths" :auth-path auth :probe t)))
          (is (equal "sk-openpaths" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest openpaths-cell-never-sends-another-familys-key ()
  (with-cell-stop ((openpaths-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "openpaths" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))
