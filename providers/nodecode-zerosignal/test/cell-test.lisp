;;;; cell-test.lisp --- the zerosignal cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, the proxy's catalog a stubbed
;;;; read, every chat round a stubbed dex:post: nothing touches the network,
;;;; the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "zerosignal" "ZEROSIGNAL-CELL-" :start nodecode-zerosignal:start-cell)

(define-cell-lifecycle-tests "zerosignal"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body)
  (:refused ("base_url" 5)))

(defun zerosignal-catalog-payload (&rest models)
  "A proxy /models body naming MODELS, each (ID . LEVELS) or (ID . :NONE)."
  (nlk:json-object
   "data"
   (coerce
    (loop for (id . levels) in models
          collect (nlk:json-object
                   "id" id
                   "reasoning" (if (eq levels :none)
                                   (nlk:json-object "supported" :false)
                                   (nlk:json-object "supported" t
                                                    "allowed_efforts" (coerce levels 'vector)))))
    'vector)))

(defun zerosignal-ask-proxy (payload)
  "Make the cell's catalog read answer PAYLOAD, as {base}/models would."
  (with-stubbed-fdefinition (nodecode-zerosignal::fetch-live-items (base key timeout)
                             (declare (ignore base key timeout))
                             (nlk:json-value payload :array "data"))
    (nodecode-zerosignal::refresh-live)))

(defvar *round-effort* "medium"
  "The reasoning effort a round binds, as a session pick would.")

(defvar *round-payload* nil
  "The proxy catalog a round seeds after the cell starts: a round starts the
cell, and a start forgets whatever map stood before it.")

(defun header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-zerosignal-round ((url headers body) model &body forms)
  "FORMS with the cell started and one chat round for zerosignal MODEL
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them.
*ROUND-PAYLOAD*, when bound, is the proxy catalog seeded in the started cell."
  `(with-cell-stop ((zerosignal-start))
     (when *round-payload* (zerosignal-ask-proxy *round-payload*))
     (let ((nle::*provider* "zerosignal") (nle::*model* ,model) (nle::*api-key* "sk-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* *round-effort*)
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

(deftest zerosignal-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((zerosignal-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "zerosignal")))
      (is (equal "ZeroSignal" (nlk:json-value row :string "name")))
      (is (equal "http://127.0.0.1:9376/v1" (nlk:json-value row :string "api")))
      (is (gethash "glm-5.3" (nlk:json-value row :object "models")) "the curated fallback is listed while the proxy has said nothing")
      (is (equal "openai-completions" (nle::configured-provider-lane "zerosignal")) "the chat lane drives it")
      (is (equal "http://127.0.0.1:9376/v1/chat/completions"
                 (nle::lane-endpoint "zerosignal" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "zerosignal")))))

(deftest zerosignal-cell-lists-what-the-proxy-names ()
  (with-cell-stop ((zerosignal-start))
    (zerosignal-ask-proxy (zerosignal-catalog-payload
                            '("kimi-k3" . ("low" "high" "max"))
                            '("glm-5.2" . :none)))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "zerosignal"))
           (models (nlk:json-value row :object "models")))
      (is (gethash "kimi-k3" models) "a model the proxy names is listed")
      (is (gethash "glm-5.2" models) "and one that declares it does not reason")
      (is (null (gethash "glm-5.3" models)) "the fallback gives way to the live list")
      (is (equal '("low" "high" "max")
                 (nle::catalog-model-efforts (gethash "kimi-k3" models))) "with the rungs it declares"))))

(deftest zerosignal-cell-clamps-the-effort-down ()
  (let ((*round-payload* (zerosignal-catalog-payload '("kimi-k3" . ("low" "high" "max")))))
    (with-zerosignal-round (url headers body) "kimi-k3"
      (is (equal "http://127.0.0.1:9376/v1/chat/completions" url))
      (is (equal "Bearer sk-test" (header headers "authorization")))
      ;; `medium', which this node does not declare, goes out as the strongest
      ;; level below it -- never as one above
      (is (equal "low" (nlk:json-value body :string "reasoning_effort"))))))

(deftest zerosignal-cell-sends-a-declared-rung-untouched ()
  (let ((*round-payload* (zerosignal-catalog-payload '("kimi-k3" . ("low" "high" "max")))))
    (let ((*round-effort* "max"))
      (with-zerosignal-round (url headers body) "kimi-k3"
        (is (equal "max" (nlk:json-value body :string "reasoning_effort")) "a rung the node declares goes out as it is"))))
  ;; below every declared level there is nothing to clamp down to: the field
  ;; is omitted and the node's own default stands, never a rung above
  (let ((*round-payload* (zerosignal-catalog-payload '("kimi-k3" . ("low" "high" "max")))))
    (let ((*round-effort* "minimal"))
      (with-zerosignal-round (url headers body) "kimi-k3"
        (is (null (gethash "reasoning_effort" body)) "nothing below low to send")))))

(deftest zerosignal-cell-omits-reasoning-for-a-model-that-does-not-think ()
  (let ((*round-payload* (zerosignal-catalog-payload '("glm-5.2" . :none))))
    (with-zerosignal-round (url headers body) "glm-5.2"
      (is (null (gethash "reasoning_effort" body)) "no reasoning parameter at all"))))

(deftest zerosignal-cell-passes-an-unnamed-model-through ()
  (let ((*round-payload* (zerosignal-catalog-payload '("kimi-k3" . ("low")))))
    (with-zerosignal-round (url headers body) "grok-4.6"
      (is (equal "medium" (nlk:json-value body :string "reasoning_effort")) "a model the proxy has not named keeps what the wire built"))))

(deftest zerosignal-cell-base-follows-the-section ()
  (with-cell-stop ((zerosignal-start "base_url" "http://127.0.0.1:9999/v1"))
    (is (equal "http://127.0.0.1:9999/v1"
               (nlk:json-value (nle::models-catalog-table) :string "zerosignal" "api")))))

(deftest zerosignal-cell-reads-its-own-key ()
  (with-cell-stop ((zerosignal-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "ZEROSIGNAL_API_KEY") "zs-key"))
        (let ((credential (nle::resolve-provider-credential "zerosignal" :auth-path auth :probe t)))
          (is (equal "zs-key" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential)))))
      ;; no key anywhere is not a refusal: the wallet is the credential, and
      ;; the proxy ignores the placeholder
      (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
        (let ((credential (nle::resolve-provider-credential "zerosignal" :auth-path auth :probe t)))
          (is (equal "zerosignal-local" (nle:credential-key credential)))
          (is (eq :public (nle:credential-source credential))))))))

(deftest zerosignal-cell-never-sends-another-familys-key ()
  (with-cell-stop ((zerosignal-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "zerosignal" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))
