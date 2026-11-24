;;;; cell-test.lisp --- the groq cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, Groq's /models a stubbed read,
;;;; every chat round a stubbed dex:post: nothing touches the network, the
;;;; environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "groq" "GROQ-CELL-" :start nodecode-groq:start-cell)

(define-cell-lifecycle-tests "groq"
  (:hooks 'nle::models-catalog-table 'nle::request-body)
  (:refused ("base_url" 5)))

(defvar *round-effort* nil
  "The reasoning effort a round binds, as a session pick or the default would.")

(defmacro with-groq-round ((url body) model &body forms)
  "FORMS with the cell started and one chat round for groq MODEL captured:
URL and BODY (the decoded request) as dex:post saw them, with *ROUND-EFFORT*
what the round binds as its reasoning effort."
  `(with-cell-stop ((groq-start))
     (let ((nle::*provider* "groq") (nle::*model* ,model) (nle::*api-key* "gsk-test")
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

(defun groq-published (ids)
  "A models.dev Groq row carrying IDS: the row the core would have read."
  (let ((models (make-hash-table :test 'equal)))
    (loop for id in ids
          do (setf (gethash id models)
                   (nle::make-catalog-model id 131072 8192 #("text") #("text")
                                             nil t nil t nil)))
    (nlk:json-object "name" "Groq" "models" models)))

(deftest groq-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((groq-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "groq")))
      (is (equal "Groq" (nlk:json-value row :string "name")))
      (is (equal "https://api.groq.com/openai/v1" (nlk:json-value row :string "api")))
      (is (equal "@ai-sdk/openai-compatible" (nlk:json-value row :string "npm")) "the chat lane's package")
      (is (gethash "openai/gpt-oss-120b" (nlk:json-value row :object "models")) "the fallback list, while Groq has said nothing")
      (is (equal "openai-completions" (nle::configured-provider-lane "groq")) "the chat lane drives it")
      (is (equal "https://api.groq.com/openai/v1/chat/completions"
                 (nle::lane-endpoint "groq" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "groq")))))

(deftest groq-cell-leaves-out-the-models-that-cannot-talk ()
  (with-cell-stop ((groq-start))
    (let* ((prior (groq-published '("openai/gpt-oss-120b" "qwen/qwen3.8-27b" "whisper-large-v3"
                                     "canopylabs/orpheus-v1-english" "allam-2-7b"
                                     "meta-llama/llama-prompt-guard-2-86m")))
           (models (nlk:json-value (nodecode-groq::catalog-row prior) :object "models")))
      (is (gethash "openai/gpt-oss-120b" models) "a chat model is listed")
      (is (gethash "qwen/qwen3.8-27b" models) "and another")
      (is (null (gethash "whisper-large-v3" models)) "speech-to-text is not a conversation")
      (is (null (gethash "canopylabs/orpheus-v1-english" models)) "nor text-to-speech")
      (is (null (gethash "allam-2-7b" models)) "nor a 4096-token window")
      (is (null (gethash "meta-llama/llama-prompt-guard-2-86m" models)) "nor a classifier"))))

(deftest groq-cell-lists-what-groq-names ()
  (with-cell-stop ((groq-start))
    (with-stubbed-fdefinitions
        ((nodecode-groq::credential-key () "gsk-test")
         (nodecode-groq::fetch-model-ids (base key timeout)
           (vector (nlk:json-object "id" "openai/gpt-oss-20b")
                   (nlk:json-object "id" "whisper-large-v3"))))
      (nodecode-groq::refresh-live))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "groq"))
           (models (nlk:json-value row :object "models")))
      (is (gethash "openai/gpt-oss-20b" models) "a model Groq names is listed")
      (is (null (gethash "whisper-large-v3" models)) "a speech model it names is not")
      (is (null (gethash "openai/gpt-oss-120b" models)) "the fallback gives way to the live list")
      (is (equal '("low" "medium" "high")
                 (nle::catalog-model-efforts (gethash "openai/gpt-oss-20b" models))) "with the rungs Groq takes for it"))))

(deftest groq-cell-sends-a-level-the-model-takes-as-it-is ()
  (let ((*round-effort* "medium"))
    (with-groq-round (url body) "openai/gpt-oss-120b"
      (is (equal "https://api.groq.com/openai/v1/chat/completions" url))
      (is (equal "medium" (nlk:json-value body :string "reasoning_effort"))))))

(deftest groq-cell-lowers-a-level-the-model-does-not-take ()
  (let ((*round-effort* "minimal"))
    (with-groq-round (url body) "openai/gpt-oss-120b"
      (is (equal "low" (nlk:json-value body :string "reasoning_effort")) "below the floor is the floor, never above")))
  (let ((*round-effort* "xhigh"))
    (with-groq-round (url body) "openai/gpt-oss-120b"
      (is (equal "high" (nlk:json-value body :string "reasoning_effort")) "above the top is the top")))
  (let ((*round-effort* "minimal"))
    (with-groq-round (url body) "qwen/qwen3.8-27b"
      (is (equal "low" (nlk:json-value body :string "reasoning_effort")) "qwen takes low above none"))))

(deftest groq-cell-turns-reasoning-off-in-the-models-own-spelling ()
  (let ((*round-effort* "off"))
    (with-groq-round (url body) "openai/gpt-oss-120b"
      (is (equal "low" (nlk:json-value body :string "reasoning_effort")) "gpt-oss always reasons: its weakest level"))
    (with-groq-round (url body) "qwen/qwen3.8-27b"
      (is (equal "none" (nlk:json-value body :string "reasoning_effort")) "qwen takes none"))))

(deftest groq-cell-omits-reasoning-for-a-model-that-does-not-think ()
  (let ((*round-effort* "medium"))
    (with-groq-round (url body) "llama-3.3-70b-versatile"
      (is (null (gethash "reasoning_effort" body)) "no reasoning parameter at all"))))

(deftest groq-cell-leaves-an-unconfigured-round-alone ()
  (let ((*round-effort* nil))
    (with-groq-round (url body) "openai/gpt-oss-120b"
      (is (null (gethash "reasoning_effort" body)) "nothing is sent, so Groq's default stands"))))

(deftest groq-cell-base-follows-the-section ()
  (with-cell-stop ((groq-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "groq" "api")))))

(deftest groq-cell-reads-groq-api-key ()
  (with-cell-stop ((groq-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "GROQ_API_KEY") "gsk-key"))
        (let ((credential (nle::resolve-provider-credential "groq" :auth-path auth :probe t)))
          (is (equal "gsk-key" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest groq-cell-never-sends-another-familys-key ()
  (with-cell-stop ((groq-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "groq" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))
