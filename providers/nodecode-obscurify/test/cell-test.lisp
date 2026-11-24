;;;; cell-test.lisp --- the obscurify cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every catalog read is a stubbed NLK:HTTP, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every chat round a stubbed DEX:POST: nothing touches
;;;; the network, the environment or the operator's files.

(in-package #:nodecode.test)

(defparameter +catalog+
  (concatenate 'string
               "{\"data\":["
               "{\"id\":\"deepseek/deepseek-v4-flash\",\"context_length\":1048576},"
               "{\"id\":\"z-ai/glm-5.3\",\"context_length\":1048576,\"reasoning\":true},"
               "{\"id\":\"moonshotai/kimi-k2-0905\",\"context_length\":262144,"
               "\"reasoning\":false},"
               "{\"id\":\"tiny/chat-8k\",\"context_length\":8192},"
               "{\"id\":\"acme/decision-1\",\"context_length\":200000,\"decisions\":true},"
               "{\"id\":\"acme/unmeasured-chat\"},"
               "{\"id\":\"deepseek/deepseek-v4-flash\",\"context_length\":1048576}]}")
  "An Obscurify.ai /v1/models answer: two models that clear the floor and
reason, one that clears it and says it does not, one under the floor, one
decision model, one that publishes no window, and a repeat of an id.")

(defparameter +featured+
  (sort (list "deepseek/deepseek-v4-flash" "z-ai/glm-5.3" "moonshotai/kimi-k2-0905"
              "qwen/qwen3-coder" "meta-llama/llama-3.3-70b-instruct")
        #'string<)
  "The plugin's fallback_models in id order: what the picker lists before a
read answers.")

(defvar *asked* nil
  "The last stubbed catalog request, (URL . ARGS); set rather than bound,
because a worker thread may make it.")

(defmacro with-obscurify-catalog ((text status) &body body)
  "BODY with NLK:HTTP stubbed to answer TEXT at STATUS -- the seam
NLE:HTTP-FETCH dials -- and the last request kept in *ASKED*."
  `(with-stubbed-fdefinition (nlk:http (method url &rest args)
                               (declare (ignore method))
                               (setf *asked* (cons url args))
                               (values ,text ,status))
     ,@body))

(defun listed (row)
  "The model ids ROW lists, in the order its table keeps them."
  (let ((models (nlk:json-value row :object "models"))
        (ids '()))
    (when (hash-table-p models)
      (maphash (lambda (id model) (declare (ignore model)) (push id ids)) models))
    (sort ids #'string<)))

(defmacro with-obscurify-round ((url body) model &body forms)
  "FORMS with the cell started and one chat round for obscurify MODEL
captured: URL and BODY (the decoded request) as dex:post saw them, with
*ROUND-EFFORT* what the round binds as its reasoning effort."
  `(with-cell-stop ((obscurify-start))
     (let ((nle::*provider* "obscurify") (nle::*model* ,model) (nle::*api-key* "sk-obs1-test")
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

(defvar *round-effort* nil
  "The reasoning effort a round binds, as a session pick or the default would.")

(define-test-slice "obscurify" "OBSCURIFY-CELL-" :start nodecode-obscurify:start-cell)

(define-cell-lifecycle-tests "obscurify"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body)
  (:fixture with-obscurify-catalog (+catalog+ 200))
  (:refused ("base_url" 5)))

;;; --- the catalog row ------------------------------------------------------------

(deftest obscurify-cell-puts-its-row-in-the-catalog ()
  (with-obscurify-catalog (+catalog+ 200)
    (with-cell-stop ((obscurify-start))
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "obscurify")))
        (is (equal "Obscurify.ai" (nlk:json-value row :string "name")))
        (is (equal "https://obscurify.ai/v1" (nlk:json-value row :string "api")))
        (is (equal "@ai-sdk/openai-compatible" (nlk:json-value row :string "npm")) "the chat lane's package")
        (is (equal '("OBSCURIFY_API_KEY") (coerce (nlk:json-value row :array "env") 'list)) "the key variable, and no other")
        (is (equal "openai-completions" (nle::configured-provider-lane "obscurify")) "the chat lane drives it")
        (is (equal "https://obscurify.ai/v1/chat/completions"
                   (nle::lane-endpoint "obscurify" "openai-completions")))
        (is (gethash "deepseek/deepseek-v4-flash"
                      (nlk:json-value row :object "models")) "a catalog model is listed"))
      (funcall stop)
      (setf stop nil)
      (is (null (nlk:json-value (nle::models-catalog-table) :object "obscurify")) "stop takes the row back"))))

(deftest obscurify-cell-lists-only-the-models-a-turn-can-run ()
  (with-obscurify-catalog (+catalog+ 200)
    (with-cell-stop ((obscurify-start))
      (let* ((row (nlk:json-value (nle::models-catalog-table) :object "obscurify"))
             (models (nlk:json-value row :object "models")))
        (is (equal '("acme/unmeasured-chat" "deepseek/deepseek-v4-flash"
                     "moonshotai/kimi-k2-0905" "z-ai/glm-5.3")
                   (listed row)) "the decision model and the 8K one are left out, the repeat once")
        (is (null (gethash "tiny/chat-8k" models)) "a model under the floor is not offered")
        (is (null (gethash "acme/decision-1" models)) "nor a decision model")
        (is (null (gethash "qwen/qwen3-coder" models)) "the featured list gives way to the catalog")
        (is (equal "https://obscurify.ai/v1/models" (car *asked*)) "the catalog's own address")
        (is (null (assoc "authorization" (getf (cdr *asked*) :headers)
                          :test #'string-equal)) "the public catalog is read without the key")))))

(deftest obscurify-cell-carries-the-catalog-s-windows-and-rungs ()
  (with-obscurify-catalog (+catalog+ 200)
    (with-cell-stop ((obscurify-start))
      (let* ((models (nlk:json-value (nle::models-catalog-table) :object "obscurify" "models"))
             (deepseek (gethash "deepseek/deepseek-v4-flash" models))
             (glm (gethash "z-ai/glm-5.3" models))
             (kimi (gethash "moonshotai/kimi-k2-0905" models))
             (unmeasured (gethash "acme/unmeasured-chat" models)))
        (is (eql 1048576 (nle::catalog-model-context deepseek)) "the window is the catalog's own")
        (is (eql 1048576 (nle::catalog-model-context glm)))
        (is (eql 262144 (nle::catalog-model-context kimi)))
        (is (null (nle::catalog-model-context unmeasured)) "a model that publishes none keeps none")
        (is (equal '("off" "minimal" "low" "medium" "high" "xhigh" "max")
                   (nle::catalog-model-efforts glm)) "a reasoning model offers the API's rungs")
        (is (null (nle::catalog-model-efforts kimi)) "a model that says it does not reason offers none")
        (is (nle::catalog-model-tool-call-p deepseek) "every listed model can run a turn")
        (is (not (nle::catalog-model-reasoning-p kimi)))))))

(deftest obscurify-cell-the-floor-is-the-section-s ()
  (with-obscurify-catalog (+catalog+ 200)
    (with-cell-stop ((obscurify-start "min_context_tokens" 4000))
      (let ((models (nlk:json-value (nle::models-catalog-table) :object "obscurify" "models")))
        (is (gethash "tiny/chat-8k" models) "an 8K model clears a 4K floor")
        (is (eql 8192 (nle::catalog-model-context (gethash "tiny/chat-8k" models))))))))

(deftest obscurify-cell-lists-the-featured-models-while-the-catalog-is-silent ()
  (with-obscurify-catalog ("" 503)
    (with-cell-stop ((obscurify-start))
      (let* ((row (nlk:json-value (nle::models-catalog-table) :object "obscurify"))
             (models (nlk:json-value row :object "models")))
        (is (equal +featured+ (listed row)) "the plugin's five fill the picker")
        (is (eql 1048576 (nle::catalog-model-context
                           (gethash "deepseek/deepseek-v4-flash" models))) "with the windows the plugin checked for them")
        (is (null (nle::catalog-model-efforts
                   (gethash "meta-llama/llama-3.3-70b-instruct" models))) "and no rungs for the one it says does not reason")))))

(deftest obscurify-cell-a-body-that-is-not-the-catalog-lists-the-featured ()
  (with-obscurify-catalog ("<html>captive portal</html>" 200)
    (with-cell-stop ((obscurify-start))
      (is (equal +featured+
                 (listed (nlk:json-value (nle::models-catalog-table) :object "obscurify"))) "an unreadable answer is no rows, never a failure to unwind"))))

(deftest obscurify-cell-base-follows-the-section ()
  (with-obscurify-catalog (+catalog+ 200)
    (with-cell-stop ((obscurify-start "base_url" "https://relay.example/v1"))
      (is (equal "https://relay.example/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "obscurify" "api")))
      (is (equal "https://relay.example/v1/models" (car *asked*)) "the relay's catalog is the one read"))))

;;; --- the key --------------------------------------------------------------------

(deftest obscurify-cell-reads-obscurify-api-key ()
  (with-obscurify-catalog (+catalog+ 200)
    (with-cell-stop ((obscurify-start))
      (with-temp-auth (auth "{}")
        (with-stubbed-fdefinition (nle::credential-env (name)
                                     (and (equal name "OBSCURIFY_API_KEY") "sk-obs1-key"))
          (let ((credential (nle::resolve-provider-credential "obscurify" :auth-path auth :probe t)))
            (is (equal "sk-obs1-key" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                     (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (let ((credential (nle::resolve-provider-credential "obscurify" :auth-path auth :probe t)))
            (is (not (equal "sk-openai" (nle:credential-key credential))) "never the chat family's own key")))))))

;;; --- reasoning effort ------------------------------------------------------------

(deftest obscurify-cell-sends-a-rung-as-the-api-spells-it ()
  (with-obscurify-catalog (+catalog+ 200)
    (let ((*round-effort* "medium"))
      (with-obscurify-round (url body) "z-ai/glm-5.3"
        (is (equal "https://obscurify.ai/v1/chat/completions" url))
        (is (equal "medium" (nlk:json-value body :string "reasoning_effort")))))
    (let ((*round-effort* "xhigh"))
      (with-obscurify-round (url body) "z-ai/glm-5.3"
        (is (equal "xhigh" (nlk:json-value body :string "reasoning_effort")))))))

(deftest obscurify-cell-sends-none-when-reasoning-is-off ()
  (with-obscurify-catalog (+catalog+ 200)
    (let ((*round-effort* "off"))
      (with-obscurify-round (url body) "deepseek/deepseek-v4-flash"
        (is (equal "none" (nlk:json-value body :string "reasoning_effort")) "the API's own spelling of the disabled rung")))))

(deftest obscurify-cell-leaves-an-unconfigured-round-alone ()
  (with-obscurify-catalog (+catalog+ 200)
    (let ((*round-effort* nil))
      (with-obscurify-round (url body) "deepseek/deepseek-v4-flash"
        (is (null (gethash "reasoning_effort" body)) "nothing was picked, so the API's default stands")))))
