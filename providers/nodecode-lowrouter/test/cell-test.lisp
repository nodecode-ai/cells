;;;; cell-test.lisp --- lifecycle, the filtered catalog, the key, the balance.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every request is a stubbed NLK:HTTP and every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing touches the network, the environment or the
;;;; operator's files.

(in-package #:nodecode.test)

(defparameter +models-json+
  (concatenate 'string
               "{\"object\":\"list\",\"data\":["
               "{\"id\":\"auto/mistralai/mistral-large-2512\",\"name\":\"mistral-large-2512\","
               "\"modality\":\"chat\",\"context_length\":256000,"
               "\"supported_parameters\":[\"tools\",\"tool_choice\"],"
               "\"architecture\":{\"input_modalities\":[\"text\",\"image\"]}},"
               "{\"id\":\"alias/coder\",\"name\":\"coder\",\"modality\":\"chat\","
               "\"context_length\":128000,\"supported_parameters\":[\"tools\"]},"
               "{\"id\":\"auto/moonshotai/kimi-k2.6\",\"name\":\"kimi-k2.6\","
               "\"modality\":\"chat\",\"supported_parameters\":[\"tools\"]},"
               "{\"id\":\"auto/qwen/qwen3-32b\",\"name\":\"qwen3-32b\",\"modality\":\"chat\","
               "\"context_length\":40960,\"supported_parameters\":[\"tools\"]},"
               "{\"id\":\"auto/ibm-granite/granite-4.2-3b\",\"name\":\"granite\","
               "\"modality\":\"chat\",\"context_length\":131072},"
               "{\"id\":\"auto/sentence-transformers/all-minilm-l6\",\"modality\":\"embedding\"},"
               "{\"id\":\"mistral/mistralai/mistral-large-2512\",\"modality\":\"chat\","
               "\"context_length\":256000,\"supported_parameters\":[\"tools\"]}"
               "]}")
  "A /v1/models answer as LowRouter serves one: two routes a turn can run
on, and four it cannot -- a pinned host route, an embedding, a model with no
tool calling, and one whose window is under the floor.")

(defparameter +credits-json+
  "{\"data\":{\"total_credits\":20.0,\"total_usage\":7.5,\"currency\":\"EUR\"}}"
  "A /v1/credits answer: twenty euros bought, seven fifty spent.")

(defvar *asked* nil
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first; a
worker thread logs here too, so it is set rather than bound.")

(defun answer-for (url)
  "The body the stub answers URL with."
  (if (search "/credits" url) +credits-json+ +models-json+))

(defmacro with-lowrouter-wire (&body body)
  "BODY with NLK:HTTP stubbed to answer the catalog or the balance, every request logged."
  `(progn
     (setf *asked* '())
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *asked*)
                                (values (answer-for url) 200))
       ,@body)))

(defmacro with-key ((value) &body body)
  "BODY with LOWROUTER_API_KEY answering VALUE through the key seam."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                               (and (member name '("LOWROUTER_API_KEY" "OPENAI_API_KEY")
                                            :test #'equal)
                                    (if (equal name "LOWROUTER_API_KEY") ,value "sk-openai")))
     ,@body))

(defun landed-rows (seconds)
  "The cell's live rows once its worker's fetch landed, or NIL after SECONDS."
  (await (:timeout seconds) (and nodecode-lowrouter::*live-rows* t))
  nodecode-lowrouter::*live-rows*)

(define-test-slice "lowrouter" "LOWROUTER-CELL-" :start nodecode-lowrouter:start-cell)

(define-cell-lifecycle-tests "lowrouter"
  (:hooks 'nle::models-catalog-table :credential)
  (:fixture with-lowrouter-wire)
  (:command "lowrouter")
  (:refused ("base_url" 5)))

(deftest lowrouter-cell-offers-only-the-routes-a-turn-can-run ()
  (with-lowrouter-wire
    (with-cell-stop ((lowrouter-start))
      (is (landed-rows 5) "the worker's fetch landed")
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "lowrouter"))
            (models (nlk:json-value (nle::models-catalog-table) :object "lowrouter" "models")))
        (is (equal "LowRouter" (nlk:json-value row :string "name")))
        (is (equal "https://api.lowrouter.ai/v1" (nlk:json-value row :string "api")))
        (is (equal "openai-completions" (nle::configured-provider-lane "lowrouter")) "the chat lane drives it")
        (is (equal "https://api.lowrouter.ai/v1/chat/completions"
                   (nle::lane-endpoint "lowrouter" "openai-completions")))
        (is (gethash "auto/mistralai/mistral-large-2512" models) "an auto/ route that takes tools is listed")
        (is (gethash "alias/coder" models) "an alias model is listed too")
        (is (gethash "auto/moonshotai/kimi-k2.6" models) "a route with no published window stays")
        (is (null (gethash "mistral/mistralai/mistral-large-2512" models)) "a pinned host route is not")
        (is (null (gethash "auto/qwen/qwen3-32b" models)) "a 40k window is under the floor")
        (is (null (gethash "auto/ibm-granite/granite-4.2-3b" models)) "a model with no tool calling is not")
        (is (null (gethash "auto/sentence-transformers/all-minilm-l6" models)) "nor an embedding")))
    (is (null (nlk:json-value (nle::models-catalog-table) :object "lowrouter")) "stop takes the row back")))

(deftest lowrouter-cell-falls-back-to-the-plugin-s-four ()
  (with-lowrouter-wire
    (with-stubbed-fdefinition (nlk:http (method url &rest args)
                               (declare (ignore method args))
                               (declare (ignore url))
                               (values "" 503))
      (with-cell-stop ((lowrouter-start))
        (let ((models (nlk:json-value (nle::models-catalog-table) :object "lowrouter" "models")))
          (is (gethash "auto/qwen/qwen3-coder-30b-a3b-instruct" models) "the plugin's fallbacks still fill the picker")
          (is (= 4 (hash-table-count models)) "all four, and nothing else"))))))

(deftest lowrouter-cell-reads-lowrouter-api-key ()
  (with-lowrouter-wire
    (with-cell-stop ((lowrouter-start))
      (with-temp-auth (auth "{}")
        (with-key ("sk-lr-123")
          (let ((credential (nle::resolve-provider-credential "lowrouter" :auth-path auth :probe t)))
            (is (equal "sk-lr-123" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (let ((credential (nle::resolve-provider-credential "lowrouter" :auth-path auth :probe t)))
            (is (not (equal "sk-openai" (nle:credential-key credential))) "never another family's variable")
            (is (eq :public (nle:credential-source credential)) "a public one instead")))))))

(deftest lowrouter-cell-command-shows-the-balance-in-euros ()
  (with-lowrouter-wire
    (with-cell-stop ((lowrouter-start))
      (with-key ("sk-lr-123")
        (let ((answer (nodecode-lowrouter::run-command "" "s1")))
          (is (search "Credits balance: €12.50" answer) "the balance is what is left of what was bought")
          (is (search "Purchased €20.00" answer) "and how much was bought")
          (is (search "/v1/credits" (format nil "~{~a ~}" (mapcar #'second *asked*))) "read from the credits endpoint"))))))

(deftest lowrouter-cell-command-says-why-there-is-no-balance ()
  (with-lowrouter-wire
    (with-cell-stop ((lowrouter-start))
      (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
        (let ((answer (nodecode-lowrouter::run-command "" "s1")))
          (is (search "no key yet" answer) "no key, and where to make one")
          (is (search "https://lowrouter.ai/app/keys" answer)))))))

(deftest lowrouter-cell-command-lists-the-routes-it-took ()
  (with-lowrouter-wire
    (with-cell-stop ((lowrouter-start))
      (let ((answer (nodecode-lowrouter::run-command "models" "s1")))
        (is (search "auto/mistralai/mistral-large-2512" answer) "the routes it offers")
        (is (search "(256000 tokens)" answer) "with their windows")
        (is (search "3 of 7" answer) "and how much of the catalog that is")))))
