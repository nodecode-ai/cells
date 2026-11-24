;;;; cell-test.lisp --- lifecycle, the catalog row, the limits, the key.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every catalog fetch is a stubbed NLK:HTTP and every key variable a
;;;; stubbed NLE::CREDENTIAL-ENV: nothing touches the network, the environment
;;;; or the operator's files.

(in-package #:nodecode.test)

(defparameter +catalog-json+
  (concatenate 'string
               "{\"object\":\"list\",\"data\":["
               ;; a hosted model: a wider window than the input it accepts
               "{\"id\":\"hashference-qwen3.8-27b\",\"protocol\":\"chat\","
               "\"context_length\":163840,\"max_input_tokens\":98304,"
               "\"max_output_tokens\":8192,\"default_max_output_tokens\":8192,"
               "\"supports_tools\":true},"
               ;; the catalog says it cannot call tools
               "{\"id\":\"hashference-deepseek-v4.1-flash\",\"protocol\":\"chat\","
               "\"max_input_tokens\":98304,\"default_max_output_tokens\":8192,"
               "\"supports_tools\":false},"
               ;; a model that states only its output cap
               "{\"id\":\"anthropic-claude-sonnet-5.5\",\"protocol\":\"responses\","
               "\"context_length\":null,\"max_input_tokens\":null,"
               "\"default_max_output_tokens\":32000,\"supports_tools\":null},"
               ;; the same model for the Messages wire, twice over
               "{\"id\":\"anthropic-claude-sonnet-5.5-messages\",\"protocol\":\"messages\","
               "\"max_input_tokens\":98304},"
               "{\"id\":\"moonshotai-kimi-k3.1-pro-messages\",\"protocol\":\"chat\","
               "\"max_input_tokens\":98304},"
               ;; a stated input limit, and no window at all
               "{\"id\":\"openai-gpt-6.1-sol\",\"protocol\":\"responses\","
               "\"max_input_tokens\":271999,\"default_max_output_tokens\":32000},"
               ;; no limits stated at all: not a refusal
               "{\"id\":\"z-ai-glm-5.3\",\"protocol\":\"responses\"},"
               ;; under the 64K a working session needs
               "{\"id\":\"tiny-embed-2\",\"protocol\":\"chat\",\"context_length\":32768}"
               "]}")
  "A catalog as Hashference serves one: four rows a turn can run on and four
it cannot, each refused for a different reason.")

(defvar *asked* nil
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first; a
worker thread logs here too, so it is set rather than bound.")

(defmacro with-hashference-wire ((text status) &body body)
  "BODY with NLK:HTTP stubbed to answer TEXT at STATUS, every request logged."
  `(progn
     (setf *asked* '())
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *asked*)
                                (values ,text ,status))
       ,@body)))

(defun landed-rows (seconds)
  "The cell's live rows once its worker's fetch landed, or NIL after SECONDS."
  (await (:timeout seconds) (and nodecode-hashference::*live-rows* t))
  nodecode-hashference::*live-rows*)

(defun asked-at (url)
  "Whether the stubbed wire was asked URL."
  (find url *asked* :key #'second :test #'equal))

(define-test-slice "hashference" "HASHFERENCE-CELL-" :start nodecode-hashference:start-cell)

(define-cell-lifecycle-tests "hashference"
  (:hooks 'nle::models-catalog-table)
  (:fixture with-hashference-wire (+catalog-json+ 200))
  (:refused ("base_url" 5)))

(deftest hashference-cell-lists-the-models-a-turn-can-run ()
  (with-hashference-wire (+catalog-json+ 200)
    (with-cell-stop ((hashference-start))
      (is (landed-rows 5) "the worker's fetch landed")
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "hashference"))
            (models (nlk:json-value (nle::models-catalog-table) :object "hashference" "models")))
        (is (equal "Hashference" (nlk:json-value row :string "name")))
        (is (equal "https://hashference.com/v1" (nlk:json-value row :string "api")))
        (is (equal "openai-completions" (nle::configured-provider-lane "hashference")) "the chat lane drives it")
        (is (equal "https://hashference.com/v1/chat/completions"
                   (nle::lane-endpoint "hashference" "openai-completions")))
        (is (= 4 (hash-table-count models)) "four of the eight catalog rows can run a turn")
        (is (null (gethash "anthropic-claude-sonnet-5.5-messages" models)) "a Messages-protocol alias is left out")
        (is (null (gethash "moonshotai-kimi-k3.1-pro-messages" models)) "so is a bare -messages alias on the chat wire")
        (is (null (gethash "hashference-deepseek-v4.1-flash" models)) "and a model the catalog says cannot call tools")
        (is (null (gethash "tiny-embed-2" models)) "and one that takes under 64K of input")
        (is (gethash "z-ai-glm-5.3" models) "a model with no stated limit is listed all the same"))
      (funcall stop)
      (setf stop nil)
      (is (null (nlk:json-value (nle::models-catalog-table) :object "hashference")) "stop takes the row back"))))

(deftest hashference-cell-sizes-each-model-by-what-it-states ()
  (with-hashference-wire (+catalog-json+ 200)
    (with-cell-stop ((hashference-start))
      (is (landed-rows 5) "the worker's fetch landed")
      (let* ((models (nlk:json-value (nle::models-catalog-table) :object "hashference" "models"))
             (hosted (gethash "hashference-qwen3.8-27b" models))
             (capped (gethash "anthropic-claude-sonnet-5.5" models)))
        (is (= 98304 (nle::catalog-model-context hosted)) "the input limit, not the 163,840 window")
        (is (= 8192 (nle::catalog-model-output hosted)) "the model's own default output cap")
        (is (= 32000 (nle::catalog-model-output capped)) "a model that states no input limit still states its cap")
        (is (null (nle::catalog-model-context capped)) "no input limit stated is no limit claimed")
        (is (eq t (nle::catalog-model-tool-call-p hosted)) "the picker only holds models that call tools")))))

(deftest hashference-cell-falls-back-to-the-curated-list ()
  (with-hashference-wire ("" 503)
    (with-cell-stop ((hashference-start))
      (let ((models (nlk:json-value (nle::models-catalog-table) :object "hashference" "models")))
        (is (gethash "hashference-qwen3.8-27b" models) "the curated models still fill the picker")
        (is (= 6 (hash-table-count models)) "the whole curated list is there")))))

(deftest hashference-cell-follows-the-section-base ()
  (with-hashference-wire (+catalog-json+ 200)
    (with-cell-stop ((hashference-start "base_url" "https://relay.example/v1"))
      (is (equal "https://relay.example/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "hashference" "api")))
      (is (equal "https://relay.example/v1/chat/completions"
                 (nle::lane-endpoint "hashference" "openai-completions")))
      (is (landed-rows 5) "a relay's own catalog is read")
      (is (asked-at "https://relay.example/v1/models") "the catalog is the relay's /models"))))

(deftest hashference-cell-reads-the-public-catalog-without-a-key ()
  (with-hashference-wire (+catalog-json+ 200)
    (let ((rows (nodecode-hashference::fetch-rows)))
      (is (= 4 (length rows)) "four runnable rows, the other four left out")
      (is (equal "https://hashference.com/v1/models" (second (car *asked*))) "the catalog is asked at /models")
      (is (null (assoc "authorization" (getf (cddr (car *asked*)) :headers) :test #'string-equal)) "and without credentials, as the original asks it"))))

(deftest hashference-cell-reads-hashference-api-key ()
  (with-hashference-wire (+catalog-json+ 200)
    (with-cell-stop ((hashference-start))
      (with-temp-auth (auth "{}")
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "HASHFERENCE_API_KEY") "sk-hf"))
          (let ((credential (nle::resolve-provider-credential "hashference" :auth-path auth :probe t)))
            (is (equal "sk-hf" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (let ((credential (nle::resolve-provider-credential "hashference" :auth-path auth :probe t)))
            (is (not (equal "sk-openai" (nle:credential-key credential))) "never another family's variable")))))))
