;;;; cell-test.lisp --- lifecycle, the catalog row, what discovery keeps, the key.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every catalog fetch is a stubbed NLK:HTTP and every key variable a
;;;; stubbed NLE::CREDENTIAL-ENV: nothing touches the network, the environment
;;;; or the operator's files.

(in-package #:nodecode.test)

(defparameter +catalog-json+
  (concatenate 'string
               "{\"data\":["
               ;; a route a turn can run on: text out, tools in, ready chat
               "{\"id\":\"lab/chat-model\",\"name\":\"Lab Chat Model\",\"context_length\":200000,"
               "\"architecture\":{\"output_modalities\":[\"text\"],"
               "\"input_modalities\":[\"text\",\"image\"]},"
               "\"supported_parameters\":[\"tools\"],"
               "\"endpoints\":[{\"route\":\"chat_completions\",\"status\":\"ready\","
               "\"supported_parameters\":[\"tools\"]}]},"
               ;; the same shape, but its Chat Completions route takes no tools
               "{\"id\":\"lab/no-tools-route\",\"name\":\"Lab No Tools\","
               "\"architecture\":{\"output_modalities\":[\"text\"]},"
               "\"supported_parameters\":[\"tools\"],"
               "\"endpoints\":[{\"route\":\"chat_completions\",\"status\":\"ready\","
               "\"supported_parameters\":[\"temperature\"]}]},"
               ;; a real model, on a route that is not a chat one
               "{\"id\":\"lab/embedding-model\",\"name\":\"Lab Embedding\","
               "\"architecture\":{\"output_modalities\":[\"text\"]},"
               "\"supported_parameters\":[\"tools\"],"
               "\"endpoints\":[{\"route\":\"embeddings\",\"status\":\"ready\","
               "\"supported_parameters\":[\"tools\"]}]}"
               "]}")
  "A hub model list: one route a turn can run on, and two it cannot.")

(defvar *asked* nil
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first; a
worker thread logs here too, so it is set rather than bound.")

(defmacro with-belive-wire ((text status) &body body)
  "BODY with NLK:HTTP stubbed to answer TEXT at STATUS, every request logged."
  `(progn
     (setf *asked* '())
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *asked*)
                                (values ,text ,status))
       ,@body)))

(defmacro with-workspace-key ((key) &body body)
  "BODY with BELIVE_TOKENHUB_API_KEY as KEY, and no other variable set."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "BELIVE_TOKENHUB_API_KEY") ,key))
     ,@body))

(defun landed-rows (seconds)
  "The cell's live rows once its worker's fetch landed, or NIL after SECONDS."
  (await (:timeout seconds) (and nodecode-belive-aihub::*live-rows* t))
  nodecode-belive-aihub::*live-rows*)

(defun models ()
  "The models the catalog row for this provider carries."
  (nlk:json-value (nle::models-catalog-table) :object "belive-aihub" "models"))

(define-test-slice "belive-aihub" "BELIVE-AIHUB-CELL-" :start nodecode-belive-aihub:start-cell)

(define-cell-lifecycle-tests "belive-aihub"
  (:hooks 'nle::models-catalog-table :credential)
  (:fixture with-belive-wire (+catalog-json+ 200))
  (:refused ("base_url" 5)))

(deftest belive-aihub-cell-lists-the-routes-a-turn-can-run-on ()
  (with-belive-wire (+catalog-json+ 200)
    (with-cell-stop ((belive-aihub-start))
      (is (landed-rows 5) "the worker's fetch landed")
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "belive-aihub"))
            (listed (models)))
        (is (equal "BeLive AI Hub" (nlk:json-value row :string "name")))
        (is (equal "https://tokenhub-api.belive.sg/api/v1" (nlk:json-value row :string "api")))
        (is (equal "openai-completions" (nle::configured-provider-lane "belive-aihub")) "the chat lane drives it")
        (is (equal "https://tokenhub-api.belive.sg/api/v1/chat/completions"
                   (nle::lane-endpoint "belive-aihub" "openai-completions")))
        (is (gethash "lab/chat-model" listed) "a route that takes tools is listed")
        (is (null (gethash "lab/no-tools-route" listed)) "a chat route that takes none is not")
        (is (null (gethash "lab/embedding-model" listed)) "an embeddings route is not")
        (let ((model (gethash "lab/chat-model" listed)))
          (is (equal "Lab Chat Model" (nle::catalog-model-name model)))
          (is (= 200000 (nle::catalog-model-context model)) "the catalog's context window rides along")
          (is (member "image" (coerce (nle::catalog-model-inputs model) 'list) :test #'equal) "a model whose catalog names images reads them")))
      (funcall stop)
      (setf stop nil)
      (is (null (nlk:json-value (nle::models-catalog-table) :object "belive-aihub")) "stop takes the row back"))))

(deftest belive-aihub-cell-keeps-an-empty-catalog-empty ()
  (with-belive-wire ("{\"data\":[]}" 200)
    (with-cell-stop ((belive-aihub-start))
      (is (equal '(t) (landed-rows 5)) "an empty answer is an answer, not a failure")
      (is (zerop (hash-table-count (models))) "the picker is empty, as the original leaves a successful empty answer"))))

(deftest belive-aihub-cell-falls-back-to-the-curated-list ()
  (with-belive-wire ("" 503)
    (with-cell-stop ((belive-aihub-start))
      (is (null (landed-rows 2)) "an unreachable catalog does not land")
      (let ((listed (models)))
        (is (gethash "qwen/qwen3.8-flash" listed) "the original's fallback models fill the picker")
        (is (gethash "z-ai/glm-5.3-flash" listed))
        (is (gethash "deepseek/deepseek-v4.1-flash-1017" listed))
        (is (= 3 (hash-table-count listed)) "exactly the three the plugin curates")))))

(deftest belive-aihub-cell-base-follows-the-section ()
  (with-belive-wire (+catalog-json+ 200)
    (with-cell-stop ((belive-aihub-start "base_url" "https://relay.example/v1"))
      (is (equal "https://relay.example/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "belive-aihub" "api")))
      (is (equal "https://relay.example/v1/chat/completions"
                 (nle::lane-endpoint "belive-aihub" "openai-completions")))
      (is (null *asked*) "a relay is not the hub's own catalog, so nothing is fetched"))))

(deftest belive-aihub-cell-fetches-its-catalog-at-the-base-with-the-key ()
  (with-belive-wire (+catalog-json+ 200)
    (with-workspace-key ("sk-belive")
      (multiple-value-bind (rows landed) (nodecode-belive-aihub::fetch-rows nodecode-belive-aihub::+staging+)
        (is landed "the catalog answered")
        (is (= 1 (length rows)) "one eligible route, the other two dropped")
        (is (equal "lab/chat-model" (getf (first rows) :id)))
        (is (equal "https://api-stg-tokenhub.belive.sg/api/v1/models" (second (car *asked*))) "staging is one of the hub's own bases")
        (let ((sent (cdr (assoc "authorization" (getf (cddr (car *asked*)) :headers)
                                 :test #'string-equal))))
          (is (equal "Bearer sk-belive" sent) "the workspace key rides along, as the original sends it"))))))

(deftest belive-aihub-cell-reads-only-its-own-variable ()
  (with-belive-wire (+catalog-json+ 200)
    (with-cell-stop ((belive-aihub-start))
      (with-temp-auth (auth "{}")
        (with-workspace-key ("sk-belive")
          (let ((credential (nle::resolve-provider-credential "belive-aihub" :auth-path auth :probe t)))
            (is (equal "sk-belive" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (let ((credential (nle::resolve-provider-credential "belive-aihub" :auth-path auth :probe t)))
            (is (not (equal "sk-openai" (nle:credential-key credential))) "never another family's variable")))))))
