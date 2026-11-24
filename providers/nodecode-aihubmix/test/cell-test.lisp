;;;; cell-test.lisp --- lifecycle, the catalog row, the base, the key.
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
               "{\"model_id\":\"claude-sonnet-5\",\"name\":\"Claude Sonnet 5\","
               "\"features\":\"tools,vision\",\"context_length\":200000},"
               "{\"model_id\":\"gpt-5.6-sol\",\"name\":\"GPT-5.6 Sol\","
               "\"features\":\"function_calling\"},"
               "{\"model_id\":\"text-embedding-4\",\"name\":\"Embedding 4\","
               "\"features\":\"embeddings\"}"
               "]}")
  "A capability catalog as AIHubMix serves one: two tool-capable routes and
one embedding route that must not reach the picker.")

(defvar *asked* nil
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first; a
worker thread logs here too, so it is set rather than bound.")

(defmacro with-aihubmix-wire ((text status) &body body)
  "BODY with NLK:HTTP stubbed to answer TEXT at STATUS, every request logged."
  `(progn
     (setf *asked* '())
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *asked*)
                                (values ,text ,status))
       ,@body)))

(defun landed-rows (seconds)
  "The cell's live rows once its worker's fetch landed, or NIL after SECONDS."
  (await (:timeout seconds) (and nodecode-aihubmix::*live-rows* t))
  nodecode-aihubmix::*live-rows*)

(define-test-slice "aihubmix" "AIHUBMIX-CELL-" :start nodecode-aihubmix:start-cell)

(define-cell-lifecycle-tests "aihubmix"
  (:hooks 'nle::models-catalog-table)
  (:fixture with-aihubmix-wire (+catalog-json+ 200))
  (:refused ("base_url" 5)))

(deftest aihubmix-cell-lists-the-tool-capable-catalog ()
  (with-aihubmix-wire (+catalog-json+ 200)
    (with-cell-stop ((aihubmix-start))
      (is (landed-rows 5) "the worker's fetch landed")
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "aihubmix"))
            (models (nlk:json-value (nle::models-catalog-table) :object "aihubmix" "models")))
        (is (equal "AIHubMix" (nlk:json-value row :string "name")))
        (is (equal "https://aihubmix.com/v1" (nlk:json-value row :string "api")))
        (is (equal "openai-completions" (nle::configured-provider-lane "aihubmix")) "the chat lane drives it")
        (is (equal "https://aihubmix.com/v1/chat/completions"
                   (nle::lane-endpoint "aihubmix" "openai-completions")))
        (is (gethash "claude-sonnet-5" models) "a `tools' route is listed")
        (is (gethash "gpt-5.6-sol" models) "a `function_calling' route is listed too")
        (is (null (gethash "text-embedding-4" models)) "an embedding route is not")))
    (is (null (nlk:json-value (nle::models-catalog-table) :object "aihubmix")) "stop takes the row back")))

(deftest aihubmix-cell-falls-back-to-the-curated-list ()
  (with-aihubmix-wire ("" 503)
    (with-cell-stop ((aihubmix-start))
      (let ((models (nlk:json-value (nle::models-catalog-table) :object "aihubmix" "models")))
        (is (gethash "claude-sonnet-5" models) "the curated flagships still fill the picker")
        (is (>= (hash-table-count models) 10) "the whole curated list is there")))))

(deftest aihubmix-cell-base-follows-the-section ()
  (with-aihubmix-wire (+catalog-json+ 200)
    (with-cell-stop ((aihubmix-start "base_url" "https://relay.example/v1"))
      (is (equal "https://relay.example/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "aihubmix" "api")))
      (is (equal "https://relay.example/v1/chat/completions"
                 (nle::lane-endpoint "aihubmix" "openai-completions")))
      (is (null *asked*) "a relay is not AIHubMix's own catalog, so nothing is fetched"))))

(deftest aihubmix-cell-fetches-its-catalog-with-the-key ()
  (with-aihubmix-wire (+catalog-json+ 200)
    (with-stubbed-fdefinition (nle::credential-env (name)
                               (and (equal name "AIHUBMIX_API_KEY") "sk-hub"))
      (let ((rows (nodecode-aihubmix::fetch-rows)))
        (is (= 2 (length rows)) "two tool-capable rows, the embedding one dropped")
        (is (equal "https://aihubmix.com/api/v1/models?types=llm" (second (car *asked*))) "the capability catalog is asked")
        (let ((sent (cdr (assoc "authorization" (getf (cddr (car *asked*)) :headers) :test #'string-equal))))
          (is (equal "Bearer sk-hub" sent) "the key rides along, as the original sends it"))))))

(deftest aihubmix-cell-reads-aihubmix-api-key ()
  (with-aihubmix-wire (+catalog-json+ 200)
    (with-cell-stop ((aihubmix-start))
      (with-temp-auth (auth "{}")
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "AIHUBMIX_API_KEY") "sk-hub"))
          (let ((credential (nle::resolve-provider-credential "aihubmix" :auth-path auth :probe t)))
            (is (equal "sk-hub" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (let ((credential (nle::resolve-provider-credential "aihubmix" :auth-path auth :probe t)))
            (is (not (equal "sk-openai" (nle:credential-key credential))) "never another family's variable")))))))
