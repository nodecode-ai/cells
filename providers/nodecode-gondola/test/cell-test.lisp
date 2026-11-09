;;;; cell-test.lisp --- lifecycle, the chat-only catalog, the base, the key.
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
               "{\"id\":\"claude-sonnet-5\",\"name\":\"Claude Sonnet 5\","
               "\"endpoint\":\"/v1/chat/completions\",\"context_length\":200000},"
               "{\"id\":\"deepseek-v4-flash\",\"endpoint\":\"/v1/chat/completions\"},"
               "{\"id\":\"kimi-k2-7-code\",\"modality\":\"text\"},"
               "{\"id\":\"gondola-image-1\",\"endpoint\":\"/v1/images/generations\"},"
               "{\"id\":\"gondola-video-1\",\"modality\":\"video\"},"
               "{\"id\":\"mystery-model\"}"
               "]}")
  "A model catalog as Gondola serves one: three chat routes, a shape the
cell reads by `endpoint', one by `modality', one that names neither (kept, as
the original keeps it), an image route and a video route that must not reach
the picker.")

(defvar *asked* nil
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first; a
worker thread logs here too, so it is set rather than bound.")

(defmacro with-gondola-wire ((text status) &body body)
  "BODY with NLK:HTTP stubbed to answer TEXT at STATUS, every request logged."
  `(progn
     (setf *asked* '())
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *asked*)
                                (values ,text ,status))
       ,@body)))

(defun landed-rows (seconds)
  "The cell's live rows once its worker's fetch landed, or NIL after SECONDS."
  (await (:timeout seconds) (and nodecode-gondola::*live-rows* t))
  nodecode-gondola::*live-rows*)

(defun listed (id)
  "The catalog's Gondola model named ID, or NIL."
  (gethash id (nlk:json-value (nle::models-catalog-table) :object "gondola" "models")))

(define-test-slice "gondola" "GONDOLA-CELL-" :start nodecode-gondola:start-cell)

(define-cell-lifecycle-tests "gondola"
  (:hooks 'nle::models-catalog-table)
  (:fixture with-gondola-wire (+catalog-json+ 200))
  (:refused ("base_url" 5)))

(deftest gondola-cell-lists-only-the-chat-catalog ()
  (with-gondola-wire (+catalog-json+ 200)
    (with-cell-stop ((gondola-start))
      (is (landed-rows 5) "the worker's fetch landed")
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "gondola")))
        (is (equal "Gondola" (nlk:json-value row :string "name")))
        (is (equal "https://api.gondola-ai.com/v1" (nlk:json-value row :string "api")))
        (is (equal "openai-completions" (nle::configured-provider-lane "gondola")) "the chat lane drives it")
        (is (equal "https://api.gondola-ai.com/v1/chat/completions"
                   (nle::lane-endpoint "gondola" "openai-completions"))))
      (is (listed "claude-sonnet-5") "a route whose `endpoint' is chat completions is listed")
      (is (listed "kimi-k2-7-code") "a route whose `modality' is text is listed too")
      (is (listed "mystery-model") "an entry naming neither is kept, as the original keeps it")
      (is (null (listed "gondola-image-1")) "an image route is not")
      (is (null (listed "gondola-video-1")) "nor a video one"))
    (is (null (nlk:json-value (nle::models-catalog-table) :object "gondola")) "stop takes the row back")))

(deftest gondola-cell-falls-back-to-the-curated-list ()
  (with-gondola-wire ("" 503)
    (with-cell-stop ((gondola-start))
      (is (null (landed-rows 1)) "an unreachable catalog leaves no live rows")
      (let ((models (nlk:json-value (nle::models-catalog-table) :object "gondola" "models")))
        (is (gethash "deepseek-v4-flash" models) "the curated routes still fill the picker")
        (is (= 5 (hash-table-count models)) "the whole curated list is there")))))

(deftest gondola-cell-base-follows-the-section ()
  (with-gondola-wire (+catalog-json+ 200)
    (with-cell-stop ((gondola-start "base_url" "https://relay.example/v1/"))
      (let ((api (nlk:json-value (nle::models-catalog-table) :string "gondola" "api"))
            (endpoint (nle::lane-endpoint "gondola" "openai-completions")))
        (is (equal "https://relay.example/v1" api) "a trailing slash is dropped from the base")
        (is (equal "https://relay.example/v1/chat/completions" endpoint) "the lane follows it too"))
      (is (landed-rows 5) "the relay's catalog is read")
      (let ((url (second (car *asked*))))
        (is (equal "https://relay.example/v1/models" url) "the fetch follows the section's base")))))

(deftest gondola-cell-fetches-its-catalog-with-the-key ()
  (with-gondola-wire (+catalog-json+ 200)
    (with-stubbed-fdefinition (nle::credential-env (name)
                               (and (equal name "GONDOLA_API_KEY") "gnd-1"))
      (let ((rows (nodecode-gondola::fetch-rows "https://api.gondola-ai.com/v1/")))
        (is (= 4 (length rows)) "the chat routes, the image and video ones dropped")
        (is (equal "https://api.gondola-ai.com/v1/models" (second (car *asked*))) "the catalog is asked")
        (let ((sent (cdr (assoc "authorization" (getf (cddr (car *asked*)) :headers) :test #'string-equal))))
          (is (equal "Bearer gnd-1" sent) "the key rides along, as the original sends it"))))))

(deftest gondola-cell-reads-gondola-api-key ()
  (with-gondola-wire (+catalog-json+ 200)
    (with-cell-stop ((gondola-start))
      (with-temp-auth (auth "{}")
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "GONDOLA_API_KEY") "gnd-1"))
          (let ((credential (nle::resolve-provider-credential "gondola" :auth-path auth :probe t)))
            (is (equal "gnd-1" (nle:credential-key credential)))
            (is (eq :env (nle:credential-source credential)))))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (let ((credential (nle::resolve-provider-credential "gondola" :auth-path auth :probe t)))
            (is (not (equal "sk-openai" (nle:credential-key credential))) "never another family's variable")))))))
