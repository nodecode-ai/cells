;;;; cell-test.lisp --- the coreweave cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path or the bound
;;;; *AUTH-FILE-PATH*, every key variable a stubbed NLE::CREDENTIAL-ENV, every
;;;; wire a stubbed NLK:HTTP: nothing touches the network, the environment or
;;;; the operator's files.

(in-package #:nodecode.test)

(define-test-slice "coreweave" "COREWEAVE-CELL-" :start nodecode-coreweave:start-cell)

(define-cell-lifecycle-tests "coreweave"
  (:hooks 'nle::models-catalog-table)
  (:command "coreweave")
  (:running (let ((row (nlk:json-value (nle::models-catalog-table) :object "coreweave")))
              (is (equal "CoreWeave Inference" (nlk:json-value row :string "name")) "the catalog carries CoreWeave's row")))
  (:stopped (let ((row (nlk:json-value (nle::models-catalog-table) :object "coreweave")))
              (is (null row) "and the stop takes it back out")))
  (:refused ("base_url" 5)))

(defmacro with-coreweave-key ((&key (key "sk-cw")) &body body)
  "FORMS with COREWEAVE_API_KEY stubbed to KEY (NIL for a box with none) and
the auth store a scratch file holding nothing."
  `(with-temp-auth (auth "{}")
     (let ((nle::*auth-file-path* auth))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and ,key (equal name "COREWEAVE_API_KEY") ,key))
         ,@body))))

(defun coreweave-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

;;; --- the catalog, the lane, the address --------------------------------------

(deftest coreweave-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((coreweave-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "coreweave")))
      (is (equal "CoreWeave Inference" (nlk:json-value row :string "name")))
      (is (equal "https://api.inference.wandb.ai/v1" (nlk:json-value row :string "api")))
      (is (equalp #("COREWEAVE_API_KEY") (nlk:json-value row :array "env")))
      (is (equal "openai-completions" (nle::configured-provider-lane "coreweave")) "the chat lane drives it")
      (let ((endpoint (nle::lane-endpoint "coreweave" "openai-completions")))
        (is (equal "https://api.inference.wandb.ai/v1/chat/completions" endpoint) "the row's base is the lane's address")))))

(deftest coreweave-cell-base-follows-the-section ()
  (with-cell-stop ((coreweave-start "base_url" "https://inference.example/v1"))
    (let ((base (nlk:json-value (nle::models-catalog-table) :string "coreweave" "api")))
      (is (equal "https://inference.example/v1" base) "the section's base is the row's base"))))

;;; --- the key -----------------------------------------------------------------

(deftest coreweave-cell-reads-coreweave-api-key ()
  (with-cell-stop ((coreweave-start))
    (with-coreweave-key (:key "sk-cw")
      (let ((credential (nle::resolve-provider-credential "coreweave" :probe t)))
        (is (equal "sk-cw" (nle:credential-key credential)))
        (is (eq :env (nle:credential-source credential)) "the variable is where it came from")))))

(deftest coreweave-cell-never-sends-another-familys-key ()
  (with-cell-stop ((coreweave-start))
    (with-temp-auth (auth "{}")
      (let ((nle::*auth-file-path* auth))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "OPENAI_API_KEY") "sk-openai"))
          (let ((credential (nle::resolve-provider-credential "coreweave" :probe t)))
            (is (not (equal "sk-openai" (nle:credential-key credential))) "another family's key never goes to CoreWeave")
            (is (eq :public (nle:credential-source credential)))))))))

(deftest coreweave-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((coreweave-start))
    (with-temp-auth (auth "{\"api_keys\":{\"coreweave\":{\"provider\":\"coreweave\",\"key\":\"sk-cw-saved\"}}}")
      (let ((nle::*auth-file-path* auth))
        (with-stubbed-fdefinition (nle::credential-env (name)
                                   (and (equal name "COREWEAVE_API_KEY") "sk-cw-env"))
          (let ((credential (nle::resolve-provider-credential "coreweave" :probe t)))
            (is (equal "sk-cw-saved" (nle:credential-key credential)) "a saved key wins over the variable")))))))

;;; --- the catalog command -------------------------------------------------------

(defparameter *coreweave-catalog*
  "{\"object\":\"list\",\"data\":[{\"id\":\"moonshotai/Kimi-K2.6\",\"object\":\"model\",\"owned_by\":\"coreweave\"},{\"id\":\"deepseek-ai/DeepSeek-V3.1\",\"object\":\"model\",\"owned_by\":\"coreweave\"}]}"
  "A model list as GET /models answers it.")

(deftest coreweave-cell-command-lists-the-models-the-key-can-use ()
  (with-cell-stop ((coreweave-start))
    (with-coreweave-key ()
      (let ((asked nil) (headers nil))
        (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                    (setf asked (list method url)
                                          headers (getf options :headers))
                                    (values *coreweave-catalog* 200 nil))
          (let ((answer (nodecode-coreweave::run-command "" "s1")))
            (is (equal (list :get "https://api.inference.wandb.ai/v1/models") asked) "one authenticated read of the model list")
            (is (equal "Bearer sk-cw" (coreweave-header headers "authorization")))
            (is (search "moonshotai/Kimi-K2.6" answer))
            (is (search "deepseek-ai/DeepSeek-V3.1" answer))
            (let* ((row (nlk:json-value (nle::models-catalog-table) :object "coreweave"))
                   (models (nlk:json-value row :object "models")))
              (is (gethash "moonshotai/Kimi-K2.6" models) "/models offers it")
              (is (nle::catalog-model-turn-p (gethash "deepseek-ai/DeepSeek-V3.1" models)) "a turn can take a listed model"))))))))

(deftest coreweave-cell-command-narrows-by-text ()
  (with-cell-stop ((coreweave-start))
    (with-coreweave-key ()
      (with-stubbed-fdefinition (nlk:http (&rest options)
                                  (declare (ignore options))
                                  (values *coreweave-catalog* 200 nil))
        (let ((answer (nodecode-coreweave::run-command "kimi" "s1")))
          (is (search "moonshotai/Kimi-K2.6" answer))
          (is (null (search "deepseek-ai/DeepSeek-V3.1" answer)) "a model the needle does not match is left out"))))))

(deftest coreweave-cell-command-says-why-the-catalog-did-not-answer ()
  (with-cell-stop ((coreweave-start))
    (with-coreweave-key ()
      (with-stubbed-fdefinition (nlk:http (&rest options)
                                  (declare (ignore options))
                                  (values "nope" 401 nil))
        (let ((answer (nodecode-coreweave::run-command "" "s1")))
          (is (search "HTTP 401" answer))
          (let ((models (nlk:json-value (nle::models-catalog-table) :object "coreweave" "models")))
            (is (null (gethash "moonshotai/Kimi-K2.6" models)) "a catalog that did not answer lists nothing")))))))

(deftest coreweave-cell-command-without-a-key-says-where-to-make-one ()
  (with-cell-stop ((coreweave-start))
    (with-coreweave-key (:key nil)
      (let ((answer (nodecode-coreweave::run-command "" "s1")))
        (is (search "https://wandb.ai/authorize" answer))
        (is (search "COREWEAVE_API_KEY" answer))
        (is (search "api.inference.wandb.ai" answer) "and names the endpoint it would read")))))
