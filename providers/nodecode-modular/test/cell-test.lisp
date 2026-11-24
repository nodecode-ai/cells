;;;; cell-test.lisp --- the modular cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every chat round a stubbed
;;;; dex:post: nothing touches the network, the environment or the operator's
;;;; files.

(in-package #:nodecode.test)

(define-test-slice "modular" "MODULAR-CELL-" :start nodecode-modular:start-cell)

(define-cell-lifecycle-tests "modular"
  (:hooks 'nle::models-catalog-table 'nle::request-body)
  (:refused ("base_url" 5)))

(defvar *round-effort* nil
  "The reasoning effort a round binds, as a session pick or the default would.")

(defmacro with-modular-round ((url body) model &body forms)
  "FORMS with the cell started and one chat round for modular MODEL captured:
URL and BODY (the decoded request) as dex:post saw them, with *ROUND-EFFORT*
what the round binds as its reasoning effort."
  `(with-cell-stop ((modular-start))
     (let ((nle::*provider* "modular") (nle::*model* ,model) (nle::*api-key* "sk-test")
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

(defun reasoning-of (body)
  "BODY's nested reasoning object, or NIL when the request carries none."
  (nlk:json-value body :object "reasoning"))

;;; --- the catalog row ---------------------------------------------------------------

(deftest modular-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((modular-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "modular")))
      (is (equal "Modular" (nlk:json-value row :string "name")))
      (is (equal "https://api.modular.com/v1" (nlk:json-value row :string "api")))
      (is (equal "@ai-sdk/openai-compatible" (nlk:json-value row :string "npm")) "the chat lane's package")
      (let ((models (nlk:json-value row :object "models")))
        (is (gethash "MiniMaxAI/MiniMax-M3.1-Flash" models) "the original's models are listed")
        (is (gethash "google/gemma-4-31b-it" models) "and the small window one")
        (is (= 4 (hash-table-count models)) "and nothing else, as its fetch_models narrowed the list"))
      (is (equal "openai-completions" (nle::configured-provider-lane "modular")) "the chat lane drives it")
      (is (equal "https://api.modular.com/v1/chat/completions"
                 (nle::lane-endpoint "modular" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "modular")))))

(deftest modular-cell-describes-each-model ()
  (with-cell-stop ((modular-start))
    (let* ((models (nlk:json-value (nle::models-catalog-table) :object "modular" "models"))
           (flash (gethash "MiniMaxAI/MiniMax-M3.1-Flash" models))
           (deepseek (gethash "deepseek/deepseek-v4.1-flash-1017" models))
           (m3 (gethash "minimax/minimax-m3" models))
           (gemma (gethash "google/gemma-4-31b-it" models)))
      (is (equal '(1048576) (remove-duplicates
                              (mapcar #'nle::catalog-model-context (list flash deepseek m3)))) "the 1M window the original measured")
      (is (eql 262144 (nle::catalog-model-context gemma)) "and gemma's own")
      (is (every #'nle::catalog-model-tool-call-p (list flash deepseek m3 gemma)) "every model takes tools")
      (is (search "image" (format nil "~s" (nle::catalog-model-inputs flash))) "and images")
      (is (equal '("low" "medium" "high" "xhigh" "max")
                 (nle::catalog-model-efforts flash)) "flash takes five levels")
      (is (equal '("low" "high" "max")
                 (nle::catalog-model-efforts deepseek)) "deepseek three")
      (is (null (nle::catalog-model-efforts m3)) "m3 takes a toggle and no level")
      (is (nle::catalog-model-reasoning-p m3) "but it does reason"))))

(deftest modular-cell-base-follows-the-section ()
  (with-cell-stop ((modular-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "modular" "api")))))

;;; --- the reasoning object ----------------------------------------------------------

(deftest modular-cell-sends-a-level-the-model-takes-as-it-is ()
  (let ((*round-effort* "medium"))
    (with-modular-round (url body) "MiniMaxAI/MiniMax-M3.1-Flash"
      (is (equal "https://api.modular.com/v1/chat/completions" url))
      (is (equal "medium" (nlk:json-value (reasoning-of body) :string "effort")) "the pick rides Modular's nested object")
      (is (null (gethash "reasoning_effort" body)) "and not the chat lane's field"))))

(deftest modular-cell-lowers-a-level-the-model-does-not-take ()
  (let ((*round-effort* "minimal"))
    (with-modular-round (url body) "deepseek/deepseek-v4.1-flash-1017"
      (is (equal "low" (nlk:json-value (reasoning-of body) :string "effort")) "below the floor is the floor, never above")))
  (let ((*round-effort* "xhigh"))
    (with-modular-round (url body) "deepseek/deepseek-v4.1-flash-1017"
      (is (equal "high" (nlk:json-value (reasoning-of body) :string "effort")) "above the top is the nearest level it takes")))
  (let ((*round-effort* "xhigh"))
    (with-modular-round (url body) "MiniMaxAI/MiniMax-M3.1-Flash"
      (is (equal "xhigh" (nlk:json-value (reasoning-of body) :string "effort")) "a level the model takes goes out as it is"))))

(deftest modular-cell-turns-reasoning-off-in-the-models-own-way ()
  (let ((*round-effort* "off"))
    (with-modular-round (url body) "deepseek/deepseek-v4.1-flash-1017"
      (is (equal "{\"enabled\":false}" (nlk:encode-json-object (reasoning-of body))) "a switch model is switched off, as JSON false on the wire"))
    (with-modular-round (url body) "MiniMaxAI/MiniMax-M3.1-Flash"
      (is (equal "low" (nlk:json-value (reasoning-of body) :string "effort")) "a model nothing stops gets its lightest level"))))

(deftest modular-cell-enables-a-model-that-only-toggles ()
  (let ((*round-effort* "high"))
    (with-modular-round (url body) "minimax/minimax-m3"
      (is (eq t (nlk:json-value (reasoning-of body) :boolean "enabled")) "no levels: a pick means think, and the switch says so")
      (is (null (nlk:json-value (reasoning-of body) :string "effort")) "with no level"))))

(deftest modular-cell-leaves-an-unpicked-round-alone ()
  (let ((*round-effort* nil))
    (with-modular-round (url body) "MiniMaxAI/MiniMax-M3.1-Flash"
      (is (null (reasoning-of body)) "a model with no switch and no pick is sent nothing"))
    (with-modular-round (url body) "google/gemma-4-31b-it"
      (is (eq t (nlk:json-value (reasoning-of body) :boolean "enabled")) "a switch model is told to think, as the original did"))))

(deftest modular-cell-leaves-an-undescribed-model-alone ()
  (let ((*round-effort* "high"))
    (with-modular-round (url body) "some/other-model"
      (is (null (reasoning-of body)) "a model the table does not describe is sent no object")
      (is (null (gethash "reasoning_effort" body)) "and not the chat lane's field either"))))

;;; --- the key ----------------------------------------------------------------------

(deftest modular-cell-reads-modular-api-key ()
  (with-cell-stop ((modular-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "MODULAR_API_KEY") "modular-key"))
        (let ((credential (nle::resolve-provider-credential "modular" :auth-path auth :probe t)))
          (is (equal "modular-key" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest modular-cell-never-sends-another-familys-key ()
  (with-cell-stop ((modular-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "modular" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))
