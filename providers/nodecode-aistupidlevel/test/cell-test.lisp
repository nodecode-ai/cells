;;;; cell-test.lisp --- the aistupidlevel cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "aistupidlevel" "AISTUPIDLEVEL-CELL-" :start nodecode-aistupidlevel:start-cell)

(define-cell-lifecycle-tests "aistupidlevel"
  (:hooks 'nle::models-catalog-table :credential)
  (:refused ("base_url" 5)))

(defun aistupidlevel-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-aistupidlevel-round ((url headers body) model &body forms)
  "FORMS with the cell started and one chat round for aistupidlevel MODEL
captured: URL, HEADERS and BODY (the decoded request) as dex:post saw them."
  `(with-cell-stop ((aistupidlevel-start))
     (let ((nle::*provider* "aistupidlevel") (nle::*model* ,model) (nle::*api-key* "aism-test")
           (nle::*endpoint* nil) (,url nil) (,headers nil) (,body nil))
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

;;; --- the catalog, the lane, the address ----------------------------------------------

(deftest aistupidlevel-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((aistupidlevel-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "aistupidlevel")))
      (is (equal "AI Stupid Level" (nlk:json-value row :string "name")))
      (is (equal "https://aistupidlevel.info/v1" (nlk:json-value row :string "api")))
      (let ((models (nlk:json-value row :object "models")))
        (is (gethash "auto-tooling" models) "the routing modes are listed")
        (is (gethash "auto-value" models))
        (is (= 7 (hash-table-count models)) "one row per routing mode, and nothing else"))
      (is (equal "openai-completions" (nle::configured-provider-lane "aistupidlevel")) "the chat lane drives it")
      (is (equal "https://aistupidlevel.info/v1/chat/completions"
                 (nle::lane-endpoint "aistupidlevel" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "aistupidlevel")))))

(deftest aistupidlevel-cell-base-follows-the-section ()
  (with-cell-stop ((aistupidlevel-start "base_url" "https://router.example/v1"))
    (is (equal "https://router.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "aistupidlevel" "api")))
    (is (equal "https://router.example/v1/chat/completions"
               (nle::lane-endpoint "aistupidlevel" "openai-completions")))))

;;; --- the key -------------------------------------------------------------------------

(deftest aistupidlevel-cell-reads-asl-router-key ()
  (with-cell-stop ((aistupidlevel-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "ASL_ROUTER_KEY") "aism-test"))
        (let ((credential (nle::resolve-provider-credential "aistupidlevel" :auth-path auth :probe t)))
          (is (equal "aism-test" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest aistupidlevel-cell-never-sends-another-familys-key ()
  (with-cell-stop ((aistupidlevel-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "aistupidlevel" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest aistupidlevel-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((aistupidlevel-start))
    (with-temp-auth (auth "{\"api_keys\":{\"aistupidlevel\":{\"provider\":\"aistupidlevel\",\"key\":\"aism-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "ASL_ROUTER_KEY") "aism-env"))
        (is (equal "aism-saved"
                   (nle:credential-key (nle::resolve-provider-credential "aistupidlevel" :auth-path auth :probe t))))))))

;;; --- one round -------------------------------------------------------------------------

(deftest aistupidlevel-cell-sends-a-routing-mode-as-it-is ()
  (with-aistupidlevel-round (url headers body) "auto-tooling"
    (is (equal "https://aistupidlevel.info/v1/chat/completions" url))
    (is (equal "Bearer aism-test" (aistupidlevel-header headers "authorization")))
    (is (equal "auto-tooling" (nlk:json-value body :string "model")) "a routing mode goes out under its own id")))

(deftest aistupidlevel-cell-sends-a-pinned-model-as-it-is ()
  (with-aistupidlevel-round (url headers body) "claude-opus-5-5"
    (is (equal "claude-opus-5-5" (nlk:json-value body :string "model")))
    (is (equal "https://aistupidlevel.info/v1/chat/completions" url))))
