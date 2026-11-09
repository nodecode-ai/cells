;;;; cell-test.lisp --- the fortytwo cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post:
;;;; nothing touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "fortytwo" "FORTYTWO-CELL-" :start nodecode-fortytwo:start-cell)

(define-cell-lifecycle-tests "fortytwo"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body
          'nle::walk-provider-stream)
  (:refused ("base_url" 5)))

(defun ft-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-fortytwo-round ((url headers body) (model &key effort config) &body forms)
  "FORMS with the cell started on CONFIG (section pairs) and one chat round
for MODEL at EFFORT captured: URL, HEADERS and BODY (the decoded request) as
dex:post saw them."
  `(with-cell-stop ((fortytwo-start ,@config))
     (let ((nle::*provider* "fortytwo") (nle::*model* ,model) (nle::*api-key* "sk-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* ,effort)
           (,url nil) (,headers nil) (,body nil))
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

;;; --- the catalog ------------------------------------------------------------------

(deftest fortytwo-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((fortytwo-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "fortytwo")))
      (is (equal "Fortytwo" (nlk:json-value row :string "name")))
      (is (equal "https://api.fortytwo.network/v1" (nlk:json-value row :string "api")))
      (is (gethash "qwen/qwen3.8-27b" (nlk:json-value row :object "models")) "the one model is listed")
      (is (equal '("FORTYTWO_API_KEY") (coerce (nlk:json-value row :array "env") 'list)))
      (is (equal "openai-completions" (nle::configured-provider-lane "fortytwo")) "the chat lane drives it")
      (is (equal "https://api.fortytwo.network/v1/chat/completions"
                 (nle::lane-endpoint "fortytwo" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "fortytwo")))))

(deftest fortytwo-cell-advertises-the-models-facts ()
  (with-cell-stop ((fortytwo-start))
    (let* ((capability (nle::resolve-model-capability "qwen/qwen3.8-27b" "fortytwo"))
           (ladder (nle::model-capability-effort-levels capability)))
      (is (= 262144 (nle::model-capability-context-tokens capability)) "the window the original advertised")
      (is (equal '("off" "low" "medium" "xhigh") ladder) "the four rungs Fortytwo takes, and no others")
      (is (nle::model-capability-reasoning-p capability) "it reasons")
      (is (eq t (nle::model-capability-known-p capability)) "the cell taught the catalog"))))

(deftest fortytwo-cell-base-follows-the-section ()
  (with-cell-stop ((fortytwo-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "fortytwo" "api")))))

;;; --- the key ----------------------------------------------------------------------

(deftest fortytwo-cell-reads-its-key-variable ()
  (with-cell-stop ((fortytwo-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "FORTYTWO_API_KEY") "sk-fortytwo"))
        (let ((credential (nle::resolve-provider-credential "fortytwo" :auth-path auth :probe t)))
          (is (equal "sk-fortytwo" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest fortytwo-cell-never-sends-another-familys-key ()
  (with-cell-stop ((fortytwo-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "fortytwo" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest fortytwo-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((fortytwo-start))
    (with-temp-auth (auth "{\"api_keys\":{\"fortytwo\":{\"provider\":\"fortytwo\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "FORTYTWO_API_KEY") "sk-fortytwo"))
        (is (equal "sk-saved"
                   (nle:credential-key (nle::resolve-provider-credential "fortytwo"
                                                                        :auth-path auth :probe t))))))))

;;; --- one round --------------------------------------------------------------------

(deftest fortytwo-cell-sends-a-round-with-attribution ()
  (with-fortytwo-round (url headers body) ("qwen/qwen3.8-27b")
    (is (equal "https://api.fortytwo.network/v1/chat/completions" url))
    (is (equal "qwen/qwen3.8-27b" (nlk:json-value body :string "model")) "the model id goes as it is")
    (is (equal "Bearer sk-test" (ft-header headers "authorization")))
    (is (equal "https://github.com/Fortytwo-Network/fortytwo-hermes"
               (ft-header headers "HTTP-Referer")))
    (is (equal "Nodecode" (ft-header headers "X-Title")))))

(deftest fortytwo-cell-asks-for-one-of-its-four-efforts ()
  (with-fortytwo-round (url headers body) ("qwen/qwen3.8-27b" :effort "low")
    (is (equal "low" (nlk:json-value body :string "reasoning_effort"))))
  (with-fortytwo-round (url headers body) ("qwen/qwen3.8-27b" :effort "xhigh")
    (is (equal "xhigh" (nlk:json-value body :string "reasoning_effort"))))
  (with-fortytwo-round (url headers body) ("qwen/qwen3.8-27b" :effort "high")
    (is (equal "medium" (nlk:json-value body :string "reasoning_effort")) "a wider rung rounds down"))
  (with-fortytwo-round (url headers body) ("qwen/qwen3.8-27b" :effort "max")
    (is (equal "xhigh" (nlk:json-value body :string "reasoning_effort"))))
  (with-fortytwo-round (url headers body) ("qwen/qwen3.8-27b" :effort "minimal")
    (is (equal "none" (nlk:json-value body :string "reasoning_effort"))))
  (with-fortytwo-round (url headers body) ("qwen/qwen3.8-27b" :effort "off")
    (is (equal "none" (nlk:json-value body :string "reasoning_effort")) "a disabled round says so"))
  (with-fortytwo-round (url headers body) ("qwen/qwen3.8-27b")
    (is (null (nth-value 1 (gethash "reasoning_effort" body))) "no effort: the API default")))

(deftest fortytwo-cell-replaces-a-stale-attribution-header ()
  (with-cell-stop ((fortytwo-start))
    (with-stubbed-fdefinition (nodecode-fortytwo::ours-p (&rest args) t)
      (let ((seen nil))
        (nodecode-fortytwo::walk
         (lambda (fold &rest args)
           (declare (ignore fold))
           (setf seen (getf args :headers))
           nil)
         nil :config nil
         :headers '(("X-Title" . "another-client") ("Accept" . "text/event-stream")))
        (is (equal "Nodecode" (ft-header seen "X-Title")))
        (is (= 1 (count "x-title" seen :key #'car :test #'string-equal)) "one copy of a header this cell owns")
        (is (equal "text/event-stream" (ft-header seen "Accept")) "the lane's own headers stay")))))

(deftest fortytwo-cell-leaves-other-providers-alone ()
  (with-cell-stop ((fortytwo-start))
    (let ((nle::*provider* "openai-completions") (nle::*model* "qwen/qwen3.8-27b")
          (nle::*api-key* "k") (nle::*endpoint* nil) (nle::*reasoning-effort* "high")
          (headers nil) (body nil))
      (with-stubbed-fdefinition
          (dex:post (asked &rest args)
           (is (stringp asked))
           (setf headers (getf args :headers) body (nlk:decode-json (getf args :content)))
           (values (make-truncated-sse-stream
                    "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}" "[DONE]")
                   200))
        (nle::call-provider-streaming (user-context)))
      (is (equal "high" (nlk:json-value body :string "reasoning_effort")) "another provider's rung is not rounded")
      (is (null (ft-header headers "X-Title")) "and carries no attribution"))))
