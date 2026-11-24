;;;; cell-test.lisp --- the netmind cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store here is a temp auth.json named through :auth-path, the
;;;; environment a stubbed NLE::CREDENTIAL-ENV, and the wire a stubbed
;;;; DEX:POST: nothing touches the network, the environment or the operator's
;;;; files. The roster is the one the plugin bundles, so no listing is asked
;;;; of anything.

(in-package #:nodecode.test)

(define-test-slice "netmind" "NETMIND-CELL-" :start nodecode-netmind:start-cell)

(define-cell-lifecycle-tests "netmind"
  (:hooks 'nle::models-catalog-table)
  (:command "netmind")
  (:refused ("base_url" 5) ("base_url" "ftp://api.example/v1")))

;;; --- fixtures ----------------------------------------------------------------

(defun netmind-row ()
  "The catalog row NetMind's hook puts in, or NIL."
  (nlk:json-value (nle::models-catalog-table) :object "netmind"))

(defun netmind-models ()
  "The models NetMind's row carries, id -> CATALOG-MODEL."
  (nlk:json-value (netmind-row) :object "models"))

(defun netmind-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defmacro with-netmind-round ((url headers body &optional config) model &body forms)
  "FORMS with the cell started (its section CONFIG) and one chat round for
netmind MODEL captured: URL, HEADERS and BODY (the decoded request) as
dex:post saw them."
  `(with-cell-stop ((apply #'netmind-start ,config))
     (let ((nle::*provider* "netmind") (nle::*model* ,model) (nle::*api-key* "sk-test")
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

;;; --- the row, the address ----------------------------------------------------

(deftest netmind-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((netmind-start))
    (let* ((row (netmind-row))
           (models (netmind-models))
           (haiku (gethash "anthropic/claude-haiku-4-5" models)))
      (is (equal "NetMind" (nlk:json-value row :string "name")))
      (is (equal "https://api.netmind.ai/inference-api/v1" (nlk:json-value row :string "api")))
      (let ((vars (coerce (nlk:json-value row :array "env") 'list)))
        (is (equal '("NETMIND_API_KEY") vars) "plugin.yaml's key variable"))
      (is (= 31 (hash-table-count models)) "the whole bundled roster is carried")
      (is (eql 200000 (nle::catalog-model-context haiku)) "with the window the plugin declares")
      (is (eql 64000 (nle::catalog-model-output haiku)) "and its answer cap")
      (is (nle::catalog-model-reasoning-p haiku))
      (is (nle::catalog-model-tool-call-p haiku))
      (is (equal '("text" "image") (coerce (nle::catalog-model-inputs haiku) 'list)))
      (is (null (nle::catalog-model-tool-call-p (gethash "openai/gpt-6-astra" models))) "no tools")
      (is (equal "openai-completions" (nle::configured-provider-lane "netmind")) "the chat lane")
      (is (equal "https://api.netmind.ai/inference-api/v1/chat/completions"
                 (nle::lane-endpoint "netmind" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (netmind-row)) "and the row goes at stop")))

(deftest netmind-cell-base-follows-the-section ()
  (with-cell-stop ((netmind-start "base_url" "https://relay.example/v1"))
    (is (equal "https://relay.example/v1" (nlk:json-value (netmind-row) :string "api")))
    (is (equal "https://relay.example/v1/chat/completions"
               (nle::lane-endpoint "netmind" "openai-completions")))))

(deftest netmind-cell-refuses-a-base-a-key-should-not-go-to ()
  ;; an empty member reads as absent, so the default base stands; the rest are refused
  (with-cell-stop ((netmind-start "base_url" ""))
    (is (equal "https://api.netmind.ai/inference-api/v1"
               (nlk:json-value (netmind-row) :string "api"))))
  (dolist (bad '("api.netmind.ai/inference-api/v1" "http://api.netmind.ai/inference-api/v1"
                 "https://user:pw@api.example/v1" "https://api.example/v1?key=1"
                 "https://api.example/v1#x" "ftp://api.example/v1" "https://api.example:0/v1"))
    (let ((text (refusal-text nlk:config-refusal (netmind-start "base_url" bad))))
      (is (search "base_url" text) (format nil "~s is refused, naming the member" bad))))
  ;; a relay on this machine answers over plain http
  (with-cell-stop ((netmind-start "base_url" "http://127.0.0.1:8080/v1"))
    (is (equal "http://127.0.0.1:8080/v1" (nlk:json-value (netmind-row) :string "api")))))

;;; --- the key ------------------------------------------------------------------

(deftest netmind-cell-reads-netmind-api-key ()
  (with-cell-stop ((netmind-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "NETMIND_API_KEY") "sk-netmind"))
        (let ((credential (nle::resolve-provider-credential "netmind" :auth-path auth :probe t)))
          (is (equal "sk-netmind" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest netmind-cell-never-sends-another-familys-key ()
  (with-cell-stop ((netmind-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "netmind" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))) "not the family's key")
          (is (eq :public (nle:credential-source credential))))))))

(deftest netmind-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((netmind-start))
    (with-temp-auth (auth "{\"api_keys\":{\"netmind\":{\"provider\":\"netmind\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "NETMIND_API_KEY") "sk-netmind"))
        (is (equal "sk-saved"
                   (nle:credential-key (nle::resolve-provider-credential
                                        "netmind" :auth-path auth :probe t))))))))

;;; --- the turn -----------------------------------------------------------------

(deftest netmind-cell-turn-goes-to-the-chat-lane ()
  (with-netmind-round (url headers body) "anthropic/claude-haiku-4-5"
    (is (equal "https://api.netmind.ai/inference-api/v1/chat/completions" url) "the row's base")
    (is (equal "Bearer sk-test" (netmind-header headers "authorization")) "with the key on it")
    (is (equal "anthropic/claude-haiku-4-5" (nlk:json-value body :string "model")) "its id")))

(deftest netmind-cell-turn-follows-the-section-base ()
  (with-netmind-round (url headers body '("base_url" "https://relay.example/v1")) "Qwen/Qwen3.8-Flash"
    (is (equal "https://relay.example/v1/chat/completions" url))
    (is (equal "Qwen/Qwen3.8-Flash" (nlk:json-value body :string "model")))))

;;; --- the command ----------------------------------------------------------------

(deftest netmind-cell-command-lists-the-roster ()
  (with-cell-stop ((netmind-start))
    (with-temp-auth (auth "{}")
      (nlk:bind ((nle::*auth-file-path* auth))
        (let* ((text (cell-entry "nodecode-netmind" "netmind" ""))
               (haiku "anthropic/claude-haiku-4-5 (200,000 context, 64,000 answer tokens)")
               (astra "openai/gpt-6-astra (1,000,000 context, 128,000 answer tokens; no tool calls)"))
          (is (search "NetMind at https://api.netmind.ai/inference-api/v1 bundles 31 models" text))
          (is (search "no key yet" text) "where a key comes from")
          (is (search haiku text) "a model with its window and answer cap")
          (is (search astra text) "and a model the picker leaves out"))))))

(deftest netmind-cell-command-answers-the-key ()
  (with-cell-stop ((netmind-start))
    (with-temp-auth (auth "{}")
      (nlk:bind ((nle::*auth-file-path* auth))
        (let ((text (cell-entry "nodecode-netmind" "netmind" "key")))
          (is (search "Make a key at https://www.netmind.ai/" text))
          (is (search "NETMIND_API_KEY" text)))
        (is (search "usage: /netmind"
                    (cell-entry "nodecode-netmind" "netmind" "everything")))))))
