;;;; cell-test.lisp --- the opencode-go cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every catalog is a scratch models.dev api.json (WITH-CATALOG-FIXTURE),
;;;; every store a temp auth.json, every key variable a stubbed
;;;; NLE::CREDENTIAL-ENV, every wire a stubbed dex:post that keeps what it was
;;;; asked and goes no further: nothing touches the network, the environment
;;;; or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "opencode-go" "OPENCODE-GO-CELL-" :start nodecode-opencode-go:start-cell)

(define-cell-lifecycle-tests "opencode-go"
  (:hooks 'nle::models-catalog-table :credential 'nle::resolve-model-lane 'nle::walk-provider-stream)
  (:refused ("base_url" 5)))

(defparameter +opencode-go-catalog+
  "{\"opencode-go\": {\"id\": \"opencode-go\", \"name\": \"OpenCode Go\",
     \"npm\": \"@ai-sdk/openai-compatible\", \"api\": \"https://opencode.ai/inference/go/openai/v1\",
     \"env\": [\"OPENCODE_API_KEY\"],
     \"models\": {
       \"glm-5.3\": {\"id\": \"glm-5.3\", \"name\": \"GLM-5.3\", \"tool_call\": true,
                     \"limit\": {\"context\": 200000, \"output\": 32000}},
       \"claude-haiku-5-5\": {\"id\": \"claude-haiku-5-5\", \"name\": \"Claude Haiku 5.5\", \"tool_call\": true,
                     \"limit\": {\"context\": 200000, \"output\": 32000},
                     \"provider\": {\"npm\": \"@ai-sdk/anthropic\", \"api\": \"https://opencode.ai/inference/go/anthropic/v1\"}},
       \"gpt-6-luna\": {\"id\": \"gpt-6-luna\", \"name\": \"GPT 6 Luna\", \"tool_call\": true,
                     \"limit\": {\"context\": 272000, \"output\": 32000},
                     \"provider\": {\"npm\": \"@ai-sdk/openai\"}},
       \"gemini-4-argon\": {\"id\": \"gemini-4-argon\", \"name\": \"Gemini 4 Argon\", \"tool_call\": true,
                     \"limit\": {\"context\": 1000000, \"output\": 64000},
                     \"provider\": {\"npm\": \"@ai-sdk/google\"}},
       \"minimax-m3\": {\"id\": \"minimax-m3\", \"name\": \"MiniMax M3\", \"tool_call\": true,
                     \"limit\": {\"context\": 200000, \"output\": 32000},
                     \"provider\": {\"npm\": \"@ai-sdk/anthropic\"}},
       \"muse-spark-1.3\": {\"id\": \"muse-spark-1.3\", \"name\": \"Muse Spark 1.3\", \"tool_call\": true,
                     \"limit\": {\"context\": 200000, \"output\": 32000}}}}}"
  "models.dev's OpenCode Go row as it publishes it: the chat package and the
/openai base for the provider, and the per-model package of each model that
speaks another wire.")

(defparameter +opencode-go-base+ "https://opencode.ai/inference/go/v1")

(defun opencode-go-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun opencode-go-round (context)
  "Run CONTEXT's round on the lane its frozen config names."
  (let ((config (nle::compiled-turn-context-provider-config context)))
    (funcall (nle::provider-lane-stream-symbol
              (nle::find-lane-by-name (nle::effective-provider-config-lane config)))
             context)))

(defmacro with-opencode-go-round ((url headers) (provider model &optional session) &body forms)
  "FORMS after one round of PROVIDER MODEL, run by a turn of SESSION (none
when NIL): URL and HEADERS as dex:post was asked them. The stub goes no
further than the asking, so no lane's stream shape matters here."
  `(let ((,url nil) (,headers nil))
     (declare (ignorable ,url ,headers))
     (let ((nle::*provider* ,provider) (nle::*model* ,model) (nle::*api-key* "sk-go")
           (nle::*endpoint* nil))
       (with-stubbed-fdefinition (nle:turn () (and ,session (list :session-id ,session)))
         (with-stubbed-fdefinition
             (dex:post (asked &rest args)
              (setf ,url asked ,headers (getf args :headers))
              (throw 'opencode-go-asked nil))
           (catch 'opencode-go-asked
             (opencode-go-round (user-context))))))
     ,@forms))

(deftest opencode-go-cell-puts-its-base-in-the-catalog ()
  (with-catalog-fixture (path +opencode-go-catalog+)
    (with-cell-stop ((opencode-go-start))
      (let ((row (nlk:json-value (nle::models-catalog-table) :object "opencode-go")))
        (is (equal +opencode-go-base+ (nlk:json-value row :string "api"))
            "one base for every wire, not models.dev's /openai one")
        (is (gethash "glm-5.3" (nlk:json-value row :object "models")) "models.dev's models are kept"))
      (funcall stop)
      (setf stop nil)
      (is (equal "https://opencode.ai/inference/go/openai/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "opencode-go" "api"))
          "a stopped cell leaves the catalog as models.dev made it"))))

(deftest opencode-go-cell-base-follows-the-section ()
  (with-catalog-fixture (path +opencode-go-catalog+)
    (with-cell-stop ((opencode-go-start "base_url" "https://relay.example/go/v1"))
      (is (equal "https://relay.example/go/v1"
                 (nlk:json-value (nle::models-catalog-table) :string "opencode-go" "api"))))))

(deftest opencode-go-cell-routes-each-model-to-its-wire ()
  (with-catalog-fixture (path +opencode-go-catalog+)
    (with-cell-stop ((opencode-go-start))
      (is (equal "openai-completions" (nle::resolve-model-lane "opencode-go" "glm-5.3"))
          "a model models.dev gives no package rides the chat lane")
      (is (equal "anthropic" (nle::resolve-model-lane "opencode-go" "claude-haiku-5-5")))
      (is (equal "openai-responses" (nle::resolve-model-lane "opencode-go" "gpt-6-luna")))
      (is (equal "google" (nle::resolve-model-lane "opencode-go" "gemini-4-argon")))
      (is (equal "openai-completions" (nle::resolve-model-lane "opencode-go" "minimax-m3"))
          "omp's pin outranks models.dev's anthropic package")
      (is (equal "openai-responses" (nle::resolve-model-lane "opencode-go" "muse-spark-1.3"))
          "the muse-spark- prefix rides Responses")
      (is (equal "openai-completions" (nle::resolve-model-lane "opencode-go" "not-listed")))
      (is (equal (concatenate 'string +opencode-go-base+ "/messages")
                 (nle::lane-endpoint "opencode-go" "anthropic")))
      (is (equal (concatenate 'string +opencode-go-base+ "/responses")
                 (nle::lane-endpoint "opencode-go" "openai-responses"))))))

(deftest opencode-go-cell-operator-sdk-outranks-the-route ()
  (with-catalog-fixture (path +opencode-go-catalog+)
    (with-temp-shared-config ("{\"providers\": {\"opencode-go\": {\"models\": [{\"id\": \"claude-haiku-5-5\", \"sdk\": \"openai-completions\"}]}}}")
      (with-cell-stop ((opencode-go-start))
        (is (equal "openai-completions" (nle::resolve-model-lane "opencode-go" "claude-haiku-5-5")))))))

(deftest opencode-go-cell-sends-the-session-header-on-every-wire ()
  (with-catalog-fixture (path +opencode-go-catalog+)
    (with-cell-stop ((opencode-go-start))
      (flet ((sent (model)
               (with-opencode-go-round (url headers) ("opencode-go" model "sess-a")
                 (list url headers))))
        (destructuring-bind (url headers) (sent "glm-5.3")
          (is (equal (concatenate 'string +opencode-go-base+ "/chat/completions") url))
          (is (equal "Bearer sk-go" (opencode-go-header headers "authorization")))
          (is (stringp (opencode-go-header headers "x-opencode-session"))))
        (destructuring-bind (url headers) (sent "claude-haiku-5-5")
          (is (equal (concatenate 'string +opencode-go-base+ "/messages") url))
          (is (equal "sk-go" (opencode-go-header headers "x-api-key"))
              "Go's Messages endpoint takes x-api-key, never a bearer alone")
          (is (stringp (opencode-go-header headers "x-opencode-session"))))
        (destructuring-bind (url headers) (sent "gpt-6-luna")
          (is (equal (concatenate 'string +opencode-go-base+ "/responses") url))
          (is (stringp (opencode-go-header headers "x-opencode-session"))))
        (destructuring-bind (url headers) (sent "gemini-4-argon")
          (is (equal (concatenate 'string +opencode-go-base+
                                  "/models/gemini-4-argon:streamGenerateContent?alt=sse")
                     url))
          (is (equal "sk-go" (opencode-go-header headers "x-goog-api-key")))
          (is (stringp (opencode-go-header headers "x-opencode-session"))))))))

(deftest opencode-go-cell-session-header-is-one-per-session ()
  (with-catalog-fixture (path +opencode-go-catalog+)
    (with-cell-stop ((opencode-go-start))
      (flet ((session (id)
               (with-opencode-go-round (url headers) ("opencode-go" "glm-5.3" id)
                 (opencode-go-header headers "x-opencode-session"))))
        (let ((a (session "discord-1-t2")) (again (session "discord-1-t2"))
              (b (session "discord-1-t3")) (none (session nil)) (none-again (session nil)))
          (is (equal a again) "every round of one session carries one id")
          (is (not (equal a b)) "another session carries another")
          (is (and (alexandria:starts-with-subseq "nodecode-" a) (= 41 (length a))))
          (is (not (search "discord" a)) "the session's own id never leaves")
          (is (and (stringp none) (equal none none-again) (not (equal none a)))
              "a round outside any turn carries this process's own id"))))))

(deftest opencode-go-cell-leaves-other-providers-alone ()
  (with-catalog-fixture (path +opencode-go-catalog+)
    (with-cell-stop ((opencode-go-start))
      (with-opencode-go-round (url headers) ("openai-completions" "kimi-k3" "sess-a")
        (is (stringp url))
        (is (null (opencode-go-header headers "x-opencode-session")))))))

(deftest opencode-go-cell-reads-opencode-api-key ()
  (with-cell-stop ((opencode-go-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENCODE_API_KEY") "sk-oc"))
        (let ((credential (nle::resolve-provider-credential "opencode-go" :auth-path auth :probe t)))
          (is (equal "sk-oc" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest opencode-go-cell-never-sends-another-familys-key ()
  (with-cell-stop ((opencode-go-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (cond ((equal name "OPENAI_API_KEY") "sk-openai")
                                       ((equal name "ANTHROPIC_API_KEY") "sk-anthropic")))
        (let ((credential (nle::resolve-provider-credential "opencode-go" :auth-path auth :probe t)))
          (is (not (member (nle:credential-key credential) '("sk-openai" "sk-anthropic") :test #'equal)))
          (is (eq :public (nle:credential-source credential))))))))

(deftest opencode-go-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((opencode-go-start))
    (with-temp-auth (auth "{\"api_keys\":{\"opencode-go\":{\"provider\":\"opencode-go\",\"key\":\"sk-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (equal name "OPENCODE_API_KEY") "sk-oc"))
        (is (equal "sk-saved"
                   (nle:credential-key (nle::resolve-provider-credential "opencode-go" :auth-path auth :probe t))))))))
