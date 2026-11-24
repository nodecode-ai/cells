;;;; cell-test.lisp --- the commandcode cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every round a stubbed dex:post
;;;; answering canned NDJSON, every listing a stubbed NLE::HTTP-FETCH: nothing
;;;; touches the network, the environment or the operator's files.

(in-package #:nodecode.test)

(define-test-slice "commandcode" "COMMANDCODE-CELL-" :start nodecode-commandcode:start-cell)

(define-cell-lifecycle-tests "commandcode"
  (:hooks 'nle::models-catalog-table :credential 'nle::list-provider-models)
  (:running (is (nle::find-lane-by-name "commandcode" nil) "the lane is registered"))
  (:stopped (is (null (nle::find-lane-by-name "commandcode" nil)) "and taken back out"))
  (:refused ("base_url" 5)))

;;; --- fixtures -------------------------------------------------------------------

(defun cc-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun cc-lines (&rest objects)
  "A character stream of OBJECTS (JSON texts), one per line: the endpoint's NDJSON."
  (make-string-input-stream (format nil "~{~a~%~}" objects)))

(defparameter +cc-tool-round+
  (list "{\"type\":\"reasoning-delta\",\"text\":\"Add them.\"}"
        "{\"type\":\"text-delta\",\"text\":\"Adding.\"}"
        "{\"type\":\"tool-call\",\"toolCallId\":\"call_1\",\"toolName\":\"eval\",\"input\":{\"form\":\"(+ 1 2)\"}}"
        "{\"type\":\"finish\",\"totalUsage\":{\"inputTokens\":120,\"outputTokens\":9,\"inputTokenDetails\":{\"cacheReadTokens\":100}}}")
  "A round that thinks, says a word, then calls eval, and finishes.")

(defparameter +cc-text-round+
  (list "{\"type\":\"text-delta\",\"text\":\"ok\"}"
        "{\"type\":\"finish\",\"totalUsage\":{\"inputTokens\":5,\"outputTokens\":1}}")
  "A round that says ok.")

(defun cc-lane-round (context)
  "One round of CONTEXT on the lane its frozen config names, the way the turn loop runs it."
  (let ((config (nle::compiled-turn-context-provider-config context)))
    (funcall (nle::provider-lane-stream-symbol
              (nle::find-lane-by-name (nle::effective-provider-config-lane config)))
             context)))

(defmacro with-cc-round ((values url headers body) (&key (model "deepseek/deepseek-v4.1-flash")
                                                         (lines '+cc-text-round+)
                                                         (context '(user-context "add one and two"))
                                                         config)
                         &body forms)
  "FORMS with the cell started on CONFIG and one round of MODEL streaming
LINES: VALUES the lane's (MESSAGE USAGE FINISH REQUEST-JSON), URL HEADERS BODY
the request as dex:post saw it (BODY decoded)."
  `(with-cell-stop ((commandcode-start ,@config))
     (let ((nle::*provider* "commandcode") (nle::*model* ,model) (nle::*api-key* "ok-test")
           (nle::*endpoint* nil) (,url nil) (,headers nil) (,body nil) (,values nil))
       (declare (ignorable ,url ,headers ,body ,values))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (apply #'cc-lines ,lines) 200))
         (setf ,values (multiple-value-list (cc-lane-round ,context))))
       ,@forms)))

(defun cc-messages (body)
  "The params.messages of a decoded request BODY, as a list."
  (coerce (nlk:json-array (nlk:json-value body :object "params") "messages") 'list))

(defun cc-params (body)
  "The params object of a decoded request BODY."
  (nlk:json-value body :object "params"))

;;; --- the catalog and the lane ---------------------------------------------------------

(deftest commandcode-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((commandcode-start))
    (let* ((row (nlk:json-value (nle::models-catalog-table) :object "commandcode"))
           (models (nlk:json-value row :object "models")))
      (is (equal "Command Code" (nlk:json-value row :string "name")))
      (is (equal "https://api.commandcode.ai" (nlk:json-value row :string "api")))
      (is (equalp #("COMMANDCODE_CLI_TOKEN") (nlk:json-value row :array "env")))
      (is (gethash "moonshotai/Kimi-K3" models) "the CLI's model snapshot is listed")
      (is (equal '("low" "high" "max") (nle::catalog-model-efforts (gethash "moonshotai/Kimi-K3" models))))
      (is (equal "commandcode" (nle::configured-provider-lane "commandcode")) "the cell's own lane drives it"))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "commandcode")))))

(deftest commandcode-cell-base-reduces-to-its-origin ()
  (with-cell-stop ((commandcode-start "base_url" "https://relay.example/provider/v1/"))
    (let ((base (nodecode-commandcode::normalized-base (nodecode-commandcode::setting :base-url))))
      (is (equal "https://relay.example" base) "the Provider API form reduces to its origin"))
    (is (equal "https://relay.example"
               (nlk:json-value (nle::models-catalog-table) :string "commandcode" "api")))))

;;; --- the credential --------------------------------------------------------------------

(defun cc-cli-store (json-text &optional (file ".commandcode/auth.json"))
  "A temp home holding FILE with JSON-TEXT: (values AUTH-FILE HOME) for a
section naming AUTH-FILE's path."
  (let* ((home (uiop:ensure-directory-pathname
                (format nil "~a/" (uiop:temporary-directory))))
         (path (uiop:subpathname home file)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write-string json-text out))
    path))

(deftest commandcode-cell-imports-the-cli-login ()
  (let ((store (cc-cli-store "{\"apiKey\":\"cli-key\"}")))
    (with-cell-stop ((commandcode-start "cli_auth_file" (uiop:native-namestring store)))
      (with-temp-auth (auth "{}")
        (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
          (let ((credential (nle::resolve-provider-credential "commandcode" :auth-path auth :probe t)))
            (is (equal "cli-key" (nle:credential-key credential)) "the CLI's stored key is the key")
            (is (eq :env (nle:credential-source credential)))))))))

(deftest commandcode-cell-never-sends-another-familys-key ()
  (with-cell-stop ((commandcode-start "read_cli_login" nil))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "commandcode" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest commandcode-cell-refuses-a-round-without-a-key ()
  (with-cell-stop ((commandcode-start "read_cli_login" nil))
    (let ((nle::*provider* "commandcode") (nle::*model* "deepseek/deepseek-v4.1-flash") (nle::*api-key* nil)
          (nle::*endpoint* nil))
      (with-temp-auth (auth "{}")
        (let ((nle::*auth-file-path* auth) (posted nil))
          (with-stubbed-fdefinitions ((nle::credential-env (name) (declare (ignore name)) nil)
                                      (dex:post (&rest args) (declare (ignore args)) (setf posted t) (values (cc-lines) 200)))
            (let ((condition (handler-case (progn (cc-lane-round (user-context)) nil)
                               (nle::provider-error (c) c))))
              (is (typep condition 'nle::provider-config-error) "no key is a config refusal")
              (is (search "commandcode.ai" (nle::provider-error-detail condition)))
              (is (null posted) "and nothing is posted"))))))))

;;; --- the request ------------------------------------------------------------------------

(deftest commandcode-cell-posts-the-alpha-generate-request ()
  (with-cc-round (values url headers body)
      (:model "moonshotai/Kimi-K3")
    (is (equal "https://api.commandcode.ai/alpha/generate" url))
    (is (equal "Bearer ok-test" (cc-header headers "authorization")))
    (is (equal "application/json" (cc-header headers "content-type")))
    (is (equal "cli" (cc-header headers "user-agent")) "the endpoint gates on the CLI's identity")
    (is (equal "1.87.1" (cc-header headers "x-command-code-version")))
    (let ((params (cc-params body)))
      (is (equal "moonshotai/Kimi-K3" (nlk:json-value params :string "model")))
      (is (eq t (gethash "stream" params)))
      (is (plusp (or (nlk:json-value params :integer "max_tokens") 0)) "a round carries an output budget")
      (is (plusp (length (or (nlk:json-value params :string "system") ""))) "the prompt rides as one system string")
      (is (equal "user" (nlk:json-value (car (last (cc-messages body))) :string "role"))))
    (is (equal "agent" (nlk:json-value body :string "mode")))
    (let ((workspace (nlk:json-value body :object "config")))
      (is (stringp (nlk:json-value workspace :string "workingDir")) "the workspace block rides along"))
    (is (equal "deepseek/deepseek-v4.1-flash"
               (nodecode-commandcode::canonical-model-id "commandcode/deepseek/deepseek-v4.1-flash")))))

;;; --- the stream -------------------------------------------------------------------------

(deftest commandcode-cell-folds-thinking-text-and-a-tool-call ()
  (with-cc-round (values url headers body) (:lines +cc-tool-round+)
    (destructuring-bind (message usage finish request-json) values
      (declare (ignore request-json))
      (is (equal "Adding." (nlk:json-value message :string "content")))
      (is (equal "Add them." (nlk:json-value message :string "reasoning_content")))
      (let ((call (aref (nlk:json-value message :array "tool_calls") 0)))
        (is (equal "eval" (nlk:json-value call :string "function" "name")))
        (is (equal "call_1" (nlk:json-value call :string "id")))
        (let ((arguments (nlk:json-value call :string "function" "arguments")))
          (is (search "(+ 1 2)" arguments) "the call's arguments go out as JSON text")))
      (is (equal "tool_calls" finish))
      (is (= 120 (nle::provider-usage-input-tokens usage)))
      (is (= 9 (nle::provider-usage-output-tokens usage)))
      (is (= 100 (nle::provider-usage-cached-input-tokens usage))))))

(deftest commandcode-cell-fails-closed-on-an-unfinished-stream ()
  (with-cell-stop ((commandcode-start))
    (let ((nle::*provider* "commandcode") (nle::*model* "deepseek/deepseek-v4.1-flash")
          (nle::*api-key* "ok-test") (nle::*endpoint* nil))
      (with-stubbed-fdefinition
          (dex:post (&rest args)
           (declare (ignore args))
           (values (cc-lines "{\"type\":\"text-delta\",\"text\":\"half\"}") 200))
        (let ((condition (handler-case (progn (cc-lane-round (user-context "hi")) nil)
                           (nle::provider-stream-incomplete (c) c))))
          (is (typep condition 'nle::provider-stream-incomplete) "an unfinished stream fails closed"))))))
