;;;; cell-test.lisp --- the requesty cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every store is a temp auth.json named through :auth-path, every key
;;;; variable a stubbed NLE::CREDENTIAL-ENV, every wire a stubbed dex:post or
;;;; NLE:HTTP: nothing touches the network, the environment or the operator's
;;;; files.

(in-package #:nodecode.test)

(define-test-slice "requesty" "REQUESTY-CELL-" :start nodecode-requesty:start-cell)

(define-cell-lifecycle-tests "requesty"
  (:hooks 'nle::models-catalog-table :credential 'nle::request-body)
  (:command "requesty")
  (:refused ("base_url" 5)))

(defun requesty-header (headers name)
  "The value HEADERS, an alist, carries for NAME, case-insensitively."
  (cdr (assoc name headers :test #'string-equal)))

(defun requesty-stream ()
  "One whole chat answer, `ok'."
  (make-truncated-sse-stream
   "{\"choices\":[{\"index\":0,\"delta\":{\"content\":\"ok\"}}]}"
   "{\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
   "[DONE]"))

(defun requesty-context (&optional (session "s1"))
  "A compiled turn context for SESSION, holding one user message, built under
whatever provider specials the caller bound."
  (nle::compile-turn-context
   (nle::snapshot-context-request session :messages (list (nle::message "user" "x")))))

(defmacro with-requesty-round ((url headers body trace) (model &key effort)
                                (&rest section) &body forms)
  "FORMS after one chat round of MODEL at EFFORT, the cell started with
SECTION: URL, HEADERS and BODY (decoded) as dex:post saw them, and TRACE the
session id the compiled context carried."
  `(with-cell-stop ((requesty-start ,@section))
     (let ((nle::*provider* "requesty") (nle::*model* ,model) (nle::*api-key* "sk-rq-test")
           (nle::*endpoint* nil) (nle::*reasoning-effort* ,effort)
           (nle::*model-capability-memo* nil)
           (,trace "s1") (,url nil) (,headers nil) (,body nil))
       (declare (ignorable ,url ,headers ,body))
       (with-stubbed-fdefinition
           (dex:post (asked &rest args)
            (setf ,url asked ,headers (getf args :headers)
                  ,body (nlk:decode-json (getf args :content)))
            (values (requesty-stream) 200))
         (nle::call-provider-streaming (requesty-context ,trace)))
       ,@forms)))

;;; --- the catalog, the lane, the address ----------------------------------------------

(deftest requesty-cell-puts-its-row-in-the-catalog ()
  (with-cell-stop ((requesty-start))
    (let ((row (nlk:json-value (nle::models-catalog-table) :object "requesty")))
      (is (equal "Requesty" (nlk:json-value row :string "name")))
      (is (equal "https://router.requesty.ai/v1" (nlk:json-value row :string "api")))
      (is (gethash "claude-sonnet-5-5" (nlk:json-value row :object "models")) "the bundled rows are listed")
      (is (equal "openai-completions" (nle::configured-provider-lane "requesty")) "the chat lane drives it")
      (is (equal "https://router.requesty.ai/v1/chat/completions"
                 (nle::lane-endpoint "requesty" "openai-completions"))))
    (funcall stop)
    (setf stop nil)
    (is (null (nlk:json-value (nle::models-catalog-table) :object "requesty")))))

(deftest requesty-cell-base-follows-the-section ()
  (with-cell-stop ((requesty-start "base_url" "https://router.example/v1"))
    (is (equal "https://router.example/v1"
               (nlk:json-value (nle::models-catalog-table) :string "requesty" "api")))))

;;; --- the key -------------------------------------------------------------------------

(deftest requesty-cell-reads-requesty-api-key ()
  (with-cell-stop ((requesty-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "REQUESTY_API_KEY") "sk-rq"))
        (let ((credential (nle::resolve-provider-credential "requesty" :auth-path auth :probe t)))
          (is (equal "sk-rq" (nle:credential-key credential)))
          (is (eq :env (nle:credential-source credential))))))))

(deftest requesty-cell-never-sends-another-familys-key ()
  (with-cell-stop ((requesty-start))
    (with-temp-auth (auth "{}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "OPENAI_API_KEY") "sk-openai"))
        (let ((credential (nle::resolve-provider-credential "requesty" :auth-path auth :probe t)))
          (is (not (equal "sk-openai" (nle:credential-key credential))))
          (is (eq :public (nle:credential-source credential))))))))

(deftest requesty-cell-saved-key-outranks-the-variable ()
  (with-cell-stop ((requesty-start))
    (with-temp-auth (auth "{\"api_keys\":{\"requesty\":{\"provider\":\"requesty\",\"key\":\"sk-rq-saved\"}}}")
      (with-stubbed-fdefinition (nle::credential-env (name) (and (equal name "REQUESTY_API_KEY") "sk-rq-env"))
        (is (equal "sk-rq-saved"
                   (nle:credential-key (nle::resolve-provider-credential "requesty" :auth-path auth :probe t))))))))

;;; --- one round -------------------------------------------------------------------------

(deftest requesty-cell-sends-a-round-as-the-original-does ()
  (with-requesty-round (url headers body trace) ("claude-sonnet-5-5" :effort "xhigh") ()
    (is (equal "https://router.requesty.ai/v1/chat/completions" url))
    (is (equal "Bearer sk-rq-test" (requesty-header headers "authorization")))
    (is (equal "claude-sonnet-5-5" (nlk:json-value body :string "model")))
    (is (equal "high" (nlk:json-value body :string "reasoning_effort")) "xhigh goes out as Requesty's high")
    (is (equal trace (nlk:json-value body :string "requesty" "trace_id")) "the session id rides as the trace")))

(deftest requesty-cell-sends-off-as-none ()
  (with-requesty-round (url headers body trace) ("claude-opus-5-5" :effort "off") ()
    (is (equal "https://router.requesty.ai/v1/chat/completions" url))
    (is (equal "none" (nlk:json-value body :string "reasoning_effort")) "an off selection is Requesty's none")))

(deftest requesty-cell-keeps-reasoning-off-a-model-the-catalog-does-not-mark ()
  (with-requesty-round (url headers body trace) ("chroma-1" :effort "high") ()
    (is (equal "https://router.requesty.ai/v1/chat/completions" url))
    (is (null (nlk:json-value body :string "reasoning_effort")) "no reasoning_effort to an unmarked model")))

;;; --- the managed catalog --------------------------------------------------------------

(defparameter *requesty-managed*
  "{\"data\":[{\"id\":\"claude-sonnet-5-5\",\"supports_tool_calling\":true,\"api\":\"chat\",\"context_window\":200000,\"supports_reasoning\":true},{\"id\":\"gpt-6.1-sol@eu\",\"supports_tool_calling\":true,\"supports_reasoning\":true},{\"id\":\"embed-only\",\"supports_tool_calling\":false,\"api\":\"embeddings\"}]}"
  "A managed catalog as GET /models/managed answers it: two routes a turn can
take, one it cannot.")

(deftest requesty-cell-command-lists-the-managed-policies ()
  (with-cell-stop ((requesty-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore options))
                                (is (equal :get method))
                                (is (search "/models/managed" url))
                                (values *requesty-managed* 200 nil))
      (let ((answer (nodecode-requesty::run-command "" "s1")))
        (is (search "claude-sonnet-5-5" answer))
        (is (search "gpt-6.1-sol@eu" answer))
        (is (null (search "embed-only" answer)) "a route that takes no tool calls is left out")
        (is (gethash "gpt-6.1-sol@eu" (nlk:json-value (nle::models-catalog-table)
                                                       :object "requesty" "models")) "/models offers it")))))

(deftest requesty-cell-command-narrows-by-text ()
  (with-cell-stop ((requesty-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (values *requesty-managed* 200 nil))
      (let ((answer (nodecode-requesty::run-command "gpt" "s1")))
        (is (search "gpt-6.1-sol@eu" answer))
        (is (null (search "claude-sonnet-5-5" answer)))))))

(deftest requesty-cell-command-says-why-the-catalog-did-not-answer ()
  (with-cell-stop ((requesty-start))
    (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url options))
                                (values "nope" 503 nil))
      (is (search "HTTP 503" (nodecode-requesty::run-command "" "s1"))))))
