;;;; cell-test.lisp --- the pethost cell against a stubbed MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP and the token a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches Pethost or the environment. The tools
;;;; are called the way a model's call reaches them -- through the real
;;;; dispatch, contract and all -- so the schema the provider is shown and the
;;;; body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "pethost" "PETHOST-CELL-" :start nodecode-pethost:start-cell
  :defaults (("token_env" "PETHOST_TOKEN")))

;;; --- fixtures --------------------------------------------------------------------

(defparameter *calls* nil
  "Every request the stub answered, oldest first, as (METHOD PARAMS HEADERS).")

(defparameter *answer* nil
  "When set, the plist (:TEXT :STATUS :HEADERS) the stub answers to tools/call.")

(defun envelope (id result)
  "One JSON-RPC answer carrying RESULT, as the wire spells it."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun call-result (params)
  "What the stub's tools/call answers: the machine for GetMachine, one line otherwise."
  (let ((tool (nlk:json-value params :string "name")))
    (if (equal tool "GetMachine")
        (nlk:json-object
         "content" (vector (nlk:json-object "type" "text" "text" "the machine"))
         "structuredContent"
         (nlk:json-object "machine" (nlk:json-object "name" "repro"
                                                       "apps_domain" "sam.pethost.app")
                          "projects" (vector (nlk:json-object "project_id" "recipes"))))
        (nlk:json-object "content" (vector (nlk:json-object "type" "text"
                                                              "text" (format nil "~a answered" tool)))))))

(defparameter *tool-list*
  (nlk:json-object
   "tools"
   (vector (nlk:json-object "name" "GetMachine"
                             "description" "The machine and every project on it.")
           (nlk:json-object "name" "DeployProject"
                             "description" "Deploy a new version of a project.")
           (nlk:json-object "name" "QueryContainerLogs"
                             "description" "The containers' logs, filtered.")))
  "The catalog the stub's tools/list answers.")

(defun session-headers ()
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) "sess-1")
    headers))

(defun stub-wire (method url &rest options)
  "NLK:HTTP, answering from the request's own method; nothing leaves the machine."
  (declare (ignore method url))
  (let* ((body (nlk:decode-json (or (getf options :content) "{}")))
         (name (nlk:json-value body :string "method"))
         (params (nlk:json-value body :object "params"))
         (id (nlk:json-value body :integer "id")))
    (push (list name params (getf options :headers)) *calls*)
    (cond ((equal name "initialize")
           (values (envelope id (nlk:json-object "protocolVersion" "2025-06-18"
                                                   "capabilities" (nlk:make-json-object)))
                   200 (session-headers)))
          ((equal name "notifications/initialized")
           (values "" 202 nil))
          ((equal name "tools/list")
           (values (envelope id *tool-list*) 200 nil))
          ((equal name "tools/call")
           (values (or (getf *answer* :text) (envelope id (call-result params)))
                   (or (getf *answer* :status) 200)
                   (getf *answer* :headers)))
          (t (values "{}" 404 nil)))))

(defmacro with-pethost (&body body)
  "BODY with the token answering, the request log emptied, no scripted answer."
  `(let ((*calls* '()) (*answer* nil))
     (with-stubbed-fdefinition (nle::credential-env (name)
                                 (and (stringp name) (equal name "PETHOST_TOKEN")
                                      "test-token"))
       ,@body)))

(defmacro with-pethost-wire (&body body)
  "BODY with NLK:HTTP stubbed; it records every request in *CALLS*."
  `(with-stubbed-fdefinition (nlk:http (method url &rest options)
                              (apply #'stub-wire method url options))
     ,@body))

(defun answer-of (id tool arguments)
  "The tool's answer to ARGUMENTS, as the model reads it: the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun request-record (name)
  "The last request of METHOD NAME: (METHOD PARAMS HEADERS), or NIL."
  (find name *calls* :key #'first :test #'equal))

(defun request-header (record header)
  (cdr (assoc header (third record) :test #'string-equal)))

(defun methods-called ()
  (mapcar #'first (reverse *calls*)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "pethost"
  (:config)
  (:help :pethost)
  (:command "pethost")
  (:fixture with-pethost)
  (:refused ("timeout" 0) ("timeout" "sixty") ("url" 5)))

(deftest pethost-cell-without-a-token-refuses ()
  ;; The one thing no verb can work without: the section names a variable, and a
  ;; variable that is unset stops the start before anything installs.
  (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
    (let ((condition (signals-error nlk:config-refusal (pethost-start))))
      (is (search "PETHOST_TOKEN" (princ-to-string condition)) "the refusal names the variable")
      (is (null (nle::find-tool "pethost_call")) "and installs nothing"))))

;;; --- pethost_machine ----------------------------------------------------------------

(deftest pethost-cell-the-machine-tool-asks-the-server (with-pethost)
  (with-cell-stop ((pethost-start))
    (with-pethost-wire ()
      (let* ((answer (answer-of "call-1" "pethost_machine" "{}"))
             (object (nlk:decode-json answer)))
        (is (equal "repro" (nlk:json-value object :string "machine" "name")) "the machine's own name reaches the model")
        (is (= 1 (length (nlk:json-array object "projects"))) "and its projects")
        (is (equal '("initialize" "notifications/initialized" "tools/call") (methods-called)) "one sign-in, its notification, then the call")
        (let ((record (request-record "tools/call")))
          (is (equal "GetMachine" (nlk:json-value (second record) :string "name")))
          (is (equal "Bearer test-token" (request-header record "authorization")) "the token rides the request, from the section")
          (is (equal "sess-1" (request-header record "mcp-session-id")) "the session id the server handed at initialize"))))))

;;; --- pethost_call ---------------------------------------------------------------

(deftest pethost-cell-the-call-tool-carries-its-arguments (with-pethost)
  (with-cell-stop ((pethost-start))
    (with-pethost-wire ()
      (let ((answer (answer-of "call-2" "pethost_call"
                                "{\"tool\":\"GetProject\",\"arguments\":{\"project_id\":\"recipes\"}}")))
        (is (search "GetProject answered" answer) "the server's text is the answer")
        (let ((params (second (request-record "tools/call"))))
          (is (equal "GetProject" (nlk:json-value params :string "name")))
          (is (equal "recipes" (nlk:json-value params :string "arguments" "project_id")) "and the arguments go through untouched"))))))

(deftest pethost-cell-a-tool-error-is-a-failed-call (with-pethost)
  (with-cell-stop ((pethost-start))
    (with-pethost-wire ()
      (setf *answer* (list :text (envelope 3
                                           (nlk:json-object
                                            "content" (vector (nlk:json-object
                                                               "type" "text"
                                                               "text" "project recipes not found"))
                                            "isError" t))))
      (let ((answer (answer-of "call-3" "pethost_machine" "{}")))
        (is (search "ERROR:" answer) "a refused call is a failed call, not a success")
        (is (search "project recipes not found" answer) "in the server's own words")))))

(deftest pethost-cell-a-token-refused-says-to-sign-in (with-pethost)
  (with-cell-stop ((pethost-start))
    (with-pethost-wire ()
      (setf *answer* (list :text "{\"jsonrpc\":\"2.0\",\"id\":4,\"error\":{\"message\":\"invalid_token\"}}"
                           :status 401))
      (let ((answer (answer-of "call-4" "pethost_machine" "{}")))
        (is (search "ERROR:" answer))
        (is (search "401" answer) "the status is said")
        (is (search "Sign in" answer) "and the remedy with it")))))

(deftest pethost-cell-a-streamed-answer-is-read (with-pethost)
  (with-cell-stop ((pethost-start))
    (with-pethost-wire ()
      (setf *answer*
            (list :text (format nil "event: message~%data: ~a~%~%"
                                 (envelope 5 (nlk:json-object
                                              "content" (vector (nlk:json-object
                                                                 "type" "text"
                                                                 "text" "from the stream")))))))
      (is (search "from the stream" (answer-of "call-5" "pethost_machine" "{}")) "a streamed body is the same answer"))))

;;; --- pethost_tools ---------------------------------------------------------------

(deftest pethost-cell-the-tools-tool-lists-and-filters (with-pethost)
  (with-cell-stop ((pethost-start))
    (with-pethost-wire ()
      (let ((all (answer-of "call-6" "pethost_tools" "{}")))
        (is (search "GetMachine" all))
        (is (search "QueryContainerLogs" all) "every tool the server lists")
        (is (search "The containers' logs" all) "with what it does"))
      (let ((filtered (answer-of "call-7" "pethost_tools" "{\"filter\":\"deploy\"}")))
        (is (search "DeployProject" filtered))
        (is (null (search "QueryContainerLogs" filtered)) "the filter keeps only what matches")))))

;;; --- the command ---------------------------------------------------------------------

(deftest pethost-cell-the-command-answers-through-stubbed-io (with-pethost)
  (with-cell-stop ((pethost-start))
    (with-pethost-wire ()
      (let ((line (cell-entry "nodecode-pethost" "pethost" "")))
        (is (search "https://mcp.pethost.dev/mcp" line) "the status names the server")
        (is (search "PETHOST_TOKEN" line) "and the variable the token is read from")
        (is (null *calls*) "status sends nothing"))
      (is (search "repro" (cell-entry "nodecode-pethost" "pethost" "ping")) "ping asks the server")
      (is (search "GetMachine" (cell-entry "nodecode-pethost" "pethost" "tools")))
      (is (search "Usage" (cell-entry "nodecode-pethost" "pethost" "wobble")) "an unknown verb says what the verbs are"))))

;;; --- a verb with nothing started --------------------------------------------------------

(deftest pethost-cell-a-verb-refuses-while-not-running (with-pethost)
  (with-cell-stop ((pethost-start))
    (let ((nodecode-pethost::*pethost* nil))
      (with-pethost-wire ()
        (is (search "not running" (nodecode-pethost:status)) "the refusal says the cell is not on")
        (is (search "not running" (cell-entry "nodecode-pethost" "pethost" "")))
        (is (null *calls*) "and nothing left the machine")))))

;;; --- the same call, from eval ------------------------------------------------------------

(deftest pethost-cell-eval-reaches-the-same-server (with-pethost)
  (with-cell-stop ((pethost-start))
    (with-pethost-wire ()
      (is (search "repro" (nodecode-pethost:call-tool "GetMachine")) "call-tool is the tool's own body"))))
