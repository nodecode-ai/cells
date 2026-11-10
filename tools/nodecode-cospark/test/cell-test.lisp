;;;; cell-test.lisp --- the cell against a stubbed Cospark MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Cospark, no token
;;;; is needed and no credit is spent. What is proved is the cell's own wiring:
;;;; the section (a healthy start, and a start with no sign-in refused), the
;;;; three tools through the dispatch the model calls them by -- the MCP method
;;;; and the tool each one names, the bearer token and the session id on the
;;;; wire, the person's yes before anything that spends -- and the /cospark
;;;; command.

(in-package #:nodecode.test)

(define-test-slice "cospark" "COSPARK-CELL-" :start nodecode-cospark:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *calls* nil
  "Every request the stub answered, newest first, as (METHOD PARAMS HEADERS).")

(defparameter *answer* nil
  "When set, (TEXT STATUS HEADERS): what the stub answers to tools/call.")

(defparameter *offered*
  (vector
   (nlk:json-object "name" "read_docs"
                     "description" "Cospark's maintained workflow guides. Call with no path for the index."
                     "annotations" (nlk:json-object "readOnlyHint" t)
                     "inputSchema" (nlk:json-object
                                    "type" "object"
                                    "properties" (nlk:json-object
                                                  "path" (nlk:json-object "type" "string"))))
   (nlk:json-object "name" "search_ad_references"
                     "description" "Find video ad references for a category and return contact sheets."
                     "annotations" (nlk:json-object "readOnlyHint" t))
   (nlk:json-object "name" "generate_video"
                     "description" "Render a video clip from a prompt and a starting frame."
                     "annotations" (nlk:json-object "readOnlyHint" nil)
                     "inputSchema" (nlk:json-object
                                    "type" "object"
                                    "properties" (nlk:json-object
                                                  "prompt" (nlk:json-object "type" "string")
                                                  "duration_seconds" (nlk:json-object "type" "integer"))
                                    "required" (vector "prompt")))
   (nlk:json-object "name" "inspect_media"
                     "description" "Inspect a finished ad for product and dialogue errors."))
  "The tools the stub server lists: two read-only, one that spends, one with no hint.")

(defun envelope (id result)
  "One JSON-RPC answer carrying RESULT, as the wire spells it."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun text-result (text &key errorp)
  "One tools/call result carrying TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                   "isError" (and errorp t)))

(defun call-result (params)
  "What the stub's tools/call answers: the tool's name and the arguments back."
  (let ((tool (nlk:json-value params :string "name"))
        (arguments (nlk:json-value params :object "arguments")))
    (text-result (format nil "~a answered~@[ ~a~]" tool
                         (and arguments (nlk:encode-json-object arguments))))))

(defun session-headers ()
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) "sess-1")
    headers))

(defun stub-wire (options)
  "NLK:HTTP, answering from the request's own method; nothing leaves the machine."
  (let* ((body (nlk:decode-json (or (getf options :content) "{}")))
         (name (nlk:json-value body :text "method"))
         (params (nlk:json-value body :object "params"))
         (id (nlk:json-value body :integer "id")))
    (push (list name params (getf options :headers)) *calls*)
    (cond ((equal name "initialize")
           (values (envelope id (nlk:json-object "protocolVersion" "2025-06-18"
                                                   "capabilities" (nlk:make-json-object)))
                   200 (session-headers) nil))
          ((equal name "notifications/initialized")
           (values "" 202 nil nil))
          ((equal name "tools/list")
           (values (envelope id (nlk:json-object "tools" *offered*)) 200 nil nil))
          ((equal name "tools/call")
           (if *answer*
               (values (first *answer*) (second *answer*) (third *answer*) nil)
               (values (envelope id (call-result params)) 200 nil nil)))
          (t (values "{}" 404 nil nil)))))

(defmacro with-cospark-wire (&body body)
  "BODY with NLK:HTTP stubbed: every request is logged and answered here."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignorable method url))
                                (stub-wire options))
       ,@body)))

(defmacro with-cospark-running (&body body)
  "BODY with the cell started on a literal section, stopped on unwind."
  `(with-cell-stop ((cospark-start "token" "tok-1"))
     ,@body))

(defun cospark-request (method)
  "The newest stubbed request of JSON-RPC METHOD, or NIL."
  (find method *calls* :test #'equal :key #'first))

(defun cospark-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun cospark-tool-call (&optional (index 0))
  "The INDEX'th newest tools/call the cell sent, as (METHOD PARAMS HEADERS)."
  (nth index (remove-if-not (lambda (call) (equal (first call) "tools/call")) *calls*)))

(defun cospark-sent-tool (&optional (index 0))
  "The MCP tool name the INDEX'th newest tools/call named."
  (let ((call (cospark-tool-call index)))
    (and call (nlk:json-value (second call) :string "name"))))

(defun cospark-sent-arguments (&optional (index 0))
  "The arguments object of the INDEX'th newest tools/call."
  (let ((call (cospark-tool-call index)))
    (and call (nlk:json-value (second call) :object "arguments"))))

(defun cospark-called (tool arguments)
  "TOOL called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "cospark-call" tool arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "cospark"
  (:fixture with-cospark-wire)
  (:config "token" "tok-1")
  (:help :cospark)
  (:command "cospark")
  (:running
   (is (nle::find-tool "cospark_docs") "the documentation tool is registered")
   (is (nle::find-tool "cospark_tools") "the listing tool is registered")
   (is (nle::find-tool "cospark_call") "and the one that runs a tool")
   (is (= 3 (count-if (lambda (entry) (eql 0 (search "cospark_" (first entry))))
                      nle::*tools*)) "three of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "cospark_docs")) "and stop takes every one back")
   (is (null (nle::find-tool "cospark_call"))))
  (:refused ("token" "tok-1" "timeout_seconds" 0)
            ("token" "tok-1" "max_chars" 10))
  (:idle nodecode-cospark:cospark-error (nodecode-cospark:list-tools nil)))

(deftest cospark-cell-a-start-with-no-sign-in-refuses (with-cospark-wire)
  (with-stubbed-fdefinitions ((nodecode-cospark::mcp-token (url) (declare (ignore url)) nil)
                              (nodecode-cospark::environment-token (name) (declare (ignore name)) nil))
    (let ((text (refusal-text nlk:config-refusal (cospark-start))))
      (is-carrying text "/mcp" ("browser login" "the operator is told where the sign-in comes from"))
      (is (null (nle::find-tool "cospark_docs")) "nothing was installed"))))

(deftest cospark-cell-the-mcp-cells-sign-in-is-reused (with-cospark-wire)
  ;; The shipped mcp cell keeps its OAuth sign-in under the home; this cell
  ;; reads that token rather than asking for one of its own.
  (let ((path (nlk:home "mcp/oauth.json")))
    (unwind-protect
         (progn
           (ensure-directories-exist path)
           (with-open-file (out path :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
             (write-string (nlk:encode-json-object
                            (nlk:json-object "https://api.cospark.so/mcp"
                                             (nlk:json-object "tokens"
                                                              (nlk:json-object "access_token" "tok-stored"))))
                           out))
           (with-cell-stop ((cospark-start))
             (cospark-called "cospark_tools" "{}")
             (is (equal "Bearer tok-stored"
                        (cospark-request-header (cospark-request "tools/list") "authorization")) "the token the mcp cell kept is the one sent")))
      (when (probe-file path) (ignore-errors (delete-file path))))))

;;; --- the wire ---------------------------------------------------------------------

(deftest cospark-cell-one-session-serves-every-call (with-cospark-wire)
  (with-cospark-running
    (cospark-called "cospark_tools" "{}")
    (cospark-called "cospark_call" "{\"name\": \"search_ad_references\"}")
    (is (= 1 (count-if (lambda (call) (equal (first call) "initialize")) *calls*)) "initialize happens once and is reused")
    (is (equal "Bearer tok-1" (cospark-request-header (cospark-tool-call) "authorization")) "with this cell's own token")
    (is (equal "sess-1" (cospark-request-header (cospark-tool-call) "mcp-session-id")) "and the session id the server handed is echoed")))

(deftest cospark-cell-an-error-the-server-reports-fails-the-call (with-cospark-wire)
  (with-cospark-running
    (setf *answer* (list (envelope 1 (text-result "the starting frame is missing" :errorp t))
                         200 nil))
    (let ((answer (cospark-called "cospark_call"
                                   "{\"name\": \"generate_video\", \"arguments\": {\"prompt\": \"hook\"}, \"confirmed\": true}")))
      (is (search "ERROR:" answer) "the call is failed, not answered")
      (is (search "the starting frame is missing" answer) "and the server's words reach the model"))))

;;; --- the tools say what the skill says ---------------------------------------------

(deftest cospark-cell-the-list-marks-what-needs-a-yes (with-cospark-wire)
  (with-cospark-running
    (let ((answer (cospark-called "cospark_tools" "{}")))
      (is (search "read_docs" answer) "every tool the server offers is named")
      (is (search "arguments: duration_seconds, prompt (required: prompt)" answer) "with the arguments the tool's own schema names")
      (is (search "search_ad_references [read-only]" answer) "a read-only tool says so")
      (is (search "generate_video [needs confirmed true]" answer) "a spending tool says it needs the person's yes")
      (is (search "inspect_media [needs confirmed true]" answer) "and a tool with no readOnlyHint is treated as one that spends"))))

(deftest cospark-cell-the-list-takes-a-filter (with-cospark-wire)
  (with-cospark-running
    (let ((answer (cospark-called "cospark_tools" "{\"filter\": \"reference\"}")))
      (is (search "search_ad_references" answer) "the matching tool is listed")
      (is (not (search "generate_video" answer)) "and the others are not"))))

;;; --- cospark_docs -----------------------------------------------------------------

(deftest cospark-cell-docs-reads-the-index-with-no-path (with-cospark-wire)
  (with-cospark-running
    (let ((answer (cospark-called "cospark_docs" "{}")))
      (is (equal "read_docs" (cospark-sent-tool)) "it is the server's own read_docs")
      (is (= 0 (hash-table-count (cospark-sent-arguments))) "with an empty arguments object")
      (is (search "read_docs answered" answer) "and the guide the server answers reaches the model"))))

(deftest cospark-cell-docs-reads-one-guide-by-path (with-cospark-wire)
  (with-cospark-running
    (cospark-called "cospark_docs" "{\"path\": \"generation/video\"}")
    (is (equal "generation/video" (nlk:json-value (cospark-sent-arguments) :text "path")) "the path the model named is the one asked for")))

(deftest cospark-cell-docs-never-asks-for-a-yes (with-cospark-wire)
  ;; Reading documentation spends nothing, so not even the confirmation gate
  ;; stands in its way.
  (with-cospark-running
    (cospark-called "cospark_docs" "{}")
    (is (equal "read_docs" (cospark-sent-tool)) "the call went through with no confirmed flag")))

;;; --- cospark_call -------------------------------------------------------------------

(deftest cospark-cell-a-read-only-tool-runs-without-confirmation (with-cospark-wire)
  (with-cospark-running
    (let ((answer (cospark-called "cospark_call" "{\"name\": \"search_ad_references\"}")))
      (is (equal "search_ad_references" (cospark-sent-tool)) "the server's own tool is called")
      (is (search "search_ad_references answered" answer) "and its answer reaches the model"))))

(deftest cospark-cell-a-spending-tool-needs-the-persons-yes (with-cospark-wire)
  (with-cospark-running
    (let ((answer (cospark-called "cospark_call"
                                   "{\"name\": \"generate_video\", \"arguments\": {\"prompt\": \"hook\"}}")))
      (is (search "confirmed true" answer) "the refusal says what the yes is")
      (is (search "credits" answer) "and what it spends")
      (is (null (cospark-tool-call)) "and nothing was rendered"))))

(deftest cospark-cell-a-confirmed-call-runs-with-its-arguments (with-cospark-wire)
  (with-cospark-running
    (cospark-called "cospark_call"
                    "{\"name\": \"generate_video\", \"arguments\": {\"prompt\": \"hook\"}, \"confirmed\": true}")
    (is (equal "generate_video" (cospark-sent-tool)))
    (is (equal "hook" (nlk:json-value (cospark-sent-arguments) :text "prompt")) "the tool's own arguments object goes through as the server names it")))

(deftest cospark-cell-an-unknown-name-lists-what-there-is (with-cospark-wire)
  (with-cospark-running
    (cospark-called "cospark_tools" "{}")
    (let ((answer (cospark-called "cospark_call" "{\"name\": \"render_everything\"}")))
      (is (search "cospark_tools" answer) "the model is told where the names are")
      (is (null (cospark-tool-call)) "and nothing was sent as a call")
      (is (= 2 (count-if (lambda (call) (equal (first call) "tools/list")) *calls*)) "the list is read again before the name is refused"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest cospark-cell-the-command-lists-the-tools (with-cospark-wire)
  (with-cospark-running
    (let ((answer (nodecode-cospark::run-command "" nil)))
      (is (search "search_ad_references" answer) "no argument lists what the server offers")
      (is (search "The sign-in in use comes from" answer) "and says where the sign-in came from"))))

(deftest cospark-cell-the-command-says-where-the-sign-in-came-from (with-cospark-wire)
  (with-cospark-running
    (let ((answer (nodecode-cospark::run-command "login" nil)))
      (is (search "token member" answer) "the operator is told, in words")
      (is (not (search "tok-1" answer)) "and never the token itself")
      (is (null *calls*) "with no request to the server"))))
