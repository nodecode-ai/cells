;;;; cell-test.lisp --- the dsers cell against the core's own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange goes through the cell's one HTTP name, and every test
;;;; stubs it: nothing here reaches the DSers server or the network.

(in-package #:nodecode.test)

(define-test-slice "dsers" "DSERS-CELL-" :start nodecode-dsers:start-cell)

(define-cell-lifecycle-tests "dsers"
  (:help :dsers)
  (:command "dsers")
  (:config "endpoint" "https://dsers.invalid/mcp" "token" "inert")
  (:running (is (search "supplier platforms" (nle:help :dsers)) "the manual topic carries what DSers works with"))
  (:refused ("timeout_seconds" 0))
  (:idle nodecode-dsers::dsers-error (nodecode-dsers:dsers-tools nil)))

;;; --- a stubbed server --------------------------------------------------------

(defvar *asked* nil
  "Every stubbed exchange of the running test, newest first.")

(defun dsers-json (object)
  "OBJECT as a JSON object: a plist of (KEY VALUE...) pairs, or as it is."
  (if (hash-table-p object)
      object
      (let ((table (make-hash-table :test #'equal)))
        (loop for (key value) on object by #'cddr
              do (setf (gethash key table) value))
        table)))

(defun dsers-tool (name description &optional properties)
  "One entry of a tools/list answer."
  (dsers-json (list "name" name "description" description "inputSchema" (dsers-json
                                    (list "type" "object" "properties" (or properties (make-hash-table :test #'equal)) "required" (vector "store_id"))))))

(defun dsers-answer (object &key (session "session-1"))
  "A server-sent answer, the way a streamable MCP server sends one."
  (let ((answer (make-hash-table :test #'equal)))
    (setf (gethash "jsonrpc" answer) "2.0"
          (gethash "id" answer) 1
          (gethash "result" answer) (dsers-json object))
    (values (format nil "event: message~%data: ~a~%~%" (nlk:encode-json-object answer))
            200
            (list (cons "Content-Type" "text/event-stream")
                  (cons "Mcp-Session-Id" session)))))

(defmacro with-dsers-wire (&body forms)
  "BODY with the cell's HTTP name answering ROUTES, (FRAGMENT FORM): FORM runs
with the request read back in *ASKED*, and answers (values TEXT STATUS HEADERS)."
  (let ((routes (remove-if-not (lambda (form) (stringp (first form))) forms))
        (body (remove-if (lambda (form) (stringp (first form))) forms)))
    `(let ((*asked* nil))
       (with-stubbed-fdefinition (nodecode-dsers::http (method url &rest arguments)
                                  (declare (ignore url))
                                  (let* ((body (getf arguments :content))
                                         (payload (ignore-errors (nlk:decode-json body))))
                                    (push (list :method method :payload payload
                                                :headers (getf arguments :headers) :body body)
                                          *asked*)
                                    (cond ,@(loop for (fragment form) in routes
                                                  collect `((search ,fragment body) ,form))
                                          (t (values "" 500 nil)))))
         ,@body))))

(defmacro with-dsers-cell ((&rest section) &body body)
  "BODY with the cell started on SECTION: an endpoint and a token, or neither."
  `(with-cell-stop ((dsers-start ,@section))
     ,@body))

(defun dsers-header (headers name)
  (cdr (assoc name headers :test #'string-equal)))

;;; --- the verbs --------------------------------------------------------------

(deftest dsers-cell-lists-the-accounts-tools ()
  (with-dsers-cell ("endpoint" "https://dsers.invalid/mcp" "token" "inert")
    (with-dsers-wire
      ("\"initialize\"" (dsers-answer (list "protocolVersion" "2025-06-18")))
      ("\"tools/list\"" (dsers-answer
                         (list "tools" (vector (dsers-tool "list_stores" "List the connected DSers stores.")
                                                (dsers-tool "place_orders_to_suppliers"
                                                             "Place supplier orders from a preview."
                                                             (dsers-json (list "store_id" (dsers-json (list "type" "string")))))))))
      ("\"notifications/initialized\"" (values "" 202 nil))
      (let ((text (nodecode-dsers:dsers-tools nil)))
        (is (search "list_stores" text) "a tool's name is listed")
        (is (search "List the connected DSers stores" text) "with the first line of what it does")
        (is (search "store_id (required)" text) "and its required arguments")
        (is (search "2 DSers tools" text) "the count is the server's, not a fixed list")
        (let* ((first (car (last *asked*)))
               (headers (getf first :headers)))
          (is (equal "application/json" (dsers-header headers "Content-Type")) "the call is JSON-RPC")
          (is (equal "Bearer inert" (dsers-header headers "Authorization")) "the token from the section authorizes it"))))))

(deftest dsers-cell-calls-a-tool-with-its-arguments ()
  (with-dsers-cell ("endpoint" "https://dsers.invalid/mcp" "token" "inert")
    (with-dsers-wire
      ("\"initialize\"" (dsers-answer (list "protocolVersion" "2025-06-18")))
      ("\"tools/call\"" (dsers-answer
                         (list "content" (vector (dsers-json (list "type" "text" "text" "2 stores: A, B"))))))
      ("\"notifications/initialized\"" (values "" 202 nil))
      (let* ((answer (nodecode-dsers:dsers-call
                      (dsers-json (list "tool" "list_stores" "arguments" (dsers-json (list "store_id" "none"))))))
             (call (find "\"tools/call\"" *asked* :key (lambda (entry) (getf entry :body))
                         :test #'search))
             (params (and call (gethash "params" (getf call :payload)))))
        (is (search "2 stores: A, B" answer) "the tool's own text comes back")
        (is (equal "list_stores" (and params (gethash "name" params))) "the named tool is the one the server calls")
        (is (equal "none" (and params (gethash "store_id" (gethash "arguments" params)))) "and the arguments are the model's own")))))

(deftest dsers-cell-reports-a-tools-own-error ()
  (with-dsers-cell ("endpoint" "https://dsers.invalid/mcp" "token" "inert")
    (with-dsers-wire
      ("\"initialize\"" (dsers-answer (list "protocolVersion" "2025-06-18")))
      ("\"tools/call\"" (dsers-answer
                         (list "isError" t "content" (vector (dsers-json
                                                  (list "type" "text" "text" "PLAN_RESTRICTION: the plan does not include MCP"))))))
      ("\"notifications/initialized\"" (values "" 202 nil))
      (let ((answer (nodecode-dsers:dsers-call (dsers-json (list "tool" "list_stores")))))
        (is (search "PLAN_RESTRICTION" answer) "the server's own error text reaches the model")
        (is (search "answered an error" answer) "and says it was one")))))

(deftest dsers-cell-without-a-token-refuses ()
  (with-dsers-cell ("endpoint" "https://dsers.invalid/mcp")
    (handler-case (progn (nodecode-dsers:dsers-tools nil)
                         (is nil "a call without a token refuses"))
      (nodecode-dsers::dsers-error (condition)
        (is (search "token" (nodecode-dsers::dsers-error-detail condition)) "the refusal names the setting to fill in")))))

(deftest dsers-cell-command-answers-without-a-round-trip ()
  (with-dsers-cell ("endpoint" "https://dsers.invalid/mcp" "token" "inert")
    (with-stubbed-fdefinition (nodecode-dsers::http (method url &rest arguments)
                               (declare (ignore method url arguments))
                               (error "the command dialled when it should not have"))
      (let ((text (nodecode-dsers:dsers-command "" nil)))
        (is (search "https://dsers.invalid/mcp" text) "it names the endpoint in use")
        (is (search "token: set" text) "and whether a token is there")
        (is (search "(help :dsers)" text) "and where the workflow is written down")))))
