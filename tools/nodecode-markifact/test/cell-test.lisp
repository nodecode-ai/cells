;;;; cell-test.lisp --- the markifact cell against its own seams.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every exchange is a stubbed NLK:HTTP answering JSON-RPC by method: no
;;;; socket opens, no sign-in runs, and the shared auth.json is a temp file.

(in-package #:nodecode.test)

(define-test-slice "markifact" "MARKIFACT-CELL-" :start nodecode-markifact:start-cell)

(defparameter +endpoint+ "https://api.markifact.com/mcp")

(defvar *wire* '()
  "Every request a stubbed NLK:HTTP answered this test, newest first:
 a plist of :method, :url, :headers, :content.")

(defvar *answer* nil
  "A form the stub runs instead of the default answer, with TOOL, ARGUMENTS and
 ID bound, when a test wants one tool to refuse.")

(defun wire-request (content)
  (and content (plusp (length content)) (ignore-errors (nlk:decode-json content))))

(defun answer-wire (method url headers content)
  "(values TEXT STATUS HEADERS TRUNCATED) for one request the cell made."
  (declare (ignore url headers))
  (let* ((request (wire-request content))
         (name (and request (gethash "method" request)))
         (id (and request (gethash "id" request))))
    (cond ((null name) (values "" 202 nil nil))
          ((equal name "initialize")
           (values (nlk:encode-json-object
                    (nlk:json-object "jsonrpc" "2.0" "id" id
                                     "result" (nlk:json-object "protocolVersion" "2025-06-18")))
                   200 (list (cons "Mcp-Session-Id" "sess-1")) nil))
          ((equal name "notifications/initialized") (values "" 202 nil nil))
          ((equal name "tools/list")
           (values (nlk:encode-json-object
                    (nlk:json-object
                     "jsonrpc" "2.0" "id" id
                     "result"
                     (nlk:json-object
                      "tools" (vector (nlk:json-object "name" "find_operations"
                                                        "description" "Find operations by intent"
                                                        "inputSchema" (nlk:json-object "type" "object"))
                                       (nlk:json-object "name" "run_write_operation"
                                                        "description" "Run an operation that changes an account"
                                                        "inputSchema" (nlk:json-object "type" "object"))))))
                   200 nil nil))
          ((equal name "tools/call")
           (let* ((params (gethash "params" request))
                  (tool (gethash "name" params))
                  (arguments (gethash "arguments" params)))
             (if *answer*
                 (values (nlk:encode-json-object
                          (nlk:json-object "jsonrpc" "2.0" "id" id "result" *answer*))
                         200 nil nil)
                 (values (nlk:encode-json-object
                          (nlk:json-object
                           "jsonrpc" "2.0" "id" id
                           "result"
                           (nlk:json-object
                            "content" (vector (nlk:json-object
                                               "type" "text"
                                               "text" (format nil "~a answered ~a" tool
                                                               (or (and arguments (nlk:encode-json-object arguments)) "{}"))))
                            "isError" (and (equal tool "run_write_operation") t))))
                         200 nil nil))))
          (t (values "{}" 200 nil nil)))))

(defmacro with-wire ((&key (token "tok-1") (auth "{}")) &body body)
  "BODY with the cell started against ENDPOINT, a temp auth.json holding AUTH,
 every exchange stubbed, and the section's access_token TOKEN."
  `(let ((*wire* '()) (*answer* nil))
     (with-temp-auth (file ,auth)
       (let ((nle::*auth-file-path* file))
         (with-stubbed-fdefinition (nle::credential-env (name) (declare (ignore name)) nil)
           (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                      (let ((headers (getf args :headers))
                                            (content (getf args :content)))
                                        (push (list :method method :url url :headers headers
                                                    :content content)
                                              *wire*)
                                        (answer-wire method url headers content)))
             (with-cell-stop ((markifact-start "endpoint" "https://api.markifact.com/mcp"
                                            "access_token" ,token))
               ,@body)))))))

(defun sent (method-name)
  "The newest request whose JSON-RPC method is METHOD-NAME."
  (find method-name *wire*
        :key (lambda (request) (let ((body (wire-request (getf request :content))))
                                 (and body (gethash "method" body))))
        :test #'equal))

(defun sent-header (request name)
  (cdr (assoc name (getf request :headers) :test #'string-equal)))

(defun call-op (&rest pairs)
  "A MARKIFACT-CALL contract from PAIRS."
  (nodecode-markifact:decode-markifact-call (apply #'nlk:make-json-object pairs)))

;;; --- the cell's own life ---------------------------------------------------------

(define-cell-lifecycle-tests "markifact"
  (:config "endpoint" "https://api.markifact.com/mcp")
  (:help :markifact)
  (:command "markifact")
  (:running
   (is (equal 60 (nodecode-markifact::setting :timeout)) "the default bound")
   (is (search "markifact_call" nodecode-markifact::+primer+) "the primer names the tool"))
  (:refused ("endpoint" 5) ("timeout" 1))
  (:idle nodecode-markifact:markifact-error
         (markifact:call-tool "find_operations" (nlk:make-json-object))))

(deftest markifact-cell-section-needs-its-endpoint ()
  (is (signals-error nodecode-markifact:markifact-error (markifact-start "access_token" "t"))
      "a section without endpoint refuses at start, naming the value to write")
  (is (search "set markifact.endpoint to https://api.markifact.com/mcp"
              (refusal-text nodecode-markifact:markifact-error (markifact-start "access_token" "t")))))

;;; --- the tools -------------------------------------------------------------------

(deftest markifact-cell-calls-a-tool-over-the-wire ()
  (with-wire ()
    (let ((answer (nodecode-markifact::tool-call
                   (call-op "tool" "find_operations"
                            "arguments" (nlk:json-object "query" "list campaigns")))))
      (is (search "find_operations answered" answer) "the server's text, passed through")
      (is (search "list campaigns" answer) "with the arguments it was given")
      (let ((initialize (sent "initialize"))
            (call (sent "tools/call")))
        (is (equal "Bearer tok-1" (sent-header initialize "Authorization"))
            "the section's token rides as a bearer")
        (is (equal "https://api.markifact.com/mcp" (getf initialize :url))
            "one endpoint, the section's")
        (is (null (sent-header initialize "Mcp-Session-Id")) "no session before initialize")
        (is (equal "sess-1" (sent-header call "Mcp-Session-Id"))
            "the session id initialize answered is sent back")
        (let ((body (wire-request (getf call :content))))
          (is (equal "2.0" (gethash "jsonrpc" body)))
          (is (equal "find_operations" (gethash "name" (gethash "params" body)))))))
    (is (= 3 (length *wire*)) "initialize, its notification, and the call: nothing else")))

(deftest markifact-cell-tools-lists-what-the-server-exposes ()
  (with-wire ()
    (let ((answer (nodecode-markifact::tool-tools (nodecode-markifact:decode-markifact-tools-query
                                                   (nlk:make-json-object)))))
      (is (search "find_operations" answer))
      (is (search "run_write_operation" answer))
      (is (search "Find operations by intent" answer) "its purpose")
      (is (search "arguments:" answer) "and the schema its arguments follow")
      (is (search "object" answer) "the schema's own words"))))

(deftest markifact-cell-says-when-the-server-refuses ()
  (with-wire ()
    (setf *answer* (nlk:json-object "content" (vector (nlk:json-object "type" "text"
                                                                     "text" "budget is required"))
                                     "isError" t))
    (is (search "budget is required"
                (refusal-text markifact:markifact-error
                              (markifact:call-tool "run_write_operation" (nlk:make-json-object)))))))

(deftest markifact-cell-refuses-with-no-credential ()
  (with-wire (:token "")
    (is (search "not signed in to Markifact"
                (refusal-text markifact:markifact-error
                              (markifact:call-tool "list_connections" (nlk:make-json-object)))))
    (is (search "/markifact login"
                (refusal-text markifact:markifact-error
                              (markifact:call-tool "list_connections" (nlk:make-json-object)))))
    (is (null *wire*) "and nothing was sent")))

;;; --- the command -----------------------------------------------------------------

(deftest markifact-cell-command-answers-status-and-a-workflow ()
  (with-wire ()
    (is (search "Signed in" (cell-entry "nodecode-markifact" "markifact" ""))
        "bare /markifact says where the sign-in stands")
    (is (search "negative-keyword-sweep" (cell-entry "nodecode-markifact" "markifact" "workflow negative-keyword-sweep"))
        "a workflow prints its steps")
    (is (search "1. *_list_report_fields"
                (cell-entry "nodecode-markifact" "markifact" "workflow negative-keyword-sweep")))
    (is (search "find_operations answered"
                (cell-entry "nodecode-markifact" "markifact"
                            "call find_operations {\"query\": \"list campaigns\"}"))
        "/markifact call reaches the server")
    (is (search "is not one of" (cell-entry "nodecode-markifact" "markifact" "nonsense")))))

(deftest markifact-cell-logs-out ()
  (with-wire ()
    (is (search "Signed out" (cell-entry "nodecode-markifact" "markifact" "logout")))
    (is (search "the markifact section carries an access token"
                (cell-entry "nodecode-markifact" "markifact" "status"))
        "and the status line names the token the section still carries")))
