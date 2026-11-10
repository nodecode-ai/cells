;;;; support.lisp --- the cell's test runner and a scripted MakeIRL server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every test here runs offline: NLK:HTTP is stubbed by WITH-STUBBED-MAKEIRL,
;;;; which answers the MCP handshake and then hands each request to *SERVER*, a
;;;; function of the decoded JSON-RPC message answering (values TEXT STATUS
;;;; HEADERS). The token is a real file the section names, so the reader, the
;;;; header and the refusal are exercised, not bypassed.

(in-package #:nodecode.test)

(define-test-slice "makeirl" "MAKEIRL-CELL-" :start nodecode-makeirl:start-cell)

(defvar *token-file* nil
  "The token file some tests' section names; TOKEN-FILE writes it.")

(defvar *requests* nil
  "Every exchange the stub saw, newest first: (METHOD HEADERS BODY).")

(defvar *server* nil
  "The scripted server: a function of one decoded JSON-RPC message answering
(values TEXT STATUS HEADERS).")

(defun token-file ()
  "A token file holding a throwaway token."
  (unless *token-file*
    (let ((path (merge-pathnames "makeirl-token" (uiop:temporary-directory))))
      (with-open-file (out path :direction :output :if-exists :supersede
                                      :if-does-not-exist :create)
        (write-string "throwaway-token" out))
      (setf *token-file* (namestring path))))
  *token-file*)

(defun json-answer (object &key (status 200) (session nil) (type "application/json"))
  (values (nlk:encode-json-object object) status
          (append (list (cons "content-type" type))
                  (and session (list (cons "mcp-session-id" session))))))

(defun sse-answer (object &key (session "session-1"))
  "OBJECT as one server-sent frame, the way a streamable-HTTP server may answer."
  (values (format nil "event: message~%data: ~a~%~%" (nlk:encode-json-object object))
          200
          (append (list (cons "content-type" "text/event-stream"))
                  (and session (list (cons "mcp-session-id" session))))))

(defun tool-json (name description &optional properties)
  "One MakeIRL tool as tools/list carries it: PROPERTIES a plist of name and
type, all required."
  (let ((names (loop for (key) on properties by #'cddr collect key)))
    (nlk:json-object "name" name
                     "description" description
                     "inputSchema" (nlk:json-object
                                    "type" "object"
                                    "properties" (apply #'nlk:make-json-object
                                                        (loop for (key type) on properties by #'cddr
                                                              append (list key (nlk:json-object "type" type))))
                                    "required" (coerce names 'vector))
                     "annotations" (nlk:json-object
                                    "readOnlyHint" (member name '("release_status" "release_list_findings")
                                                           :test #'equal)))))

(defun scripted-makeirl (message)
  "A small MakeIRL: its two read tools, an upload, and a status that answers
the run state it was asked about."
  (let* ((method (gethash "method" message))
         (params (gethash "params" message))
         (id (gethash "id" message)))
    (cond
      ((equal method "initialize")
       (json-answer (nlk:json-object "jsonrpc" "2.0" "id" id
                                      "result" (nlk:json-object
                                                "protocolVersion" "2025-06-18"
                                                "serverInfo" (nlk:json-object "name" "makeirl")))
                    :session "session-1"))
      ((equal method "notifications/initialized") (values "" 202 nil))
      ((equal method "tools/list")
       (json-answer (nlk:json-object
                      "jsonrpc" "2.0" "id" id
                      "result" (nlk:json-object
                                "tools" (vector (tool-json "release_create_upload"
                                                            "Upload a release's project files"
                                                            '("product" "string" "files" "array"))
                                                (tool-json "release_status"
                                                            "The state of one run"
                                                            '("runId" "string"))
                                                (tool-json "release_list_findings"
                                                            "The findings of one run"
                                                            '("runId" "string")))))))
      ((equal method "tools/call")
       (let ((name (gethash "name" params))
             (arguments (gethash "arguments" params)))
         (cond
           ((equal name "release_status")
            (json-answer (nlk:json-object
                          "jsonrpc" "2.0" "id" id
                          "result" (nlk:json-object
                                    "content" (vector (nlk:json-object
                                                       "type" "text"
                                                       "text" (format nil "run ~a: REVIEW"
                                                                      (gethash "runId" arguments))))))))
           ((equal name "release_list_findings")
            (sse-answer (nlk:json-object
                          "jsonrpc" "2.0" "id" id
                          "result" (nlk:json-object
                                    "content" (vector (nlk:json-object
                                                       "type" "text"
                                                       "text" (format nil "BLOCKED: ~d unconnected pads"
                                                                      (length (gethash "files" arguments)))))))))
           (t (json-answer (nlk:json-object
                             "jsonrpc" "2.0" "id" id
                             "result" (nlk:json-object
                                       "content" (vector (nlk:json-object
                                                          "type" "text"
                                                          "text" (format nil "~a ok: ~a" name
                                                                         (nlk:encode-json-object arguments)))))))))))
      (t (json-answer (nlk:json-object "jsonrpc" "2.0" "id" id "result" (nlk:json-object)))))))

(defun stub-exchange (options)
  "One stubbed exchange: record it, then let *SERVER* answer it."
  (let* ((body (getf options :content))
         (message (and (stringp body) (nlk:decode-json body))))
    (push (list (and (hash-table-p message) (gethash "method" message))
                (getf options :headers)
                body)
          *requests*)
    (if (hash-table-p message)
        (funcall *server* message)
        (values "{}" 500 nil))))

(defmacro with-stubbed-makeirl (&body body)
  "BODY over a throwaway token file and a stubbed NLK:HTTP answering *SERVER*."
  `(with-saved-globals (*requests* *server* *token-file*
                         nodecode-makeirl::*session* nodecode-makeirl::*request-id*
                         nle:*tools*)
     (let ((*requests* '())
           (*server* #'scripted-makeirl))
       (token-file)
       (setf nodecode-makeirl::*session* nil)
       (with-stubbed-fdefinitions
           ((nlk:http (&rest options)
              (stub-exchange options)))
         ,@body))))

(defun call-tool (name &rest pairs)
  "NAME's registered body, called as the dispatcher calls it: with the record
a contract materializes, or the table a plist schema leaves."
  (let* ((entry (nle::find-tool name))
         (schema (second entry)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry)
             (if (typep schema 'nlk:contract)
                 (nlk:contract-decode schema (apply #'nlk:make-json-object pairs))
                 (apply #'nlk:make-json-object pairs)))))

(defun registered-tools ()
  (loop for (name . nil) in nle:*tools*
        when (uiop:string-prefix-p "makeirl_" name) collect name))

(defun request-header (headers name)
  (let ((pair (find name headers
                    :key (lambda (entry) (if (consp entry) (car entry) entry))
                    :test #'string-equal)))
    (and (consp pair) (cdr pair))))
