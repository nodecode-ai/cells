;;;; support.lisp --- the cell's test runner and a scripted ChorePilot house.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every test here runs offline: NLK:HTTP is stubbed by WITH-STUBBED-HOUSE,
;;;; which answers the MCP handshake and then hands each request to *HOUSE*, a
;;;; function of the decoded JSON-RPC message answering (values TEXT STATUS
;;;; HEADERS). The token is a real file the section names, so the one-of and
;;;; the file reader are exercised, not bypassed.

(in-package #:nodecode.test)

(define-test-slice "chorepilot" "CHOREPILOT-CELL-" :start nodecode-chorepilot:start-cell)

(defvar *token-file* nil
  "The token file every test's section names; WITH-STUBBED-HOUSE writes it.")

(defvar *requests* nil
  "Every exchange the stub saw, newest first: (METHOD BODY).")

(defvar *house* nil
  "The scripted server: a function of one decoded JSON-RPC message answering
(values TEXT STATUS HEADERS).")

(defun token-file ()
  "A token file holding a throwaway token."
  (unless *token-file*
    (let ((path (merge-pathnames "chorepilot-token" (uiop:temporary-directory))))
      (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
        (write-string "throwaway-token" out))
      (setf *token-file* (namestring path))))
  *token-file*)

(defun json-answer (object &key (status 200) (session nil))
  (values (nlk:encode-json-object object) status
          (and session (list (cons "mcp-session-id" session)))))

(defun tool-json (name description &optional properties)
  "One house tool as tools/list carries it: PROPERTIES a plist of name and type."
  (nlk:json-object "name" name
                   "description" description
                   "inputSchema" (nlk:make-json-object
                                  "type" "object"
                                  "properties" (apply #'nlk:make-json-object
                                                      (loop for (key type) on properties by #'cddr
                                                            append (list key (nlk:json-object "type" type))))
                                  "required" #())
                   "annotations" (nlk:json-object "readOnlyHint" (equal name "whoami"))))

(defun scripted-house (message)
  "A small house: two device tools, a chore tool, whoami, and a water
action that comes back confirmed."
  (let* ((method (gethash "method" message))
         (params (gethash "params" message)))
    (cond
      ((equal method "initialize")
       (json-answer (nlk:json-object "jsonrpc" "2.0" "id" 0
                                      "result" (nlk:json-object
                                                "protocolVersion" "2025-06-18"
                                                "instructions" "the house's own words"))
                    :session "session-1"))
      ((equal method "notifications/initialized") (values "" 202 nil))
      ((equal method "tools/list")
       (json-answer (nlk:json-object
                      "jsonrpc" "2.0" "id" 1
                      "result" (nlk:json-object
                                "tools" (vector (tool-json "whoami" "Which person this connection is")
                                                (tool-json "set_light" "Set a light" '("device" "string" "action" "string"))
                                                (tool-json "start_dishwasher" "Start the dishwasher" '("program" "string")))))))
      ((equal method "tools/call")
       (let ((name (gethash "name" params))
             (arguments (gethash "arguments" params)))
         (json-answer (nlk:json-object
                        "jsonrpc" "2.0" "id" 1
                        "result" (nlk:json-object
                                  "structuredContent"
                                  (if (equal name "start_dishwasher")
                                      (nlk:json-object "ok" t "tool" name
                                                       "confirmation" (nlk:json-object "via" "telegram"))
                                      (nlk:json-object "ok" t "tool" name "arguments" arguments)))))))
      (t (json-answer (nlk:json-object "jsonrpc" "2.0" "id" 1 "result" (nlk:json-object)))))))

(defun stub-exchange (options)
  "One stubbed exchange: record it, then let *HOUSE* answer it."
  (let* ((body (getf options :content))
         (message (and (stringp body) (nlk:decode-json body))))
    (push (list (and (hash-table-p message) (gethash "method" message)) body) *requests*)
    (if (hash-table-p message)
        (funcall *house* message)
        (values "{}" 500 nil))))

(defmacro with-stubbed-house (&body body)
  "BODY over a throwaway token file and a stubbed NLK:HTTP answering *HOUSE*."
  `(with-saved-globals (*requests* *house* *token-file*
                         nodecode-chorepilot::*session* nle:*tools*)
     (let ((*requests* '())
           (*house* #'scripted-house))
       (token-file)
       (setf nodecode-chorepilot::*session* nil)
       (with-stubbed-fdefinitions
           ((nlk:http (&rest options)
              (stub-exchange options)))
         ,@body))))

(defun call-tool (name &rest pairs)
  "NAME's registered body, called as the dispatcher calls it: one decoded
arguments object."
  (let ((entry (nle::find-tool name)))
    (assert entry () "~a is not registered" name)
    (funcall (third entry) (apply #'nlk:make-json-object pairs))))

(defun registered-tools ()
  (loop for (name . nil) in nle:*tools*
        when (uiop:string-prefix-p "chorepilot_" name) collect name))

(defun house-description (name)
  "The description the provider reads for tool NAME."
  (let ((entry (nle::find-tool name)))
    (and entry (getf (second entry) :description))))
