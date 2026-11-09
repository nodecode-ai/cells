;;;; support.lisp --- mcp-creator test runner and shared fixtures.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; These tests register into the same nodecode.test registry as every other
;;;; cell's, under an MCP-CREATOR-CELL- name prefix, so this system's test-op
;;;; runs exactly that slice. Nothing here opens a socket or starts a process:
;;;; the cell's one connection seam, NODECODE-MCP-CREATOR::*CONNECT*, is bound
;;;; to a scripted server, and every write lands in a temporary directory.

(in-package #:nodecode.test)

(define-test-slice "mcp-creator" "MCP-CREATOR-CELL-" :start nodecode-mcp-creator:start-cell)

;;; --- a scripted server -------------------------------------------------------

(defun mc-tool (name &key (description "does a thing") (params '("path")) required)
  "One tools/list entry."
  (let ((properties (nlk:make-json-object)))
    (dolist (param params) (setf (gethash param properties) (nlk:json-object "type" "string")))
    (nlk:json-object "name" name "description" description
                     "inputSchema" (nlk:json-object "type" "object"
                                                     "properties" properties
                                                     "required" (coerce (or required params) 'vector)))))

(defun mc-conn (tools &key (instructions "be careful"))
  "A connection whose ASK answers a catalog of TOOLS."
  (let ((conn (nodecode-mcp-creator::make-conn)))
    (setf (nodecode-mcp-creator::conn-kind conn) :fake
          (nodecode-mcp-creator::conn-protocol conn) "2025-06-18"
          (nodecode-mcp-creator::conn-server-info conn) (nlk:json-object "name" "files" "version" "1.2.0")
          (nodecode-mcp-creator::conn-close conn) (lambda () nil)
          (nodecode-mcp-creator::conn-ask conn)
          (lambda (method &optional params)
            (declare (ignore params))
            (cond ((equal method "initialize")
                   (nlk:json-object "protocolVersion" "2025-06-18" "instructions" instructions
                                    "serverInfo" (nlk:json-object "name" "files" "version" "1.2.0")))
                  ((equal method "tools/list")
                   (nlk:json-object "tools" (coerce tools 'vector)))
                  (t nil))))
    conn))

(defun mc-connect (tools)
  (lambda (server) (declare (ignore server)) (mc-conn tools)))

;;; --- a config ----------------------------------------------------------------

(defun mc-entry (&rest pairs) (apply #'nlk:make-json-object pairs))

(defun mc-table (&rest pairs)
  "A JSON object of PAIRS, built member by member."
  (let ((table (nlk:make-json-object)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key table) value))
    table))

(defun mc-config (&rest section-pairs)
  "A whole config: the mcp section with two servers, and an mcp-creator
section made of SECTION-PAIRS (string key, value)."
  (mc-table
   "mcp" (mc-table "servers" (mc-table "files" (mc-entry "command" "npx"
                                                          "args" (vector "-y" "@modelcontextprotocol/server-filesystem"
                                                                         "/home/someone/docs")
                                                          "env" (mc-table "FILES_TOKEN" "sk-live-abcdef1234567890abcdef"))
                                      "docs" (mc-entry "url" "https://docs.example.test/mcp"
                                                       "headers" (mc-table "Authorization" "Bearer abcdefghijklmnop"))))
   "mcp-creator" (apply #'mc-table section-pairs)))

(defmacro with-creator ((config) &body body)
  "The cell started on CONFIG, stopped after."
  `(progn
     (nodecode-mcp-creator:stop-cell)
     (unwind-protect
          (progn (nodecode-mcp-creator:start-cell ,config)
                 ;; the servers a test configures, without writing a config file
                 (setf nodecode-mcp-creator::*servers*
                       (gethash "servers" (gethash "mcp" ,config)))
                 ,@body)
       (nodecode-mcp-creator:stop-cell))))

(defmacro with-data-dir ((directory) &body body)
  "The cell's data seam pointed at a scratch DIRECTORY."
  `(let ((nodecode-mcp-creator::*data-dir* ,directory))
     ,@body))

(defun mc-command-named (name)
  "The registered slash command NAME, or NIL."
  (find name nle::*registered-commands* :key (lambda (command) (slot-value command 'nle::name))
                                          :test #'equal))

(defun mc-tool-text (name arguments)
  "Call a registered tool through the real dispatch seam."
  (execute-wire-call "call-mc" name arguments))
