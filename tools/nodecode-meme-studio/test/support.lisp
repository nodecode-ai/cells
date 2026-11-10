;;;; support.lisp --- the meme-studio test runner and its scripted seam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; MEME-STUDIO-CELL- prefix, and RUN-MEME-STUDIO-TESTS runs exactly that
;;;; slice, so this system's test-op never re-runs the core suite.
;;;;
;;;; The one network call is NODECODE-MEME-STUDIO::POST; WITH-SCRIPTED-MCP
;;;; stubs that name, records every JSON-RPC request and answers a scripted
;;;; result, so no test ever opens a socket.

(in-package #:nodecode.test)

(define-test-slice "meme-studio" "MEME-STUDIO-CELL-" :start nodecode-meme-studio:start-cell)

;;; --- config and lifecycle fixtures ----------------------------------------------

(defmacro with-meme-studio ((&rest config-pairs) &body body)
  "Run BODY with the cell started on CONFIG-PAIRS and stopped on unwind."
  `(with-cell-stop ((meme-studio-start ,@config-pairs))
     ,@body))

;;; --- the scripted seam -----------------------------------------------------------

(defun mcp-result (&rest pairs)
  "A tool result object, built from alternating member/value PAIRS."
  (apply #'nlk:make-json-object pairs))

(defun mcp-text-result (&rest texts)
  "A tool result whose content is one text item per TEXT, as the server sends
one."
  (mcp-result "content"
              (coerce (loop for text in texts
                             collect (nlk:make-json-object "type" "text" "text" text))
                      'vector)))

(defun mcp-picture-result (text data mime)
  "A tool result carrying TEXT and one picture item, as create_meme sends."
  (mcp-result "content"
              (vector (nlk:make-json-object "type" "text" "text" text)
                      (nlk:make-json-object "type" "image" "data" data "mimeType" mime))))

(defun mcp-error-result (text)
  "A tool result the server marks as an error, as a miss answers."
  (mcp-result "isError" t "content"
              (vector (nlk:make-json-object "type" "text" "text" text))))

(defstruct (mcp-call (:copier nil)) method tool arguments)

(defmacro with-scripted-mcp ((calls &key result) &body body)
  "Run BODY with the cell's one network call stubbed: every request recorded
into CALLS, oldest first, and RESULT -- a tool result object, or a function of
the call answering one -- answered as HTTP 200."
  `(let ((,calls '()))
     (with-stubbed-fdefinition (nodecode-meme-studio::post (request)
                                  (let* ((object (nlk:decode-json request))
                                         (params (gethash "params" object))
                                         (call (make-mcp-call
                                                :method (gethash "method" object)
                                                :tool (and (hash-table-p params) (gethash "name" params))
                                                :arguments (and (hash-table-p params)
                                                                (gethash "arguments" params)))))
                                    (push call ,calls)
                                    (values (nlk:encode-json-object
                                             (nlk:make-json-object
                                              "jsonrpc" "2.0" "id" 1
                                              "result" (if (functionp ,result)
                                                           (funcall ,result call)
                                                           ,result)))
                                            200)))
       ,@body)))

;;; --- calling what the cell contributes --------------------------------------------

(defun meme-tool (name &rest pairs)
  "The registered tool NAME, called the way the engine calls it: with the
decoded arguments table the body receives."
  (let ((entry (nle::find-tool name)))
    (assert entry () "the ~a tool is not registered" name)
    (funcall (third entry) (apply #'nlk:make-json-object pairs))))

(defun meme-command (args)
  "The /meme command, run the way a shell runs it: (values TEXT DIALOG)."
  (let ((entry (nle::find-registered-command "meme")))
    (assert entry () "the /meme command is not registered")
    (funcall (nle::slash-command-headless entry) args "s1")))
