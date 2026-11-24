;;;; router.lisp --- the workspace router child, and one answer from it.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original keeps one long-lived router per session and multiplexes
;;;; replies by id; a gesture here is rare enough that each call starts the
;;;; router, initializes it, makes its one tools/call and closes it, so a
;;;; dead or silent router costs nothing to recover from. JSON-RPC over the
;;;; child's stdio, as the original has it; the host-to-VM failover is the
;;;; router's own, and every reply says which host answered in _meta.

(in-package #:nodecode-alans-way)

(nlk:define-peripheral alans-way :not-running t)

(defun router-argv ()
  "The command and its arguments, as the section names them."
  (cons (setting :router-command) (coerce (setting :router-args) 'list)))

(defun json-line (object)
  (concatenate 'string (nlk:encode-json-object object) (string #\Newline)))

(defun initialize-frame (id)
  (json-line
   (nlk:json-object
    "jsonrpc" "2.0" "id" id "method" "initialize"
    "params" (nlk:json-object
               "protocolVersion" "2024-11-05"
               "capabilities" (nlk:json-object)
               "clientInfo" (nlk:json-object "name" "nodecode-alans-way" "version" "0.1.0")))))

(defun initialized-frame ()
  (json-line (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized")))

(defun call-frame (id tool arguments)
  (json-line
   (nlk:json-object "jsonrpc" "2.0" "id" id "method" "tools/call"
                     "params" (nlk:json-object "name" tool "arguments" arguments))))

(defun reply-with (stream id deadline)
  "The JSON-RPC reply with ID, read off STREAM past any other frame; DEADLINE
is an internal-real-time moment. A stream that ends first, or the deadline,
is a refusal."
  (loop
    (when (> (get-internal-real-time) deadline)
      (fail "the workspace router did not answer within ~ds" (setting :call-timeout)))
    (let ((line (read-line stream nil nil)))
      (unless line
        (fail "the workspace router closed its output before answering"))
      (let ((body (ignore-errors (nlk:decode-json line))))
        (when (and (hash-table-p body) (equal id (nlk:json-value body :any "id")))
          (return body))))))

(defun router-call (tool arguments)
  "Start the workspace router, initialize it, call TOOL with ARGUMENTS, and
answer its result object. The child is terminated on every path out."
  (let ((argv (router-argv))
        (limit (setting :call-timeout))
        (process nil))
    (handler-case
        (unwind-protect
             (progn
               (handler-case
                   (setf process (uiop:launch-program argv :input :stream :output :stream
                                                      :error-output :output
                                                      :element-type 'character))
                 (error (condition)
                   (fail "cannot start the workspace router (~{~a~^ ~}): ~a" argv condition)))
               (let ((in (uiop:process-info-input process))
                     (out (uiop:process-info-output process))
                     (deadline (+ (get-internal-real-time)
                                   (* limit internal-time-units-per-second))))
                 (write-string (initialize-frame 1) in)
                 (finish-output in)
                 (reply-with out 1 deadline)
                 (write-string (concatenate 'string (initialized-frame)
                                            (call-frame 2 tool arguments))
                               in)
                 (finish-output in)
                 (let ((reply (reply-with out 2 deadline)))
                   (or (nlk:json-value reply :object "result")
                       (fail "~a: ~a" tool
                             (or (nlk:json-value reply :string "error" "message") "no result"))))))
          (when process
            (ignore-errors (uiop:terminate-process process))))
      ((or error sb-sys:deadline-timeout) (condition)
        (if (typep condition 'alans-way-error)
            (error condition)
            (fail "the workspace router: ~a" condition))))))

;;; --- one answer, read -------------------------------------------------------

(defun content-of (result)
  (or (nlk:json-value result :array "content") #()))

(defun texts-of (result)
  "RESULT's text items, in order."
  (loop for item across (content-of result)
        when (equal "text" (nlk:json-value item :string "type"))
          collect (or (nlk:json-value item :string "text") "")))

(defun image-of (result)
  "RESULT's first image item, or NIL."
  (loop for item across (content-of result)
        when (equal "image" (nlk:json-value item :string "type"))
          return item))

(defun host-of (result)
  "The host RESULT came from, or NIL when the router did not say."
  (nlk:json-value result :string "_meta" "workspace" "host"))

(defun object-of (text)
  "TEXT as a decoded object; anything else is an empty one."
  (if (and text (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
      (handler-case (let ((value (nlk:decode-json text)))
                      (if (hash-table-p value) value (nlk:json-object)))
        (error () (nlk:json-object)))
      (nlk:json-object)))

(defun ask (tool arguments)
  "Call TOOL with ARGUMENTS; answer (values BODY HOST NOTES): its first
text as an object, the host that answered, and the router's further notes.
An isError answer is a refusal, its code kept in front for the model."
  (let* ((result (funcall *router-call* tool arguments))
         (texts (texts-of result)))
    (when (nlk:json-value result :boolean "isError")
      (let ((code (or (nlk:json-value result :string "code")
                      (nlk:json-value result :string "structuredContent" "code"))))
        (fail "~@[~a: ~]~a" code (or (first texts) "the router refused"))))
    (values (object-of (first texts)) (host-of result) (rest texts))))

(defvar *router-call* #'router-call
  "How one router tool is called: (TOOL ARGUMENTS) => the MCP result object
as decoded JSON. The tests bind it, so no process starts in them.")
