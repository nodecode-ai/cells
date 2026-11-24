;;;; client.lisp --- the Kiwi buyer runtime as a child process, one call at a
;;;; time.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; The original registers the runtime as an MCP server and leaves the
;;;; session open. Here each call starts the pinned runtime, shakes hands,
;;;; makes its one tools/call and closes it: the buyer's own state lives in
;;;; the runtime's SQLite store, not in the process, so a session that dies
;;;; costs nothing to recover from -- the next call starts another.
;;;;
;;;; JSON-RPC over the child's stdio, one JSON object a line, as MCP's stdio
;;;; transport has it. The one seam that reaches outside is EXCHANGE; a test
;;;; replaces it and nothing ever starts a process.

(in-package #:nodecode-kiwi)

(defparameter +protocol-version+ "2025-06-18"
  "The MCP protocol revision the handshake names, as the plugin's own health
check has it.")

(defparameter +timeout-default+ 120
  "Seconds one call may take when the section names none: the first launch
downloads the pinned package from npm.")

;;; --- what is started --------------------------------------------------------

(defun nonblank (value)
  "VALUE as a trimmed string, or NIL when it is empty or not a string."
  (let ((trimmed (and (stringp value)
                       (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (and trimmed (plusp (length trimmed)) trimmed)))

(defun runtime-argv ()
  "The command and its arguments, as the section names them: the launcher,
the package to run, then the runtime's own `mcp serve' verbs."
  (getf *kiwi* :argv))

(defun runtime-env ()
  "The environment the section adds to the child's: the store's path, when
one is named. The runtime's own default keeps it under the home."
  (let ((db (getf *kiwi* :db-path)))
    (when db (list (cons :kiwi_mcp_db db)))))

;;; --- the frames -------------------------------------------------------------

(defun json-line (object)
  "OBJECT as one JSON-RPC frame: one line of JSON."
  (concatenate 'string (nlk:encode-json-object object) (string #\Newline)))

(defun initialize-frame (id)
  "The handshake, as MCP's stdio transport writes it."
  (json-line
   (nlk:json-object
    "jsonrpc" "2.0" "id" id "method" "initialize"
    "params" (nlk:json-object
               "protocolVersion" +protocol-version+
               "capabilities" (nlk:json-object)
               "clientInfo" (nlk:json-object "name" "nodecode-kiwi" "version" "0.1.0")))))

(defun initialized-frame ()
  "The notification that closes the handshake: a frame with no id."
  (json-line (nlk:json-object "jsonrpc" "2.0" "method" "notifications/initialized")))

(defun request-frame (id method params)
  "One request, its PARAMS the object the method takes."
  (json-line (nlk:json-object "jsonrpc" "2.0" "id" id "method" method "params" params)))

(defun reply-with (stream id deadline)
  "The reply frame carrying ID, read off STREAM past every other line."
  ;; A launcher writes its own noise on the same stream -- npm's progress, a
  ;; deprecation warning -- so a line that is not JSON, or is another frame,
  ;; is passed over. DEADLINE is an internal-real-time moment.
  (loop
    (when (> (get-internal-real-time) deadline)
      (fail "the Kiwi buyer runtime did not answer within ~as" (getf *kiwi* :timeout-seconds)))
    (let ((line (read-line stream nil nil)))
      (unless line
        (fail "the Kiwi buyer runtime closed its output before answering; check that Node.js and npm are on PATH"))
      (let ((body (ignore-errors (nlk:decode-json line :whole t))))
        (when (and (hash-table-p body)
                   (equal id (nlk:json-value body :any "id")))
          (return body))))))

(defun error-text (reply)
  "REPLY's JSON-RPC error as one line of words."
  (let ((error (nlk:json-value reply :object "error")))
    (if (null error)
        "no result"
        (format nil "~a (~a)"
                (or (nlk:json-value error :string "message") "an error with no message")
                (let ((code (nlk:json-value error :any "code")))
                  (if (numberp code) code "no code"))))))

;;; --- one exchange -----------------------------------------------------------

(defun exchange (method params)
  "Start the buyer runtime, shake hands, send METHOD with PARAMS, and answer
its reply frame. The child is terminated on every path out."
  (let ((argv (runtime-argv))
        (limit (getf *kiwi* :timeout-seconds))
        (process nil))
    (handler-case
        (unwind-protect
             (progn
               (handler-case
                   (setf process (uiop:launch-program argv
                                                      :input :stream :output :stream
                                                      :error-output :output
                                                      :element-type 'character
                                                      :env (runtime-env)))
                 (error (condition)
                   (fail "cannot start the Kiwi buyer runtime (~{~a~^ ~}): ~a. Node.js and npm must be on PATH; the kiwi section's command and package name what is started."
                         argv condition)))
               (let ((in (uiop:process-info-input process))
                     (out (uiop:process-info-output process))
                     (deadline (+ (get-internal-real-time)
                                  (* limit internal-time-units-per-second))))
                 (sb-ext:with-timeout limit
                   (write-string (initialize-frame 1) in)
                   (finish-output in)
                   (let ((hello (reply-with out 1 deadline)))
                     (when (nlk:json-value hello :object "error")
                       (fail "the Kiwi buyer runtime refused its handshake: ~a" (error-text hello))))
                   (write-string (concatenate 'string (initialized-frame)
                                              (request-frame 2 method params))
                                 in)
                   (finish-output in)
                   (reply-with out 2 deadline))))
          (when process
            (ignore-errors (uiop:terminate-process process))))
      (sb-ext:timeout ()
        (fail "the Kiwi buyer runtime did not answer ~a within ~as" method limit))
      (kiwi-error (condition) (error condition))
      (error (condition)
        (fail "the Kiwi buyer runtime: ~a"
              (or (ignore-errors (princ-to-string condition)) (type-of condition)))))))

(defun rpc (method params)
  "METHOD's result object, or a refusal naming the error the runtime answered."
  ;; EXCHANGE is called through its name, not through a captured definition:
  ;; a test that stubs the one function reaches every caller.
  (let ((reply (exchange method params)))
    (when (and (hash-table-p reply) (nlk:json-value reply :object "error"))
      (fail "~a: ~a" method (error-text reply)))
    (or (and (hash-table-p reply) (nlk:json-value reply :object "result"))
        (nlk:make-json-object))))

;;; --- reading an answer -------------------------------------------------------

(defun result-text (result)
  "RESULT's content items as text, one to a line."
  ;; A tool answers MCP content blocks; a runtime that answers
  ;; structuredContent instead gets that written out, and one that answers
  ;; neither gets its result object.
  (let ((parts (loop for item across (or (nlk:json-value result :array "content") #())
                     when (equal "text" (nlk:json-value item :string "type"))
                       collect (or (nlk:json-value item :string "text") ""))))
    (cond ((plusp (length parts)) (format nil "~{~a~^~%~}" parts))
          ((nlk:json-value result :object "structuredContent")
           (nlk:encode-json-object (nlk:json-value result :object "structuredContent")))
          (t (nlk:encode-json-object result)))))

(defun call-mcp (name arguments)
  "NAME, one tool of the buyer runtime, called with ARGUMENTS (a JSON
object). => its text; a failure answers ERROR: text the model can act on."
  (handler-case
      (let* ((arguments (if (hash-table-p arguments) arguments (nlk:make-json-object)))
             (result (rpc "tools/call"
                          (nlk:json-object "name" name "arguments" arguments)))
             (text (result-text result)))
        (if (nlk:json-value result :any "isError")
            (format nil "ERROR: ~a: ~a" name text)
            text))
    (kiwi-error (condition) (format nil "ERROR: KIWI: ~a" condition))
    (error (condition) (format nil "ERROR: KIWI: ~a" condition))))

(defun tools ()
  "The tools the running buyer runtime answers, as it describes them."
  (coerce (or (nlk:json-value (rpc "tools/list" (nlk:make-json-object)) :array "tools")
              #())
          'list))

(defun clipped (text limit)
  "TEXT on one line, at most LIMIT characters."
  (let* ((flat (substitute #\Space #\Newline (substitute #\Space #\Tab (or text ""))))
         (trimmed (string-trim " " flat)))
    (if (> (length trimmed) limit)
        (concatenate 'string (subseq trimmed 0 (- limit 3)) "...")
        trimmed)))

(defun tools-text (&optional (tools (tools)))
  "What the runtime answers, in the words /kiwi and kiwi_tools give: one line
a tool, then how to call one."
  (with-output-to-string (out)
    (format out "Kiwi buyer runtime ~a: ~a tool~:p.~%"
            (getf *kiwi* :package) (length tools))
    (dolist (tool tools)
      (format out "  ~a  ~a~%"
              (nlk:json-value tool :string "name")
              (clipped (nlk:json-value tool :string "description") 160)))
    (format out "Call one by name, with the arguments it declares: /kiwi takes no arguments of its own, so ask for the sourcing you want in words, and (help :kiwi) has the workflow.")))
