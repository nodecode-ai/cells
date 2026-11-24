;;;; cell-test.lisp --- the rea cell against a stubbed MCP server.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The run has no network and must not need Node: every request the cell
;;;; sends is answered by REA-STUB, a transport class of whose four generic
;;;; functions the suite defines what it wants. So the handshake, the tool
;;;; list, one tool call and the server's own refusal are all exercised
;;;; without an npx, a process or a socket anywhere.

(in-package #:nodecode.test)

(define-test-slice "rea" "REA-CELL-" :start nodecode-rea:start-cell
                   :defaults (("server" "rea-agents@6.7.0")))

;;; --- the stub server --------------------------------------------------------------

(defclass rea-stub (nodecode-rea::transport)
  ((calls :initform '() :accessor rea-stub-calls)
   (closed :initform nil :accessor rea-stub-closed)))

(defvar *stub* nil
  "The stub this test's factory answers, so a test can read what was asked.")

(defun rea-stub-answer (text)
  "TEXT as the decoded JSON body the cell reads."
  (cell-json text))

(defun rea-stub-tools ()
  "Three tools, as the server answers tools/list."
  (rea-stub-answer
   "{\"tools\":[{\"name\":\"open_binary\",\"description\":\"Open a native binary, library, .NET assembly or firmware image and answer its summary.\",\"inputSchema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}}}},{\"name\":\"analyze_function\",\"description\":\"Decompile one function by address or name.\",\"inputSchema\":{\"type\":\"object\"}},{\"name\":\"inspect_android_package\",\"description\":\"Inventory an APK: activities, services, native libraries.\",\"inputSchema\":{\"type\":\"object\"}}]}"))

(defmethod nodecode-rea::transport-describe ((transport rea-stub))
  "stub: rea-agents@6.7.0 mcp")

(defmethod nodecode-rea::transport-notify ((transport rea-stub) method params)
  (push (list method params) (rea-stub-calls transport))
  nil)

(defmethod nodecode-rea::transport-close ((transport rea-stub))
  (setf (rea-stub-closed transport) t)
  nil)

(defmethod nodecode-rea::transport-request ((transport rea-stub) method params &key timeout)
  (declare (ignore timeout))
  (push (list method params) (rea-stub-calls transport))
  (cond
    ((equal method "initialize")
     (rea-stub-answer "{\"protocolVersion\":\"2024-11-05\",\"capabilities\":{},\"serverInfo\":{\"name\":\"rea\",\"version\":\"6.7.0\"}}"))
    ((equal method "tools/list")
     (rea-stub-tools))
    ((equal method "tools/call")
     (let ((name (and (hash-table-p params) (gethash "name" params))))
       (cond
         ((equal name "open_binary")
          (rea-stub-answer "{\"content\":[{\"type\":\"text\",\"text\":\"opened /tmp/parser: 42 functions, 3 modules\"}],\"isError\":false}"))
         ((equal name "inspect_android_package")
          (rea-stub-answer "{\"content\":[{\"type\":\"text\",\"text\":\"com.example.app: 3 activities, 1 native library\"}],\"isError\":false}"))
         (t (rea-stub-answer "{\"content\":[{\"type\":\"text\",\"text\":\"open a binary before asking for a function\"}],\"isError\":true}")))))
    (t (rea-stub-answer "{}"))))

(defmacro with-rea-stub (&body body)
  "BODY with a transport factory that answers a fresh stub, so no process runs."
  `(let* ((*stub* (make-instance 'rea-stub))
          (nodecode-rea::*transport-factory* (lambda (command env)
                                               (declare (ignore command env))
                                               *stub*)))
     (unwind-protect (progn ,@body)
       (setf nodecode-rea::*connection* nil))))

(defmacro with-rea-cell (&body body)
  "BODY with the cell started on the pinned server against the stub, stopped on unwind."
  `(with-rea-stub
     (with-cell-stop ((rea-start "server" "rea-agents@6.7.0")) ,@body)))

;;; --- the lifecycle ------------------------------------------------------------------

(define-cell-lifecycle-tests "rea"
  (:config "server" "rea-agents@6.7.0")
  (:help :rea)
  (:command "rea")
  (:fixture with-rea-stub)
  (:running (is (null nodecode-rea::*connection*) "a start spends no npx"))
  (:stopped (is (null nodecode-rea::*connection*) "and stop leaves none"))
  (:refused ("server" 7) ("call_timeout_seconds" 0))
  (:idle nodecode-rea::rea-error (nodecode-rea:tools) (nodecode-rea:call "open_binary")))

;;; --- the verbs ----------------------------------------------------------------------

(deftest rea-cell-lists-the-servers-tools ()
  (with-rea-cell
    (let ((tools (nodecode-rea:tools)))
      (is (= 3 (length tools)) "every tool the server lists")
      (is (equal "open_binary" (getf (first tools) :name)) "name first, as the schema says")
      (is (search "native binary" (getf (first tools) :description)) "with its description")
      (is (= 3 nodecode-rea::*tool-count*) "and the count /rea reports"))
    (let ((android (nodecode-rea:tools :search "android")))
      (is (= 1 (length android)) "a word narrows to the tools that name it")
      (is (equal "inspect_android_package" (getf (first android) :name))))
    (is (null (nodecode-rea:tools :search "frida")) "and to nothing when nothing matches")
    (is (equal "tools/list" (first (first (rea-stub-calls *stub*)))) "the list came from the server, not from the cell")))

(deftest rea-cell-calls-one-tool ()
  (with-rea-cell
    (is (search "opened /tmp/parser" (nodecode-rea:call "open_binary" :file-path "/tmp/parser")) "a tool's text is the answer")
    (let* ((call (find "tools/call" (rea-stub-calls *stub*) :key #'first :test #'equal))
           (arguments (and call (gethash "arguments" (second call)))))
      (is (string= "open_binary" (gethash "name" (second call))) "named as the model named it")
      (is (string= "/tmp/parser" (gethash "file_path" arguments)) "a keyword folds to the schema's own spelling"))
    (is (search "open a binary before"
                (refusal-text nodecode-rea::rea-error
                              (nodecode-rea:call "analyze_function" :name "main"))) "a tool that failed says so in its own words")))

(deftest rea-cell-schema-answers-for-a-tool ()
  (with-rea-cell
    (let ((schema (nodecode-rea:schema "open_binary")))
      (is (hash-table-p schema) "the input schema comes back as the server wrote it")
      (is (equal "object" (gethash "type" schema))))
    (is (search "no tool named"
                (refusal-text nodecode-rea::rea-error (nodecode-rea:schema "frida"))) "an unknown tool is a refusal that says so")))

;;; --- what the model calls -------------------------------------------------------------

(deftest rea-cell-model-tools-answer-through-the-stub ()
  (with-rea-cell
    (let ((listed (nodecode-rea::tool-tools
                   (nodecode-rea::decode-rea-tools-request (rea-stub-answer "{}")))))
      (is (search "open_binary" listed) "rea_tools names the tools")
      (is (search "analyze_function" listed))
      (is (search "3 REA tools" listed) "and how many the server carries"))
    (let ((narrowed (nodecode-rea::tool-tools
                     (nodecode-rea::decode-rea-tools-request
                      (rea-stub-answer "{\"search\":\"android\"}")))))
      (is (search "inspect_android_package" narrowed) "a search narrows")
      (is (not (search "open_binary" narrowed))))
    (is (search "opened /tmp/parser"
                (nodecode-rea::tool-call
                 (nodecode-rea::decode-rea-call-request
                  (rea-stub-answer "{\"tool\":\"open_binary\",\"arguments\":{\"path\":\"/tmp/parser\"}}")))) "rea_call runs one tool with its JSON arguments")))

(deftest rea-cell-result-is-capped ()
  (with-rea-cell
    (let ((long (nodecode-rea::result-text
                 (rea-stub-answer "{\"content\":[{\"type\":\"text\",\"text\":\"xxxxxxxxxx\"}]}")
                 :limit 4)))
      (is (search "xxxx" long) "the head of the answer")
      (is (search "more characters" long) "and how much the cap left out")
      (is (< (length long) 200) "so one decompilation cannot fill the context"))))

;;; --- /rea ---------------------------------------------------------------------------

(deftest rea-cell-command-answers ()
  (with-rea-cell
    (let ((status (cell-entry "nodecode-rea" "rea" "")))
      (is (search "not connected" status) "the command does not connect on its own")
      (is (search "rea-agents@6.7.0" status) "it names the pinned server")
      (is (search "/settings" (cell-entry "nodecode-rea" "rea" "status")) "and where to change it")
      (let ((tools (cell-entry "nodecode-rea" "rea" "tools android")))
        (is (search "inspect_android_package" tools) "/rea tools lists the server's tools")
        (is (not (search "open_binary" tools))))
      (is (search "restarted" (cell-entry "nodecode-rea" "rea" "restart")) "restart reconnects")
      (is (search "is not a verb" (cell-entry "nodecode-rea" "rea" "frobnicate")) "an unknown verb says what it takes"))))

;;; --- what the original forwarded, and what it did not ----------------------------------

(deftest rea-cell-forwards-the-plugins-environment ()
  (is (equal '("REA_GHIDRA_STARTUP_TIMEOUT_MS" . "900000")
             (first (nodecode-rea::server-env '(:ghidra-startup-timeout-ms 900000)))) "Ghidra's deadline rides as the plugin's mcp.json forwarded it")
  (is (equal '("REA_PWNTOOLS_PYTHON" . "/opt/venv/bin/python")
             (second (nodecode-rea::server-env '(:ghidra-startup-timeout-ms 900000
                                                  :pwntools-python "/opt/venv/bin/python")))) "and the pwntools Python, when the operator named one")
  (is (equal '("npx" "-y" "rea-agents@6.7.0" "mcp")
             (nodecode-rea::command-line '(:npx "npx" :server "rea-agents@6.7.0"))) "the launch is the plugin's own, pinned")
  (is (search "names no server"
              (refusal-text nodecode-rea::rea-error
                            (nodecode-rea::command-line '(:npx "npx" :server "")))) "and a section without one refuses, naming the setting"))
