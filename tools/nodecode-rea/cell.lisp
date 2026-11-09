;;;; cell.lisp --- the section, the verbs, the two tools, /rea and the primer.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin gave Hermes one thing it could not have written down and one
;;;; thing it could: the server (a pinned `npx rea-agents@<pin> mcp' over
;;;; stdio, two environment variables forwarded, and no config edit needed),
;;;; and the reverse-engineering workflow skill. This file is both: the
;;;; connection is made here, so `nodecode add nc://rea' is the whole
;;;; install, and the workflow is what (help :rea) answers.
;;;;
;;;; REA registers 207 tools whose schemas total some 800 KB. Declaring them
;;;; all here would put them in every request, so the cell declares two:
;;;; one to find a tool, one to run it. The rest is (rea:tools ...) from
;;;; eval, which costs one call and no context.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "rea": {"server": "rea-agents@6.7.0", "npx": "npx",
;;;;           "call_timeout_seconds": 300,
;;;;           "ghidra_startup_timeout_ms": 900000,
;;;;           "pwntools_python": ""}
;;;; A vetoed section ("enabled": false) installs nothing and connects nothing.

(in-package #:nodecode-rea)

(defparameter +primer+
  "REA reverse-engineers a program through its own MCP server: 207 tools that read a
binary, an application package, a firmware image or a core dump and answer Evidence -
decompiled code, call graphs, application graphs, captures. Read the Evidence; REA does not
explain it.
  (rea:tools &key search)          (rea:schema \"open_binary\")     (rea:status)   (rea:reconnect)
  (rea:call \"open_binary\" :path \"/tmp/prog\" :analysis-depth \"standard\")
      Verbs: `rea:tools' lists the server's tools (a word narrows them), `rea:schema' one tool's
      arguments, `rea:status' the connection, `rea:reconnect' reconnects. `rea:call' runs a tool
      with keyword arguments, folded to the schema's names (file-path becomes file_path); a
      single JSON object answers for tools whose arguments nest. Its answer is the tool's text,
      capped at 7000 characters (limit changes it). REA-ERROR is the server's refusal, and it
      names what to fix.
Work it in this order. Route by target first: native executable or library, .NET assembly,
Electron or JavaScript app (ASAR), Android package, firmware, HAR or packet capture. Open the
target once - `open_binary' and its siblings - because the first Ghidra-backed query imports
and analyses it and can take minutes, and every later query reuses that session. Reopening
discards the analysis. Then take a summary before a page: a function list or module map, the
application graph, the call graph, and only then the one function that matters. Pseudocode is
decompiler output, not source: cross-check a consequential claim against assembly and its
callers, mark names and types REA inferred, and say when symbols are missing. A core dump is a
recorded run: read it with the crash tools, place the program counter with the binary's layout,
and walk back from the crashing function to the caller that produced the bad value.
Some tools run or change something - a command under a PTY, a browser or native window, an
Android device, Frida, gdb or rizin, an arbitrary rizin command, a file overwritten at a path you
name. Name the exact command, app or device and ask the operator before any of them, and skip
them in an unattended run. Analyse an untrusted program somewhere you chose for it. REA sends no
telemetry; it reaches the network only for the target it is pointed at."
  "What (help :rea) answers while the cell runs.")

(defvar *connection* nil
  "The live transport to the REA server, or NIL. Made by the first verb that
needs one, closed on stop, after which the next verb makes another.")

(defvar *tool-count* nil
  "How many tools the last tools/list answered, for /rea's line. Not a cache:
the list itself is asked for every time, because a server's tools can change.")

(defun command-line (settings)
  "The argv of the server, as the plugin's mcp.json wrote it."
  (let ((server (getf settings :server))
        (npx (getf settings :npx)))
    (when (or (null server) (string= server ""))
      (fail "the rea section names no server: set server to the pinned package (rea-agents@6.7.0 as the plugin pinned it) in /settings, then (restart-cells \"nodecode-rea\")"))
    (list (if (and npx (not (string= npx ""))) npx "npx") "-y" server "mcp")))

(defun server-env (settings)
  "What mcp.json forwarded. A value left empty is not set, so REA's own
default stands instead of an empty string."
  (list (cons "REA_GHIDRA_STARTUP_TIMEOUT_MS"
              (princ-to-string (or (getf settings :ghidra-startup-timeout-ms) 900000)))
        (cons "REA_PWNTOOLS_PYTHON" (getf settings :pwntools-python))))

(defun connect ()
  "Launch the server and shake hands. Nothing calls this at start: a cell that
is on and unused spends no npx."
  (let* ((settings (running-settings))
         (transport nil))
    (unwind-protect
         (progn
           (setf transport (open-transport (command-line settings) (server-env settings)))
           (handshake transport)
           (setf *connection* transport
                 *tool-count* nil)
           transport)
      (unless (eq transport *connection*)
        (when transport (ignore-errors (transport-close transport)))))))

(defun connection ()
  "The live transport, connecting if there is none."
  (or *connection* (connect)))

(defun disconnect ()
  (when *connection*
    (ignore-errors (transport-close *connection*))
    (setf *connection* nil))
  nil)

;;; --- the verbs ---------------------------------------------------------------

(defun shorten (text limit)
  (let ((text (or text "")))
    (substitute #\Space #\Newline text)
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 (1- limit)) "...")
        text)))

(defun fold-key (key)
  "A keyword argument's wire name: :file-path is file_path, as the schema
spells it."
  (string-downcase (substitute #\_ #\- (string key))))

(defun arguments-object (arguments)
  "ARGUMENTS, a hash table or a plist of keyword pairs, as the JSON object the
server reads."
  (cond
    ((and (= 1 (length arguments)) (hash-table-p (first arguments)))
     (first arguments))
    ((evenp (length arguments))
     (let ((object (nlk:json-object)))
       (loop for (key value) on arguments by #'cddr
             do (setf (gethash (fold-key key) object) value))
       object))
    (t (fail "arguments come as keyword pairs (:path \"/tmp/a\") or one JSON object; ~s is not a pair"
             arguments))))

(defun result-text (result &key (limit 7000))
  "RESULT's content blocks as text. A tool that failed says so in its own words,
which are the model's to read."
  (let* ((content (and (hash-table-p result) (gethash "content" result)))
         (pieces (loop for block across (if (vectorp content) content (or content #()))
                       when (and (hash-table-p block)
                                 (or (null (gethash "type" block))
                                     (equal (gethash "type" block) "text")))
                         collect (or (gethash "text" block) "")))
         (text (format nil "~{~a~^~%~}" pieces))
         (text (if (zerop (length text))
                   (shasht:write-json (or result (nlk:json-object)) nil)
                   text)))
    (when (and (hash-table-p result) (gethash "isError" result))
      (fail "~a" (shorten text limit)))
    (if (> (length text) limit)
        (concatenate 'string (subseq text 0 limit)
                     (format nil "~%[~d more characters; limit raises the cap]"
                             (- (length text) limit)))
        text)))

(define-verb tools (&key search)
  "List the server's tools as (:name :description) plists, or the ones whose
name or description holds SEARCH."
  (let* ((settings (running-settings))
         (result (transport-request (connection) "tools/list" nil
                                     :timeout (getf settings :timeout)))
         (rows (and (hash-table-p result) (gethash "tools" result)))
         (rows (coerce (if (vectorp rows) rows #()) 'list))
         (needle (and search (plusp (length (string search))) (string-downcase search)))
         (all (loop for row in rows
                    when (hash-table-p row)
                      collect (list :name (gethash "name" row)
                                    :description (or (gethash "description" row) "")))))
    (setf *tool-count* (length all))
    (if needle
        (remove-if-not (lambda (row)
                         (or (cl:search needle (string-downcase (getf row :name)))
                             (cl:search needle (string-downcase (getf row :description)))))
                       all)
        all)))

(define-verb schema (tool)
  "TOOL's input schema, as the server declares it."
  (let* ((settings (running-settings))
         (result (transport-request (connection) "tools/list" nil
                                     :timeout (getf settings :timeout)))
         (rows (and (hash-table-p result) (gethash "tools" result))))
    (or (loop for row across (if (vectorp rows) rows #())
              when (and (hash-table-p row) (equal (gethash "name" row) tool))
                return (gethash "inputSchema" row))
        (fail "REA has no tool named ~s: (rea:tools :search ~s) finds the nearest" tool tool))))

(define-verb call (tool &rest arguments)
  "Run TOOL with ARGUMENTS - keyword pairs (:path \"/tmp/a\"), or one JSON
object - and answer its text."
  (let ((settings (running-settings)))
    (result-text (transport-request (connection) "tools/call"
                                    (nlk:json-object "name" tool
                                                     "arguments" (arguments-object arguments))
                                    :timeout (getf settings :timeout)))))

(defun status-plist ()
  (list :state (if *connection* "connected" "not connected")
        :command (and *connection* (transport-describe *connection*))
        :server (getf (running-settings) :server)
        :tools *tool-count*
        :timeout (getf (running-settings) :timeout)))

(define-verb status ()
  "The connection, the pinned server and how many tools it last answered."
  (status-plist))

(define-verb reconnect ()
  "Close the connection and make another: after a setting changes, or after the
server died. The Ghidra session is discarded with it."
  (disconnect)
  (connect)
  (status-plist))

;;; --- what the model calls ------------------------------------------------------

(nlk:define-contract rea-tools-request
  (:doc "Find one of REA's reverse-engineering tools: one that opens a binary, an app package, a
firmware image or a core dump, or steps through the Evidence it returns.")
  ("search" :text :default "" :slot word
   :doc "a word to match in a tool's name or description (decompile, android, har, crash); empty lists every tool"))

(nlk:define-contract rea-call-request
  (:doc "Run one REA tool. REA returns Evidence as text: decompiled code, a call graph, an
application graph, a capture.")
  ("tool" :text :required t
   :doc "the tool's name, as rea_tools lists it (open_binary, analyze_function, inspect_android_package)")
  ("arguments" :object
   :doc "the tool's arguments as a JSON object, as rea:schema describes them"))

(defun tool-tools (request)
  "The model's tool: a readable list, capped and countable."
  (let* ((search (rea-tools-request-word request))
         (found (tools :search (and search (plusp (length search)) search))))
    (cond
      ((null found)
       (format nil "REA names no tool matching ~s. (rea:tools) lists all ~a."
               search (or *tool-count* "of its")))
      (t
       (format nil "~d REA tool~:p~@[ matching ~s~]~@[, of ~d~]:~%~{~a~^~%~}~@[~%~d more: narrow the search~]"
               (length found) search *tool-count*
               (loop for row in (if (> (length found) 80) (subseq found 0 80) found)
                     collect (format nil "~a - ~a" (getf row :name)
                                     (shorten (getf row :description) 110)))
               (let ((more (- (length found) 80)))
                 (and (plusp more) more)))))))

(defun tool-call (request)
  "The model's tool: one REA call, its text back."
  (let ((arguments (rea-call-request-arguments request)))
    (apply #'call (rea-call-request-tool request)
           (if arguments (list arguments) '()))))

;;; --- /rea ---------------------------------------------------------------------

(defun status-line ()
  "One line for /rea: the connection, the pin, and where the section lives."
  (let ((status (status-plist)))
    (format nil "REA: ~a~@[~%  ~a~]~%  server ~a, ~a~%  (rea:tools) lists the tools, (rea:call \"open_binary\" :path \"...\") runs one; set the section in /settings"
            (getf status :state)
            (getf status :command)
            (or (getf status :server) "(unset: set server in /settings)")
            (if nodecode-rea::*tool-count*
                (format nil "~d tools" nodecode-rea::*tool-count*)
                "no tools listed yet"))))

(defun run-command (args session-id)
  "The /rea command: status, tools [WORD], or restart."
  (declare (ignore session-id))
  (let* ((text (string-trim '(#\Space #\Tab) (or args "")))
         (space (position #\Space text))
         (verb (if space (subseq text 0 space) text))
         (rest (and space (string-trim '(#\Space #\Tab) (subseq text (1+ space))))))
    (handler-case
        (cond
          ((or (string= verb "") (string= verb "status"))
           (status-line))
          ((string= verb "tools")
           (let ((found (tools :search (and rest (plusp (length rest)) rest))))
             (format nil "~d REA tool~:p~@[ matching ~s~]:~%~{~a~^~%~}"
                     (length found) rest
                     (loop for row in found
                           collect (format nil "~a - ~a" (getf row :name)
                                           (shorten (getf row :description) 90))))))
          ((string= verb "restart")
           (reconnect)
           (format nil "REA restarted: ~a" (status-line)))
          (t (format nil "rea: ~s is not a verb. /rea takes status, tools [WORD] or restart." verb)))
      (rea-error (condition)
        (format nil "REA: ~a" condition)))))

;;; --- the cell ------------------------------------------------------------------

(defun start ()
  "Nothing connects until a verb asks for something."
  (disconnect)
  (setf *tool-count* nil)
  (nle:on-stop #'disconnect))

(nle:define-cell rea
  (:section ("rea")
    (:guide "server is the pinned npm package the MCP server runs from, rea-agents@6.7.0 as the plugin pinned it; npx is the launcher and needs Node.js 22.19+, 24.11+ or 26+; pwntools_python names a Python with pwntools, pyelftools and Unicorn, which core dumps need; then ask for a binary, an app or a core")
    ("server" :string :required t
     :doc "the pinned npm package the REA MCP server runs from, rea-agents@<version>")
    ("npx" :string :default "npx"
     :doc "the npx that launches it: on PATH, or a path")
    ("call_timeout_seconds" :integer :default 300 :min 1
     :doc "one call's deadline; a first Ghidra import can take minutes")
    ("ghidra_startup_timeout_ms" :integer :default 900000 :min 1
     :doc "forwarded as REA_GHIDRA_STARTUP_TIMEOUT_MS, REA's own import deadline")
    ("pwntools_python" :string :default ""
     :doc "forwarded as REA_PWNTOOLS_PYTHON: a Python with pwntools, pyelftools and Unicorn, for core dumps"))
  (:tool "rea_tools" (nlk:contract 'rea-tools-request) 'tool-tools)
  (:tool "rea_call" (nlk:contract 'rea-call-request) 'tool-call)
  (:help :rea "rea: reverse-engineer a binary, app or core dump through REA's MCP server - (rea:tools), (rea:call \"open_binary\" :path \"...\")" +primer+)
  (:command "rea" 'run-command
            :description "REA: the server's state, its tools, a restart"
            :argument-hint "[status|tools [WORD]|restart]")
  (:start #'start))
