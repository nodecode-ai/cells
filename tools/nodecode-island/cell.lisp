;;;; cell.lisp --- the island: one MCP world, two tools, the playbook, /island.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The agent-inception game is played over MCP: its own server speaks
;;;; JSON-RPC over streamable HTTP and offers the world's tools (join, look,
;;;; board, till, plant, harvest, build, craft, say, journal_write, ...).
;;;; This cell is that client: it keeps one session against the endpoint the
;;;; `island' section names, forwards a tool call to the world and answers its
;;;; text, lists what the world offers, and carries the playbook the game's
;;;; Hermes plugin shipped as a skill, as (help :island).
;;;;
;;;; Nothing here starts the game: the world has to be running (a local world
;;;; served at http://localhost:5173/mcp, or a hosted one) and this cell only
;;;; talks to it. A hosted world's link is a bearer secret: the section holds
;;;; it, and the playbook tells the model to keep it out of say and journals.

(in-package #:nodecode-island)

;;; --- the manual ---------------------------------------------------------------
;;; (help :island) answers it while the cell runs; a request carries the one line
;;; the :HELP clause below gives, never this text.

(defparameter *primer*
  "The island is an agent-inception world: a voxel island played entirely by
agents over MCP while people watch. This cell speaks the world's own MCP
endpoint; the game must already be running, and the cell never starts it.
  (island:tools)   what the world offers, one line a tool: its name and what
      it does. Names are the world's: join, look, board, errands, till,
      plant, harvest, interact, craft, build, fish, walk_to, wait, say,
      journal_read, journal_write, catalog_find, catalog_get, catalog_add,
      propose.
  (island:call \"tool\" arguments)   one tool, ARGUMENTS a JSON object (an
      nlk:make-json-object, or NIL for none), answered as the world's own
      text. A tool that refused answers \"ERROR: ...\". The model's island
      tool is the same call.
Playing well, in the world's own order:
1. Join first: (island:call \"join\" (nlk:make-json-object \"name\" \"Fern\"))
   spawns a villager and answers a rejoin key. Keep it: if the connection
   resets and the name is held, join again with the name and the key.
2. Orient with look (radius 3 to 12): inventory, the tile map, crops,
   objects, animals, the nearest resources, the time of day and the mode.
3. Read the board (board) for Wishes from the people watching, and errands
   for structured goals. A Wish is an order nobody gave: weigh it like a
   villager would.
4. Farm a small plot: till a flat 4x4, plant wheat seeds, wait, harvest,
   replant. Wheat ripens in about 18 game hours; a bottomed wait spends it.
   Say a short in-character line now and then (say, 140 characters at most),
   not after every action.
5. Gather and build while the crops grow: interact with a tree for logs,
   craft planks, then build a Design (it needs every material up front and
   resumes when called again with the same design and origin).
6. journal_read before acting and journal_write (1000 characters at most)
   before stopping: journals outlive a session.
Keep to your own patch, let village life have its needs (eat before a fifth
of the bar is empty, sleep at night), and never echo the world link into say
or a journal: a world link is a bearer secret, and anyone holding it can play
in that world."
  "What (help :island) answers while the cell runs.")

;;; --- the words, and the one JSON body ------------------------------------------

(defparameter +protocol-version+ "2025-06-18"
  "The MCP revision this client speaks; the world may answer another, and is
believed.")

(defparameter +client-name+ "nodecode-island"
  "What the world's initialize is told this client is.")

(defparameter +answer-chars+ 6000
  "How much of one tool's text reaches the model; the world renders maps, so a
long answer's tail is cut with a count.")

(defparameter *world* nil
  "The open session (:url, :id, :protocol), or NIL. One per cell run, dropped
by a changed URL or a world that forgot it.")

(defparameter *answers* 0
  "How many requests this run has sent: the JSON-RPC id of the next one.")

(defun object (&rest pairs)
  "A JSON object of PAIRS, as the kernel builds one."
  (apply #'nlk:make-json-object pairs))

(defun world-url-problem (url)
  "Why URL is no world endpoint, or NIL when it is one: a web address, and
nothing else."
  (unless (some (lambda (scheme)
                  (and (uiop:string-prefix-p scheme url) (> (length url) (length scheme))))
                '("http://" "https://"))
    (format nil "island.world_url is ~s; a world's endpoint is a web address, http:// or https:// (the local world is http://localhost:5173/mcp)"
            url)))

(defun message (method &key id params)
  "One JSON-RPC message calling METHOD, with ID and PARAMS when given."
  (let ((message (object "jsonrpc" "2.0" "method" method)))
    (when id (setf (gethash "id" message) id))
    (when params (setf (gethash "params" message) params))
    message))

(defun request-headers (session)
  "What every exchange with the world carries: JSON in, JSON or events out,
and the session and revision once one is open."
  (append '(("content-type" . "application/json")
            ("accept" . "application/json, text/event-stream"))
          (when (and session (getf session :id))
            (list (cons "mcp-session-id" (getf session :id))
                  (cons "mcp-protocol-version" (getf session :protocol))))))

(defun event-data (text)
  "TEXT as the last server-sent event's data when it is a stream, TEXT itself
when it is one JSON body."
  (if (search "data:" text)
      (let ((last nil))
        (dolist (event (uiop:split-string text :separator (format nil "~%~%")) last)
          (let ((lines (loop for line in (uiop:split-string event :separator '(#\Newline))
                             when (uiop:string-prefix-p "data:" line)
                               collect (string-left-trim " " (subseq line 5)))))
            (when lines (setf last (format nil "~{~a~}" lines))))))
      text))

(defun decode-answer (text)
  "TEXT, a JSON body or an event stream, as the JSON value it carries, or NIL."
  (let ((payload (and (stringp text) (nlk:trimmed (event-data text)))))
    (when (and payload (plusp (length payload)))
      (handler-case (nlk:decode-json payload) (error () nil)))))

(defun header (headers name)
  "NAME's value among the HTTP HEADERS an exchange answered, or NIL."
  (cdr (assoc name headers :test #'string-equal)))

(defun post (body &optional (session *world*))
  "POST BODY to the world; => (values MESSAGE STATUS HEADERS). A world that
cannot be reached is ISLAND-ERROR naming where it looked."
  (multiple-value-bind (text status headers)
      (handler-case
          (nlk:http :post (setting :world-url) :headers (request-headers session)
                    :content body :timeout (setting :timeout-seconds))
        (error (condition)
          (fail "the world at ~a did not answer (~a); is the game running there?"
                (setting :world-url) condition)))
    (values (decode-answer text) status headers)))

(defun result-of (answer)
  "ANSWER's JSON-RPC result, or ISLAND-ERROR carrying the world's own error."
  (when (and answer (hash-table-p answer))
    (let ((problem (gethash "error" answer)))
      (when (hash-table-p problem)
        (fail "the world refused: ~a"
              (or (nlk:json-value problem :string "message") "an unnamed error")))))
  (and (hash-table-p answer) (gethash "result" answer)))

(defun open-world ()
  "Initialize a session against the world and answer it."
  (setf *world* nil *answers* 0)
  (multiple-value-bind (answer status headers)
      (post (nlk:encode-json-object
             (message "initialize" :id 0
                      :params (object "protocolVersion" +protocol-version+
                                      "capabilities" (object)
                                      "clientInfo" (object "name" +client-name+
                                                            "version" "0.1.0")))))
    (declare (ignore status))
    (let ((result (result-of answer)))
      (setf *world* (list :url (setting :world-url)
                           :id (header headers "mcp-session-id")
                           :protocol (or (nlk:json-value result :string "protocolVersion")
                                         +protocol-version+)))
      (ignore-errors (post (nlk:encode-json-object (message "notifications/initialized"))))
      *world*)))

(defun ask (method &optional params)
  "One request of METHOD, its result object. The session is opened on the
first call, and reopened once when the world has forgotten it."
  (unless (and *world* (equal (setting :world-url) (getf *world* :url)))
    (open-world))
  (let* ((id (incf *answers*))
         (body (nlk:encode-json-object (message method :id id :params params))))
    (multiple-value-bind (answer status) (post body)
      (when (and (member status '(400 404)) (getf *world* :id))
        (open-world)
        (multiple-value-setq (answer status)
          (post (nlk:encode-json-object (message method :id (incf *answers*)
                                                  :params params)))))
      (result-of answer))))

;;; --- what the world's answer says ----------------------------------------------

(defun one-line (text &optional (chars 120))
  "TEXT with its whitespace runs collapsed, cut at CHARS."
  (let ((text (if (stringp text)
                  (format nil "~{~a~^ ~}"
                          (remove "" (uiop:split-string text :separator '(#\Space #\Tab #\Newline #\Return))
                                  :test #'string=))
                  "")))
    (if (> (length text) chars) (format nil "~a..." (subseq text 0 chars)) text)))

(defun content-text (content)
  "CONTENT, the world's content blocks, as one text."
  (let ((text (format nil "~{~a~^~%~}"
                      (loop for block across (or content #())
                            for piece = (and (hash-table-p block)
                                             (nlk:json-value block :string "text"))
                            when piece collect piece))))
    (cond ((zerop (length text)) "(the world answered no text)")
          ((> (length text) +answer-chars+)
           (format nil "~a~%[... ~d more characters]" (subseq text 0 +answer-chars+)
                   (- (length text) +answer-chars+)))
          (t text))))

;;; --- the two verbs -------------------------------------------------------------

(defun call (tool &optional arguments)
  "Call the world's TOOL with ARGUMENTS, a JSON object; answer its text. The
world's own refusal answers as \"ERROR: ...\"."
  (running-settings)
  (let* ((tool (nlk:trimmed (princ-to-string (or tool ""))))
         (result (ask "tools/call" (object "name" tool
                                            "arguments" (or arguments (object)))))
         (text (content-text (nlk:json-value result :array "content"))))
    (if (nlk:json-value result :boolean "isError")
        (format nil "ERROR: ~a" text)
        text)))

(defun tools (&optional filter)
  "The world's tools, one per line: its name, then what it does. FILTER, when
given, keeps only the tools whose name or description names it."
  (running-settings)
  (let* ((result (ask "tools/list" (object)))
         (offered (nlk:json-value result :array "tools"))
         (filter (nlk:trimmed (princ-to-string (or filter ""))))
         (lines (loop for tool across (or offered #())
                      for name = (nlk:json-value tool :string "name")
                      for what = (one-line (nlk:json-value tool :string "description"))
                      when (or (zerop (length filter))
                               (search (string-downcase filter) (string-downcase name))
                               (search (string-downcase filter) (string-downcase what)))
                        collect (format nil "~a~@[ - ~a~]" name what))))
    (if (null lines)
        "the world offers no tool by that name"
        (format nil "~d tool~:p at ~a~%~{~a~%~}" (length lines) (setting :world-url) lines))))

;;; --- the declarations ----------------------------------------------------------

(nlk:define-contract island-call
  (:doc "Call one of the island world's tools: the agent-inception game speaks MCP, and its tools are join, look, board, till, plant, harvest, build, craft, say, journal_read, journal_write and the rest. (island:tools) lists what this world offers. Join before anything else; the answer carries a rejoin key to keep.")
  ("tool" :text :required t
          :doc "the world's tool, as (island:tools) names it")
  ("arguments" :object
               :doc "the tool's own arguments as one JSON object, {} when it takes none; the playbook in (help :island) has the usual ones"))

(nlk:define-contract island-world
  (:doc "List the tools the island world offers right now, with what each one does.")
  ("filter" :string :default ""
            :doc "keep only tools whose name or description contains this text; empty lists every tool"))

(defun call-tool (request)
  "The island tool: forward REQUEST's tool and arguments to the world."
  (call (island-call-tool request) (island-call-arguments request)))

(defun list-tools (request)
  "The island_tools tool: what the world offers, narrowed by REQUEST's filter."
  (tools (island-world-filter request)))

(defun run-slash (args session-id)
  "One /island invocation => the answer's text: where the world is and what it
offers, or the tools themselves with `tools'."
  (declare (ignore session-id))
  (let ((args (string-downcase (nlk:trimmed (or args "")))))
    (handler-case
        (if (string= args "tools")
            (tools)
            (let* ((result (ask "tools/list" (object)))
                   (offered (nlk:json-value result :array "tools"))
                   (names (loop for tool across (or offered #())
                                collect (nlk:json-value tool :string "name"))))
              (format nil "island: ~a answers ~d tool~:p~%~{~a~^, ~}~%ask for a villager's day on the island; (help :island) has the playbook"
                      (setting :world-url) (length names) names)))
      (island-error (condition)
        (format nil "island: ~a" condition)))))

(nle:define-cell island
  (:section ("island")
    (:guide "world_url is the game's MCP endpoint: the local world is http://localhost:5173/mcp, a hosted one https://<link>/mcp (a link is a bearer secret); the world must be running already, this cell never starts it; timeout_seconds bounds one call")
    ("world_url" :string :default "http://localhost:5173/mcp"
                 :doc "the agent-inception world's MCP endpoint")
    ("timeout_seconds" :integer :default 20 :min 1
                        :doc "one exchange with the world's ceiling, in seconds"))
  (:settings (lambda (values table)
               (declare (ignore table))
               (let ((url (string-right-trim "/" (princ-to-string (or (getf values :world-url) "")))))
                 (unless (some (lambda (scheme)
                                 (and (uiop:string-prefix-p scheme url)
                                      (> (length url) (length scheme))))
                               '("http://" "https://"))
                   (nlk:config-error "island.world_url is ~s; a world's endpoint is a web address, http:// or https:// (the local world is http://localhost:5173/mcp)"
                                     (getf values :world-url)))
                 (setf (getf values :world-url) url)
                 values)))
  (:tool "island" (nlk:contract 'island-call) 'call-tool)
  (:tool "island_tools" (nlk:contract 'island-world) 'list-tools)
  (:help :island
         "island: play the agent-inception voxel island over its MCP world - (island:tools) lists what the world offers, (island:call \"look\" ...) calls one"
         *primer*)
  (:command "island" 'run-slash
            :description "The island world: where it is, what it offers; tools lists them"
            :argument-hint "tools")
  (:start (lambda ()
            ;; The section's own reader trims the URL; the check is here as well
            ;; so a blank endpoint refuses the start whatever the section path did.
            (let ((problem (world-url-problem (setting :world-url))))
              (when problem (nlk:config-error "~a" problem)))
            (setf *world* nil *answers* 0))))
