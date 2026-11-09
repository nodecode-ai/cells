;;;; cell.lisp --- the primer, the section, the two tools, /tam.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Config, a sibling top-level key:
;;;;   "tam": {"url": "http://127.0.0.1:3737/mcp/", "token_env": "TAM_API_TOKEN",
;;;;           "project": "nodecode", "recall_limit": 6, "timeout_seconds": 15}
;;;; A TAM or tam-team server is what the cell talks to: the original's remote
;;;; mode, over the same MCP tools. The token is a team server's, read from
;;;; the variable token_env names, never written here.

(in-package #:nodecode-tam)

;;; --- the manual ---------------------------------------------------------------
;;; (help :tam) answers it while the cell runs; a request carries the one
;;; line the :HELP clause below gives, never this text.

(defparameter *primer*
  "TAM (total-agent-memory) is a memory store shared by every agent pointed at it, and
this cell is Nodecode's door to it through two tools and the same calls from eval.
  tam_recall with a query, an optional limit and project
      The memories TAM ranks highest for the query, best first: type, content, project,
      and who wrote it on a team server. Search before asking the operator something
      they may already have told another agent.
  tam_save with content (required), and type, project, tags, importance, context
      Store one durable thing: a fact, decision, solution, lesson or convention, in one
      or two sentences that stand alone out of this conversation. TAM's own quality
      gate may refuse it, and it may answer that the memory was already there.
  (tam:recall query &key limit project) and (tam:save content &key type project tags importance context)
      The same two answers, as text, for the operator who types them.
/tam asks the server what it is (name, version, tools); /tam QUERY recalls.
ERROR: TAM-ERROR names the problem: a server that is not reachable at tam.url, a team
token the variable tam.token_env names, or TAM's own refusal. Say so to the operator;
nothing else in the turn depends on this cell."
  "What (help :tam) explains while the cell runs.")

(defparameter +record-types+ '("fact" "decision" "solution" "lesson" "convention")
  "The record types TAM stores, as the original's tool offered them.")

(defparameter +importance-levels+ '("low" "medium" "high" "critical")
  "How much a memory matters, as the original's tool offered it.")

;;; --- reading the section --------------------------------------------------------

(defun read-settings (values table)
  "The declared VALUES, with the endpoint checked here, once: an http(s) url the
cell can POST to, else a refusal."
  (declare (ignore table))
  (let ((plist (copy-list values))
        (url (nonblank (getf values :url))))
    (unless url
      (nlk:config-error "no url: paste the MCP endpoint of a running TAM server, such as ~
http://127.0.0.1:3737/mcp/, into url"))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (nlk:config-error "the url must be an http:// or https:// MCP endpoint, not ~s" url))
    (setf (getf plist :url) url)
    plist))

;;; --- reading TAM's answers --------------------------------------------------------

(defun score-of (record)
  "RECORD's score as a number, zero when it carries none."
  (let ((score (gethash "score" record)))
    (cond ((numberp score) (coerce score 'single-float))
          ((stringp score) (or (ignore-errors (coerce (read-from-string score) 'single-float)) 0.0))
          (t 0.0))))

(defun tags-of (record)
  "RECORD's tags as a list of strings, whether it sent an array or a JSON text."
  (let ((tags (gethash "tags" record)))
    (cond ((vectorp tags) (coerce tags 'list))
          ((stringp tags)
           (let ((read (ignore-errors (nlk:decode-json tags))))
             (if (vectorp read) (coerce read 'list) '())))
          ((listp tags) tags)
          (t '()))))

(defun author-of (record)
  "Who wrote RECORD on a team server, or the empty string."
  (let ((creator (gethash "created_by" record)))
    (if (hash-table-p creator)
        (or (nonblank (gethash "display_name" creator))
            (nonblank (gethash "user_id" creator))
            "")
        "")))

(defun memory-of (record kind author)
  "One memory of RECORD, KIND its group on a local server, AUTHOR who wrote it."
  (list :content (or (nonblank (gethash "content" record)) "")
        :type (or (nonblank (gethash "type" record)) kind)
        :project (or (nonblank (gethash "project" record)) "")
        :score (score-of record)
        :tags (tags-of record)
        :author author))

(defun memories-of (payload)
  "PAYLOAD as memories, best first: a local server groups them by type, a team
server answers a ranked list of {scope, record}."
  (let ((results (and (hash-table-p payload) (gethash "results" payload))))
    (let ((memories
            (cond ((hash-table-p results)
                   (loop for kind being the hash-keys of results using (hash-value group)
                         when (vectorp group)
                           append (loop for item across group
                                        when (hash-table-p item)
                                          collect (memory-of item kind ""))))
                  ((vectorp results)
                   (loop for item across results
                         for record = (and (hash-table-p item) (gethash "record" item))
                         when (hash-table-p record)
                           collect (memory-of record "fact" (author-of record))))
                  (t '()))))
      (let ((kept (remove "" memories :key (lambda (memory) (getf memory :content)) :test #'string=)))
        (stable-sort kept #'> :key (lambda (memory) (getf memory :score)))))))

(defun memory-line (memory)
  "One memory as a line a model reads."
  (format nil "[~a] ~a~@[ (project ~a)~]~@[ (by ~a)~]"
          (getf memory :type) (getf memory :content)
          (nonblank (getf memory :project)) (nonblank (getf memory :author))))

(defun render-memories (memories &optional (limit (setting :recall-limit)))
  "MEMORIES, at most LIMIT of them, one line each, or a line saying there were none."
  (if (null memories)
      "TAM has no memory answering that yet."
      (format nil "~{~a~^~%~}" (loop for memory in memories
                                       repeat limit
                                       collect (memory-line memory)))))

;;; --- the two verbs ---------------------------------------------------------------

(defun recall-arguments (query project limit)
  "The arguments a recall call carries; the team server refuses the local
server's detail member, which is added here for the local one."
  (lambda (backend)
    (let ((arguments (nlk:json-object "query" query "limit" limit)))
      (when project (setf (gethash "project" arguments) project))
      (when (eq backend :local) (setf (gethash "detail" arguments) "full"))
      arguments)))

(defun recall (query &key limit project)
  "Ask TAM for the memories nearest QUERY, best first, as lines a model reads."
  (let ((query (nonblank query))
        (limit (or limit (setting :recall-limit))))
    (unless query
      (fail "TAM recall needs a query"))
    (multiple-value-bind (payload backend)
        (call-tool "memory_recall" (recall-arguments query (nonblank project) limit))
      (declare (ignore backend))
      (render-memories (memories-of payload) limit))))

(defun record-type (type)
  "TYPE as one of TAM's record types, a fact when the call names none."
  (let ((type (string-downcase (or (nonblank type) "fact"))))
    (if (member type +record-types+ :test #'string=)
        type
        (fail "a memory's type is one of ~{~a~^, ~}, not ~s" +record-types+ type))))

(defun save-arguments (content type project tags importance context)
  "The arguments a save call carries, in the shape the original sent."
  (lambda (backend)
    (declare (ignore backend))
    (nlk:json-object "content" content
                      "type" type
                      "project" project
                      "tags" (coerce tags 'vector)
                      "context" (or context "")
                      "importance" importance
                      "source_format" "auto")))

(defun save (content &key type project tags importance context)
  "Store one durable thing in TAM. => what TAM did with it, as a line."
  (let ((content (nonblank content)))
    (unless content
      (fail "TAM save needs something to store"))
    (multiple-value-bind (payload backend)
        (call-tool "memory_save"
                    (save-arguments content (record-type type)
                                     (or (nonblank project) (setting :project))
                                     (remove nil (mapcar #'nonblank tags))
                                     (or (nonblank importance) "medium")
                                     (nonblank context)))
      (save-reply payload backend))))

(defun save-reply (payload backend)
  "PAYLOAD as the line a model reads: stored, already there, or refused and why."
  (let* ((data (if (and (eq backend :team) (hash-table-p (gethash "data" payload)))
                   (gethash "data" payload)
                   payload))
         (saved (and (hash-table-p data) (gethash "saved" data)))
         (id (and (hash-table-p data) (gethash "id" data)))
         (quality (and (hash-table-p data) (gethash "quality" data)))
         (reason (or (and (hash-table-p quality) (gethash "reason" quality))
                     (and (hash-table-p data) (gethash "reason" data)))))
    (cond ((not (hash-table-p data))
           (format nil "TAM answered nothing this cell could read: ~a" (clipped payload)))
          ((and saved (gethash "deduplicated" data))
           (format nil "already in TAM~@[ as record ~a~]: nothing new was stored" id))
          (saved (format nil "saved to TAM~@[ as record ~a~]" id))
          (t (format nil "TAM kept nothing: ~a"
                     (or (nonblank reason) "its quality gate rejected the memory"))))))

;;; --- the connection -------------------------------------------------------------

(defun status ()
  "What TAM answers this cell: its name, version, which server it is, its tools."
  (multiple-value-bind (session server version tools) (open-session)
    (declare (ignore session))
    (format nil "TAM connected at ~a: ~a ~a (~a server), tools ~{~a~^, ~}"
            (endpoint) (or (nonblank server) "TAM") (or (nonblank version) "?")
            (if (eq (backend-of tools) :team) "team" "local")
            tools)))

(defun run-slash (args session-id)
  "/tam: what the server is; /tam QUERY recalls what TAM holds about it."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (if (zerop (length text)) (status) (recall text))))

;;; --- the tools the model calls -----------------------------------------------------

(defun recall-tool (arguments)
  "tam_recall: the memories nearest the query, best first."
  (recall (or (nlk:json-value arguments :string "query") "")
          :limit (nlk:json-value arguments :integer "limit")
          :project (nlk:json-value arguments :string "project")))

(defun save-tool (arguments)
  "tam_save: one durable thing into TAM."
  (let ((tags (gethash "tags" arguments)))
    (save (or (nlk:json-value arguments :string "content") "")
          :type (nlk:json-value arguments :string "type")
          :project (nlk:json-value arguments :string "project")
          :tags (and (vectorp tags) (coerce tags 'list))
          :importance (nlk:json-value arguments :string "importance")
          :context (nlk:json-value arguments :string "context"))))

(defparameter +recall-schema+
  (list :description "Search the TAM memory store shared with every other agent pointed at it: the memories nearest the query, best first."
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "query" (nlk:json-object "type" "string"
                                                             "description" "what to look for, in words")
                                   "limit" (nlk:json-object "type" "integer"
                                                             "description" "how many memories at most; the section's recall_limit answers when the call names none")
                                   "project" (nlk:json-object "type" "string"
                                                               "description" "one TAM project to search; empty searches every project"))
                     "required" (vector "query")))
  "tam_recall as the model sees it.")

(defparameter +save-schema+
  (list :description "Store one durable thing in the TAM memory store every agent shares: a fact, decision, solution, lesson or convention, in one or two sentences that stand alone out of this conversation."
        :parameters (nlk:json-object
                     "type" "object"
                     "properties" (nlk:json-object
                                   "content" (nlk:json-object "type" "string"
                                                               "description" "the memory, one or two self-contained sentences")
                                   "type" (nlk:json-object "type" "string" "enum" (coerce +record-types+ 'vector)
                                                            "description" "what kind of thing it is; fact when the call names none")
                                   "project" (nlk:json-object "type" "string"
                                                               "description" "the TAM project it belongs to; the section's project answers when the call names none")
                                   "tags" (nlk:json-object "type" "array"
                                                            "items" (nlk:json-object "type" "string")
                                                            "description" "a few lowercase words it can be found by")
                                   "importance" (nlk:json-object "type" "string" "enum" (coerce +importance-levels+ 'vector)
                                                                  "description" "how much it matters; medium when the call names none")
                                   "context" (nlk:json-object "type" "string"
                                                               "description" "where it came from, one line, optional"))
                     "required" (vector "content")))
  "tam_save as the model sees it.")

;;; --- the declaration ---------------------------------------------------------------

(nle:define-cell tam
  (:section ("tam")
    (:guide "run a TAM server and paste its MCP endpoint into url: a local one serves http://127.0.0.1:3737/mcp/, a team server asks for the token in the variable token_env; project is where memories saved without one land, recall_limit how many a call naming none gets back")
    ("url" :string
           :doc "the MCP endpoint of a running TAM server, such as http://127.0.0.1:3737/mcp/")
    ("token_env" :env :default "TAM_API_TOKEN"
                 :doc "the environment variable holding a team server's bearer token; unset for a local server")
    ("project" :string :default "nodecode"
               :doc "the TAM project a save that names none belongs to")
    ("recall_limit" :integer :default 6 :min 1
                    :doc "how many memories a recall that names no limit gets back")
    ("timeout_seconds" :integer :default 15 :min 1
                       :doc "seconds one exchange with TAM may take"))
  (:settings #'read-settings)
  (:tool "tam_recall" +recall-schema+ #'recall-tool)
  (:tool "tam_save" +save-schema+ #'save-tool)
  (:help :tam
   "tam: recall and save memories in a TAM store shared with every other agent - tam_recall, tam_save, /tam"
   *primer*)
  (:command "tam" 'run-slash
            :catches 'tam-error
            :description "What the TAM server is; with a query, what it remembers about it"
            :argument-hint "QUERY"))
