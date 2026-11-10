;;;; cell-test.lisp --- the cell against a stubbed AgentStatus MCP Index.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing leaves the machine and no
;;;; index is asked. What is proved is the cell's own wiring: the section (a
;;;; start with no section at all, and one that must refuse), the seven tools
;;;; through the dispatch the model calls them by -- the MCP method each one
;;;; names, the arguments it sends, the session id on the wire, the index's own
;;;; catalog read once and kept -- and the /agentstatus-index command.

(in-package #:nodecode.test)

(define-test-slice "agentstatus-index" "AGENTSTATUS-INDEX-CELL-"
  :start nodecode-agentstatus-index:start-cell)

;;; --- fixtures --------------------------------------------------------------------

(defparameter *calls* nil
  "Every request the stub answered, newest first, as (METHOD PARAMS HEADERS).")

(defparameter *answer* nil
  "When set, (TEXT STATUS HEADERS): what the stub answers to tools/call.")

(defparameter *catalog-spec*
  '(("search_records" ("query") ("query" "host"))
    ("recommend_records" ("task") ("task" "host"))
    ("get_record" ("slug") ("slug"))
    ("compare_records" ("slugs") ("slugs"))
    ("list_sections" () ())
    ("call_indexed_tool" ("slug" "tool") ("slug" "tool" "arguments"))) "The tools the stub's index lists, as (NAME REQUIRED PROPERTIES).")

(defun envelope (id result) "One JSON-RPC answer carrying RESULT, as the wire spells it."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" id "result" result)))

(defun text-result (text &key errorp) "One tools/call result carrying TEXT."
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text)) "isError" (and errorp t)))

(defun schema-object (spec) "One catalog entry's inputSchema, from its SPEC."
  (destructuring-bind (name required properties) spec
    (declare (ignore name))
    (nlk:json-object
     "type" "object"
     "required" (coerce required 'vector) "properties" (nodecode-agentstatus-index::json-object-of
                   (loop for property in properties
                         collect (cons property (nlk:json-object "type" "string")))))))

(defun catalog-vector () "The tools the stub's tools/list answers."
  (coerce (loop for spec in *catalog-spec*
                collect (nlk:json-object
                         "name" (first spec) "description" (format nil "~a: as the index describes it" (first spec)) "inputSchema" (schema-object spec)))
          'vector))

(defun call-result (params) "What the stub's tools/call answers: the tool's name and the arguments back."
  (let ((tool (nlk:json-value params :string "name"))
        (arguments (nlk:json-value params :object "arguments")))
    (text-result (format nil "~a answered~@[ ~a~]" tool
                         (and arguments (nlk:encode-json-object arguments))))))

(defun session-headers ()
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "mcp-session-id" headers) "sess-1")
    headers))

(defun stub-wire (options) "NLK:HTTP, answering from the request's own method; nothing leaves the machine."
  (let* ((body (nlk:decode-json (or (getf options :content) "{}")))
         (name (nlk:json-value body :text "method"))
         (params (nlk:json-value body :object "params"))
         (id (nlk:json-value body :integer "id")))
    (push (list name params (getf options :headers)) *calls*)
    (cond ((equal name "initialize")
           (values (envelope id (nlk:json-object "protocolVersion" "2025-06-18"
                                                   "capabilities" (nlk:make-json-object)))
                   200 (session-headers) nil))
          ((equal name "notifications/initialized")
           (values "" 202 nil nil))
          ((equal name "tools/list")
           (values (envelope id (nlk:json-object "tools" (catalog-vector))) 200 nil nil))
          ((equal name "tools/call")
           (if *answer*
               (values (first *answer*) (second *answer*) (third *answer*) nil)
               (values (envelope id (call-result params)) 200 nil nil)))
          (t (values "{}" 404 nil nil)))))

(defmacro with-index-wire (&body body) "BODY with NLK:HTTP stubbed: every request is logged and answered here."
  `(let ((*calls* '())
         (*answer* nil))
     (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                (declare (ignore method url))
                                (stub-wire options))
       ,@body)))

(defmacro with-index-running (&body body) "BODY with the cell started on a section naming only the index, stopped on unwind."
  `(with-cell-stop ((agentstatus-index-start))
     ,@body))

(defun index-request (method) "The newest stubbed request of JSON-RPC METHOD, or NIL."
  (find method *calls* :test #'equal :key #'first))

(defun index-request-header (request name) "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (third request) :test #'string-equal)))

(defun index-tool-call (&optional (index 0)) "The INDEX'th newest tools/call the cell sent, as (METHOD PARAMS HEADERS)."
  (nth index (remove-if-not (lambda (call) (equal (first call) "tools/call")) *calls*)))

(defun index-sent-tool (&optional (index 0)) "The MCP tool name the INDEX'th newest tools/call named."
  (let ((call (index-tool-call index)))
    (and call (nlk:json-value (second call) :string "name"))))

(defun index-sent-arguments (&optional (index 0)) "The arguments object of the INDEX'th newest tools/call."
  (let ((call (index-tool-call index)))
    (and call (nlk:json-value (second call) :object "arguments"))))

(defun index-called (name arguments) "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "agentstatus-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "agentstatus-index"
  (:fixture with-index-wire)
  (:help :agentstatus-index)
  (:command "agentstatus-index")
  (:running
   (is (nle::find-tool "agentstatus_search") "the search tool is registered")
   (is (nle::find-tool "agentstatus_recommend") "and the recommendation")
   (is (nle::find-tool "agentstatus_record") "and one record")
   (is (nle::find-tool "agentstatus_compare") "and the comparison")
   (is (nle::find-tool "agentstatus_sections") "and the section list")
   (is (nle::find-tool "agentstatus_call") "and one call on a listed server")
   (is (nle::find-tool "agentstatus_catalog") "and the index's own catalog")
   (is (= 7 (count-if (lambda (entry) (eql 0 (search "agentstatus_" (first entry))))
                      nle::*tools*)) "seven of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "agentstatus_search")) "and stop takes every one back")
   (is (null (nle::find-tool "agentstatus_catalog"))))
  (:refused ("server_url" "ftp://example.test/mcp")
            ("timeout_seconds" 0)))

(deftest agentstatus-index-cell-no-section-at-all-is-the-public-index (with-index-wire)
  ;; Nothing is needed to reach this index, so a fresh install with no section
  ;; must answer, on its default address.
  (with-index-running
    (is (equal "https://index.mcp.agentstatus.dev/mcp"
               (nodecode-agentstatus-index::setting :server-url)) "the section defaults to the public index")
    (is (nle::find-tool "agentstatus_search") "and the tools are registered")))

;;; --- the wire ---------------------------------------------------------------------

(deftest agentstatus-index-cell-one-session-serves-every-call (with-index-wire)
  (with-index-running
    (index-called "agentstatus_sections" "{}")
    (index-called "agentstatus_sections" "{}")
    (is (= 1 (count-if (lambda (call) (equal (first call) "initialize")) *calls*)) "initialize happens once and is reused")
    (is (equal "sess-1" (index-request-header (index-tool-call) "mcp-session-id")) "and the session id the server handed is echoed")
    (is (equal "application/json, text/event-stream"
               (index-request-header (index-tool-call) "accept")) "an answer may arrive as a stream")))

(deftest agentstatus-index-cell-an-error-the-index-reports-fails-the-call (with-index-wire)
  (with-index-running
    (setf *answer* (list (envelope 1 (text-result "no such slug" :errorp t)) 200 nil))
    (let ((answer (index-called "agentstatus_sections" "{}")))
      (is (search "no such slug" answer) "the index's own words reach the model")
      (is (search "ERROR" answer) "as a failed call, not a result"))))

(deftest agentstatus-index-cell-a-dead-index-says-so (with-index-wire)
  (with-index-running
    (setf *answer* (list "{}" 500 nil))
    (let ((answer (index-called "agentstatus_sections" "{}")))
      (is (search "HTTP 500" answer) "a status the index did not mean is named"))))

;;; --- the tools say what the skill said ----------------------------------------------

(deftest agentstatus-index-cell-search-sends-the-query (with-index-wire)
  (with-index-running
    (index-called "agentstatus_search" "{\"query\": \"pdf forms\"}")
    (is (equal "search_records" (index-sent-tool)) "the index's own search")
    (is (equal "pdf forms" (nlk:json-value (index-sent-arguments) :text "query")))
    (is (null (nlk:json-value (index-sent-arguments) :text "host")) "and no host when the person named none")))

(deftest agentstatus-index-cell-search-narrows-to-the-client (with-index-wire)
  (with-index-running
    (index-called "agentstatus_search" "{\"query\": \"pdf\", \"host\": \"cursor\"}")
    (is (equal "cursor" (nlk:json-value (index-sent-arguments) :text "host")) "a host the person named goes through")))

(deftest agentstatus-index-cell-recommend-sends-the-job-in-words (with-index-wire)
  (with-index-running
    (index-called "agentstatus_recommend" "{\"task\": \"summarise a mailbox\"}")
    (is (equal "recommend_records" (index-sent-tool)))
    (is (equal "summarise a mailbox" (nlk:json-value (index-sent-arguments) :text "task")))))

(deftest agentstatus-index-cell-one-record-and-a-comparison (with-index-wire)
  (with-index-running
    (index-called "agentstatus_record" "{\"slug\": \"acme-search\"}")
    (is (equal "get_record" (index-sent-tool)))
    (is (equal "acme-search" (nlk:json-value (index-sent-arguments) :text "slug")))
    (index-called "agentstatus_compare" "{\"slugs\": [\"acme-search\", \"other-fetch\"]}")
    (is (equal "compare_records" (index-sent-tool)))
    (is (= 2 (length (nlk:json-array (index-sent-arguments) "slugs"))))))

(deftest agentstatus-index-cell-a-comparison-of-one-refuses (with-index-wire)
  (with-index-running
    (let ((answer (index-called "agentstatus_compare" "{\"slugs\": [\"acme-search\"]}")))
      (is (search "two to five" answer) "the index compares two to five")
      (is (null (index-tool-call)) "and nothing was asked of it"))))

(deftest agentstatus-index-cell-call-forwards-one-call-to-a-listed-server (with-index-wire)
  (with-index-running
    (index-called "agentstatus_call"
                  "{\"slug\": \"acme-search\", \"tool\": \"search\", \"arguments\": {\"q\": \"forms\"}}")
    (is (equal "call_indexed_tool" (index-sent-tool)))
    (let ((sent (index-sent-arguments)))
      (is (equal "acme-search" (nlk:json-value sent :text "slug")))
      (is (equal "search" (nlk:json-value sent :text "tool")))
      (is (equal "forms" (nlk:json-value (nlk:json-value sent :object "arguments") :text "q")) "the listed tool's own arguments, untouched"))))

(deftest agentstatus-index-cell-a-call-without-its-tool-is-refused (with-index-wire)
  (with-index-running
    (let ((answer (index-called "agentstatus_call" "{\"slug\": \"acme-search\"}")))
      (is (search "tool" answer) "the member a call cannot go without is named")
      (is (null (index-tool-call)) "and nothing was sent"))
    (let ((verb (nodecode-agentstatus-index:call-indexed
                 (nlk:decode-json "{\"slug\": \"acme-search\"}"))))
      (is (search "tool is required" verb) "the verb itself says which member is missing"))))

(deftest agentstatus-index-cell-the-indexs-own-catalog-can-refuse-a-call (with-index-wire)
  ;; The index's tools/list is the authority on what a call may carry: a member
  ;; it wants and this cell did not send is named back to the model.
  (with-index-running
    (let ((*catalog-spec*
            (mapcar (lambda (spec)
                      (if (equal (first spec) "search_records")
                          (list "search_records" '("q") '("q" "host"))
                          spec))
                    *catalog-spec*)))
      (let ((answer (index-called "agentstatus_search" "{\"query\": \"pdf\"}")))
        (is (search "needs q" answer) "the member the index wants is named")
        (is (search "host" answer) "beside the ones it accepts")
        (is (null (index-tool-call)) "and nothing was sent")))))

(deftest agentstatus-index-cell-a-tool-the-index-does-not-list-is-refused (with-index-wire)
  (with-index-running
    (let* ((*catalog-spec* (remove "list_sections" *catalog-spec*
                                    :test #'equal :key #'first))
           (nodecode-agentstatus-index::*catalog* nil)
           (answer (index-called "agentstatus_sections" "{}")))
      (is (search "lists no tool named list_sections" answer) "the refusal says which")
      (is (search "search_records" answer) "and what it does list"))))

;;; --- the operator's command ----------------------------------------------------------

(deftest agentstatus-index-cell-the-command-lists-the-indexs-tools (with-index-wire)
  (with-index-running
    (let ((answer (nodecode-agentstatus-index::run-command "" nil)))
      (is (search "index.mcp.agentstatus.dev" answer) "no argument names the index it talks to")
      (is (search "search_records" answer) "and the tools that index lists")
      (is (search "call_indexed_tool" answer) "including the one that forwards a call"))))

(deftest agentstatus-index-cell-the-command-searches-what-was-typed (with-index-wire)
  (with-index-running
    (nodecode-agentstatus-index::run-command "pdf" nil)
    (is (equal "search_records" (index-sent-tool)))
    (is (equal "pdf" (nlk:json-value (index-sent-arguments) :text "query")))))

(deftest agentstatus-index-cell-the-command-asks-for-the-sections (with-index-wire)
  (with-index-running
    (nodecode-agentstatus-index::run-command "sections" nil)
    (is (equal "list_sections" (index-sent-tool)) "one word asks the index for its sections")))
