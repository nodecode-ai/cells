;;;; cell-test.lisp --- lifecycle, the four tools, /kumiho, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a stubbed wire: the
;;;; section (a healthy start, a missing key, a wrong URL and an integer under
;;;; its floor refused), the MCP handshake once and the session id echoed
;;;; after it, each tool's JSON-RPC frame and the arguments it maps to, an SSE
;;;; answer read as well as a JSON one, an error answer, and /kumiho with and
;;;; without a query. Every request is a stubbed NLK:HTTP, so nothing reaches
;;;; Kumiho or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS
=> (values TEXT STATUS HEADERS).")

(defmacro with-kumiho-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Kumiho."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected Kumiho request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun kumiho-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun kumiho-frame (request)
  "REQUEST's JSON-RPC frame, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun kumiho-method (request)
  (nlk:json-value (kumiho-frame request) :string "method"))

(defun kumiho-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun kumiho-headers (&key (content-type "application/json") (session "sess-1"))
  "An answer's header table, the way dexador builds one."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "content-type" table) content-type)
    (when session (setf (gethash "mcp-session-id" table) session))
    table))

(defun kumiho-tool-calls ()
  "The tools/call frames among the stubbed requests, oldest first."
  (remove-if-not (lambda (request) (equal "tools/call" (kumiho-method request)))
                 (kumiho-asked)))

(defun kumiho-called ()
  "The newest tools/call frame's params, decoded."
  (nlk:json-value (kumiho-frame (first (kumiho-tool-calls))) :object "params"))

(defun kumiho-arguments ()
  "The newest tools/call's arguments, decoded."
  (nlk:json-value (kumiho-called) :object "arguments"))

(defun kumiho-arguments-of (index)
  "The INDEXth tools/call's arguments, oldest first."
  (nlk:json-value (nlk:json-value (kumiho-frame (nth index (kumiho-tool-calls))) :object "params")
                   :object "arguments"))

(defun tool-result (text &key is-error)
  (nlk:json-object "content" (vector (nlk:json-object "type" "text" "text" text))
                    "isError" is-error))

(defun rpc-reply (frame result)
  (nlk:encode-json-object (nlk:json-object "jsonrpc" "2.0"
                                            "id" (nlk:json-value frame :integer "id")
                                            "result" result)))

(defun kumiho-wire (&key (text "a memory") (is-error nil))
  "Answer the handshake, the initialized notification and every tools/call:
the calls answer TEXT."
  (setf *answer-fn*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((frame (nlk:decode-json (getf args :content)))
                 (name (nlk:json-value frame :string "method")))
            (cond
              ((equal name "initialize")
               (values (rpc-reply frame (nlk:json-object "protocolVersion" "2025-06-18"
                                                          "capabilities" (nlk:json-object)))
                       200 (kumiho-headers)))
              ((equal name "notifications/initialized")
               (values "" 202 (kumiho-headers :session nil)))
              (t (values (rpc-reply frame (tool-result text :is-error is-error))
                         200 (kumiho-headers))))))))

(defun kumiho-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "kumiho-memory" "KUMIHO-MEMORY-CELL-")

(defun kumiho-memory-config (&rest pairs)
  "A whole shared config with one `kumiho' section built from PAIRS."
  (nlk:json-object "kumiho" (apply #'nlk:make-json-object pairs)))

(defun kumiho-memory-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-kumiho-memory:start-cell (apply #'kumiho-memory-config pairs)))

(define-cell-lifecycle-tests "kumiho-memory"
  (:fixture with-kumiho-wire)
  (:config "api_key" "k-test")
  (:help :kumiho)
  (:command "kumiho")
  (:running (is (nle::find-tool "kumiho_recall") "kumiho_recall is registered")
            (is (nle::find-tool "kumiho_latest") "kumiho_latest is registered")
            (is (nle::find-tool "kumiho_remember") "kumiho_remember is registered")
            (is (nle::find-tool "kumiho_forget") "kumiho_forget is registered")
            (is (nle::find-tool "kumiho_spaces") "kumiho_spaces is registered")
            (is (nle::find-tool "kumiho_one_memory") "kumiho_one_memory is registered")
            (is (nle::find-tool "kumiho_summary") "kumiho_summary is registered")
            (is (equal "k-test" (nodecode-kumiho-memory::setting :key)) "the key the section wrote")
            (is (= 30 (nodecode-kumiho-memory::setting :timeout-seconds)) "the default ceiling")
            (is (equal "https://mcp.kumiho.cloud/mcp" (nodecode-kumiho-memory::setting :mcp-url)) "the default endpoint"))
  (:stopped (is (null (nle::find-tool "kumiho_recall")) "stop unregisters the tools")
            (is (null (nle::find-tool "kumiho_forget")) "all of them"))
  (:refused ("timeout_seconds" 0) ("mcp_url" "mcp.kumiho.cloud/mcp"))
  (:idle nodecode-kumiho-memory:kumiho-memory-error
         (nodecode-kumiho-memory::recall-memories "anything")))

(deftest kumiho-memory-cell-a-missing-key-refuses-the-start ()
  (with-kumiho-wire
    (with-stubbed-fdefinition (uiop:getenv (variable) (and variable nil))
      (is-carrying (answer (refusal-text nlk:config-refusal (kumiho-memory-start)))
                   ("no key") ("api_key" "the refusal names what to set"))
      (is (null (nle::find-tool "kumiho_recall")) "a refused start installed nothing"))))

(deftest kumiho-memory-cell-the-key-may-come-from-the-environment ()
  (with-kumiho-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                               (cond ((string= name "KUMIHO_HOME_KEY") "k-env")
                                     ((string= name "KUMIHO_API_KEY") "k-fallback")
                                     (t nil)))
      (with-cell-stop ((kumiho-memory-start "api_key_env" "KUMIHO_HOME_KEY"))
        (is (equal "k-env" (nodecode-kumiho-memory::setting :key)) "api_key_env names the variable"))
      (with-cell-stop ((kumiho-memory-start))
        (is (equal "k-fallback" (nodecode-kumiho-memory::setting :key)) "KUMIHO_API_KEY is read either way")))))

;;; --- the handshake ---------------------------------------------------------------

(deftest kumiho-memory-cell-one-handshake-then-the-session-id-on-every-frame ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (kumiho-wire)
      (kumiho-tool "kumiho_latest" (nlk:json-object))
      (kumiho-tool "kumiho_latest" (nlk:json-object))
      (let ((asked (kumiho-asked)))
        (is (equal '("initialize" "notifications/initialized" "tools/call" "tools/call") (mapcar #'kumiho-method asked)) "one handshake, then a call a request")
        (is (null (kumiho-header (first asked) "mcp-session-id")) "the handshake carries no session")
        (is (equal "sess-1" (kumiho-header (third asked) "mcp-session-id")) "every later frame echoes the session id")
        (is (equal "2025-06-18" (kumiho-header (third asked) "mcp-protocol-version")) "and the agreed revision")
        (is (equal "k-test" (kumiho-header (third asked) "x-api-key")) "and the key, in x-api-key")))))

;;; --- kumiho_recall ---------------------------------------------------------------

(deftest kumiho-memory-cell-recall-asks-engage-and-keeps-the-answer ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (kumiho-wire :text "Weekly planning is on Monday (kref://CognitiveMemory/decisions/planning).")
      (let ((answer (kumiho-tool "kumiho_recall"
                                  (nlk:json-object "query" "weekly planning meeting"
                                                   "limit" 3
                                                   "space" "/CognitiveMemory/decisions"
                                                   "types" (vector "decision")))))
        (is (search "Weekly planning is on Monday" answer) "the server's text comes back whole")
        (let ((params (kumiho-called))
              (arguments (kumiho-arguments)))
          (is (equal "kumiho_memory_engage" (nlk:json-value params :string "name")) "recall is the original's engage tool")
          (is (equal "weekly planning meeting" (nlk:json-value arguments :string "query")) "the query goes as query")
          (is (= 3 (nlk:json-value arguments :integer "limit")) "the limit as a number")
          (is (equal '("/CognitiveMemory/decisions") (coerce (nlk:json-array arguments "space_paths") 'list)) "space goes as space_paths")
          (is (equal '("decision") (coerce (nlk:json-array arguments "memory_types") 'list)) "types go as memory_types"))))))

(deftest kumiho-memory-cell-an-optional-argument-left-out-leaves-the-member-out ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (kumiho-wire)
      (kumiho-tool "kumiho_latest" (nlk:json-object))
      (let ((arguments (kumiho-arguments)))
        (is (equal "latest" (nlk:json-value arguments :string "mode")) "latest is the retrieve tool in latest mode")
        (is (null (nlk:json-value arguments :integer "limit")) "no limit was asked for")
        (is (zerop (length (nlk:json-array arguments "space_paths"))) "and no space")
        (is (zerop (length (nlk:json-array arguments "memory_types"))) "and no types")))))

;;; --- kumiho_remember -------------------------------------------------------------

(deftest kumiho-memory-cell-remember-sends-one-typed-capture ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (kumiho-wire :text "stored kref://CognitiveMemory/decisions/planning?r=1")
      (let ((answer (kumiho-tool "kumiho_remember"
                                  (nlk:json-object "title" "Weekly planning moved to Monday"
                                                   "content" "Everyone can attend on Monday."
                                                   "type" "decision"
                                                   "space" "/CognitiveMemory/decisions"
                                                   "revises" "kref://CognitiveMemory/decisions/planning"))))
        (is (search "stored kref://" answer) "the stored reference comes back")
        (let* ((params (kumiho-called))
               (arguments (kumiho-arguments))
               (captures (nlk:json-array arguments "captures"))
               (capture (and (plusp (length captures)) (aref captures 0))))
          (is (equal "kumiho_memory_reflect" (nlk:json-value params :string "name")) "remember is the original's reflect tool")
          (is (= 1 (length captures)) "one capture")
          (is (equal "decision" (nlk:json-value capture :string "type")) "its type")
          (is (equal "Weekly planning moved to Monday" (nlk:json-value capture :string "title")) "its title")
          (is (equal "/CognitiveMemory/decisions" (nlk:json-value capture :string "space_hint")) "the space goes as space_hint")
          (is (equal "kref://CognitiveMemory/decisions/planning" (nlk:json-value capture :string "revises")) "a correction names the memory it revises"))))))

;;; --- kumiho_forget, and the answers that are not results ---------------------------

(deftest kumiho-memory-cell-forget-retires-by-item-kref-and-reports-an-error ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (kumiho-wire :text "retired; it can be restored")
      (let ((kref "kref://CognitiveMemory/decisions/planning"))
        (is (search "retired" (kumiho-tool "kumiho_forget" (nlk:json-object "item_kref" kref))) "the retirement reads back"))
      (is (equal "kumiho_deprecate_item" (nlk:json-value (kumiho-called) :string "name")) "forget is the original's deprecate_item tool")
      (is (equal "kref://CognitiveMemory/decisions/planning" (nlk:json-value (kumiho-arguments) :string "item_kref")) "by item_kref")
      (kumiho-wire :text "no workspace" :is-error t)
      (is (search "ERROR: no workspace" (kumiho-tool "kumiho_latest" (nlk:json-object))) "an isError result reads as an error"))))

(deftest kumiho-memory-cell-a-server-error-becomes-the-answer ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore method url))
              (let* ((frame (nlk:decode-json (getf args :content)))
                     (name (nlk:json-value frame :string "method")))
                (cond
                  ((equal name "initialize")
                   (values (rpc-reply frame (nlk:json-object "protocolVersion" "2025-06-18"))
                           200 (kumiho-headers)))
                  ((equal name "notifications/initialized")
                   (values "" 202 (kumiho-headers :session nil)))
                  (t (values (nlk:encode-json-object
                               (nlk:json-object "jsonrpc" "2.0"
                                                "id" (nlk:json-value frame :integer "id")
                                                "error" (nlk:json-object "code" -32000
                                                                          "message" "memory_disabled")))
                              200 (kumiho-headers)))))))
      (is-carrying (answer (kumiho-tool "kumiho_recall" (nlk:json-object "query" "anything")))
                   ("ERROR:" "the refusal reads as an error")
                   ("memory_disabled" "the server's own words")))))

(deftest kumiho-memory-cell-an-sse-answer-is-read-too ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore method url))
              (let* ((frame (nlk:decode-json (getf args :content)))
                     (name (nlk:json-value frame :string "method")))
                (cond
                  ((equal name "initialize")
                   (values (rpc-reply frame (nlk:json-object "protocolVersion" "2025-06-18"))
                           200 (kumiho-headers)))
                  ((equal name "notifications/initialized")
                   (values "" 202 (kumiho-headers :session nil)))
                  (t (values (format nil "event: message~%data: ~a~%~%"
                                      (rpc-reply frame (tool-result "from the stream")))
                             200 (kumiho-headers :content-type "text/event-stream")))))))
      (is (search "from the stream" (kumiho-tool "kumiho_latest" (nlk:json-object))) "a data: frame carries the result"))))

;;; --- kumiho_spaces, kumiho_one_memory and the session_required retry --------------

(deftest kumiho-memory-cell-spaces-lists-what-a-save-can-name ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (kumiho-wire :text "/CognitiveMemory/decisions, /CognitiveMemory/preferences")
      (is (search "/CognitiveMemory/preferences"
                  (kumiho-tool "kumiho_spaces" (nlk:json-object)))
          "the spaces come back as the workspace lists them")
      (let* ((params (kumiho-called))
             (arguments (nlk:json-value params :object "arguments")))
        (is (equal "kumiho_get_spaces" (nlk:json-value params :string "name"))
            "spaces is the original's get_spaces tool")
        (is (null (nlk:json-value arguments :string "project_kref"))
            "no project named, none sent")))))

(deftest kumiho-memory-cell-one-memory-reads-the-latest-tag-by-default ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (kumiho-wire :text "the full text of the decision")
      (is (search "full text"
                  (kumiho-tool "kumiho_one_memory"
                               (nlk:json-object "item_kref" "kref://CognitiveMemory/decisions/planning")))
          "the revision's text comes back")
      (let* ((params (kumiho-called))
             (arguments (nlk:json-value params :object "arguments")))
        (is (equal "kumiho_get_revision_by_tag" (nlk:json-value params :string "name"))
            "one memory is the original's get_revision_by_tag tool")
        (is (equal "latest" (nlk:json-value arguments :string "tag"))
            "tag latest, which every item has")
        (is (equal "kref://CognitiveMemory/decisions/planning"
                   (nlk:json-value arguments :string "item_kref"))
            "by the item kref"))
      (kumiho-tool "kumiho_one_memory"
                    (nlk:json-object "item_kref" "kref://CognitiveMemory/decisions/planning"
                                     "tag" "r=2"))
      (is (equal "r=2" (nlk:json-value (kumiho-arguments-of 1) :string "tag"))
          "a named tag is what is asked for"))))

(defun kumiho-session-wire ()
  "Answer the handshake, then session_required to a buffer call that carries no
session_id, and a save to the retry that does -- as the hosted server behaves."
  (setf *answer-fn*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((frame (nlk:decode-json (getf args :content)))
                 (name (nlk:json-value frame :string "method"))
                 (arguments (nlk:json-value (nlk:json-value frame :object "params")
                                             :object "arguments")))
            (cond ((equal name "initialize")
                   (values (rpc-reply frame (nlk:json-object "protocolVersion" "2025-06-18"))
                           200 (kumiho-headers)))
                  ((equal name "notifications/initialized")
                   (values "" 202 (kumiho-headers :session nil)))
                  ((null (nlk:json-value arguments :string "session_id"))
                   (values (rpc-reply frame
                                      (tool-result (nlk:encode-json-object
                                                    (nlk:json-object "error" "session_required"
                                                                     "session_id" "km1_abc_001"))
                                                   :is-error t))
                           200 (kumiho-headers)))
                  (t (values (rpc-reply frame
                                         (tool-result "stored, kref://CognitiveMemory/decisions/planning?r=1"))
                             200 (kumiho-headers))))))))

(deftest kumiho-memory-cell-a-save-retries-once-with-the-session-it-is-issued ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (kumiho-session-wire)
      (let ((answer (kumiho-tool "kumiho_remember"
                                  (nlk:json-object "title" "Planning on Monday"
                                                   "content" "Everyone can attend then."
                                                   "type" "decision"))))
        (is (search "stored" answer) "the retry's save is what the model reads")
        (is (= 2 (length (kumiho-tool-calls))) "one refused call, then the same call again")
        (is (null (nlk:json-value (kumiho-arguments-of 0) :string "session_id"))
            "the first ask carries no session id, as the original's skills do")
        (let ((retried (kumiho-arguments-of 1)))
          (is (equal "km1_abc_001" (nlk:json-value retried :string "session_id"))
              "the retry carries the id the server issued, not an invented one")
          (is (equal "Planning on Monday" (nlk:json-value retried :string "response"))
              "and is the same call, not a second memory")))
      (kumiho-wire :text "stored again")
      (kumiho-tool "kumiho_remember"
                    (nlk:json-object "title" "Another" "content" "Second save." "type" "fact"))
      (is (equal "km1_abc_001" (nlk:json-value (kumiho-arguments-of 2) :string "session_id"))
          "and the id is kept for the conversation's later saves"))))

(deftest kumiho-memory-cell-summary-consolidates-with-the-session ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (setf nodecode-kumiho-memory::*buffer-session* "km1_abc_002")
      (kumiho-wire :text "{\"success\": true}")
      (let ((answer (kumiho-tool "kumiho_summary" (nlk:json-object "summary" "We planned Monday."))))
        (is (search "success" answer) "the workspace's answer comes back")
        (let* ((params (kumiho-called))
               (arguments (nlk:json-value params :object "arguments")))
          (is (equal "kumiho_memory_consolidate" (nlk:json-value params :string "name"))
              "a summary is the original's consolidate tool")
          (is (equal "We planned Monday." (nlk:json-value arguments :string "summary"))
              "with the summary the model wrote, so no server-side model is needed")
          (is (equal "km1_abc_002" (nlk:json-value arguments :string "session_id"))
              "and this conversation's id"))))))

;;; --- /kumiho --------------------------------------------------------------------

(deftest kumiho-memory-cell-slash-says-where-it-points-and-recalls-with-a-query ()
  (with-kumiho-wire
    (with-cell-stop ((kumiho-memory-start "api_key" "k-test"))
      (let ((status (nodecode-kumiho-memory::run-command "" nil)))
        (is (search "https://mcp.kumiho.cloud/mcp" status) "the endpoint is named")
        (is (null (kumiho-asked)) "and nothing was asked over the wire"))
      (kumiho-wire :text "the Monday decision")
      (is (search "the Monday decision" (nodecode-kumiho-memory::run-command "  planning  " nil)) "a query is a recall")
      (is (equal "planning" (nlk:json-value (kumiho-arguments) :string "query")) "trimmed, as typed"))))
