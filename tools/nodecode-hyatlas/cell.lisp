;;;; cell.lisp --- HyAtlas memory: four tools, /hyatlas, recall before a turn, capture after it.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of tuancookiez-hub's HyAtlas-Memory Hermes plugin (see NOTICE).
;;;; HyAtlas is a separate program: a Go server (hyatlas-go) on your machine
;;;; that keeps eight layers of memory in an embedded vector store, with its
;;;; own knowledge graph. This cell is the client side, as the plugin was: the
;;;; four tools (hyatlas_status, hyatlas_search, hyatlas_recent, hyatlas_add),
;;;; the /hyatlas command, the block a turn is given before it is asked, and
;;;; the turn sent back afterwards. The wire is the server's own /api/v1/*:
;;;; the same paths, bodies and answer shapes the plugin used.
;;;;
;;;; The harness seams are Nodecode's: the section is the config, the tools are
;;;; NLE:REGISTER-TOOL registrations, recall is the :ROUND point (a reminder
;;;; recorded into the round about to be asked) and capture is the :FRAME point.

(defpackage #:nodecode-hyatlas
  (:use #:cl)
  (:export #:hyatlas-error #:hyatlas-error-detail))

(in-package #:nodecode-hyatlas)

(nlk:define-peripheral hyatlas :not-running t :lock "hyatlas")

(defparameter +default-root+ "http://127.0.0.1:19528"
  "Where the server listens unless the section says otherwise: its own default.")

(defparameter +layers+
  '("l1_profile" "l2_raw" "l3_fact" "l4_summary"
    "l5_knowledge" "l6_schema" "l7_intention" "l8_entity")
  "The eight layers, as the tool schemas offer them.")

(defparameter +channels+ '("pages" "profile" "proactive" "experiences" "normal")
  "The search channels in the order a server that sends none is read in; the
server's own \"order\" leads when it sends one.")

(defparameter +snippet-chars+ 300
  "How much of one hit the prompt shows; an entity page gets more.")

(defparameter +page-chars+ 600
  "How much of one entity page a hit line shows: it is a whole topic.")

(defparameter +now-chars+ 1500
  "How much of the Now page a block carries: it is read every turn.")

(defparameter +history-chars+ 160
  "How much of one superseded version a hit shows.")

(defparameter +max-message-chars+ 4000
  "How much of one message a turn's capture sends, as the original caps it.")

(defparameter +max-turn-chars+ 12000
  "How much of one whole turn a capture sends.")

(defparameter +queued-turns+ 8
  "Finished turns waiting for capture; the newest are kept.")

(defvar *queued* '()
  "Finished turns waiting for capture, newest first: (TURN-ID SESSION . TEXTS).")

(defvar *turns* (make-hash-table :test #'equal)
  "Each running turn's two texts, by turn id: (USER . ANSWER).")

(defvar *recalled* (make-hash-table :test #'equal)
  "Turn ids a recall was made for, so one turn is recalled once.")

(defvar *worker* nil
  "The capture worker (NLK:WORKER-START) while the cell runs, or NIL.")

;;; --- the section, read back --------------------------------------------------

(defun root ()
  "The server's address, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-root+)))

(defun check-config ()
  "Refuse a section that names no server: an address is the one thing not guessed."
  (when (null (setting :base-url))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "hyatlas: \"base_url\" is required: where the HyAtlas server is, "
                                 "http://127.0.0.1:19528 for the usual local one"))))

(defun scope (args name setting-name)
  "ARGS' NAME, else the section's SETTING-NAME, else an empty string: the scope
a call asks over, exactly as the original resolves it."
  (let ((value (and (hash-table-p args) (gethash name args))))
    (if (and (stringp value) (plusp (length value))) value (or (setting setting-name) ""))))

;;; --- the wire -----------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((hash-table-p answer) answer)
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "error")
        (nlk:json-value body :string "message")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun call (method path &key body)
  "METHOD PATH on the configured server, BODY its JSON text => the decoded answer.
A failure below HTTP or a refused status signals HYATLAS-ERROR."
  (let* ((url (concatenate 'string (root) path))
         (limit (setting :timeout-seconds))
         (headers (list (cons "accept" "application/json")
                        (cons "user-agent" "nodecode-hyatlas/0.1.0")))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (if body (cons (cons "content-type" "application/json") headers)
                                 headers)
                    :content body
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a: ~a" (string-downcase (string method))
              (nle:transport-failure-label condition url))))
    (if (< status 300)
        (decode-body answer)
        (fail "~a ~a: HTTP ~d~@[: ~a~]" (string-upcase (string method)) path status
              (body-error-text answer)))))

(defun search-memories (query &key user-id agent-id layer (limit (setting :search-limit)) session-id)
  "The server's answer to a search for QUERY: memories by channel, its order, its Now page."
  (let ((body (apply #'nlk:make-json-object
                      (append (list "query" query "limit" limit
                                    "user_id" (or user-id "")
                                    "agent_id" (or agent-id ""))
                              (when (and layer (plusp (length layer))) (list "layer" layer))
                              (when (and session-id (plusp (length session-id)))
                                (list "session_id" session-id))))))
    (call :post "/api/v1/search" :body (nlk:encode-json-object body))))

(defun server-mode ()
  "The server's mode (\"lite\", \"pro\", \"ultra\"), or \"\" when it cannot say."
  (handler-case (or (nlk:json-value (call :get "/api/v1/status") :string "mode") "")
    (error () "")))

(defun list-memories (&key user-id agent-id layer (limit (setting :recent-limit)) offset include-raw)
  "The server's newest memories. INCLUDE-RAW NIL asks for the default the mode
implies: lite stores only raw rows, so it includes them; every other mode
leaves them out."
  (let* ((raw (if (null include-raw) (equal "lite" (server-mode)) include-raw))
         (body (apply #'nlk:make-json-object
                      (append (list "user_id" (or user-id "") "agent_id" (or agent-id "")
                                    "limit" limit "offset" (or offset 0)
                                    "include_raw" (and raw t))
                              (when (and layer (plusp (length layer))) (list "layer" layer))))))
    (call :post "/api/v1/list" :body (nlk:encode-json-object body))))

(defun add-memory (text &key user-id agent-id session-id)
  "Store TEXT as one memory, as the original's hyatlas_add does."
  (let ((body (nlk:make-json-object
               "text" text
               "user_id" (or user-id "")
               "agent_id" (or agent-id "")
               "session_id" (or session-id ""))))
    (call :post "/api/v1/add" :body (nlk:encode-json-object body))))

;;; --- what a hit reads like ------------------------------------------------------

(defun iso-date (seconds)
  "The YYYY-MM-DD (UTC) of a Unix timestamp, or \"\" when it has none."
  (if (and (integerp seconds) (plusp seconds))
      (multiple-value-bind (second minute hour day month year)
          (decode-universal-time (+ seconds 2208988800) 0)
        (declare (ignore second minute hour))
        (format nil "~4,'0d-~2,'0d-~2,'0d" year month day))
      ""))

(defun event-date (text)
  "TEXT when it is a stored event date, YYYY-MM-DD or YYYY-MM; else \"\". A stray
word in the field never reaches the prompt as if it were a date."
  (flet ((digits (from to)
           (and (<= to (length text))
                (loop for index from from below to
                      always (digit-char-p (char text index))))))
    (if (and (stringp text) (member (length text) '(7 10))
             (digits 0 4) (char= #\- (char text 4)) (digits 5 7)
             (or (= (length text) 7)
                 (and (char= #\- (char text 7)) (digits 8 10))))
        text
        "")))

(defun series-note (hit)
  "The \", 3 times (2026-03-02, 2026-05-14)\" of a recurring event, or \"\"."
  (let ((count (nlk:json-value hit :integer "series_count")))
    (if (and count (>= count 2))
        (let ((dates (remove "" (map 'list (lambda (date) (event-date date))
                                     (or (nlk:json-value hit :array "series_dates") #()))
                             :test #'string=)))
          (format nil ", ~d times~@[ (~{~a~^, ~})~]"
                  count (and dates (last dates (min 3 (length dates))))))
        "")))

(defun format-hit-line (hit limit)
  "HIT as one line for the prompt: its layer, its dates, its score, its content,
then up to two versions it replaced. \"\" when it has no content."
  (let ((content (nlk:json-value hit :string "content")))
    (when (and content (plusp (length content)))
      (let* ((saved (iso-date (nlk:json-value hit :integer "gmt_created")))
             (event (event-date (nlk:json-value hit :string "event_date")))
             (when-text (if (plusp (length event))
                            (format nil ", happened ~a~@[, saved ~a~]"
                                    event (and (plusp (length saved)) saved))
                            (if (plusp (length saved)) (format nil ", ~a" saved) "")))
             (score (or (nlk:json-value hit :number "score") 0))
             (layer (or (nlk:json-value hit :string "layer") ""))
             (lines (list (format nil "- (~a~a, score ~,2f~a) ~a"
                                  layer when-text (coerce score 'double-float)
                                  (series-note hit) (subseq content 0 (min limit (length content)))))))
        (loop for entry across (or (nlk:json-value hit :array "history") #())
              for old = (and (hash-table-p entry) (nlk:json-value entry :string "content"))
              while (< (length lines) 3)
              when (and old (plusp (length old)))
                do (let ((date (or (nlk:json-value entry :string "date") "")))
                     (push (format nil "    was: ~a~@[ (until ~a)~]"
                                   (subseq old 0 (min +history-chars+ (length old)))
                                   (and (plusp (length date)) date))
                           lines)))
        (format nil "~{~a~^~%~}" lines)))))

(defun channel-names (results)
  "The channels to show, in the server's order; a channel it sends but does not
list follows the listed ones, and \"now\" is no memory channel."
  (let* ((memories (or (nlk:json-value results :object "memories") (nlk:json-object)))
         (sent (remove "now" (coerce (or (nlk:json-value results :array "order") #()) 'list)
                       :test #'string=))
         (listed (if sent sent +channels+))
         (extra (loop for name being the hash-keys of memories
                       unless (member name listed :test #'string=) collect name)))
    (remove-duplicates (append listed extra) :test #'string=)))

(defun render-results (results &key (items 5))
  "RESULTS as the lines a turn reads: the Now page first, then each channel
with at most ITEMS lines. \"\" when the server recalled nothing."
  (let* ((now (let ((value (nlk:json-value results :object "now"))) (and (hash-table-p value) value)))
         (now-text (and now (string-trim '(#\Space #\Newline)
                                          (or (nlk:json-value now :string "text") ""))))
         (memories (or (nlk:json-value results :object "memories") (nlk:json-object)))
         (blocks '()))
    (when (and now-text (plusp (length now-text)))
      (let ((updated (subseq (or (nlk:json-value now :string "updated") "") 0
                             (min 10 (length (or (nlk:json-value now :string "updated") ""))))))
        (push (format nil "[NOW]~@[ (updated ~a)~]~%~a"
                      (and (plusp (length updated)) updated)
                      (subseq now-text 0 (min +now-chars+ (length now-text))))
              blocks)))
    (dolist (name (channel-names results))
      (let* ((hits (or (nlk:json-value memories :array name) #()))
             (limit (if (equal name "pages") +page-chars+ +snippet-chars+))
             (lines (list (format nil "[~a]" (string-upcase name)))))
        (loop for hit across hits
              for index from 0
              while (< index items)
              for line = (and (hash-table-p hit) (format-hit-line hit limit))
              when line do (push line lines))
        (when (> (length lines) 1) (push (format nil "~{~a~^~%~}" (nreverse lines)) blocks))))
    (format nil "~{~a~^~%~%~}" (nreverse blocks))))

(defun render-recall (results query)
  "The one block a turn is given: RESULTS under the query they answer, as the
original's prefetch writes it. \"\" when nothing matched."
  (let ((body (render-results results)))
    (if (plusp (length body))
        (format nil "<relevant-memories query=\"~a\">~%~a~%</relevant-memories>"
                (subseq query 0 (min 50 (length query))) body)
        "")))

(defun render-status (status)
  "STATUS as the few lines the model reads: the mode, the three services, the
counts, then the layers."
  (flet ((field (key) (or (nlk:json-value status :string key) "?"))
         (total (key) (or (nlk:json-value status :integer key) 0)))
    (let ((layers (let ((value (nlk:json-value status :object "layers")))
                    (and (hash-table-p value) value))))
      (format nil "HyAtlas ~a: mode ~a; vdb ~a, embed ~a, llm ~a~@[ (~a)~]; write pipeline ~a; ~d writes, ~d searches; graph ~d nodes, ~d edges; ~d unextracted~@[~%~a~]"
              (field "version") (field "mode") (field "vdb") (field "embed") (field "llm")
              (nlk:json-value status :string "llm_model")
              (field "write_pipeline")
              (total "writes") (total "searches")
              (total "graph_nodes") (total "graph_edges") (total "unextracted")
              (and layers
                   (format nil "layers: ~{~a~^, ~}"
                           (loop for name in +layers+
                                 collect (format nil "~a ~d" name
                                                 (or (gethash name layers) 0)))))))))

(defun render-list (answer)
  "ANSWER -- the server's /api/v1/list -- as one line a memory."
  (let ((rows (or (nlk:json-value answer :array "memories") #())))
    (if (zerop (length rows))
        "No memories yet."
        (format nil "~d of ~d memories:~%~{~a~^~%~}"
                (length rows) (or (nlk:json-value answer :integer "total") (length rows))
                (loop for row across rows
                      for content = (and (hash-table-p row) (nlk:json-value row :string "content"))
                      when (and content (plusp (length content)))
                        collect (or (format-hit-line row +snippet-chars+)
                                    (format nil "- ~a" content)))))))

;;; --- the tools ------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: hyatlas: ~a" condition)))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space) value))))
        (string-trim '(#\Space) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun argument-layer (args)
  "ARGS' LAYER, refused unless it names one of the eight."
  (let ((layer (gethash "layer" args)))
    (cond ((null layer) "")
          ((member layer +layers+ :test #'string=) layer)
          (t (fail "layer is one of ~{~a~^, ~}, not ~s" +layers+ layer)))))

(defun argument-limit (args name default)
  (let ((value (gethash name args)))
    (if (integerp value) (max 1 (min value 200)) default)))

(defun argument-boolean (args name)
  "ARGS' NAME when it is there, else NIL; NIL also means the section's default."
  (multiple-value-bind (value present) (gethash name args)
    (and present (and value t))))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P &optional ENUM) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *layer-enum* (coerce +layers+ 'vector)
  "The eight layer names as the schema's enum, since TOOL-SCHEMA carries no enum.")

(defparameter *search-schema*
  (list :description
        "Semantic search the HyAtlas memory store for memories relevant to the query. Returns the channels pages, profile, proactive, experiences and normal, each hit with score, layer, content, timestamp and, when known, its event date and the older versions it replaced. Queries may name a time ('last week', '2026-10-05'). Prefer this over relying on the recall block when you need explicit, up-to-date recall."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "query" (nlk:json-object "type" "string" "description" "Natural language search query")
          "user_id" (nlk:json-object "type" "string"
                                      "description" "User scope (defaults to the section's user_id)")
          "agent_id" (nlk:json-object "type" "string"
                                       "description" "Agent scope (defaults to the section's agent_id)")
          "layer" (nlk:json-object "type" "string" "enum" *layer-enum*
                                    "description" "Restrict to one layer")
          "limit" (nlk:json-object "type" "integer"
                                    "description" "Max results per channel (default the section's search_limit)"))
         "required" (vector "query")))
  "The hyatlas_search tool, as the original's HYATLAS_SEARCH_SCHEMA has it.")

(defparameter *status-schema*
  (list :description
        "Get the health and layer counts of the HyAtlas memory server: vdb, embed, llm and write_pipeline status, per-layer counts, graph nodes and edges. Use this to confirm the memory system is healthy before recalling or writing."
        :parameters (nlk:json-object "type" "object" "properties" (nlk:json-object)
                                      "required" (vector)))
  "The hyatlas_status tool.")

(defparameter *recent-schema*
  (list :description
        "List the most recent HyAtlas memories, optionally filtered by layer. Use this to see what was just saved or to browse the latest activity without a semantic query."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "user_id" (nlk:json-object "type" "string" "description" "User scope")
          "agent_id" (nlk:json-object "type" "string" "description" "Agent scope")
          "layer" (nlk:json-object "type" "string" "enum" *layer-enum*
                                    "description" "Restrict to one layer")
          "limit" (nlk:json-object "type" "integer"
                                    "description" "Max items (default the section's recent_limit)")
          "include_raw" (nlk:json-object "type" "boolean"
                                          "description" "Include L2 raw rows, the unprocessed input. Omit it for the server's mode: lite includes them, the other modes do not."))
         "required" (vector)))
  "The hyatlas_recent tool.")

(defparameter *add-schema*
  (list :description
        "Save a memory directly to HyAtlas: durable facts worth remembering. The text is stored raw (L2), and in pro and ultra mode the server's LLM extracts facts with dates, preferences, intentions, entity pages and task experiences from it; summaries and the knowledge graph follow in the background. Prefer the built-in memory tool for atomic facts; this is for richer context dumps."
        :parameters
        (nlk:json-object
         "type" "object"
         "properties"
         (nlk:make-json-object
          "text" (nlk:json-object "type" "string" "description" "Memory text to store (LLM-extracted)")
          "user_id" (nlk:json-object "type" "string" "description" "User scope")
          "agent_id" (nlk:json-object "type" "string" "description" "Agent scope")
          "session_id" (nlk:json-object "type" "string" "description" "Session scope"))
         "required" (vector "text")))
  "The hyatlas_add tool.")

(defun tool-status (args)
  (declare (ignore args))
  (answering (render-status (call :get "/api/v1/status"))))

(defun tool-search (args)
  (answering
    (let* ((query (argument-text args "query"))
           (layer (argument-layer args))
           (results (search-memories query
                                      :user-id (scope args "user_id" :user-id)
                                      :agent-id (scope args "agent_id" :agent-id)
                                      :layer layer
                                      :limit (argument-limit args "limit" (setting :search-limit)))))
      (let ((body (render-results results)))
        (if (plusp (length body))
            (format nil "~d memories for ~s:~%~a"
                    (loop for name in (channel-names results)
                          sum (length (or (nlk:json-value (nlk:json-value results :object "memories")
                                                           :array name)
                                          #())))
                    query body)
            (format nil "No memory matched ~s." query))))))

(defun tool-recent (args)
  (answering
    (render-list (list-memories :user-id (scope args "user_id" :user-id)
                                 :agent-id (scope args "agent_id" :agent-id)
                                 :layer (argument-layer args)
                                 :limit (argument-limit args "limit" (setting :recent-limit))
                                 :include-raw (argument-boolean args "include_raw")))))

(defun tool-add (args)
  (answering
    (let* ((text (argument-text args "text"))
           (answer (add-memory text
                               :user-id (scope args "user_id" :user-id)
                               :agent-id (scope args "agent_id" :agent-id)
                               :session-id (or (gethash "session_id" args) ""))))
      (format nil "Stored~@[ as ~a~]~@[ (extraction ~a)~]."
              (nlk:json-value answer :string "memory_id")
              (nlk:json-value answer :string "extraction_status")))))

;;; --- the turn: recall before it, capture after it --------------------------------

(defun turn-input (session turn)
  "The text of TURN's newest user message in SESSION, or \"\"."
  (let ((turn-id (and turn (nlk:durable-turn-turn-id turn))))
    (or (and turn-id
             (nlk:fact-message-content
              (first (nlk:events :session-id session :turn-id turn-id
                                 :kind "turn.input_committed" :order :newest
                                 :limit 1 :as :payloads))))
        "")))

(defun turn-prompt (session turn)
  "TURN's own id, and SESSION's newest user text in it. The one read of the
store the recall makes, so a test can stand in for it."
  (let ((turn-id (and turn (nlk:durable-turn-turn-id turn))))
    (values (if turn-id (turn-input session turn) "") turn-id)))

(defun recall (op)
  "The :ROUND hook: the memories nearest this turn's own message, as one
reminder in the round about to be asked -- once per turn, whatever the round
numbering, so a long turn is not searched every round."
  (let ((session (getf op :session-id))
        (turn (getf op :turn)))
    (when (and *hyatlas* (setting :recall) turn)
      (multiple-value-bind (query turn-id) (turn-prompt session turn)
        (when (and turn-id (plusp (length query)) (not (gethash turn-id *recalled*)))
          (setf (gethash turn-id *recalled*) t)
          (handler-case
              (let ((text (render-recall (search-memories query
                                                          :user-id (setting :user-id)
                                                          :agent-id (setting :agent-id)
                                                          :limit (setting :recall-limit))
                                         query)))
                (when (plusp (length text))
                  (nlk:record-turn-context-prompt turn :text text)))
            (error (condition)
              (when (> (hash-table-count *recalled*) 512)
                (setf *recalled* (make-hash-table :test #'equal)))
              (warn "hyatlas: recall failed: ~a" condition))))))))

;;; --- capture: the turn's two texts, sent after it -------------------------------

(defun clip (text limit)
  "TEXT, cut to LIMIT characters, saying how much was dropped, as the
original's _clip does."
  (let ((text (or text "")))
    (if (<= (length text) limit)
        text
        (format nil "~a~%...[truncated ~d chars]" (subseq text 0 limit) (- (length text) limit)))))

(defun turn-text (user answer)
  "One turn as the server is given it: the two messages, each capped, and the
pair capped as a whole -- the original's _turn_pair."
  (let ((parts (remove nil
                       (list (and user (plusp (length user))
                                  (format nil "USER: ~a" (clip user +max-message-chars+)))
                             (and answer (plusp (length answer))
                                  (format nil "ASSISTANT: ~a" (clip answer +max-message-chars+)))))))
    (clip (format nil "~{~a~^~%~%~}" parts) +max-turn-chars+)))

(defun remember-turn (turn key text)
  "Keep TEXT, the :USER or :ANSWER half of TURN, until the turn completes."
  (when (and (stringp turn) (stringp text) (plusp (length text)))
    (let ((entry (or (gethash turn *turns*)
                     (setf (gethash turn *turns*) (cons nil nil)))))
      (if (eq key :user) (setf (car entry) text) (setf (cdr entry) text)))))

(defun take-turn (turn)
  (let ((entry (gethash turn *turns*)))
    (when entry (remhash turn *turns*))
    entry))

(defun enqueue-turn (session texts)
  "Queue TEXTS, a finished turn's (USER . ANSWER), for capture; the newest are kept."
  (when (and (car texts) (cdr texts))
    (setf *queued* (cons (cons session texts) *queued*))
    (when (> (length *queued*) +queued-turns+)
      (setf *queued* (subseq *queued* 0 +queued-turns+)))
    (when *worker* (nlk:worker-poke *worker*))))

(defun pop-queued ()
  (let ((entry (first *queued*)))
    (when entry (setf *queued* (rest *queued*)))
    entry))

(defun capture-turn (session texts)
  "Send one finished turn to the server, best effort: a failure loses a turn's
memory, never the turn."
  (let ((text (turn-text (car texts) (cdr texts))))
    (when (plusp (length text))
      (handler-case
          (add-memory text :user-id (setting :user-id) :agent-id (setting :agent-id)
                      :session-id (or session ""))
        (error (condition) (warn "hyatlas: a turn was not captured: ~a" condition))))))

(defun capture-step ()
  "The worker's step: send every queued turn."
  (loop for entry = (pop-queued)
        while entry
        do (capture-turn (car entry) (cdr entry)))
  nil)

(defun observe-frame (op)
  "Fold one published frame: a turn's two texts kept, a finished turn queued."
  (multiple-value-bind (type payload metadata turn) (nlk:frame-fact op)
    (declare (ignore metadata))
    (when (and type turn *hyatlas* (setting :capture))
      (cond ((equal type "turn.input_committed")
             (remember-turn turn :user (nlk:fact-message-content payload)))
            ((equal type "turn.assistant_message_completed")
             (remember-turn turn :answer (nlk:fact-message-content payload)))
            ((equal type "turn.completed")
             (let ((texts (take-turn turn)))
               (when texts (enqueue-turn (getf op :session-id) texts))))))))

;;; --- the manual and /hyatlas ----------------------------------------------------

(defparameter +primer+
  "HyAtlas keeps a user's memory on a server of their own (hyatlas-go, on this machine by
default): eight layers, an embedded vector store, a knowledge graph, and no cloud in lite
mode. (help :hyatlas) is this page; the tools are the four below.
  hyatlas_search {query, user_id?, agent_id?, layer?, limit?}  the memories nearest a
      question, by channel, each with its layer, date and score
  hyatlas_recent {user_id?, agent_id?, layer?, limit?, include_raw?}  the newest memories
  hyatlas_add {text, user_id?, agent_id?, session_id?}  store a context dump
  hyatlas_status {}                                 the server's health and layer counts
Search before answering anything that may depend on what you know about this user. Before
every turn the server's nearest memories for your message are put in the round already,
and every finished turn is sent back for extraction, so search when you need more than
that. Queries may name a time ('last week', '2026-10-05'). The hyatlas section of
config.jsonc names the server (base_url, required), the user_id and agent_id your
memories are scoped to, and whether recalls and capture run at all."
  "What (help :hyatlas) answers while the cell runs.")

(defun usage-line ()
  "Usage: /hyatlas [status|search <query>|add <text>|recent]")

(defun run-command (args session-id)
  "One /hyatlas invocation: no argument the server's health, else a subcommand,
as the original's slash command has them."
  (declare (ignore session-id))
  (let* ((text (nlk:trimmed (or args "")))
         (space (position #\Space text))
         (command (string-downcase (if space (subseq text 0 space) text)))
         (rest (if space (nlk:trimmed (subseq text (1+ space))) "")))
    (cond ((zerop (length text))
           (answering (format nil "HyAtlas at ~a, user ~s, agent ~s.~%~a"
                               (root) (setting :user-id) (setting :agent-id)
                               (render-status (call :get "/api/v1/status")))))
          ((equal command "status") (tool-status nil))
          ((equal command "search")
           (if (plusp (length rest))
               (tool-search (nlk:json-object "query" rest))
               "Usage: /hyatlas search <query>"))
          ((equal command "add")
           (if (plusp (length rest))
               (tool-add (nlk:json-object "text" rest))
               "Usage: /hyatlas add <text>"))
          ((equal command "recent") (tool-recent (nlk:json-object)))
          (t (format nil "~a~%Unknown subcommand ~s." (usage-line) command)))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell hyatlas
  (:section ("hyatlas")
    (:guide "base_url is where your HyAtlas server listens (http://127.0.0.1:19528 for a local one); user_id and agent_id are the scope your memories live under; recall and capture are on unless you turn them off; next step: /hyatlas shows the server's health")
    ("base_url" :string :required t
     :doc "the HyAtlas server's address, with scheme, e.g. http://127.0.0.1:19528")
    ("user_id" :string :default "default"
     :doc "whose memories these are; the same id across sessions finds the same memories")
    ("agent_id" :string :default "default"
     :doc "the agent id every write is stamped with")
    ("recall" :boolean :default t
     :doc "search the server for each turn's own message and put the nearest memories in it")
    ("capture" :boolean :default t
     :doc "send each finished turn to the server for fact extraction")
    ("search_limit" :integer :default 10 :min 1
     :doc "how many memories one search asks for per channel unless the call says otherwise")
    ("recall_limit" :integer :default 5 :min 1
     :doc "how many memories one turn's own recall asks for per channel")
    ("recent_limit" :integer :default 20 :min 1
     :doc "how many memories hyatlas_recent lists unless the call says otherwise")
    ("timeout_seconds" :integer :default 15 :min 1
     :doc "one request's ceiling"))
  (:help :hyatlas "hyatlas: hyatlas_search recalls a self-hosted eight-layer memory; hyatlas_add, hyatlas_recent and hyatlas_status keep and inspect it" +primer+)
  (:command "hyatlas" 'run-command
            :description "Show the HyAtlas server's health, or search, add and list memories"
            :argument-hint "[status|search <query>|add <text>|recent]")
  (:tool "hyatlas_search" *search-schema* 'tool-search)
  (:tool "hyatlas_recent" *recent-schema* 'tool-recent)
  (:tool "hyatlas_add" *add-schema* 'tool-add)
  (:tool "hyatlas_status" *status-schema* 'tool-status)
  (:hook :round (nlk:observer #'recall "nodecode-hyatlas"))
  (:hook :frame (nlk:observer #'observe-frame "nodecode-hyatlas"))
  (:start (lambda ()
            (check-config)
            (setf *turns* (make-hash-table :test #'equal)
                  *recalled* (make-hash-table :test #'equal)
                  *queued* '())
            (setf *worker* (nlk:worker-start "hyatlas-capture" #'capture-step :wake 60))
            (nle:on-stop (lambda ()
                           (nlk:worker-stop *worker*)
                           (setf *worker* nil
                                 *queued* '()
                                 *turns* (make-hash-table :test #'equal)
                                 *recalled* (make-hash-table :test #'equal)))))))
