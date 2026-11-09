;;;; cell.lisp --- one wiki all your agents read and write: six tools, a recall, /dexio.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A rewrite of dexio-wiki/hermes-plugin (see NOTICE). Dexio is one wiki
;;;; over MCP (https://dexio.wiki): before each turn the pages that match the
;;;; user's message are put in front of the model, and the model searches,
;;;; reads and writes pages other agents and people share. Nothing is written
;;;; automatically -- the agent files what is worth keeping, as a person adds
;;;; to a team wiki.
;;;;
;;;; The wire is the original's: one JSON-RPC `tools/call' per request,
;;;; POSTed to <url>/mcp with `Authorization: Bearer <key>', answered plain
;;;; JSON or as a short server-sent stream; the second content item of a
;;;; read is the page itself. The names, arguments and descriptions of the
;;;; six tools are the original's, and so is the recall block's shape.
;;;;
;;;; The harness seams are Nodecode's: the section is the config, the tools
;;;; are NLE:DEFINE-CELL's :TOOL clauses, the recall before a request is the
;;;; :ROUND point (the seam that carries the turn's own message), and the
;;;; paragraph about the wiki rides every request as a harness section.

(defpackage #:nodecode-dexio
  (:use #:cl)
  (:export #:dexio-error #:dexio-error-detail))

(in-package #:nodecode-dexio)

(nlk:define-peripheral dexio :not-running t :lock "dexio")

(defparameter +version+ "0.2.0"
  "The plugin release this cell carries; it rides the User-Agent, as the original's does.")

(defparameter +default-url+ "https://app.dexio.wiki"
  "The hosted wiki, where an unset `url' points; a self-hosted server answers the same wire.")

(defparameter +keys-url+ "https://app.dexio.wiki/settings/agents"
  "Where a person makes an API key, as the original's KEYS_URL says.")

(defparameter +query-chars+ 300
  "How much of a message the recall searches by: the original's QUERY_CHARS.")

(defparameter +matches-per-page+ 3
  "How many matching lines of a page one recall shows, as the original asks the wiki for.")

(defparameter +snippet-chars+ 240
  "One matching line, clipped, as the original's format_recall clips it.")

(defparameter +max-results-cap+ 10
  "The most pages one search asks for, the original's clamp.")

(defparameter +recall-cache-size+ 32
  "Distinct messages kept in *RECALL-CACHE*, as the original's recall cache keeps.")

(defvar *ids* 0
  "The last JSON-RPC id this image sent; one more each call, as the original numbers them.")

(defvar *recall-cache* (make-hash-table :test #'equal)
  "Each message this session recalled by, against the block it recalled.")

;;; --- the section, read back ------------------------------------------------------

(defun api-key ()
  "The key the section names, inline or by environment variable, else DEXIO_API_KEY."
  (or (let ((inline (setting :api-key))) (and inline (plusp (length inline)) inline))
      (let ((variable (setting :api-key-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))
      (uiop:getenv "DEXIO_API_KEY")))

(defun root ()
  "The wiki this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :url) ""))))
    (if (plusp (length given)) given +default-url+)))

(defun endpoint ()
  "The MCP endpoint: the wiki's own /mcp, as the original's DexioClient builds it."
  (concatenate 'string (root) "/mcp"))

(defun agent-name ()
  "The name Dexio records on every page this cell writes, the original's profile identity."
  (let ((named (setting :agent)))
    (if (and named (plusp (length named))) named "nodecode")))

(defun result-limit ()
  "How many pages one recall asks for: the section's max_results, 1 to 10."
  (max 1 (min +max-results-cap+ (or (setting :max-results) 4))))

(defun check-config ()
  "Refuse a section that could never reach a wiki, naming what is missing."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "dexio: needs an API key: set DEXIO_API_KEY, or \"api_key\" "
                                 "(or \"api_key_env\") in the dexio section; make one at "
                                 +keys-url+ " (Settings > Agents)"))))

;;; --- the wire --------------------------------------------------------------------

(defun call (tool arguments)
  "One MCP tools/call of TOOL with ARGUMENTS, a JSON object, as the original's
DexioClient.call does: => the answer's data object. Every failure is DEXIO-ERROR."
  (let* ((id (with-dexio-lock (incf *ids*)))
         (body (nlk:encode-json-object
                (nlk:json-object "jsonrpc" "2.0"
                                 "id" id
                                 "method" "tools/call"
                                 "params" (nlk:json-object "name" tool
                                                            "arguments" arguments))))
         (limit (setting :timeout-seconds))
         (raw nil)
         (status nil))
    (handler-case
        (multiple-value-setq (raw status)
          (nlk:http :post (endpoint)
                    :headers (list (cons "authorization" (format nil "Bearer ~a" (or (api-key) "")))
                                   (cons "content-type" "application/json")
                                   (cons "accept" "application/json, text/event-stream")
                                   (cons "user-agent" (format nil "nodecode-dexio/~a" +version+)))
                    :content body
                    :timeout limit
                    :connect-timeout (min 6 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "could not reach ~a: ~a" (endpoint) (nle:transport-failure-label condition (endpoint)))))
    (when (>= status 300)
      (if (= status 401)
          (fail "the wiki refused the API key (401): make a new one in Settings > Agents (~a) ~
                 and set DEXIO_API_KEY" +keys-url+)
          (fail "the wiki answered HTTP ~d" status)))
    (result-data (jsonrpc-message raw))))

(defun jsonrpc-message (raw)
  "RAW -- a JSON body or a short server-sent stream -- as the JSON-RPC message it carries."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (or raw ""))))
    (cond ((zerop (length text)) (fail "the wiki answered nothing"))
          ((char= (char text 0) #\{) (or (ignore-errors (nlk:decode-json text))
                                          (fail "unreadable reply from the wiki")))
          (t (let ((message nil))
               (dolist (line (nlk:lines text) (or message (fail "unreadable reply from the wiki")))
                 (when (and (null message) (uiop:string-prefix-p "data:" line))
                   (let ((payload (string-trim '(#\Space #\Tab #\Return) (subseq line 5))))
                     (when (plusp (length payload))
                       (let ((decoded (and (char= (char payload 0) #\{)
                                           (ignore-errors (nlk:decode-json payload)))))
                         (when (or (nlk:json-value decoded :object "result")
                                   (nlk:json-value decoded :object "error"))
                           (setf message decoded))))))))))))

(defun content-texts (message)
  "MESSAGE's result content, each item's text, in order."
  (let ((content (nlk:json-value message :array "result" "content")))
    (if (vectorp content)
        (loop for item across content
              for text = (nlk:json-value item :string "text")
              when text collect text)
        '())))

(defun parse-item (text)
  "TEXT -- one content item -- as a JSON object when it is one, else {\"text\": TEXT}."
  (let ((trimmed (and text (string-left-trim '(#\Space #\Tab #\Newline) text))))
    (cond ((or (null trimmed) (zerop (length trimmed))) (nlk:json-object))
          ((char= (char trimmed 0) #\{)
           (let ((decoded (ignore-errors (nlk:decode-json trimmed))))
             (if (hash-table-p decoded) decoded (nlk:json-object "text" text))))
          (t (nlk:json-object "text" text)))))

(defun result-data (message)
  "MESSAGE's tool result as the data object the model reads: its first content
item parsed as JSON where it is JSON, and a second item's text as the page."
  (let ((error (nlk:json-value message :object "error")))
    (when error
      (fail "~a" (or (nlk:json-value error :string "message") "the wiki refused the call"))))
  (let* ((texts (content-texts message))
         (head (first texts))
         (data (parse-item head)))
    (when (nlk:json-value message :boolean "result" "isError")
      (fail "~a" (or (nlk:json-value data :string "text") head "the wiki returned an error")))
    (when (second texts)                 ; read_page: a JSON header, then the page itself
      (setf (gethash "text" data) (second texts)))
    data))

;;; --- the words -------------------------------------------------------------------

(defparameter +framing+
  "## From your Dexio wiki
Pages that match this message. Read one with dexio_read before relying on it."
  "One recall's first two lines, as the original's format_recall writes them.")

(defun one-line (text)
  "TEXT with its runs of whitespace folded to one space, trimmed."
  (nlk:trimmed (format nil "~{~a~^ ~}" (remove "" (mapcar #'nlk:trimmed (nlk:lines (or text "")))
                                                :test #'equal))))

(defun hits (data)
  "DATA's hit objects: the \"results\" array of an object, or the array itself."
  (coerce (or (cond ((hash-table-p data) (nlk:json-value data :array "results"))
                    ((vectorp data) data))
               #())
          'list))

(defun clip-lines (lines limit &key (header ""))
  "HEADER and LINES joined by a newline, at most LIMIT characters, as the
original clips its recall block: the last whole line inside the budget."
  (let ((block (format nil "~@[~a~%~]~{~a~^~%~}" header lines)))
    (if (<= (length block) limit)
        block
        (let* ((window (subseq block 0 limit))
               (cut (position #\Newline window :from-end t)))
          (if cut (subseq block 0 cut) window)))))

(defun hit-lines (data &key (limit (result-limit)) (per-page +matches-per-page+) (header ""))
  "DATA's hits as the lines the model reads: the title and path of each page,
then the lines that matched it -- the original's format_recall."
  (let* ((found (hits data))
         (lines (loop for hit in (subseq found 0 (min limit (length found)))
                      nconc (let ((title (or (nlk:json-value hit :string "title")
                                             (nlk:json-value hit :string "path")))
                                  (path (nlk:json-value hit :string "path"))
                                  (matches (nlk:json-value hit :array "matches")))
                              (cons (format nil "- ~a (`~a`)" (or title path) path)
                                    (loop for index below (min per-page (if (vectorp matches)
                                                                            (length matches)
                                                                            0))
                                          for text = (one-line (or (nlk:json-value (aref matches index)
                                                                                   :string "text")
                                                                   ""))
                                          when (and (plusp (length text))
                                                    (not (equal text (format nil "title: ~a"
                                                                             (or title path)))))
                                            collect (format nil "    ~a"
                                                            (nlk:clip text +snippet-chars+))))))))
    (clip-lines lines (or (setting :recall-chars) 3000) :header header)))

(defun page-lines (data)
  "DATA's page objects as the lines dexio_list answers: one page a line."
  (let ((pages (or (nlk:json-value data :array "pages") (nlk:json-value data :array "results"))))
    (loop for page across (or pages #())
          for path = (nlk:json-value page :string "path")
          for title = (nlk:json-value page :string "title")
          for summary = (one-line (or (nlk:json-value page :string "description")
                                      (nlk:json-value page :string "summary") ""))
          collect (format nil "- ~a~@[ (~a)~]~@[ -- ~a~]"
                          (or title path) (and title path) summary))))

(defun answer-line (verb path data)
  "One line saying what a write did, over the answer's path and version."
  (format nil "~a ~a~@[ (version ~a)~]"
          verb
          (or (nlk:json-value data :string "path") path)
          (nlk:json-value data :string "version")))

;;; --- the tools -------------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: dexio: ~a" condition)))))

(defun arguments (&rest pairs)
  "PAIRS -- NAME VALUE ... -- as a JSON object, a NIL or \"\" VALUE left out,
as the original's _pick leaves it out."
  (apply #'nlk:make-json-object
         (loop for (name value) on pairs by #'cddr
               unless (or (null value) (and (stringp value) (zerop (length value))))
                 nconc (list name value))))

(defun argument-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or a refusal naming it."
  (let ((value (gethash name args)))
    (if (and (stringp value) (plusp (length (string-trim '(#\Space #\Tab #\Newline) value))))
        (string-trim '(#\Space #\Tab #\Newline) value)
        (fail "~a is required" (substitute #\_ #\- name)))))

(defun optional-text (args name)
  "ARGS' NAME as a trimmed non-empty string, or NIL."
  (let ((value (gethash name args)))
    (and (stringp value) (plusp (length (string-trim '(#\Space #\Tab #\Newline) value)))
         (string-trim '(#\Space #\Tab #\Newline) value))))

(defparameter +aliases+
  '(("dexio_edit" ("old_string" . "old_text") ("new_string" . "new_text"))
    ("dexio_write" ("content" . "text"))
    ("dexio_append" ("content" . "text")))
  "Names models reach for out of habit, mapped onto the documented ones -- the
original's _ALIASES, used only when the documented name is absent.")

(defun normalize-arguments (tool args)
  "ARGS with the alias members TOOL carries renamed, an explicit argument winning."
  (let ((out (make-hash-table :test #'equal)))
    (loop for key being the hash-keys of args using (hash-value value)
          do (setf (gethash key out) value))
    (dolist (alias (cdr (assoc tool +aliases+ :test #'equal)) out)
      (when (and (gethash (car alias) out) (not (gethash (cdr alias) out)))
        (setf (gethash (cdr alias) out) (gethash (car alias) out))
        (remhash (car alias) out)))))

(defun after-write ()
  "The next request's recall should see the change, as the original clears its cache."
  (clrhash *recall-cache*))

(defun tool-search (args)
  (answering
    (let* ((args (normalize-arguments "dexio_search" args))
           (query (argument-text args "query"))
           (given (gethash "limit" args))
           (limit (if (integerp given) (max 1 (min +max-results-cap+ given)) 10))
           (data (call "search_pages"
                       (arguments "query" query
                                  "folder" (optional-text args "folder")
                                  "limit" limit
                                  "matches_per_page" +matches-per-page+)))
           (found (hits data)))
      (if (null found)
          (format nil "No page of the wiki matched ~s." query)
          (hit-lines data :limit limit
                          :header (format nil "~d page~:p for ~s:" (length found) query))))))

(defun tool-read (args)
  (answering
    (let* ((args (normalize-arguments "dexio_read" args))
           (path (argument-text args "path"))
           (data (call "read_page" (arguments "path" path
                                              "section" (optional-text args "section"))))
           (text (nlk:json-value data :string "text")))
      (if (and text (plusp (length text)))
          (format nil "~a~@[ (version ~a)~]~%~%~a"
                  (or (nlk:json-value data :string "path") path)
                  (nlk:json-value data :string "version")
                  text)
          (nlk:encode-json-object data)))))

(defun tool-list (args)
  (answering
    (let* ((args (normalize-arguments "dexio_list" args))
           (folder (optional-text args "folder"))
           (data (call "list_pages" (arguments "folder" folder)))
           (lines (page-lines data)))
      (if lines
          (format nil "~d page~:p~@[ in ~a~]:~%~{~a~^~%~}" (length lines) folder lines)
          (nlk:encode-json-object data)))))

(defun tool-write (args)
  (answering
    (let* ((args (normalize-arguments "dexio_write" args))
           (path (argument-text args "path"))
           (text (argument-text args "text"))
           (data (call "write_page"
                       (arguments "path" path
                                  "text" text
                                  "note" (optional-text args "note")
                                  "base_version" (optional-text args "base_version")
                                  "agent" (agent-name)))))
      (after-write)
      (answer-line "Wrote" path data))))

(defun tool-edit (args)
  (answering
    (let* ((args (normalize-arguments "dexio_edit" args))
           (path (argument-text args "path"))
           (old (argument-text args "old_text"))
           (new (gethash "new_text" args)))
      ;; A missing member is not an empty one: as "" it would delete OLD_TEXT.
      (unless (stringp new)
        (fail "new_text is required (the replacement; pass \"\" to delete old_text)"))
      (let* ((note (optional-text args "note"))
             (data (call "edit_page"
                          (apply #'nlk:make-json-object
                                 (append (list "path" path "old_text" old "new_text" new
                                               "agent" (agent-name))
                                         (when note (list "note" note)))))))
        (after-write)
        (answer-line "Edited" path data)))))

(defun tool-append (args)
  (answering
    (let* ((args (normalize-arguments "dexio_append" args))
           (path (argument-text args "path"))
           (text (argument-text args "text"))
           (data (call "append_page"
                       (arguments "path" path
                                  "text" text
                                  "note" (optional-text args "note")
                                  "agent" (agent-name)))))
      (after-write)
      (answer-line "Appended to" path data))))

;;; --- the recall before a request --------------------------------------------------

(defun turn-input (session-id)
  "The message the running turn started with, or NIL."
  (nlk:json-value (nlk:events :session-id session-id :kind "turn.started"
                              :order :newest :limit 1 :as :payload)
                   :string "input"))

(defun recall-block (query)
  "The block one message recalls: the wiki's matching pages, or \"\" when none does."
  (let ((cached (gethash query *recall-cache* :none)))
    (when (eq cached :none)
      (let ((data (call "search_pages"
                        (arguments "query" query
                                   "limit" (result-limit)
                                   "matches_per_page" +matches-per-page+))))
        (setf cached (if (hits data)
                         (hit-lines data :limit (result-limit) :header +framing+)
                         ""))
        (when (> (hash-table-count *recall-cache*) +recall-cache-size+)
          (clrhash *recall-cache*))
        (setf (gethash query *recall-cache*) cached)))
    cached))

(defun recall-at-round (op)
  "At OP, one model request about to be built: the pages of the wiki that
match this turn's message, filed as the turn's context so the very request
carries them. The first round only, and never for a scheduled run."
  (let ((session (getf op :session-id))
        (turn (getf op :turn))
        (round (getf op :round)))
    (when (and session turn (eql 0 round)
               (nlk:store-open-p)
               (setting :auto-recall)
               (not (equal "cron" (nlk:session-source-kind session))))
      (let ((prompt (ignore-errors (turn-input session))))
        (when prompt
          (let* ((text (one-line prompt))
                 (query (subseq text 0 (min +query-chars+ (length text)))))
            (when (>= (length query) 3)
              (let ((block (recall-block query)))
                (when (and block (plusp (length block)))
                  (nlk:record-turn-context-prompt turn :text block))))))))))

;;; --- the paragraph every request carries ------------------------------------------

(defun standing-primer (next session-id)
  "Every request's harness sections, with Dexio's first: what the wiki is,
where it is, and when to file into it -- the original's system_prompt_block."
  (acons "dexio"
         (format nil "You share a Dexio wiki with the user's other agents and their team (~a). ~
                      Before each request, the pages that match the user's message appear under ~
                      \"From your Dexio wiki\"; read one with dexio_read before relying on it. ~
                      Use dexio_search and dexio_list to look things up, and file what other ~
                      agents or people should know later (a decision and its reason, how a ~
                      system works, a fact about a customer or project) with dexio_write, ~
                      dexio_edit or dexio_append: search first and update the existing page ~
                      instead of making a duplicate, link related pages with [[path]], and give ~
                      a one-line note. Never file a secret or passing chatter."
                 (root))
         (remove "dexio" (funcall next session-id) :key #'car :test #'equal)))

;;; --- the manual and /dexio -------------------------------------------------------

(defparameter +primer+
  "Dexio is one wiki all your agents read and write over MCP, shared with the user's other
agents and their team. (help :dexio) is this page.
  dexio_search {query, folder?, limit?}          pages matching words, best first, with the lines
  dexio_read {path, section?}                    one page's markdown, with its version and links
  dexio_list {folder?}                           the pages, or one folder's, with descriptions
  dexio_write {path, text, note?, base_version?} create a page or replace its whole text
  dexio_edit {path, old_text, new_text, note?}   replace one exact, unique piece of a page
  dexio_append {path, text, note?}               add text to the end, creating the page
Search first and update the existing page rather than making a duplicate; link related pages
with [[path]]; give a one-line note saying what changed and why. Nothing is written
automatically: file what other agents or people should know later, never a secret or chatter.
Before each request the pages matching the message arrive by themselves under \"From your
Dexio wiki\" (dexio.auto_recall turns that off). The dexio section of config.jsonc names the
server (url), the key (api_key, api_key_env, or DEXIO_API_KEY), and the name your writes
carry (agent)."
  "What (help :dexio) answers while the cell runs.")

(defun status-line ()
  "What the wiki says of itself: its address and how many pages it holds."
  (handler-case
      (let ((data (call "list_pages" (arguments "limit" 1))))
        (format nil "Dexio wiki at ~a~@[, ~a pages~]. /dexio QUERY searches; the tools dexio_search, ~
                     dexio_read, dexio_list, dexio_write, dexio_edit and dexio_append do the rest."
                (root)
                (or (nlk:json-value data :integer "total") (nlk:json-value data :integer "count"))))
    (dexio-error (condition)
      (format nil "not connected: ~a" (dexio-error-detail condition)))))

(defun run-command (args session-id)
  "One /dexio invocation: a query answers what the wiki holds, no argument the connection."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (let ((data (call "search_pages"
                          (arguments "query" query
                                     "limit" (result-limit)
                                     "matches_per_page" +matches-per-page+))))
          (if (null (hits data))
              (format nil "No page of the wiki matched ~s." query)
              (hit-lines data :limit (result-limit))))
        (status-line))))

;;; --- the schemas, as the original's TOOLS carries them -----------------------------

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC REQUIRED-P) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name (nlk:json-object "type" type
                                                                     "description" doc))))
         "required" (coerce (loop for property in properties
                                   when (fourth property) collect (first property))
                            'vector))))

(defparameter *search-schema*
  (tool-schema
   "Search the shared Dexio wiki (paths, titles and text). Matches a question's words, best first, with the matching lines."
   '(("query" "string" "Words to look for" t)
     ("folder" "string" "Only search this folder (optional)" nil)
     ("limit" "integer" "Most pages to return (default 10)" nil)))
  "The dexio_search tool, as the original's TOOLS has it.")

(defparameter *read-schema*
  (tool-schema
   "Read one page of the Dexio wiki: its markdown, plus its version and the pages it links to and from."
   '(("path" "string" "Page path, e.g. customers/acme" t)
     ("section" "string" "Only this heading's section (optional)" nil)))
  "The dexio_read tool, as the original's TOOLS has it.")

(defparameter *list-schema*
  (tool-schema
   "List the wiki's pages, or one folder's, with titles and descriptions."
   '(("folder" "string" "Folder to list (optional)" nil)))
  "The dexio_list tool, as the original's TOOLS has it.")

(defparameter *write-schema*
  (tool-schema
   "Create a page or replace its whole text in the Dexio wiki. Search first and update the existing page rather than creating a duplicate; use dexio_edit for part of a page. Link related pages with [[path]]."
   '(("path" "string" "Page path, e.g. decisions/pricing" t)
     ("text" "string" "The whole page, in markdown" t)
     ("note" "string" "One line: what changed and why" nil)
     ("base_version" "string" "Fail if the page changed since this version" nil)))
  "The dexio_write tool, as the original's TOOLS has it.")

(defparameter *edit-schema*
  (tool-schema
   "Change part of a Dexio page: replace an exact piece of text (old_text, unique on the page) with new_text."
   '(("path" "string" "Page path" t)
     ("old_text" "string" "Exact text to replace" t)
     ("new_text" "string" "The replacement text (required; an empty string deletes old_text)" t)
     ("note" "string" "One line: what changed and why" nil)))
  "The dexio_edit tool, as the original's TOOLS has it.")

(defparameter *append-schema*
  (tool-schema
   "Add text to the end of a Dexio page, creating it if needed (logs, lists)."
   '(("path" "string" "Page path" t)
     ("text" "string" "What to add" t)
     ("note" "string" "One line: what changed and why" nil)))
  "The dexio_append tool, as the original's TOOLS has it.")

;;; --- the declaration -------------------------------------------------------------

(nle:define-cell dexio
  (:section ("dexio")
    (:guide "paste a Dexio API key into api_key (or leave api_key_env naming the variable that holds it, DEXIO_API_KEY either way) -- get one at app.dexio.wiki under Settings > Agents; url is the server; auto_recall puts the pages matching each request in front of the model; next step: /dexio shows the connection")
    ("api_key" :secret
     :doc "a Dexio API key (dxk_...), from Settings > Agents on your wiki")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (DEXIO_API_KEY is read either way)")
    ("url" :string
     :doc "the Dexio server: https://app.dexio.wiki unless answered, or your own self-hosted one")
    ("agent" :string :default "nodecode"
     :doc "the name Dexio records on every page this cell writes")
    ("auto_recall" :boolean :default t
     :doc "before each request, put the wiki's pages that match the message in front of the model")
    ("max_results" :integer :default 4 :min 1
     :doc "how many pages one recall or search shows, 1 to 10")
    ("recall_chars" :integer :default 3000 :min 200
     :doc "the longest recall one request carries, in characters")
    ("timeout_seconds" :integer :default 6 :min 1
     :doc "one request's ceiling"))
  (:tool "dexio_search" *search-schema* 'tool-search)
  (:tool "dexio_read" *read-schema* 'tool-read)
  (:tool "dexio_list" *list-schema* 'tool-list)
  (:tool "dexio_write" *write-schema* 'tool-write)
  (:tool "dexio_edit" *edit-schema* 'tool-edit)
  (:tool "dexio_append" *append-schema* 'tool-append)
  (:help :dexio "dexio: a shared wiki as memory - dexio_search, dexio_read and dexio_write keep pages other agents also use" +primer+)
  (:command "dexio" 'run-command
            :catches 'dexio-error
            :description "Show the Dexio wiki this cell talks to, or search it"
            :argument-hint "QUERY")
  (:hook 'nle::read-harness-sections #'standing-primer)
  (:hook :round (nle:observer #'recall-at-round "dexio"))
  (:start (lambda ()
            (check-config)
            (clrhash *recall-cache*)
            (nle:on-stop (lambda () (clrhash *recall-cache*))))))
