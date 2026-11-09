;;;; cell.lisp --- one MCP read tool, its ids, and what is new since last time.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A cron job that asks an MCP server "anything new?" spends a model call
;;;; to be told "no" most of the time. This cell answers that question in
;;;; Lisp, before any call: FLEET_NEWS calls one tool on the endpoint the
;;;; section names, keeps the ids it has already seen in a small state file
;;;; per watch, and reports only the items whose id was never seen. Nothing
;;;; new answers the gate line {"wakeAgent": false} -- the stock wake gate a
;;;; scheduler reads -- so a job that watches a mailbox, a feed or a queue
;;;; wakes only when there is something to read.
;;;;
;;;; It compares ids, not the tool's output: a read tool's answer carries
;;;; volatile fields (timestamps, request ids, signed urls) that change on
;;;; every call, and an item that disappears is not news. The pieces:
;;;;
;;;;   the call      one MCP session over streamable HTTP: initialize,
;;;;                 notifications/initialized, then one tools/call, on
;;;;                 ((setting :server-url)); a bearer token, when the
;;;;                 endpoint wants one, is named by an environment variable
;;;;                 in ((setting :token-env)) -- the key itself is not config
;;;;   the items     every outermost object carrying a scalar id field, at any
;;;;                 depth of the answer, including a JSON document a relay
;;;;                 wrapped in a string; nothing inside an item is searched
;;;;   the envelope  successful/success false, or a non-empty error, is a
;;;;                 failed read: the state is left alone, so nothing is
;;;;                 marked as seen by a call that failed
;;;;   the state     <state dir>/<watch>.state.json, ids oldest first, written
;;;;                 whole through a temp file and a rename; a corrupt file is
;;;;                 a refusal, never a silent fresh start
;;;;
;;;; The rewrite keeps the original's contract -- the config shape, the
;;;; baseline-on-first-run rule, the gate line, the bounded id set, the
;;;; failure semantics -- and adds the two things a cell gets for free: the
;;;; endpoint is the section (so it is one setting, not a file per watch) and
;;;; the verb is also a tool the model calls.

(in-package #:nodecode-fleet-policy)

(defparameter +tool-name+ "fleet_news")

(defparameter +primer+
  "fleet_news asks one MCP read tool what it has, and answers only what nobody has seen:
  (fleet-news :tool \"LIST_MAIL\" :arguments ((\"limit\" . 50)) :id-field \"id\" :show-fields '(\"subject\"))
      The endpoint is the fleet-policy section's server_url; the section's token_env names the
      environment variable holding a bearer token, when the endpoint wants one. The first call
      for a watch records a baseline and reports nothing; every later call reports the items
      whose id was never seen, and answers {\"wakeAgent\": false} when there are none, which is
      what a cron job's wake gate reads to skip its model call. \"/fleet-policy\" lists what each
      watch remembers, and \"/fleet-policy LIST_MAIL\" prints what a call would report now
      without recording it (a dry run). One watch per tool is enough: the state is keyed on the
      tool (or on :watch, for two watches of one tool with different arguments)."
  "What (help :fleet-policy) answers while the cell runs.")

(defparameter +gate-line+ "{\"wakeAgent\": false}"
  "What a watch answers when nothing is new: the stock wake gate a scheduler
reads before it spends a model call. The same text the original printed.")

(defparameter +default-id-field+ "id"
  "The field an item is identified by, unless the caller names another.")

(defparameter +default-show-fields+ '("title")
  "What a report shows of a new item, unless the caller names other fields.")

(defparameter +default-max-new+ 20
  "How many new items one report lists; the rest are counted.")

(defparameter +default-max-seen+ 5000
  "How many ids one watch remembers before the oldest unseen ones go.")

(defparameter +timeout-seconds+ 60
  "One HTTP exchange's ceiling, connect and read alike.")

(defparameter +state-version+ 1)
(defparameter +max-value-chars+ 120)
(defparameter +depth-cap+ 8
  "How deep the readers look into one answer: deeper is a document, not an item.")

;;; --- small readers ------------------------------------------------------------

(defun now-iso ()
  "This instant, UTC, as 2026-01-31T09:00:00Z."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            year month day hour minute second)))

(defun trimmed (text)
  "TEXT without surrounding white space, or NIL when it is not a non-empty string."
  (let ((text (and (stringp text) (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
    (and text (plusp (length text)) text)))

(defun scalar-json-value-p (value)
  "True when VALUE is a JSON scalar an id or a field may be: a string, a number, true."
  (or (stringp value) (integerp value) (floatp value)))

(defun empty-json-p (value)
  "True when VALUE carries nothing: null, false, \"\", {}, []."
  (or (null value)
      (equal value "")
      (and (hash-table-p value) (zerop (hash-table-count value)))
      (and (vectorp value) (not (stringp value)) (zerop (length value)))))

(defun short-value (value &optional (limit +max-value-chars+))
  "VALUE as one line of at most LIMIT characters: a string as it is, anything
else as its JSON text."
  (let* ((text (cond ((stringp value) value)
                      ((or (integerp value) (floatp value) (eq value t))
                       (princ-to-string value))
                      ((hash-table-p value) (nlk:encode-json-object value))
                      ((vectorp value) (nlk:encode-json-object value))
                      (t (princ-to-string value))))
         (one (nlk:one-line text :cap limit)))
    one))

(defun watch-slug (name)
  "NAME as a file name: letters, digits, dot, dash and underscore, at most 64."
  (let ((slug (map 'string
                   (lambda (char)
                     (if (or (alphanumericp char) (find char "._-")) char #\-))
                   name)))
    (subseq slug 0 (min 64 (length slug)))))

;;; --- the state file -----------------------------------------------------------

(defun state-directory ()
  "The folder the state files live in: the section's, else fleet-policy/ in the home."
  (uiop:ensure-directory-pathname
   (or (and *fleet-policy* (setting :state-dir))
       (nlk:home "fleet-policy"))))

(defun state-file (name)
  (merge-pathnames (format nil "~a.state.json" (watch-slug name)) (state-directory)))

(defun read-state (path)
  "Two values: the ids PATH remembers (oldest first) and whether there is a
state at all. A corrupt file refuses rather than start over: starting over
would swallow whatever arrived in between."
  (if (null (probe-file path))
      (values '() nil)
      (let* ((doc (ignore-errors (nlk:decode-json (uiop:read-file-string path))))
             (seen (and (hash-table-p doc) (gethash "seen" doc))))
        (unless (and (vectorp seen) (not (stringp seen)))
          (fail "the watch state ~a is corrupt; delete it to record a new baseline"
                (namestring path)))
        (values (loop for id across seen collect (princ-to-string id)) t))))

(defun write-state (path tool id-field seen)
  "Record SEEN as PATH, whole, through a temp file and a rename."
  (let ((doc (make-hash-table :test #'equal)))
    (setf (gethash "version" doc) +state-version+
          (gethash "tool" doc) tool
          (gethash "id_field" doc) id-field
          (gethash "updated_at" doc) (now-iso)
          (gethash "seen" doc) (coerce seen 'vector))
    (ensure-directories-exist path)
    (let ((tmp (make-pathname :name (format nil ".~a.tmp" (pathname-name path))
                               :type nil :defaults path)))
      (with-open-file (out tmp :direction :output :if-exists :supersede
                               :external-format :utf-8)
        (write-string (nlk:encode-json-object doc) out)
        (terpri out))
      (uiop:rename-file-overwriting-target tmp path)))
  seen)

(defun merge-seen (seen current max-seen)
  "SEEN plus the ids of CURRENT it lacks, bounded: past MAX-SEEN the oldest
ids the tool no longer returns go first, never a live one, so an id is
reported once and never twice."
  (let* ((merged (append seen (remove-if (lambda (id) (member id seen :test #'equal)) current)))
         (live (remove-duplicates current :test #'equal))
         (limit (max max-seen (length live)))
         (excess (- (length merged) limit)))
    (if (<= excess 0)
        merged
        (let ((kept '()))
          (dolist (id merged (nreverse kept))
            (if (and (plusp excess) (not (member id live :test #'equal)))
                (decf excess)
                (push id kept)))))))

;;; --- an answer's items --------------------------------------------------------

(defun maybe-json (value)
  "VALUE, a string holding a JSON object or array, decoded; anything else
unchanged. Relays wrap a provider's answer that way."
  (let ((text (and (stringp value) (trimmed value))))
    (if (and text (find (char text 0) "{["))
        (let ((decoded (ignore-errors (nlk:decode-json text))))
          (if (null decoded) value decoded))
        value)))

(defun false-field-p (object name)
  "True when OBJECT carries NAME as JSON false, which decodes to NIL. A key that
is absent reads NIL too, so the presence is what tells them apart."
  (multiple-value-bind (value present) (gethash name object)
    (and present (null value))))

(defun object-error (object)
  "OBJECT's envelope error, or NIL when it reports none."
  (let ((value (gethash "error" object)))
    (unless (empty-json-p value) value)))

(defun analyze (payload id-field)
  "Two values: the items (the outermost objects carrying a scalar ID-FIELD, in
the order the answer gave them) and the envelope errors found on the way.
Inside an item nothing is searched: a nested id is an attribute, and an
`error' there is data about the item, not a failed call."
  (let ((items '()) (errors '() ))
    (labels ((walk (node path)
               (let ((node (maybe-json node)))
                 (cond
                   ((hash-table-p node)
                    (if (scalar-json-value-p (gethash id-field node))
                        (push node items)
                        (progn
                          (when (or (false-field-p node "successful")
                                    (false-field-p node "success")
                                    (object-error node))
                            (push (format nil "~a: ~a"
                                          (if (plusp (length path)) path "$")
                                          (let ((reported (object-error node)))
                                            (if reported
                                                (format nil "error: ~a" (short-value reported 300))
                                                "successful=false")))
                                  errors))
                          (maphash (lambda (key value)
                                     (walk value (format nil "~a.~a" path key)))
                                   node))))
                   ((and (vectorp node) (not (stringp node)))
                    (loop for index from 0
                          for value across node
                          do (walk value (format nil "~a[~d]" path index))))))))
      (walk payload "")
      (values (nreverse items) (nreverse errors)))))

(defun item-pairs (items id-field)
  "ITEMS as (ID . ITEM) pairs, first occurrence of an id only."
  (let ((seen '()) (pairs '()))
    (dolist (item items (nreverse pairs))
      (let ((id (short-value (gethash id-field item) 200)))
        (unless (member id seen :test #'equal)
          (push id seen)
          (push (cons id item) pairs))))))

(defun dotted-field (node name)
  "NAME's value under NODE, following a dotted path; NIL when it does not resolve."
  (block nil
    (let ((current node))
      (dolist (part (uiop:split-string name :separator ".") nil)
        (setf current (maybe-json current))
        (unless (hash-table-p current) (return nil))
        (multiple-value-bind (value present) (gethash part current)
          (unless present (return nil))
          (setf current value)))
      current)))

(defun find-field (node name &optional (depth 0))
  "NAME's scalar value under NODE: a dotted path, else the first scalar of that
name at any depth (breadth-first order of the walk), at most DEPTH-CAP deep."
  (cond
    ((> depth +depth-cap+) nil)
    ((hash-table-p node)
     (if (find #\. name)
         (dotted-field node name)
         (multiple-value-bind (value present) (gethash name node)
           (cond ((and present (scalar-json-value-p value)) value)
                 (t (loop for inner being the hash-values of node
                          for found = (find-field inner name (1+ depth))
                          when found return found))))))
    ((and (vectorp node) (not (stringp node)))
     (loop for inner across node
           for found = (find-field inner name (1+ depth))
           when found return found))
    (t (let ((decoded (maybe-json node)))
         (when (and (stringp node) (not (eq decoded node)))
           (find-field decoded name (1+ depth)))))))

(defun render-item (pair show-fields)
  "One report line for PAIR: its id, then each of SHOW-FIELDS it carries."
  (let ((parts (list (format nil "id=~a" (short-value (car pair) 80)))))
    (dolist (name show-fields)
      (let ((value (find-field (cdr pair) name)))
        (when (and value (not (equal value "")))
          (setf parts (append parts (list (format nil "~a: ~a" name (short-value value))))))))
    (format nil "- ~{~a~^ | ~}" parts)))

(defun clipped-line (pair show-fields)
  (nlk:one-line (render-item pair show-fields) :cap (* 2 +max-value-chars+)))

;;; --- one MCP session ----------------------------------------------------------

(defun rpc-message (id method params)
  "One JSON-RPC message."
  (let ((message (make-hash-table :test #'equal)))
    (setf (gethash "jsonrpc" message) "2.0"
          (gethash "method" message) method)
    (when id (setf (gethash "id" message) id))
    (when params (setf (gethash "params" message) params))
    message))

(defun endpoint ()
  "The MCP endpoint the section names, without a trailing slash."
  (let ((url (trimmed (setting :server-url))))
    (unless url
      (fail "no server_url is set: put the MCP endpoint in the fleet-policy section of the config, then (restart-cells)"))
    (unless (or (uiop:string-prefix-p "http://" url) (uiop:string-prefix-p "https://" url))
      (fail "server_url is ~s; an MCP endpoint is an http:// or https:// address" url))
    (string-right-trim "/" url)))

(defun request-headers ()
  (let ((headers '(("Content-Type" . "application/json")
                    ("Accept" . "application/json, text/event-stream")))
        (variable (trimmed (setting :token-env))))
    (if variable
        (let ((token (trimmed (uiop:getenv variable))))
          (unless token
            (fail "token_env names ~a, which is not set" variable))
          (acons "Authorization" (format nil "Bearer ~a" token) headers))
        headers)))

(defun sse-data (text)
  "The last event's data of a server-sent stream TEXT, or NIL when TEXT is not one."
  (let ((data (loop for line in (uiop:split-string text :separator '(#\Newline))
                    for trimmed = (string-left-trim '(#\Space #\Return) line)
                    when (uiop:string-prefix-p "data:" trimmed)
                      collect (string-left-trim '(#\Space) (subseq trimmed 5)))))
    (and data (first (last data)))))

(defun decode-rpc-body (text)
  "TEXT, an MCP response body, as one decoded JSON value: the body itself, or
the data of a server-sent event."
  (let ((text (and (stringp text) (trimmed text))))
    (when text
      (let ((json (if (find (char text 0) "{")
                      text
                      (sse-data text))))
        (and json (ignore-errors (nlk:decode-json json)))))))

(defun header-value (headers name)
  (cdr (assoc name headers :test #'string-equal)))

(defun rpc (message &key session)
  "One JSON-RPC exchange with the endpoint. => (values DOC SESSION) where DOC
is the decoded answer, NIL for a notification's empty one."
  (multiple-value-bind (body status headers)
      (nlk:http :post (endpoint)
                :headers (if session
                              (acons "Mcp-Session-Id" session (request-headers))
                              (request-headers))
                :content (nlk:encode-json-object message)
                :timeout +timeout-seconds+)
    (unless (and (integerp status) (<= 200 status 299))
      (fail "the MCP endpoint answered ~a~@[: ~a~]"
            status (and (stringp body) (short-value body 300))))
    (values (decode-rpc-body body)
            (or (header-value headers "Mcp-Session-Id") session))))

(defun rpc-refusal (doc)
  "Signal DOC's JSON-RPC error, when it carries one."
  (when (hash-table-p doc)
    (let ((error (gethash "error" doc)))
      (when (and error (not (empty-json-p error)))
        (fail "the MCP endpoint refused the call: ~a"
              (short-value (if (hash-table-p error) (gethash "message" error) error) 300))))))

(defun open-session ()
  "Initialise an MCP session; => its id, or NIL when the server keeps none."
  (let* ((params (make-hash-table :test #'equal))
         (client (make-hash-table :test #'equal)))
    (setf (gethash "name" client) "nodecode-fleet-policy"
          (gethash "version" client) "0.1.0"
          (gethash "protocolVersion" params) "2025-06-18"
          (gethash "capabilities" params) (make-hash-table :test #'equal)
          (gethash "clientInfo" params) client)
    (multiple-value-bind (doc session)
        (rpc (rpc-message 1 "initialize" params))
      (rpc-refusal doc)
      (rpc (rpc-message nil "notifications/initialized" nil) :session session)
      session)))

(defun parse-tool-result (tool result)
  "RESULT, an MCP CallToolResult, as the Lisp value the tool answered."
  (unless (hash-table-p result)
    (fail "~a answered no tool result" tool))
  (let* ((content (gethash "content" result))
         (text (if (and (vectorp content) (not (stringp content)))
                   (with-output-to-string (out)
                     (loop for block across content
                           when (hash-table-p block)
                             do (write-string (or (gethash "text" block) "") out)))
                   ""))
         (structured (let ((value (gethash "structuredContent" result)))
                       (if (null value) (gethash "structured_content" result) value))))
    (when (or (gethash "isError" result) (gethash "is_error" result))
      (fail "~a returned an error: ~a" tool
            (or (trimmed text) "(no detail)")))
    (cond (structured structured)
          ((zerop (length text)) nil)
          (t (or (ignore-errors (nlk:decode-json text))
                 (fail "~a did not answer JSON: ~a" tool (short-value text 200)))))))

(defun arguments-object (arguments)
  "ARGUMENTS as the JSON object one tools/call carries: a hash table as it is, an
(alist of (\"name\" . value)) made into one, anything else nothing."
  (cond
    ((hash-table-p arguments) arguments)
    ((and (consp arguments) (every #'consp arguments))
     (let ((table (make-hash-table :test #'equal)))
       (dolist (pair arguments table)
         (setf (gethash (princ-to-string (car pair)) table) (cdr pair)))))
    (t nil)))

(defun mcp-call (tool arguments)
  "One tools/call of TOOL with ARGUMENTS on the section's endpoint; => its result."
  (let ((session (open-session))
        (params (make-hash-table :test #'equal)))
    (setf (gethash "name" params) tool)
    (let ((arguments (arguments-object arguments)))
      (when (and arguments (plusp (hash-table-count arguments)))
        (setf (gethash "arguments" params) arguments)))
    (multiple-value-bind (doc)
        (rpc (rpc-message 2 "tools/call" params) :session session)
      (rpc-refusal doc)
      (parse-tool-result tool (gethash "result" doc)))))

;;; --- the verb -----------------------------------------------------------------

(defun watch-new-items (&key tool arguments (id-field +default-id-field+)
                           (show-fields +default-show-fields+)
                           watch (max-new +default-max-new+) dry-run)
  "Report the items TOOL answers whose ID-FIELD was never seen; the gate line
when there are none. With DRY-RUN, print what would be reported and record
nothing. The state file is named after WATCH, else after the tool."
  (let* ((tool (or (trimmed tool) (fail "a tool name is required")))
         (id-field (or (trimmed id-field) +default-id-field+))
         (show-fields (if (and (listp show-fields) show-fields)
                          (remove-if-not #'stringp show-fields)
                          +default-show-fields+))
         (name (or (trimmed watch) tool))
         (path (state-file name))
         (max-new (max 1 (or max-new +default-max-new+))))
    (multiple-value-bind (previous have-state) (read-state path)
      (multiple-value-bind (items errors) (analyze (mcp-call tool arguments) id-field)
        (when errors
          (fail "~a reported a failure: ~a" tool
                (format nil "~{~a~^; ~}" (subseq errors 0 (min 3 (length errors))))))
        (let* ((current (item-pairs items id-field))
               (known (if have-state previous '()))
               (new (remove-if (lambda (pair) (member (car pair) known :test #'equal))
                               current))
               (max-seen (or (and *fleet-policy* (setting :max-seen)) +default-max-seen+))
               (ids (mapcar #'car current)))
          (cond
            (dry-run
             (let ((lines (list (if have-state
                                     (format nil "dry run: ~a answered ~d id(s) in '~a', ~d new (nothing recorded):"
                                             tool (length ids) id-field (length new))
                                     (format nil "dry run: ~a answered ~d id(s) in '~a'; no state yet, so a real call would record them as the baseline:"
                                             tool (length ids) id-field))))
                   (shown (if have-state new current)))
               (when (null ids)
                 (setf lines (append lines (list (format nil "warning: no '~a' found in the answer; check id_field and the arguments" id-field)))))
               (dolist (pair (subseq shown 0 (min max-new (length shown))))
                 (setf lines (append lines (list (clipped-line pair show-fields)))))
               (when (> (length shown) max-new)
                 (setf lines (append lines (list (format nil "... and ~d more" (- (length shown) max-new))))))
               (format nil "~{~a~%~}" lines)))
            ((not have-state)
             (write-state path tool id-field (merge-seen '() ids max-seen))
             (format nil "baseline recorded: ~d id(s); nothing is reported on the first call~%~a"
                     (length ids) +gate-line+))
            ((null new) +gate-line+)
            (t
             (write-state path tool id-field (merge-seen previous ids max-seen))
             (let ((lines (list (format nil "~d new item(s) from ~a (watch '~a'):"
                                         (length new) tool name))))
               (dolist (pair (subseq new 0 (min max-new (length new))))
                 (setf lines (append lines (list (clipped-line pair show-fields)))))
               (when (> (length new) max-new)
                 (setf lines (append lines (list (format nil "... and ~d more (not shown; all marked as seen)"
                                                         (- (length new) max-new))))))
               (format nil "~{~a~%~}" lines)))))))))

;;; --- what the model calls -------------------------------------------------------

(nlk:define-contract news-call
  (:doc "Ask one MCP read tool what it has and hear only what nobody has seen
before: one report line per new item, or {\"wakeAgent\": false} when nothing
is new. The first call for a watch records a baseline and reports nothing;
the ids it keeps make sure an item is reported once. Use it before a scheduled
turn, or whenever the question is 'is there anything new?' rather than 'what is
there?'. The endpoint and its token come from the fleet-policy section.")
  ("tool" :text :required t
          :doc "the MCP tool to call, exactly as the server names it")
  ("arguments" :object
               :doc "the JSON arguments that tool takes, as an object ({}) when it takes none")
  ("id_field" :text :default "id"
              :doc "the field that identifies an item; found at any depth of the answer, dotted paths in show_fields included")
  ("show_fields" :array :of :string
                 :default (vector "title")
                 :doc "which fields of a new item the report shows: a name, or a dotted path like \"author.name\"")
  ("watch" :text :default ""
           :doc "the state this call is remembered under; the tool name by default")
  ("max_new" :integer :default 20
             :doc "how many new items the report lists")
  ("dry_run" :boolean :default nil
             :doc "report what a call would answer, and record nothing"))

(defun fleet-news (news)
  "The fleet_news tool: one watch, one answer."
  (watch-new-items :tool (news-call-tool news)
                    :arguments (news-call-arguments news)
                    :id-field (news-call-id-field news)
                    :show-fields (news-call-show-fields news)
                    :watch (news-call-watch news)
                    :max-new (news-call-max-new news)
                    :dry-run (news-call-dry-run news)))

;;; --- the operator's command -----------------------------------------------------

(defun state-line (path)
  "One line for the watch state PATH, as /fleet-policy lists it."
  (let* ((name (pathname-name path))
         (label (if (uiop:string-suffix-p name ".state") (subseq name 0 (- (length name) 6)) name))
         (doc (ignore-errors (nlk:decode-json (uiop:read-file-string path))))
         (seen (and (hash-table-p doc) (gethash "seen" doc)))
         (count (if (and (vectorp seen) (not (stringp seen))) (length seen) 0))
         (tool (and (hash-table-p doc) (gethash "tool" doc)))
         (updated (and (hash-table-p doc) (gethash "updated_at" doc))))
    (format nil "  ~a  ~d id(s)  ~a~@[  last ~a~]"
            label count (or tool "?") updated)))

(defun watches-report ()
  "Every watch this home remembers, or a line saying there is none yet."
  (let* ((directory (state-directory))
         (files (and (probe-file directory)
                     (sort (directory (merge-pathnames "*.state.json" directory))
                           #'string< :key #'namestring))))
    (if (null files)
        (format nil "fleet-policy: no watch has run yet; the endpoint is ~a~%~a"
                (or (ignore-errors (endpoint)) "(no server_url set)")
                "ask for one with fleet_news, or /fleet-policy TOOL for a dry run")
        (format nil "fleet-policy: endpoint ~a, state in ~a~%~{~a~%~}"
                (or (ignore-errors (endpoint)) "(no server_url set)")
                (namestring directory)
                (mapcar #'state-line files)))))

(defun run-command (args session-id)
  "One /fleet-policy invocation: no argument lists what each watch remembers,
one names a tool and prints what a call to it would answer now."
  (declare (ignore session-id))
  (nlk:with-handlers ((fleet-policy-error (condition)
                        (format nil "fleet-policy: ~a" condition)))
    (let ((words (nlk:split-words args)))
      (if (null words)
          (watches-report)
          (watch-new-items :tool (first words) :dry-run t)))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell fleet-policy
  ;; The endpoint is the one thing every watch shares, so it is the section's
  ;; and not a file per watch: set it once -- in /settings, or on the page's
  ;; Settings tab -- and every watch asks that server and no other. The token
  ;; is named, never pasted here.
  (:section ("fleet-policy")
    (:guide "one MCP endpoint (streamable HTTP) and, when it wants a token, the NAME of the environment variable holding it; nothing else is required -- a watch is named when the model calls fleet_news, and its ids are kept under the state folder")
    ("server_url" :string :required t
                  :doc "the MCP endpoint every watch calls, an http:// or https:// address")
    ("token_env" :env
                 :doc "environment variable holding a bearer token for that endpoint, when it wants one")
    ("state_dir" :directory :default "fleet-policy"
                 :doc "folder the watch states live in, under the Nodecode home by default")
    ("max_seen" :integer :default +default-max-seen+ :min 1
                :doc "how many ids one watch remembers before the oldest it no longer returns are dropped"))
  ;; A server_url that is not a web address would fail every watch: refused at
  ;; the start, in the loader's own words, rather than at the first call.
  (:settings (lambda (values table &aux (url (trimmed (getf values :server-url))))
               (when (and url (not (or (uiop:string-prefix-p "http://" url)
                                       (uiop:string-prefix-p "https://" url))))
                 (nlk:config-error "server_url is ~s; an MCP endpoint is an http:// or https:// address"
                                   url))
               values))
  (:tool +tool-name+ (nlk:contract 'news-call) 'fleet-news)
  (:help :fleet-policy
         "fleet-policy: fleet_news reports only the items an MCP read tool never returned before"
         +primer+)
  (:command "fleet-policy" 'run-command
            :description "What each watch has seen, and what one tool would report now"
            :argument-hint "TOOL-NAME"))
