;;;; cell.lisp --- Cognee memory: recall, remember and forget over Cognee's HTTP API.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; A rewrite of topoteretes's Hermes memory-provider plugin (see NOTICE).
;;;; The six tool schemas, the JSON shapes of the wire and the two-phase
;;;; forget are the original's; the harness seams are Nodecode's: the section
;;;; is the config, the tools are NLE:REGISTER-TOOL registrations, and
;;;; /cognee is the shell's way to the same server.
;;;;
;;;; One wire, Cognee's own HTTP API (a local server on 127.0.0.1:8011, or a
;;;; remote one with a key):
;;;;   POST /api/v1/recall            JSON    query, top_k, datasets, scope, search_type
;;;;   POST /api/v1/remember          multipart  datasetName, run_in_background, data
;;;;   POST /api/v1/forget            JSON    dataset | datasetId+dataId
;;;;   GET  /api/v1/datasets          the datasets this principal sees
;;;;   GET  /api/v1/datasets/<id>/data[/<id>/raw]   a dataset's documents, and one's text
;;;; Authenticated with X-Api-Key when a key is set (the server enforces it
;;;; even on localhost; one with auth off ignores it).

(defpackage #:nodecode-cognee
  (:use #:cl)
  (:export #:cognee-error #:cognee-error-detail))

(in-package #:nodecode-cognee)

(nlk:define-peripheral cognee :not-running t :lock "cognee")

(defparameter +default-root+ "http://127.0.0.1:8011"
  "Where cognee's own server listens when the section names no base_url.")

(defparameter +preview-chars+ 240
  "How much of a stored document a forget candidate shows, as the original.")

(defparameter +max-scanned+ 100
  "Documents one forget-find reads before it answers, as the original.")

(defparameter +max-candidates+ 8
  "Candidates one forget-find lists, as the original.")

(defparameter +text-clip+ 500
  "How much of one recalled passage the model reads, as the original's line.")

(defvar *dataset* nil
  "The dataset recall and remember work in while the cell runs; the section's,
until cognee_switch_dataset moves it.")

;;; --- the section, read back ------------------------------------------------

(defun api-key ()
  "The key the section names, inline or by environment variable, else COGNEE_API_KEY."
  (or (let ((inline (setting :api-key))) (and inline (plusp (length inline)) inline))
      (let ((variable (setting :api-key-env)))
        (and variable (plusp (length variable)) (uiop:getenv variable)))
      (uiop:getenv "COGNEE_API_KEY")))

(defun root ()
  "The server this cell talks to, without a trailing slash."
  (let ((given (string-right-trim "/" (or (setting :base-url) ""))))
    (if (plusp (length given)) given +default-root+)))

(defun local-host-p (url)
  "True when URL points at this machine: a local cognee needs no key."
  (let* ((scheme-end (search "://" url))
         (rest (if scheme-end (subseq url (+ scheme-end 3)) url))
         (host-end (or (position-if (lambda (c) (member c '(#\/ #\: #\?))) rest) (length rest)))
         (host (string-downcase (subseq rest 0 host-end))))
    (member host '("127.0.0.1" "localhost" "::1" "[::1]" "0.0.0.0") :test #'equal)))

(defun check-config ()
  "Refuse a section that could never reach a server, naming the member."
  (when (and (not (local-host-p (root))) (null (api-key)))
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "cognee: a remote server needs a key: set COGNEE_API_KEY, or "
                                 "\"api_key\" (or \"api_key_env\") in the cognee section; "
                                 "a server on this machine needs none"))))

;;; --- the wire --------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return) answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((vectorp answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "detail")
        (nlk:json-value body :string "message")
        (nlk:json-value body :string "error")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun json-value-at (value key)
  "VALUE's KEY, one level down, whatever its JSON type."
  (when (hash-table-p value) (gethash key value)))

(defun auth-headers ()
  "Cognee's own header: X-Api-Key, sent whenever there is one."
  (let ((key (api-key)))
    (if (and key (plusp (length key))) (list (cons "x-api-key" key)) '())))

(defun call (method path &key body (timeout (setting :write-timeout-seconds)) extra-headers)
  "METHOD PATH on the configured server => the decoded answer. A failure
below HTTP or a refused status signals COGNEE-ERROR with the server's words."
  (let* ((url (concatenate 'string (root) path))
         (answer nil)
         (status nil))
    (handler-case
        (multiple-value-setq (answer status)
          (funcall (symbol-function 'nlk:http) method url
                   :headers (append extra-headers (auth-headers))
                   :content body
                   :timeout timeout
                   :connect-timeout (min 10 timeout)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "~a ~a: ~a" (string-upcase (string method)) path
              (nle:transport-failure-label condition url))))
    (if (< status 300)
        (decode-body answer)
        (fail "~a ~a: HTTP ~d~@[: ~a~]" (string-upcase (string method)) path status
              (body-error-text answer)))))

(defun digest-string (text)
  "TEXT's 32 hex characters: two FNV-1a 64 runs. One content keeps one
document name on the server, different content gets another -- a repeated
name carrying other content is a 409 the server answers with."
  (flet ((fnv (seed)
           (let ((hash seed))
             (loop for character across text
                   do (setf hash (logand (* (logxor hash (char-code character)) 1099511628211)
                                         #xffffffffffffffff)))
             hash)))
    (format nil "~16,'0x~16,'0x" (fnv 14695981039346656037) (fnv 1099511628211))))

(defun multipart-body (fields part)
  "FIELDS -- (NAME . VALUE) pairs -- and PART -- (NAME FILENAME CONTENT) --
as a multipart body and the boundary its content type announces."
  (let* ((boundary (concatenate 'string "nodecode-cognee-"
                                 (digest-string (format nil "~{~a~}-~a"
                                                        (mapcar #'cdr fields) (third part)))))
         (out (make-string-output-stream)))
    (flet ((line (control &rest args)
             (let ((text (apply #'format nil control args)))
               (loop for character across text
                     do (write-char (if (char= character #\Newline) #\Space character) out))
               (write-char #\Return out)
               (write-char #\Newline out))))
      (dolist (field fields)
        (line "--~a" boundary)
        (line "Content-Disposition: form-data; name=\"~a\"" (car field))
        (line "")
        (write-string (cdr field) out)
        (write-char #\Return out)
        (write-char #\Newline out))
      (line "--~a" boundary)
      (line "Content-Disposition: form-data; name=\"~a\"; filename=\"~a\"" (first part) (second part))
      (line "Content-Type: text/plain")
      (line "")
      (write-string (third part) out)
      (write-char #\Return out)
      (write-char #\Newline out)
      (line "--~a--" boundary))
    (values (get-output-stream-string out) boundary)))

;;; --- recall ----------------------------------------------------------------

(defun search-type-override (given)
  "GIVEN -- a call's or the section's search_type -- upper-cased, or NIL for
the server's own routing."
  (let ((text (string-upcase (string-trim '(#\Space #\Tab) (or given "")))))
    (and (plusp (length text)) text)))

(defun read-result-text (item)
  "ITEM's memory text: the first readable text slot, in the original's order."
  (flet ((readable (value)
           (cond ((and (stringp value) (plusp (length (string-trim '(#\Space) value)))) value)
                 ((and (vectorp value) (plusp (length value))
                       (every #'stringp value))
                  (format nil "~{~a~^~%~}" (remove-if-not (lambda (s) (plusp (length s))) value)))
                 (t nil))))
    (cond ((stringp item) item)
          ((hash-table-p item)
           (or (loop for key in '("answer" "text" "content" "chunk_text" "summary")
                     for found = (readable (json-value-at item key))
                     when found return found)
               ""))
          (t ""))))

(defun recall-items (answer)
  "ANSWER's items: the array the endpoint answers, or the one object it wraps."
  (cond ((vectorp answer) (coerce answer 'list))
        ((hash-table-p answer)
         (if (nlk:json-value answer :string "error")
             (fail "recall: ~a" (nlk:json-value answer :string "error"))
             (list answer)))
        ((null answer) '())
        (t (list answer))))

(defun recall (query &key search-type top-k dataset)
  "QUERY against the graph scope of DATASET (this conversation's default) =>
the server's result items."
  (let ((body (nlk:json-object
               "query" query
               "top_k" (or top-k (setting :top-k))
               "datasets" (vector (or dataset *dataset*))
               "scope" (vector "graph")
               "search_type" (search-type-override (or search-type (setting :search-type))))))
    (recall-items (call :post "/api/v1/recall" :body (nlk:encode-json-object body)
                                        :timeout (setting :recall-timeout-seconds)))))

(defun render-recall (query items)
  "ITEMS as the lines the model reads, one per memory."
  (let ((texts (remove-if (lambda (text) (zerop (length text)))
                          (mapcar #'read-result-text items))))
    (if (null texts)
        (format nil "No relevant Cognee memory for ~s." query)
        (with-output-to-string (out)
          (format out "~d Cognee ~a for ~s:" (length texts)
                  (if (= 1 (length texts)) "memory" "memories") query)
          (dolist (text texts)
            (format out "~% - ~a" (subseq text 0 (min +text-clip+ (length text)))))))))

;;; --- remember -------------------------------------------------------------

(defun remember (content &key dataset)
  "Store CONTENT as one document in DATASET, the pipeline running server-side."
  (multiple-value-bind (body boundary)
      (multipart-body (list (cons "datasetName" (or dataset *dataset*))
                            (cons "run_in_background" "true"))
                      (list "data" (format nil "memory-~a.txt" (digest-string content)) content))
    (call :post "/api/v1/remember" :body body
          :extra-headers (list (cons "content-type"
                                      (format nil "multipart/form-data; boundary=~a" boundary)))
          :timeout (setting :write-timeout-seconds))))

;;; --- forget: find the candidates, then delete what the user confirmed -------

(defun datasets ()
  "Every dataset this principal sees, as (NAME . ID) pairs."
  (let ((answer (call :get "/api/v1/datasets")))
    (loop for row in (if (vectorp answer) (coerce answer 'list) (list answer))
          when (hash-table-p row)
            collect (cons (or (nlk:json-value row :string "name") "")
                          (or (nlk:json-value row :string "id") "")))))

(defun dataset-id (name)
  "NAME's id, or a refusal naming the dataset."
  (or (cdr (assoc name (datasets) :test #'equal))
      (fail "dataset ~s was not found" name)))

(defun dataset-data (dataset-id)
  "DATASET-ID's documents, (ID . NAME) pairs, newest first."
  (let ((answer (call :get (format nil "/api/v1/datasets/~a/data" dataset-id))))
    (loop for row in (if (vectorp answer) (coerce answer 'list) (list answer))
          when (hash-table-p row)
            collect (cons (or (nlk:json-value row :string "id") "")
                          (or (nlk:json-value row :string "name") "")))))

(defun raw-data (dataset-id data-id)
  "One document's stored text; a JSON-encoded string is unwrapped."
  (let ((text (call :get (format nil "/api/v1/datasets/~a/data/~a/raw" dataset-id data-id))))
    (if (and (stringp text) (plusp (length text)) (char= (char text 0) #\"))
        (or (ignore-errors (nlk:decode-json text)) text)
        (if (stringp text) text ""))))

(defun forget-terms (text)
  "TEXT's words, lower-cased, two letters or more, as the original's."
  (remove-if (lambda (word) (< (length word) 2))
             (uiop:split-string (string-downcase (or text ""))
                                :separator '(#\, #\Space #\Tab #\Newline))))

(defun forget-find (terms dataset)
  "DATASET's documents whose stored text holds one of TERMS, as candidate plists."
  (let* ((id (dataset-id dataset))
         (items (dataset-data id))
         (candidates '()))
    (loop for (data-id . name) in (subseq items 0 (min +max-scanned+ (length items)))
          do (let ((text (ignore-errors (raw-data id data-id))))
               (when (stringp text)
                 (let ((lowered (string-downcase text))
                       (matched (remove-if-not (lambda (term) (search term (string-downcase text)))
                                                terms)))
                   (when matched
                     (let ((first (search (first matched) lowered)))
                       (push (list :data-id data-id :name name :matched matched
                                   :preview (string-trim '(#\Space #\Newline #\Return #\Tab)
                                                         (subseq text (max 0 (- first (floor +preview-chars+ 3)))
                                                                 (min (length text)
                                                                      (+ (max 0 (- first (floor +preview-chars+ 3)))
                                                                         +preview-chars+)))))
                             candidates)))))))
    (subseq (sort candidates #'>
                  :key (lambda (candidate) (length (getf candidate :matched))))
            0 (min +max-candidates+ (length candidates)))))

(defun render-candidates (dataset candidates total)
  "CANDIDATES as the lines the model shows the user before one delete."
  (if (null candidates)
      (format nil "No stored document in ~s matched those terms (~d read)." dataset total)
      (with-output-to-string (out)
        (format out "~d candidate~:*~[s~;~:;s~] in ~s; delete only what the user confirms:~%"
                (length candidates) dataset)
        (dolist (candidate candidates)
          (format out " - data_id ~a (~a): ~a~%"
                  (getf candidate :data-id)
                  (getf candidate :name)
                  (getf candidate :preview))))))

(defun forget-everything (dataset)
  "Drop every document of DATASET, on the coarse endpoint the original uses."
  (call :post "/api/v1/forget"
        :body (nlk:encode-json-object (nlk:json-object "dataset" dataset
                                                        "everything" nil
                                                        "memory_only" nil))))

(defun forget-document (dataset data-id)
  "Delete exactly one document of DATASET."
  (call :post "/api/v1/forget"
        :body (nlk:encode-json-object (nlk:json-object "datasetId" (dataset-id dataset)
                                                       "dataId" data-id))))

;;; --- the tools -------------------------------------------------------------

(defmacro answering (&body body)
  "BODY's answer, or the refusal it signals as the `ERROR:' line the model repairs from."
  `(handler-case (progn ,@body)
     ((or error sb-sys:deadline-timeout) (condition)
       (nle:failure (format nil "ERROR: cognee: ~a" condition)))))

(defun argument-text (args name &key (required t))
  "ARGS' NAME as a trimmed string, or a refusal naming it when REQUIRED."
  (let ((value (gethash name args)))
    (cond ((and (stringp value) (plusp (length (string-trim '(#\Space) value))))
           (string-trim '(#\Space) value))
          (required (fail "~a is required" (substitute #\_ #\- name)))
          (t nil))))

(defun argument-integer (args name default &key (ceiling 20))
  (let ((value (gethash name args)))
    (if (integerp value) (max 1 (min value ceiling)) default)))

(defun argument-boolean (args name)
  "NAME's boolean: only JSON true is true, as the original reads a payload."
  (let ((value (gethash name args)))
    (and (eq value t) t)))

(defun argument-strings (args name)
  "NAME's array of strings, as a list; a lone string reads as one."
  (let ((value (gethash name args)))
    (cond ((vectorp value) (remove-if-not #'stringp (coerce value 'list)))
          ((stringp value) (list value))
          (t '()))))

(defun tool-recall (args)
  (answering
    (let* ((query (argument-text args "query"))
           (items (recall query
                           :search-type (argument-text args "search_type" :required nil)
                           :top-k (argument-integer args "top_k" (setting :top-k))
                           :dataset (argument-text args "dataset" :required nil))))
      (render-recall query items))))

(defun tool-remember (args)
  (answering
    (let* ((content (argument-text args "content"))
           (dataset (or (argument-text args "dataset" :required nil) *dataset*))
           (answer (remember content :dataset dataset))
           (status (nlk:json-value answer :string "status")))
      (format nil "Content stored in Cognee (~a).~@[ status ~a~]"
              dataset (or status "completed")))))

(defun tool-forget (args)
  (answering
    (let* ((action (string-downcase (or (argument-text args "action") "")))
           (dataset (or (argument-text args "dataset" :required nil) *dataset*)))
      (cond
        ((equal action "find")
         (let ((terms (forget-terms (argument-text args "terms"))))
           (when (null terms) (fail "action='find' requires terms describing the content"))
           (let* ((id (dataset-id dataset))
                  (items (dataset-data id)))
             (declare (ignore id))
             (render-candidates dataset (forget-find terms dataset) (length items)))))
        ((equal action "forget")
         (unless (argument-boolean args "confirm")
           (fail "deletion requires confirm=true, after the user has confirmed"))
         (cond
           ((argument-boolean args "everything_in_dataset")
            (forget-everything dataset)
            (format nil "Dataset ~s deleted." dataset))
           (t
            (let ((ids (argument-strings args "data_ids")))
              (when (null ids) (fail "action='forget' requires data_ids (from action='find')"))
              (let ((deleted (loop for id in ids
                                   when (handler-case (progn (forget-document dataset id) t)
                                          ((or error sb-sys:deadline-timeout) () nil))
                                     collect id)))
                (format nil "Deleted ~d document~:*~[s~;~:;s~]~@[ from ~s~]: ~{~a~^, ~}."
                        (length deleted) (and deleted dataset) deleted))))))
        (t (fail "specify action='find' (list candidates by terms) or action='forget' (delete confirmed data_ids)"))))))

(defun tool-switch-dataset (args)
  (answering
    (let ((action (string-downcase (or (argument-text args "action") ""))))
      (cond
        ((equal action "list")
         (let ((rows (datasets)))
           (if (null rows)
               "No dataset is visible to this key."
               (with-output-to-string (out)
                 (format out "Datasets (~d):" (length rows))
                 (dolist (row rows)
                   (format out "~% - ~a~@[ (id ~a)~]~:[~; *current*~]"
                           (car row) (cdr row) (equal (car row) *dataset*)))))))
        ((equal action "current")
         (format nil "This conversation writes to and recalls from the ~s dataset." *dataset*))
        ((equal action "switch")
         (let ((target (argument-text args "dataset")))
           (unless (member target (mapcar #'car (datasets)) :test #'equal)
             (fail "dataset ~s was not found: cognee_switch_dataset with action='list' shows the ones there are" target))
           (setf *dataset* target)
           (format nil "This conversation now uses the ~s dataset." target)))
        ((equal action "reset")
         (setf *dataset* (setting :dataset))
         (format nil "Back to the configured dataset, ~s." *dataset*))
        (t (fail "specify action='list', 'current', 'switch' or 'reset'"))))))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC ENUM-OR-NIL REQUIRED-P) ... --
as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc enum) in properties
                                   nconc (list name (let ((property (nlk:json-object
                                                                     "type" type "description" doc)))
                                                      (when enum
                                                        (setf (gethash "enum" property)
                                                              (coerce enum 'vector)))
                                                      property))))
         "required" (coerce (loop for property in properties
                                   when (fifth property) collect (first property))
                            'vector))))

(defparameter *recall-schema*
  (tool-schema
   "Search Cognee's persistent knowledge graph for relevant information. Use for questions that may depend on prior conversations, stored facts, project context, or knowledge already captured by Cognee."
   '(("query" "string" "Natural language query to search for." nil t)
     ("search_type" "string" "Optional Cognee SearchType override, for example GRAPH_COMPLETION, RAG_COMPLETION, CHUNKS, CHUNKS_LEXICAL, TEMPORAL, or FEELING_LUCKY. CHUNKS returns matching stored text directly -- fast, no LLM in the loop. GRAPH_COMPLETION (the default) synthesizes an answer with an LLM per query, which can be slow on local models; prefer CHUNKS when a query times out or raw excerpts are enough." nil nil)
     ("top_k" "integer" "Maximum number of results to return. Default: provider config." nil nil)
     ("dataset" "string" "Optional Cognee dataset name. Defaults to this conversation's dataset." nil nil)))
  "The cognee_recall tool, as the original's RECALL_SCHEMA has it.")

(defparameter *remember-schema*
  (tool-schema
   "Persist important content into Cognee's knowledge graph. Use when the user explicitly asks to remember, store, save, or preserve a durable fact or decision."
   '(("content" "string" "Text content to store permanently." nil t)
     ("dataset" "string" "Optional Cognee dataset name. Defaults to this conversation's dataset." nil nil)))
  "The cognee_remember tool, as the original's REMEMBER_SCHEMA has it.")

(defparameter *forget-schema*
  (tool-schema
   "Delete specific content from Cognee memory when the user asks to forget something (e.g. 'forget what we said about tennis'). Two-phase: call with action='find' and terms describing what to forget -- it lists candidate documents with previews; show them to the user, then call with action='forget', the confirmed data_ids, and confirm=true to delete exactly those documents (irreversible). Dataset-wide deletion requires an explicit user request: action='forget' with everything_in_dataset plus confirm=true."
   '(("action" "string" "find: list matching documents. forget: delete listed ids." ("find" "forget") t)
     ("terms" "string" "For action='find': words or phrases describing the content to forget; candidates are matched against their raw stored text." nil nil)
     ("data_ids" "array" "For action='forget': the data ids to delete, exactly as returned by action='find'." nil nil)
     ("confirm" "boolean" "Required true for action='forget'. Only set after the user has seen the candidates and confirmed the deletion." nil nil)
     ("dataset" "string" "Optional dataset name. Defaults to this conversation's dataset." nil nil)
     ("everything_in_dataset" "boolean" "Delete the whole dataset instead of individual documents. Only when the user explicitly asked to clear the entire dataset." nil nil)))
  "The cognee_forget tool, as the original's FORGET_SCHEMA has it.")

(defparameter *switch-schema*
  (tool-schema
   "Move this conversation to another Cognee dataset. action='list' shows the datasets visible to this principal, 'current' reports the active one, 'switch' re-points recall, remember and forget at the named dataset, 'reset' returns to the configured default."
   '(("action" "string" "list, current, switch, or reset." ("list" "current" "switch" "reset") t)
     ("dataset" "string" "For action='switch': the target dataset name." nil nil)))
  "The cognee_switch_dataset tool, as the original's SWITCH_DATASET_SCHEMA has it.")

;;; --- the manual and /cognee -------------------------------------------------

(defparameter +primer+
  "Cognee keeps a knowledge graph of what a user stored: documents are remembered into a dataset,
cognified server-side, and found again by meaning. (help :cognee) is this page.
  cognee_recall {query, search_type?, top_k?, dataset?}   passages near a question
  cognee_remember {content, dataset?}                     store one durable fact
  cognee_forget {action, terms?|data_ids?, confirm?, dataset?, everything_in_dataset?}
                                                          find candidates, then delete confirmed ones
  cognee_switch_dataset {action, dataset?}                list, current, switch, reset
Recall before answering anything that may depend on earlier sessions. Use CHUNKS as the
search_type when an answer is needed fast or a GRAPH_COMPLETION query timed out. Forget is
two-phase on purpose: find first, show the candidates to the user, delete only what they
confirm. The cognee section of config.jsonc names the server (base_url: a local cognee on
127.0.0.1:8011, or a remote one, which needs a key from COGNEE_API_KEY) and the dataset
these memories live in."
  "What (help :cognee) answers while the cell runs.")

(defun status-line ()
  (format nil "Cognee memory: ~a, dataset ~s~@[, key set~].~%~
               /cognee QUERY recalls; the tools cognee_recall, cognee_remember, cognee_forget ~
               and cognee_switch_dataset do the rest."
          (root) *dataset* (and (api-key) t)))

(defun run-command (args session-id)
  "One /cognee invocation: a query recalls what Cognee holds, no argument the server."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (answering (render-recall query (recall query)))
        (progn (running-settings) (status-line)))))

;;; --- the cell ---------------------------------------------------------------

(nle:define-cell cognee
  (:section ("cognee")
    (:guide "base_url names the server: a local cognee on http://127.0.0.1:8011, or a remote one, which needs a key from COGNEE_API_KEY; dataset is where these memories live; next step: /cognee shows the server and its dataset")
    ("base_url" :string
     :doc "the Cognee API address: http://127.0.0.1:8011 unless answered")
    ("api_key" :secret
     :doc "the Cognee API key a remote server wants; a server on this machine needs none")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (COGNEE_API_KEY is read either way)")
    ("dataset" :string :default "nodecode"
     :doc "the dataset these memories live in; cognee_switch_dataset moves one conversation off it")
    ("search_type" :string
     :doc "the Cognee SearchType every recall asks for, e.g. CHUNKS for fast raw passages; blank lets Cognee route the query")
    ("top_k" :integer :default 10 :min 1
     :doc "how many passages one recall asks for unless the call says otherwise")
    ("recall_timeout_seconds" :integer :default 120 :min 1
     :doc "a recall's ceiling: a GRAPH_COMPLETION search runs an LLM per query")
    ("write_timeout_seconds" :integer :default 120 :min 1
     :doc "a remember, forget or dataset lookup's ceiling"))
  (:help :cognee "cognee: cognee_recall searches a Cognee knowledge graph; cognee_remember and cognee_forget keep it" +primer+)
  (:command "cognee" 'run-command
            :description "Show the Cognee server and dataset, or recall from it"
            :argument-hint "QUERY")
  (:tool "cognee_recall" *recall-schema* 'tool-recall)
  (:tool "cognee_remember" *remember-schema* 'tool-remember)
  (:tool "cognee_forget" *forget-schema* 'tool-forget)
  (:tool "cognee_switch_dataset" *switch-schema* 'tool-switch-dataset)
  (:start (lambda ()
            (check-config)
            (setf *dataset* (setting :dataset))
            (nle:on-stop (lambda () (setf *dataset* nil))))))
