;;;; cell.lisp --- xAI Collections: one read-only search tool and /collections.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; A Nodecode rewrite of Julientalbot/hermes-xai's collections module (see
;;;; NOTICE). Two REST calls, both the original's, on one origin:
;;;;
;;;;   GET  https://api.x.ai/v1/collections       the team's collections
;;;;   POST https://api.x.ai/v1/documents/search  the passages nearest a query
;;;;
;;;; Read only: nothing is uploaded, created or deleted, and the key rides
;;;; only to api.x.ai. The original's other half, a Grok Voice composer in
;;;; Hermes Desktop, is a window rather than a wire, and is not ported.

(defpackage #:nodecode-xai-collections
  (:use #:cl)
  (:nicknames #:xai-collections)
  (:export #:xai-collections-error #:xai-collections-error-detail))

(in-package #:nodecode-xai-collections)

(nlk:define-peripheral xai-collections :not-running t)

(defparameter +api-root+ "https://api.x.ai/v1"
  "xAI's REST root: the one origin this cell talks to.")

(defparameter +collections-path+ "/collections"
  "GET: the collections this team indexed on console.x.ai.")

(defparameter +search-path+ "/documents/search"
  "POST: the best-matching passages of one query.")

(defparameter +default-limit+ 5
  "Passages one search returns unless the call or the section says otherwise.")

(defparameter +max-limit+ 20
  "The most passages one search may ask for, as the vendor caps it.")

(defparameter +max-chunk-chars+ 2000
  "How much of one passage an answer carries: the original's own cap.")

(defparameter +id-body-length+ 36
  "A collection id is collection_ and 36 characters of lowercase hex and dashes.")

;;; --- the section, read back --------------------------------------------------

(defun non-empty (text)
  "TEXT when it holds more than blanks, else NIL."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun api-key ()
  "The key the section names, inline or by environment variable, else XAI_API_KEY."
  (or (non-empty (setting :api-key))
      (let ((variable (non-empty (setting :api-key-env))))
        (and variable (non-empty (uiop:getenv variable))))
      (non-empty (uiop:getenv "XAI_API_KEY"))))

(defun default-ids ()
  "The collection ids the section searches when a call passes none."
  (setting :default-collection-ids))

(defun check-config ()
  "Refuse a section that could reach no team, naming the member."
  (unless (api-key)
    (error 'nlk:config-refusal
           :detail (concatenate 'string
                                 "xai-collections: an xAI API key is required: set XAI_API_KEY, or "
                                 "\"api_key\" (or \"api_key_env\") in the xai-collections section"))))

;;; --- the wire -----------------------------------------------------------------

(defun decode-body (answer)
  "ANSWER -- an HTTP body -- as a decoded value; an empty body is an empty object."
  (cond ((stringp answer)
         (if (plusp (length (nlk:trimmed answer)))
             (handler-case (nlk:decode-json answer) (error () (nlk:json-object)))
             (nlk:json-object)))
        ((hash-table-p answer) answer)
        (t (nlk:json-object))))

(defun body-error-text (answer)
  "What a failed answer says, for the one line the model reads."
  (let ((body (and (stringp answer) (ignore-errors (nlk:decode-json answer)))))
    (or (nlk:json-value body :string "error")
        (nlk:json-value body :string "message")
        (and (stringp answer) (subseq answer 0 (min 200 (length answer)))))))

(defun call (method path &key body)
  "METHOD PATH on xAI's API, BODY its JSON text => the decoded answer.
A failure below HTTP, or a status outside 2xx, signals XAI-COLLECTIONS-ERROR."
  (let ((key (api-key))
        (limit (setting :timeout-seconds))
        (url (concatenate 'string +api-root+ path))
        (answer nil)
        (status nil))
    (unless key
      (fail "no key: set XAI_API_KEY, or \"api_key\" in the xai-collections section"))
    (handler-case
        (multiple-value-setq (answer status)
          (nlk:http method url
                    :headers (list (cons "authorization" (format nil "Bearer ~a" key))
                                   (cons "content-type" "application/json"))
                    :content body
                    :timeout limit
                    :connect-timeout (min 10 limit)))
      ((or error sb-sys:deadline-timeout) (condition)
        (fail "xAI unreachable: ~a" (nle:transport-failure-label condition url))))
    (cond ((and (integerp status) (< status 300)) (decode-body answer))
          ((integerp status) (fail "xAI answered HTTP ~d~@[: ~a~]" status (body-error-text answer)))
          (t (fail "xAI answered nothing at ~a" url)))))

(defun collection-id-p (id)
  "Whether ID is an xAI collection id: collection_ and 36 characters of hex and dashes."
  (and (stringp id)
       (= (length id) (+ (length "collection_") +id-body-length+))
       (uiop:string-prefix-p "collection_" id)
       (every (lambda (character) (find character "0123456789abcdef-"))
              (subseq id (length "collection_")))))

(defun clean-ids (ids)
  "IDS -- a string of them separated by commas or blanks, a vector, or a list --
as a list of collection ids, refused when one is not shaped like one."
  (let* ((ids (cond ((stringp ids)
                      (uiop:split-string ids :separator '(#\, #\Space #\Tab #\Newline)))
                     ((vectorp ids) (coerce ids 'list))
                     ((listp ids) ids)
                     (t '())))
         (cleaned (loop for id in ids
                        for text = (and (stringp id) (non-empty id))
                        when text collect text)))
    (dolist (id cleaned)
      (unless (collection-id-p id)
        (fail "not an xAI collection id: ~s (expected collection_<uuid>)" id)))
    cleaned))

(defun clip (text chars)
  "TEXT cut at CHARS characters; the original's per-passage cap."
  (let ((text (or text "")))
    (if (<= (length text) chars) text (subseq text 0 chars))))

(defun list-collections ()
  "This team's collections, as the answer carries them: id, name, description, documents."
  (let ((body (call :get +collections-path+)))
    (loop for collection across (nlk:json-array body "collections")
          collect (nlk:json-object
                   :opt "id" (nlk:json-value collection :string "collection_id")
                   :opt "name" (nlk:json-value collection :string "collection_name")
                   :opt "description" (nlk:json-value collection :string "collection_description")
                   :opt "documents" (nlk:json-value collection :integer "documents_count")))))

(defun search-collections (query &key collection-ids limit filter)
  "The passages nearest QUERY in COLLECTION-IDS, else in the section's defaults,
else in every collection of the team. => (values MATCHES IDS)."
  (let ((query (non-empty query)))
    (unless query (fail "query is empty"))
    (let ((ids (or (clean-ids collection-ids)
                   (clean-ids (default-ids))
                   (remove nil (loop for collection in (list-collections)
                                     collect (nlk:json-value collection :string "id"))))))
      (unless ids
        (fail "this xAI team has no collection yet: create one on console.x.ai"))
      (let* ((limit (max 1 (min +max-limit+ (or limit (setting :default-limit)))))
             (payload (nlk:json-object
                       "query" query
                       "source" (nlk:json-object "collection_ids" (coerce ids 'vector))
                       "limit" limit
                       :opt "filter" (non-empty filter)))
             (body (call :post +search-path+ :body (nlk:encode-json-object payload))))
        (values
         (loop for match across (nlk:json-array body "matches")
               collect (nlk:json-object
                        "title" (or (nlk:json-value match :string "fields" "title")
                                    (nlk:json-value match :string "file_id")
                                    "")
                        :opt "file_id" (nlk:json-value match :string "file_id")
                        :opt "score" (nlk:json-value match :number "score")
                        :opt "page" (nlk:json-value match :integer "page_number")
                        "text" (clip (nlk:json-value match :string "chunk_content")
                                     +max-chunk-chars+)))
         ids)))))

;;; --- answers a person reads -----------------------------------------------------

(defun format-collections (collections)
  "COLLECTIONS as the lines /collections prints."
  (if (null collections)
      "No collection yet on this xAI team: create one on console.x.ai."
      (with-output-to-string (out)
        (format out "~d collection~:p on xAI:" (length collections))
        (dolist (collection collections)
          (format out "~% - [~a] ~a~@[ (~d documents)~]"
                  (or (nlk:json-value collection :string "id") "?")
                  (or (nlk:json-value collection :string "name") "(unnamed)")
                  (nlk:json-value collection :integer "documents"))))))

(defun format-matches (query matches &key (chars 400))
  "MATCHES as the lines /collections prints for one search."
  (if (null matches)
      (format nil "No passage matched ~s." query)
      (with-output-to-string (out)
        (format out "~d passage~:p for ~s:" (length matches) query)
        (dolist (match matches)
          (format out "~% - ~a~@[ (score ~,2f)~]~@[ p.~a~]~%   ~a"
                  (nlk:json-value match :string "title")
                  (nlk:json-value match :number "score")
                  (nlk:json-value match :integer "page")
                  (clip (nlk:json-value match :string "text") chars))))))

;;; --- the tool --------------------------------------------------------------------

(defun argument-string (args name)
  "ARGS' NAME as a trimmed non-empty string, or NIL."
  (let ((value (and (hash-table-p args) (gethash name args))))
    (and (stringp value) (non-empty value))))

(defun argument-integer (args name)
  "ARGS' NAME when it is an integer, else NIL."
  (let ((value (and (hash-table-p args) (gethash name args))))
    (and (integerp value) value)))

(defun argument-boolean (args name)
  "ARGS' NAME when it is JSON true, else NIL."
  (and (hash-table-p args) (eq t (gethash name args))))

(defun tool-search (args)
  "The xai_collections_search tool: the passages as JSON, or the collections
when the call asks for them, or one ERROR: line the model repairs from."
  (handler-case
      (if (argument-boolean args "list_collections")
          (nlk:encode-json-object
           (nlk:json-object "collections" (coerce (list-collections) 'vector)))
          (let ((query (argument-string args "query")))
            (multiple-value-bind (matches ids)
                (search-collections query
                                    :collection-ids (and (hash-table-p args)
                                                         (gethash "collection_ids" args))
                                    :limit (argument-integer args "limit")
                                    :filter (argument-string args "filter"))
              (nlk:encode-json-object
               (nlk:json-object "query" (or query "")
                                "collection_ids" (coerce ids 'vector)
                                "matches" (coerce matches 'vector))))))
    ((or error sb-sys:deadline-timeout) (condition)
      (nle:failure (format nil "ERROR: xai collections: ~a" condition)))))

(defun tool-schema (description properties)
  "DESCRIPTION and PROPERTIES -- (NAME TYPE DOC) ... -- as a tool schema."
  (list :description description
        :parameters
        (nlk:json-object
         "type" "object"
         "properties" (apply #'nlk:make-json-object
                             (loop for (name type doc) in properties
                                   nconc (list name
                                               (if (equal type "array")
                                                   (nlk:json-object
                                                    "type" "array"
                                                    "items" (nlk:json-object "type" "string")
                                                    "description" doc)
                                                   (nlk:json-object
                                                    "type" type
                                                    "description" doc)))))
         "required" (vector))))

(defparameter *search-schema*
  (tool-schema
   (concatenate 'string
                "Search the user's own documents indexed in xAI Collections (console.x.ai) and return "
                "the best-matching passages with their file title and score. Use it when the user "
                "refers to their uploaded documents, knowledge base or a named collection. Read only. "
                "Leave collection_ids empty to search the configured default collections, or all of "
                "them; pass list_collections=true to see which collections exist.")
   '(("query" "string" "What to look for, in natural language.")
     ("collection_ids" "array" "Optional collection ids (collection_<uuid>).")
     ("limit" "integer" "Passages to return (1-20, default 5).")
     ("filter" "string" "Optional metadata filter, AIP-160 syntax, e.g. year > 2024 AND type = \"contract\".")
     ("list_collections" "boolean" "Return the available collections instead of searching.")))
  "The xai_collections_search tool, as the original's COLLECTIONS_SEARCH_SCHEMA has it.")

;;; --- the manual and /collections ---------------------------------------------------

(defparameter +primer+
  "xAI Collections holds documents a team indexed on console.x.ai; upload them there, this cell only
reads. The tool is xai_collections_search {query, collection_ids?, limit?, filter?,
list_collections?}: it returns the best-matching passages with their file title, page and score.
Leave collection_ids out to search the section's default collections, or every collection of the
team; pass list_collections=true to see which collections exist and their ids. filter takes xAI's
AIP-160 metadata syntax (year > 2024 AND type = \"contract\"). Use it when the user refers to
documents they uploaded, a knowledge base or a named collection, and cite the passages' titles.
/collections lists the collections; /collections QUERY searches them from the shell.
The key is the xAI API key from XAI_API_KEY (or api_key in the xai-collections section) and it
rides only to api.x.ai."
  "What (help :xai-collections) answers while the cell runs.")

(defun run-command (args session-id)
  "One /collections invocation: no argument lists the collections, a query
searches them, and a refusal is the command layer's own failure line."
  (declare (ignore session-id))
  (let ((query (non-empty (or args ""))))
    (if query
        (multiple-value-bind (matches) (search-collections query)
          (format-matches query matches))
        (format-collections (list-collections)))))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell xai-collections
  (:section ("xai-collections")
    (:guide "an xAI API key is required: set XAI_API_KEY, or api_key (or api_key_env) here; default_collection_ids names the collections searched when the model passes none (empty = every collection of the team); next step: /collections lists them")
    ("api_key" :secret
     :doc "the xAI API key")
    ("api_key_env" :env
     :doc "the environment variable holding the xAI API key, when it is not written here (XAI_API_KEY is read either way)")
    ("default_collection_ids" :list
     :doc "collection_<uuid> ids searched when the model passes none; empty searches every collection")
    ("default_limit" :integer :default 5 :min 1
     :doc "passages one search returns unless the call says otherwise, at most 20")
    ("timeout_seconds" :integer :default 30 :min 1
     :doc "one request's ceiling"))
  (:help :xai-collections "xai-collections: xai_collections_search reads the documents you indexed in xAI Collections" +primer+)
  (:command "collections" 'run-command
            :description "List the xAI Collections, or search them"
            :argument-hint "QUERY")
  (:tool "xai_collections_search" *search-schema* 'tool-search)
  (:start (lambda () (check-config))))
