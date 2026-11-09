;;;; cell.lisp --- Twenty CRM's four tools, over its REST and GraphQL APIs.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The model learns the verbs through the manual: while the cell runs,
;;;; (help :twenty-crm) answers the primer below, and every request's help
;;;; section carries one line naming it. Nothing hooks the provider path: a
;;;; Twenty instance speaks HTTP, and NLK:HTTP is the one seam.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "twenty-crm": {"base_url": "https://api.twenty.com",
;;;;                  "api_key_env": "TWENTY_API_KEY"}
;;;; A vetoed section ("enabled": false) installs nothing.
;;;;
;;;; Ported from TREE-Ind's Hermes Agent plugin (see NOTICE): the four tools
;;;; and their schemas, the auth modes, the path and api rules each one
;;;; enforces, the introspection query, and the shapes the tools answer with.
;;;; The plugin's Docker-managed self-hosting, its session hooks and its OAuth
;;;; client-credentials flow are the parts this rewrite leaves to the instance.

(in-package #:nodecode-twenty-crm)

;;; --- the manual ---------------------------------------------------------------

(defparameter *manual*
  "Twenty CRM is available through the nodecode-twenty-crm cell: four tools, which a person
can also call from eval in the twenty-crm package.
  twenty_describe_workspace  {include_oauth_discovery}
      The base URL the cell is pointed at, its REST, GraphQL and metadata endpoints, whether
      an API key is configured, and optionally the instance's OAuth discovery document.
      Call it first, and after a 401.
  twenty_rest  {path, method, params, body}
      One REST endpoint: /rest/companies, /rest/people, /rest/opportunities, or one
      record's own path. path must begin with /rest/. params are query parameters as an
      object ({\"limit\": 10, \"filter\": \"name[eq]:Acme\"}); body is the JSON a POST,
      PATCH or PUT carries. Twenty answers a paginated envelope: read data and the paging
      members.
  twenty_graphql  {query, variables, api, operation_name}
      Twenty's GraphQL API: api=core for CRM records and relations, api=metadata for
      objects, fields, views, dashboards, roles and workflows. A GraphQL error is a
      failure, listing the server's own messages.
  twenty_schema  {api, type_name, max_types}
      The live schema. Twenty object, field, input and enum names differ between
      workspaces, so read them before assuming any: type_name inspects one type, omission
      lists the type names. Api=metadata for the types a data-model change runs on.
/twenty status reports the base URL and whether the instance answers; /twenty doctor prints
the whole connection, endpoints included.
The auth modes are the API key (api_key_env or api_key_file) sent as a bearer token. Keep
GraphQL documents small and split them rather than aliasing one root field many times."
  "What (help :twenty-crm) answers while the cell runs.")

(defparameter +introspection-query+
  "query TwentyIntrospectionQuery {
  __schema {
    queryType { name }
    mutationType { name }
    types {
      kind name description
      fields(includeDeprecated: true) {
        name description defaultValue
        args { name description defaultValue type { ...TypeRef } }
        type { ...TypeRef }
      }
      inputFields { name description defaultValue type { ...TypeRef } }
      enumValues(includeDeprecated: true) { name description }
    }
  }
}
fragment TypeRef on __Type {
  kind name
  ofType { kind name ofType { kind name ofType { kind name ofType { kind name ofType { kind name } } } } }
}"
  "Twenty's schema, asked for in one document: the same query the original
plugin sends, so a workspace answers the same shape.")

(defparameter +note-chars+ 300
  "How much of Twenty's own error body a refusal quotes back.")

;;; --- answers, and the one condition ---------------------------------------------

(defun refuse (format-control &rest args)
  "The model's answer when a call cannot go on: the words, as the call's
failure, so the result fact records it failed and every transcript draws it so."
  (nle:failure (format nil "ERROR: ~a" (apply #'format nil format-control args))))

(defmacro answering (&body body)
  "BODY's text; a refusal becomes that text, never an unhandled error."
  `(handler-case (progn ,@body)
     (twenty-crm-error (condition)
       (nle:failure (format nil "ERROR: ~a" (twenty-crm-error-detail condition))))
     (error (condition)
       (nle:failure (format nil "ERROR: Twenty CRM: ~a"
                             (nlk:one-line (princ-to-string condition)))))))

(defun answer-detail (data)
  "Twenty's own words for a failed call, clipped, or NIL: its body, text or
JSON, never the whole of it."
  (let* ((text (cond ((stringp data) data)
                     ((hash-table-p data) (nlk:encode-json-object data))
                     (t "")))
         (text (nlk:trimmed text)))
    (and (plusp (length text)) (nlk:clip text +note-chars+))))

;;; --- reading the section --------------------------------------------------------

(defun nonblank (text)
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun resolve-key (env file)
  "=> (VALUE . SOURCE): the API key the section names, read once here."
  (cond
    (env (let ((value (nonblank (nle::credential-env env))))
           (unless value
             (nlk:config-error "api_key_env names ~a, which is unset or empty" env))
           (cons value (format nil "environment variable ~a" env))))
    (file (let ((value (nonblank (ignore-errors
                                   (uiop:read-file-string (nlk:expand-home file))))))
            (unless value
              (nlk:config-error "api_key_file ~a cannot be read, or is empty" file))
            (cons value (format nil "file ~a" file))))
    (t (nlk:config-error "no API key: set api_key_env to the environment variable holding a Twenty API key, or api_key_file to a file holding one"))))

(defun read-settings (values table)
  "The declared VALUES, plus the API key the section names: read once, here."
  (declare (ignore table))
  (let* ((base (string-right-trim "/" (or (getf values :base-url) "")))
         (env (nonblank (getf values :api-key-env)))
         (file (nonblank (getf values :api-key-file)))
         (plist (copy-list values)))
    (when (and env file)
      (nlk:config-error "set one of api_key_env and api_key_file, not both"))
    (unless (or (uiop:string-prefix-p "http://" base) (uiop:string-prefix-p "https://" base))
      (nlk:config-error "base_url ~s has no http:// or https:// scheme: set it to the full base URL of the workspace, e.g. \"https://api.twenty.com\" or \"http://127.0.0.1:3000\"" base))
    (let ((key (resolve-key env file)))
      (setf (getf plist :base-url) base
            (getf plist :api-key) (car key)
            (getf plist :token-source) (cdr key)
            (getf plist :max-types) (or (getf values :max-types) 80)
            (getf plist :timeout) (or (getf values :timeout) 30)))
    plist))

;;; --- URLs and bodies ------------------------------------------------------------

(defun url-safe-octet-p (octet)
  (or (<= 48 octet 57) (<= 65 octet 90) (<= 97 octet 122)
      (member octet '(45 46 95 126))))

(defun percent-encode (text)
  "TEXT with everything but an unreserved character percent-encoded, as a query
parameter must be."
  (with-output-to-string (out)
    (loop for octet across (sb-ext:string-to-octets (princ-to-string text)
                                                    :external-format :utf-8)
          do (if (url-safe-octet-p octet)
                 (write-char (code-char octet) out)
                 (format out "%~2,'0X" octet)))))

(defun wire-text (value)
  "VALUE as the text a query parameter carries."
  (cond ((stringp value) value)
        ((eq value t) "true")
        ((numberp value) (princ-to-string value))
        ((null value) "")
        (t (princ-to-string value))))

(defun query-string (params)
  "PARAMS — a JSON object — as ?a=b&c=d, each array element its own pair; NIL
when there is nothing to send."
  (let ((pairs '()))
    (when (hash-table-p params)
      (loop for key being the hash-keys of params using (hash-value value)
            do (cond ((vectorp value)
                      (loop for item across value
                            do (push (cons key item) pairs)))
                     ((null value))
                     (t (push (cons key value) pairs)))))
    (when pairs
      (format nil "?~{~a~^&~}"
              (loop for (key . value) in (nreverse pairs)
                    collect (format nil "~a=~a" (percent-encode key)
                                    (percent-encode (wire-text value))))))))

(defun decode-body (body)
  "BODY — text, or the octets NLK:HTTP answers — as the value it carries: the
decoded JSON, the text when it is not JSON, or NIL when it is empty."
  (let* ((text (cond ((stringp body) body)
                      ((null body) "")
                      (t (nlk:body-text body))))
         (trimmed (nlk:trimmed text)))
    (cond ((zerop (length trimmed)) nil)
          (t (or (ignore-errors (nlk:decode-json trimmed)) trimmed)))))

;;; --- one exchange --------------------------------------------------------------

(defun succeeded-p (status)
  (and (integerp status) (< status 400)))

(defun api-request (method path &key payload params (authenticated t))
  "One Twenty exchange => (values DATA STATUS URL). DATA is the decoded body,
or the text when the answer was not JSON. A dial that fails is a refusal.
PATH is appended to the section's base URL, so a caller passes \"/rest/...\",
\"/graphql\", \"/metadata\" or \"/healthz\"."
  (let* ((url (concatenate 'string (setting :base-url) path (query-string params)))
         (headers (list (cons "accept" "application/json")))
         (content nil))
    (when authenticated
      (push (cons "authorization" (format nil "Bearer ~a" (setting :api-key))) headers))
    (when payload
      (setf content (nlk:encode-json-object payload))
      (push (cons "content-type" "application/json") headers))
    (handler-case
        (multiple-value-bind (body status)
            (nlk:http method url
                      :headers headers
                      :content content
                      :timeout (setting :timeout)
                      :connect-timeout (setting :timeout))
          (values (decode-body body) status url))
      (error (condition)
        (fail "~a ~a did not answer: ~a"
              (string-upcase (string method)) url
              (nlk:one-line (princ-to-string condition)))))))

(defun answered (url status data &key error)
  "The one answer shape every tool uses: what was asked, what came back, and
Twenty's own words when it went wrong."
  (let ((answer (nlk:json-object
                 "success" (and (succeeded-p status) (null error))
                 "url" url
                 "status_code" (or status 0)
                 "data" data)))
    (when error
      (setf (gethash "error" answer) error))
    (nlk:encode-json-object answer)))

(defun http-failure (url status data &optional prefix)
  "The answer to a call Twenty refused, quoting its own words."
  (answered url status data
            :error (format nil "~@[~a: ~]HTTP ~a~@[: ~a~]"
                           prefix status (answer-detail data))))

;;; --- twenty_describe_workspace ---------------------------------------------------

(defun describe-workspace (call)
  "The twenty_describe_workspace tool: what the cell is pointed at."
  (answering
    (let* ((base (setting :base-url))
           (summary (nlk:json-object
                     "success" t
                     "connection_mode" "external"
                     "base_url" base
                     "rest_base" (concatenate 'string base "/rest/")
                     "graphql_endpoint" (concatenate 'string base "/graphql")
                     "metadata_graphql_endpoint" (concatenate 'string base "/metadata")
                     "configured_auth_modes" (vector "api_key")
                     "active_auth_mode" "api_key"
                     "token_source" (setting :token-source)
                     "timeout_seconds" (setting :timeout))))
      (when (describe-call-include-oauth-discovery call)
        (multiple-value-bind (data status url)
            (api-request :get "/.well-known/oauth-authorization-server" :authenticated nil)
          (setf (gethash "oauth_discovery" summary)
                (nlk:json-object "success" (succeeded-p status)
                                 "url" url
                                 "status_code" (or status 0)
                                 "data" data))))
      (nlk:encode-json-object summary))))

;;; --- twenty_rest ------------------------------------------------------------------

(defun twenty-rest (call)
  "The twenty_rest tool: one REST endpoint, with the section's key."
  (answering
    (let ((path (nlk:trimmed (or (rest-call-path call) ""))))
      (unless (uiop:string-prefix-p "/rest/" path)
        (return-from twenty-rest
          (refuse "path ~s must begin with /rest/ (e.g. /rest/companies)" path)))
      (let ((method (intern (string-upcase (or (nonblank (rest-call-http-method call)) "GET"))
                            :keyword)))
        (multiple-value-bind (data status url)
            (api-request method path
                          :params (rest-call-params call)
                          :payload (rest-call-body call))
          (if (succeeded-p status)
              (answered url status data)
              (http-failure url status data)))))))

;;; --- twenty_graphql ---------------------------------------------------------------

(defun graphql-endpoint (api)
  (if (string= api "metadata") "/metadata" "/graphql"))

(defun graphql-request (query &key variables operation api)
  "One GraphQL document => (values DATA STATUS URL)."
  (let ((body (nlk:json-object "query" query
                                "variables" (or variables (nlk:json-object)))))
    (when (nonblank operation)
      (setf (gethash "operationName" body) operation))
    (api-request :post (graphql-endpoint api) :payload body)))

(defun graphql-answer (data status url &optional api)
  "The answer to a GraphQL call: Twenty answers errors with HTTP 200, so a
document that carries them is the call's failure."
  (let* ((errors (and (hash-table-p data) (ignore-errors (nlk:json-array data "errors"))))
         (failed (and errors (plusp (length errors))))
         (answer (nlk:json-object
                  "success" (and (succeeded-p status) (not failed))
                  "api" api
                  "url" url
                  "status_code" (or status 0)
                  "data" data)))
    (when failed
      (setf (gethash "error" answer) errors))
    (nlk:encode-json-object answer)))

(defun twenty-graphql (call)
  "The twenty_graphql tool: one document, core or metadata."
  (answering
    (let* ((query (nlk:trimmed (or (graphql-call-query call) "")))
           (api (or (nonblank (graphql-call-api call)) "core")))
      (unless (plusp (length query))
        (return-from twenty-graphql (refuse "query is required")))
      (multiple-value-bind (data status url)
          (graphql-request query
                            :variables (graphql-call-variables call)
                            :operation (graphql-call-operation-name call)
                            :api api)
        (graphql-answer data status url api)))))

;;; --- twenty_schema ------------------------------------------------------------------

(defun type-ref (value)
  "A GraphQL type reference, printed the way a schema writes it: ID!, [Company]."
  (cond ((null value) "Unknown")
        ((equal "NON_NULL" (nlk:json-value value :string "kind"))
         (format nil "~a!" (type-ref (nlk:json-value value :object "ofType"))))
        ((equal "LIST" (nlk:json-value value :string "kind"))
         (format nil "[~a]" (type-ref (nlk:json-value value :object "ofType"))))
        (t (or (nlk:json-value value :string "name")
               (nlk:json-value value :string "kind")
               "Unknown"))))

(defun schema-types (schema)
  "SCHEMA's types, without the introspection types a caller never writes."
  (remove-if (lambda (item)
               (uiop:string-prefix-p "__" (or (nlk:json-value item :string "name") "")))
             (coerce (or (nlk:json-array schema "types") #()) 'list)))

(defun described-fields (item key)
  (map 'vector
       (lambda (field)
         (nlk:json-object
          "name" (or (nlk:json-value field :string "name") "")
          "type" (type-ref (nlk:json-value field :object "type"))
          "args" (map 'vector
                      (lambda (arg)
                        (nlk:json-object "name" (or (nlk:json-value arg :string "name") "")
                                         "type" (type-ref (nlk:json-value arg :object "type"))))
                      (or (nlk:json-array field "args") #()))))
       (or (nlk:json-array item key) #())))

(defun describe-type (item)
  "One schema type as the model reads it."
  (nlk:json-object
   "name" (or (nlk:json-value item :string "name") "")
   "kind" (or (nlk:json-value item :string "kind") "")
   "description" (nlk:json-value item :string "description")
   "fields" (described-fields item "fields")
   "input_fields" (map 'vector
                       (lambda (field)
                         (nlk:json-object
                          "name" (or (nlk:json-value field :string "name") "")
                          "type" (type-ref (nlk:json-value field :object "type"))))
                       (or (nlk:json-array item "inputFields") #()))
   "enum_values" (map 'vector
                      (lambda (value) (or (nlk:json-value value :string "name") ""))
                      (or (nlk:json-array item "enumValues") #()))))

(defun twenty-schema (call)
  "The twenty_schema tool: the live schema, one type or the index of names."
  (answering
    (let* ((api (or (nonblank (schema-call-api call)) "core"))
           (asked (schema-call-max-types call))
           (maximum (max 1 (min (or (and (integerp asked) (plusp asked) asked)
                                    (setting :max-types))
                                500))))
      (multiple-value-bind (data status url)
          (graphql-request +introspection-query+
                            :operation "TwentyIntrospectionQuery"
                            :api api)
        (unless (succeeded-p status)
          (return-from twenty-schema (graphql-answer data status url api)))
        (let* ((payload (nlk:json-value data :object "data"))
               (schema (nlk:json-value payload :object "__schema")))
          (unless schema
            (return-from twenty-schema
              (refuse "the ~a schema came back without a __schema object" api)))
          (let* ((types (schema-types schema))
                 (target (nonblank (schema-call-type-name call)))
                 (result (nlk:json-object
                          "success" t
                          "api" api
                          "query_type" (nlk:json-value schema :string "queryType" "name")
                          "mutation_type" (nlk:json-value schema :string "mutationType" "name")
                          "total_types" (length types))))
            (cond
              (target
               (let ((item (find target types :test #'string=
                                         :key (lambda (one) (nlk:json-value one :string "name")))))
                 (if item
                     (setf (gethash "type" result) (describe-type item))
                     (setf (gethash "success" result) nil
                           (gethash "error" result)
                           (format nil "Type ~s not found in the ~a schema. Call twenty_schema without type_name for the list of names." target api)))))
              (t
               (let ((names (loop for item in types
                                  when (member (nlk:json-value item :string "kind")
                                               '("OBJECT" "INPUT_OBJECT" "ENUM") :test #'string=)
                                    collect (nlk:json-value item :string "name"))))
                 (setf (gethash "type_names" result)
                       (coerce (if (> (length names) maximum)
                                   (subseq names 0 maximum)
                                   names)
                               'vector))
                 (when (> (length names) maximum)
                   (setf (gethash "truncated" result) t
                         (gethash "truncated_note" result)
                         (format nil "Showing ~a of ~a types. Pass type_name for one type, api=metadata for the data-model side, or a larger max_types."
                                 maximum (length names)))))))
            (nlk:encode-json-object result)))))))

;;; --- /twenty ---------------------------------------------------------------------

(defun command-words (args)
  (remove "" (uiop:split-string (nlk:trimmed (or args "")) :separator '(#\Space #\Tab))
          :test #'string=))

(defun command-status ()
  "Whether the workspace answers, in one line a person reads."
  (handler-case
      (multiple-value-bind (data status) (api-request :get "/healthz" :authenticated nil)
        (declare (ignore data))
        (format nil "twenty-crm: ~a~%health: ~a~%api key: ~a~%timeout: ~as"
                (setting :base-url)
                (if (succeeded-p status)
                    (format nil "HTTP ~a, answering" status)
                    (format nil "HTTP ~a" status))
                (setting :token-source)
                (setting :timeout)))
    (error (condition)
      (format nil "twenty-crm: ~a did not answer: ~a"
              (setting :base-url) (nlk:one-line (princ-to-string condition))))))

(defun command-doctor ()
  "The whole connection, endpoints included."
  (let ((base (setting :base-url)))
    (format nil "twenty-crm: external connection~%base url: ~a~%rest: ~a~%graphql: ~a~%metadata: ~a~%api key: ~a~%timeout: ~as~%max types: ~a~%next step: ~a"
            base
            (concatenate 'string base "/rest/")
            (concatenate 'string base "/graphql")
            (concatenate 'string base "/metadata")
            (setting :token-source)
            (setting :timeout)
            (setting :max-types)
            "call twenty_describe_workspace, then twenty_schema before any mutation")))

(defun run-command (args session-id)
  "The /twenty command: status (the default) or doctor, as text."
  (declare (ignore session-id))
  (let ((action (string-downcase (or (first (command-words args)) "status"))))
    (values
     (cond ((string= action "status") (command-status))
           ((string= action "doctor") (command-doctor))
           (t (format nil "~a~%usage: /twenty status | /twenty doctor"
                      (if (string= action "help")
                          "The /twenty command reports the connection this cell is pointed at."
                          (format nil "/twenty does not know the action ~s." action)))))
     nil)))

;;; --- the cell --------------------------------------------------------------------

(nle:define-cell twenty-crm
  (:section ("twenty-crm")
    (:guide "a Twenty API key (in the workspace UI: Settings, Developers, API keys) named in api_key_env or kept in api_key_file; base_url is the workspace's base URL, scheme included - https://api.twenty.com for Twenty Cloud, http://127.0.0.1:3000 for a self-hosted stack; next step: ask what companies are in the CRM")
    (:one-of "api_key_env" "api_key_file")
    ("base_url" :string :default "https://api.twenty.com"
     :doc "the workspace's base URL, http:// or https:// included")
    ("api_key_env" :env
     :doc "the environment variable holding a Twenty API key (TWENTY_API_KEY)")
    ("api_key_file" :path
     :doc "a file holding a Twenty API key, which the file's own permissions guard")
    ("timeout" :integer :default 30 :min 1
     :doc "how long one Twenty request may take, in seconds")
    ("max_types" :integer :default 80 :min 1
     :doc "the cap on twenty_schema's list of type names; pass type_name for one type"))
  (:settings #'read-settings)
  (:help :twenty-crm
         "twenty-crm: twenty_describe_workspace, twenty_rest, twenty_graphql and twenty_schema read and write Twenty CRM records, schema and metadata"
         *manual*)
  (:tool "twenty_describe_workspace" (nlk:contract 'describe-call) #'describe-workspace)
  (:tool "twenty_rest" (nlk:contract 'rest-call) #'twenty-rest)
  (:tool "twenty_graphql" (nlk:contract 'graphql-call) #'twenty-graphql)
  (:tool "twenty_schema" (nlk:contract 'schema-call) #'twenty-schema)
  (:command "twenty" 'run-command
            :description "Report the Twenty CRM connection this cell is pointed at"
            :argument-hint "status | doctor"))
