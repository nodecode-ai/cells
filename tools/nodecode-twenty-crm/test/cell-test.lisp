;;;; cell-test.lisp --- the twenty-crm cell against a stubbed workspace.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every Twenty endpoint is a stubbed NLK:HTTP and the API key a stubbed
;;;; NLE::CREDENTIAL-ENV: nothing reaches a workspace, the environment or the
;;;; operator's files. Each tool is called the way a model's call reaches it —
;;;; through the real dispatch, contract and all — so the schema the provider
;;;; is shown and the body that runs are one shape.

(in-package #:nodecode.test)

(define-test-slice "twenty-crm" "TWENTY-CRM-CELL-"
  :start nodecode-twenty-crm:start-cell
  :defaults (("api_key_env" "TWENTY_API_KEY")
              ("base_url" "https://api.twenty.test")))

(defmacro with-twenty-key (&body body)
  "BODY with the key environment answering one fixed key."
  `(with-stubbed-fdefinition (nle::credential-env (name)
                              (and (equal name "TWENTY_API_KEY") "test-key"))
     ,@body))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-twenty-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD, URL and OPTIONS bound and answers (values TEXT
STATUS), as NLK:HTTP does. ASKED collects (METHOD URL CONTENT HEADERS),
oldest first, once BODY reads it."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url
                                               (getf options :content)
                                               (getf options :headers))
                                         ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((search ,fragment url) ,form))
                                         (t (error "unexpected Twenty request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +companies+
  (concatenate
   'string
   "{\"data\":{\"companies\":[{\"id\":\"c1\",\"name\":\"Acme\","
   "\"domainName\":\"acme.test\"}]},\"totalCount\":1,\"pageInfo\":"
   "{\"hasNextPage\":false,\"startCursor\":\"c1\",\"endCursor\":\"c1\"}}")
  "One page of companies, as GET /rest/companies answers it.")

(defparameter +schema+
  (concatenate
   'string
   "{\"data\":{\"__schema\":{\"queryType\":{\"name\":\"Query\"},"
   "\"mutationType\":{\"name\":\"Mutation\"},\"types\":["
   "{\"kind\":\"OBJECT\",\"name\":\"Company\",\"fields\":[{\"name\":\"id\","
   "\"type\":{\"kind\":\"NON_NULL\",\"ofType\":{\"kind\":\"SCALAR\",\"name\":\"ID\"}},"
   "\"args\":[]}]},"
   "{\"kind\":\"ENUM\",\"name\":\"OrderBy\"},"
   "{\"kind\":\"SCALAR\",\"name\":\"__Schema\"}]}}}")
  "A workspace's introspection answer: one object, one enum, one introspection type.")

(defun twenty-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun twenty-object (id tool arguments)
  "TOOL's answer to ARGUMENTS, decoded."
  (nlk:decode-json (twenty-answer id tool arguments)))

(defun asked-headers (asked)
  "The headers of the first request in ASKED, as an alist."
  (fourth (first asked)))

;;; --- the lifecycle, from the one declaration ----------------------------------------

(define-cell-lifecycle-tests "twenty-crm"
  (:config "api_key_env" "TWENTY_API_KEY")
  (:help :twenty-crm)
  (:command "twenty")
  (:fixture with-twenty-key)
  (:running
   (is (nle::find-tool "twenty_rest") "twenty_rest is registered")
   (is (nle::find-tool "twenty_graphql") "twenty_graphql is registered")
   (is (nle::find-tool "twenty_schema") "twenty_schema is registered")
   (is (nle::find-tool "twenty_describe_workspace") "twenty_describe_workspace is registered")
   (is (equal "https://api.twenty.test" (nodecode-twenty-crm::setting :base-url)) "the base URL is the one configured")
   (is (equal "test-key" (nodecode-twenty-crm::setting :api-key)) "and the key was read"))
  (:stopped
   (is (null (nle::find-tool "twenty_rest")) "stop unregisters the tools"))
  (:refused ("api_key_env" "") ("base_url" "api.twenty.test")))

(deftest twenty-crm-cell-without-a-key-refuses ()
  ;; The one setting no verb can work without: the section names a key, and a
  ;; section that names none is refused at start, before anything installs.
  (let ((condition (signals-error nlk:config-refusal (twenty-crm-start "api_key_env" ""))))
    (is (search "api_key" (princ-to-string condition)) "the refusal names what is missing")
    (is (null (nle::find-tool "twenty_rest")) "and installs nothing")))

(deftest twenty-crm-cell-refuses-a-base-url-without-a-scheme ()
  (let ((condition (signals-error nlk:config-refusal
                                  (twenty-crm-start "base_url" "api.twenty.test"))))
    (is (search "http" (princ-to-string condition)) "the refusal says what a base URL looks like")
    (is (null (nle::find-tool "twenty_graphql")) "and installs nothing")))

;;; --- twenty_describe_workspace ------------------------------------------------------

(deftest twenty-crm-cell-describes-the-workspace (with-twenty-key)
  (with-cell-stop ((twenty-crm-start))
    (with-twenty-wire (asked ("/.well-known/oauth-authorization-server"
                               (values "{\"issuer\":\"https://api.twenty.test\"}" 200)))
      (let ((object (twenty-object "d1" "twenty_describe_workspace" "{}")))
        (is (equal "https://api.twenty.test" (nlk:json-value object :string "base_url")))
        (is (equal "https://api.twenty.test/rest/" (nlk:json-value object :string "rest_base")))
        (is (equal "https://api.twenty.test/metadata"
                   (nlk:json-value object :string "metadata_graphql_endpoint")))
        (is (equal "api_key" (nlk:json-value object :string "active_auth_mode")))
        (is (eq t (nlk:json-value object :boolean "oauth_discovery" "success")) "the discovery document is carried in the answer")
        (is (search "environment variable TWENTY_API_KEY"
                    (nlk:json-value object :string "token_source")) "and says where the key came from, never the key itself")
        (is (not (search "test-key" (nlk:encode-json-object object))) "the key never reaches the answer")
        (is (null (cdr (assoc "authorization" (asked-headers asked) :test #'string=))) "the discovery document is asked for unauthenticated"))
      ;; Asking not to discover is one request fewer.
      (let ((object (twenty-object "d2" "twenty_describe_workspace"
                                    "{\"include_oauth_discovery\":false}")))
        (is (null (nlk:json-value object :object "oauth_discovery")) "no discovery document")
        (is (= 1 (length asked)) "and no second request")))))

;;; --- twenty_rest ---------------------------------------------------------------------

(deftest twenty-crm-cell-reads-a-rest-page (with-twenty-key)
  (with-cell-stop ((twenty-crm-start))
    (with-twenty-wire (asked ("/rest/companies" (values +companies+ 200)))
      (let* ((object (twenty-object "r1" "twenty_rest"
                                     "{\"path\":\"/rest/companies\",\"params\":{\"limit\":2}}"))
             ;; the answer carries the workspace's own body under data
             (body (nlk:json-value object :object "data"))
             (data (nlk:json-value body :object "data"))
             (companies (nlk:json-array data "companies")))
        (is (eq t (nlk:json-value object :boolean "success")))
        (is (= 200 (nlk:json-value object :integer "status_code")))
        (is (= 1 (length companies)) "the page's records come through")
        (is (equal "Acme" (nlk:json-value (aref companies 0) :string "name")))
        (is (= 1 (nlk:json-value body :integer "totalCount")) "and its paging envelope")
        (is (equal :get (first (first asked))) "a plain read is one GET")
        (is (search "/rest/companies?limit=2" (second (first asked))) "the caller's query parameters ride the URL")
        (is (equal "Bearer test-key"
                   (cdr (assoc "authorization" (asked-headers asked) :test #'string=))) "with the workspace's key")))))

(deftest twenty-crm-cell-refuses-a-path-outside-rest (with-twenty-key)
  (with-cell-stop ((twenty-crm-start))
    (with-twenty-wire (asked)
      (let ((answer (twenty-answer "r2" "twenty_rest" "{\"path\":\"/graphql\"}")))
        (is (search "must begin with /rest/" answer) "the rule is said, not guessed at")
        (is (null asked) "and nothing was sent")))))

(deftest twenty-crm-cell-reports-what-twenty-refused (with-twenty-key)
  (with-cell-stop ((twenty-crm-start))
    (with-twenty-wire (asked ("/rest/companies"
                               (values "{\"messages\":[\"invalid api key\"]}" 401)))
      (let ((object (twenty-object "r3" "twenty_rest" "{\"path\":\"/rest/companies\"}")))
        (is (null (nlk:json-value object :boolean "success")) "a refused call is a failed call")
        (is (= 401 (nlk:json-value object :integer "status_code")))
        (is (search "invalid api key" (nlk:json-value object :string "error")) "Twenty's own words come back, in its error member")))))

;;; --- twenty_graphql -------------------------------------------------------------------

(deftest twenty-crm-cell-posts-a-graphql-document (with-twenty-key)
  (with-cell-stop ((twenty-crm-start))
    (with-twenty-wire (asked ("/graphql" (values "{\"data\":{\"companies\":{\"edges\":[]}}}" 200)))
      (let* ((object (twenty-object "g1" "twenty_graphql"
                                     "{\"query\":\"query { companies { edges { node { id } } } }\",\"operation_name\":\"ListCompanies\"}"))
             (payload (nlk:decode-json (third (first asked)))))
        (is (eq t (nlk:json-value object :boolean "success")))
        (is (equal "core" (nlk:json-value object :string "api")))
        (is (equal :post (first (first asked))) "a GraphQL document is a POST")
        (is (search "companies" (nlk:json-value payload :string "query")) "the document rides the body")
        (is (equal "ListCompanies" (nlk:json-value payload :string "operationName")) "and the operation name with it"))
      ;; The query is required: a call without one never leaves.
      (let ((answer (twenty-answer "g2" "twenty_graphql" "{}")))
        (is (search "preflight" answer) "the contract refuses it before the body"))
      (is (= 1 (length asked))))))

(deftest twenty-crm-cell-calls-the-metadata-endpoint (with-twenty-key)
  (with-cell-stop ((twenty-crm-start))
    (with-twenty-wire (asked ("/metadata"
                               (values "{\"errors\":[{\"message\":\"Cannot query field nope\"}]}" 200)))
      (let ((object (twenty-object "g3" "twenty_graphql"
                                    "{\"query\":\"query { nope }\",\"api\":\"metadata\"}")))
        (is (search "/metadata" (second (first asked))) "api=metadata is the metadata endpoint")
        (is (null (nlk:json-value object :boolean "success")) "a GraphQL error is the call's failure, though Twenty answers HTTP 200")
        (is (search "Cannot query field nope" (nlk:encode-json-object object)) "the server's message is passed on")))))

;;; --- twenty_schema ---------------------------------------------------------------------

(deftest twenty-crm-cell-lists-and-inspects-schema-types (with-twenty-key)
  (with-cell-stop ((twenty-crm-start))
    (with-twenty-wire (asked ("/graphql" (values +schema+ 200)))
      (let ((object (twenty-object "s1" "twenty_schema" "{}")))
        (is (eq t (nlk:json-value object :boolean "success")))
        (is (equal "Query" (nlk:json-value object :string "query_type")))
        (is (= 2 (nlk:json-value object :integer "total_types")) "the introspection types are left out of the count")
        (is (equal "Company" (aref (nlk:json-array object "type_names") 0)) "the object names come back"))
      (let ((object (twenty-object "s2" "twenty_schema" "{\"max_types\":1}")))
        (is (= 1 (length (nlk:json-array object "type_names"))) "the per-call cap wins")
        (is (eq t (nlk:json-value object :boolean "truncated")) "and the cut is marked"))
      (let* ((object (twenty-object "s3" "twenty_schema" "{\"type_name\":\"Company\"}"))
             (type (nlk:json-value object :object "type"))
             (fields (nlk:json-array type "fields")))
        (is (equal "OBJECT" (nlk:json-value type :string "kind")))
        (is (equal "ID!" (nlk:json-value (aref fields 0) :string "type")) "a field's exact type, nullable and list markers included"))
      (let ((object (twenty-object "s4" "twenty_schema" "{\"type_name\":\"Absent\"}")))
        (is (null (nlk:json-value object :boolean "success")))
        (is (search "not found" (nlk:json-value object :string "error")))))))

;;; --- /twenty ----------------------------------------------------------------------------

(deftest twenty-crm-cell-command-reports-the-connection (with-twenty-key)
  (with-cell-stop ((twenty-crm-start))
    (with-twenty-wire (asked ("/healthz" (values "{\"status\":\"ok\"}" 200)))
      (let ((text (cell-entry "nodecode-twenty-crm" "twenty" "status")))
        (is (search "https://api.twenty.test" text) "status names the workspace")
        (is (search "HTTP 200" text) "and whether it answered"))
      (let ((text (cell-entry "nodecode-twenty-crm" "twenty" "doctor")))
        (is (search "/metadata" text) "doctor prints every endpoint")
        (is (search "TWENTY_API_KEY" text) "and where the key is read from"))
      (let ((text (cell-entry "nodecode-twenty-crm" "twenty" "restart")))
        (is (search "usage" text) "an unknown action says what the command does"))
      (is (= 1 (length asked)) "only status asks the workspace anything"))))

(deftest twenty-crm-cell-command-says-why-a-workspace-is-silent (with-twenty-key)
  (with-cell-stop ((twenty-crm-start))
    (with-twenty-wire (asked ("/healthz" (error "connection refused")))
      (let ((text (cell-entry "nodecode-twenty-crm" "twenty" "status")))
        (is (search "did not answer" text) "a silent workspace is said, not thrown")
        (is (search "api.twenty.test" text)))
      (is (= 1 (length asked)) "the one request that was tried"))))
