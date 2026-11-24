;;;; contracts.lisp --- the four tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before cell.lisp, whose bodies read the records.

(in-package #:nodecode-twenty-crm)

(defparameter +graphql-apis+ '("core" "metadata")
  "The two GraphQL endpoints a workspace answers: CRM records, and metadata.")

(defparameter +http-methods+ '("GET" "POST" "PATCH" "PUT" "DELETE")
  "The methods twenty_rest offers. A method that is not one of these is
refused by the contract, before anything is sent.")

(nlk:define-contract describe-call
  (:doc "Inspect the configured Twenty CRM workspace: its base URL, its REST, GraphQL and metadata endpoints, whether an API key is configured, and optionally the OAuth discovery document the instance publishes. Use it before a new Twenty task, and after a 401, to see what the cell is actually pointed at.")
  ("include_oauth_discovery" :boolean :default t
   :doc "Fetch the instance's /.well-known/oauth-authorization-server document, unauthenticated, and carry it in the answer. Defaults to true"))

(nlk:define-contract rest-call
  (:doc "Call one Twenty REST endpoint with the configured API key. Use paths such as /rest/companies, /rest/people or /rest/opportunities; a path must begin with /rest/. Twenty's REST API answers a paginated envelope, so read 'data' and the paging members rather than assuming a bare array. For relations, metadata, views, dashboards and workflows use twenty_graphql instead.")
  ("path" :text :required t
   :doc "the REST path, beginning with /rest/ (/rest/companies, /rest/companies/c1, /rest/metadata/objects)")
  ("method" :string :slot http-method :default "GET" :options +http-methods+
   :doc "GET, POST, PATCH, PUT or DELETE; defaults to GET")
  ("params" :object
   :doc "query parameters as a JSON object, e.g. {\"limit\": 10, \"filter\": \"name[eq]:Acme\"}")
  ("body" :object
   :doc "the JSON request body a POST, PATCH or PUT carries"))

(nlk:define-contract graphql-call
  (:doc "Call Twenty's GraphQL API. Use api=core for CRM records and their relations, and api=metadata for objects, fields, views, dashboards, roles and workflow administration. Twenty schemas are workspace-specific: call twenty_schema for the exact object, field and input type names before writing a mutation. A GraphQL error comes back as a failure listing the server's own messages.")
  ("query" :text :required t
   :doc "the GraphQL query or mutation")
  ("variables" :object
   :doc "the operation's variables as a JSON object")
  ("api" :string :default "core" :options +graphql-apis+
   :doc "core (CRM records, /graphql) or metadata (/metadata); defaults to core")
  ("operation_name" :string :default ""
   :doc "the operation to run, when the document carries more than one"))

(nlk:define-contract schema-call
  (:doc "Introspect the live Twenty GraphQL schema. Twenty object, field, input and enum names differ between workspaces and versions, so read this before assuming any of them: pass type_name to see one type's fields and their exact types, or omit it for the list of type names. Use api=metadata for the types a mutation of the data model runs on.")
  ("api" :string :default "core" :options +graphql-apis+
   :doc "core or metadata; defaults to core")
  ("type_name" :string :default ""
   :doc "one exact GraphQL type to inspect (Company, CompanyCreateInput, ...); omit for the list of names")
  ("max_types" :integer
   :doc "the most type names to answer when type_name is omitted; overrides the operator's max_types for this one call"))
