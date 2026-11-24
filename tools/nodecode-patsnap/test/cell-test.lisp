;;;; cell-test.lisp --- lifecycle, every tool, the command, and each refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Patsnap: the section
;;;; (an address that is not one refuses the start; no key refuses before the
;;;; first request and names the member to write), each tool through the real
;;;; dispatch seam against a stubbed server, that one session serves every
;;;; call, that the key rides a header and never the URL, that a lost session
;;;; and an HTTP refusal are said and not retried in a loop, and that the
;;;; server's own schema is one call away.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "patsnap"
  (:fixture with-wire)
  (:config "endpoint" +endpoint+ "api_key" "test-key" "api_key_env" +key-env+)
  (:help :patsnap)
  (:command "patsnap")
  (:running
   (is (equal +endpoint+ (nodecode-patsnap::setting :endpoint)) "the endpoint is what the section names")
   (is (equal "patsnap_search_patent" (nodecode-patsnap::tool-name "search_patent")) "a tool name is the prefix and the server's own name")
   (is (= 10 (length (nodecode-patsnap::registered-tools))) "nine server tools and the schema lookup")
   (dolist (name (nodecode-patsnap::registered-tools))
     (is (nle::find-tool name) (format nil "the ~a tool is registered" name))))
  (:stopped
   (dolist (name '("patsnap_search_patent" "patsnap_fetch_literature" "patsnap_tools"))
     (is (null (nle::find-tool name)) (format nil "stop unregisters ~a" name))))
  (:refused
   ("endpoint" "not a url at all")
   ("tool_prefix" "Bad")
   ("limit" 0)))

;;; --- a search --------------------------------------------------------------------

(deftest patsnap-cell-a-patent-search-answers-the-records-the-server-sent (with-patsnap ("endpoint" +endpoint+ "api_key" "test-key")
    (let ((text (call "patsnap_search_patent" "{\"semantic_query\":\"solid-state electrolyte\",\"limit\":5}")))
      (is (search "Found 2 patents" text))
      (is (search "US-11234567-B2" text) "the publication number a citation needs")
      (is (search "US-10987654-B2" text)))
    (is (equal "tools/call" (third (first (calls-to "tools/call")))))
    (let ((body (newest-body "tools/call")))
      (is (search "search_patent" body) "the server's own tool name reaches the wire")
      (is (search "solid-state electrolyte" body) "and the arguments go through whole")
      (is (search "\"limit\":5" body) "a limit the call names is kept"))))

(deftest patsnap-cell-a-search-that-names-no-limit-asks-for-the-configured-one
    (with-patsnap ("endpoint" +endpoint+ "api_key" "test-key" "limit" 3)
    (call "patsnap_search_patent" "{\"keywords\":[\"electrolyte\"]}")
    (is (search "\"limit\":3" (newest-body "tools/call")) "the section's limit bounds a casual search")
    (call "patsnap_search_literature" "{\"keywords\":[\"electrolyte\"],\"offset\":20}")
    (is (null (search "\"limit\"" (newest-body "tools/call"))) "a paging call keeps its own page")))

;;; --- the session and the key ---------------------------------------------------------

(deftest patsnap-cell-one-session-serves-every-call-and-the-key-rides-a-header
    (with-patsnap ("endpoint" +endpoint+ "api_key" "test-key")
    (call "patsnap_search_patent" "{\"semantic_query\":\"a\"}")
    (call "patsnap_fetch_patent" "{\"patent_id\":\"US-11234567-B2\"}")
    (is (= 1 (length (calls-to "initialize"))) "one initialize for the start, not one per call")
    (is (= 2 (length (calls-to "tools/call"))))
    (is (equal "sess-1" (cdr (assoc "mcp-session-id" (newest-headers "tools/call") :test #'string-equal))) "later calls carry the session the first answer issued")
    (is (equal "test-key" (cdr (assoc "apikey" (newest-headers "tools/call") :test #'string-equal))) "the key rides the apikey header")
    (is (null (search "test-key" (second (first (calls-to "tools/call"))))) "and never the URL")
    (is (= 2 nodecode-patsnap::*calls*) "two calls, two credits")))

(deftest patsnap-cell-a-missing-key-refuses-before-any-request
    (with-patsnap ("endpoint" +endpoint+ "api_key_env" +no-key-env+)
    (let ((text (call "patsnap_fetch_patent" "{\"patent_id\":\"US-1\"}")))
      (is (search "no Patsnap key" text))
      (is (search "api_key" text) "the refusal names the member to write")
      (is (search +no-key-env+ text) "and the variable it would read"))
    (is (null *calls*) "and nothing was dialled")))

(deftest patsnap-cell-a-lost-session-is-said-once-and-not-retried
    (with-patsnap ("endpoint" +endpoint+ "api_key" "test-key")
    (call "patsnap_search_patent" "{\"semantic_query\":\"a\"}")
    (script "tools/call:search_patent"
            (lambda (message) (declare (ignore message)) (values "{}" 404)))
    (let ((text (call "patsnap_search_patent" "{\"semantic_query\":\"a\"}")))
      (is (search "dropped this cell's session" text))
      (is (search "call again" text) "the model is told what to do"))
    (is (= 2 (length (calls-to "tools/call"))) "an answer, not a retry loop")
    (is (null nodecode-patsnap::*session*) "and the next call opens a new one")))

;;; --- the server's own schemas ---------------------------------------------------------

(deftest patsnap-cell-the-server-s-own-schema-is-one-call-away
    (with-patsnap ("endpoint" +endpoint+ "api_key" "test-key")
    (let ((one (call "patsnap_tools" "{\"tool\":\"search_patent\"}"))
          (all (call "patsnap_tools" "{}")))
      (is (search "Search patents." one) "one tool's description, as the server wrote it")
      (is (search "semantic_query" one) "and its own argument schema")
      (is (search "search_patent" all))
      (is (search "One patent." all) "the list names every tool"))
    (is (search "no tool named" (call "patsnap_tools" "{\"tool\":\"nope\"}")))))

(deftest patsnap-cell-an-event-stream-answer-is-read-as-a-json-one
    (with-patsnap ("endpoint" +endpoint+ "api_key" "test-key")
    (script "tools/call:search_literature"
            (lambda (message)
              (values (sse-body message "Found 1 paper on the stream.") 200 (session-headers))))
    (is (search "Found 1 paper on the stream."
                (call "patsnap_search_literature" "{\"keywords\":[\"electrolyte\"]}")))))

(deftest patsnap-cell-an-http-refusal-carries-the-status-and-never-the-key
    (with-patsnap ("endpoint" +endpoint+ "api_key" "test-key")
    (script "initialize"
            (lambda (message) (declare (ignore message))
              (values "{\"error\":\"invalid api key\"}" 401)))
    (let ((text (call "patsnap_search_patent" "{\"semantic_query\":\"a\"}")))
      (is (search "HTTP 401" text) "the status")
      (is (search "invalid api key" text) "and Patsnap's own sentence")
      (is (null (search "test-key" text)) "never the key"))
    (is (= 1 (length *calls*)) "a refusal is not retried")))

;;; --- the command -----------------------------------------------------------------

(deftest patsnap-cell-the-command-says-what-this-cell-is
    (with-patsnap ("endpoint" +endpoint+ "api_key" "test-key")
    (let ((status (nodecode-patsnap::run-command "" "s1"))
          (bogus (nodecode-patsnap::run-command "bogus" "s1")))
      (is (search "patsnap: https://mcp.test/patsnap" status))
      (is (search "key: set" status))
      (is (search "patsnap_search_patent" status) "it names the tools this start registered")
      (is (search "calls this start: 0" status) "and what it has spent")
      (is (search "not one of its choices" bogus)))
    (is (null *calls*) "the status command dials nothing")
    (is (search "Patsnap's server lists 2 tools" (nodecode-patsnap::run-command "tools" "s1")) "and `tools' asks the server")))
