;;;; cell-test.lisp --- lifecycle, the tools, the refusals, /index.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring over a stubbed Index: that
;;;; a start installs the nine tools, /index and the manual, and a stop takes
;;;; every one of them back; that a wrong setting refuses; that each read
;;;; asks the configured endpoint with the section's key and renders what
;;;; came back; that a write posts what the call carried and nowhere else;
;;;; that a refusal names what Index said; that a keyless cell refuses
;;;; without dialing; and that /index answers from the shell.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "index-network"
  (:help :index-network)
  (:command "index")
  (:config "api_url" "https://index.test" "api_key" "test-key")
  (:running
   (is (equal nodecode-index-network::+primer+ (nle:help :index-network)) "the manual")
   (is (nle::find-tool "index_read_intents") "the signals tool is registered")
   (is (nle::find-tool "index_list_opportunities") "the opportunities tool is registered")
   (is (nle::find-tool "index_read_docs") "the docs tool is registered")
   (is (equal "https://index.test" (nodecode-index-network::api-url)) "the section's endpoint")
   (is (= 20 (nodecode-index-network::setting :limit)) "limit defaults to twenty"))
  (:refused ("limit" 0) ("timeout" 0) ("api_url" 5))
  (:stopped
   (is (null (nle::find-tool "index_read_intents")) "stop unregisters every tool")
   (is (null (nle::find-tool "index_read_docs")))))

;;; --- reading ---------------------------------------------------------------------

(deftest index-network-cell-reading-signals-asks-the-callers-own-list ()
  (with-index ()
    (index-answering "/intents/list"
                     (answering-json
                      (nlk:json-object
                       "intents"
                       (vector (intent-row "6f2a-11" "a design partner for a prototype")
                               (intent-row "9c11-22" "a seed round in Berlin" :status "PAUSED")))))
    (is-carrying (answer (nle::message-content
                          (execute-wire-call "ix-1" "index_read_intents"
                                             "{\"limit\":5,\"query\":\"design\"}")))
      ("1. a design partner for a prototype (ACTIVE)")
      ("2. a seed round in Berlin (PAUSED)")
      ("id 6f2a-11"))
    (let ((request (first (index-requests))))
      (is (equal "POST" (first request)))
      (is (equal "https://index.test/intents/list" (second request)) "the plugin's path")
      (is (equal "Bearer test-key" (index-header request "authorization")) "the key sent")
      (let ((body (index-body request)))
        (is (= 5 (gethash "limit" body)) "the call's page size")
        (is (equal "design" (gethash "q" body)) "the call's text")
        (is (eq nil (gethash "archived" body)) "archived stays false unless asked")))))

(deftest index-network-cell-a-community-read-asks-that-community ()
  (with-index ()
    (index-answering "/networks/net-7/intents"
                     (answering-json
                      (nlk:json-object "intents" (vector (intent-row "aa-1" "a cofounder")))))
    (is-carrying (answer (nle::message-content
                          (execute-wire-call "ix-2" "index_read_intents"
                                             "{\"network_id\":\"net-7\",\"limit\":3}")))
      ("1. a cofounder (ACTIVE)"))
    (is (equal "GET" (first (first (index-requests)))) "a community read is a GET")
    (is (equal "https://index.test/networks/net-7/intents?limit=3"
               (second (first (index-requests)))) "community path, page in the query")))

(deftest index-network-cell-the-docs-answer-whichever-shape-index-sends ()
  (with-index ()
    (index-answering "/docs"
                     (answering-json
                      (nlk:json-object "signals"
                                       "A signal says what you are looking for.")))
    (is-carrying (answer (nle::message-content
                          (execute-wire-call "ix-3" "index_read_docs" "{\"topic\":\"signals\"}")))
      ("The topics Index documents:")
      ("A signal says what you are looking for."))
    (is (equal "https://index.test/docs?topic=signals" (second (first (index-requests)))))))

;;; --- writing ---------------------------------------------------------------------

(deftest index-network-cell-creating-a-signal-posts-the-persons-own-words ()
  (with-index ()
    (index-answering "/intents"
                     (answering-json (nlk:json-object "id" "new-9" "status" "ACTIVE")))
    (is-carrying (answer (nle::message-content
                          (execute-wire-call "ix-4" "index_create_intent"
                                             "{\"description\":\"a design partner\"}")))
      ("Signal created: new-9 (ACTIVE)")
      ("https://index.network/i/new-9" "a deep link to the new signal"))
    (let* ((request (first (index-requests)))
           (body (index-body request)))
      (is (equal "POST" (first request)))
      (is (equal "https://index.test/intents" (second request)))
      (is (equal "a design partner" (gethash "description" body)))
      (is (eq nil (gethash "networkIds" body)) "no community named, so none is sent"))))

(deftest index-network-cell-an-opportunity-is-accepted-by-id ()
  (with-index ()
    (index-answering "/opportunities/op-3/status" (answering-json (nlk:json-object)))
    (is (equal "Opportunity op-3 is accepted.
  https://index.network/o/op-3"
               (nle::message-content
                (execute-wire-call "ix-5" "index_update_opportunity"
                                   "{\"opportunity_id\":\"op-3\",\"status\":\"accepted\"}"))))
    (let ((request (first (index-requests))))
      (is (equal "PATCH" (first request)) "a status move is a PATCH")
      (is (equal "https://index.test/opportunities/op-3/status" (second request)))
      (is (equal "accepted" (gethash "status" (index-body request)))))))

(deftest index-network-cell-listing-opportunities-names-who-and-why ()
  (with-index ()
    (index-answering "/opportunities"
                     (answering-json
                      (nlk:json-object
                       "opportunities"
                       (vector (opportunity-row "op-3" "Ari the builder" :status "pending")))))
    (is-carrying (answer (nle::message-content
                          (execute-wire-call "ix-6" "index_list_opportunities" "{\"limit\":10}")))
      ("1. Ari the builder (pending)")
      ("id op-3"))
    (is (equal "https://index.test/opportunities?limit=10" (second (first (index-requests)))))))

;;; --- refusals --------------------------------------------------------------------

(deftest index-network-cell-a-refusal-names-what-index-said ()
  (with-index ()
    (index-answering "/intents/list" (answering-error "session is not valid" :status 401))
    (is-carrying (answer (nle::message-content
                          (execute-wire-call "ix-7" "index_read_intents" "{}")))
      ("ERROR: INDEX-NETWORK-ERROR" "the refusal is a failure the model reads")
      ("HTTP 401" "with the status")
      ("session is not valid" "and Index's own sentence"))
    (is (search "HTTP 401" nodecode-index-network::*last-error*) "and /index keeps it")))

(deftest index-network-cell-without-a-key-refuses-and-never-dials ()
  (with-index ("api_key" "")
    (is-carrying (answer (nle::message-content
                          (execute-wire-call "ix-8" "index_read_intents" "{}")))
      ("ERROR: INDEX-NETWORK-ERROR")
      ("no Index API key" "the refusal names what to set")
      ("INDEX_API_KEY" "and the variable it reads"))
    (is (null (index-requests)) "nothing was dialed")
    (is (search "no Index API key" (indexnetwork:signals)) "the verb refuses the same way")))

(deftest index-network-cell-a-page-size-out-of-range-refuses-before-the-wire ()
  (with-index ()
    (is (search "limit must be at most 100"
                (nle::message-content (execute-wire-call "ix-9" "index_read_intents"
                                                         "{\"limit\":500}"))))
    (is (null (index-requests)) "nothing was dialed")))

;;; --- /index ----------------------------------------------------------------------

(deftest index-network-cell-slash-answers-status-and-the-newest-signals ()
  (with-index ()
    (is-carrying (line (nodecode-index-network::run-slash "status" "s1"))
      ("index-network: https://index.test") ("key set") ("verbs: signals"))
    (is-carrying (line (nodecode-index-network::run-slash "bogus" "s1"))
      ("bogus is not one of its verbs"))
    (is (null (index-requests)) "a status line and a bare word dial nothing")
    (is-carrying (line (nodecode-index-network::run-slash "" "s1"))
      ("index-network: https://index.test" "bare /index says where it points")
      ("No signals here yet." "and what it found"))
    (index-answering "/intents/list"
                     (answering-json (nlk:json-object
                                      "intents" (vector (intent-row "aa-1" "a designer")))))
    (is-carrying (line (nodecode-index-network::run-slash "signals designer" "s1"))
      ("1. a designer (ACTIVE)"))
    (let ((body (index-body (car (last (index-requests))))))
      (is (equal "designer" (gethash "q" body)) "the words after the verb are its query"))))
