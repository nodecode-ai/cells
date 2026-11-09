;;;; cell-test.lisp --- lifecycle, the five tools, /corpus, and every refusal.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring, not Corpus: the config
;;;; surface (an address that is not an address refuses the start), each tool
;;;; through the real dispatch seam against a stubbed server, the session the
;;;; client claims and reuses, the key that rides when one is configured, an
;;;; answer framed as an event stream, and the refusal a rate-limited floor
;;;; gets.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "corpus"
  (:config "endpoint" "https://corpuslaw.us/api/mcp")
  (:fixture with-wire)
  (:help :corpus)
  (:command "corpus")
  (:running
   (is (nle::find-tool "law_search") "law_search is registered")
   (is (nle::find-tool "law_get_node") "law_get_node is registered")
   (is (nle::find-tool "law_coverage") "law_coverage is registered")
   (is (nle::find-tool "corpus_account") "corpus_account is registered")
   (is (nle::find-tool "corpus_call") "corpus_call is registered")
   (is (equal "https://corpuslaw.us/api/mcp" (nodecode-corpus::endpoint)) "the section's address is what the client dials"))
  (:stopped
   (is (null (nle::find-tool "law_search")) "stop unregisters law_search")
   (is (null (nle::find-tool "corpus_call")) "stop unregisters corpus_call"))
  (:refused
   ("endpoint" "")
   ("endpoint" "corpuslaw.us/api/mcp")
   ("timeout_seconds" 1)))

;;; --- the search -----------------------------------------------------------------

(deftest corpus-cell-search-hands-back-the-server-s-answer (with-corpus ()
    (let ((text (corpus-call-tool "law_search" "{\"query\":\"food truck from a driveway in Austin\"}")))
      (is (cl:search "Sec. 217.042" text) "the citation, verbatim")
      (is (cl:search "municipal permit" text)))
    (let* ((call (first (calls-of "tools/call")))
           (arguments (arguments-of call)))
      (is (equal "law.search" (getf call :tool)))
      (is (equal "food truck from a driveway in Austin"
                 (nlk:json-value arguments :string "query")))
      (is (null (nlk:json-value arguments :string "jurisdiction")) "no jurisdiction anywhere means none is sent")
      (is (equal "sess-1" (header-of call "mcp-session-id")) "the session the initialize handed out rides the call"))))

(deftest corpus-cell-the-section-s-jurisdiction-answers-when-a-call-names-none
    (with-corpus ("jurisdiction" "Austin, TX")
  (corpus-call-tool "law_search" "{\"query\":\"food truck\"}")
  (is (equal "Austin, TX" (nlk:json-value (arguments-of (first (calls-of "tools/call")))
                                             :string "jurisdiction")))
  (corpus-call-tool "law_search" "{\"query\":\"food truck\",\"jurisdiction\":\"Travis County, TX\"}")
  (is (equal "Travis County, TX" (nlk:json-value (arguments-of (first (calls-of "tools/call")))
                                                   :string "jurisdiction")) "a call's own jurisdiction wins")))

(deftest corpus-cell-one-session-serves-every-call (with-corpus ()
    (corpus-call-tool "law_search" "{\"query\":\"one\"}")
    (corpus-call-tool "law_coverage" "{}")
    (is (= 1 (length (calls-of "initialize"))) "one handshake, not one per call")
    (is (= 1 (length (calls-of "notifications/initialized"))))
    (is (= 2 (length (calls-of "tools/call"))))
    (dolist (call (calls-of "tools/call"))
      (is (equal "sess-1" (header-of call "mcp-session-id"))))))

;;; --- the provision, the coverage, the account -------------------------------------

(deftest corpus-cell-get-node-reads-the-citation-a-call-carries (with-corpus ()
    (let ((text (corpus-call-tool "law_get_node"
                                   "{\"citation\":\"Tex. Loc. Gov't Code Sec. 217.042\"}")))
      (is (cl:search "MOBILE FOOD VENDORS" text)))
    (let ((call (first (calls-of "tools/call"))))
      (is (equal "law.get_node" (getf call :tool)))
      (is (equal "Tex. Loc. Gov't Code Sec. 217.042"
                 (nlk:json-value (arguments-of call) :string "citation"))))))

(deftest corpus-cell-coverage-and-account-take-no-arguments (with-corpus ()
    (is (cl:search "Austin, TX" (corpus-call-tool "law_coverage" "{}")))
    (is (cl:search "anonymous floor" (corpus-call-tool "corpus_account" "{}")))
    (dolist (call (calls-of "tools/call"))
      (is (hash-table-p (arguments-of call)) "an empty object, not a missing one")
      (is (zerop (hash-table-count (arguments-of call)))))))

(deftest corpus-cell-call-passes-a-dotted-tool-and-its-own-object (with-corpus ()
    (let ((text (corpus-call-tool "corpus_call"
                                   "{\"tool\":\"formation.requirements\",\"arguments\":{\"entityType\":\"llc\",\"state\":\"MS\"}}")))
      (is (cl:search "stateFeeCents" text)))
    (let ((call (first (calls-of "tools/call"))))
      (is (equal "formation.requirements" (getf call :tool)))
      (is (equal "llc" (nlk:json-value (arguments-of call) :string "entityType")))
      (is (equal "MS" (nlk:json-value (arguments-of call) :string "state"))))))

;;; --- the key, the wire, the refusals ---------------------------------------------

(deftest corpus-cell-a-configured-key-rides-as-a-bearer (with-corpus ()
    (is (null (header-of (progn (corpus-call-tool "law_coverage" "{}")
                                 (first (calls-of "tools/call")))
                        "authorization")) "no key, no header")))

(deftest corpus-cell-the-variable-the-section-names-supplies-the-key
    (with-corpus ("api_key_env" "CORPUS_TEST_KEY")
  (setf *env* (list (cons "CORPUS_TEST_KEY" "cp_test_key")))
  (corpus-call-tool "law_coverage" "{}")
  (is (equal "Bearer cp_test_key" (header-of (first (calls-of "tools/call")) "authorization")))))

(deftest corpus-cell-an-inline-secret-outranks-the-variable
    (with-corpus ("api_key" "cp_inline"
                  "api_key_env" "CORPUS_TEST_KEY")
  (setf *env* (list (cons "CORPUS_TEST_KEY" "cp_from_env")))
  (corpus-call-tool "law_coverage" "{}")
  (is (equal "Bearer cp_inline" (header-of (first (calls-of "tools/call")) "authorization")))))

(deftest corpus-cell-an-event-stream-answers-as-a-body-does (with-corpus ()
    (script "law.search"
            (lambda (body)
              (values (format nil "event: message~%data: ~a~%~%" (answer-message body +search-text+))
                      200 '(("content-type" . "text/event-stream")))))
    (let ((text (corpus-call-tool "law_search" "{\"query\":\"food truck\"}")))
      (is (cl:search "Sec. 217.042" text) "the data line is the answer"))))

(deftest corpus-cell-a-rate-limited-floor-names-the-free-key (with-corpus ()
    (script "law.search"
            (lambda (body)
              (declare (ignore body))
              (values "{\"error\":{\"message\":\"too many requests\"}}" 429 '())))
    (let ((text (corpus-call-tool "law_search" "{\"query\":\"food truck\"}")))
      (is (cl:search "HTTP 429" text))
      (is (cl:search "corpuslaw.us/settings" text) "where the free key comes from"))))

(deftest corpus-cell-a-refused-call-carries-the-server-s-sentence (with-corpus ()
    (script "law.search"
            (lambda (body)
              (declare (ignore body))
              (values "{\"error\":{\"message\":\"unknown tool\"}}" 500 '())))
    (let ((text (corpus-call-tool "law_search" "{\"query\":\"food truck\"}")))
      (is (cl:search "HTTP 500" text))
      (is (cl:search "unknown tool" text)))))

;;; --- the boundary and the command --------------------------------------------------

(deftest corpus-cell-a-call-without-its-query-never-reaches-the-wire (with-corpus ()
    (let ((text (corpus-call-tool "law_search" "{}")))
      (is (cl:search "query" text) "the refusal names the missing member"))
    (is (null (calls-of "tools/call")) "and nothing was dialled")))

(deftest corpus-cell-the-eval-verbs-answer-as-the-tools-do (with-corpus ()
    (is (cl:search "Sec. 217.042" (nodecode-corpus:search "food truck from a driveway")))
    (is (cl:search "MOBILE FOOD VENDORS" (nodecode-corpus:provision "Tex. Loc. Gov't Code Sec. 217.042")))
    (is (cl:search "Austin, TX" (nodecode-corpus:coverage)))
    (is (cl:search "stateFeeCents" (nodecode-corpus:call-tool "formation.requirements"
                                                               (nlk:json-object "entityType" "llc"))))))

(deftest corpus-cell-the-command-searches-and-explains-itself (with-corpus ()
    (is (cl:search "Sec. 217.042" (nodecode-corpus::run-slash "food truck in Austin" "s1")))
    (let ((bare (nodecode-corpus::run-slash "" "s1")))
      (is (cl:search "law_search" bare) "with no query it names what the cell gives")
      (is (cl:search "corpus_call" bare)))))
