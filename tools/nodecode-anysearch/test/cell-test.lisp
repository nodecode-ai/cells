;;;; cell-test.lisp --- lifecycle, the two tools, /anysearch, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start with no key, a member under its floor or a root without a scheme
;;;; refused, an api_key_env naming an empty variable refused), the two tools
;;;; through the seam the model calls them by -- their URL, their headers,
;;;; their body and the answer they build -- the order and the cap on a
;;;; multi-URL extract, the MCP endpoint answering when REST does not, and
;;;; /anysearch with and without a query. Every request is a stubbed
;;;; NLK:HTTP, so nothing reaches AnySearch or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *anysearch-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *anysearch-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-anysearch-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *ANYSEARCH-REQUESTS* and
answered by *ANYSEARCH-ANSWER-FN*, so no test dials AnySearch. UIOP:GETENV
answers nothing either, so a key in the machine's environment cannot reach a
test that did not ask for one."
  `(let ((*anysearch-requests* '())
         (*anysearch-answer-fn* (lambda (method url args)
                                   (declare (ignore method url args))
                                   (error "unexpected AnySearch request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *anysearch-requests*)
                                (funcall *anysearch-answer-fn* method url args))
       (with-stubbed-fdefinition (uiop:getenv (name)
                                  (declare (ignore name))
                                  nil)
         ,@body))))

(defun anysearch-asked ()
  "The stubbed requests, oldest first."
  (reverse *anysearch-requests*))

(defun anysearch-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun anysearch-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun anysearch-answer (text &optional (status 200))
  (setf *anysearch-answer-fn* (lambda (method url args)
                                 (declare (ignore method url args))
                                 (values text status))))

(defun anysearch-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "anysearch" "ANYSEARCH-CELL-")

(defun anysearch-config (&rest pairs)
  "A whole shared config with one `anysearch' section built from PAIRS."
  (nlk:json-object "anysearch" (apply #'nlk:make-json-object pairs)))

(defun anysearch-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-anysearch:start-cell (apply #'anysearch-config pairs)))

(define-cell-lifecycle-tests "anysearch"
  (:fixture with-anysearch-wire)
  (:help :anysearch)
  (:command "anysearch")
  (:running (is (nle::find-tool "anysearch_search") "anysearch_search is registered")
            (is (nle::find-tool "anysearch_extract") "anysearch_extract is registered")
            (let ((root (nodecode-anysearch::setting :base-url)))
              (is (equal "https://api.anysearch.com" root) "the section's default root"))
            (is (null (nodecode-anysearch::setting :key)) "no key: the anonymous tier answers")
            (is (= 5 (nodecode-anysearch::setting :max-results)) "the default result count")
            (is (eq t (nodecode-anysearch::setting :mcp-fallback)) "the MCP fallback is on"))
  (:stopped (is (null (nle::find-tool "anysearch_search")) "stop unregisters the tools")
            (is (null (nle::find-tool "anysearch_extract")) "both of them"))
  (:refused ("max_results" 0) ("max_urls" 0) ("timeout_seconds" 0)
            ("base_url" "api.anysearch.com"))
  (:idle nodecode-anysearch:anysearch-error (nodecode-anysearch::run-command "" nil)))

(deftest anysearch-cell-a-named-but-empty-key-variable-refuses ()
  (with-anysearch-wire
    (is-carrying (answer (refusal-text nlk:config-refusal
                                       (anysearch-start "api_key_env" "ANYSEARCH_TEST_KEY")))
                 ("api_key_env names ANYSEARCH_TEST_KEY" "the refusal names the variable")
                 ("anonymous tier" "and says what to do instead"))
    (is (null (nle::find-tool "anysearch_search")) "a refused start installed nothing")))

(deftest anysearch-cell-the-key-may-come-from-the-environment ()
  (with-anysearch-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                               (cond ((string= name "ANYSEARCH_HOME_KEY") "k-env")
                                     ((string= name "ANYSEARCH_API_KEY") "k-fallback")
                                     (t nil)))
      (with-cell-stop ((anysearch-start "api_key_env" "ANYSEARCH_HOME_KEY"))
        (is (equal "k-env" (nodecode-anysearch::setting :key)) "api_key_env names the variable"))
      (with-cell-stop ((anysearch-start))
        (is (equal "k-fallback" (nodecode-anysearch::setting :key)) "ANYSEARCH_API_KEY is read either way")))))

;;; --- anysearch_search ----------------------------------------------------------

(deftest anysearch-cell-search-answers-titles-links-and-abstracts ()
  (with-anysearch-wire
    (with-cell-stop ((anysearch-start))
      (anysearch-answer "{\"data\":{\"results\":[{\"title\":\"Oslo weather\",\"url\":\"https://example.com/oslo\",\"content\":\"Rain all week.\"},{\"title\":\"\",\"url\":\"https://example.com/two\",\"snippet\":\"A second page.\"}]}}")
      (let ((text (anysearch-tool "anysearch_search" (nlk:json-object "query" "oslo weather" "limit" 5))))
        (is (search "2 results for \"oslo weather\" (via AnySearch):" text))
        (is (search "1. Oslo weather" text))
        (is (search "https://example.com/oslo" text))
        (is (search "Rain all week." text))
        (is (search "(untitled)" text) "a result with no title still reads")
        (is (search "A second page." text) "a snippet stands in for content"))
      (let ((request (first (anysearch-asked))))
        (is (= 1 (length (anysearch-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://api.anysearch.com/v1/search" (second request)))
        (is (null (anysearch-request-header request "authorization")) "anonymous: no key header")
        (is (equal "application/json" (anysearch-request-header request "content-type")))
        (let ((body (anysearch-request-body request)))
          (is (equal "oslo weather" (nlk:json-value body :string "query")))
          (is (= 5 (nlk:json-value body :integer "max_results")) "the call's limit")
          (is (equal "markdown" (nlk:json-value body :string "format")) "the abstract format"))))))

(deftest anysearch-cell-a-key-is-sent-and-a-limit-clamped ()
  (with-anysearch-wire
    (with-cell-stop ((anysearch-start "api_key" "k-test"))
      (anysearch-answer "{\"results\":[]}")
      (let ((text (anysearch-tool "anysearch_search" (nlk:json-object "query" "x" "limit" 50))))
        (is (equal "No results for \"x\" (AnySearch, 10 asked for)." text)))
      (let ((request (first (anysearch-asked))))
        (is (equal "Bearer k-test" (anysearch-request-header request "authorization")) "the section's key")
        (is (= 10 (nlk:json-value (anysearch-request-body request) :integer "max_results")) "the endpoint's cap")))))

(deftest anysearch-cell-a-framed-answer-decodes ()
  (with-anysearch-wire
    (with-cell-stop ((anysearch-start))
      (anysearch-answer "data: {\"data\":{\"results\":[{\"title\":\"Framed\",\"url\":\"https://example.com/f\"}]}}")
      (let ((text (anysearch-tool "anysearch_search" (nlk:json-object "query" "framed"))))
        (is (search "1. Framed" text) "a data: framed body reads like a plain one")))))

(deftest anysearch-cell-a-refused-answer-is-a-repair-not-a-crash ()
  (with-anysearch-wire
    (with-cell-stop ((anysearch-start "api_key" "bad-key"))
      (anysearch-answer "{\"error\":\"Invalid API key\"}" 401)
      (is-carrying (answer (anysearch-tool "anysearch_search" (nlk:json-object "query" "x")))
                   ("ERROR: AnySearch answered HTTP 401") ("Invalid API key")))))

;;; --- anysearch_extract ------------------------------------------------------------

(deftest anysearch-cell-extract-fetches-each-url ()
  (with-anysearch-wire
    (with-cell-stop ((anysearch-start))
      (setf *anysearch-answer-fn*
            (lambda (method url args)
              (declare (ignore method url))
              (let* ((body (nlk:decode-json (getf args :content)))
                     (target (nlk:json-value body :string "url")))
                (values (format nil "{\"data\":{\"title\":\"Page ~a\",\"content\":\"# ~a\"}}"
                                target target)
                        200))))
      (let* ((text (anysearch-tool "anysearch_extract"
                                    (nlk:json-object "urls" (vector "https://a.test/1" "https://b.test/2"))))
             (requests (anysearch-asked))
             (first-body (anysearch-request-body (first requests)))
             (second-body (anysearch-request-body (second requests))))
        (is (= 2 (length requests)) "one extract per url")
        (is (equal "https://api.anysearch.com/v1/extract" (second (first requests))))
        (is (equal "https://a.test/1" (nlk:json-value first-body :string "url")))
        (is (equal "https://b.test/2" (nlk:json-value second-body :string "url")) "in the order the call named them")
        (is (search "1. https://a.test/1 -- Page https://a.test/1" text))
        (is (search "# https://b.test/2" text) "the page's own text")))))

(deftest anysearch-cell-extract-fetches-at-most-max-urls ()
  (with-anysearch-wire
    (with-cell-stop ((anysearch-start "max_urls" 1))
      (anysearch-answer "{\"data\":{\"url\":\"https://a.test/1\",\"content\":\"one\"}}")
      (let ((text (anysearch-tool "anysearch_extract"
                                   (nlk:json-object "urls" (vector "https://a.test/1" "https://b.test/2")))))
        (is (= 1 (length (anysearch-asked))) "one call, one extract")
        (is (search "(2 URLs named; the first 1 fetched -- max_urls)" text))))))

(deftest anysearch-cell-the-mcp-endpoint-answers-when-rest-does-not ()
  (with-anysearch-wire
    (with-cell-stop ((anysearch-start))
      (setf *anysearch-answer-fn*
            (lambda (method url args)
              (declare (ignore method args))
              (cond ((search "/v1/extract" url)
                     (values "{\"data\":{\"url\":\"https://a.test/1\"}}" 200))
                    ((search "/mcp" url)
                     (values "data: {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"content\":[{\"type\":\"text\",\"text\":\"{\\\"title\\\":\\\"Fetched\\\",\\\"content\\\":\\\"the page\\\"}\"}]}}"
                             200))
                    (t (error "unexpected request to ~a" url)))))
      (let ((text (anysearch-tool "anysearch_extract"
                                   (nlk:json-object "urls" (vector "https://a.test/1")))))
        (is (= 2 (length (anysearch-asked))) "the REST endpoint, then MCP")
        (let* ((mcp (second (anysearch-asked)))
               (body (anysearch-request-body mcp)))
          (is (equal "https://api.anysearch.com/mcp" (second mcp)) "MCP is the second request")
          (is (equal "tools/call" (nlk:json-value body :string "method")) "one JSON-RPC tools/call")
          (is (equal "application/json, text/event-stream" (anysearch-request-header mcp "accept")) "its own accept")
          (is (equal "extract" (nlk:json-value (nlk:json-value body :object "params") :string "name"))))
        (is (search "1. https://a.test/1 -- Fetched" text))
        (is (search "the page" text) "the inner JSON document is unpacked, not shown raw")))))

(deftest anysearch-cell-a-failed-extract-is-that-url-s-own-line ()
  (with-anysearch-wire
    (with-cell-stop ((anysearch-start "mcp_fallback" nil))
      (anysearch-answer "{\"message\":\"boom\"}" 500)
      (let ((text (anysearch-tool "anysearch_extract"
                                   (nlk:json-object "urls" (vector "https://a.test/1")))))
        (is (= 1 (length (anysearch-asked))) "no MCP request when mcp_fallback is off")
        (is (search "1. https://a.test/1" text) "the URL is still that URL's line")
        (is (search "failed: AnySearch answered HTTP 500 on /v1/extract: boom" text)
            "one URL's failure is its own line")))))

;;; --- /anysearch -----------------------------------------------------------------

(deftest anysearch-cell-the-command-shows-the-endpoint-or-searches ()
  (with-anysearch-wire
    (with-cell-stop ((anysearch-start))
      (let ((text (cell-entry "nodecode-anysearch" "anysearch" "")))
        (is (search "AnySearch web: https://api.anysearch.com, 5 results a search" text))
        (is (search "at most 5 URLs an extract" text))
        (is (search "without a key (the anonymous tier)" text))
        (is (null (anysearch-asked)) "a status read asks AnySearch nothing"))
      (anysearch-answer "{\"data\":{\"results\":[{\"title\":\"Oslo\",\"url\":\"https://example.com/oslo\",\"content\":\"Rain.\"}]}}")
      (let ((text (cell-entry "nodecode-anysearch" "anysearch" "oslo weather")))
        (is (search "1 result for \"oslo weather\" (via AnySearch):" text))
        (is (equal "oslo weather" (nlk:json-value (anysearch-request-body (first (anysearch-asked)))
                                                   :string "query")))))))
