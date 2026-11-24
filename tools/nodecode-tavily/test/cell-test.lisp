;;;; cell-test.lisp --- lifecycle, the five tools, and /tavily, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Tavily, needs a
;;;; key or touches the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start, wrong values and a missing key refused), the
;;;; five tools through the seam the model calls them by -- their url, their
;;;; body, their headers and their answer -- and the operator's two commands.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-tavily-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials Tavily."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected tavily request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun tavily-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun tavily-reply (text &optional (status 200))
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun tavily-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun tavily-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun tavily-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "tavily-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "tavily" "TAVILY-CELL-" :start nodecode-tavily:start-cell)

(define-cell-lifecycle-tests "tavily"
  (:fixture with-tavily-wire)
  (:config "api_key" "tvly-test")
  (:help :tavily)
  (:command "tavily")
  (:running
   (is (nle::find-tool "tavily_search") "search is registered")
   (is (nle::find-tool "tavily_extract") "and extract")
   (is (nle::find-tool "tavily_map") "and map")
   (is (nle::find-tool "tavily_crawl") "and crawl")
   (is (nle::find-tool "tavily_research") "and research, the original's five")
   (is (= 5 (count-if (lambda (entry) (eql 0 (search "tavily_" (first entry))))
                      nle::*tools*)) "five of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "tavily_search")) "and stop takes every one back")
   (is (null (nle::find-tool "tavily_research"))))
  (:refused ("max_results" 0)
            ("timeout_seconds" 0)
            ("max_chars" 10)
            ("research_timeout_seconds" 5)))

(deftest tavily-cell-a-start-with-no-key-refuses-and-installs-nothing (with-tavily-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (tavily-start))))
      (is-carrying text "needs a key" "TAVILY_API_KEY")
      (is (null (nle::find-tool "tavily_search")) "nothing was installed"))))

(deftest tavily-cell-the-key-may-come-from-the-environment (with-tavily-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (and (equal name "TAVILY_API_KEY") "tvly-env"))
    (with-cell-stop ((tavily-start))
      (tavily-reply "{\"results\": []}")
      (tavily-call "tavily_search" "{\"query\": \"x\"}")
      (let ((sent (tavily-request-header (first (tavily-asked)) "authorization")))
        (is (equal "Bearer tvly-env" sent) "the exported key is the one sent")))))

;;; --- search ---------------------------------------------------------------------

(deftest tavily-cell-search-posts-the-query-and-reads-the-sources (with-tavily-wire)
  (with-cell-stop ((tavily-start "api_key" "tvly-test"))
    (tavily-reply
     (concatenate 'string
                  "{\"query\": \"lisp\", \"answer\": \"short answer\", \"request_id\": \"req-1\","
                  " \"results\": [{\"title\": \"A page\", \"url\": \"https://a.test/1\","
                  " \"content\": \"snippet a\", \"published_date\": \"2026-01-02\", \"score\": 0.9}]}"))
    (let ((answer (tavily-call "tavily_search"
                                "{\"query\": \"lisp\", \"include_domains\": [\"sec.gov\"], \"topic\": \"news\"}")))
      (let* ((request (first (tavily-asked)))
             (body (tavily-request-body request)))
        (is (eq :post (first request)) "one POST")
        (is (equal "https://api.tavily.com/search" (second request)) "to Tavily's search endpoint")
        (is (equal "Bearer tvly-test" (tavily-request-header request "authorization")) "with the section's key")
        (is (equal "lisp" (nlk:json-value body :text "query")) "carrying the query")
        (is (= 5 (nlk:json-value body :integer "max_results")) "and no count, so the section's default")
        (is (equal "news" (nlk:json-value body :text "topic")) "and the topic the call named")
        (let ((domains (nlk:json-value body :array "include_domains")))
          (is (equal '("sec.gov") (coerce domains 'list)) "the domain filter, as an array")))
      (is (search "answer: short answer" answer) "the answer Tavily wrote is first")
      (is (search "1. A page" answer) "then the numbered source")
      (is (search "https://a.test/1" answer) "with its url")
      (is (search "snippet a" answer) "its snippet")
      (is (search "2026-01-02" answer) "and its date")
      (is (search "req-1" answer) "and the request id, for a support question"))))

(deftest tavily-cell-a-refused-key-is-the-model-s-repair (with-tavily-wire)
  (with-cell-stop ((tavily-start "api_key" "tvly-test"))
    (tavily-reply "{\"detail\": {\"error\": \"Unauthorized: missing or invalid API key\"}}" 401)
    (let ((answer (tavily-call "tavily_search" "{\"query\": \"x\"}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "HTTP 401" answer) "and names the status")
      (is (search "api_key" answer) "and tells the model what to check"))))

;;; --- extract, map, crawl ----------------------------------------------------------

(deftest tavily-cell-extract-reads-the-urls-it-was-given (with-tavily-wire)
  (with-cell-stop ((tavily-start "api_key" "tvly-test"))
    (tavily-reply
     (concatenate 'string
                  "{\"results\": [{\"url\": \"https://a.test/1\", \"raw_content\": \"page one\"}],"
                  " \"failed_results\": [{\"url\": \"https://b.test/2\", \"error\": \"404 Not Found\"}]}"))
    (let ((answer (tavily-call "tavily_extract"
                                "{\"urls\": [\"https://a.test/1\", \"https://b.test/2\"], \"extract_depth\": \"advanced\"}")))
      (let* ((request (first (tavily-asked)))
             (body (tavily-request-body request)))
        (is (equal "https://api.tavily.com/extract" (second request)) "the extract endpoint")
        (let ((urls (nlk:json-value body :array "urls")))
          (is (equal '("https://a.test/1" "https://b.test/2") (coerce urls 'list)) "both urls"))
        (is (equal "advanced" (nlk:json-value body :text "extract_depth")) "with the depth it asked for"))
      (is (search "page one" answer) "the page's text reaches the model")
      (is (search "https://b.test/2 - 404 Not Found" answer) "and a page that failed is named, not hidden"))))

(deftest tavily-cell-extract-refuses-a-call-with-no-urls (with-tavily-wire)
  (with-cell-stop ((tavily-start "api_key" "tvly-test"))
    (let ((answer (tavily-call "tavily_extract" "{}")))
      (is (search "urls" answer) "the refusal names the member the call left out")
      (is (null (tavily-asked)) "and nothing was sent"))))

(deftest tavily-cell-map-lists-the-urls-of-a-site (with-tavily-wire)
  (with-cell-stop ((tavily-start "api_key" "tvly-test"))
    (tavily-reply "{\"base_url\": \"https://docs.test\", \"results\": [\"https://docs.test/a\", \"https://docs.test/b\"]}")
    (let ((answer (tavily-call "tavily_map" "{\"url\": \"https://docs.test\", \"limit\": 20}")))
      (let* ((request (first (tavily-asked)))
             (body (tavily-request-body request)))
        (is (equal "https://api.tavily.com/map" (second request)) "the map endpoint")
        (is (equal "https://docs.test" (nlk:json-value body :text "url")) "the starting url")
        (is (= 20 (nlk:json-value body :integer "limit")) "and the bound the call set"))
      (is (search "https://docs.test/a" answer) "every url it answered")
      (is (search "https://docs.test/b" answer) "is in the list"))))

(deftest tavily-cell-crawl-reads-a-bounded-part-of-a-site (with-tavily-wire)
  (with-cell-stop ((tavily-start "api_key" "tvly-test" "max_chars" 200))
    (tavily-reply
     (format nil "{\"base_url\": \"https://docs.test\", \"results\": [{\"url\": \"https://docs.test/a\", \"raw_content\": \"~a\"}]}"
             (make-string 260 :initial-element #\x)))
    (let ((answer (tavily-call "tavily_crawl" "{\"url\": \"https://docs.test\", \"limit\": 3, \"instructions\": \"the docs\"}")))
      (let* ((request (first (tavily-asked)))
             (body (tavily-request-body request)))
        (is (equal "https://api.tavily.com/crawl" (second request)) "the crawl endpoint")
        (is (= 3 (nlk:json-value body :integer "limit")) "bounded, as the call asked")
        (is (equal "the docs" (nlk:json-value body :text "instructions")) "with the crawl's instruction"))
      (is (search "crawled https://docs.test: 1 page" answer) "the answer says what was read")
      (is (search "60 more characters" answer) "and a page is cut to the section's max_chars"))))

;;; --- research, and the operator's command -----------------------------------------

(deftest tavily-cell-research-answers-a-report-with-its-sources (with-tavily-wire)
  (with-cell-stop ((tavily-start "api_key" "tvly-test"))
    (tavily-reply
     "{\"answer\": \"The report body.\", \"request_id\": \"req-3\", \"sources\": [{\"title\": \"S\", \"url\": \"https://s.test\"}]}")
    (let ((answer (tavily-call "tavily_research" "{\"query\": \"why\", \"model\": \"mini\"}")))
      (let* ((request (first (tavily-asked)))
             (body (tavily-request-body request)))
        (is (equal "https://api.tavily.com/research" (second request)) "the research endpoint")
        (is (equal "mini" (nlk:json-value body :text "model")) "with the model the call named"))
      (is (search "The report body." answer) "the report is the answer")
      (is (search "S - https://s.test" answer) "and its sources are cited"))))

(deftest tavily-cell-the-command-searches-or-says-what-it-is-connected-to (with-tavily-wire)
  (with-cell-stop ((tavily-start "api_key" "tvly-test"))
    (let ((status (nodecode-tavily::run-command "" nil)))
      (is (search "https://api.tavily.com" status) "no argument reports the connection")
      (is (search "key set" status) "and that a key is there")
      (is (null (search "tvly-test" status)) "without ever printing the key")
      (is (null (tavily-asked)) "and sends nothing")
    (tavily-reply "{\"results\": [{\"title\": \"A page\", \"url\": \"https://a.test/1\", \"content\": \"s\"}]}")
    (let ((answer (nodecode-tavily::run-command "  lisp  " nil)))
      (is (search "1. A page" answer) "an argument runs one search and answers its sources")
      (let ((query (nlk:json-value (tavily-request-body (first (tavily-asked))) :text "query")))
        (is (equal "lisp" query) "the query trimmed, as typed"))))))
