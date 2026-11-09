;;;; cell-test.lisp --- the web-search-plus cell against stubbed providers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every engine endpoint is a stubbed NLK:HTTP and every key is a literal or
;;;; a stubbed NLE::CREDENTIAL-ENV: nothing reaches the network, the
;;;; environment or the operator's files. Each tool is called the way a
;;;; model's call reaches it - through the real dispatch, contract and all.

(in-package #:nodecode.test)

(define-test-slice "web-search-plus" "WEB-SEARCH-PLUS-CELL-"
  :start nodecode-web-search-plus:start-cell
  :defaults (("brave_api_key" "test-brave-key")
              ("serper_api_key" "test-serper-key")))

;;; --- fixtures --------------------------------------------------------------------

(defmacro with-wsp-wire ((asked &rest routes) &body body)
  "BODY with NLK:HTTP answering by URL. ROUTES are (FRAGMENT FORM), tried in
order; FORM runs with METHOD and URL bound and answers (values TEXT STATUS), as
NLK:HTTP does. ASKED collects (METHOD URL CONTENT HEADERS), oldest first."
  (let ((log (gensym "LOG")))
    `(let ((,log '()))
       (with-stubbed-fdefinition (nlk:http (method url &rest options)
                                   (push (list method url
                                               (getf options :content)
                                               (getf options :headers))
                                         ,log)
                                   (cond ,@(loop for (fragment form) in routes
                                                 collect `((cl:search ,fragment url) ,form))
                                         (t (error "unexpected request to ~a" url))))
         (symbol-macrolet ((,asked (reverse ,log)))
           ,@body)))))

(defparameter +brave+
  (concatenate
   'string
   "{\"web\":{\"results\":["
   "{\"title\":\"Brave One\",\"url\":\"https://example.org/first\",\"description\":\"the first page\"},"
   "{\"title\":\"Brave Two\",\"url\":\"https://pinterest.example/second\",\"description\":\"the second page\"}]}}")
  "Two results, as Brave's web endpoint answers them.")

(defparameter +serper+
  "{\"organic\":[{\"title\":\"Serper One\",\"link\":\"https://example.org/google\",\"snippet\":\"from google\"}]}"
  "One result, as Serper answers it.")

(defparameter +page+
  (concatenate
   'string
   "<html><head><title>A Readable Page</title><style>p{color:red}</style></head><body>"
   "<script>var hidden = 1;</script><p>Hello &amp; welcome.</p><p>Second line.</p></body></html>")
  "One page, as the plain fetch reads it.")

(defun wsp-answer (id tool arguments)
  "TOOL's answer to ARGUMENTS, as the model reads it: through the real dispatch."
  (nle::message-content (execute-wire-call id tool arguments)))

(defun wsp-object (id tool arguments)
  (nlk:decode-json (wsp-answer id tool arguments)))

(defun wsp-first-page (object)
  (aref (nlk:json-array object "pages") 0))

;;; --- the lifecycle, from the one declaration --------------------------------------

(define-cell-lifecycle-tests "web-search-plus"
  (:config "brave_api_key" "test-brave-key")
  (:help :web-search-plus)
  (:refused ("brave_api_key" "" "serper_api_key" "" "tavily_api_key" ""
             "exa_api_key" "" "firecrawl_api_key" "" "searxng_url" ""))
  (:running (is (nle::find-tool "web_search_plus") "the search tool is registered")
            (is (nle::find-tool "web_extract_plus") "the extract tool is registered")))

(deftest web-search-plus-cell-without-a-key-refuses ()
  ;; The one thing no verb can work without: at least one provider key, or a
  ;; SearXNG URL. The section that names none refuses at start.
  (let ((condition (signals-error nlk:config-refusal
                     (web-search-plus-start "brave_api_key" "" "serper_api_key" ""
                                            "tavily_api_key" "" "exa_api_key" ""
                                            "firecrawl_api_key" "" "searxng_url" ""))))
    (is (cl:search "no provider key" (princ-to-string condition))
        "the refusal says what is missing")
    (is (null (nle::find-tool "web_search_plus")) "and installs nothing")))

;;; --- web_search_plus ---------------------------------------------------------------

(deftest web-search-plus-cell-searches-brave ()
  (with-cell-stop ((web-search-plus-start))
    (with-wsp-wire (asked ("api.search.brave.com" (values +brave+ 200)))
      (let ((object (wsp-object "wsp-1" "web_search_plus" "{\"query\":\"a general question\"}")))
        (is (equal "brave" (nlk:json-value object :string "provider")))
        (is (= 2 (nlk:json-value object :integer "count")))
        (is (equal "Brave One"
                   (nlk:json-value (aref (nlk:json-array object "results") 0) :string "title")))
        (is (equal "the first page"
                   (nlk:json-value (aref (nlk:json-array object "results") 0) :string "snippet"))))
      (is (= 1 (length asked)) "one request answered it")
      (is (equal "test-brave-key"
                 (cdr (assoc "x-subscription-token" (fourth (first asked)) :test #'string=)))
          "the key rides the header")
      (is (cl:search "count=5" (second (first asked)))
          "the section's max_results is what was asked for"))))

(deftest web-search-plus-cell-falls-back-to-the-next-engine ()
  ;; Brave fails; the next configured engine answers, and the note says so.
  (with-cell-stop ((web-search-plus-start))
    (with-wsp-wire (asked
                     ("api.search.brave.com" (values "{\"error\":\"down\"}" 503))
                     ("google.serper.dev" (values +serper+ 200)))
      (let ((object (wsp-object "wsp-2" "web_search_plus" "{\"query\":\"a general question\"}")))
        (is (equal "serper" (nlk:json-value object :string "provider"))
            "the second engine answered")
        (is (cl:search "brave: HTTP 503"
                       (nlk:encode-json-object (nlk:json-array object "notes")))
            "and the note names the engine that failed")
        (is (= 2 (length asked)) "both were asked")))))

(deftest web-search-plus-cell-routes-a-security-query-to-serper ()
  (with-cell-stop ((web-search-plus-start))
    (with-wsp-wire (asked ("google.serper.dev" (values +serper+ 200)))
      (let ((object (wsp-object "wsp-3" "web_search_plus"
                                 "{\"query\":\"cve 2026 exploit\",\"search_type\":\"news\",\"freshness\":\"week\"}")))
        (is (equal "serper" (nlk:json-value object :string "provider"))
            "a security query leads with Google's results")
        (is (cl:search "google.serper.dev/news" (second (first asked)))
            "the news vertical is the endpoint")
        (is (cl:search "qdr:w" (third (first asked)))
            "freshness becomes Serper's own recency filter")))))

(deftest web-search-plus-cell-refuses-a-forced-engine-without-a-key ()
  (with-cell-stop ((web-search-plus-start))
    (with-wsp-wire (asked)
      (let ((answer (wsp-answer "wsp-4" "web_search_plus"
                                 "{\"query\":\"anything\",\"provider\":\"exa\"}")))
        (is (cl:search "exa is not configured" answer)
            "the answer names the engine and the fix")
        (is (null asked) "and nothing is asked of any engine")))))

(deftest web-search-plus-cell-filters-by-domain-here ()
  (with-cell-stop ((web-search-plus-start))
    (with-wsp-wire (asked ("api.search.brave.com" (values +brave+ 200)))
      (let ((object (wsp-object "wsp-5" "web_search_plus"
                                 "{\"query\":\"a general question\",\"include_domains\":[\"example.org\"]}")))
        (is (= 1 (nlk:json-value object :integer "count"))
            "only the result on the named host is kept")
        (is (cl:search "the domain lists kept 1"
                       (nlk:encode-json-object (nlk:json-array object "notes")))
            "and the note says the list was applied here")
        (is (cl:search "count=15" (second (first asked)))
            "more results were asked for, because filtering happens here")))))

(deftest web-search-plus-cell-research-reads-the-top-pages ()
  (with-cell-stop ((web-search-plus-start))
    (with-wsp-wire (asked
                     ("api.search.brave.com" (values +brave+ 200))
                     ("example.org" (values +page+ 200)))
      (let ((object (wsp-object "wsp-6" "web_search_plus"
                                 "{\"query\":\"a general question\",\"mode\":\"research\"}")))
        (is (equal "brave" (nlk:json-value object :string "provider")))
        (let ((page (wsp-first-page object)))
          (is (equal "https://example.org/first" (nlk:json-value page :string "url"))
              "the first result is the page read")
          (is (cl:search "Hello & welcome." (nlk:json-value page :string "text"))
              "and its text is the page's, entities decoded"))))))

;;; --- web_extract_plus --------------------------------------------------------------

(deftest web-search-plus-cell-reads-a-page-as-text ()
  (with-cell-stop ((web-search-plus-start))
    (with-wsp-wire (asked ("example.org/article" (values +page+ 200)))
      (let* ((object (wsp-object "wsp-7" "web_extract_plus"
                                  "{\"urls\":[\"https://example.org/article\"],\"provider\":\"native\"}"))
             (page (wsp-first-page object)))
        (is (equal "A Readable Page" (nlk:json-value page :string "title"))
            "the title comes from the page's own <title>")
        (is (cl:search "Second line." (nlk:json-value page :string "text"))
            "the text is there, tags gone")
        (is (not (cl:search "var hidden" (nlk:json-value page :string "text")))
            "and the script's code is not")
        (is (plusp (nlk:json-value page :integer "chars")))))))

(deftest web-search-plus-cell-falls-through-to-the-plain-fetch ()
  (with-cell-stop ((web-search-plus-start "tavily_api_key" "test-tavily-key"))
    (with-wsp-wire (asked
                     ("api.tavily.com" (values "{\"detail\":\"nope\"}" 401))
                     ("example.org/article" (values +page+ 200)))
      (let ((object (wsp-object "wsp-8" "web_extract_plus"
                                 "{\"urls\":[\"https://example.org/article\"]}")))
        (is (cl:search "tavily: HTTP 401"
                       (nlk:encode-json-object (nlk:json-array object "notes")))
            "the provider that failed is a note")
        (is (cl:search "Hello & welcome."
                       (nlk:json-value (wsp-first-page object) :string "text"))
            "the plain fetch answered instead")))))

(deftest web-search-plus-cell-refuses-a-url-it-will-not-fetch ()
  (with-cell-stop ((web-search-plus-start))
    (with-wsp-wire (asked)
      (let ((object (wsp-object "wsp-9" "web_extract_plus"
                                 "{\"urls\":[\"file:///etc/passwd\"]}")))
        (is (cl:search "not an http:// or https:// url"
                       (nlk:encode-json-object (nlk:json-array object "notes")))
            "only http and https are fetched")
        (is (equal "no page could be read" (nlk:json-value object :string "error")))
        (is (null asked) "and nothing left the process")))))

(deftest web-search-plus-cell-cuts-a-page-and-says-so ()
  (with-cell-stop ((web-search-plus-start))
    (with-wsp-wire (asked ("example.org/long" (values +page+ 200)))
      (let ((page (wsp-first-page
                   (wsp-object "wsp-10" "web_extract_plus"
                               "{\"urls\":[\"https://example.org/long\"],\"provider\":\"native\",\"max_chars\":10}"))))
        (is (eq t (nlk:json-value page :boolean "truncated")))
        (is (= 10 (length (nlk:json-value page :string "text"))))
        (is (> (nlk:json-value page :integer "chars") 10)
            "the answer still reports how much the page held")))))
