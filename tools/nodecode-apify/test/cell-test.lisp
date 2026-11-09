;;;; cell-test.lisp --- lifecycle, the three tools, the two verbs, /apify.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, integer fields under their floors refused, a start with no token
;;;; refused), each tool through the seam the model calls it by -- its url,
;;;; its Bearer header, its body and its answer -- the two eval verbs, and
;;;; /apify with and without a query. Every request is a stubbed NLK:HTTP, so
;;;; nothing reaches Apify or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-apify-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Apify."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected apify request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defmacro with-apify-answers ((&rest routes) &body body)
  "BODY with the stubbed wire answering ROUTES, (FRAGMENT FORM): the first
whose FRAGMENT the url holds answers FORM's (values TEXT STATUS); a url no
route names is an error."
  `(with-apify-wire
     (let ((*answer-fn* (lambda (method url args)
                           (declare (ignore method args))
                           (cond ,@(loop for (fragment form) in routes
                                         collect `((search ,fragment url) ,form))
                                 (t (error "unexpected request to ~a" url))))))
       (with-cell-stop ((apify-start "api_token" "t-test"))
         ,@body))))

(defun apify-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun apify-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun apify-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun apify-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "apify" "APIFY-CELL-" :start nodecode-apify:start-cell)

(define-cell-lifecycle-tests "apify"
  (:fixture with-apify-wire)
  (:config "api_token" "t-test")
  (:help :apify)
  (:command "apify")
  (:running (is (nle::find-tool "apify_discover") "apify_discover is registered")
            (is (nle::find-tool "apify_start"))
            (is (nle::find-tool "apify_collect"))
            (is (equal "https://api.apify.com" (nodecode-apify::root)))
            (is (equal "https://web-fetch.apify.actor/" (nodecode-apify::web-fetch-root)))
            (is (= 5 (nodecode-apify::setting :search-results)) "five results by default")
            (is (= 50000 (nodecode-apify::setting :max-chars))))
  (:stopped (is (null (nle::find-tool "apify_discover")) "stop unregisters the tools"))
  (:refused ("search_results" 0) ("timeout_seconds" 0) ("max_chars" 5))
  (:idle apify:apify-error (apify:search "x")))

(deftest apify-cell-a-token-is-required ()
  (with-apify-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignorable name)) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (apify-start)))
                   ("apify: no token" "APIFY_API_TOKEN"))
      (is (null (nle::find-tool "apify_discover")) "a refused start installed nothing"))))

;;; --- apify_discover --------------------------------------------------------------

(deftest apify-cell-discover-searches-the-store ()
  (with-apify-answers (("/v2/store" (values (concatenate 'string
                                                         "{\"data\":{\"items\":["
                                                         "{\"username\":\"apify\",\"name\":\"google-search-scraper\","
                                                         "\"title\":\"Google Search Scraper\",\"description\":\"SERP data\","
                                                         "\"stats\":{\"totalRuns\":12345}}]}}")
                                          200)))
    (is-carrying (answer (apify-tool "apify_discover" (nlk:json-object "query" "google search")))
      ("apify~google-search-scraper") ("Google Search Scraper") ("12345")
      ("\"run_count\":12345" "the run count rides as a number"))
    (let ((request (first (apify-asked))))
      (is (= 1 (length (apify-asked))) "one request")
      (is (eq :get (first request)))
      (is (search "/v2/store?" (second request)))
      (is (search "search=google%20search" (second request)) "the query is encoded")
      (is (search "sortBy=relevance" (second request)))
      (is (equal "Bearer t-test" (apify-request-header request "authorization")) "the token rides the header")
      (is (null (search "t-test" (second request))) "not in the url"))))

(deftest apify-cell-discover-an-actor-answers-its-schema ()
  (with-apify-answers (("/builds/default" (values (concatenate 'string
                                                               "{\"data\":{\"actorDefinition\":{"
                                                               "\"input\":{\"type\":\"object\",\"properties\":{\"query\":{\"type\":\"string\"}}},"
                                                               "\"readme\":\"# Instagram Scraper\"}}}")
                                                 200))
                       ("/v2/acts/apify~instagram-scraper"
                        (values (concatenate 'string
                                             "{\"data\":{\"username\":\"apify\",\"name\":\"instagram-scraper\","
                                             "\"title\":\"Instagram Scraper\",\"description\":\"all Instagram data\"}}")
                                200)))
    (is-carrying (answer (apify-tool "apify_discover"
                                      (nlk:json-object "actor_id" "apify~instagram-scraper")))
      ("apify~instagram-scraper") ("Instagram Scraper") ("all Instagram data")
      ("input_schema") ("\\\"query\\\"" "the schema rides as text") ("# Instagram Scraper")
      ("Use apify_start with actor_id='apify~instagram-scraper'"))
    (is (equal '(:get :get) (map 'list #'first (apify-asked))) "the Actor, then its default build")
    (is (search "/v2/acts/apify~instagram-scraper/builds/default" (second (second (apify-asked)))))))

(deftest apify-cell-discover-refuses-both-or-neither ()
  (with-apify-answers (("/v2/store" (values "{}" 200)))
    (is-carrying (answer (apify-tool "apify_discover"
                                      (nlk:json-object "query" "x" "actor_id" "apify~x")))
      ("ERROR: apify:" "exactly one of"))
    (is-carrying (answer (apify-tool "apify_discover" (nlk:json-object)))
      ("ERROR: apify:" "'query'"))))

;;; --- apify_start -----------------------------------------------------------------

(deftest apify-cell-start-posts-each-run-and-answers-refs ()
  (with-apify-answers (("/v2/acts/apify~google-search-scraper/runs"
                        (values (concatenate 'string
                                             "{\"data\":{\"id\":\"run-1\",\"defaultDatasetId\":\"ds-1\","
                                             "\"status\":\"READY\"}}")
                                200))
                       ("/v2/acts/nobody~missing/runs"
                        (values "{\"error\":{\"message\":\"Actor was not found\"}}" 404)))
    (is-carrying (answer (apify-tool "apify_start"
                                      (nlk:json-object
                                       "runs" (vector (nlk:json-object
                                                        "actor_id" "apify~google-search-scraper"
                                                        "input" (nlk:json-object "queries" "cars")
                                                        "label" "cars")
                                                      (nlk:json-object "actor_id" "nobody~missing")))))
      ("run-1") ("ds-1") ("READY") ("cars") ("errors") ("HTTP 404") ("Actor was not found"))
    (let* ((requests (apify-asked))
           (body (apify-request-body (first requests))))
      (is (= 2 (length requests)) "one start per spec")
      (is (eq :post (first (first requests))))
      (is (equal "cars" (nlk:json-value body :string "queries")) "the run input is the body"))))

(deftest apify-cell-start-refuses-a-batch-over-ten ()
  (with-apify-answers (("/runs" (values "{}" 200)))
    (is-carrying (answer (apify-tool "apify_start"
                                      (nlk:json-object "runs"
                                                       (coerce (loop repeat 11
                                                                     collect (nlk:json-object "actor_id" "apify~x"))
                                                               'vector))))
      ("ERROR: apify:") ("batch too large") ("maximum is 10"))))

;;; --- apify_collect ---------------------------------------------------------------

(deftest apify-cell-collect-reads-a-finished-run-and-leaves-a-running-one ()
  (with-apify-answers (("/v2/actor-runs/run-1"
                        (values "{\"data\":{\"status\":\"SUCCEEDED\",\"defaultDatasetId\":\"ds-1\"}}" 200))
                       ("/v2/actor-runs/run-2"
                        (values "{\"data\":{\"status\":\"RUNNING\"}}" 200))
                       ("/v2/datasets/ds-1/items"
                        (values "[{\"title\":\"a page\",\"url\":\"https://example.com/\"}]" 200)))
    (is-carrying (answer (apify-tool "apify_collect"
                                      (nlk:json-object
                                       "runs" (vector (nlk:json-object "run_id" "run-1"
                                                                        "actor_id" "apify~google-search-scraper"
                                                                        "dataset_id" "ds-1")
                                                      (nlk:json-object "run_id" "run-2"
                                                                        "actor_id" "apify~google-search-scraper"
                                                                        "dataset_id" "ds-2")))))
      ("\"all_done\":false" "the running ref keeps the answer open")
      ("run-1") ("\"status\":\"SUCCEEDED\"") ("a page")
      ("<<<EXTERNAL_UNTRUSTED_CONTENT>>>" "scraped data is marked as data")
      ("<<<END_EXTERNAL_UNTRUSTED_CONTENT>>>")
      ("run-2") ("\"pending\""))
    (let ((items (find "/v2/datasets/ds-1/items" (apify-asked) :key #'second :test #'search)))
      (is (search "limit=100" (second items)) "the default item count")
      (is (search "format=json" (second items))))))

(deftest apify-cell-collect-reports-a-failed-run-as-an-error ()
  (with-apify-answers (("/v2/actor-runs/run-3"
                        (values "{\"data\":{\"status\":\"FAILED\"}}" 200)))
    (is-carrying (answer (apify-tool "apify_collect"
                                      (nlk:json-object
                                       "runs" (vector (nlk:json-object "run_id" "run-3"
                                                                        "actor_id" "apify~x"
                                                                        "dataset_id" "ds-3")))))
      ("\"all_done\":true") ("errors") ("Run ended with status: FAILED"))))

;;; --- the two verbs ---------------------------------------------------------------

(deftest apify-cell-search-runs-the-rag-web-browser ()
  (with-apify-answers (("/run-sync-get-dataset-items"
                        (values (concatenate 'string
                                             "[{\"searchResult\":{\"title\":\"Apify docs\",\"url\":\"https://docs.apify.com/\","
                                             "\"description\":\"Actors, runs and datasets.\"}},"
                                             "{\"searchResult\":{\"title\":\"Second\",\"url\":\"https://example.com/2\","
                                             "\"description\":\"another\"}}]")
                                200)))
    (is-carrying (answer (apify:search "apify actors" :n 2))
      ("2 web results for \"apify actors\"")
      ("1. Apify docs") ("   https://docs.apify.com/") ("Actors, runs and datasets.")
      ("2. Second"))
    (let* ((request (first (apify-asked)))
           (body (apify-request-body request)))
      (is (search "apify~rag-web-browser" (second request)))
      (is (search "timeout=90" (second request)) "the section's search_timeout")
      (is (equal "apify actors" (nlk:json-value body :string "query")))
      (is (= 2 (nlk:json-value body :integer "maxResults"))))))

(deftest apify-cell-fetch-reads-a-page-through-web-fetch ()
  (with-apify-answers (("https://web-fetch.apify.actor/"
                        (values (concatenate 'string
                                             "{\"fetch\":{\"loadedUrl\":\"https://example.com/\",\"httpStatusCode\":200},"
                                             "\"metadata\":{\"title\":\"Example Domain\"},\"markdown\":\"# Example\\n\\nhi\"}")
                                200)))
    (is-carrying (answer (apify:fetch "https://example.com/"))
      ("Example Domain") ("https://example.com/") ("# Example"))
    (let* ((request (first (apify-asked)))
           (body (apify-request-body request)))
      (is (eq :post (first request)))
      (is (equal "https://example.com/" (nlk:json-value body :string "url")))
      (is (equal "markdown" (aref (nlk:json-value body :array "formats") 0))))))

(deftest apify-cell-fetch-names-a-target-that-refused ()
  (with-apify-answers (("https://web-fetch.apify.actor/"
                        (values (concatenate 'string
                                             "{\"fetch\":{\"loadedUrl\":\"https://example.com/gone\",\"httpStatusCode\":404},"
                                             "\"metadata\":{},\"markdown\":\"not found\"}")
                                200)))
    (is-carrying (answer (refusal-text apify:apify-error (apify:fetch "https://example.com/gone")))
      ("returned HTTP 404"))))

;;; --- /apify --------------------------------------------------------------------

(deftest apify-cell-the-command-shows-the-account-or-searches ()
  (with-apify-answers (("/v2/users/me" (values "{\"data\":{\"username\":\"someone\"}}" 200))
                       ("/v2/store"
                        (values (concatenate 'string
                                             "{\"data\":{\"items\":[{\"username\":\"apify\",\"name\":\"instagram-scraper\","
                                             "\"title\":\"Instagram Scraper\",\"description\":\"all Instagram data\"}]}}")
                                200)))
    (is-carrying (answer (cell-entry "nodecode-apify" "apify" ""))
      ("apify: the token is someone's") ("https://api.apify.com")
      ("apify_discover, apify_start, apify_collect") ("apify:search"))
    (is-carrying (answer (cell-entry "nodecode-apify" "apify" "instagram"))
      ("1 Actors:") ("apify~instagram-scraper") ("Instagram Scraper"))))

;;; --- a refusal is a repair, never a crash -----------------------------------------

(deftest apify-cell-a-refused-answer-is-the-model-repair ()
  (with-apify-answers (("/v2/store" (values "{\"error\":{\"message\":\"Invalid token\"}}" 401)))
    (is-carrying (answer (apify-tool "apify_discover" (nlk:json-object "query" "x")))
      ("ERROR: apify:") ("HTTP 401") ("Invalid token"))))
