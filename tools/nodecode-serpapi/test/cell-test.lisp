;;;; cell-test.lisp --- lifecycle, the eight tools, and /serpapi, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches SerpApi, needs a
;;;; key or touches the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start, wrong values and a missing key refused), the
;;;; tools through the seam the model calls them by -- their url, their
;;;; parameters, their answer -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *serpapi-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *serpapi-reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-serpapi-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged and answered by
*SERPAPI-REPLY*, so no test dials SerpApi."
  `(let ((*serpapi-requests* '())
         (*serpapi-reply* (lambda (method url args)
                             (declare (ignore method url args))
                             (error "unexpected serpapi request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *serpapi-requests*)
                                (funcall *serpapi-reply* method url args))
       ,@body)))

(defun serpapi-asked ()
  "The stubbed requests, oldest first."
  (reverse *serpapi-requests*))

(defun serpapi-reply (text &optional (status 200))
  (setf *serpapi-reply* (lambda (method url args)
                           (declare (ignore method url args))
                           (values text status))))

(defun serpapi-params (request)
  "REQUEST's query parameters, decoded, as an alist."
  (quri:uri-query-params (quri:uri (second request))))

(defun serpapi-param (request name)
  (cdr (assoc name (serpapi-params request) :test #'string=)))

(defun serpapi-result (body index)
  "BODY's results array, entry INDEX."
  (aref (nlk:json-value body :array "results") index))

(defun serpapi-page (body index)
  "BODY's pages array, entry INDEX."
  (aref (nlk:json-value body :array "pages") index))

(defun serpapi-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "serpapi-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "serpapi" "SERPAPI-CELL-" :start nodecode-serpapi:start-cell)

(define-cell-lifecycle-tests "serpapi"
  (:fixture with-serpapi-wire)
  (:config "api_key" "test-key")
  (:help :serpapi)
  (:command "serpapi")
  (:running
   (is (nle::find-tool "serpapi_search") "search is registered")
   (is (nle::find-tool "serpapi_maps_search") "and maps")
   (is (nle::find-tool "serpapi_news_search") "and news")
   (is (nle::find-tool "serpapi_shopping_search") "and shopping")
   (is (nle::find-tool "serpapi_hotels_search") "and hotels")
   (is (nle::find-tool "serpapi_flights_search") "and flights")
   (is (nle::find-tool "serpapi_travel_explore_search") "and travel explore")
   (is (nle::find-tool "serpapi_extract") "and extract, the original's seven and its search")
   (is (= 8 (count-if (lambda (entry) (eql 0 (search "serpapi_" (first entry)))) nle::*tools*)) "eight of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "serpapi_search")) "and stop takes every one back")
   (is (null (nle::find-tool "serpapi_extract"))))
  (:refused ("timeout_seconds" 0)
            ("extract_timeout_seconds" 2)
            ("max_results" 0)
            ("max_chars" 10)))

(deftest serpapi-cell-a-start-with-no-key-refuses-and-installs-nothing (with-serpapi-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (serpapi-start))))
      (is-carrying text "needs a key" "SERPAPI_API_KEY")
      (is (null (nle::find-tool "serpapi_search")) "nothing was installed"))))

(deftest serpapi-cell-the-key-may-come-from-the-environment (with-serpapi-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (and (equal name "SERPAPI_API_KEY") "env-key"))
    (with-cell-stop ((serpapi-start))
      (serpapi-reply "# Results")
      (serpapi-call "serpapi_search" "{\"query\": \"x\"}")
      (let ((sent (serpapi-param (first (serpapi-asked)) "api_key")))
        (is (equal "env-key" sent) "the exported key is the one sent")))))

;;; --- search, and the operator's command -----------------------------------------

(deftest serpapi-cell-search-asks-one-engine-and-answers-its-markdown (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (serpapi-reply "# Google Search Results

## Organic Results

| # | Title | Link |
| --- | --- | --- |
| 1 | A page | https://a.test/1 |
")
    (let ((answer (serpapi-call "serpapi_search" "{\"query\": \"lisp\", \"country\": \"US\"}")))
      (let* ((request (first (serpapi-asked)))
             (url (second request)))
        (is (eq :get (first request)) "one GET")
        (is (search "https://serpapi.com/search" url) "to SerpApi's search endpoint")
        (is (equal "google_light" (serpapi-param request "engine")) "the provider's own engine")
        (is (equal "lisp" (serpapi-param request "q")) "carrying the query")
        (is (equal "md" (serpapi-param request "output")) "asking for Markdown by default")
        (is (equal "us" (serpapi-param request "gl")) "the market code, lowercased")
        (is (equal "test-key" (serpapi-param request "api_key")) "with the section's key"))
      (is (search "1 | A page" answer) "SerpApi's own Markdown reaches the model"))))

(deftest serpapi-cell-a-refused-key-is-the-model-s-repair (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (serpapi-reply "{\"error\": \"Invalid API key\"}" 401)
    (let ((answer (serpapi-call "serpapi_search" "{\"query\": \"x\"}")))
      (is (search "401" answer) "the failure names the status")
      (is (search "api_key" answer) "and tells the model what to check")
      (is (null (search "test-key" answer)) "and never echoes the key"))))

(deftest serpapi-cell-search-answers-json-when-asked (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (serpapi-reply
     "{\"organic_results\": [{\"title\": \"A\", \"link\": \"https://a.test/1\", \"snippet\": \"s\"},
                             {\"title\": \"B\", \"link\": \"https://b.test/2\", \"snippet\": \"t\"}]}")
    (let ((answer (serpapi-call "serpapi_search" "{\"query\": \"x\", \"output\": \"json\", \"limit\": 1}")))
      (let ((body (nlk:decode-json answer :whole t)))
        (is (= 1 (nlk:json-value body :integer "results_count")) "the limit is applied")
        (is (equal "A" (nlk:json-value (serpapi-result body 0) :text "title")) "the first result is a row")
        (is (equal "google_light" (nlk:json-value body :text "engine")) "and the engine is named")))))

(deftest serpapi-cell-the-command-searches-or-says-what-it-is-connected-to (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (let ((status (nodecode-serpapi::run-command "" nil)))
      (is (search "https://serpapi.com/search" status) "no argument reports the connection")
      (is (search "key set" status) "and that a key is there")
      (is (null (search "test-key" status)) "without ever printing the key")
      (is (search "8 tools" status) "and how many tools it serves")
      (is (null (serpapi-asked)) "and sends nothing"))
    (serpapi-reply "# Results")
    (let ((answer (nodecode-serpapi::run-command "  lisp  " nil)))
      (is (search "# Results" answer) "an argument runs one search and answers it")
      (is (equal "lisp" (serpapi-param (first (serpapi-asked)) "q")) "the query trimmed, as typed"))))

;;; --- maps, news, shopping -------------------------------------------------------

(deftest serpapi-cell-maps-carries-a-place-row-and-its-coordinates (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (serpapi-reply
     "{\"local_results\": [{\"title\": \"Cafe One\", \"address\": \"1 Main St\", \"rating\": 4.5,
        \"gps_coordinates\": {\"latitude\": 51.5, \"longitude\": -0.1}},
       {\"title\": \"Cafe Two\"}]}")
    (let ((answer (serpapi-call "serpapi_maps_search"
                                "{\"query\": \"coffee\", \"location\": \"London\", \"output\": \"json\", \"limit\": 1}")))
      (let ((request (first (serpapi-asked))))
        (is (equal "google_maps" (serpapi-param request "engine")) "the maps engine")
        (is (equal "London" (serpapi-param request "location")) "the origin")
        (is (equal "14" (serpapi-param request "z")) "with the default zoom"))
      (let ((body (nlk:decode-json answer :whole t)))
        (is (= 1 (nlk:json-value body :integer "results_count")) "the limit is applied")
        (is (equal "Cafe One" (nlk:json-value (serpapi-result body 0) :text "title")) "the place")
        (let ((coordinates (nlk:json-value (serpapi-result body 0) :any "coordinates")))
          (is (= 51.5 (nlk:json-value coordinates :number "latitude")) "with its coordinates"))))))

(deftest serpapi-cell-maps-refuses-a-half-specified-origin (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (let ((answer (serpapi-call "serpapi_maps_search" "{\"query\": \"x\", \"latitude\": 10}")))
      (is (search "latitude and longitude" answer) "a latitude without a longitude is refused")
      (is (null (serpapi-asked)) "and nothing was sent"))))

(deftest serpapi-cell-news-cuts-the-table-to-the-limit-asked-for (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (serpapi-reply
     "# News

## News Results

| # | Title |
| --- | --- |
| 1 | First |
| 2 | Second |
| 3 | Third |

## Pagination

| Next | x |
")
    (let ((answer (serpapi-call "serpapi_news_search" "{\"query\": \"x\", \"limit\": 2}")))
      (is (search "First" answer) "the first row is kept")
      (is (search "Second" answer) "so is the second")
      (is (null (search "Third" answer)) "the third is cut")
      (is (search "| Next | x |" answer) "and no other table is touched"))))

(deftest serpapi-cell-shopping-reports-price-and-link (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (serpapi-reply
     "{\"shopping_results\": [{\"title\": \"Kettle\", \"source\": \"Shop\", \"price\": \"$20\",
        \"extracted_price\": 20, \"product_link\": \"https://shop.test/k\"}]}")
    (let ((answer (serpapi-call "serpapi_shopping_search"
                                "{\"query\": \"kettle\", \"sort\": \"price_low_to_high\", \"output\": \"json\"}")))
      (let ((request (first (serpapi-asked))))
        (is (equal "google_shopping_light" (serpapi-param request "engine")) "the shopping engine")
        (is (equal "1" (serpapi-param request "sort_by")) "the price sort, as Google's code"))
      (let ((body (nlk:decode-json answer :whole t)))
        (is (equal "Kettle" (nlk:json-value (serpapi-result body 0) :text "title")) "the product")
        (let ((link (nlk:json-value (serpapi-result body 0) :text "link")))
          (is (equal "https://shop.test/k" link) "and its link"))))))

;;; --- travel ---------------------------------------------------------------------

(deftest serpapi-cell-hotels-refuses-a-stay-that-ends-before-it-begins (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (let ((answer (serpapi-call "serpapi_hotels_search"
                                "{\"query\": \"Oslo\", \"check_in_date\": \"2026-05-10\",
                                  \"check_out_date\": \"2026-05-09\"}")))
      (is (search "check_out_date must be after check_in_date" answer) "the dates are checked")
      (is (null (serpapi-asked)) "and nothing was sent"))))

(deftest serpapi-cell-hotels-carry-the-stay-and-its-guests (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (serpapi-reply "# Hotels")
    (let ((answer (serpapi-call "serpapi_hotels_search"
                                "{\"query\": \"Oslo\", \"check_in_date\": \"2026-05-10\",
                                  \"check_out_date\": \"2026-05-12\", \"adults\": 2,
                                  \"children\": 1, \"children_ages\": [7]}")))
      (let ((request (first (serpapi-asked))))
        (is (equal "google_hotels" (serpapi-param request "engine")) "the hotels engine")
        (is (equal "2026-05-10" (serpapi-param request "check_in_date")) "the check-in date")
        (is (equal "1" (serpapi-param request "children")) "the child count")
        (is (equal "7" (serpapi-param request "children_ages")) "and the child's age"))
      (is (search "# Hotels" answer) "SerpApi's Markdown is the answer"))))

(deftest serpapi-cell-flights-need-a-return-date-for-a-round-trip (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (let ((answer (serpapi-call "serpapi_flights_search"
                                "{\"departure_id\": \"LHR\", \"arrival_id\": \"CDG\",
                                  \"outbound_date\": \"2026-05-10\", \"trip_type\": \"round_trip\"}")))
      (is (search "return_date is required" answer) "the round trip is refused without it")
      (is (null (serpapi-asked)) "and nothing was sent"))
    (serpapi-reply "# Flights")
    (let ((answer (serpapi-call "serpapi_flights_search"
                                "{\"departure_id\": \"LHR\", \"arrival_id\": \"CDG\",
                                  \"outbound_date\": \"2026-05-10\", \"return_date\": \"2026-05-14\"}")))
      (let ((request (first (serpapi-asked))))
        (is (equal "google_flights" (serpapi-param request "engine")) "the flights engine")
        (is (equal "1" (serpapi-param request "type")) "a round trip, as Google's code")
        (is (equal "1" (serpapi-param request "adults")) "one adult by default")
        (is (equal "2026-05-14" (serpapi-param request "return_date")) "and both dates"))
      (is (search "# Flights" answer) "the itineraries come back"))))

(deftest serpapi-cell-travel-explore-refuses-fixed-dates-and-a-month-together (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (let ((answer (serpapi-call "serpapi_travel_explore_search"
                                "{\"departure_id\": \"LHR\", \"outbound_date\": \"2026-05-10\",
                                  \"month\": 6}")))
      (is (search "use fixed dates or month" answer) "the two shapes of a search are exclusive")
      (is (null (serpapi-asked)) "and nothing was sent"))))

;;; --- extract ---------------------------------------------------------------------

(deftest serpapi-cell-extract-scrapes-one-page (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (serpapi-reply
     "{\"search_parameters\": {\"engine\": \"extract\", \"mode\": \"scrape\"},
       \"markdown\": \"# The page\\n\\nIts text.\"}")
    (let ((answer (serpapi-call "serpapi_extract" "{\"url\": \"https://a.test/page\"}")))
      (let ((request (first (serpapi-asked))))
        (is (equal "extract" (serpapi-param request "engine")) "the extract engine")
        (is (equal "scrape" (serpapi-param request "mode")) "scraping by default")
        (is (equal "https://a.test/page" (serpapi-param request "url")) "the page it was given"))
      (is (search "Its text." answer) "the page's Markdown is the answer"))))

(deftest serpapi-cell-extract-maps-the-urls-of-a-site (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (serpapi-reply
     "{\"search_parameters\": {\"engine\": \"extract\", \"mode\": \"map\"},
       \"urls\": [{\"url\": \"https://docs.test/a\", \"title\": \"A\"}, {\"url\": \"https://docs.test/b\"}]}")
    (let ((answer (serpapi-call "serpapi_extract" "{\"url\": \"https://docs.test\", \"mode\": \"map\"}")))
      (is (equal "map" (serpapi-param (first (serpapi-asked)) "mode")) "the map mode is sent")
      (is (search "- https://docs.test/a - A" answer) "every url is listed, with its title")
      (is (search "- https://docs.test/b" answer) "including the untitled one"))))

(deftest serpapi-cell-extract-answers-the-full-json-when-asked (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (serpapi-reply
     "{\"search_parameters\": {\"engine\": \"extract\", \"mode\": \"crawl\"},
       \"pages\": [{\"url\": \"https://docs.test/a\", \"markdown\": \"Page A\"}]}")
    (let ((answer (serpapi-call "serpapi_extract"
                                "{\"url\": \"https://docs.test\", \"mode\": \"crawl\",
                                  \"compact_mode\": false, \"max_pages\": 2}")))
      (is (equal "2" (serpapi-param (first (serpapi-asked)) "max_pages")) "the crawl bound is sent")
      (let ((body (nlk:decode-json answer :whole t)))
        (let ((markdown (nlk:json-value (serpapi-page body 0) :text "markdown")))
          (is (equal "Page A" markdown) "the structured response comes back whole"))))))

(deftest serpapi-cell-extract-refuses-a-private-or-credentialed-url (with-serpapi-wire)
  (with-cell-stop ((serpapi-start "api_key" "test-key"))
    (let ((answer (serpapi-call "serpapi_extract" "{\"url\": \"https://user:pw@a.test/x\"}")))
      (is (search "without credentials" answer) "a url carrying credentials is refused")
      (is (null (serpapi-asked)) "and nothing was sent"))))
