;;;; cell.lisp --- the primer, the section, the /serpapi command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin registered one web-search provider and seven tools on
;;;; SerpApi's single endpoint. The provider's own search is the eighth tool
;;;; here; the seven keep their names, arguments and engines. Nothing serves
;;;; a model, so no provider path is hooked.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "serpapi": {"api_key": "...", "api_key_env": "SERPAPI_API_KEY",
;;;;               "base_url": "https://serpapi.com/search", "timeout_seconds": 15,
;;;;               "extract_timeout_seconds": 60, "max_results": 5, "max_chars": 4000}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-serpapi)

(defparameter +primer+
  "SerpApi is available through the nodecode-serpapi cell: eight tools over Google's results, on
the SerpApi account the key belongs to. Every call spends one search credit, and a crawl spends
one per page.
  serpapi_search              the web. query, location, language, country, limit. Answers title,
                               url and snippet per result.
  serpapi_maps_search         places, businesses, restaurants. query, plus a location or a
                               latitude and longitude (never both), zoom, nearby, limit.
  serpapi_news_search         recent reporting. query, location, language, country, limit.
  serpapi_shopping_search     products and prices. query, location, minimum_price,
                               maximum_price, sort, free_shipping, on_sale, limit.
  serpapi_hotels_search       stays. query, check_in_date and check_out_date (YYYY-MM-DD),
                               adults, children with children_ages, currency, price bounds,
                               hotel_class, sort, vacation_rentals.
  serpapi_flights_search      fares. departure_id and arrival_id as three-letter airport codes
                               or /m/ or /g/ KGMIDs (not city codes), outbound_date and
                               return_date, passengers, cabin, stops, sort, departure_token for
                               return choices.
  serpapi_travel_explore_search  destination ideas from a departure point: month or fixed dates,
                               travel_duration, price ceiling.
  serpapi_extract              a page, a site's urls, or a crawl. url, mode (scrape, map,
                               crawl), include_tags and exclude_tags, and for a crawl
                               max_pages and max_depth.
Every tool answers Markdown unless the call asks for output json, except extract, whose
compact_mode false answers the full JSON. Check the hostname of a url before trusting it, cite
the urls in the answer, and on HTTP 401 check the key, on 429 stop and tell the operator rather
than retrying. The operator searches from the keyboard with /serpapi <query>."
  "What (help :serpapi) answers while the cell runs: the plugin's tools in one topic.")

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's values with every default made explicit."
  (declare (ignore table))
  (list :api-key (nonempty (getf values :api-key))
        :api-key-env (nonempty (getf values :api-key-env))
        :base-url (nonempty (getf values :base-url))
        :timeout-seconds (getf values :timeout-seconds)
        :extract-timeout-seconds (getf values :extract-timeout-seconds)
        :max-results (getf values :max-results)
        :max-chars (getf values :max-chars)))

;;; --- the operator's command --------------------------------------------------

(defun status-text ()
  "What this cell is connected to, without ever printing the key."
  (format nil "serpapi: ~a, key ~a; search timeout ~ds, extract ~ds; ~d tools~%\
Search from here with /serpapi <query>."
          (root)
          (if (api-key) "set" "MISSING")
          (setting :timeout-seconds)
          (setting :extract-timeout-seconds)
          (count-if (lambda (entry) (eql 0 (search "serpapi_" (first entry)))) nle::*tools*)))

(defun run-command (args session-id)
  "One /serpapi invocation: with no argument, what the cell is connected to;
with one, a web search the operator reads."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (tool-search (decode-search-call (nlk:make-json-object "query" query)))
        (status-text))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell serpapi
  (:section ("serpapi")
    (:guide "make a key at serpapi.com/manage-api-key and paste it in api_key, or export SERPAPI_API_KEY (or name the variable in api_key_env); base_url only for a proxy of your own; max_results bounds a result list; max_chars bounds one crawled page; next step: /serpapi shows the connection")
    ("api_key" :secret
     :doc "the SerpApi API key")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (SERPAPI_API_KEY is read either way)")
    ("base_url" :string
     :doc "the endpoint: https://serpapi.com/search unless you go through a proxy of your own")
    ("timeout_seconds" :integer :default 15 :min 1
     :doc "one search request's ceiling; SerpApi answers these quickly")
    ("extract_timeout_seconds" :integer :default 60 :min 5
     :doc "one extract request's ceiling: a crawl reads many pages")
    ("max_results" :integer :default 5 :min 1
     :doc "how many results a search keeps unless the call names its own count")
    ("max_chars" :integer :default 4000 :min 200
     :doc "how much of one crawled page's text a crawl carries"))
  (:settings 'read-settings)
  (:help :serpapi
         "serpapi: web, maps, news, shopping, hotels, flights, travel and page extraction - (help :serpapi)"
         +primer+)
  (:command "serpapi" 'run-command
            :description "SerpApi: the connection, or a web search"
            :argument-hint "QUERY")
  (:tool "serpapi_search" (nlk:contract 'search-call) #'tool-search)
  (:tool "serpapi_maps_search" (nlk:contract 'maps-call) #'tool-maps)
  (:tool "serpapi_news_search" (nlk:contract 'news-call) #'tool-news)
  (:tool "serpapi_shopping_search" (nlk:contract 'shopping-call) #'tool-shopping)
  (:tool "serpapi_hotels_search" (nlk:contract 'hotels-call) #'tool-hotels)
  (:tool "serpapi_flights_search" (nlk:contract 'flights-call) #'tool-flights)
  (:tool "serpapi_travel_explore_search" (nlk:contract 'travel-call) #'tool-travel)
  (:tool "serpapi_extract" (nlk:contract 'extract-call) #'tool-extract)
  (:start (lambda ()
            (check-config))))
