;;;; tools.lisp --- the five tools, their arguments, and what each answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin exposed five tools from Tavily's hosted MCP server. The names,
;;;; the argument names and the endpoints are Tavily's, unchanged; the
;;;; transport is the part that changed, from MCP over HTTP to one REST POST
;;;; per call (see NOTICE). Each tool is one NLK:DEFINE-CONTRACT, so the
;;;; shape the model is shown and the shape the body reads are one
;;;; declaration, and a member the declaration does not carry has no reader.
;;;;
;;;; Every tool answers text: the model reads it, and the numbers and URLs
;;;; stay where it can cite them.

(in-package #:nodecode-tavily)

;;; --- tavily_search ----------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search the web. Answers the sources Tavily found — title, url, snippet, date — and its
own short answer when one was asked for. Use it to discover sources; use tavily_extract to read
one.")
  ("query" :text :required t
   :doc "the search query, under 400 characters")
  ("search_depth" :string :options '("ultra-fast" "fast" "basic" "advanced")
   :doc "ultra-fast, fast, basic or advanced; advanced costs more credits and reads more pages")
  ("max_results" :integer
   :doc "how many sources to return; the section's max_results when the call names none")
  ("topic" :string :options '("general" "news")
   :doc "general by default; news for an explicit news request")
  ("time_range" :string :options '("day" "week" "month" "year")
   :doc "keep results from the last day, week, month or year")
  ("start_date" :string :doc "earliest publication date, YYYY-MM-DD")
  ("end_date" :string :doc "latest publication date, YYYY-MM-DD")
  ("include_domains" :array :of :string
   :doc "only these domains, as host names: sec.gov, arxiv.org")
  ("exclude_domains" :array :of :string
   :doc "never these domains")
  ("country" :string
   :doc "a full country name, to boost results from it on a general-topic search")
  ("include_raw_content" :boolean
   :doc "carry each page's content too, instead of reading it with tavily_extract")
  ("include_images" :boolean :doc "also answer the images the results carry")
  ("include_image_descriptions" :boolean
   :doc "give each image a description; needs include_images")
  ("include_favicon" :boolean :doc "carry each source's favicon url")
  ("exact_match" :boolean :doc "match the query's quoted phrases exactly"))

(defun format-search (body max-chars)
  "BODY — a /search answer — as the text the model reads."
  (let ((results (objects-of body))
        (answer (object-text body "answer"))
        (request (object-text body "request_id")))
    (with-output-to-string (stream)
      (when answer (format stream "answer: ~a~%~%" answer))
      (if (null results)
          (format stream "no results~@[ (request ~a - keep it for a support question)~]~%" request)
          (progn
            (loop for result in results
                  for index from 1
                  do (format stream "~d. ~a~%   ~a~@[   [~a]~]~%~@[   ~a~%~]"
                             index
                             (or (object-text result "title") "(untitled)")
                             (or (object-text result "url") "")
                             (object-text result "published_date")
                             (clip (object-text result "content") max-chars)))
            (when request (format stream "~%request ~a~%" request)))))))

(defun tool-search (call)
  "tavily_search: Tavily's /search."
  (let ((body (post "/search"
                     (bolt "query" (search-call-query call)
                           "search_depth" (search-call-search-depth call)
                           "max_results" (or (search-call-max-results call) (setting :max-results))
                           "topic" (search-call-topic call)
                           "time_range" (search-call-time-range call)
                           "start_date" (nonempty (search-call-start-date call))
                           "end_date" (nonempty (search-call-end-date call))
                           "include_domains" (strings (search-call-include-domains call))
                           "exclude_domains" (strings (search-call-exclude-domains call))
                           "country" (search-call-country call)
                           "include_raw_content" (search-call-include-raw-content call)
                           "include_images" (search-call-include-images call)
                           "include_image_descriptions" (search-call-include-image-descriptions call)
                           "include_favicon" (search-call-include-favicon call)
                           "exact_match" (search-call-exact-match call)))))
    (format-search body (setting :max-chars))))

;;; --- tavily_extract ---------------------------------------------------------

(nlk:define-contract extract-call
  (:doc "Read the content of pages already known: give their urls, answer their text. Use it
after tavily_search, on the sources that matter, instead of searching again.")
  ("urls" :array :of :string :required t
   :doc "the urls to read, as absolute http or https urls")
  ("extract_depth" :string :options '("basic" "advanced")
   :doc "advanced reads JavaScript-heavy and dynamic pages, and costs more credits")
  ("format" :string :options '("markdown" "text") :slot format-mode
   :doc "markdown keeps the page's headings and links; text is plain")
  ("include_images" :boolean :doc "also answer the images the pages carry"))

(defun format-extract (body max-chars)
  "BODY — an /extract answer — as the text the model reads."
  (let ((results (objects-of body))
        (failed (nlk:json-value body :array "failed_results")))
    (with-output-to-string (stream)
      (loop for result in results
            do (format stream "url: ~a~%~a~%~%"
                       (or (object-text result "url") "")
                       (or (clip (object-text result "raw_content") max-chars) "(empty)")))
      (when (and failed (plusp (length failed)))
        (format stream "failed:~%")
        (loop for failure across failed
              when (hash-table-p failure)
                do (format stream "  ~a - ~a~%"
                           (or (object-text failure "url") "")
                           (or (object-text failure "error") "unknown")))))))

(defun tool-extract (call)
  "tavily_extract: Tavily's /extract."
  (let ((urls (strings (extract-call-urls call))))
    (unless urls (fail "tavily_extract: urls is empty; name at least one absolute url"))
    (format-extract (post "/extract"
                          (bolt "urls" urls
                                "extract_depth" (extract-call-extract-depth call)
                                "format" (extract-call-format-mode call)
                                "include_images" (extract-call-include-images call)))
                    (setting :max-chars))))

;;; --- tavily_map -------------------------------------------------------------

(nlk:define-contract map-call
  (:doc "Discover the urls of a website, without reading the pages. Use it before choosing which
pages to read with tavily_extract.")
  ("url" :text :required t :doc "the site's starting url: https://docs.example.com")
  ("max_depth" :integer :doc "how many links deep to walk from the start (1-5)")
  ("max_breadth" :integer
   :doc "how many links to follow per level (1-500)")
  ("limit" :integer :doc "how many urls to return in all")
  ("instructions" :string
   :doc "a sentence naming what to find, so the walk follows the pages that matter")
  ("select_paths" :array :of :string :doc "only paths matching these, e.g. /docs/.*")
  ("select_domains" :array :of :string :doc "only these domains")
  ("allow_external" :boolean :doc "follow links to other domains too"))

(defun url-list (body)
  "BODY's urls as a list: /map answers strings, /crawl objects."
  (let ((results (nlk:json-value body :array "results")))
    (loop for entry across (or results #())
          collect (cond ((stringp entry) entry)
                        ((hash-table-p entry) (object-text entry "url"))
                        (t nil))
          into urls
          finally (return (remove nil urls)))))

(defun tool-map (call)
  "tavily_map: Tavily's /map."
  (let* ((body (post "/map"
                      (bolt "url" (map-call-url call)
                            "max_depth" (map-call-max-depth call)
                            "max_breadth" (map-call-max-breadth call)
                            "limit" (map-call-limit call)
                            "instructions" (nonempty (map-call-instructions call))
                            "select_paths" (strings (map-call-select-paths call))
                            "select_domains" (strings (map-call-select-domains call))
                            "allow_external" (map-call-allow-external call))))
         (urls (url-list body)))
    (with-output-to-string (stream)
      (format stream "~a: ~d url~:p~%~@[base ~a~%~]"
              (or (object-text body "base_url") (map-call-url call))
              (length urls)
              (object-text body "base_url"))
      (dolist (url urls) (format stream "  ~a~%" url)))))

;;; --- tavily_crawl -----------------------------------------------------------

(nlk:define-contract crawl-call
  (:doc "Read a bounded part of a website: crawl from a starting url and answer each page's
content. Bounded by limit, max_depth and max_breadth — a site is never crawled whole.")
  ("url" :text :required t :doc "the starting url: https://docs.example.com")
  ("max_depth" :integer :doc "how many links deep to walk (1-5)")
  ("max_breadth" :integer :doc "how many links to follow per level (1-500)")
  ("limit" :integer :doc "how many pages to read in all")
  ("instructions" :string :doc "a sentence naming what to collect")
  ("select_paths" :array :of :string :doc "only paths matching these, e.g. /docs/.*")
  ("select_domains" :array :of :string :doc "only these domains")
  ("allow_external" :boolean :doc "follow links to other domains too")
  ("chunks_per_source" :integer
   :doc "how many excerpts of each page to keep (1-3)")
  ("extract_depth" :string :options '("basic" "advanced")
   :doc "advanced reads JavaScript-heavy pages, and costs more credits")
  ("include_images" :boolean :doc "also answer the images the pages carry"))

(defun tool-crawl (call)
  "tavily_crawl: Tavily's /crawl."
  (let* ((body (post "/crawl"
                      (bolt "url" (crawl-call-url call)
                            "max_depth" (crawl-call-max-depth call)
                            "max_breadth" (crawl-call-max-breadth call)
                            "limit" (crawl-call-limit call)
                            "instructions" (nonempty (crawl-call-instructions call))
                            "select_paths" (strings (crawl-call-select-paths call))
                            "select_domains" (strings (crawl-call-select-domains call))
                            "allow_external" (crawl-call-allow-external call)
                            "chunks_per_source" (crawl-call-chunks-per-source call)
                            "extract_depth" (crawl-call-extract-depth call)
                            "include_images" (crawl-call-include-images call))))
         (results (objects-of body)))
    (with-output-to-string (stream)
      (format stream "crawled ~a: ~d page~:p~%" (crawl-call-url call) (length results))
      (loop for result in results
            do (format stream "~%~a~%~a"
                       (or (object-text result "url") "")
                       (or (clip (object-text result "raw_content") (setting :max-chars))
                           "(empty)"))))))

;;; --- tavily_research --------------------------------------------------------

(nlk:define-contract research-call
  (:doc "Ask a research question and get a multi-source report with citations. Slow and paid:
one call, minutes at most, and the question should be worth a report rather than a search.")
  ("query" :text :required t
   :doc "the research question, in one or two sentences")
  ("model" :string :options '("mini" "pro")
   :doc "mini answers in well under a minute for most questions; pro researches deeper and takes longer"))

(defun research-report (body)
  "BODY's report text, whichever member carries it."
  (or (object-text body "answer")
      (object-text body "content")
      (object-text body "report")
      (object-text body "response")))

(defun research-sources (body)
  "BODY's sources as a list of objects, whichever member carries them."
  (let ((sources (or (nlk:json-value body :array "sources")
                      (nlk:json-value body :array "citations")
                      (nlk:json-value body :array "results"))))
    (and sources (remove-if-not #'hash-table-p (coerce sources 'list)))))

(defun format-research (body)
  "BODY — a /research answer — as the text the model reads."
  (let ((report (research-report body))
        (sources (research-sources body)))
    (with-output-to-string (stream)
      (format stream "~a~%"
              (or report "(the report is still being written; ask again in a minute)"))
      (when sources
        (format stream "~%sources:~%")
        (loop for source in sources
              for index from 1
              do (format stream "  ~d. ~a~@[ - ~a~]~%"
                         index
                         (or (object-text source "title") (object-text source "url") "")
                         (and (object-text source "title") (object-text source "url")))))
      (let ((request (object-text body "request_id")))
        (when request (format stream "~%request ~a~%" request))))))

(defun tool-research (call)
  "tavily_research: Tavily's /research, under the section's own deadline."
  (let ((body (post "/research"
                     (bolt "query" (research-call-query call)
                           "model" (research-call-model call))
                     :timeout (setting :research-timeout-seconds))))
    (format-research body)))
