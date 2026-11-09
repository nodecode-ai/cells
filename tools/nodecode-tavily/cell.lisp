;;;; cell.lisp --- the primer, the section, the /tavily command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was an MCP server entry plus six skills. The entry is the five
;;;; tools tools.lisp declares; the skills are +PRIMER+, which the model reads
;;;; with (help :tavily) and whose one line rides every request. Nothing hooks
;;;; the provider path: no model is served here.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "tavily": {"api_key": "...", "api_key_env": "TAVILY_API_KEY",
;;;;              "base_url": "https://api.tavily.com", "timeout_seconds": 60,
;;;;              "research_timeout_seconds": 600, "max_results": 5,
;;;;              "max_chars": 4000}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-tavily)

(defparameter +primer+
  "Tavily is available through the nodecode-tavily cell: five tools that search and read the web,
on the Tavily account the key belongs to. Every call spends the account's credits.
  tavily_search    find sources. query, then search_depth (ultra-fast, fast, basic, advanced),
                   max_results, topic (general or news), time_range (day, week, month, year) or
                   start_date and end_date (YYYY-MM-DD), include_domains and exclude_domains,
                   country, include_answer-free extras: include_raw_content, include_images,
                   include_favicon, exact_match. Answers the numbered sources - title, url,
                   snippet, date - and Tavily's own short answer when there is one.
  tavily_extract   read pages already known. urls, extract_depth (advanced for JavaScript
                   pages), format (markdown keeps headings and links), include_images. Answers
                   each page's text, then any url that failed and why.
  tavily_map       list a website's urls without reading them. url, max_depth, max_breadth,
                   limit, instructions, select_paths, select_domains, allow_external.
  tavily_crawl     read a bounded part of a site. url and the same bounds, plus
                   chunks_per_source and extract_depth. A site is never crawled whole: say limit.
  tavily_research  a cited multi-source report on one question. query, model (mini, pro). Slow
                   and paid - minutes, not seconds. Ask once, then read the report.
How to work: search to discover, extract the few pages that matter, map before a crawl, and
research only a question that deserves a report. Check the hostname of a url before trusting it.
Cite the urls in your answer, and keep Tavily's request id when you report a failure. A search
with no results proves no absence. On a 432 or 429 the account is out of credits or rate: stop
and tell the operator rather than retrying.
The operator runs a search from the keyboard with /tavily <query>."
  "What (help :tavily) answers while the cell runs: the plugin's six skills in one topic.")

(defun read-settings (values table)
  "The settings the cell runs on: the declaration's values, with the key
resolved out of its variable, and the two endpoints' own defaults."
  (declare (ignore table))
  (list :api-key (or (nonempty (getf values :api-key))
                      (let ((variable (nonempty (getf values :api-key-env))))
                        (and variable (nonempty (uiop:getenv variable))))
                      (nonempty (uiop:getenv "TAVILY_API_KEY")))
        :api-key-env (getf values :api-key-env)
        :base-url (or (nonempty (getf values :base-url)) +base-url+)
        :timeout-seconds (getf values :timeout-seconds)
        :research-timeout-seconds (getf values :research-timeout-seconds)
        :max-results (getf values :max-results)
        :max-chars (getf values :max-chars)))

;;; --- the operator's command --------------------------------------------------

(defun status-text ()
  "What this cell is connected to, without ever printing the key."
  (format nil "tavily: ~a, key ~a; search timeout ~ds, research ~ds; ~d tools (~{~a~^, ~})~%\
Search from here with /tavily <query>."
          (root)
          (if (api-key) "set" "MISSING")
          (setting :timeout-seconds)
          (setting :research-timeout-seconds)
          (count-if (lambda (entry) (eql 0 (search "tavily_" (first entry)))) nle::*tools*)
          '("tavily_search" "tavily_extract" "tavily_map" "tavily_crawl" "tavily_research")))

(defun run-command (args session-id)
  "One /tavily invocation: with no argument, what the cell is connected to;
with one, a search the operator reads."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (plusp (length query))
        (tool-search (decode-search-call (nlk:make-json-object "query" query)))
        (status-text))))

;;; --- the cell -------------------------------------------------------------------

(nle:define-cell tavily
  (:section ("tavily")
    (:guide "make a key at app.tavily.com and paste it in api_key, or export TAVILY_API_KEY (or name the variable in api_key_env); base_url only for a proxy of your own; max_results and max_chars bound one answer's size; research_timeout_seconds bounds a report; next step: /tavily shows the connection")
    ("api_key" :secret
     :doc "the Tavily API key (starts with tvly-)")
    ("api_key_env" :env
     :doc "the environment variable holding the key, when it is not written here (TAVILY_API_KEY is read either way)")
    ("base_url" :string
     :doc "the API address: https://api.tavily.com unless you go through a proxy of your own")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one search, extract, map or crawl request's ceiling")
    ("research_timeout_seconds" :integer :default 600 :min 30
     :doc "one research report's ceiling: a pro report can take minutes")
    ("max_results" :integer :default 5 :min 1
     :doc "how many sources a search answers with unless the call names its own count")
    ("max_chars" :integer :default 4000 :min 200
     :doc "how much of one page's text a result carries"))
  (:settings 'read-settings)
  (:help :tavily
         "tavily: search the web, extract pages, map and crawl sites, and research a question - (help :tavily)"
         +primer+)
  (:command "tavily" 'run-command
            :description "Tavily: the connection, or a web search"
            :argument-hint "QUERY"
            :session nil)
  (:tool "tavily_search" (nlk:contract 'search-call) #'tool-search)
  (:tool "tavily_extract" (nlk:contract 'extract-call) #'tool-extract)
  (:tool "tavily_map" (nlk:contract 'map-call) #'tool-map)
  (:tool "tavily_crawl" (nlk:contract 'crawl-call) #'tool-crawl)
  (:tool "tavily_research" (nlk:contract 'research-call) #'tool-research)
  (:start (lambda ()
            (check-config))))
