;;;; contracts.lisp --- the two tools' arguments, one declaration each.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The one authority for a tool's shape: the JSON Schema the provider is
;;;; shown is projected from these, the value a body runs on is materialized
;;;; from them, and a call that does not satisfy one is refused before the
;;;; body sees it. They load before web.lisp and cell.lisp, whose bodies read
;;;; the records.

(in-package #:nodecode-web-search-plus)

(nlk:define-contract search-request
  (:doc "Search the web through a configured provider. With provider auto the query's shape picks the first engine (Brave for a general question, Serper for security, shopping and news, Exa for documentation and papers, SearXNG for a self-hosted instance) and the next configured engine answers if that one fails or finds nothing. Search to discover: read the result list, and extract a page only when a line does not already answer. Cite the urls you used.")
  ("query" :text :required t
   :doc "the search query, in the words a person would use")
  ("provider" :text :default "auto"
   :doc "auto (routing plus fallback), or one engine to force: brave, serper, tavily, exa, searxng")
  ("count" :integer :slot results-count :default 0
   :doc "how many results to answer; 0 uses the section's max_results")
  ("freshness" :text :default ""
   :doc "only recent pages: day, week, month or year. Applied natively by brave, serper, tavily and searxng")
  ("search_type" :text :default "search"
   :doc "search (the whole web) or news. Serper answers news natively; the other engines run the normal search and a note says so")
  ("include_domains" :array :of :string
   :doc "keep only results on these hosts or their subdomains, e.g. arxiv.org")
  ("exclude_domains" :array :of :string
   :doc "drop results on these hosts or their subdomains, e.g. pinterest.com")
  ("mode" :text :default "normal"
   :doc "normal reads the result list; research also reads the top pages and answers their text under pages"))

(nlk:define-contract extract-request
  (:doc "Read pages as clean text, with provider fallback: Tavily, Firecrawl, Exa, then a plain fetch here that strips the HTML. A page a provider cannot read falls through to the next one, and each failure is a note. Pass a url you already know; the answer carries the page's text, cut to a length the note reports, with the url and title above it.")
  ("urls" :array :of :string :required t
   :doc "the pages to read, http:// or https://, one to five per call")
  ("provider" :text :default "auto"
   :doc "auto (fallback down the chain), or one to force: tavily, firecrawl, exa, native (a plain fetch here)")
  ("max_chars" :integer :default 0
   :doc "the most characters to answer per page; 0 uses the section's max_chars")
  ("render_js" :boolean :default nil
   :doc "read through Firecrawl, which renders JavaScript, first; a page that needs it answers empty from a plain fetch"))
