;;;; tools.lisp --- the two calls, the two tool bodies, the words.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original gives the model two MCP tools, web_search and web_fetch,
;;;; and its bundle's skill teaches when to reach for each. This file is the
;;;; same two calls as Lisp verbs plus the same two tools registered against
;;;; boundary contracts, so a model call and an eval form land in one place.
;;;;
;;;; The server does the ranking and the excerpting: this cell sends the
;;;; queries and the urls, and hands back the text the server answered with,
;;;; unchanged. Nothing is trimmed, summarised or re-sorted here.

(in-package #:nodecode-parallel-search)

;;; --- the boundary: what a model call carries ---------------------------------------

(nlk:define-contract web-search-request
  (:doc "Search the web through Parallel's free Search server: ranked results with
answer-ready excerpts. Read the excerpts first; fetch a page only when the question
needs the page itself.")
  ("objective" :text :required t
   :doc "what the search is for, in one sentence; it steers the ranking")
  ("search_queries" :array :of :string
   :doc "one to three short queries; a broad task passes several in one call instead of chaining calls")
  ("max_results" :integer
   :doc "how many results to ask for; the section's max_results answers when this is absent")
  ("max_chars_per_result" :integer
   :doc "the excerpt budget per result, in characters"))

(nlk:define-contract web-fetch-request
  (:doc "Read specific pages through Parallel's free Search server, as token-efficient
markdown. Use it when the user named a url, when exact wording is needed, or when the
search excerpts disagree.")
  ("urls" :array :of :string :required t
   :doc "the pages to read; several related urls can go in one call")
  ("objective" :text
   :doc "what the pages are being read for, in one sentence"))

;;; --- the verbs --------------------------------------------------------------------

(defun as-list (value)
  "VALUE -- a string, a list or a JSON array -- as a list of strings."
  (etypecase value
    (string (list value))
    (vector (coerce value 'list))
    (list value)))

(defun trimmed-list (value)
  "VALUE's strings, trimmed, the empty ones dropped."
  (loop for item in (as-list value)
        for text = (nlk:trimmed (or item ""))
        when (plusp (length text)) collect text))

(defun search (queries &key objective max-results max-chars-per-result)
  "Search the web through Parallel's free Search server. QUERIES is one query
or a list of them; OBJECTIVE says what the search is for and defaults to the
first query. => the server's own text: ranked results with excerpts and their
urls. Cite them."
  (let* ((wanted (trimmed-list queries))
         (objective (or (and objective (nlk:trimmed objective))
                        (first wanted))))
    (unless (and objective (plusp (length objective)))
      (fail "search needs a query: give one, or an :objective saying what the search is for"))
    (rpc-call "web_search"
              (nlk:json-object "objective" objective
                                :when wanted "search_queries" (coerce wanted 'vector)
                                "max_results" (or max-results (setting :max-results))
                                :when max-chars-per-result
                                "max_chars_per_result" max-chars-per-result))))

(defun fetch (urls &key objective)
  "Read URLs through Parallel's free Search server. URLS is one url or a list
of them. => the server's own text: each page as markdown, bounded by the
server."
  (let ((wanted (trimmed-list urls))
        (objective (and objective (nlk:trimmed objective))))
    (unless wanted
      (fail "fetch needs a url: http:// or https://, one or a list of them"))
    (rpc-call "web_fetch"
              (nlk:json-object "urls" (coerce wanted 'vector)
                                :when (and objective (plusp (length objective)))
                                "objective" objective))))

;;; --- the two tools the model calls -------------------------------------------------

(defun web-search-tool (request)
  "The web_search tool: the queries a call carries, answered by the server."
  (search (or (web-search-request-search-queries request)
              (web-search-request-objective request))
          :objective (web-search-request-objective request)
          :max-results (web-search-request-max-results request)
          :max-chars-per-result (web-search-request-max-chars-per-result request)))

(defun web-fetch-tool (request)
  "The web_fetch tool: the pages a call names, answered as markdown."
  (fetch (web-fetch-request-urls request)
         :objective (web-fetch-request-objective request)))

;;; --- the operator's own door -------------------------------------------------------

(defun run-slash (args session-id)
  "/parallel QUERY: the operator's own search, answered as the server answers
the model."
  (declare (ignore session-id))
  (let ((query (nlk:trimmed (or args ""))))
    (if (zerop (length query))
        "parallel: type a query to search the web with Parallel's free server. ~
         The cell also gives the model web_search and web_fetch."
        (search query))))

;;; --- the words the model reads -----------------------------------------------------

(defparameter *primer*
  "Parallel Search is available through the nodecode-parallel-search cell: two model
tools, web_search and web_fetch, and the same two calls in the PARALLEL package from
eval. Both answer the server's own text and need no key.
  (parallel:search \"query\" &key objective max-results max-chars-per-result)
      One query, or several in a list for one broad task, answered with ranked results:
      title, url and an excerpt meant to answer on its own. The objective says what the
      search is for and steers the ranking; it defaults to the first query. Read the
      excerpts before asking again - when a result already answers, do not fetch it to
      confirm - and cite the urls you used.
  (parallel:fetch \"https://...\" &key objective)
      Specific pages as token-efficient markdown, one url or a list of them in one call.
      Use it when the user named a page, when exact wording is needed, or when excerpts
      conflict or fall short.
Doctrine: search to discover, fetch a url you already know. Search first for anything
current, factual or comparative; answer from the excerpts and cite them. ERROR:
PARALLEL-SEARCH-ERROR names the problem. The anonymous free tier is rate limited
(HTTP 429): wait, or send fewer queries, rather than retrying into the limit."
  "What (help :parallel) answers while the cell runs.")
