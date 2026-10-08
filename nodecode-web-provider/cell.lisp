;;;; cell.lisp --- the verb, the manual, the settings, START-CELL.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; omp's `web' provider is no model: its rows (kind `search') are the
;;;; engines its web search tool can pick, keyed or not. This cell carries the
;;;; keyless ones the shipped websearch cell does not -- Google, Startpage,
;;;; Ecosia, Mojeek, a SearXNG instance of your own, and omp's merge of the
;;;; five scrapers -- as one verb in the engines: package the model calls
;;;; through eval. While the cell runs, (help :engines) answers the manual,
;;;; and every request's help section carries one line naming the verb.
;;;;
;;;; Config, a sibling top-level key (not `web', the core's own, nor
;;;; `websearch', the shipped cell's):
;;;;   "web-provider": {"engine": "public", "max_results": 10,
;;;;                    "searxng_endpoint": "https://searx.example.org", ...}

(in-package #:nodecode-web-provider)

(defparameter +manual+
  "Keyless web search engines are available through the nodecode-web-provider cell: one Lisp
function in the engines: package, called through eval. It returns a string.
  (engines:search \"query\" &key engine n recency)
      ENGINE is one of \"public\" (the default: Startpage, Google, DuckDuckGo, Ecosia and Mojeek asked at
      once, a page named by several ranked first), \"google\", \"startpage\", \"duckduckgo\", \"ecosia\",
      \"mojeek\", or \"searxng\" (an instance configured as web-provider.searxng_endpoint). N results
      (default 15 for public, 10 for one engine; at most 30 and 20). RECENCY is \"day\", \"week\",
      \"month\" or \"year\", a time filter an engine without one ignores. Google-style operators
      (site:, quotes, -word) pass to the engines as typed.
      Each result is one numbered line - title, snippet, date when known - with its url under it; an
      engine of the merge that failed or ran out of time is a [note] above them.
These engines scrape result pages: one may answer a bot challenge instead of results, and says so.
To read a page, use web:fetch from the websearch cell when it runs. ERROR: WEB-PROVIDER-ERROR names
the problem; retry a failing call at most once, with another engine."
  "What (help :engines) answers while the cell runs.")

(defparameter +title-limit+ 120)
(defparameter +snippet-limit+ 240)
(defparameter +answer-limit+ 6000)

(defun render (query answer hits notes related)
  "The answer text: each note in brackets, the engine's own answer when it
gave one, one numbered line per hit -- title, snippet, date -- with its url
under it, then related searches; `No results' when nothing came back."
  (nlk:clip
   (with-output-to-string (out)
     (dolist (note notes) (format out "[~a]~%" note))
     (when notes (terpri out))
     (when (nonblank answer) (format out "~a~%~%" (nonblank answer)))
     (if hits
         (loop for hit in hits
               for i from 1
               do (format out "~:[~%~%~;~]~d. ~a~@[ - ~a~]~@[ (~a)~]~%   ~a"
                          (= i 1) i
                          (nlk:clip (nlk:one-line (getf hit :title)) +title-limit+ :ellipsis "…")
                          (and (getf hit :snippet) (nlk:clip (nlk:one-line (getf hit :snippet)) +snippet-limit+ :ellipsis "…"))
                          (getf hit :date) (getf hit :url)))
         (format out "No results for ~s." query))
     (when related (format out "~%~%Related: ~{~a~^; ~}" (subseq related 0 (min 5 (length related))))))
   +answer-limit+ :disclose t))

(defun name-of (value what options)
  "VALUE, a string or a symbol, as one of OPTIONS, or a refusal naming WHAT."
  (let ((name (and (or (stringp value) (symbolp value)) (string-downcase (string value)))))
    (or (find name options :test #'equal)
        (fail "~a must be one of ~{~a~^, ~}, got ~s" what options value))))

(define-verb search (query &key engine n recency)
  "Search the web for QUERY with a keyless engine. ENGINE is public (Startpage,
Google, DuckDuckGo, Ecosia and Mojeek at once, merged; the default),
google, startpage, duckduckgo, ecosia, mojeek or searxng. N results (15 for
public, 10 otherwise; at most 30 and 20). RECENCY day, week, month or year.
Returns a string: [notes], an engine's own answer, then one numbered line
per result -- title, snippet, date when known -- and its url."
  (nle:receipt "calls" (list* :verb "engines:search" :family "search"
                              (and (stringp query) (list :source query))))
  (unless (nonblank query) (fail "query must be a non-empty string"))
  (let* ((engine (if engine (name-of engine "engine" +engines+) (setting :engine)))
         (recency (and recency (name-of recency "recency" +recencies+)))
         (public (equal engine "public"))
         (ceiling (if public 30 20))
         (n (min ceiling (max 1 (if (integerp n) n (or (setting :max-results) (if public 15 10)))))))
    (handler-case
        (if public
            (multiple-value-bind (hits notes) (search-public query n recency)
              (render query nil hits notes nil))
            (multiple-value-bind (hits answer related) (one-engine engine query n recency)
              (render query answer hits nil related)))
      (engine-failure (condition) (fail "~a" (failure-detail condition))))))

(nle:define-cell web-provider
  (:section ("web-provider")
    (:guide "no key: engine picks who answers engines:search (public merges Startpage, Google, DuckDuckGo, Ecosia and Mojeek); searxng needs searxng_endpoint, an instance of your own, with a token or Basic auth when it asks for one")
    ("engine" :choice :options +engines+ :default "public"
     :doc "the engine a search asks when it names none")
    ("max_results" :integer :min 1
     :doc "how many results a search answers with by default (15 for public, 10 for one engine when unset)")
    ("searxng_endpoint" :string :default ""
     :doc "the SearXNG instance's base URL; empty reads SEARXNG_ENDPOINT")
    ("searxng_token" :secret
     :doc "a bearer token for the instance; empty reads SEARXNG_TOKEN")
    ("searxng_basic_username" :string :default ""
     :doc "a Basic auth user for the instance; empty reads SEARXNG_BASIC_USERNAME")
    ("searxng_basic_password" :secret
     :doc "its password; empty reads SEARXNG_BASIC_PASSWORD")
    ("searxng_categories" :string :default "" :doc "categories, comma-separated")
    ("searxng_engines" :string :default "" :doc "the instance's engines, names or shortcuts, comma-separated")
    ("searxng_language" :string :default "" :doc "a language code, e.g. en or zh-CN")
    ("searxng_safesearch" :choice :options '("" "0" "1" "2") :default ""
     :doc "0 off, 1 moderate, 2 strict; empty is the instance's own"))
  (:help :engines "engines:search asks keyless engines (Google, Startpage, DuckDuckGo, Ecosia, Mojeek, merged; or SearXNG)" +manual+))
