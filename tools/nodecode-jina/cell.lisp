;;;; cell.lisp --- the primer, the section, the /jina command, the declaration.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin was one Python module registering a web backend with the
;;;; harness, so the harness' web_extract and web_search reached Jina through
;;;; it. Nodecode has no such switchboard, so what the plugin did is two
;;;; tools (tools.lisp) and this file: the section that holds the key, the
;;;; guide the model reads with (help :jina), and /jina for the operator.
;;;;
;;;; Config, a sibling top-level key:
;;;;   "jina": {"api_key": "...", "api_key_env": "JINA_API_KEY",
;;;;            "reader_url": "https://r.jina.ai/", "search_url": "https://s.jina.ai/",
;;;;            "timeout_seconds": 60, "max_results": 5, "max_chars": 6000,
;;;;            "max_pages": 8}
;;;; A vetoed section ("enabled": false) installs nothing.

(in-package #:nodecode-jina)

(defparameter +primer+
  "Jina is available through the nodecode-jina cell: reading sends a url to r.jina.ai, which
fetches the page and answers it as clean Markdown, and search asks s.jina.ai.
  jina_read    read pages. urls, absolute http or https, up to max_pages a call. Answers each
               page's final url, title and text, then any url that failed and why. Needs no key:
               the anonymous Reader is rate limited to about 20 requests a minute.
  jina_search  find sources. query, and num (1 to 20, the section's max_results by default).
               Needs JINA_API_KEY. Answers numbered sources - title, url, snippet - and no page
               text: read the few that matter with jina_read.
How to work: search to find urls, read the pages that matter, and cite the url you used. A page
behind a login, a paywall or a bot wall can come back empty or partial, and that is one page's
error, not the call's. On HTTP 429 the anonymous rate limit is spent: slow down, or set a key. Jina
fetches the page on its own servers, so a url only this machine can reach will not read.
The operator reads one page or runs one search from the keyboard with /jina <url or words>."
  "What (help :jina) answers while the cell runs.")

(defun status-text ()
  "What this cell is connected to, without ever printing the key."
  (format nil "jina: reader ~a, search ~a; key ~a; one call reads ~d page~:p of up to ~d ~
characters, ~d source~:p a search~%Read a page with /jina <url>, search with /jina <words>."
          (endpoint (setting :reader-url) +reader-url+)
          (endpoint (setting :search-url) +search-url+)
          (if (api-key) "set" "MISSING (reading works without one)")
          (setting :max-pages)
          (setting :max-chars)
          (setting :max-results)))

(defun run-command (args session-id)
  "One /jina invocation: with no argument, what the cell is connected to; with
a url, that page read; with anything else, a search."
  (declare (ignore session-id))
  (let ((text (nlk:trimmed (or args ""))))
    (cond ((zerop (length text)) (status-text))
          ((web-url-p text)
           (tool-read (decode-read-call (nlk:make-json-object "urls" (vector text)))))
          (t (tool-search (decode-search-call (nlk:make-json-object "query" text)))))))

(defun announce ()
  "Say, once, whether a key is there: the plugin's own is_available gate, said
where the operator and the model both read it."
  (nle:notice (if (api-key)
                   "jina: ready; a key is set, so search works and reading is not at the anonymous limit"
                   (format nil "jina: no JINA_API_KEY, so reading runs at Jina's anonymous limit ~
(about 20 requests a minute) and jina_search will refuse; a key is at ~a"
                           +key-url+))
              :key "nodecode-jina"))

(nle:define-cell jina
  (:section ("jina")
    (:guide "reading needs no key at all; a key at https://jina.ai/api-dashboard/ turns on search and lifts the reading limit, so paste it in api_key or export JINA_API_KEY; reader_url and search_url only change for a proxy of your own; next step: /jina shows what the cell is using")
    ("api_key" :secret
     :doc "the Jina API key (optional: reading works without one)")
    ("api_key_env" :env
     :doc "the environment variable holding the key (JINA_API_KEY is read either way)")
    ("reader_url" :string
     :doc "the reader's address: https://r.jina.ai/ unless you go through a proxy")
    ("search_url" :string
     :doc "the search address: https://s.jina.ai/ unless you go through a proxy")
    ("timeout_seconds" :integer :default 60 :min 1
     :doc "one request's ceiling")
    ("max_results" :integer :default 5 :min 1
     :doc "how many sources a search answers with unless the call names its own count")
    ("max_chars" :integer :default 6000 :min 200
     :doc "how much of one page's text a result carries")
    ("max_pages" :integer :default 8 :min 1
     :doc "how many urls one jina_read call reads"))
  (:help :jina
         "jina: read any page as Markdown, and search the web through Jina - (help :jina)"
         +primer+)
  (:command "jina" 'run-command
            :description "Jina: read a page, search the web, or say what the cell is using"
            :session nil)
  (:tool "jina_read" (nlk:contract 'read-call) #'tool-read)
  (:tool "jina_search" (nlk:contract 'search-call) #'tool-search)
  (:start (lambda ()
            (announce)
            (nle:on-stop (lambda () (nle:notice nil :key "nodecode-jina"))))))
