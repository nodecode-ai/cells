;;;; tools.lisp --- the two boundaries, the two verbs, the tool bodies, /keenable.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The original offered its harness two MCP tools, search_web_pages and
;;;; fetch_page_content, and two skills that taught the model to use them.
;;;; Here the same two calls are a contract each (the wire members the server
;;;; publishes, member for member), the same calls as Lisp verbs, and the
;;;; skills' guidance in the primer cell.lisp carries.

(in-package #:nodecode-keenable)

;;; --- what a model call carries -----------------------------------------------------
;; The server's own inputSchema, reduced to the members worth offering: it
;; refuses an unknown member, so the contract carries exactly these.

(nlk:define-contract keenable-search-request
  (:doc "Search the web with Keenable and answer the pages it found, each with
its title, url, a date when known, and snippets. Describe the page you want,
not keywords; narrow by site and publish date.")
  ("query" :text :required t
   :doc "a natural-language description of the page you want, e.g. \"official changelog for FastAPI 0.120\"")
  ("site" :text
   :doc "one domain to search inside, e.g. docs.python.org")
  ("published_after" :text
   :doc "YYYY-MM-DD: only pages published on or after it; this is the recency filter")
  ("published_before" :text
   :doc "YYYY-MM-DD: only pages published on or before it")
  ("max_results" :integer :slot result-count
   :doc "how many results, 1 to 50; the section's max_results answers when this is absent")
  ("snippet_max_length" :integer :slot snippet-length
   :doc "characters of snippet per result, 180 to 10000; lower saves context")
  ("mode" :text :options '("pro" "realtime")
   :doc "pro (default) ranks harder; realtime answers fastest"))

(nlk:define-contract keenable-fetch-request
  (:doc "Read one page through Keenable: clean markdown, or only the part a
prompt asks for. Use it when a search snippet falls short, or the user gave a url.")
  ("url" :text :required t
   :doc "the http(s) page to read")
  ("prompt" :text
   :doc "an instruction, at most 2000 characters: the page is read server-side and only the answer comes back")
  ("max_chars" :integer :slot page-chars
   :doc "cap on returned characters; the server's own default applies when absent"))

(defparameter +result-ceiling+ 50
  "The most results the server answers with in one call.")

(defparameter +snippet-bounds+ '(180 . 10000)
  "The narrowest and widest snippet the server takes.")

(defparameter +prompt-chars+ 2000
  "The longest prompt the server takes.")

(defun nonblank (value)
  "VALUE as a trimmed non-empty string, or NIL."
  (let ((text (and (stringp value) (nlk:trimmed value))))
    (and text (plusp (length text)) text)))

(defun json-object-with (&rest pairs)
  "A JSON object from PAIRS, an alist of (KEY . VALUE), leaving out every
member whose value is NIL or empty: an absent member is the default the
server picks."
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key . value) in pairs
          when (and value (or (not (stringp value)) (plusp (length value))))
            do (setf (gethash key object) value))
    object))

(defun result-count (count)
  "COUNT within 1 and the ceiling; the section's max_results answers a call
that names none."
  (max 1 (min +result-ceiling+ (or count (setting :max-results) 10))))

(defun snippet-length (length)
  "LENGTH within the server's snippet bounds, or NIL to let it pick."
  (when length
    (max (car +snippet-bounds+) (min (cdr +snippet-bounds+) length))))

;;; --- the verbs -----------------------------------------------------------------

(define-verb search (query &key site published-after published-before
                            max-results snippet-max-length mode)
  "Search the web through Keenable. QUERY describes the page wanted; SITE
confines it to one domain; PUBLISHED-AFTER and PUBLISHED-BEFORE bound the
publish date as YYYY-MM-DD; MAX-RESULTS 1-50 (the section's answers when
absent); SNIPPET-MAX-LENGTH 180-10000 characters; MODE \"pro\" or
\"realtime\". => the pages found, each with its title, url, date and snippets."
  (let ((query (nonblank query)))
    (unless query
      (fail "search needs a query: describe the page you want, e.g. (keenable:search \"official changelog for FastAPI 0.120\")"))
    (call-tool "search_web_pages"
                (json-object-with
                 (cons "query" query)
                 (cons "site" (nonblank site))
                 (cons "published_after" (nonblank published-after))
                 (cons "published_before" (nonblank published-before))
                 (cons "max_results" (result-count max-results))
                 (cons "snippet_max_length" (snippet-length snippet-max-length))
                 (cons "mode" (nonblank mode))))))

(define-verb fetch (url &key prompt max-chars)
  "Read one page through Keenable as clean markdown, or only the part PROMPT
asks for. URL is the http(s) page; PROMPT is an instruction at most 2000
characters; MAX-CHARS caps what comes back. => the page, or the answer."
  (let ((url (nonblank url))
        (prompt (nonblank prompt)))
    (unless (and url (or (uiop:string-prefix-p "https://" url)
                          (uiop:string-prefix-p "http://" url)))
      (fail "fetch needs an http(s) url, e.g. (keenable:fetch \"https://docs.keenable.ai/rate-limits\")"))
    (when (and prompt (> (length prompt) +prompt-chars+))
      (fail "a prompt is at most ~d characters and this one is ~d" +prompt-chars+ (length prompt)))
    (call-tool "fetch_page_content"
                (json-object-with
                 (cons "url" url)
                 (cons "prompt" prompt)
                 (cons "max_chars" max-chars)))))

;;; --- the tools the model calls -----------------------------------------------------

(defun search-tool (request)
  "The keenable_search tool: the members one call carries, answered as the
verb answers them."
  (search (keenable-search-request-query request)
          :site (keenable-search-request-site request)
          :published-after (keenable-search-request-published-after request)
          :published-before (keenable-search-request-published-before request)
          :max-results (keenable-search-request-result-count request)
          :snippet-max-length (keenable-search-request-snippet-length request)
          :mode (keenable-search-request-mode request)))

(defun fetch-tool (request)
  "The keenable_fetch tool: one url, answered as the verb answers it."
  (fetch (keenable-fetch-request-url request)
         :prompt (keenable-fetch-request-prompt request)
         :max-chars (keenable-fetch-request-page-chars request)))

;;; --- the operator's own door -------------------------------------------------------

(defun run-slash (args session-id)
  "/keenable QUERY: the operator's own search, answered as the model's tool
is; typed alone, what the cell gives and how to set it."
  (declare (ignore session-id))
  (let ((query (nonblank args)))
    (if query
        (search query)
        (format nil "keenable: type a query, e.g. /keenable common lisp implementations. ~
This cell gives the model two tools -- keenable_search (~d results when it names no count) ~
and keenable_fetch -- through Keenable's hosted server. No key is needed; one in ~
api_key only lifts the shared pool's hourly cap." (result-count nil)))))
