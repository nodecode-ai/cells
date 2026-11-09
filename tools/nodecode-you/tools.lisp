;;;; tools.lisp --- the four verbs, and the tools that reach them.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Each tool is one declaration (NLK:DEFINE-CONTRACT) of the arguments a
;;;; model may pass, and one body that turns the record into an MCP call. The
;;;; declaration is the shape the provider is shown and the shape the body
;;;; reads, so the two cannot drift; a member the declaration does not carry
;;;; has no reader the body could use.
;;;;
;;;; The tool names are the original MCP servers' (`you-search',
;;;; `you-contents', `you-research', `you-finance'), and the argument names are
;;;; the ones the plugin's skills tell a model to pass, so a model that read
;;;; those skills calls the same things here.

(in-package #:nodecode-you)

;;; --- the key ---------------------------------------------------------------

(defun api-key ()
  "The You.com API key: the section's, else YDC_API_KEY, else NIL."
  (let ((key (or (setting :api-key) (uiop:getenv "YDC_API_KEY"))))
    (and (stringp key) (plusp (length (nlk:trimmed key))) (nlk:trimmed key))))

(defun require-key (tool)
  "The key, or a refusal naming TOOL and where a key comes from."
  (or (api-key)
      (fail "~a needs a You.com API key: paste one into the you section's api_key, or set ~
YDC_API_KEY in this machine's environment; you_search alone answers without one"
            tool)))

(defun headers (key)
  "The request headers a call carries: the key as an Authorization bearer when there is one."
  (when key `(("authorization" . ,(format nil "Bearer ~a" key)))))

;;; --- what one answer carries -------------------------------------------------

(defun result-text (result)
  "RESULT's text blocks, else its structuredContent as JSON."
  (let* ((blocks (nlk:json-value result :array "content"))
         (texts (and blocks
                     (loop for block across blocks
                           for text = (nlk:json-value block :string "text")
                           when (and text (plusp (length text))) collect text)))
         (structured (nlk:json-value result :object "structuredContent")))
    (cond ((and texts (plusp (length texts))) (format nil "~{~a~^~%~}" texts))
          (structured (nlk:pretty-json structured))
          (t "You.com answered with nothing to read."))))

(defun cut (text)
  "TEXT under the section's max_chars, saying what it left out."
  (let ((limit (setting :max-chars)))
    (if (<= (length text) limit)
        text
        (format nil "~a~%[cut at ~d of ~d characters: raise the section's max_chars to read ~
on, or ask a narrower question]"
                (subseq text 0 limit) limit (length text)))))

(defun call-tool (url tool arguments key)
  "TOOL called on URL; => its answer as text, or a refusal carrying what it said was wrong."
  (let ((result (rpc url tool arguments (headers key))))
    (if (nlk:json-value result :boolean "isError")
        (fail "~a" (result-text result))
        (cut (result-text result)))))

;;; --- the verbs --------------------------------------------------------------

(defun search (query &key count freshness country safesearch)
  "Search the live web with You.com; => the answer's text, naming who answered.
Without a key the free profile answers: search results only."
  (let* ((key (api-key))
         (arguments (nlk:json-object "query" query
                                      :opt "count" count
                                      :opt "freshness" freshness
                                      :opt "country" country
                                      :opt "safesearch" safesearch)))
    (format nil "You.com search, ~a:~%~a"
            (if key
                "the account's key"
                "free profile, no key: snippets and urls, no you_contents")
            (call-tool (if key +endpoint+ +free-endpoint+) "you-search" arguments key))))

(defun contents (urls &key formats extraction)
  "Read URLs through You.com; => the pages as text."
  (let ((arguments (nlk:json-object "urls" (coerce urls 'vector)
                                     "formats" (coerce (or formats '("markdown")) 'vector)
                                     :opt "extraction" extraction)))
    (call-tool +endpoint+ "you-contents" arguments (require-key "you_contents"))))

(defun research (input &key effort)
  "One shot cited research: INPUT answered by You.com's managed pipeline."
  (call-tool +research-endpoint+ "you-research"
             (nlk:json-object "input" input :opt "research_effort" effort)
             (require-key "you_research")))

(defun finance (query)
  "A finance question answered from company financials, filings and market sources."
  (call-tool +finance-endpoint+ "you-finance" (nlk:json-object "query" query)
             (require-key "you_finance")))

;;; --- you_search -------------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search the live web with You.com: the pages that answer a question, with their urls and snippets. Read a page with you_contents before relying on an exact number; a snippet is not reading.")
  ("query" :text :required t
   :doc "the question as search words, one facet per call: 3-6 words work better than a sentence")
  ("count" :integer :slot result-count
   :doc "how many results, 1 to 10; the section's you.count when absent")
  ("freshness" :string :options '("day" "week" "month" "year")
   :doc "keep only pages published or updated in this window")
  ("country" :string
   :doc "two-letter country code to search from, e.g. US")
  ("safesearch" :string :options '("off" "moderate" "strict")
   :doc "the adult-content filter"))

(defun search-tool (report)
  "you_search: You.com's search, on the free profile when there is no key."
  (let ((count (or (search-call-result-count report) (setting :count))))
    (unless (<= 1 count 10)
      (fail "count is 1 to 10 results, and this call asked for ~d" count))
    (search (search-call-query report)
            :count count
            :freshness (search-call-freshness report)
            :country (search-call-country report)
            :safesearch (search-call-safesearch report))))

;;; --- you_contents -----------------------------------------------------------

(nlk:define-contract contents-call
  (:doc "Read specific pages through You.com and answer their text: markdown, html, or the query-relevant highlights. Use it on the urls a search named, one to three at a time.")
  ("urls" :array :required t :of :string
   :doc "the pages to read, absolute http or https urls")
  ("formats" :array :of :string
   :doc "how each page is returned: markdown (the default), html, text; html only when layout or tables matter")
  ("extraction" :string :options '("full" "highlights")
   :doc "highlights returns the passages that bear on the question, for one focused fact; full reads the page"))

(defun http-url-p (url)
  (and (stringp url)
       (or (uiop:string-prefix-p "https://" url) (uiop:string-prefix-p "http://" url))))

(defun contents-tool (report)
  "you_contents: the pages behind the urls, read in full."
  (let* ((urls (coerce (contents-call-urls report) 'list))
         (formats (coerce (or (contents-call-formats report) '("markdown")) 'list)))
    (unless (plusp (length urls))
      (fail "urls needs at least one http or https url"))
    (dolist (url urls)
      (unless (http-url-p url) (fail "~s is not an http or https url" url)))
    (contents urls :formats formats :extraction (contents-call-extraction report))))

;;; --- you_research -----------------------------------------------------------

(nlk:define-contract research-call
  (:doc "One shot cited research: You.com's managed pipeline searches, reads and answers in one call. Use it for a question that wants a synthesized, sourced answer; use you_search and you_contents when you want to choose the sources yourself.")
  ("input" :text :required t
   :doc "the research question, whole, in one or two sentences")
  ("research_effort" :string :options '("lite" "standard" "deep" "exhaustive")
   :doc "how far to go: lite is a quick answer, exhaustive a long one; the cost and the wait rise with it"))

(defun research-tool (report)
  "you_research: one question, one cited answer."
  (research (research-call-input report) :effort (research-call-research-effort report)))

;;; --- you_finance ------------------------------------------------------------

(nlk:define-contract finance-call
  (:doc "A finance question answered from company financials, SEC filings, earnings, guidance, analyst context and valuation comparisons. Include the date or timeframe for anything market-sensitive.")
  ("query" :text :required t
   :doc "the finance question, naming the company or ticker and the period"))

(defun finance-tool (report)
  "you_finance: the market question, answered with its sources."
  (finance (finance-call-query report)))
