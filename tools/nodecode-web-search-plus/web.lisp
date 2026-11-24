;;;; web.lisp --- the providers, the one network seam, and the plain fetch.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every byte this cell moves goes through NLK:HTTP, so a test stubs that one
;;;; name and never touches the network. An HTTP status is a fact the caller
;;;; records as a note; only a transport failure (DNS, TLS, a timeout) is an
;;;; error, and it is caught here and turned into the same note.
;;;;
;;;; The engines are the original's, cut to the ones whose API is one request:
;;;; Brave, Serper, Tavily and Exa by key, SearXNG by URL. The extraction side
;;;; is Tavily, Firecrawl, Exa, then a plain fetch that turns HTML into text
;;;; here.

(in-package #:nodecode-web-search-plus)

;;; --- what a model's answer is built from -----------------------------------------

(defparameter +note-chars+ 300
  "How much of a provider's own error body a note quotes back.")

(defun nonblank (text)
  "TEXT trimmed, or NIL when it is not a non-empty string."
  (let ((text (and (stringp text) (nlk:trimmed text))))
    (and text (plusp (length text)) text)))

(defun first-of (vector)
  "VECTOR's first element, or NIL when it is absent or empty."
  (and vector (vectorp vector) (plusp (length vector)) (aref vector 0)))

(defun body-note (text status)
  "A failed exchange as one line: the status and the provider's own words."
  (let ((words (nlk:clip (nlk:trimmed (or text "")) +note-chars+)))
    (format nil "HTTP ~a~@[: ~a~]" status (and (plusp (length words)) words))))

(defun result (title url snippet &optional age)
  "One result as the plist the answer is built from."
  (list :title (or title "")
        :url (or url "")
        :snippet (nlk:one-line (or snippet ""))
        :age (or (and (nonblank age) (nlk:one-line age)) "")))

;;; --- the one seam ----------------------------------------------------------------

(defun request-json (method url &key headers content)
  "URL's decoded JSON answer => (values JSON STATUS NOTE): NOTE says why when
JSON is NIL, and a transport failure is a note, not an error."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http method url
                    :headers (append (list (cons "accept" "application/json")) headers)
                    :content content
                    :timeout (setting :timeout)
                    :connect-timeout (setting :timeout))
        (let ((text (if (stringp body) body (nlk:body-text body))))
          (if (and (integerp status) (< status 400))
              (values (or (ignore-errors (nlk:decode-json text)) (nlk:json-object)) status nil)
              (values nil status (body-note text status)))))
    (error (condition)
      (values nil nil (nlk:one-line (princ-to-string condition))))))

(defun request-text (url)
  "URL's body as text => (values TEXT STATUS NOTE), for the plain fetch."
  (handler-case
      (multiple-value-bind (body status)
          (nlk:http :get url
                    :headers (list (cons "accept" "text/html,application/xhtml+xml,text/plain;q=0.9,*/*;q=0.5"))
                    :timeout (setting :timeout)
                    :connect-timeout (setting :timeout)
                    :max-bytes 4000000)
        (let ((text (if (stringp body) body (nlk:body-text body))))
          (if (and (integerp status) (< status 400))
              (values text status nil)
              (values nil status (body-note text status)))))
    (error (condition)
      (values nil nil (nlk:one-line (princ-to-string condition))))))

(defun api-key (provider)
  "PROVIDER's key from the section, or NIL."
  (cond ((string= provider "brave") (setting :brave-api-key))
        ((string= provider "serper") (setting :serper-api-key))
        ((string= provider "tavily") (setting :tavily-api-key))
        ((string= provider "exa") (setting :exa-api-key))
        ((string= provider "firecrawl") (setting :firecrawl-api-key))
        (t nil)))

(defun provider-configured-p (provider)
  "Whether PROVIDER can be asked at all: a key, or a SearXNG URL."
  (if (string= provider "searxng")
      (plusp (length (or (nonblank (setting :searxng-url)) "")))
      (nonblank (api-key provider))))

;;; --- query, freshness, domains ---------------------------------------------------

(defparameter +search-order+ '("brave" "serper" "tavily" "exa" "searxng")
  "Every search engine this cell speaks to, in the order tried after the routed one.")

(defparameter +extract-order+ '("tavily" "firecrawl" "exa" "native")
  "Every extraction route, in the order tried: the three providers by key, then a plain fetch.")

(defparameter +security-query+
  "(?i)(cve-|vulnerab|exploit|malware|ransomware|phishing|breach|\\bsecurity\\b|\\bsecure\\b|\\bprice\\b|cheapest|discount|coupon|\\bdeal\\b|\\bshop\\b|shopping)"
  "A security or shopping query: Serper's Google results lead, as the original routes them.")

(defparameter +docs-query+
  "(?i)(\\bapi\\b|documentation|\\bdocs\\b|reference|\\bpaper\\b|arxiv|\\bstudy\\b|thesis|journal|\\bsdk\\b|\\blibrar)"
  "A documentation or academic query: Exa leads.")

(defun routed-order (query)
  "The engines to try for QUERY: the routed first choice, then the rest."
  (let ((first (cond ((ppcre:scan +security-query+ query) "serper")
                     ((ppcre:scan +docs-query+ query) "exa")
                     (t "brave"))))
    (cons first (remove first +search-order+ :test #'string=))))

(defun freshness-value (freshness table)
  "FRESHNESS as TABLE maps it, or NIL: an unknown word is no filter."
  (cdr (assoc (string-downcase (nlk:trimmed (or freshness ""))) table :test #'string=)))

(defun url-host (url)
  "URL's host in lower case, without a port, or \"\" when it has none."
  (let* ((url (or url ""))
         (start (let ((mark (cl:search "://" url)))
                  (if mark (+ mark 3) 0)))
         (rest (subseq url start))
         (end (or (position-if (lambda (character) (find character "/?#")) rest) (length rest)))
         (host (string-downcase (subseq rest 0 end))))
    (if (find #\: host) (subseq host 0 (position #\: host)) host)))

(defun host-matches-p (host domain)
  "Whether HOST is DOMAIN or a subdomain of it."
  (let ((domain (string-downcase (nlk:trimmed (or domain "")))))
    (and (plusp (length domain))
         (or (string= host domain)
             (uiop:string-suffix-p host (concatenate 'string "." domain))))))

(defun filter-results (results include exclude)
  "RESULTS kept by the caller's domain lists, filtered here because most
engines take no domain filter on their search endpoint."
  (if (and (null include) (null exclude))
      results
      (remove-if (lambda (entry)
                   (let ((host (url-host (getf entry :url))))
                     (or (loop for domain in exclude thereis (host-matches-p host domain))
                         (and include
                              (not (loop for domain in include
                                         thereis (host-matches-p host domain)))))))
                 results)))

;;; --- the search engines ------------------------------------------------------------

(defun brave-search (query count freshness)
  "Brave's web endpoint. => (values RESULTS NOTE)."
  (let ((params (remove nil
                        (list (cons "q" query)
                              (cons "count" (princ-to-string count))
                              (cons "safesearch" "moderate")
                              (let ((fresh (freshness-value
                                            freshness
                                            '(("day" . "pd") ("week" . "pw")
                                              ("month" . "pm") ("year" . "py")))))
                                (and fresh (cons "freshness" fresh)))))))
    (multiple-value-bind (json status note)
        (request-json :get (format nil "https://api.search.brave.com/res/v1/web/search?~a"
                                   (quri:url-encode-params params))
                      :headers (list (cons "x-subscription-token" (api-key "brave"))))
      (declare (ignore status))
      (if (null json)
          (values nil (format nil "brave: ~a" (or note "no answer")))
          (values (loop for item across (nlk:json-array (nlk:json-value json :object "web") "results")
                        collect (result (nlk:json-value item :string "title")
                                        (nlk:json-value item :string "url")
                                        (or (nlk:json-value item :string "description")
                                            (nlk:json-value item :string "snippet"))
                                        (nlk:json-value item :string "page_age")))
                  nil)))))

(defun serper-search (query count freshness search-type)
  "Serper's Google endpoint, which is also the news vertical. => (values RESULTS NOTE)."
  (let* ((news (string= (string-downcase (nlk:trimmed (or search-type "search"))) "news"))
         (body (nlk:json-object "q" query "num" count "autocorrect" t))
         (tbs (freshness-value freshness
                                '(("day" . "qdr:d") ("week" . "qdr:w")
                                  ("month" . "qdr:m") ("year" . "qdr:y")))))
    (when tbs (setf (gethash "tbs" body) tbs))
    (multiple-value-bind (json status note)
        (request-json :post (format nil "https://google.serper.dev/~a" (if news "news" "search"))
                      :headers (list (cons "x-api-key" (api-key "serper")))
                      :content (nlk:encode-json-object body))
      (declare (ignore status))
      (if (null json)
          (values nil (format nil "serper: ~a" (or note "no answer")))
          (values (loop for item across (nlk:json-array json "organic")
                        collect (result (nlk:json-value item :string "title")
                                        (nlk:json-value item :string "link")
                                        (nlk:json-value item :string "snippet")
                                        (nlk:json-value item :string "date")))
                  nil)))))

(defun tavily-search (query count freshness)
  "Tavily's search endpoint: an AI-facing engine, snippets are page extracts."
  (let ((body (nlk:json-object "query" query "max_results" count "search_depth" "basic"))
        (fresh (freshness-value freshness
                                 '(("day" . "day") ("week" . "week")
                                   ("month" . "month") ("year" . "year")))))
    (when fresh
      (setf (gethash "time_range" body) fresh))
    (multiple-value-bind (json status note)
        (request-json :post "https://api.tavily.com/search"
                      :headers (list (cons "authorization"
                                           (format nil "Bearer ~a" (api-key "tavily"))))
                      :content (nlk:encode-json-object body))
      (declare (ignore status))
      (if (null json)
          (values nil (format nil "tavily: ~a" (or note "no answer")))
          (values (loop for item across (nlk:json-array json "results")
                        collect (result (nlk:json-value item :string "title")
                                        (nlk:json-value item :string "url")
                                        (nlk:json-value item :string "content")))
                  nil)))))

(defun exa-search (query count)
  "Exa's neural search, asking for a text excerpt with each result."
  (let* ((limits (nlk:json-object "maxCharacters" 1200))
         (contents (nlk:json-object "text" limits))
         (body (nlk:json-object "query" query "numResults" count "type" "auto"
                                "contents" contents)))
    (multiple-value-bind (json status note)
        (request-json :post "https://api.exa.ai/search"
                      :headers (list (cons "x-api-key" (api-key "exa")))
                      :content (nlk:encode-json-object body))
      (declare (ignore status))
      (if (null json)
          (values nil (format nil "exa: ~a" (or note "no answer")))
          (values (loop for item across (nlk:json-array json "results")
                        collect (result (or (nlk:json-value item :string "title")
                                            (nlk:json-value item :string "url"))
                                        (nlk:json-value item :string "url")
                                        (or (nlk:json-value item :string "text")
                                            (nlk:json-value item :string "summary"))
                                        (nlk:json-value item :string "publishedDate")))
                  nil)))))

(defun searxng-search (query freshness)
  "A self-hosted SearXNG instance's JSON API; no key, the operator's own URL."
  (let ((fresh (freshness-value freshness
                                 '(("day" . "day") ("week" . "week")
                                   ("month" . "month") ("year" . "year"))))
        (params (remove nil
                        (list (cons "q" query)
                              (cons "format" "json")
                              (cons "safesearch" "1")))))
    (when fresh (setf params (append params (list (cons "time_range" fresh)))))
    (multiple-value-bind (json status note)
        (request-json :get (format nil "~a/search?~a" (setting :searxng-url)
                                   (quri:url-encode-params params)))
      (declare (ignore status))
      (if (null json)
          (values nil (format nil "searxng: ~a" (or note "no answer")))
          (values (loop for item across (nlk:json-array json "results")
                        collect (result (nlk:json-value item :string "title")
                                        (nlk:json-value item :string "url")
                                        (nlk:json-value item :string "content")
                                        (nlk:json-value item :string "publishedDate")))
                  nil)))))

(defun provider-search (provider query count freshness search-type)
  "PROVIDER's answer to QUERY => (values RESULTS NOTE)."
  (cond ((string= provider "brave") (brave-search query count freshness))
        ((string= provider "serper") (serper-search query count freshness search-type))
        ((string= provider "tavily") (tavily-search query count freshness))
        ((string= provider "exa") (exa-search query count))
        ((string= provider "searxng") (searxng-search query freshness))
        (t (values nil (format nil "~a is not a search provider" provider)))))

;;; --- one search, with routing and fallback -----------------------------------------

(defun freshness-note (provider freshness)
  "A note when PROVIDER cannot honour FRESHNESS, else NIL."
  (when (and (nonblank freshness)
             (not (member provider '("brave" "serper" "tavily" "searxng") :test #'string=)))
    (format nil "~a has no recency filter; freshness=~a was not applied" provider freshness)))

(defun run-search (query &key (provider "auto") (count 5) freshness (search-type "search")
                            include exclude)
  "QUERY answered by the routed engine or the next configured one =>
(values PROVIDER RESULTS NOTES). PROVIDER is NIL when nothing answered."
  (let* ((wanted (string-downcase (nlk:trimmed (or provider "auto"))))
         (forced (not (string= wanted "auto")))
         (order (if forced (list wanted) (routed-order query)))
         (order (remove-if-not #'provider-configured-p order))
         (news (string= (string-downcase (nlk:trimmed (or search-type "search"))) "news"))
         (asked (if (or include exclude) (min 20 (* 3 count)) count))
         (notes '()))
    (flet ((try (name)
             (multiple-value-bind (results note)
                 (provider-search name query asked freshness (if news "news" "search"))
               (let ((kept (filter-results results include exclude)))
                 (cond
                   ((plusp (length kept))
                    (when (> (length results) (length kept))
                      (push (format nil "~a found ~a results; the domain lists kept ~a"
                                    name (length results) (length kept))
                            notes))
                    (let ((fresh (freshness-note name freshness)))
                      (when fresh (push fresh notes)))
                    (when (and news (not (string= name "serper")))
                      (push (format nil "~a has no news vertical; it ran the normal search" name)
                            notes))
                    (values (subseq kept 0 (min count (length kept))) (nreverse notes)))
                   (t (push (or note (format nil "~a found nothing" name)) notes)
                      nil))))))
      (cond
        ((null order)
         (values nil nil
                 (list (if forced
                           (format nil "~a is not configured: set its key in the web-search-plus section, or provider auto"
                                   wanted)
                           "no search provider is configured: set brave_api_key, serper_api_key, tavily_api_key, exa_api_key, firecrawl_api_key or searxng_url"))))
        (t (loop for name in order
                 do (multiple-value-bind (kept found) (try name)
                      (when kept
                        (return (values name kept found))))
                 finally (return (values nil nil (nreverse notes)))))))))

;;; --- extraction --------------------------------------------------------------------

(defun url-ok-p (url)
  "Whether URL is an http:// or https:// address, the only scheme fetched."
  (let ((url (string-downcase (nlk:trimmed (or url "")))))
    (or (uiop:string-prefix-p "http://" url)
        (uiop:string-prefix-p "https://" url))))

(defun tavily-extract (url)
  "=> (values TEXT TITLE NOTE)."
  (multiple-value-bind (json status note)
      (request-json :post "https://api.tavily.com/extract"
                    :headers (list (cons "authorization"
                                         (format nil "Bearer ~a" (api-key "tavily"))))
                    :content (nlk:encode-json-object (nlk:json-object "urls" (vector url))))
    (declare (ignore status))
    (cond ((null json) (values nil nil (format nil "tavily: ~a" (or note "no answer"))))
          (t (let ((entry (first-of (nlk:json-array json "results"))))
               (if entry
                   (values (nlk:json-value entry :string "raw_content") nil nil)
                   (values nil nil
                           (format nil "tavily: ~a"
                                   (or (nlk:json-value (first-of (nlk:json-array json "failed_results"))
                                                       :string "error")
                                       "no content")))))))))

(defun firecrawl-extract (url)
  "Firecrawl renders JavaScript, so it is also the route for render_js.
=> (values TEXT TITLE NOTE)."
  (multiple-value-bind (json status note)
      (request-json :post "https://api.firecrawl.dev/v1/scrape"
                    :headers (list (cons "authorization"
                                         (format nil "Bearer ~a" (api-key "firecrawl"))))
                    :content (nlk:encode-json-object
                              (nlk:json-object "url" url
                                                "formats" (vector "markdown")
                                                "onlyMainContent" t)))
    (declare (ignore status))
    (cond ((null json) (values nil nil (format nil "firecrawl: ~a" (or note "no answer"))))
          (t (let ((data (nlk:json-value json :object "data")))
               (if data
                   (values (or (nlk:json-value data :string "markdown")
                               (nlk:json-value data :string "content"))
                           (nlk:json-value data :string "metadata" "title")
                           nil)
                   (values nil nil
                           (format nil "firecrawl: ~a"
                                   (or (nlk:json-value json :string "error") "no content")))))))))

(defun exa-extract (url)
  "=> (values TEXT TITLE NOTE)."
  (multiple-value-bind (json status note)
      (request-json :post "https://api.exa.ai/contents"
                    :headers (list (cons "x-api-key" (api-key "exa")))
                    :content (nlk:encode-json-object
                              (nlk:json-object "urls" (vector url) "text" t)))
    (declare (ignore status))
    (let ((entry (and json (first-of (nlk:json-array json "results")))))
      (if entry
          (values (nlk:json-value entry :string "text")
                  (nlk:json-value entry :string "title")
                  nil)
          (values nil nil (format nil "exa: ~a" (or note "no content")))))))

(defun strip-tags (html)
  (ppcre:regex-replace-all "(?s)<[^>]*>" (or html "") " "))

(defun html-unescape (text)
  "TEXT with the entities a page's text carries turned back into characters."
  (let* ((text (ppcre:regex-replace-all "&#(\\d+);" text
                                       (lambda (match registers)
                                         (declare (ignore match))
                                         (or (ignore-errors
                                              (string (code-char (parse-integer (first registers)))))
                                             ""))))
        (text (ppcre:regex-replace-all "&#[xX]([0-9a-fA-F]+);" text
                                       (lambda (match registers)
                                         (declare (ignore match))
                                         (or (ignore-errors
                                              (string (code-char (parse-integer (first registers) :radix 16))))
                                             "")))))
    (dolist (pair '(("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"")
                     ("&apos;" . "'") ("&nbsp;" . " ")))
      (setf text (ppcre:regex-replace-all (ppcre:quote-meta-chars (car pair)) text (cdr pair))))
    text))

(defun html-to-text (html)
  "HTML's readable text and its title, as two values: scripts and styles
dropped, block ends as line breaks, tags removed, entities decoded, blank
lines gone."
  (let* ((title (ppcre:register-groups-bind (value)
                     ("(?is)<title[^>]*>(.*?)</title>" (or html ""))
                   (nlk:trimmed (html-unescape (strip-tags value)))))
         (body (ppcre:regex-replace-all "(?is)<(script|style|noscript|template)\\b.*?</\\1\\s*>"
                                         (or html "") ""))
         (body (ppcre:regex-replace-all
                "(?i)<(br|hr)\\s*/?>|</(p|div|li|tr|h[1-6]|section|article|header|footer|blockquote|pre)\\s*>"
                body (string #\Newline)))
         (body (ppcre:regex-replace-all "(?s)<[^>]*>" body " "))
         (body (html-unescape body))
         (lines (loop for line in (ppcre:split "\\n" body)
                      for clean = (nlk:trimmed (ppcre:regex-replace-all "[ \t\r\f]+" line " "))
                      when (plusp (length clean)) collect clean)))
    (values (format nil "~{~a~^~%~}" lines) title)))

(defun native-extract (url)
  "URL read here: a plain GET, then HTML to text. => (values TEXT TITLE NOTE)."
  (multiple-value-bind (text status note) (request-text url)
    (declare (ignore status))
    (if text
        (multiple-value-bind (body title) (html-to-text text)
          (if (plusp (length body))
              (values body title nil)
              (values nil nil "native: the page had no readable text")))
        (values nil nil (format nil "native: ~a" (or note "no answer"))))))

(defun provider-extract (provider url)
  "PROVIDER's text of URL => (values TEXT TITLE NOTE)."
  (cond ((string= provider "tavily") (tavily-extract url))
        ((string= provider "firecrawl") (firecrawl-extract url))
        ((string= provider "exa") (exa-extract url))
        ((string= provider "native") (native-extract url))
        (t (values nil nil (format nil "~a is not an extraction provider" provider)))))

(defun page-of (url title text max-chars)
  "TEXT as the page the answer carries: cut to MAX-CHARS, saying how much."
  (let* ((text (nlk:trimmed (or text "")))
         (total (length text))
         (kept (if (> total max-chars) (subseq text 0 max-chars) text)))
    (list :url url
          :title (or title "")
          :chars total
          :truncated (> total max-chars)
          :text kept)))

(defun extract-page (url provider max-chars render-js)
  "URL read by PROVIDER (or the whole chain) => (values PAGE NOTES)."
  (let* ((chain (if (string= provider "auto")
                    (if render-js
                        (cons "firecrawl" (remove "firecrawl" +extract-order+ :test #'string=))
                        (copy-list +extract-order+))
                    (list provider)))
         (chain (remove-if-not (lambda (name)
                                 (or (string= name "native") (provider-configured-p name)))
                               chain))
         (notes '()))
    (when (null chain)
      (return-from extract-page
        (values nil (list (format nil "~a is not configured: set its key in the web-search-plus section, or provider auto"
                                   provider)))))
    (dolist (name chain)
      (multiple-value-bind (text title note) (provider-extract name url)
        (if (nonblank text)
            (return-from extract-page
              (values (page-of url title text max-chars)
                      (nreverse (if (string= name "native")
                                    notes
                                    (cons (format nil "read by ~a" name) notes)))))
            (push (or note (format nil "~a answered nothing" name)) notes))))
    (values nil (nreverse notes))))
