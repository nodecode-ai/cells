;;;; tools.lisp --- jina_read and jina_search, their arguments and their answers.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The plugin registered no tool of its own: it was a web backend, so the
;;;; harness' own web_extract and web_search reached Jina through it. The two
;;;; operations are the plugin's, and here they are the two tools the model
;;;; calls. Each is one NLK:DEFINE-CONTRACT, so the shape the model is shown
;;;; and the shape the body reads are one declaration.
;;;;
;;;; jina_read answers pages one by one: as the plugin answered a batch, one
;;;; broken url is its own error and the rest still arrive.

(in-package #:nodecode-jina)

;;; --- jina_read -----------------------------------------------------------------

(nlk:define-contract read-call
  (:doc "Read web pages as clean Markdown, through Jina Reader. Give absolute http or https urls;
answers each page's final url, title and text. Reading works without a key.")
  ("urls" :array :of :string :required t
   :doc "the urls to read, absolute http or https; one call reads the section's max_pages of them"))

(defun reader-data (url)
  "URL read by r.jina.ai => its envelope's `data' object. Keyless is allowed,
as the plugin let the anonymous Reader answer without one."
  (let ((key (api-key)))
    (multiple-value-bind (answer status)
        (ask :post (endpoint (setting :reader-url) +reader-url+)
             :content (nlk:encode-json-object (nlk:make-json-object "url" url))
             :headers (headers :key key
                               :extra (list (cons "content-type" "application/json")
                                            (cons "x-retain-images" "none"))))
      (data-of answer status "Jina Reader" :keyed (and key t)))))

(defun read-one (url)
  "URL read => (values TEXT NIL) or (values NIL ERROR-TEXT)."
  (if (not (web-url-p url))
      (values nil "only http:// and https:// urls can be read")
      (handler-case
          (page-text url (reader-data url) (setting :max-chars))
        (jina-error (condition) (values nil (jina-error-detail condition)))
        (error (condition) (values nil (nle:transport-failure-label condition url))))))

(defun tool-read (call)
  "jina_read: one request per url, each answered on its own, up to max_pages."
  (let* ((urls (remove nil (coerce (read-call-urls call) 'list)))
         (pages (max 1 (setting :max-pages)))
         (kept (subseq urls 0 (min pages (length urls))))
         (skipped (- (length urls) (length kept)))
         (failures '()))
    (unless kept
      (fail "jina_read: urls is empty; name at least one absolute http or https url"))
    (with-output-to-string (stream)
      (dolist (url kept)
        (multiple-value-bind (text error) (read-one url)
          (if text
              (format stream "~a~%" text)
              (push (cons url error) failures))))
      (when (plusp skipped)
        (format stream "[~d more url~:p not read: one call reads ~d page~:p (max_pages);~%~
                        ask again for the rest]~%"
                skipped pages))
      (when failures
        (format stream "failed:~%")
        (dolist (failure (reverse failures))
          (format stream "  ~a - ~a~%" (car failure) (cdr failure)))))))

;;; --- jina_search ---------------------------------------------------------------

(nlk:define-contract search-call
  (:doc "Search the web through Jina Search. Answers numbered sources - title, url, snippet. Needs
JINA_API_KEY; jina_read does not.")
  ("query" :text :required t :doc "the search query")
  ("num" :integer
   :doc "how many sources, 1 to 20; the section's max_results when the call names none"))

(defun search-data (query limit)
  "QUERY searched by s.jina.ai, at most LIMIT sources => its envelope's `data'.
A key is required here, as the plugin required one for search and for nothing
else."
  (let ((key (api-key)))
    (unless key
      (fail "jina_search needs JINA_API_KEY: make one at ~a and set it as api_key in the jina ~
             section, or export it. jina_read works without a key." +key-url+))
    (multiple-value-bind (answer status)
        (ask :get (format nil "~a?q=~a&num=~d"
                          (endpoint (setting :search-url) +search-url+)
                          (quri:url-encode query :encoding :utf-8)
                          limit)
             :headers (headers :key key
                               :extra (list (cons "x-respond-with" "no-content"))))
      (data-of answer status "Jina Search" :keyed t))))

(defun tool-search (call)
  "jina_search: the sources for one query, snippets only."
  (let* ((query (search-call-query call))
         (limit (max 1 (min (or (search-call-num call) (setting :max-results))
                            +search-limit-max+))))
    (multiple-value-bind (text error) (results-text query (search-data query limit))
      (or text (fail "jina_search ~s: ~a" query error)))))
