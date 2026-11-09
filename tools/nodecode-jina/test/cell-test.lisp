;;;; cell-test.lisp --- lifecycle, the two tools and /jina, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Jina, needs a key
;;;; or touches the network. What is proved is the cell's own wiring: the
;;;; section (a healthy start, wrong values refused), the two tools through
;;;; the seam the model calls them by -- their url, their body, their headers
;;;; and their answer -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-jina-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials Jina."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected jina request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun jina-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun jina-reply (text &optional (status 200))
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun jina-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun jina-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun jina-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "jina-call" name arguments)))

(defun jina-reader-page (url content)
  "A reader envelope for URL carrying CONTENT, as Jina writes it."
  (format nil "{\"code\": 200, \"status\": 20000, \"data\": {\"url\": \"~a\", \"title\": \"A page\", ~
\"content\": ~s}}"
          url content))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "jina" "JINA-CELL-" :start nodecode-jina:start-cell)

(define-cell-lifecycle-tests "jina"
  (:fixture with-jina-wire)
  (:config "api_key" "jina-test")
  (:help :jina)
  (:command "jina")
  (:running
   (is (nle::find-tool "jina_read") "reading is registered")
   (is (nle::find-tool "jina_search") "and search, the plugin's two operations")
   (is (= 2 (count-if (lambda (entry) (eql 0 (search "jina_" (first entry)))) nle::*tools*)) "two of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "jina_read")) "and stop takes every one back")
   (is (null (nle::find-tool "jina_search"))))
  (:refused ("timeout_seconds" 0)
            ("max_results" 0)
            ("max_chars" 10)
            ("max_pages" 0)))

;;; --- search ----------------------------------------------------------------------

(deftest jina-cell-search-with-no-key-refuses-and-sends-nothing (with-jina-wire)
  ;; Search is the one operation the plugin gated on a key; reading is not.
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (with-cell-stop ((jina-start))
      (let ((answer (jina-call "jina_search" "{\"query\": \"clean markdown\"}")))
        (is (search "ERROR: " answer) "a refusal is the tool result, not a condition up the stack")
        (is (search "JINA_API_KEY" answer) "naming the key it needs")
        (is (search "api-dashboard" answer) "and where a key is made")
        (is (null (jina-asked)) "and nothing was sent")))))

(deftest jina-cell-search-gets-the-query-and-reads-the-sources (with-jina-wire)
  (with-cell-stop ((jina-start "api_key" "jina-test"))
    (jina-reply
     (concatenate 'string
                  "{\"code\": 200, \"status\": 20000, \"data\": ["
                  "{\"title\": \"A page\", \"url\": \"https://a.test/1\", \"description\": \"snippet a\"}]}"))
    (let ((answer (jina-call "jina_search" "{\"query\": \"clean markdown\", \"num\": 3}")))
      (let* ((request (first (jina-asked)))
             (url (second request)))
        (is (eq :get (first request)) "one GET")
        (is (eql 0 (search "https://s.jina.ai/?q=" url)) "to Jina's search endpoint, with the query")
        (is (search "q=clean" url) "the query as typed")
        (is (search "num=3" url) "and the count the call named")
        (is (equal "Bearer jina-test" (jina-request-header request "authorization")) "with the section's key")
        (is (equal "no-content" (jina-request-header request "x-respond-with")) "asking for snippets only, as the plugin did"))
      (is (search "1. A page" answer) "the numbered source")
      (is (search "https://a.test/1" answer) "with its url")
      (is (search "snippet a" answer) "and its snippet"))))

(deftest jina-cell-a-search-names-the-section-s-count-when-the-call-names-none (with-jina-wire)
  (with-cell-stop ((jina-start "api_key" "jina-test" "max_results" 7))
    (jina-reply "{\"code\": 200, \"data\": []}")
    (let ((answer (jina-call "jina_search" "{\"query\": \"x\"}")))
      (is (search "num=7" (second (first (jina-asked)))) "the section's max_results rides the url")
      (is (search "no results for \"x\"" answer) "and no source is said so, not left blank"))))

(deftest jina-cell-a-refused-key-is-the-model-s-repair (with-jina-wire)
  (with-cell-stop ((jina-start "api_key" "jina-test"))
    (jina-reply "{\"readableMessage\": \"Authentication failed\", \"code\": 401}" 401)
    (let ((answer (jina-call "jina_search" "{\"query\": \"x\"}")))
      (is (search "ERROR: " answer) "the failure is the tool result")
      (is (search "HTTP 401" answer) "naming the status")
      (is (search "jina section" answer) "and telling the model what to check")
      (is (search "Authentication failed" answer) "with Jina's own message"))))

;;; --- reading -----------------------------------------------------------------------

(deftest jina-cell-read-posts-each-url-and-answers-its-markdown (with-jina-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (with-cell-stop ((jina-start))
      (jina-reply
       (format nil "{\"code\": 200, \"status\": 20000, \"data\": {\"url\": \"https://a.test/final\", ~
\"title\": \"A page\", \"content\": \"# Heading\\ntext\", \"warning\": \"cached copy\"}}"))
      (let ((answer (jina-call "jina_read" "{\"urls\": [\"https://a.test/1\"]}")))
        (let ((request (first (jina-asked))))
          (is (eq :post (first request)) "one POST")
          (is (equal "https://r.jina.ai/" (second request)) "to Jina Reader")
          (is (equal "https://a.test/1" (nlk:json-value (jina-request-body request) :text "url")) "carrying the url the call named")
          (is (equal "application/json" (jina-request-header request "accept")) "asking for JSON")
          (is (equal "none" (jina-request-header request "x-retain-images")) "images dropped, as the plugin dropped them")
          (is (null (jina-request-header request "authorization")) "and no key sent: reading works without one"))
        (is (search "url: https://a.test/final" answer) "the final url, not the one asked for")
        (is (search "title: A page" answer) "the title")
        (is (search "# Heading" answer) "and the page's Markdown")
        (is (search "warning: cached copy" answer) "with Jina's own warning when it served a cache")))))

(deftest jina-cell-a-key-on-the-section-is-the-one-sent (with-jina-wire)
  (with-cell-stop ((jina-start "api_key" "jina-test"))
    (jina-reply (jina-reader-page "https://a.test/1" "page one"))
    (jina-call "jina_read" "{\"urls\": [\"https://a.test/1\"]}")
    (is (equal "Bearer jina-test" (jina-request-header (first (jina-asked)) "authorization")) "the section's key rides the read too")))

(deftest jina-cell-one-broken-page-fails-alone (with-jina-wire)
  (with-cell-stop ((jina-start))
    (setf *reply*
          (lambda (method url args)
            (declare (ignore method url))
            (if (search "b.test" (or (getf args :content) ""))
                (values "{\"readableMessage\": \"The page could not be loaded\", \"code\": 422}" 422)
                (values (jina-reader-page "https://a.test/1" "page one") 200))))
    (let ((answer (jina-call "jina_read"
                              "{\"urls\": [\"https://a.test/1\", \"https://b.test/2\", \"ftp://files.test/x\"]}")))
      (is (search "page one" answer) "the page that read is there")
      (is (search "failed:" answer) "a page that did not is named under it")
      (is (search "https://b.test/2 - Jina Reader: HTTP 422" answer) "with its status")
      (is (search "The page could not be loaded" answer) "and Jina's own message")
      (is (search "ftp://files.test/x - only http" answer) "a url that is not http or https is refused without a request")
      (is (= 2 (length (jina-asked))) "so only the two web urls were asked for"))))

(deftest jina-cell-a-page-is-cut-and-a-call-is-bounded (with-jina-wire)
  (with-cell-stop ((jina-start "max_chars" 200 "max_pages" 1))
    (jina-reply (jina-reader-page "https://a.test/1" (make-string 260 :initial-element #\x)))
    (let ((answer (jina-call "jina_read"
                              "{\"urls\": [\"https://a.test/1\", \"https://a.test/2\"]}")))
      (is (= 1 (length (jina-asked))) "one page read, as max_pages says")
      (is (search "60 more characters" answer) "and it is cut to the section's max_chars")
      (is (search "1 more url not read" answer) "the rest is named, not silently dropped"))))

(deftest jina-cell-an-empty-url-list-refuses-before-a-request (with-jina-wire)
  (with-cell-stop ((jina-start))
    (let ((answer (jina-call "jina_read" "{}")))
      (is (search "urls" answer) "the refusal names the member the call left out")
      (is (null (jina-asked)) "and nothing was sent"))))

;;; --- the operator's command ----------------------------------------------------------

(deftest jina-cell-the-command-reports-reads-and-searches (with-jina-wire)
  (with-cell-stop ((jina-start "api_key" "jina-test"))
    (let ((status (nodecode-jina::run-command "" nil)))
      (is (search "https://r.jina.ai/" status) "no argument reports the reader")
      (is (search "key set" status) "and that a key is there")
      (is (null (search "jina-test" status)) "without ever printing the key")
      (is (null (jina-asked)) "and sends nothing"))
    (jina-reply (jina-reader-page "https://a.test/1" "page one"))
    (let ((answer (nodecode-jina::run-command "  https://a.test/1  " nil)))
      (is (search "page one" answer) "a url argument reads that page")
      (is (eq :post (first (first (jina-asked)))) "through the reader"))
    (jina-reply "{\"code\": 200, \"data\": [{\"title\": \"A page\", \"url\": \"https://a.test/1\"}]}")
    (let ((answer (nodecode-jina::run-command "clean markdown" nil)))
      (is (search "1. A page" answer) "anything else is a search")
      (is (search "q=clean" (second (car *requests*))) "with the words on the url"))))
