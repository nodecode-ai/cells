;;;; cell-test.lisp --- lifecycle, the two tools, /ollama-search, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a member under its floor or a root without a scheme refused, a key
;;;; variable with nothing in it refused), the two tools through the seam the
;;;; model calls them by -- their URL, their headers, their body and the
;;;; answer they build -- the result cap, an HTTP failure read back as text,
;;;; and /ollama-search with and without a query. Every request is a stubbed
;;;; NLK:HTTP, and the key a stubbed NLE::CREDENTIAL-ENV, so nothing reaches
;;;; Ollama, the network or the machine's environment.

(in-package #:nodecode.test)


;;; --- fixtures -----------------------------------------------------------------

(defvar *ollama-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *ollama-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-ollama-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *OLLAMA-REQUESTS* and
answered by *OLLAMA-ANSWER-FN*, so no test dials Ollama. NLE::CREDENTIAL-ENV
answers a test key for OLLAMA_API_KEY and nothing else, so the machine's own
environment cannot reach a test that did not ask for it."
  `(let ((*ollama-requests* '())
         (*ollama-answer-fn* (lambda (method url args)
                                (declare (ignore method args))
                                (error "unexpected request to ~a" url))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *ollama-requests*)
                                (funcall *ollama-answer-fn* method url args))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "OLLAMA_API_KEY") "o-key"))
         ,@body))))

(defun ollama-answer (text &optional (status 200))
  "The stubbed wire answers TEXT, STATUS from here on."
  (setf *ollama-answer-fn* (lambda (method url args)
                              (declare (ignore method url args))
                              (values text status))))

(defun ollama-asked ()
  "The stubbed requests, oldest first."
  (reverse *ollama-requests*))

(defun ollama-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun ollama-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun ollama-results-json (&rest rows)
  "An /api/web_search answer carrying ROWS of (TITLE URL CONTENT)."
  (nlk:encode-json-object
   (nlk:json-object "results"
                     (coerce (loop for (title url content) in rows
                                   collect (nlk:json-object "title" title
                                                             "url" url
                                                             "content" content))
                             'vector))))

(defun ollama-page-json (title content &optional links)
  "An /api/web_fetch answer."
  (nlk:encode-json-object
   (nlk:json-object "title" title "content" content
                     "links" (coerce (or links '()) 'vector))))

(defun ollama-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "ollama-search" "OLLAMA-SEARCH-CELL-"
  :start nodecode-ollama-search:start-cell)

(define-cell-lifecycle-tests "ollama-search"
  (:fixture with-ollama-wire)
  (:help :ollama-search)
  (:command "ollama-search")
  (:running (is (nle::find-tool "ollama_search") "ollama_search is registered")
            (is (nle::find-tool "ollama_fetch") "ollama_fetch is registered")
            (is (equal "https://ollama.com" (nodecode-ollama-search::setting :base-url)) "the section's default host")
            (is (equal "o-key" (nodecode-ollama-search::setting :key)) "the key comes from the variable the section names")
            (is (= 5 (nodecode-ollama-search::setting :max-results)) "the default result count"))
  (:stopped (is (null (nle::find-tool "ollama_search")) "stop unregisters the tools")
            (is (null (nle::find-tool "ollama_fetch")) "both of them"))
  (:refused ("max_results" 0) ("timeout_seconds" 0) ("fetch_timeout_seconds" 0)
            ("base_url" "ollama.com"))
  (:idle nodecode-ollama-search:ollama-search-error
         (nodecode-ollama-search:search "x")
         (nodecode-ollama-search:fetch "https://example.com/")))

;;; --- search -------------------------------------------------------------------

(deftest ollama-search-cell-searches-through-ollama (with-ollama-wire)
  (ollama-answer (ollama-results-json (list "Ollama docs" "https://example.com/docs" "How Ollama serves models")
                                       (list "Ollama Cloud" "https://example.com/cloud" "A hosted API")))
  (with-cell-stop ((ollama-search-start))
    (is-carrying (answer (ollama-tool "ollama_search" (nlk:json-object "query" "ollama cloud")))
                 ("2 results" "both rows are listed")
                 ("Ollama docs" "with the first title")
                 ("https://example.com/cloud" "and the second url")))
  (let ((request (first (ollama-asked))))
    (is (equal "https://ollama.com/api/web_search" (second request)) "the one endpoint the original asks")
    (is (equal "Bearer o-key" (ollama-request-header request "authorization")) "the key rides as a bearer")
    (let ((body (ollama-request-body request)))
      (is (equal "ollama cloud" (nlk:json-value body :string "query")) "the query goes as the original sends it")
      (is (= 5 (nlk:json-value body :integer "max_results")) "and the section's count"))))

(deftest ollama-search-cell-a-limit-over-ten-is-clamped (with-ollama-wire)
  (ollama-answer (ollama-results-json))
  (with-cell-stop ((ollama-search-start))
    (is (search "No results" (ollama-tool "ollama_search" (nlk:json-object "query" "nothing" "limit" 50))) "an empty answer is said, not signalled"))
  (is (= 10 (nlk:json-value (ollama-request-body (first (ollama-asked))) :integer "max_results")) "at most ten are asked for, as the endpoint caps them"))

;;; --- fetch --------------------------------------------------------------------

(deftest ollama-search-cell-fetches-a-page (with-ollama-wire)
  (ollama-answer (ollama-page-json "A page" "The page's own words." '("https://example.com/more")))
  (with-cell-stop ((ollama-search-start))
    (is-carrying (answer (ollama-tool "ollama_fetch" (nlk:json-object "url" "https://example.com/a")))
                 ("https://example.com/a" "the url is named")
                 ("A page" "the title Ollama extracted")
                 ("The page's own words." "and the text itself")))
  (let ((request (first (ollama-asked))))
    (is (equal "https://ollama.com/api/web_fetch" (second request)) "the fetch endpoint")
    (is (equal "https://example.com/a"
               (nlk:json-value (ollama-request-body request) :string "url")) "one url per request, as the original sends them")))

(deftest ollama-search-cell-an-http-failure-is-text-not-a-crash (with-ollama-wire)
  (ollama-answer "{\"error\":\"invalid api key\"}" 401)
  (with-cell-stop ((ollama-search-start))
    (is-carrying (answer (ollama-tool "ollama_search" (nlk:json-object "query" "x")))
                 ("ERROR:" "a refusal the model reads")
                 ("401" "naming the status")
                 ("invalid api key" "and what Ollama said"))))

(deftest ollama-search-cell-a-url-without-a-scheme-refuses (with-ollama-wire)
  (ollama-answer (ollama-page-json "x" "y"))
  (with-cell-stop ((ollama-search-start))
    (is (search "not an http or https url"
                (ollama-tool "ollama_fetch" (nlk:json-object "url" "example.com/a"))) "a bare host is refused before any request"))
  (is (null (ollama-asked)) "and nothing was sent"))

;;; --- the section --------------------------------------------------------------

(deftest ollama-search-cell-a-key-variable-with-nothing-in-it-refuses (with-ollama-wire)
  (with-stubbed-fdefinition (nle::credential-env (name)
                             (declare (ignore name))
                             nil)
    (is-carrying (text (refusal-text nlk:config-refusal
                                     (ollama-search-start "api_key_env" "OLLAMA_TEST_KEY")))
                 ("OLLAMA_TEST_KEY" "the refusal names the variable")
                 ("https://ollama.com/settings/keys" "and where a key comes from")))
  (is (null (nle::find-tool "ollama_search")) "a refused start installed nothing"))

(deftest ollama-search-cell-a-key-written-inline-wins (with-ollama-wire)
  (ollama-answer (ollama-results-json (list "t" "https://example.com/" "s")))
  (with-cell-stop ((ollama-search-start "api_key" "inline-key"))
    (is (equal "inline-key" (nodecode-ollama-search::setting :key)) "the section's own key")
    (ollama-tool "ollama_search" (nlk:json-object "query" "x")))
  (is (equal "Bearer inline-key" (ollama-request-header (first (ollama-asked)) "authorization")) "it is the key that rides the request"))

;;; --- /ollama-search -------------------------------------------------------------

(deftest ollama-search-cell-the-command-searches-and-explains-itself (with-ollama-wire)
  (ollama-answer (ollama-results-json (list "A hit" "https://example.com/hit" "about it")))
  (with-cell-stop ((ollama-search-start))
    (is-carrying (status (cell-entry "nodecode-ollama-search" "ollama-search" ""))
                 ("https://ollama.com" "no argument: where it points")
                 ("5 results" "and how many a search asks for"))
    (is-carrying (answer (cell-entry "nodecode-ollama-search" "ollama-search" "find me something"))
                 ("A hit" "an argument: the search's own answer")
                 ("https://example.com/hit" "with its url")))
  (is (equal "find me something"
             (nlk:json-value (ollama-request-body (first (ollama-asked))) :string "query")) "the operator's words are the query"))
