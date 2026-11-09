;;;; cell-test.lisp --- lifecycle, the two tools, /venice, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a wrong choice or an integer under its floor refused, a start
;;;; with no key refused), the two tools through the seam the model calls
;;;; them by -- their URL, their headers, their body and the answer they
;;;; build -- the order and the cap on a multi-URL extract, and /venice with
;;;; and without a query. Every request is a stubbed NLK:HTTP, so nothing
;;;; reaches Venice or the network.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-venice-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *ANSWER-FN*, so no test dials Venice."
  `(let ((*requests* '())
         (*answer-fn* (lambda (method url args)
                        (declare (ignore method url args))
                        (error "unexpected Venice request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *answer-fn* method url args))
       ,@body)))

(defun venice-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun venice-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun venice-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun venice-answer (text &optional (status 200))
  (setf *answer-fn* (lambda (method url args)
                      (declare (ignore method url args))
                      (values text status))))

(defun venice-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "venice-web" "VENICE-WEB-CELL-")

(defun venice-web-config (&rest pairs)
  "A whole shared config with one `venice' section built from PAIRS."
  (nlk:json-object "venice" (apply #'nlk:make-json-object pairs)))

(defun venice-web-start (&rest pairs)
  "START-CELL against a literal config, the way the gateway calls it."
  (nodecode-venice-web:start-cell (apply #'venice-web-config pairs)))

(define-cell-lifecycle-tests "venice-web"
  (:fixture with-venice-wire)
  (:config "api_key" "k-test")
  (:help :venice)
  (:command "venice")
  (:running (is (nle::find-tool "venice_search") "venice_search is registered")
            (is (nle::find-tool "venice_extract") "venice_extract is registered")
            (let ((root (nodecode-venice-web::setting :base-url)))
              (is (equal "https://api.venice.ai/api/v1/augment" root) "the section's default root"))
            (is (equal "k-test" (nodecode-venice-web::setting :key)) "the key the section wrote")
            (is (= 10 (nodecode-venice-web::setting :max-results)) "the default result count")
            (is (equal "brave" (nodecode-venice-web::setting :search-provider)) "Brave, zero retention"))
  (:stopped (is (null (nle::find-tool "venice_search")) "stop unregisters the tools")
            (is (null (nle::find-tool "venice_extract")) "both of them"))
  (:refused ("search_provider" "bing") ("max_results" 0) ("timeout_seconds" 0)
            ("base_url" "api.venice.ai"))
  (:idle nodecode-venice-web:venice-web-error (nodecode-venice-web::run-command "" nil)))

(deftest venice-web-cell-a-missing-key-refuses-the-start ()
  (with-venice-wire
    (with-stubbed-fdefinition (uiop:getenv (name) (declare (ignore name)) nil)
      (is-carrying (answer (refusal-text nlk:config-refusal (venice-web-start)))
                   ("no key") ("VENICE_API_KEY" "the refusal names what to set"))
      (is (null (nle::find-tool "venice_search")) "a refused start installed nothing"))))

(deftest venice-web-cell-the-key-may-come-from-the-environment ()
  (with-venice-wire
    (with-stubbed-fdefinition (uiop:getenv (name)
                               (cond ((string= name "VENICE_HOME_KEY") "k-env")
                                     ((string= name "VENICE_API_KEY") "k-fallback")
                                     (t nil)))
      (with-cell-stop ((venice-web-start "api_key_env" "VENICE_HOME_KEY"))
        (is (equal "k-env" (nodecode-venice-web::setting :key)) "api_key_env names the variable"))
      (with-cell-stop ((venice-web-start))
        (is (equal "k-fallback" (nodecode-venice-web::setting :key)) "VENICE_API_KEY is read either way")))))

;;; --- venice_search ---------------------------------------------------------------

(deftest venice-web-cell-search-answers-titles-links-and-snippets ()
  (with-venice-wire
    (with-cell-stop ((venice-web-start "api_key" "k-test"))
      (venice-answer "{\"results\":[{\"title\":\"Oslo weather\",\"url\":\"https://example.com/oslo\",\"content\":\"Rain all week.\"},{\"title\":\"\",\"url\":\"https://example.com/two\",\"snippet\":\"A second page.\"}]}")
      (let ((answer (venice-tool "venice_search" (nlk:json-object "query" "oslo weather" "limit" 5))))
        (is (search "2 results for \"oslo weather\" (via brave):" answer))
        (is (search "1. Oslo weather" answer))
        (is (search "https://example.com/oslo" answer))
        (is (search "Rain all week." answer))
        (is (search "(untitled)" answer) "a result with no title still reads")
        (is (search "A second page." answer) "a snippet stands in for content"))
      (let ((request (first (venice-asked))))
        (is (= 1 (length (venice-asked))) "one request")
        (is (eq :post (first request)))
        (is (equal "https://api.venice.ai/api/v1/augment/search" (second request)))
        (is (equal "Bearer k-test" (venice-request-header request "authorization")))
        (is (equal "application/json" (venice-request-header request "content-type")))
        (let ((body (venice-request-body request)))
          (is (equal "oslo weather" (nlk:json-value body :string "query")))
          (is (= 5 (nlk:json-value body :integer "limit")) "the call's limit")
          (is (equal "brave" (nlk:json-value body :string "search_provider")) "the section's engine"))))))

(deftest venice-web-cell-a-limit-is-clamped-and-a-provider-chosen ()
  (with-venice-wire
    (with-cell-stop ((venice-web-start "api_key" "k-test"))
      (venice-answer "{\"results\":[]}")
      (let ((answer (venice-tool "venice_search"
                                  (nlk:json-object "query" "x" "limit" 50 "provider" "google"))))
        (is (equal "No results for \"x\" (google, 20 asked for)." answer)))
      (let ((body (venice-request-body (first (venice-asked)))))
        (is (= 20 (nlk:json-value body :integer "limit")) "clamped to the endpoint's cap")
        (is (equal "google" (nlk:json-value body :string "search_provider")) "the call's provider")))))

;;; --- venice_extract ----------------------------------------------------------------

(deftest venice-web-cell-extract-fetches-each-url ()
  (with-venice-wire
    (with-cell-stop ((venice-web-start "api_key" "k-test"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore method url))
              (let* ((body (nlk:decode-json (getf args :content)))
                     (target (nlk:json-value body :string "url")))
                (values (format nil "{\"data\":{\"url\":\"~a\",\"title\":\"Page ~a\",\"content\":\"# ~a\"}}"
                                target target target)
                        200))))
      (let ((answer (venice-tool "venice_extract"
                                  (nlk:json-object "urls" (vector "https://a.test/1" "https://b.test/2")))))
        (is (= 2 (length (venice-asked))) "one scrape per url")
        (is (equal "https://api.venice.ai/api/v1/augment/scrape" (second (first (venice-asked)))))
        (is (equal "https://a.test/1" (nlk:json-value (venice-request-body (first (venice-asked)))
                                                       :string "url")))
        (let ((second-body (venice-request-body (second (venice-asked)))))
          (is (equal "https://b.test/2" (nlk:json-value second-body :string "url")) "in the order the call named them"))
        (is (search "1. https://a.test/1 -- Page https://a.test/1" answer))
        (is (search "# https://b.test/2" answer) "the page's own Markdown")))))

(deftest venice-web-cell-extract-fetches-at-most-max_urls ()
  (with-venice-wire
    (with-cell-stop ((venice-web-start "api_key" "k-test" "max_urls" 1))
      (venice-answer "{\"data\":{\"url\":\"https://a.test/1\",\"content\":\"one\"}}")
      (let ((answer (venice-tool "venice_extract"
                                  (nlk:json-object "urls" (vector "https://a.test/1" "https://b.test/2")))))
        (is (= 1 (length (venice-asked))) "one call, one scrape")
        (is (search "(2 URLs named; the first 1 fetched -- max_urls)" answer))))))

(deftest venice-web-cell-a-refused-answer-is-a-repair-not-a-crash ()
  (with-venice-wire
    (with-cell-stop ((venice-web-start "api_key" "k-test"))
      (venice-answer "{\"error\":\"Invalid API key\"}" 401)
      (is-carrying (answer (venice-tool "venice_search" (nlk:json-object "query" "x")))
                   ("ERROR: Venice answered HTTP 401") ("Invalid API key"))
      (setf *answer-fn*
            (lambda (method url args)
              (declare (ignore method url args))
              (values "{\"message\":\"boom\"}" 500)))
      (is-carrying (answer (venice-tool "venice_extract"
                                         (nlk:json-object "urls" (vector "https://a.test/1"))))
                   ("1. https://a.test/1" "the URL is still that URL's line")
                   ("failed: Venice answered HTTP 500 on /scrape: boom" "one URL's failure is its own line")))))

;;; --- /venice ---------------------------------------------------------------------

(deftest venice-web-cell-the-command-shows-the-endpoint-or-searches ()
  (with-venice-wire
    (with-cell-stop ((venice-web-start "api_key" "k-test" "search_provider" "google"))
      (let ((answer (cell-entry "nodecode-venice-web" "venice" "")))
        (is (search "Venice AI web: https://api.venice.ai/api/v1/augment, searching via google" answer))
        (is (search "at most 5 URLs an extract" answer))
        (is (null (venice-asked)) "a status read asks Venice nothing"))
      (venice-answer "{\"results\":[{\"title\":\"Oslo\",\"url\":\"https://example.com/oslo\",\"content\":\"Rain.\"}]}")
      (let ((answer (cell-entry "nodecode-venice-web" "venice" "oslo weather")))
        (is (search "1 result for \"oslo weather\" (via google):" answer))
        (is (equal "oslo weather" (nlk:json-value (venice-request-body (first (venice-asked)))
                                                   :string "query")))))))
