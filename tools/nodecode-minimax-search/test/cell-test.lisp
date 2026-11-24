;;;; cell-test.lisp --- lifecycle, the tool, /minimax-search, and the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: the section (a healthy
;;;; start, a member under its floor, a region outside its options or a host
;;;; without a scheme refused, a key variable with nothing in it refused), the
;;;; tool through the seam the model calls it by -- its URL, its headers, its
;;;; body and the answer it builds -- the result cap, an error the endpoint
;;;; reports in the body and an HTTP failure read back as text, the CN host,
;;;; and /minimax-search with and without a query. Every request is a stubbed
;;;; NLK:HTTP and the key a stubbed NLE::CREDENTIAL-ENV, so nothing reaches
;;;; MiniMax, the network or the machine's environment.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *minimax-requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *minimax-answer-fn* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-minimax-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *MINIMAX-REQUESTS* and
answered by *MINIMAX-ANSWER-FN*, so no test dials MiniMax. NLE::CREDENTIAL-ENV
answers the Token Plan key for MINIMAX_CODE_PLAN_KEY and nothing else, so the
machine's own environment cannot reach a test that did not ask for it."
  `(let ((*minimax-requests* '())
         (*minimax-answer-fn* (lambda (method url args)
                                 (declare (ignore method args))
                                 (error "unexpected request to ~a" url))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *minimax-requests*)
                                (funcall *minimax-answer-fn* method url args))
       (with-stubbed-fdefinition (nle::credential-env (name)
                                  (and (equal name "MINIMAX_CODE_PLAN_KEY") "sk-cp-key"))
         ,@body))))

(defun minimax-answer (text &optional (status 200))
  "The stubbed wire answers TEXT, STATUS from here on."
  (setf *minimax-answer-fn* (lambda (method url args)
                              (declare (ignore method args))
                              (values text status))))

(defun minimax-asked ()
  "The stubbed requests, oldest first."
  (reverse *minimax-requests*))

(defun minimax-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun minimax-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun minimax-results-json (&rest rows)
  "A search answer carrying ROWS of (TITLE URL SNIPPET DATE), its base_resp a success."
  (nlk:encode-json-object
   (nlk:json-object "base_resp" (nlk:json-object "status_code" 0 "status_msg" "")
                    "organic" (coerce (loop for (title url snippet date) in rows
                                             collect (nlk:json-object "title" title "link" url
                                                                      "snippet" snippet "date" date))
                                      'vector)
                    "related_searches" (coerce '("an older question" "a newer one") 'vector))))

(defun minimax-failure-json (code message)
  "A search answer whose base_resp carries the application error CODE and MESSAGE."
  (nlk:encode-json-object
   (nlk:json-object "base_resp" (nlk:json-object "status_code" code "status_msg" message)
                    "organic" #())))

(defun minimax-tool (name arguments)
  "NAME called through the wire with the JSON object ARGUMENTS, as a provider
sends it -- the contract decoded, the hooks run; => the result text."
  (assert (nle::find-tool name) () "no tool named ~a" name)
  (nle::message-content (execute-wire-call "call-1" name (nlk:encode-json-object arguments))))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "minimax-search" "MINIMAX-SEARCH-CELL-"
  :start nodecode-minimax-search:start-cell)

(define-cell-lifecycle-tests "minimax-search"
  (:fixture with-minimax-wire)
  (:help :minimax-search)
  (:command "minimax-search")
  (:running (is (nle::find-tool "minimax_search") "minimax_search is registered")
            (is (equal "https://api.minimax.io" (nodecode-minimax-search::setting :base-url)) "the global host by default")
            (is (equal "sk-cp-key" (nodecode-minimax-search::setting :key)) "the key comes from the variable the section names")
            (is (= 5 (nodecode-minimax-search::setting :max-results)) "the default result count"))
  (:stopped (is (null (nle::find-tool "minimax_search")) "stop unregisters the tool"))
  (:refused ("max_results" 0) ("timeout_seconds" 0) ("region" "mars") ("base_url" "api.minimax.io"))
  (:idle nodecode-minimax-search:minimax-search-error
         (nodecode-minimax-search:search "x")))

;;; --- search -------------------------------------------------------------------

(deftest minimax-search-cell-searches-through-minimax (with-minimax-wire)
  (minimax-answer (minimax-results-json (list "MiniMax docs" "https://example.com/docs" "How the API answers" "2026-01-02")
                                         (list "Token Plan" "https://example.com/plan" "The subscription" nil)))
  (with-cell-stop ((minimax-search-start))
    (is-carrying (answer (minimax-tool "minimax_search" (nlk:json-object "query" "minimax search")))
                 ("2 results" "both rows are listed")
                 ("MiniMax docs" "with the first title")
                 ("https://example.com/plan" "and the second url")
                 ("2026-01-02" "a row's date is carried")
                 ("related searches" "and the endpoint's suggestions are named")))
  (let ((request (first (minimax-asked))))
    (is (equal "https://api.minimax.io/v1/coding_plan/search" (second request)) "the one endpoint the original asks")
    (is (equal "Bearer sk-cp-key" (minimax-request-header request "authorization")) "the Token Plan key rides as a bearer")
    (is (equal "application/json" (minimax-request-header request "content-type")) "the body goes as JSON")
    (let ((body (minimax-request-body request)))
      (is (equal "minimax search" (nlk:json-value body :string "q")) "the query goes as q, the name the endpoint reads")
      (is (null (nlk:json-value body :any "query")) "not as query, which the original found answers 2013"))))

(deftest minimax-search-cell-a-limit-over-ten-is-clamped (with-minimax-wire)
  (minimax-answer (minimax-results-json))
  (with-cell-stop ((minimax-search-start))
    (let ((answer (minimax-tool "minimax_search" (nlk:json-object "query" "nothing" "limit" 50)))
          (direct (nodecode-minimax-search:search "nothing" :limit 50)))
      (is (search "No results" answer) "an empty answer is said, not signalled")
      (is (search "(MiniMax Token Plan search, 10 asked for)" direct) "at most ten rows, as the endpoint caps them"))))

(deftest minimax-search-cell-a-row-without-a-link-is-not-a-result (with-minimax-wire)
  (minimax-answer (minimax-results-json (list "no url here" nil "a snippet" nil)
                                        (list "a hit" "https://example.com/hit" nil nil)))
  (with-cell-stop ((minimax-search-start))
    (is-carrying (answer (minimax-tool "minimax_search" (nlk:json-object "query" "q")))
                 ("1 result for" "only the row with a link counts")
                 ("a hit" "which is the one listed"))))

;;; --- failures -----------------------------------------------------------------

(deftest minimax-search-cell-an-in-body-error-is-text-not-a-crash (with-minimax-wire)
  (minimax-answer (minimax-failure-json 2013 "invalid params"))
  (with-cell-stop ((minimax-search-start))
    (is-carrying (answer (minimax-tool "minimax_search" (nlk:json-object "query" "q")))
                 ("ERROR:" "a refusal the model reads")
                 ("2013" "naming the code the endpoint reported")
                 ("invalid params" "and its own message"))))

(deftest minimax-search-cell-an-http-failure-is-text-not-a-crash (with-minimax-wire)
  (minimax-answer "{\"base_resp\":{\"status_code\":1004,\"status_msg\":\"invalid api key\"}}" 401)
  (with-cell-stop ((minimax-search-start))
    (is-carrying (answer (minimax-tool "minimax_search" (nlk:json-object "query" "x")))
                 ("ERROR:" "a refusal the model reads")
                 ("401" "naming the status")
                 ("invalid api key" "and what MiniMax said"))))

(deftest minimax-search-cell-an-empty-query-refuses-before-any-request (with-minimax-wire)
  (with-cell-stop ((minimax-search-start))
    (let ((answer (minimax-tool "minimax_search" (nlk:json-object "query" "   "))))
      (is (search "needs a query" answer) "a blank query is refused")))
  (is (null (minimax-asked)) "and nothing was sent"))

;;; --- the section --------------------------------------------------------------

(deftest minimax-search-cell-a-key-variable-with-nothing-in-it-refuses (with-minimax-wire)
  (with-stubbed-fdefinition (nle::credential-env (name)
                             (declare (ignore name))
                             nil)
    (is-carrying (text (refusal-text nlk:config-refusal
                                     (minimax-search-start "api_key_env" "MINIMAX_TEST_KEY")))
                 ("MINIMAX_TEST_KEY" "the refusal names the variable")
                 ("platform.minimax.io" "and where a key comes from")))
  (is (null (nle::find-tool "minimax_search")) "a refused start installed nothing"))

(deftest minimax-search-cell-a-key-written-inline-wins-on-the-cn-host (with-minimax-wire)
  (minimax-answer (minimax-results-json (list "t" "https://example.com/" "s" nil)))
  (with-cell-stop ((minimax-search-start "api_key" "inline-key" "region" "cn" "max_results" 2))
    (is (equal "inline-key" (nodecode-minimax-search::setting :key)) "the section's own key")
    (is (equal "https://api.minimaxi.com" (nodecode-minimax-search::setting :base-url)) "the CN host for a CN key")
    (minimax-tool "minimax_search" (nlk:json-object "query" "x")))
  (let ((request (first (minimax-asked))))
    (is (equal "https://api.minimaxi.com/v1/coding_plan/search" (second request)) "the key is asked on the host it was minted on")
    (is (equal "Bearer inline-key" (minimax-request-header request "authorization")) "and it is the key that rides the request")))

(deftest minimax-search-cell-a-base-url-moves-every-call (with-minimax-wire)
  (minimax-answer (minimax-results-json (list "t" "https://example.com/" "s" nil)))
  (with-cell-stop ((minimax-search-start "base_url" "http://proxy.example.com:8080/" "max_results" 1))
    (minimax-tool "minimax_search" (nlk:json-object "query" "x")))
  (let ((url (second (first (minimax-asked)))))
    (is (equal "http://proxy.example.com:8080/v1/coding_plan/search" url) "a base url replaces the host")))

;;; --- /minimax-search -------------------------------------------------------------

(deftest minimax-search-cell-the-command-searches-and-explains-itself (with-minimax-wire)
  (minimax-answer (minimax-results-json (list "A hit" "https://example.com/hit" "about it" nil)))
  (with-cell-stop ((minimax-search-start))
    (is-carrying (status (cell-entry "nodecode-minimax-search" "minimax-search" ""))
                 ("https://api.minimax.io" "no argument: where it points")
                 ("5 results" "and how many a search lists"))
    (is-carrying (answer (cell-entry "nodecode-minimax-search" "minimax-search" "find me something"))
                 ("A hit" "an argument: the search's own answer")
                 ("https://example.com/hit" "with its url")))
  (is (equal "find me something"
             (nlk:json-value (minimax-request-body (first (minimax-asked))) :string "q")) "the operator's words are the query"))
