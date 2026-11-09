;;;; cell-test.lisp --- lifecycle, the fan-out, the line rules, and /federated-search.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches a search API, no
;;;; key is needed and the run touches no network. What is proved is the cell's
;;;; own wiring: the section (a healthy start, and a section with no line
;;;; refused), the fan-out across two lines -- their urls, their headers, their
;;;; bodies -- and the merge, the dedupe, the keyword order, the required and
;;;; min_backends rules, the three response shapes, and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *routes* '()
  "What the stubbed NLK:HTTP answers, as (URL-MATCH . REPLY): REPLY is (TEXT STATUS), or a
function of the request answering (values TEXT STATUS), or :boom to fail below HTTP.")

(defmacro with-federated-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered by
*ROUTES*, so no test dials a search API."
  `(let ((*requests* '()) (*routes* '()))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (federated-reply method url args))
       ,@body)))

(defun federated-reply (method url args)
  "The answer *ROUTES* holds for URL, else a failure: a test that forgot its route says so."
  (let ((route (find-if (lambda (route) (search (car route) url)) *routes*)))
    (unless route (error "unexpected federated search request to ~a" url))
    (let ((reply (cdr route)))
      (cond ((eq reply :boom) (error "connection refused"))
            ((functionp reply) (funcall reply method url args))
            (t (values (first reply) (second reply)))))))

(defun route (match text &optional (status 200))
  (push (cons match (list text status)) *routes*))

(defun asked (match)
  "The stubbed requests whose url carries MATCH, oldest first."
  (reverse (remove-if-not (lambda (request) (search match (second request))) *requests*)))

(defun asked-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun asked-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun federated-call (arguments)
  "federated_search called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "federated-call" "federated_search" arguments)))

;;; --- the lines a test configures ---------------------------------------------

(defun line (name &rest pairs)
  "One backend object, as the section's backends array holds it."
  (apply #'nlk:make-json-object "name" name pairs))

(defun notes-line ()
  "A line of the test's own: an endpoint with no key, asked with GET."
  (line "notes" "base_url" "https://notes.test" "search_path" "/search"
        "query_param" "q" "auth_style" "" "method" "GET" "count_param" ""))

(defun two-lines ()
  "Two lines of the test's own: a keyed one over the bearer header, a keyless GET one."
  (vector (line "tavily") (notes-line)))

(defun count-of (needle haystack)
  "How many times NEEDLE appears in HAYSTACK."
  (let ((at 0) (found 0))
    (loop for hit = (search needle haystack :start2 at)
          while hit
          do (setf at (+ hit (length needle))) (incf found))
    found))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "federated-search" "FEDERATED-SEARCH-CELL-"
  :start nodecode-federated-search:start-cell)

(define-cell-lifecycle-tests "federated-search"
  (:fixture with-federated-wire)
  (:config "backends" (two-lines) "min_backends" 1)
  (:help :federated-search)
  (:command "federated-search")
  (:running
   (is (nle::find-tool "federated_search") "the model's verb is registered")
   (is (= 2 (length (nodecode-federated-search::setting :lines))) "the section's two lines are read"))
  (:stopped
   (is (null (nle::find-tool "federated_search")) "and stop takes it back"))
  (:refused ("backends" (vector))
            ("timeout" 0)
            ("max_results" 0)
            ("max_chars" 10)
            ("pool_strategy" "sometimes")
            ("backends" (two-lines) "min_backends" 3)
            ("backends" (vector (line "" "base_url" "https://x.test")))))

(deftest federated-search-cell-a-section-with-no-line-refuses (with-federated-wire)
  (let ((text (refusal-text nlk:config-refusal (federated-search-start "enabled" t))))
    (is-carrying text "configures no search line" "backends")
    (is (null (nle::find-tool "federated_search")) "nothing was installed")))

(deftest federated-search-cell-a-key-is-a-name-not-a-value (with-federated-wire)
  (let ((text (refusal-text nlk:config-refusal
                            (federated-search-start "backends" (vector (line "x" "base_url" "https://x.test"
                                                                           "credentials" (vector "sk-live-123")))))))
    (is-carrying text "environment variable name" "names only")
    (is (null (nle::find-tool "federated_search")) "nothing was installed")))

;;; --- the fan-out ----------------------------------------------------------------

(defmacro with-test-key (&body body)
  "BODY with the environment holding the test's own keys and nothing else."
  `(with-stubbed-fdefinition (uiop:getenv (name)
                               (and (member name '("TAVILY_API_KEY" "KEY_ONE" "KEY_TWO")
                                            :test #'equal)
                                    "test-key"))
     ,@body))

(deftest federated-search-cell-two-lines-merge-dedupe-and-rank (with-federated-wire)
  (with-test-key
    (with-cell-stop ((federated-search-start "backends" (two-lines) "min_backends" 1))
      (route "https://api.tavily.com/search"
             "{\"results\": [{\"title\": \"Lisp page\", \"url\": \"https://a.test/1\", \"content\": \"lisp and page\"}, {\"title\": \"Shared\", \"url\": \"https://dup.test/1\", \"content\": \"nothing\"}]}")
      (route "https://notes.test/search"
             "{\"organic\": [{\"title\": \"Notes\", \"link\": \"https://b.test/2\", \"snippet\": \"lisp\"}, {\"title\": \"Shared again\", \"link\": \"https://dup.test/1\", \"snippet\": \"dup\"}]}")
      (let ((answer (federated-call "{\"query\": \"lisp page\"}"))
            (post (first (asked "api.tavily.com")))
            (get (first (asked "notes.test"))))
        (is (eq :post (first post)) "the keyed line is asked with a POST")
        (is (equal "lisp page" (nlk:json-value (asked-body post) :text "query")) "carrying the query as it came")
        (is (= 8 (nlk:json-value (asked-body post) :integer "max_results")) "and the section's max_results")
        (is (equal "Bearer test-key" (asked-header post "authorization")) "with the key the environment holds")
        (is (eq :get (first get)) "the keyless line is asked with a GET")
        (is (search "q=lisp" (second get)) "with the query under the field its line names")
        (is (null (asked-header get "authorization")) "and no key header at all")
        (is (search "2 of 2 lines answered" answer) "both lines answered")
        (is (search "https://a.test/1" answer) "the keyed line's result is there")
        (is (search "https://b.test/2" answer) "and the keyless line's")
        (is (= 1 (count-of "https://dup.test/1" answer)) "a url both lines found is listed once")
        (is (< (search "https://a.test/1" answer) (search "https://b.test/2" answer)) "the result carrying both query terms ranks first")))))

(deftest federated-search-cell-a-refused-line-is-a-note (with-federated-wire)
  (with-test-key
    (with-cell-stop ((federated-search-start "backends" (two-lines)))
      (route "https://api.tavily.com/search" "{\"detail\": \"bad key\"}" 401)
      (route "https://notes.test/search"
             "{\"results\": [{\"title\": \"Notes\", \"url\": \"https://b.test/2\", \"content\": \"s\"}]}")
      (let ((answer (federated-call "{\"query\": \"x\"}")))
        (is (search "line \"tavily\": HTTP 401" answer) "the failing line is named in a note")
        (is (search "bad key" answer) "with what the service said")
        (is (search "https://b.test/2" answer) "and the other line's result still comes back")))))

(deftest federated-search-cell-a-line-that-cannot-be-reached-is-a-note (with-federated-wire)
  (with-test-key
    (with-cell-stop ((federated-search-start "backends" (two-lines)))
      (push (cons "https://api.tavily.com/search" :boom) *routes*)
      (route "https://notes.test/search"
             "{\"results\": [{\"title\": \"Notes\", \"url\": \"https://b.test/2\", \"content\": \"s\"}]}")
      (let ((answer (federated-call "{\"query\": \"x\"}")))
        (is (search "line \"tavily\":" answer) "the line that failed below HTTP is named")
        (is (search "https://b.test/2" answer) "and the answer is still made of what answered")))))

(deftest federated-search-cell-min-backends-refuses-a-thin-answer (with-federated-wire)
  (with-test-key
    (with-cell-stop ((federated-search-start "backends" (two-lines) "min_backends" 2))
      (route "https://api.tavily.com/search" "{}" 500)
      (route "https://notes.test/search"
             "{\"results\": [{\"title\": \"Notes\", \"url\": \"https://b.test/2\", \"content\": \"s\"}]}")
      (let ((answer (federated-call "{\"query\": \"x\"}")))
        (is (search "ERROR: " answer) "a thin answer is a failure the model repairs from")
        (is (search "min_backends is 2" answer) "naming the rule that was not met")
        (is (search "HTTP 500" answer) "and the line that failed")))))

(deftest federated-search-cell-a-required-line-must-answer (with-federated-wire)
  (with-test-key
    (with-cell-stop ((federated-search-start "backends"
                                      (vector (line "tavily" "required" t) (notes-line))
                                      "min_backends" 1))
      (route "https://api.tavily.com/search" "{}" 503)
      (route "https://notes.test/search"
             "{\"results\": [{\"title\": \"Notes\", \"url\": \"https://b.test/2\", \"content\": \"s\"}]}")
      (let ((answer (federated-call "{\"query\": \"x\"}")))
        (is (search "ERROR: required line" answer) "a required line that failed fails the search")
        (is (search "tavily" answer) "naming the line the section marked required")))))

(deftest federated-search-cell-every-answer-shape-is-read (with-federated-wire)
  (with-test-key
    (with-cell-stop ((federated-search-start
                      "backends"
                      (vector (line "organic" "base_url" "https://organic.test"
                                    "api_key_env" "KEY_ONE")
                              (line "web" "base_url" "https://web.test" "api_key_env" "KEY_TWO"
                                    "auth_style" "x-api-key")
                              (line "plain" "base_url" "https://plain.test" "api_key_env" "KEY_ONE"))))
      (route "https://organic.test"
             "{\"organic\": [{\"title\": \"Organic hit\", \"link\": \"https://o.test/1\", \"snippet\": \"one\"}]}")
      (route "https://web.test"
             "{\"data\": {\"web\": [{\"title\": \"Web hit\", \"url\": \"https://w.test/2\", \"description\": \"two\"}]}}")
      (route "https://plain.test"
             "{\"results\": [{\"title\": \"Plain hit\", \"url\": \"https://p.test/3\", \"content\": \"three\"}]}")
      (let ((answer (federated-call "{\"query\": \"hit\"}")))
        (is (search "Organic hit" answer) "the organic shape is read")
        (is (search "https://o.test/1" answer) "with its link field as the url")
        (is (search "Web hit" answer) "the nested web shape is read")
        (is (search "Plain hit" answer) "and the plain results shape")
        (is (equal "test-key" (asked-header (first (asked "web.test")) "x-api-key")) "a line asking for x-api-key gets it there")
        (is (null (asked-header (first (asked "web.test")) "authorization")) "and nowhere else")))))

(deftest federated-search-cell-a-line-with-no-key-is-not-dialled (with-federated-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (with-cell-stop ((federated-search-start "backends"
                                      (vector (line "keyed" "base_url" "https://keyed.test"
                                                    "api_key_env" "MISSING_KEY"))))
      (let ((answer (federated-call "{\"query\": \"x\"}")))
        (is (null (asked "keyed.test")) "the line was never dialled")
        (is (search "no key set" answer) "and the answer says why")
        (is (search "MISSING_KEY" answer) "naming the variable to set")))))

(deftest federated-search-cell-the-limit-caps-the-answer (with-federated-wire)
  (with-test-key
    (with-cell-stop ((federated-search-start "backends" (vector (notes-line)) "max_results" 2))
      (route "https://notes.test/search"
             "{\"results\": [{\"title\": \"One\", \"url\": \"https://n.test/1\", \"content\": \"a\"}, {\"title\": \"Two\", \"url\": \"https://n.test/2\", \"content\": \"b\"}, {\"title\": \"Three\", \"url\": \"https://n.test/3\", \"content\": \"c\"}]}")
      (let ((answer (federated-call "{\"query\": \"x\"}")))
        (is (search "1. One" answer) "the first result is listed")
        (is (search "2. Two" answer) "and the second")
        (is (null (search "Three" answer)) "and the section's max_results caps the rest")))))

(deftest federated-search-cell-a-card-that-just-failed-rests (with-federated-wire)
  (with-test-key
    (with-cell-stop ((federated-search-start "backends" (vector (line "tavily"))))
      (route "https://api.tavily.com/search" "{}" 500)
      (federated-call "{\"query\": \"x\"}")
      (let ((first-count (length (asked "api.tavily.com"))))
        (federated-call "{\"query\": \"y\"}")
        (is (= first-count (length (asked "api.tavily.com"))) "a key that just failed rests, and is not spent again at once")))))

;;; --- the operator's command --------------------------------------------------------

(deftest federated-search-cell-the-command-reports-or-searches (with-federated-wire)
  (with-test-key
    (with-cell-stop ((federated-search-start "backends" (two-lines)))
      (let ((status (nodecode-federated-search::run-command "" nil)))
        (is (search "federated-search: 2 lines" status) "no argument reports what is configured")
        (is (search "https://notes.test" status) "naming every line's endpoint")
        (is (search "TAVILY_API_KEY set" status) "and whether each key is there")
        (is (null (search "test-key" status)) "without ever printing a key")
        (is (null (asked "")) "and sends nothing"))
      (route "https://notes.test/search"
             "{\"results\": [{\"title\": \"Notes\", \"url\": \"https://b.test/2\", \"content\": \"s\"}]}")
      (route "https://api.tavily.com/search" "{\"results\": []}")
      (let ((answer (nodecode-federated-search::run-command "  lisp  " nil)))
        (is (search "https://b.test/2" answer) "an argument runs one search and answers its results")
        (is (equal "lisp" (nlk:json-value (asked-body (first (asked "api.tavily.com"))) :text "query")) "the query trimmed, as typed")))))
