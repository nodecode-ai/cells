;;;; cell-test.lisp --- lifecycle, the two tools, the two verbs, and what the vendor fixes.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring: a healthy start that
;;;; registers the help topic and both tools, a stop that takes them back, a
;;;; refused section and an idle verb, a start with no key anywhere, and
;;;; then each endpoint through the one seam the cell calls (NLK:HTTP) --
;;;; the url, the bearer header, the body the vendor asks for, and the
;;;; answer the model reads. Every request is stubbed and the retry wait is
;;;; stubbed too, so nothing reaches DeepSeek and no run sleeps.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *wire* nil
  "The answers the stubbed NLK:HTTP still owes, oldest first, as (TEXT STATUS HEADERS).")

(defvar *asked* nil
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *waits* nil
  "Every wait a retry asked for, newest first.")

(defun wire-answer (text &optional (status 200) (headers '()))
  "Queue one answer for the next stubbed request."
  (setf *wire* (append *wire* (list (list text status headers)))))

(defmacro with-deepseek-wire (&body body)
  "BODY with NLK:HTTP stubbed (answering from *WIRE*), DEEPSEEK_API_KEY absent
from the environment, and no retry actually sleeping."
  `(let ((*wire* '()) (*asked* '()) (*waits* '())
         (nodecode-deepseek-search::*sleep*
           (lambda (seconds) (push seconds *waits*))))
     (with-stubbed-fdefinition (uiop:getenv (name) nil)
       (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                  (push (list* method url args) *asked*)
                                  (let ((next (pop *wire*)))
                                    (unless next
                                      (error "unexpected request to ~a" url))
                                    (destructuring-bind (text status headers) next
                                      (values text status headers))))
         ,@body))))

(defun asked-first ()
  "The test's first stubbed request, or NIL."
  (first (last *asked*)))

(defun asked-count ()
  "How many requests the test made."
  (length *asked*))

(defun asked-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun asked-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun deepseek-tool (name arguments)
  "Run the registered tool NAME with the JSON object ARGUMENTS, as dispatch does."
  (let ((entry (nle::find-tool name)))
    (assert entry () "no tool named ~a" name)
    (funcall (third entry) arguments)))

;;; --- lifecycle ----------------------------------------------------------------

(define-test-slice "deepseek-search" "DEEPSEEK-SEARCH-CELL-"
  :start nodecode-deepseek-search:start-cell)

(define-cell-lifecycle-tests "deepseek-search"
  (:fixture with-deepseek-wire)
  (:config "api_key" "k-test")
  (:help :deepseek-search)
  (:running (is (nle::find-tool "deepseek_search") "deepseek_search is registered")
            (is (nle::find-tool "deepseek_fetch") "deepseek_fetch is registered")
            (is (equal "k-test" (nodecode-deepseek-search::api-key)) "the section's key")
            (is (equal "https://api.deepseek.com" (nodecode-deepseek-search::base-url)))
            (is (= 5 (nodecode-deepseek-search::setting :count)) "the default count"))
  (:stopped (is (null (nle::find-tool "deepseek_search")) "stop unregisters the search tool")
            (is (null (nle::find-tool "deepseek_fetch")) "and the fetch tool"))
  (:refused ("count" 0) ("max_chars" 5) ("timeout_seconds" 0))
  (:idle nodecode-deepseek-search:deepseek-search-error
         (deepseek-search:search "x")))

(deftest deepseek-search-cell-needs-a-key-to-start ()
  (with-deepseek-wire
    (is-carrying (answer (refusal-text nlk:config-refusal
                                       (deepseek-search-start "base_url" "https://api.deepseek.test")))
                 ("no DeepSeek API key" "DEEPSEEK_API_KEY"))
    (is (null (nle::find-tool "deepseek_search")) "a refused start installed nothing")))

;;; --- search ------------------------------------------------------------------

(defparameter +two-hits+
  "{\"results\":[{\"title\":\"Kestrel facts\",\"url\":\"https://birds.example/kestrel\",
     \"summary\":\"The kestrel hovers before it strikes.\",\"date\":\"2026-03-04\",
     \"site_name\":\"birds.example\",\"rerank_score\":2.25,\"authority\":3},
    {\"title\":\"Kestrel\",\"url\":\"https://en.wikipedia.org/wiki/Kestrel\",
     \"snippet\":\"A small falcon.\",\"authority\":-1}]}"
  "One search answer with two hits, the second missing most signals.")

(deftest deepseek-search-cell-search-answers-the-hits ()
  (with-deepseek-wire
    (with-cell-stop ((deepseek-search-start "api_key" "k-test"))
      (wire-answer +two-hits+)
      (let ((answer (deepseek-tool "deepseek_search"
                                    (nlk:json-object "query" "kestrel" "count" 3))))
        (is (search "2 DeepSeek results for \"kestrel\"" answer) "the count line")
        (is (search "1. Kestrel facts" answer))
        (is (search "https://birds.example/kestrel" answer))
        (is (search "The kestrel hovers before it strikes." answer))
        (is (search "2026-03-04" answer) "the vendor's date")
        (is (search "rerank 2.25" answer) "the vendor's quality score")
        (is (search "authority 3" answer))
        (is (not (search "authority -1" answer)) "an unknown authority is left out")
        (is (search "A small falcon." answer) "the snippet stands in for a summary"))
      (let ((request (asked-first)))
        (is (= 1 (asked-count)) "one request")
        (is (eq :post (first request)))
        (is (equal "https://api.deepseek.com/v1/tools/search" (second request)))
        (is (equal "Bearer k-test" (asked-header request "authorization")))
        (let ((body (asked-body request)))
          (is (equal "kestrel" (nlk:json-value body :string "query")))
          (is (= 3 (nlk:json-value body :integer "count")) "the call's count")
          (is (null (gethash "need_content" body)) "content only when asked"))))))

(deftest deepseek-search-cell-search-clamps-a-long-query ()
  (with-deepseek-wire
    (with-cell-stop ((deepseek-search-start "api_key" "k-test"))
      (wire-answer "{\"results\":[]}")
      (let* ((query (with-output-to-string (out)
                       (loop repeat 20 do (write-string "kestrel " out))))
             (answer (deepseek-search:search query)))
        (is (search "No DeepSeek results" answer) "an empty set is not an error")
        (let* ((body (asked-body (asked-first)))
               (sent (nlk:json-value body :string "query")))
          (is (<= (length sent) 128) "the vendor's 128-character ceiling")
          (is (string= "kestrel" (subseq sent 0 7)) "cut at a word boundary"))))))

(deftest deepseek-search-cell-a-refused-status-is-a-repair ()
  (with-deepseek-wire
    (with-cell-stop ((deepseek-search-start "api_key" "k-test"))
      (wire-answer "{\"error\":{\"message\":\"Insufficient balance\"}}" 402)
      (is-carrying (answer (deepseek-tool "deepseek_search" (nlk:json-object "query" "x")))
                   ("ERROR: deepseek-search:")
                   ("HTTP 402")
                   ("Insufficient balance")
                   ("Top up the DeepSeek account")))))

(deftest deepseek-search-cell-retries-a-429-once-and-then-answers ()
  (with-deepseek-wire
    (with-cell-stop ((deepseek-search-start "api_key" "k-test"))
      (wire-answer "{\"error\":{\"message\":\"slow down\"}}" 429)
      (wire-answer "{\"results\":[]}")
      (deepseek-tool "deepseek_search" (nlk:json-object "query" "x"))
      (is (= 2 (asked-count)) "one retry")
      (is (= 1 (length *waits*)) "one wait")
      (is (<= 1.0 (first *waits*) 1.25) "the first backoff, plus jitter"))))

;;; --- fetch -------------------------------------------------------------------

(deftest deepseek-search-cell-fetch-unescapes-markdown ()
  (with-deepseek-wire
    (with-cell-stop ((deepseek-search-start "api_key" "k-test" "max_chars" 2000))
      (wire-answer (nlk:encode-json-object
                    (nlk:json-object
                     "title" "Kestrel"
                     "content" (format nil "\\# Heading~%\\[a link\\]\\(https\\:\\/\\/x\\)~%C:\\path and \\d"))))
      (let ((answer (deepseek-tool "deepseek_fetch"
                                    (nlk:json-object "url" "https://birds.example/kestrel"))))
        (is (search "# Heading" answer) "the escaping is reversed")
        (is (search "[a link](https://x)" answer))
        (is (search "C:\\path and \\d" answer) "a page's own letter escapes survive"))
      (let ((request (asked-first)))
        (is (eq :post (first request)))
        (is (equal "https://api.deepseek.com/v1/tools/fetch" (second request)))
        (is (equal "https://birds.example/kestrel"
                   (nlk:json-value (asked-body request) :string "url")))))))

(deftest deepseek-search-cell-settings-shape-the-request ()
  (with-deepseek-wire
    (with-cell-stop ((deepseek-search-start "api_key" "k-test"
                                            "need_content" t
                                            "localization" "203.0.113.9"))
      (wire-answer "{\"results\":[{\"title\":\"One\",\"url\":\"https://a.example\",\"content\":\"the page text\"}]}")
      (let ((answer (deepseek-search:search "kestrel"))
            (body (asked-body (asked-first))))
        (is (eq t (nlk:json-value body :boolean "need_content")) "the section's need_content")
        (is (equal "203.0.113.9" (nlk:json-value body :string "localization")))
        (is (search "the page text" answer) "a hit's page text when asked for"))
      (wire-answer "{\"results\":[]}")
      (with-cell-stop ((deepseek-search-start "api_key" "k-test" "localization" "not-an-ip"))
        (deepseek-search:search "kestrel")
        (is (null (gethash "localization" (asked-body (first *asked*)))) "a value that is not an IP is not sent")))))

(deftest deepseek-search-cell-fetch-cuts-a-long-page-and-keeps-going ()
  (with-deepseek-wire
    (with-cell-stop ((deepseek-search-start "api_key" "k-test" "max_chars" 200))
      (wire-answer (format nil "{\"title\":\"Long\",\"content\":\"~a\"}"
                            (with-output-to-string (out) (loop repeat 60 do (write-string "word " out)))))
      (wire-answer "{\"error\":{\"message\":\"blocked by the site\"}}" 403)
      (let ((answer (deepseek-search:fetch (list "https://a.example/one" "https://a.example/two"))))
        (is (search "https://a.example/one" answer))
        (is (search "[cut at 200 of" answer) "the page is cut, and says so")
        (is (search "https://a.example/two" answer) "the second page is still tried")
        (is (search "HTTP 403" answer) "its failure is one line, not the batch")
        (is (= 2 (asked-count)))))))
