;;;; cell-test.lisp --- lifecycle, the tool, the request, the refusals.
;;;;
;;;; SPDX-License-Identifier: Apache-2.0
;;;;
;;;; What is proved here is the cell's own wiring, not Grounder: the config
;;;; surface (a section naming no key refuses the start), the tool through the
;;;; real dispatch seam against a stubbed API, every member it sends, the lines
;;;; its answer is rendered into, and the refusal each kind of failure gets.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "grounder"
  (:config "api_key" +key+)
  (:help :grounder)
  (:command "grounder")
  (:running
   (is (nle::find-tool "grounder_search") "grounder_search is registered")
   (is (equal +key+ (nodecode-grounder::setting :api-key)) "the key the section carries"))
  (:stopped
   (is (null (nle::find-tool "grounder_search")) "stop unregisters the tool"))
  (:refused
   ("api_key" +key+ "max_results" 0)
   ("api_key" +key+ "description_chars" 0)
   ("api_key" +key+ "timeout_seconds" 0))
  (:idle nodecode-grounder:grounder-error (grounder:search "x")))

(deftest grounder-cell-without-a-key-refuses ()
  ;; The one setting no search can work without: a section naming no key, over
  ;; a variable that holds none, is refused at start.
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (grounder-start "api_key" ""
                                                    "api_key_env" "UNSET_GROUNDER_KEY"))))
      (is (cl:search "no key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "grounder_search")) "and installs nothing"))))

;;; --- the tool -------------------------------------------------------------------

(deftest grounder-cell-the-tool-asks-grounder-and-answers-its-rows (with-grounder ()
  (let ((text (grounder-call-tool "grounder_search" "{\"query\":\"offshore wind\"}")))
    (is (cl:search "2 results for \"offshore wind\"" text) "the answer counts what it got")
    (is (cl:search "[1] Offshore wind: the state of the fleet" text) "a numbered title")
    (is (cl:search "https://example.test/wind" text) "its url")
    (is (cl:search "A summary of the passage" text) "and its description on one line")
    (is (cl:search "[2] Second source" text) "the second result too"))
  (let ((call (first *calls*)))
    (is (= 1 (length *calls*)) "one request, not one a result")
    (is (eq :post (getf call :method)) "a POST")
    (is (equal "https://api.grounder.net/v1/search" (getf call :url)) "to Grounder's search endpoint")
    (is (equal "Bearer grounder-test-key" (sent-header call "authorization")) "with the section's key")
    (is (cl:search "nodecode-grounder" (sent-header call "user-agent")) "naming this cell")
    (let ((body (sent-body call)))
      (is (equal "offshore wind" (nlk:json-value body :string "query")))
      (is (= 5 (nlk:json-value body :integer "top_k")) "the section's max_results answers a call that names none")
      (is (= 800 (nlk:json-value body :integer "text_max_chars")) "the text budget rides the request")
      (is (= 45 (getf call :timeout)) "and the section's timeout bounds it")))))

(deftest grounder-cell-a-call-s-limit-is-kept-in-range (with-grounder ()
  (grounder-call-tool "grounder_search" "{\"query\":\"q\",\"limit\":50}")
  (is (= 20 (nlk:json-value (sent-body (first *calls*)) :integer "top_k")) "a limit past twenty is clamped, as the original clamped it")
  (grounder-call-tool "grounder_search" "{\"query\":\"q\",\"limit\":0}")
  (is (= 1 (nlk:json-value (sent-body (first *calls*)) :integer "top_k")) "and one below is a single result")))

(deftest grounder-cell-a-query-is-cut-to-two-thousand-characters (with-grounder ()
  (nodecode-grounder:search (make-string 2500 :initial-element #\x))
  (is (= 2000 (length (nlk:json-value (sent-body (first *calls*)) :string "query"))) "Grounder's own limit on a query, as the original sent it")))

(deftest grounder-cell-the-section-s-base-url-is-the-endpoint (with-grounder ("base_url" "https://api.example.test/")
  (nodecode-grounder:search "q")
  (is (equal "https://api.example.test/v1/search" (getf (first *calls*) :url)) "the origin the section names, its trailing slash and all")))

;;; --- the text a model reads --------------------------------------------------------

(deftest grounder-cell-a-row-is-the-summary-else-the-text-on-one-line (with-grounder ()
  (let ((text (nodecode-grounder:search "offshore wind")))
    (is (cl:search "A summary of the passage" text) "the summary, its whitespace collapsed")
    (is (null (cl:search "text the summary wins over" text)) "the text answers only without a summary")
    (is (cl:search (make-string 799 :initial-element #\#) text) "a text past the budget keeps its first characters")
    (is (not (cl:search (make-string 800 :initial-element #\#) text)) "and stops before the budget")
    (is (cl:search "…" text) "saying it was cut"))))

(deftest grounder-cell-a-text-at-the-budget-is-not-cut (with-grounder ()
  (setf *answer* (lambda (call)
                   (declare (ignore call))
                   (values (nlk:encode-json-object
                            (nlk:json-object
                             "results"
                             (vector (nlk:json-object "title" "t" "url" "https://e.test/"
                                                       "text" (make-string 800 :initial-element #\w)))))
                           200)))
  (let ((text (nodecode-grounder:search "q")))
    (is (cl:search (make-string 800 :initial-element #\w) text) "the whole text")
    (is (not (cl:search "…" text)) "with no mark of a cut"))))

(deftest grounder-cell-no-results-says-so (with-grounder ()
  (setf *answer* (lambda (call)
                   (declare (ignore call))
                   (values "{\"results\":[]}" 200)))
  (let ((text (nodecode-grounder:search "nothing at all")))
    (is (cl:search "no results" text) "an empty list is no results")
    (is (cl:search "\"nothing at all\"" text) "and names the query it found none for"))))

;;; --- the refusals ------------------------------------------------------------------

(deftest grounder-cell-a-refused-key-says-where-to-fix-it (with-grounder ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values +error+ 401)))
  (let ((detail (refusal-text nodecode-grounder:grounder-error (nodecode-grounder:search "q"))))
    (is (cl:search "rejected the search key in GROUNDER_API_KEY" detail) "the variable it read is named")
    (is (cl:search "grounder.net/account/keys" detail) "and where a new key comes from"))))

(deftest grounder-cell-a-failed-search-carries-grounder-s-own-sentence (with-grounder ()
  (setf *answer* (lambda (call)
                   (declare (ignore call))
                   (values "{\"error\":{\"message\":\"60 per minute\"}}" 429)))
  (is (cl:search "rate limit reached: 60 per minute"
                 (refusal-text nodecode-grounder:grounder-error (nodecode-grounder:search "q"))))
  (setf *answer* (lambda (call)
                   (declare (ignore call))
                   (values "{\"error\":{\"message\":\"boom\"}}" 500)))
  (is (cl:search "HTTP 500: boom"
                 (refusal-text nodecode-grounder:grounder-error (nodecode-grounder:search "q"))))
  (setf *answer* (lambda (call) (declare (ignore call)) (values "<html>Bad gateway</html>" 502)))
  (is (cl:search "HTTP 502"
                 (refusal-text nodecode-grounder:grounder-error (nodecode-grounder:search "q"))))
  (setf *answer* (lambda (call) (declare (ignore call)) (values "" 200)))
  (is (cl:search "parse"
                 (refusal-text nodecode-grounder:grounder-error (nodecode-grounder:search "q"))))))

(deftest grounder-cell-an-unreachable-api-is-a-refusal-not-a-crash (with-grounder ()
  (setf *answer* (lambda (call) (declare (ignore call)) (error "connection reset by peer")))
  (is (cl:search "Could not reach Grounder"
                 (refusal-text nodecode-grounder:grounder-error (nodecode-grounder:search "q"))))))

(deftest grounder-cell-a-blank-query-never-reaches-the-wire (with-grounder ()
  (is (cl:search "needs a query"
                 (refusal-text nodecode-grounder:grounder-error (nodecode-grounder:search "  "))))
  (is (null *calls*) "and nothing was dialled")))

;;; --- the command -------------------------------------------------------------------

(deftest grounder-cell-the-command-searches-and-explains-itself (with-grounder ()
  (is (cl:search "Offshore wind: the state of the fleet" (cell-entry "nodecode-grounder" "grounder" "offshore wind" "grounder-cell-1")) "/grounder searches")
  (let ((bare (cell-entry "nodecode-grounder" "grounder" "" "grounder-cell-1")))
    (is (cl:search "grounder_search" bare) "with no query it names what the cell gives")
    (is (cl:search "5 results" bare) "and the count a bare call asks for"))))
