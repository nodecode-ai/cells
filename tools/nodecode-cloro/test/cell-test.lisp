;;;; cell-test.lisp --- lifecycle, the tool, the parameters, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not cloro: the config
;;;; surface (a section naming no key refuses the start), the tool through
;;;; the real dispatch seam against a stubbed API, every parameter it sends,
;;;; the text its answer is cleaned into, and the refusal a refused key gets.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "cloro"
  (:config "api_key" +key+)
  (:help :cloro)
  (:command "cloro")
  (:running
   (is (nle::find-tool "cloro_search") "cloro_search is registered")
   (is (equal +key+ (nodecode-cloro::setting :api-key)) "the key the section carries"))
  (:stopped
   (is (null (nle::find-tool "cloro_search")) "stop unregisters the tool"))
  (:refused
   ("api_key" "" "api_key_env" "UNSET_CLORO_KEY")
   ("api_key" +key+ "count" 0)
   ("api_key" +key+ "timeout_seconds" 0)))

(deftest cloro-cell-without-a-key-refuses ()
  ;; The one setting no call can work without: a section naming no key, over a
  ;; variable that holds none, is refused at start, as the original offered no
  ;; backend without CLORO_API_KEY.
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (cloro-start "api_key" ""
                                                 "api_key_env" "UNSET_CLORO_KEY"))))
      (is (cl:search "no key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "cloro_search")) "and installs nothing"))))

;;; --- the tool -------------------------------------------------------------------

(deftest cloro-cell-the-tool-posts-the-query-and-answers-cloro-s-results (with-cloro ()
  (let ((text (cloro-call-tool "cloro_search" "{\"query\":\"rust release notes\"}")))
    (is (cl:search "[1] Rust & Cargo: 1.78.0 is out" text) "the first result, its title cleaned")
    (is (cl:search "https://blog.rust-lang.org/releases/1.78.0" text) "with its url")
    (is (cl:search "Announcing Rust 1.78.0" text) "its snippet, with the tags cut off")
    (is (cl:search "the release notes' details & more" text) "and its entities decoded")
    (is (cl:search "[2] Rust 1.78 changelog" text) "the second resolved result too")
    (is (null (cl:search "without a resolved link" text)) "a result with no link is skipped"))
  (let ((call (first *calls*)))
    (is (= 1 (length *calls*)) "one request, not one a result")
    (is (eq :post (getf call :method)) "a POST")
    (is (equal "https://api.cloro.dev/v1/monitor/google" (getf call :url)) "to cloro's endpoint")
    (is (equal (format nil "Bearer ~a" +key+) (header call "authorization")) "with the section's key as a bearer token")
    (is (equal "application/json" (header call "content-type")))
    (is (equal "rust release notes" (body-value call "query")) "the query in the body")
    (is (equal "US" (body-value call "country")) "and the section's country, as the original sent it"))))

(deftest cloro-cell-a-call-s-own-parameters-ride-the-request (with-cloro ()
  (let ((text (cloro-call-tool "cloro_search"
                                "{\"query\":\"monsoon\",\"count\":50,\"country\":\"de\"}")))
    (is (cl:search "2 results for \"monsoon\"" text) "a count past the ceiling still answers what cloro had")
    (is (cl:search "(country DE)" text) "and the answer says where the results are located"))
  (let ((call (first *calls*)))
    (is (equal "DE" (body-value call "country")) "a call's own country is upcased into the body"))))

(deftest cloro-cell-the-section-s-defaults-ride-a-bare-call (with-cloro ("country" "de" "count" 1)
  (let ((text (cloro-call-tool "cloro_search" "{\"query\":\"today\"}")))
    (is (cl:search "1 result for \"today\"" text) "the section's count answers a call that names none")
    (let ((call (first *calls*)))
      (is (equal "DE" (body-value call "country")) "and its country, in the body")))))

;;; --- the text a model reads --------------------------------------------------------

(deftest cloro-cell-a-title-and-snippet-arrive-as-plain-text (with-cloro ()
  (let ((text (nodecode-cloro:search "rust release notes")))
    (is (null (cl:search "<" text)) "no tag survives")
    (is (cl:search "the release notes' details & more" text) "and an entity is decoded, not dropped"))))

(deftest cloro-cell-no-results-says-so (with-cloro ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values +empty+ 200)))
  (let ((text (nodecode-cloro:search "nothing at all")))
    (is (cl:search "no results" text) "an answer with no organic results is no results")
    (is (cl:search "\"nothing at all\"" text) "and names the query it found them for"))))

(deftest cloro-cell-an-answer-with-no-results-member-refuses (with-cloro ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values "{\"success\":true}" 200)))
  (let ((condition (signals-error nodecode-cloro:cloro-error
                                  (nodecode-cloro:search "rust"))))
    (is (cl:search "organicResults" (princ-to-string condition)) "the missing member is named"))))

(deftest cloro-cell-a-refused-key-carries-cloro-s-own-sentence (with-cloro ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values +error+ 401)))
  (let ((condition (signals-error nodecode-cloro:cloro-error
                                  (nodecode-cloro:search "rust"))))
    (is (cl:search "HTTP 401" (princ-to-string condition)) "the status is named")
    (is (cl:search "The API key is invalid." (princ-to-string condition)) "and cloro's own sentence"))))

;;; --- the boundary and the command --------------------------------------------------

(deftest cloro-cell-a-call-with-no-query-never-reaches-the-wire (with-cloro ()
  (let ((condition (signals-error nodecode-cloro:cloro-error
                                  (nodecode-cloro:search "  "))))
    (is (cl:search "needs a query" (princ-to-string condition))))
  (is (null *calls*) "and nothing was dialled")))

(deftest cloro-cell-a-country-that-is-not-two-letters-refuses (with-cloro ()
  (let ((condition (signals-error nodecode-cloro:cloro-error
                                  (nodecode-cloro:search "rust" :country "USA"))))
    (is (cl:search "2-letter ISO 3166-1" (princ-to-string condition)) "the code it wanted is named"))
  (is (null *calls*) "and nothing was dialled")))

(deftest cloro-cell-the-command-searches-and-explains-itself (with-cloro ()
  (is (cl:search "Rust & Cargo"
                 (cell-entry "nodecode-cloro" "cloro" "rust release notes" "cloro-cell-1")) "/cloro searches")
  (let ((bare (cell-entry "nodecode-cloro" "cloro" "" "cloro-cell-1")))
    (is (cl:search "cloro_search" bare) "with no query it names what the cell gives")
    (is (cl:search "5 results" bare) "and the count a bare call asks for"))))
