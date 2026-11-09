;;;; cell-test.lisp --- lifecycle, the tool, the parameters, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Brave: the config
;;;; surface (a section naming no key refuses the start), the tool through
;;;; the real dispatch seam against a stubbed API, every parameter it sends,
;;;; the text its answer is cleaned into, and the refusal a refused token
;;;; gets.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "brave-search"
  (:config "api_key" +key+)
  (:help :brave)
  (:command "brave")
  (:running
   (is (nle::find-tool "brave_web_search") "brave_web_search is registered")
   (is (equal +key+ (nodecode-brave-search::setting :api-key)) "the key the section carries"))
  (:stopped
   (is (null (nle::find-tool "brave_web_search")) "stop unregisters the tool"))
  (:refused
   ("api_key" "" "api_key_env" "UNSET_BRAVE_KEY")
   ("api_key" +key+ "count" 0)
   ("api_key" +key+ "safesearch" "loud")
   ("api_key" +key+ "timeout_seconds" 0)))

(deftest brave-search-cell-without-a-key-refuses ()
  ;; The one setting no call can work without: a section naming no key, over a
  ;; variable that holds none, is refused at start, as the original offered no
  ;; tool without BRAVE_API_KEY.
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (brave-search-start "api_key" ""
                                                        "api_key_env" "UNSET_BRAVE_KEY"))))
      (is (cl:search "no key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "brave_web_search")) "and installs nothing"))))

;;; --- the tool -------------------------------------------------------------------

(deftest brave-search-cell-the-tool-asks-brave-and-answers-its-results (with-brave ()
  (let ((text (brave-call-tool "brave_web_search" "{\"query\":\"rust release notes\"}")))
    (is (cl:search "[1] Rust & Cargo: 1.78.0 is out" text) "the first result, its title cleaned")
    (is (cl:search "https://blog.rust-lang.org/releases/1.78.0" text) "with its url")
    (is (cl:search "Rust Blog" text) "and who published it")
    (is (cl:search "2 weeks ago" text) "and how old Brave says it is")
    (is (cl:search "[2] Rust 1.78 changelog" text) "the second result too"))
  (let ((call (first *calls*)))
    (is (= 1 (length *calls*)) "one request, not one a result")
    (is (cl:search "api.search.brave.com/res/v1/web/search" (getf call :url)) "to Brave's endpoint")
    (is (equal +key+ (header call "x-subscription-token")) "with the section's token")
    (is (equal "rust release notes" (param call "q")))
    (is (equal "10" (param call "count"))
        "the section's count answers when the call names none")
    (is (equal "moderate" (param call "safesearch")) "and its safe-search mode")
    (is (null (param call "country")) "an unset member is not sent at all"))))

(deftest brave-search-cell-a-call-s-own-parameters-ride-the-request (with-brave ()
  (brave-call-tool "brave_web_search"
                    "{\"query\":\"monsoon\",\"count\":50,\"country\":\"in\",\"search_lang\":\"en\",\"freshness\":\"pw\",\"safesearch\":\"strict\"}")
  (let ((call (first *calls*)))
    (is (equal "20" (param call "count")) "a count past twenty is clamped, as the original clamped it")
    (is (equal "in" (param call "country")))
    (is (equal "en" (param call "search_lang")))
    (is (equal "pw" (param call "freshness")))
    (is (equal "strict" (param call "safesearch")) "a call's own mode wins over the section's"))))

(deftest brave-search-cell-the-section-s-defaults-ride-a-bare-call (with-brave ("freshness" "pd" "country" "us")
  (let ((text (brave-call-tool "brave_web_search" "{\"query\":\"today\"}")))
    (is (cl:search "(freshness pd)" text) "the answer says which window answered")
    (let ((call (first *calls*)))
      (is (equal "pd" (param call "freshness")))
      (is (equal "us" (param call "country")))
      (is (equal "10" (param call "count"))
          "the section's own count answers a call that names none")))))

;;; --- the text a model reads --------------------------------------------------------

(deftest brave-search-cell-a-title-and-description-arrive-as-plain-text (with-brave ()
  (let ((text (nodecode-brave-search:search "rust release notes")))
    (is (null (cl:search "<" text)) "no tag survives")
    (is (cl:search "the release notes' details & more" text) "and an entity is decoded, not dropped"))))

(deftest brave-search-cell-no-results-says-so (with-brave ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values "{}" 200)))
  (let ((text (nodecode-brave-search:search "nothing at all")))
    (is (cl:search "no results" text) "an answer with no web member is no results")
    (is (cl:search "\"nothing at all\"" text) "and names the query it found them for"))))

(deftest brave-search-cell-a-refused-token-carries-brave-s-own-sentence (with-brave ()
  (setf *answer* (lambda (call) (declare (ignore call)) (values +error+ 401)))
  (let ((condition (signals-error nodecode-brave-search:brave-search-error
                                  (nodecode-brave-search:search "rust"))))
    (is (cl:search "HTTP 401" (princ-to-string condition)) "the status is named")
    (is (cl:search "SUBSCRIPTION_TOKEN_INVALID" (princ-to-string condition)) "and Brave's own reason"))))

;;; --- the boundary and the command --------------------------------------------------

(deftest brave-search-cell-a-call-with-no-query-never-reaches-the-wire (with-brave ()
  (let ((condition (signals-error nodecode-brave-search:brave-search-error
                                  (nodecode-brave-search:search "  "))))
    (is (cl:search "needs a query" (princ-to-string condition))))
  (is (null *calls*) "and nothing was dialled")))

(deftest brave-search-cell-the-command-searches-and-explains-itself (with-brave ()
  (is (cl:search "Rust & Cargo"
                 (cell-entry "nodecode-brave-search" "brave" "rust release notes" "brave-cell-1"))
      "/brave searches")
  (let ((bare (cell-entry "nodecode-brave-search" "brave" "" "brave-cell-1")))
    (is (cl:search "brave_web_search" bare) "with no query it names what the cell gives")
    (is (cl:search "10 results" bare) "and the count a bare call asks for"))))
