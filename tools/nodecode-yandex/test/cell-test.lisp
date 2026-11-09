;;;; cell-test.lisp --- lifecycle, both tools, the guards, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Yandex: the config
;;;; surface (a section naming no key or no folder refuses the start), each
;;;; tool through the real dispatch seam against a stubbed API, every member
;;;; it sends, the text its answer is cleaned into, the report's guards (a
;;;; DTD, a Base64 that is not), and the refusal a refused key gets.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "yandex"
  (:config "api_key" +key+ "folder_id" +folder+)
  (:help :yandex)
  (:command "yandex")
  (:running
   (is (nle::find-tool "yandex_web_search") "yandex_web_search is registered")
   (is (nle::find-tool "yandex_generative_search") "yandex_generative_search is registered")
   (is (equal +key+ (nodecode-yandex::setting :api-key)) "the key the section carries")
   (is (equal +folder+ (nodecode-yandex::setting :folder-id)) "and the folder"))
  (:stopped
   (is (null (nle::find-tool "yandex_web_search")) "stop unregisters the web tool")
   (is (null (nle::find-tool "yandex_generative_search")) "and the generative tool"))
  (:refused
   ("api_key" "" "api_key_env" "UNSET_YANDEX_KEY")
   ("api_key" +key+ "folder_id" "" "folder_id_env" "UNSET_YANDEX_FOLDER")
   ("api_key" +key+ "folder_id" +folder+ "search_type" "SEARCH_TYPE_MARS")
   ("api_key" +key+ "folder_id" +folder+ "max_results" 0)
   ("api_key" +key+ "folder_id" +folder+ "timeout_seconds" 0))
  (:idle nodecode-yandex:yandex-error (yandex:search "x")))

(deftest yandex-cell-the-two-credentials-are-named-when-missing ()
  (with-wire
    (let ((condition (signals-error nlk:config-refusal (yandex-start "api_key" "" "api_key_env" "UNSET_YANDEX_KEY" "folder_id" +folder+))))
      (is (cl:search "no API key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "yandex_web_search")) "and installs nothing"))
    (let ((condition (signals-error nlk:config-refusal (yandex-start "api_key" +key+ "folder_id" "" "folder_id_env" "UNSET_YANDEX_FOLDER"))))
      (is (cl:search "no folder id" (princ-to-string condition)) "the folder is named the same way")
      (is (null (nle::find-tool "yandex_generative_search")) "with nothing installed"))))

;;; --- the web tool ---------------------------------------------------------------

(deftest yandex-cell-the-web-tool-asks-yandex-and-answers-its-documents (with-yandex ()
  (let ((text (yandex-call-tool "yandex_web_search" "{\"query\":\"moscow weather\"}")))
    (is (cl:search "[1] Moscow weather today" text) "the first document, its highlight tags dropped")
    (is (cl:search "https://example.org/moscow" text) "with its url")
    (is (cl:search "example.org" text) "and its domain")
    (is (cl:search "The forecast for Moscow is mild. More rain later." text) "its passages joined into one snippet")
    (is (cl:search "[2] Second page" text) "the second document too")
    (is (cl:search "Second snippet & more." text) "entities decoded, not dropped")
    (is (not (cl:search "No url here" text)) "a document with no url is left out"))
  (let* ((call (first *calls*))
         (body (yandex-body call)))
    (is (= 1 (length *calls*)) "one request, not one a result")
    (is (cl:search "searchapi.api.cloud.yandex.net/v2/web/search" (getf call :url)) "Yandex's endpoint")
    (is (equal (format nil "Api-Key ~a" +key+) (yandex-header call "authorization")) "the section's key, as Api-Key")
    (is (equal +folder+ (nlk:json-value body :string "folderId")) "the folder the request bills")
    (is (equal "moscow weather" (nlk:json-value body :string "query" "queryText")) "the query as typed")
    (is (equal "SEARCH_TYPE_RU" (nlk:json-value body :string "query" "searchType")) "the section's market")
    (is (equal 5 (nlk:json-value body :integer "groupSpec" "groupsOnPage")) "the section's max_results answers a call that names none")
    (is (equal "FORMAT_XML" (nlk:json-value body :string "responseFormat")) "the report is asked for as XML"))))

(deftest yandex-cell-a-call-s-own-count-and-page-ride-the-request (with-yandex ()
  (yandex-call-tool "yandex_web_search" "{\"query\":\"x\",\"n\":500}")
  (let ((body (yandex-body (first *calls*))))
    (is (equal 100 (nlk:json-value body :integer "groupSpec" "groupsOnPage")) "a count past a hundred is clamped, as the original clamped it"))
  (nodecode-yandex:search "x" :n 3 :page 2)
  (let ((body (yandex-body (first *calls*))))
    (is (equal 3 (nlk:json-value body :integer "groupSpec" "groupsOnPage")) "the verb's own count")
    (is (equal 2 (nlk:json-value body :integer "query" "page")) "and its page"))))

(deftest yandex-cell-the-section-s-market-and-results-ride-a-bare-call
    (with-yandex ("search_type" "SEARCH_TYPE_TR" "max_results" 2)
  (nodecode-yandex:search "x")
  (let ((body (yandex-body (first *calls*))))
    (is (equal "SEARCH_TYPE_TR" (nlk:json-value body :string "query" "searchType")) "the section's market answers when nothing names one")
    (is (equal 2 (nlk:json-value body :integer "groupSpec" "groupsOnPage")) "and the section's max_results"))
  (let ((text (nodecode-yandex:search "x")))
    (is (search "2 results for" text) "a section bounded to two answers with two"))))

;;; --- the generative tool ---------------------------------------------------------

(deftest yandex-cell-the-generative-tool-answers-with-its-sources (with-yandex ()
  (setf *answer* (stub-json +generative-answer+))
  (let ((text (yandex-call-tool "yandex_generative_search" "{\"query\":\"when did the metro open\",\"sites\":[\"example.org\"]}")))
    (is (cl:search "The first metro opened in 1863." text) "the answer")
    (is (cl:search "[1] Metro history" text) "the first source, by its title")
    (is (cl:search "https://example.org/metro" text) "with its url")
    (is (cl:search "opened in London in 1863" text) "and the text Yandex used")
    (is (cl:search "[2] (untitled)" text) "a source with no title says so")
    (is (cl:search "Yandex searched: first metro opened; first underground railway" text) "the queries Yandex ran, both shapes read")
    (is (cl:search "Corrected query: first metro opened" text) "and its correction"))
  (let* ((call (first *calls*))
         (body (yandex-body call))
         (message (first (coerce (nlk:json-array body "messages") 'list))))
    (is (cl:search "/v2/gen/search" (getf call :url)) "the generative endpoint")
    (is (equal "ROLE_USER" (nlk:json-value message :string "role")) "one user turn")
    (is (equal "when did the metro open" (nlk:json-value message :string "content")) "carrying the question")
    (is (equal '("example.org") (coerce (nlk:json-array body "site" "site") 'list)) "the sites restrict the sources")
    (is (eq t (nlk:json-value body :any "fixMisspell")) "and typos are corrected"))))

(deftest yandex-cell-a-generative-answer-wrapped-in-an-array-reads-its-last-element
    (with-yandex ()
  (setf *answer* (stub-json (format nil "[~a]" +generative-answer+)))
  (is (cl:search "The first metro opened in 1863." (nodecode-yandex:answer "when did the metro open")) "a streaming-off answer arrives in a one-element array")))

(deftest yandex-cell-sites-are-capped-at-five (with-yandex ()
  (setf *answer* (stub-json +generative-answer+))
  (nodecode-yandex:answer "x" :sites '("a.example" "b.example" "c.example" "d.example" "e.example" "f.example" ""))
  (is (equal '("a.example" "b.example" "c.example" "d.example" "e.example") (coerce (nlk:json-array (yandex-body (first *calls*)) "site" "site") 'list)) "at most five domains, the blank one dropped")))

;;; --- the guards on the report -----------------------------------------------------

(deftest yandex-cell-a-report-with-a-dtd-or-a-bad-base64-is-refused (with-yandex ()
  (setf *answer* (stub-json (raw-data-answer "<!DOCTYPE yandexsearch [<!ENTITY a \"b\">]><yandexsearch/>")))
  (let ((condition (signals-error nodecode-yandex:yandex-error (nodecode-yandex:search "x"))))
    (is (cl:search "DTD" (princ-to-string condition)) "a DTD is refused whole"))
  (setf *answer* (stub-json "{\"rawData\":\"not base64!\"}"))
  (let ((condition (signals-error nodecode-yandex:yandex-error (nodecode-yandex:search "x"))))
    (is (cl:search "Base64" (princ-to-string condition)) "so is a rawData outside the alphabet"))
  (setf *answer* (stub-json "{}"))
  (let ((condition (signals-error nodecode-yandex:yandex-error (nodecode-yandex:search "x"))))
    (is (cl:search "without rawData" (princ-to-string condition)) "and an answer with none"))))

(deftest yandex-cell-a-refused-key-carries-yandex-s-own-sentence (with-yandex ()
  (setf *answer* (stub-json "{\"message\":\"Unable to authorize the request\",\"details\":[{\"requestId\":\"r-42\"}]}" 401))
  (let ((condition (signals-error nodecode-yandex:yandex-error (nodecode-yandex:search "x"))))
    (is (cl:search "HTTP 401" (princ-to-string condition)) "the status is named")
    (is (cl:search "Unable to authorize the request" (princ-to-string condition)) "and Yandex's own reason")
    (is (cl:search "r-42" (princ-to-string condition)) "and the request id to quote"))))

(deftest yandex-cell-a-search-with-no-query-never-reaches-the-wire (with-yandex ()
  (let ((condition (signals-error nodecode-yandex:yandex-error (nodecode-yandex:search "  "))))
    (is (cl:search "needs a query" (princ-to-string condition)) "a blank query is refused"))
  (let ((condition (signals-error nodecode-yandex:yandex-error (yandex:answer ""))))
    (is (cl:search "needs a question" (princ-to-string condition)) "and so is a blank question"))
  (is (null *calls*) "and nothing was dialled")))

;;; --- the command ------------------------------------------------------------------

(deftest yandex-cell-the-command-searches-and-explains-itself (with-yandex ()
  (is (cl:search "Moscow weather today" (cell-entry "nodecode-yandex" "yandex" "moscow weather" "yandex-cell-1")) "/yandex searches")
  (let ((bare (cell-entry "nodecode-yandex" "yandex" "" "yandex-cell-1")))
    (is (cl:search "yandex_web_search" bare) "with no query it names what the cell gives")
    (is (cl:search "5 results" bare) "and the count a bare call asks for"))))
