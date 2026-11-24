;;;; cell-test.lisp --- lifecycle, the three tools, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Meilisearch: the config
;;;; surface (a host that is not an origin refuses the start), every tool
;;;; through the real dispatch seam against a stubbed server, the bytes of each
;;;; request, the answer a page becomes, the watch's baseline and its new ids,
;;;; and the sentence a missing setting earns.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "index-search"
  (:config "host" "http://127.0.0.1:7700" "api_key" +key+)
  (:help :index-search)
  (:command "index-search")
  (:running
   (is (nle::find-tool "index_search") "index_search is registered")
   (is (nle::find-tool "index_health") "index_health is registered")
   (is (nle::find-tool "index_watch") "index_watch is registered")
   (is (equal +key+ (nodecode-index-search::setting :api-key)) "the key the section carries")
   (is (equal "id" (nodecode-index-search::setting :id-field)) "and the id field /index-search defaults to"))
  (:stopped
   (is (null (nle::find-tool "index_search")) "stop unregisters the tools"))
  (:refused
   ("host" "not-an-origin")
   ("host" "ftp://127.0.0.1:7700")
   ("host" "http://127.0.0.1:7700/notes")
   ("host" "http://127.0.0.1:99999")
   ("host" "http://127.0.0.1:7700" "timeout_seconds" 0))
  (:idle nodecode-index-search:index-search-error
         (nodecode-index-search::search-one "notes" "cats" "id" "")))

;;; --- a setting that is absent -----------------------------------------------------

(deftest index-search-cell-a-missing-host-refuses-every-verb ()
  ;; The one member no call can work without: a section that names no origin
  ;; answers with the sentence that says what to set, and dials nothing.
  (with-index ("host" "")
    (let ((text (call-tool "index_search"
                            "{\"index\":\"notes\",\"q\":\"cats\",\"id_field\":\"id\"}")))
      (is (cl:search "ERROR: The host is not set" text) "the search says what is missing")
      (is (cl:search "http://127.0.0.1:7700" text) "and names the usual origin to put there"))
    (is (cl:search "ERROR: The host is not set" (call-tool "index_health" "{}")) "the health check says it too")
    (is (null *calls*) "and no request left the machine")))

;;; --- the health check --------------------------------------------------------------

(deftest index-search-cell-health-checks-the-server-with-no-key ()
  (with-index ()
    (let ((text (call-tool "index_health" "{}")))
      (is (equal "The server status is available." text) "an available server says so")
      (is (= 1 (length *calls*)) "one request answers it")
      (let ((call (first *calls*)))
        (is (equal :get (getf call :method)) "GET /health, as the original sent")
        (is (cl:search "/health" (getf call :url)) "on the section's origin")
        (is (null (header call "authorization")) "and the key never rides it")))
    (answer-with "{\"status\":\"maintenance\"}")
    (is (equal "The server is not healthy." (call-tool "index_health" "{}")) "any other status is not healthy")))

(deftest index-search-cell-health-that-never-answers-says-so ()
  (with-index ()
    (answer-with "" 0)
    (is (equal "The health check did not complete." (call-tool "index_health" "{}")) "a server that did not answer is named as such")))

;;; --- the search --------------------------------------------------------------------

(deftest index-search-cell-searches-one-page-of-the-index ()
  (with-index ()
    (answer-with (index-page-text (list (hit "id" "a-1" "title" "First note")
                                  (hit "id" "a-2" "title" "Second note"))
                             2))
    (let ((text (call-tool "index_search"
                            "{\"index\":\"notes\",\"q\":\"cats\",\"id_field\":\"id\",\"title_field\":\"title\"}")))
      (is (cl:search "Hits: \"a-1\" (First note), \"a-2\" (Second note)." text) "every hit's id, with its title")
      (is (cl:search "estimatedTotalHits is 2." text) "and how many matched"))
    (let* ((call (first *calls*))
           (body (sent-body call)))
      (is (= 1 (length *calls*)) "one request, not one a hit")
      (is (equal :post (getf call :method)) "POST, as Meilisearch's search takes it")
      (is (cl:search "/indexes/notes/search" (getf call :url)) "to the index the call names")
      (is (equal (format nil "Bearer ~a" +key+) (header call "authorization")) "with the section's key")
      (is (equal "cats" (gethash "q" body)) "the query travels")
      (is (= 20 (gethash "limit" body)) "for the one page size the original reports"))))

(deftest index-search-cell-a-page-that-is-not-a-search-result-is-refused ()
  (with-index ()
    (answer-with (index-page-text (list (hit "id" "a-1")) 0))
    (is (cl:search "ERROR: The search returned an unusable page"
                   (call-tool "index_search" "{\"index\":\"notes\",\"q\":\"cats\",\"id_field\":\"id\"}")) "a total of zero beside a hit is not a page")
    (answer-with "{\"code\":\"index_not_found\",\"message\":\"Index not found.\"}" 404)
    (let ((text (call-tool "index_search" "{\"index\":\"notes\",\"q\":\"cats\",\"id_field\":\"id\"}")))
      (is (cl:search "ERROR: The search failed (HTTP 404, index_not_found)." text) "a refused search names the status and the server's own code")
      (is (cl:search "Seen ids were not advanced." text) "and says what did not move"))))

(deftest index-search-cell-a-bad-argument-never-leaves-the-machine ()
  (with-index ()
    (let ((text (call-tool "index_search"
                            "{\"index\":\"no tes\",\"q\":\"cats\",\"id_field\":\"id\"}")))
      (is (cl:search "ERROR: The index uid must be 1 to 64 letters" text) "an index uid the server would refuse is refused here"))
    (is (cl:search "ERROR: The query must be a non-empty string"
                   (call-tool "index_search" "{\"index\":\"notes\",\"q\":\" \",\"id_field\":\"id\"}")) "and so is an empty query")
    (is (null *calls*) "neither cost a request")))

(deftest index-search-cell-a-page-with-no-hits-is-an-answer ()
  (with-index ()
    (answer-with (index-page-text '() 0))
    (is (cl:search "This page has 0 hits."
                   (call-tool "index_search" "{\"index\":\"notes\",\"q\":\"cats\",\"id_field\":\"id\"}")) "an empty page is an answer, not a failure")))

;;; --- the watch ----------------------------------------------------------------------

(deftest index-search-cell-the-watch-baselines-then-reports-what-is-new ()
  (with-index ()
    (let ((saved (call-tool "index_watch"
                             "{\"action\":\"save\",\"index\":\"notes\",\"q\":\"cats\",\"id_field\":\"id\",\"title_field\":\"title\"}")))
      (is (cl:search "Watch query saved. Index: notes. Query: cats." saved) "the query is saved")
      (is (cl:search "This call did not search." saved) "saving does not dial")
      (is (null *calls*) "and no request was made"))
    (is (cl:search "A watch query is saved. Index: notes."
                   (call-tool "index_watch" "{\"action\":\"status\"}")) "status reads it back")
    (answer-with (index-page-text (list (hit "id" "a-1" "title" "First")
                                  (hit "id" "a-2" "title" "Second"))
                            2))
    (let ((first (call-tool "index_watch" "{\"action\":\"check\"}")))
      (is (cl:search "Baseline saved. These ids are not reported as new." first) "the first read is a baseline, not news")
      (is (cl:search "estimatedTotalHits is 2." first) "and says how many the page held"))
    (answer-with (index-page-text (list (hit "id" "a-2" "title" "Second")
                                  (hit "id" "a-3" "title" "Third"))
                            9))
    (let ((second (call-tool "index_watch" "{\"action\":\"check\"}")))
      (is (cl:search "New ids: \"a-3\" (Third)." second) "only the id it has not reported")
      (is (equal nil (cl:search "\"a-2\"" (subseq second 0 (or (cl:search "estimatedTotalHits" second)
                                                               (length second))))) "the one it reported before is not news again")
      (is (cl:search "estimatedTotalHits is 9." second) "and the page's real size is still said"))))

(deftest index-search-cell-the-watch-survives-a-restart ()
  (with-index ()
    (call-tool "index_watch" "{\"action\":\"save\",\"index\":\"notes\",\"q\":\"cats\",\"id_field\":\"id\"}")
    (index-search-start)
    (is (cl:search "A watch query is saved. Index: notes."
                   (call-tool "index_watch" "{\"action\":\"status\"}")) "the file is what holds the watch, not the cell's memory")))

(deftest index-search-cell-a-check-with-nothing-saved-says-so ()
  (with-index ()
    (is (cl:search "ERROR: No watch query is saved"
                   (call-tool "index_watch" "{\"action\":\"check\"}")) "a check before a save names what to do")
    (is (null *calls*) "without dialling")))

;;; --- the command ----------------------------------------------------------------------

(deftest index-search-cell-the-command-searches-and-checks ()
  (with-index ()
    (answer-with (index-page-text (list (hit "id" "a-1" "title" "First note")) 1))
    (let ((text (cell-entry "nodecode-index-search" "index-search" "notes cats" nil)))
      (is (cl:search "Hits: \"a-1\"." text) "INDEX QUERY searches that index")
      (is (cl:search "/indexes/notes/search" (getf (first *calls*) :url)) "at the index it names")
      (is (equal "cats" (gethash "q" (sent-body (first *calls*)))) "for the words after it"))
    (is (cl:search "No watch query is saved."
                   (cell-entry "nodecode-index-search" "index-search" "status" nil)) "status reads the watch, and says when there is none")
    (multiple-value-bind (text dialog)
        (cell-entry "nodecode-index-search" "index-search" "" nil)
      (is (cl:search "search the index you run" text) "bare, it says what it does")
      (is (cl:search "Index and query" (nlk:encode-json-object dialog)) "and offers a row to type in"))))
