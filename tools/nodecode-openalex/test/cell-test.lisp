;;;; cell-test.lisp --- lifecycle, the tools and /openalex, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches OpenAlex, no key is
;;;; needed and no cent is spent. What is proved is the cell's own wiring: the
;;;; section (a healthy start, wrong values and an unreadable budget refused),
;;;; the ten tools through the seam the model calls them by -- their url, their
;;;; headers, their parameters and their answer -- and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-openalex-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered by
*REPLY*, so no test dials OpenAlex."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected openalex request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun openalex-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun openalex-reply (text &optional (status 200))
  "The next stubbed answer: TEXT with STATUS."
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun openalex-url (request)
  "The url of one stubbed request."
  (second request))

(defun openalex-query (request name)
  "The query parameter NAME of a stubbed request, or NIL."
  (let* ((url (openalex-url request))
         (at (search "?" url))
         (pairs (and at (loop for start = (1+ at) then (1+ end)
                               for end = (position #\& url :start start)
                               collect (subseq url start end)
                               while end))))
    (loop for pair in pairs
          for equals = (position #\= pair)
          when (and equals (equal name (subseq pair 0 equals)))
            return (quri:url-decode (subseq pair (1+ equals))))))

(defun openalex-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun openalex-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "openalex-call" name arguments)))

;;; --- lifecycle ------------------------------------------------------------------------

(define-test-slice "openalex" "OPENALEX-CELL-" :start nodecode-openalex:start-cell)

(define-cell-lifecycle-tests "openalex"
  (:fixture with-openalex-wire)
  (:config "budget_usd" "0.05")
  (:help :openalex)
  (:command "openalex")
  (:running
   (is (nle::find-tool "openalex_resolve") "resolve is registered")
   (is (nle::find-tool "openalex_get") "and get")
   (is (nle::find-tool "openalex_count") "and count")
   (is (nle::find-tool "openalex_search") "and search")
   (is (nle::find-tool "openalex_related") "and related")
   (is (nle::find-tool "openalex_account") "and account")
   (is (nle::find-tool "openalex_fields") "and fields")
   (is (nle::find-tool "openalex_classify") "and classify")
   (is (nle::find-tool "openalex_harvest") "and harvest")
   (is (nle::find-tool "openalex_fulltext") "and fulltext, the original's ten")
   (is (= 10 (count-if (lambda (entry) (eql 0 (search "openalex_" (first entry))))
                       nle::*tools*))
       "ten of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "openalex_resolve")) "and stop takes every one back")
   (is (null (nle::find-tool "openalex_fulltext"))))
  (:refused ("max_result_chars" 10)
            ("timeout_seconds" 0)
            ("budget_usd" "lots")
            ("verbosity" "verbose")))

;;; --- resolve and get: the free path ----------------------------------------------------

(deftest openalex-cell-resolve-turns-a-name-into-a-filterable-id (with-openalex-wire)
  (with-cell-stop ((openalex-start "budget_usd" "0.05"))
    (openalex-reply
     (concatenate 'string
                  "{\"results\": [{\"id\": \"https://openalex.org/A5023888391\","
                  " \"display_name\": \"Yoshua Bengio\", \"entity_type\": \"author\","
                  " \"hint\": \"Universite de Montreal\", \"works_count\": 812,"
                  " \"filter_key\": \"authorships.author.id\"}]}"))
    (let ((answer (openalex-call "openalex_resolve" "{\"query\": \"bengio\"}")))
      (let ((request (first (openalex-asked))))
        (is (equal :get (first request)) "one GET")
        (is (eql 0 (search "https://api.openalex.org/autocomplete?" (openalex-url request)))
            "to the autocomplete path, which is free")
        (is (equal "bengio" (openalex-query request "q")) "carrying the query"))
      (is (search "A5023888391" answer) "the bare id, not the url form")
      (is (search "Yoshua Bengio" answer) "the name")
      (is (search "authorships.author.id" answer) "and the filter key to use next")
      (is (search "free" answer) "and that it cost nothing"))))

(deftest openalex-cell-get-normalizes-a-doi-and-reads-one-record (with-openalex-wire)
  (with-cell-stop ((openalex-start "budget_usd" "0.05" "api_key" "k-test"))
    (openalex-reply
     (concatenate 'string
                  "{\"id\": \"https://openalex.org/W2741809807\", \"doi\": \"https://doi.org/10.7717/peerj.4375\","
                  " \"display_name\": \"The state of OA\", \"publication_year\": 2018,"
                  " \"type\": \"article\", \"cited_by_count\": 812,"
                  " \"open_access\": {\"oa_status\": \"gold\", \"oa_url\": \"https://a.test/1\"},"
                  " \"primary_location\": {\"source\": {\"id\": \"https://openalex.org/S1\","
                  " \"display_name\": \"PeerJ\"}},"
                  " \"authorships\": [{\"author\": {\"id\": \"https://openalex.org/A1\","
                  " \"display_name\": \"Heather Piwowar\"}}]}"))
    (let ((answer (openalex-call "openalex_get" "{\"id\": \"https://doi.org/10.7717/peerj.4375\"}")))
      (let ((request (first (openalex-asked))))
        (is (eql 0 (search "https://api.openalex.org/works/doi:10.7717/peerj.4375?"
                             (openalex-url request)))
            "the DOI normalized to the free prefixed form")
        (is (equal "Bearer k-test" (openalex-header request "authorization"))
            "the section's key, as a bearer token")
        (is (search "," (or (openalex-query request "select") "")) "with a select list"))
      (is (search "The state of OA" answer) "the title")
      (is (search "812 citations" answer) "the citation count")
      (is (search "PeerJ" answer) "its venue")
      (is (search "Heather Piwowar" answer) "and its author"))))

(deftest openalex-cell-get-reports-an-identifier-that-finds-nothing (with-openalex-wire)
  (with-cell-stop ((openalex-start "budget_usd" "0.05"))
    (openalex-reply "not found" 404)
    (let ((answer (openalex-call "openalex_get" "{\"id\": \"doi:10.9999/none\"}")))
      (is (search "not found: doi:10.9999/none" answer) "the identifier is named")
      (is (search "prefix" answer) "and the form it should have"))))

;;; --- count, search and the budget --------------------------------------------------------

(deftest openalex-cell-count-groups-without-a-select (with-openalex-wire)
  (with-cell-stop ((openalex-start "budget_usd" "0.05"))
    (openalex-reply
     (concatenate 'string
                  "{\"meta\": {\"count\": 42}, \"group_by\": ["
                  "{\"key\": \"2020\", \"key_display_name\": \"2020\", \"count\": 12},"
                  "{\"key\": \"2021\", \"key_display_name\": \"2021\", \"count\": 30}]}"))
    (let ((answer (openalex-call "openalex_count"
                                  "{\"filter\": \"is_oa:true\", \"group_by\": \"publication_year\"}")))
      (let ((request (first (openalex-asked))))
        (is (equal "publication_year" (openalex-query request "group_by")) "grouped, as asked")
        (is (equal "is_oa:true" (openalex-query request "filter")) "with the filter")
        (is (null (openalex-query request "select")) "and no select, which group_by refuses beside"))
      (is (search "42 matching" answer) "the total")
      (is (search "2021" answer) "and a group") 
      (is (search "30" answer) "with its count"))))

(deftest openalex-cell-a-search-past-the-budget-refuses-before-it-is-sent (with-openalex-wire)
  (with-cell-stop ((openalex-start "budget_usd" "0"))
    (let ((answer (openalex-call "openalex_search" "{\"search\": \"crispr\"}")))
      (is (search "ERROR: " answer) "a refusal is the tool result, not a condition up the stack")
      (is (search "budget" answer) "naming the ceiling")
      (is (search "openalex_get" answer) "and the free call that still works")
      (is (null (openalex-asked)) "and nothing was sent"))))

(deftest openalex-cell-search-reports-what-it-cost (with-openalex-wire)
  (with-cell-stop ((openalex-start "budget_usd" "0.05"))
    (openalex-reply
     (concatenate 'string
                  "{\"meta\": {\"count\": 900}, \"results\": ["
                  "{\"id\": \"https://openalex.org/W2\", \"display_name\": \"A paper\","
                  " \"publication_year\": 2022, \"cited_by_count\": 3}]}"))
    (let ((answer (openalex-call "openalex_search"
                                  "{\"search\": \"lisp\", \"search_field\": \"title_and_abstract\"}")))
      (let ((request (first (openalex-asked))))
        (is (equal "title_and_abstract.search:lisp" (openalex-query request "filter"))
            "a title-and-abstract search is a filter, not the full-text parameter")
        (is (null (openalex-query request "search")) "so the broad parameter is left out"))
      (is (search "A paper" answer) "the record")
      (is (search "$0.0010" answer) "and what the call cost"))))

;;; --- classify, fulltext, and the command ------------------------------------------------

(deftest openalex-cell-classify-is-off-until-the-section-allows-it (with-openalex-wire)
  (with-cell-stop ((openalex-start "budget_usd" "0.05"))
    (let ((answer (openalex-call "openalex_classify" "{\"title\": \"a title\"}")))
      (is (search "off" answer) "classification is off by default")
      (is (search "allow_text_classification" answer) "and says which setting turns it on")
      (is (null (openalex-asked)) "nothing was sent")))
  (with-cell-stop ((openalex-start "budget_usd" "0.05" "allow_text_classification" t))
    (openalex-reply
     (concatenate 'string
                  "{\"primary_topic\": {\"display_name\": \"Natural Language Processing\","
                  " \"score\": 0.91, \"field\": {\"display_name\": \"Computer Science\"}},"
                  " \"topics\": []}"))
    (let ((answer (openalex-call "openalex_classify" "{\"title\": \"attention\"}")))
      (let ((request (first (openalex-asked))))
        (is (eql 0 (search "https://api.openalex.org/text/topics?" (openalex-url request)))
            "the text path")
        (is (equal "attention" (openalex-query request "title")) "with the title"))
      (is (search "Natural Language Processing" answer) "the topic")
      (is (search "Computer Science" answer) "and its field"))))

(deftest openalex-cell-fulltext-is-free-to-ask-and-paid-to-download (with-openalex-wire)
  (with-cell-stop ((openalex-start "budget_usd" "0.05"))
    (openalex-reply
     (concatenate 'string
                  "{\"id\": \"https://openalex.org/W3\", \"display_name\": \"A work\","
                  " \"has_content\": {\"grobid_xml\": true, \"pdf\": true},"
                  " \"content_urls\": {\"grobid_xml\": \"https://content.openalex.org/W3.xml\","
                  " \"pdf\": \"https://content.openalex.org/W3.pdf\"},"
                  " \"open_access\": {\"oa_url\": \"https://a.test/3\"}}"))
    (let ((answer (openalex-call "openalex_fulltext" "{\"id\": \"W3\"}")))
      (is (search "grobid_xml" answer) "the formats available")
      (is (search "confirm=true" answer) "and that nothing was downloaded")
      (is (= 1 (length (openalex-asked))) "one free lookup, no download"))
    ;; the confirm call asks twice: the record, then the artifact itself.
    (setf *reply*
          (lambda (method url args)
            (declare (ignore method args))
            (if (search "content.openalex.org" url)
                (values "<TEI><body><p>the whole paper</p></body></TEI>" 200)
                (values (concatenate 'string
                                     "{\"id\": \"https://openalex.org/W3\","
                                     " \"display_name\": \"A work\","
                                     " \"has_content\": {\"grobid_xml\": true},"
                                     " \"content_urls\": {\"grobid_xml\": \"https://content.openalex.org/W3.xml\"}}")
                        200))))
    (let ((answer (openalex-call "openalex_fulltext" "{\"id\": \"W3\", \"confirm\": true}")))
      (is (search "the whole paper" answer) "the prose of the TEI")
      (is (search "W3.xml" answer) "and where it came from"))))

(deftest openalex-cell-the-command-fetches-resolves-or-reports-the-budget (with-openalex-wire)
  (with-cell-stop ((openalex-start "budget_usd" "0.05" "api_key" "k-test"))
    (openalex-reply "{\"rate_limit\": {\"daily_remaining_usd\": 0.94, \"daily_budget_usd\": 1.0}}")
    (let* ((budget (nodecode-openalex::run-command "budget" nil))
           (request (first (openalex-asked))))
      (is (eql 0 (search "https://api.openalex.org/rate-limit" (openalex-url request)))
          "no argument reports the state, and a key makes it read the live allowance")
      (is (null (search "k-test" budget)) "without ever printing the key")
      (is (search "prices:" budget) "and the price table rides along")
      (is (search "0.94" budget) "with what is left of the daily allowance"))
    (setf *requests* '())
    (openalex-reply "{\"results\": [{\"id\": \"https://openalex.org/A9\", \"display_name\": \"Someone\"}]}")
    (let ((answer (nodecode-openalex::run-command "  bengio  " nil)))
      (is (search "autocomplete" (openalex-url (first (openalex-asked)))) "a name resolves")
      (is (equal "bengio" (openalex-query (first (openalex-asked)) "q")) "trimmed, as typed")
      (is (search "A9" answer) "and its matches come back"))))
