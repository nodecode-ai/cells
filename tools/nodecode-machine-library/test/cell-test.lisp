;;;; cell-test.lisp --- lifecycle, the two tools, and /machine-library, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Machine Library,
;;;; needs a key or touches the network. What is proved is the cell's own
;;;; wiring: the section (a healthy start, wrong values and a missing key
;;;; refused), the two tools through the seam the model calls them by -- their
;;;; url, their body, their headers and their answer -- and the operator's
;;;; command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-machine-library-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials Machine Library."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected machine library request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun machine-library-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun machine-library-reply (text &optional (status 200))
  "Answer every request with TEXT at STATUS."
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun machine-library-request-body (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun machine-library-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun machine-library-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "machine-library-call" name arguments)))

(defparameter +search-answer+
  (concatenate 'string
               "{\"hits\": ["
               "{\"id\": \"doc1\", \"score\": 0.9,"
               " \"snippets\": [{\"field\": \"content\", \"text\": \"The  Transformer\\nuses attention.\"}],"
               " \"document\": {\"title\": \"Attention Is All You Need\", \"type\": \"journal-article\","
               "  \"authors\": [{\"name\": \"A\"}, {\"name\": \"B\"}, {\"name\": \"C\"}, {\"name\": \"D\"}],"
               "  \"issued_at\": 1497225600,"
               "  \"uris\": [\"arxiv://1706.03762\", \"doi://10.48550/arXiv.1706.03762\"]}},"
               "{\"id\": \"iso1\", \"snippets\": [],"
               " \"document\": {\"title\": \"Fluid power systems\", \"type\": \"standard\","
               "  \"abstract\": \"Graphical symbols.\", \"metadata\": {\"publisher\": \"ISO\"},"
               "  \"uris\": [\"iso://1219-1:2012\"]}}]}")
  "A /v2/search answer with a journal article and a standard: the provider's own fixture.")

(defparameter +document-answer+
  (concatenate 'string
               "{\"id\": \"abc123\", \"uris\": [\"doi://10.1000/xyz\"],"
               " \"document\": {\"title\": \"A Paper\", \"abstract\": \"Abs.\", \"content\": \"Body.\","
               "  \"content_length\": 99, \"type\": \"book\"}}")
  "A /v2/documents answer: one document with a title, an abstract and text.")

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "machine-library" "MACHINE-LIBRARY-CELL-" :start nodecode-machine-library:start-cell)

(define-cell-lifecycle-tests "machine-library"
  (:fixture with-machine-library-wire)
  (:config "api_key" "ml-test")
  (:help :machine-library)
  (:command "machine-library")
  (:running
   (is (nle::find-tool "machine_library_search") "search is registered")
   (is (nle::find-tool "machine_library_extract") "and extract")
   (is (= 2 (count-if (lambda (entry) (eql 0 (search "machine_library_" (first entry))))
                      nle::*tools*)) "two of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "machine_library_search")) "and stop takes both back")
   (is (null (nle::find-tool "machine_library_extract"))))
  (:refused ("timeout_seconds" 0)
            ("snippet_chars" 10)
            ("max_chars" 100)
            ("api_key" "ml-test" "max_results" 0)
            ("api_key" "ml-test" "max_results" 50)))

(deftest machine-library-cell-a-start-with-no-key-refuses-and-installs-nothing (with-machine-library-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (machine-library-start))))
      (is-carrying text "needs a key" "MACHINELIBRARY_API_KEY")
      (is (null (nle::find-tool "machine_library_search")) "nothing was installed"))))

(deftest machine-library-cell-the-key-may-come-from-the-environment (with-machine-library-wire)
  (with-stubbed-fdefinition (uiop:getenv (name)
                             (and (equal name "MACHINELIBRARY_API_KEY") "ml-env"))
    (with-cell-stop ((machine-library-start))
      (machine-library-reply "{\"hits\": []}")
      (machine-library-call "machine_library_search" "{\"query\": \"x\"}")
      (let ((sent (machine-library-request-header (first (machine-library-asked)) "x-api-key")))
        (is (equal "ml-env" sent) "the exported key is the one sent")))))

;;; --- search ---------------------------------------------------------------------

(deftest machine-library-cell-search-posts-the-query-and-reads-the-sources (with-machine-library-wire)
  (with-cell-stop ((machine-library-start "api_key" "ml-test"))
    (machine-library-reply +search-answer+)
    (let ((answer (machine-library-call "machine_library_search" "{\"query\": \"transformer attention\"}")))
      (let* ((request (first (machine-library-asked)))
             (body (machine-library-request-body request)))
        (is (eq :post (first request)) "one POST")
        (is (equal "https://api.machinelibrary.ai/v2/search/" (second request)) "to the documents index")
        (is (equal "ml-test" (machine-library-request-header request "x-api-key")) "with the section's key")
        (is (equal "api" (machine-library-request-header request "x-request-source")) "and the attribution header the provider sent")
        (is (equal "transformer attention" (nlk:json-value body :text "query")) "carrying the query")
        (is (= 5 (nlk:json-value body :integer "limit")) "and no count, so the section's default"))
      (is (search "1. Attention Is All You Need" answer) "the first source is numbered and titled")
      (is (search "https://machinelibrary.ai/r/10.48550/arXiv.1706.03762" answer) "with the page its DOI gives")
      (is (search "journal article · A, B, C et al. · 2017" answer) "and the byline the provider built")
      (is (search "The Transformer uses attention." answer) "and the passage that matched, whitespace collapsed")
      (is (search "source: doi://10.48550/arXiv.1706.03762" answer) "and the identifier to cite")
      (is (search "https://machinelibrary.ai/r/iso1" answer) "a document with no DOI cites its id")
      (is (search "standard · ISO" answer) "and still carries its byline"))))

(deftest machine-library-cell-search-caps-the-count-it-is-given (with-machine-library-wire)
  (with-cell-stop ((machine-library-start "api_key" "ml-test"))
    (machine-library-reply "{\"hits\": []}")
    (let ((answer (machine-library-call "machine_library_search" "{\"query\": \"lisp\", \"limit\": 50}")))
      (is (= 20 (nlk:json-value (machine-library-request-body (first (machine-library-asked)))
                                 :integer "limit")) "a count past the API's ceiling is cut to 20")
      (is (search "no Machine Library document matched" answer) "and an empty answer says so"))))

(deftest machine-library-cell-a-refused-key-is-the-model-s-repair (with-machine-library-wire)
  (with-cell-stop ((machine-library-start "api_key" "ml-test"))
    (machine-library-reply "{\"detail\": \"invalid api key\"}" 401)
    (let ((answer (machine-library-call "machine_library_search" "{\"query\": \"x\"}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "HTTP 401" answer) "and names the status")
      (is (search "api_key" answer) "and tells the model what to check"))))

(deftest machine-library-cell-a-spent-balance-says-where-to-top-up (with-machine-library-wire)
  (with-cell-stop ((machine-library-start "api_key" "ml-test"))
    (machine-library-reply "{}" 402)
    (let ((answer (machine-library-call "machine_library_search" "{\"query\": \"x\"}")))
      (is (search "HTTP 402" answer) "the balance is named")
      (is (search "machinelibrary.ai/keys" answer) "and the page to top it up on"))))

;;; --- extract ---------------------------------------------------------------------

(deftest machine-library-cell-extract-reads-a-document-page (with-machine-library-wire)
  (with-cell-stop ((machine-library-start "api_key" "ml-test"))
    (machine-library-reply +document-answer+)
    (let ((answer (machine-library-call "machine_library_extract"
                                        "{\"urls\": [\"https://machinelibrary.ai/r/10.1000/xyz\"]}")))
      (let ((request (first (machine-library-asked))))
        (is (eq :get (first request)) "one GET")
        (is (equal "https://api.machinelibrary.ai/v2/documents/by-uri/doi%3A%2F%2F10.1000%2Fxyz?max_chars=12000"
                   (second request)) "a /r/ page with a DOI is resolved by its identifier, as the provider resolved it"))
      (is (search "url: https://machinelibrary.ai/r/10.1000/xyz" answer) "the url asked for is named")
      (is (search "source: doi://10.1000/xyz" answer) "with the identifier behind it")
      (is (search "# A Paper" answer) "the title")
      (is (search "Abs." answer) "the abstract")
      (is (search "Body." answer) "and the stored text")
      (is (search "the stored document is 99 characters long" answer) "saying it was cut"))))

(deftest machine-library-cell-extract-reads-a-document-by-its-id (with-machine-library-wire)
  (with-cell-stop ((machine-library-start "api_key" "ml-test"))
    (machine-library-reply +document-answer+)
    (machine-library-call "machine_library_extract"
                          "{\"urls\": [\"https://www.machinelibrary.ai/r/abc123/some-title-slug\"]}")
    (is (equal "https://api.machinelibrary.ai/v2/documents/abc123?max_tokens=3000"
               (second (first (machine-library-asked)))) "an id page is fetched by id, with the token bound the provider set")))

(deftest machine-library-cell-extract-answers-each-url-on-its-own (with-machine-library-wire)
  (with-cell-stop ((machine-library-start "api_key" "ml-test"))
    (setf *reply* (lambda (method url args)
                    (declare (ignore method args))
                    (if (search "by-uri/" url)
                        (values "{\"id\": \"d1\", \"uris\": [\"doi://10.1/x\"], \"document\": {\"title\": \"T\", \"content\": \"Body.\"}}"
                                200)
                        (values "" 404))))
    (let ((answer (machine-library-call "machine_library_extract"
                                        "{\"urls\": [\"doi://10.1/x\", \"https://machinelibrary.ai/r/gone\"]}")))
      (is (search "Body." answer) "the document the library holds is read")
      (is (search "url: https://machinelibrary.ai/r/gone" answer) "the url it does not hold is still answered")
      (is (search "no Machine Library document" answer) "with the reason it has none")
      (is (= 2 (length (machine-library-asked))) "one request per url, neither abandoned"))))

(deftest machine-library-cell-extract-refuses-a-url-it-cannot-hold (with-machine-library-wire)
  (with-cell-stop ((machine-library-start "api_key" "ml-test"))
    (let ((answer (machine-library-call "machine_library_extract"
                                        "{\"urls\": [\"https://machinelibrary.ai/search\"]}")))
      (is (search "not a Machine Library document page" answer) "the model is told what a document url is")
      (is (null (machine-library-asked)) "and the API was never asked"))))

(deftest machine-library-cell-extract-refuses-a-call-with-no-urls (with-machine-library-wire)
  (with-cell-stop ((machine-library-start "api_key" "ml-test"))
    (let ((answer (machine-library-call "machine_library_extract" "{}")))
      (is (search "urls" answer) "the refusal names the member the call left out")
      (is (null (machine-library-asked)) "and nothing was sent"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest machine-library-cell-the-command-searches-or-says-what-it-is-connected-to (with-machine-library-wire)
  (with-cell-stop ((machine-library-start "api_key" "ml-test"))
    (let ((status (nodecode-machine-library::run-command "" nil)))
      (is (search "https://api.machinelibrary.ai" status) "no argument reports the connection")
      (is (search "key set" status) "and that a key is there")
      (is (null (search "ml-test" status)) "without ever printing the key")
      (is (null (machine-library-asked)) "and sends nothing"))
    (machine-library-reply +search-answer+)
    (let ((answer (nodecode-machine-library::run-command "  transformer attention  " nil)))
      (is (search "1. Attention Is All You Need" answer) "an argument runs one search and answers its sources")
      (let ((query (nlk:json-value (machine-library-request-body (first (machine-library-asked)))
                                    :text "query")))
        (is (equal "transformer attention" query) "the query trimmed, as typed")))))
