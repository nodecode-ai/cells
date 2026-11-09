;;;; cell-test.lisp --- lifecycle, the five tools, and /adextract, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches the server, needs
;;;; a key or touches the network. What is proved is the cell's own wiring:
;;;; the section (a healthy start, wrong values and a missing key refused),
;;;; the MCP exchange it speaks (initialize, then the library's own tool), the
;;;; five tools through the seam the model calls them by -- their tool name,
;;;; their arguments, their headers and their answer -- and the operator's
;;;; command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS HEADERS).")

(defmacro with-adextract-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials the server."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected adextract request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun adextract-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun adextract-request-body (request)
  "REQUEST's JSON body, decoded."
  (let ((content (getf (cddr request) :content)))
    (and (stringp content) (nlk:decode-json content))))

(defun adextract-method (request)
  "The JSON-RPC method REQUEST carried."
  (nlk:json-value (adextract-request-body request) :text "method"))

(defun adextract-call-requests ()
  "The stubbed tools/call requests, oldest first."
  (remove-if-not (lambda (request) (equal "tools/call" (adextract-method request)))
                 (adextract-asked)))

(defun adextract-params (request)
  "The params object a stubbed JSON-RPC REQUEST carried."
  (nlk:json-value (adextract-request-body request) :object "params"))

(defun adextract-arguments (request)
  "The arguments object a stubbed tools/call REQUEST carried."
  (nlk:json-value (adextract-params request) :object "arguments"))

(defun adextract-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun adextract-headers (&optional (content-type "application/json") session)
  "A decoded header table, as NLK:HTTP answers one."
  (let ((table (make-hash-table :test 'equal)))
    (when content-type (setf (gethash "content-type" table) content-type))
    (when session (setf (gethash "mcp-session-id" table) session))
    table))

(defun adextract-result (text &key errorp)
  "An MCP tool result whose one text block is TEXT."
  (nlk:encode-json-object
   (nlk:make-json-object "content"
                          (vector (nlk:make-json-object "type" "text" "text" text))
                          "isError" (and errorp t))))

(defun adextract-serving (result)
  "A server that answers the handshake and gives RESULT — an encoded MCP result — to any tool."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url))
          (let* ((body (nlk:decode-json (getf args :content)))
                 (rpc (nlk:json-value body :text "method"))
                 (id (nlk:json-value body :integer "id")))
            (cond
              ((equal rpc "initialize")
               (values "{\"jsonrpc\": \"2.0\", \"result\": {\"protocolVersion\": \"2025-06-18\"}}"
                       200 (adextract-headers "application/json" "session-1")))
              ((equal rpc "notifications/initialized")
               (values "" 202 (adextract-headers)))
              ((equal rpc "tools/call")
               (values (format nil "{\"jsonrpc\": \"2.0\", \"id\": ~d, \"result\": ~a}" id result)
                       200 (adextract-headers "application/json" "session-1")))
              (t (error "unexpected JSON-RPC method ~a" rpc)))))))

(defun adextract-answer (text)
  "The server answers TEXT as its one text block, whatever tool is called."
  (adextract-serving (adextract-result text)))

(defun adextract-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "adextract-call" name arguments)))

(defparameter +meta-envelope+
  (format nil "{\"engine\": \"meta_ad_library\", \"search_information\": {\"total_results\": 482},~
                \"items\": [{\"id\": \"ad-1\", \"advertiser\": \"Nike\", \"headline\": \"Just do it\",~
                \"body\": \"body text of the ad\", \"platforms\": [\"FACEBOOK\", \"INSTAGRAM\"],~
                \"is_active\": true, \"media\": [{\"type\": \"image\", \"url\": \"https://cdn.test/1.jpg\"}]}],~
                \"pagination\": {\"next_page_token\": \"tok-2\"}}")
  "A search answer in the envelope the plugin's docs describe.")

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "adextract" "ADEXTRACT-CELL-" :start nodecode-adextract:start-cell)

(define-cell-lifecycle-tests "adextract"
  (:fixture with-adextract-wire)
  (:config "api_key" "adx-test")
  (:help :adextract)
  (:command "adextract")
  (:running
   (is (nle::find-tool "adextract_search") "search is registered")
   (is (nle::find-tool "adextract_advertisers") "and advertisers")
   (is (nle::find-tool "adextract_advertiser_info") "and advertiser info")
   (is (nle::find-tool "adextract_ad") "and the single ad")
   (is (nle::find-tool "adextract_read_creative") "and creative analysis")
   (is (= 5 (count-if (lambda (entry) (eql 0 (search "adextract_" (first entry))))
                      nle::*tools*)) "five of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "adextract_search")) "and stop takes every one back")
   (is (null (nle::find-tool "adextract_read_creative"))))
  (:refused ("max_items" 0)
            ("timeout_seconds" 0)
            ("max_chars" 10)))

(deftest adextract-cell-a-start-with-no-key-refuses-and-installs-nothing (with-adextract-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) nil)
    (let ((text (refusal-text nlk:config-refusal (adextract-start))))
      (is-carrying text "needs a key" "ADEXTRACT_API_KEY")
      (is (null (nle::find-tool "adextract_search")) "nothing was installed"))))

(deftest adextract-cell-the-key-may-come-from-the-environment (with-adextract-wire)
  (with-stubbed-fdefinition (uiop:getenv (name) (and (equal name "ADEXTRACT_API_KEY") "adx-env"))
    (with-cell-stop ((adextract-start))
      (adextract-answer "{}")
      (adextract-call "adextract_read_creative" "{\"url\": \"https://cdn.test/1.jpg\"}")
      (let ((sent (adextract-request-header (first (adextract-asked)) "authorization")))
        (is (equal "Bearer adx-env" sent) "the exported key is the one sent")))))

;;; --- the exchange, and the search ------------------------------------------------

(deftest adextract-cell-search-handshakes-then-calls-the-library-s-tool (with-adextract-wire)
  (with-cell-stop ((adextract-start "api_key" "adx-test"))
    (adextract-answer +meta-envelope+)
    (let ((answer (adextract-call "adextract_search"
                                  "{\"platform\": \"meta\", \"q\": \"nike\", \"country\": \"US\"}")))
      (let ((requests (adextract-asked)))
        (is (equal '("initialize" "notifications/initialized" "tools/call")
                   (mapcar #'adextract-method requests)) "one call is initialize, initialized, tools/call")
        (is (equal "https://mcp.adextract.co/mcp" (second (first requests))) "to the plugin's endpoint")
        (is (equal "Bearer adx-test" (adextract-request-header (first requests) "authorization")) "with the section's key")
        (is (equal "session-1" (adextract-request-header (third requests) "mcp-session-id")) "and the session id initialize issued"))
      (is (equal "search_meta_ads" (nlk:json-value (adextract-params (first (adextract-call-requests)))
                                                     :text "name")) "meta is the Meta Ad Library's own tool")
      (let ((arguments (adextract-arguments (first (adextract-call-requests)))))
        (is (equal "nike" (nlk:json-value arguments :text "q")) "carrying the query")
        (is (equal "US" (nlk:json-value arguments :text "country")) "and the market")
        (is (null (nlk:json-value arguments :text "media_type")) "and nothing the call left out"))
      (is (search "meta_ad_library: 1 ad in this answer, 482 reported in total" answer) "the answer says which library, and how much there was")
      (is (search "1. Nike - Just do it" answer) "then the ad")
      (is (search "body text of the ad" answer) "its own words")
      (is (search "FACEBOOK, INSTAGRAM" answer) "where it ran")
      (is (search "active" answer) "whether it runs")
      (is (search "https://cdn.test/1.jpg" answer) "and the creative's url")
      (is (search "next page: tok-2" answer) "the next page is offered, not silently fetched"))))

(deftest adextract-cell-an-extra-member-goes-through-as-the-server-names-it (with-adextract-wire)
  (with-cell-stop ((adextract-start "api_key" "adx-test"))
    (adextract-answer +meta-envelope+)
    (adextract-call "adextract_search"
                     "{\"platform\": \"meta\", \"q\": \"nike\", \"extra\": {\"sort_by\": \"impressions_high_to_low\"}}")
    (let ((arguments (adextract-arguments (first (adextract-call-requests)))))
      (is (equal "impressions_high_to_low" (nlk:json-value arguments :text "sort_by")) "extra's member reaches the server unchanged"))))

(deftest adextract-cell-google-searches-by-advertiser-and-needs-one (with-adextract-wire)
  (with-cell-stop ((adextract-start "api_key" "adx-test"))
    (adextract-answer +meta-envelope+)
    (let ((missing (adextract-call "adextract_search"
                                   "{\"platform\": \"google\", \"q\": \"nike\"}")))
      (is (search "ERROR: " missing) "a google search with no advertiser is a failure")
      (is (search "advertiser" missing) "that says to name one")
      (is (null (adextract-call-requests)) "and sends nothing"))
    (let ((answer (adextract-call "adextract_search"
                                  "{\"platform\": \"google\", \"advertiser\": \"nike.com\"}")))
      (is (search "1. Nike" answer) "a domain searches the Transparency Center")
      (let* ((request (first (adextract-call-requests)))
             (arguments (adextract-arguments request)))
        (is (equal "search_google_ads" (nlk:json-value (adextract-params request) :text "name")) "the Google tool")
        (is (equal "nike.com" (nlk:json-value arguments :text "domain")) "by domain")))))

(deftest adextract-cell-a-refused-key-is-the-model-s-repair (with-adextract-wire)
  (with-cell-stop ((adextract-start "api_key" "adx-test"))
    (setf *reply*
          (lambda (method url args)
            (declare (ignore method url args))
            (values "{\"jsonrpc\": \"2.0\", \"id\": 3, \"error\": {\"code\": -32001, \"message\": \"unauthorized: the API key was refused\"}}"
                    401 (adextract-headers))))
    (let ((answer (adextract-call "adextract_advertisers" "{\"platform\": \"tiktok\", \"q\": \"nike\"}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "unauthorized" answer) "and carries the server's own words"))))

;;; --- the rest of the tools ---------------------------------------------------------

(deftest adextract-cell-each-tool-calls-the-tool-it-wraps (with-adextract-wire)
  (with-cell-stop ((adextract-start "api_key" "adx-test"))
    (adextract-answer "{\"advertiser\": \"Nike\"}")
    (adextract-call "adextract_advertisers" "{\"platform\": \"meta\", \"q\": \"nike\"}")
    (adextract-call "adextract_advertisers" "{\"platform\": \"google\", \"q\": \"nike\"}")
    (adextract-call "adextract_advertisers" "{\"platform\": \"tiktok\", \"q\": \"nike\"}")
    (adextract-call "adextract_advertiser_info" "{\"platform\": \"meta\", \"advertiser\": \"12345\"}")
    (adextract-call "adextract_advertiser_info" "{\"platform\": \"google\", \"advertiser\": \"AR998\"}")
    (adextract-call "adextract_ad" "{\"platform\": \"meta\", \"ad\": \"987654\"}")
    (adextract-call "adextract_ad" "{\"platform\": \"google\", \"ad\": \"CR7\", \"advertiser\": \"AR998\"}")
    (adextract-call "adextract_ad" "{\"platform\": \"tiktok\", \"ad\": \"tok-9\"}")
    (adextract-call "adextract_read_creative" "{\"url\": \"https://cdn.test/1.jpg\"}")
    (is (equal '("search_meta_pages" "search_google_ads_advertisers" "search_tiktok_advertisers"
                   "get_meta_page_info" "get_google_ads_advertiser_info"
                   "get_meta_ad_details" "get_google_ad_details" "get_tiktok_ad_details"
                   "ad_reader")
                 (mapcar (lambda (request)
                           (nlk:json-value (adextract-params request) :text "name"))
                         (adextract-call-requests))) "every tool reaches the server's own tool for its platform")))

(deftest adextract-cell-a-meta-ad-id-is-sent-as-the-id-it-looks-like (with-adextract-wire)
  (with-cell-stop ((adextract-start "api_key" "adx-test"))
    (adextract-answer "{}")
    (adextract-call "adextract_ad" "{\"platform\": \"meta\", \"ad\": \"1234567890\"}")
    (adextract-call "adextract_ad" "{\"platform\": \"meta\", \"ad\": \"aGVsbG8tdG9rZW4\"}")
    (let ((numeric (adextract-arguments (first (adextract-call-requests))))
          (token (adextract-arguments (second (adextract-call-requests)))))
      (is (equal "1234567890" (nlk:json-value numeric :text "ad_archive_id")) "a bare number is an archive id")
      (is (equal "aGVsbG8tdG9rZW4" (nlk:json-value token :text "ad_details_token")) "anything else is an ad details token"))))

(deftest adextract-cell-a-tool-that-takes-an-object-answers-its-members (with-adextract-wire)
  (with-cell-stop ((adextract-start "api_key" "adx-test"))
    (adextract-answer "{\"format\": {\"type\": \"video\"}, \"emotion\": \"warm\", \"archetypes\": [\"hero\"]}")
    (let ((answer (adextract-call "adextract_read_creative" "{\"url\": \"https://cdn.test/1.mp4\"}")))
      (is (search "format: " answer) "an object is laid out by member")
      (is (search "emotion: warm" answer) "so the analysis reads as lines")
      (is (search "1. hero" answer) "and an array of strings as a list"))))

;;; --- the operator's command ---------------------------------------------------------

(deftest adextract-cell-the-command-searches-or-says-what-it-is-connected-to (with-adextract-wire)
  (with-cell-stop ((adextract-start "api_key" "adx-test"))
    (let ((status (nodecode-adextract::run-command "" nil)))
      (is (search "https://mcp.adextract.co/mcp" status) "no argument reports the connection")
      (is (search "key set" status) "and that a key is there")
      (is (null (search "adx-test" status)) "without ever printing the key")
      (is (null (adextract-asked)) "and sends nothing"))
    (adextract-answer +meta-envelope+)
    (let ((answer (nodecode-adextract::run-command "  meta  nike  " nil)))
      (is (search "1. Nike - Just do it" answer) "a platform and a query run one search")
      (let ((arguments (adextract-arguments (first (adextract-call-requests)))))
        (is (equal "nike" (nlk:json-value arguments :text "q")) "the query trimmed, as typed")))
    (let ((usage (nodecode-adextract::run-command "nike" nil)))
      (is (search "usage: /adextract" usage) "a query with no platform says how to give one"))))
