;;;; cell-test.lisp --- lifecycle, the four tools and /genesispay, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches the catalog and
;;;; no network is needed. What is proved is the cell's own wiring: the
;;;; section (a healthy start, a setting it cannot honour refused, a config
;;;; that names no catalog refused), the four tools through the seam the model
;;;; calls them by -- their JSON-RPC method, the tool name, the arguments and
;;;; the answer -- the rename the catalog made (recent, once trending), an
;;;; event-stream answer, and the operator's command.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-genesispay-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials the catalog."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected request to the catalog"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun genesispay-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun genesispay-envelope (result)
  "RESULT as the JSON-RPC answer body the catalog sends."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" 1 "result" result)))

(defun genesispay-text-result (text)
  "TEXT as the one content block a tools/call answer carries."
  (nlk:json-object "content" (coerce (list (nlk:json-object "type" "text" "text" text))
                                       'vector)))

(defun genesispay-reply (text &optional (status 200))
  "From now on the stubbed catalog answers the hello, and every tool call with
TEXT as one content block."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url args))
          (values (genesispay-envelope
                   (if (equal "tools/call" (genesispay-method (first *requests*)))
                       (genesispay-text-result text)
                       (nlk:json-object "protocolVersion" "2025-06-18")))
                  status))))

(defun genesispay-refuse (message &optional (status 200))
  "From now on every tool call is refused with JSON-RPC error MESSAGE."
  (setf *reply*
        (lambda (method url args)
          (declare (ignore method url args))
          (values (nlk:encode-json-object
                   (nlk:json-object "jsonrpc" "2.0" "id" 1
                                     "error" (nlk:json-object "code" -32602 "message" message)))
                  status)))
  t)

(defun genesispay-rpc (request)
  "The JSON-RPC object one stubbed request carried."
  (nlk:decode-json (getf (cddr request) :content)))

(defun genesispay-method (request)
  "The JSON-RPC method one stubbed request asked for."
  (nlk:json-value (genesispay-rpc request) :text "method"))

(defun genesispay-params (request)
  "The params object one stubbed request carried."
  (nlk:json-value (genesispay-rpc request) :object "params"))

(defun genesispay-tool-calls ()
  "The stubbed requests that were a tools/call, oldest first."
  (remove-if-not (lambda (request) (equal "tools/call" (genesispay-method request)))
                 (genesispay-asked)))

(defun genesispay-tool-name (request)
  "The tool one stubbed tools/call named."
  (nlk:json-value (genesispay-params request) :string "name"))

(defun genesispay-argument (request name)
  "The argument NAME one stubbed tools/call sent, or NIL."
  (nlk:json-value (nlk:json-value (genesispay-params request) :object "arguments") :any name))

(defun genesispay-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "genesispay-call" name arguments)))

;;; --- lifecycle ------------------------------------------------------------------------

(define-test-slice "genesispay" "GENESISPAY-CELL-" :start nodecode-genesispay:start-cell)

(define-cell-lifecycle-tests "genesispay"
  (:fixture with-genesispay-wire)
  (:config "timeout_seconds" 60)
  (:help :genesispay)
  (:command "genesispay")
  (:running
   (is (nle::find-tool "genesispay_catalog_search") "search is registered")
   (is (nle::find-tool "genesispay_catalog_shops") "and shops")
   (is (nle::find-tool "genesispay_catalog_trending") "and trending")
   (is (nle::find-tool "genesispay_catalog_reviews") "and reviews, the plugin's four"))
  (:stopped
   (is (null (nle::find-tool "genesispay_catalog_search")) "and stop takes every one back")
   (is (null (nle::find-tool "genesispay_catalog_reviews"))))
  (:refused ("timeout_seconds" 0)
            ("max_result_chars" 10)
            ("base_url" "catalog.genesispay.finance")))

;;; --- the hello, and a search -----------------------------------------------------------

(deftest genesispay-cell-a-search-says-hello-then-asks-for-products (with-genesispay-wire)
  (with-cell-stop ((genesispay-start))
    (genesispay-reply "{\"count\": 1, \"products\": [{\"title\": \"Wool Runner\"}]}")
    (let ((answer (genesispay-call "genesispay_catalog_search"
                                    "{\"query\": \"wool runner\", \"limit\": 3, \"currency\": \"USD\"}")))
      (let ((asked (genesispay-asked)))
        (is (equal '("initialize" "notifications/initialized" "tools/call")
                   (mapcar #'genesispay-method asked)) "the hello, its notification, then one tools/call")
        (is (equal "initialize" (genesispay-method (first asked))) "the hello comes first, as MCP asks")
        (is (eql 0 (search "https://catalog.genesispay.finance/mcp/catalog"
                            (second (first asked)))) "and goes to the catalog's own endpoint")
        (is (equal '("content-type" . "application/json")
                   (assoc "content-type" (getf (cddr (first asked)) :headers)
                          :test #'string-equal)) "as JSON in"))
      (let ((call (first (genesispay-tool-calls))))
        (is (equal "genesispay_catalog_search" (genesispay-tool-name call)) "the tool the plugin names")
        (is (equal "wool runner" (genesispay-argument call "query")) "with the words, as typed")
        (is (= 3 (genesispay-argument call "limit")) "the limit asked for")
        (is (equal "USD" (genesispay-argument call "currency")) "and the currency"))
      (is (search "Wool Runner" answer) "the catalog's own text comes back to the model"))))

(deftest genesispay-cell-the-answer-may-come-as-an-event-stream (with-genesispay-wire)
  (with-cell-stop ((genesispay-start))
    (setf *reply*
          (lambda (method url args)
            (declare (ignore method url args))
            (if (equal "tools/call" (genesispay-method (first *requests*)))
                (values (format nil "event: message~%data: ~a~%~%"
                                (genesispay-envelope (genesispay-text-result "a store answered")))
                        200)
                (values (genesispay-envelope
                         (nlk:json-object "protocolVersion" "2025-06-18"))
                        200))))
    (let ((answer (genesispay-call "genesispay_catalog_shops" "{}")))
      (is (search "a store answered" answer) "the data line's payload is the answer"))))

(deftest genesispay-cell-a-search-with-nothing-to-go-on-refuses-before-it-is-sent (with-genesispay-wire)
  (with-cell-stop ((genesispay-start))
    (let ((answer (genesispay-call "genesispay_catalog_search" "{}")))
      (is (search "ERROR: " answer) "the refusal is the tool result")
      (is (search "query" answer) "naming the argument that would have narrowed it")
      (is (null (genesispay-asked)) "and nothing was sent"))))

;;; --- trending: the name the catalog moved on from ------------------------------------------

(deftest genesispay-cell-trending-falls-back-to-the-other-tool-name (with-genesispay-wire)
  (with-cell-stop ((genesispay-start))
    (let ((attempts '()))
      (setf *reply*
            (lambda (method url args)
              (declare (ignore method url args))
              (let ((request (first *requests*)))
                (if (equal "tools/call" (genesispay-method request))
                    (progn
                      (push (genesispay-tool-name request) attempts)
                      (if (= 1 (length attempts))
                          (values (nlk:encode-json-object
                                   (nlk:json-object
                                    "jsonrpc" "2.0" "id" 1
                                    "error" (nlk:json-object
                                             "code" -32602
                                             "message" "Unknown tool: genesispay_catalog_recent")))
                                  200)
                          (values (genesispay-envelope (genesispay-text-result "{\"count\": 2}"))
                                  200)))
                    (values (genesispay-envelope
                             (nlk:json-object "protocolVersion" "2025-06-18"))
                            200)))))
      (let ((answer (genesispay-call "genesispay_catalog_trending" "{\"limit\": 5}")))
        (is (equal '("genesispay_catalog_recent" "genesispay_catalog_trending") (nreverse attempts)) "the newer name first, then the one the plugin documented")
        (is (search "count" answer) "and the second name's answer is what the model reads")))))

;;; --- shops, reviews, and what a refusal says -----------------------------------------------

(deftest genesispay-cell-shops-pass-the-filters-through (with-genesispay-wire)
  (with-cell-stop ((genesispay-start))
    (genesispay-reply "{\"count\": 1, \"shops\": [{\"host\": \"allbirds.com\"}]}")
    (let ((answer (genesispay-call "genesispay_catalog_shops"
                                    "{\"query\": \"shoes\", \"ucpOnly\": true, \"limit\": 2}")))
      (let ((call (first (genesispay-tool-calls))))
        (is (equal "genesispay_catalog_shops" (genesispay-tool-name call)) "the shops tool")
        (is (equal "shoes" (genesispay-argument call "query")) "the words")
        (is (eq t (genesispay-argument call "ucpOnly")) "the boolean, as JSON true")
        (is (null (genesispay-argument call "cursor")) "and no cursor, which was not given"))
      (is (search "allbirds.com" answer) "the store comes back"))))

(deftest genesispay-cell-reviews-send-the-id-and-report-a-server-refusal (with-genesispay-wire)
  (with-cell-stop ((genesispay-start))
    (genesispay-reply "{\"id\": \"prod_allbirds.com_0123456789abcdef\", \"rating\": 4.5}")
    (let ((answer (genesispay-call "genesispay_catalog_reviews"
                                    "{\"id\": \"prod_allbirds.com_0123456789abcdef\"}")))
      (is (equal "prod_allbirds.com_0123456789abcdef"
                 (genesispay-argument (first (genesispay-tool-calls)) "id")) "the id, unchanged")
      (is (search "rating" answer) "and the reviews come back"))
    (setf *requests* '())
    (genesispay-refuse "Unknown product")
    (let ((answer (genesispay-call "genesispay_catalog_reviews" "{\"id\": \"ext_thing\"}")))
      (is (search "ERROR: " answer) "a refusal is one line the model reads")
      (is (search "Unknown product" answer) "carrying the catalog's own words"))))

(deftest genesispay-cell-a-tool-without-its-required-id-refuses (with-genesispay-wire)
  (with-cell-stop ((genesispay-start))
    (let ((answer (genesispay-call "genesispay_catalog_reviews" "{}")))
      (is (search "ERROR: " answer) "an id is required")
      (is (search "id" answer) "and the refusal names it")
      (is (null (genesispay-tool-calls)) "nothing was sent"))))

;;; --- the operator's command -----------------------------------------------------------------

(deftest genesispay-cell-the-command-routes-words-to-the-right-tool (with-genesispay-wire)
  (with-cell-stop ((genesispay-start))
    (let ((usage (nodecode-genesispay::run-command "" nil)))
      (is (search "/genesispay" usage) "no argument shows the usage")
      (is (null (genesispay-asked)) "and asks the catalog nothing"))
    (let ((status (nodecode-genesispay::run-command "status" nil)))
      (is (search "catalog.genesispay.finance" status) "status names the endpoint")
      (is (search "read-only" status) "and says what it is")
      (is (null (genesispay-asked)) "without a request"))
    (genesispay-reply "{}")
    (nodecode-genesispay::run-command "trending" nil)
    (is (equal "genesispay_catalog_recent" (genesispay-tool-name (first (genesispay-tool-calls)))) "trending is the newest products")
    (setf *requests* '())
    (genesispay-reply "{}")
    (nodecode-genesispay::run-command "shops shoes" nil)
    (is (equal "genesispay_catalog_shops" (genesispay-tool-name (first (genesispay-tool-calls)))) "shops is the stores")
    (is (equal "shoes" (genesispay-argument (first (genesispay-tool-calls)) "query")) "with the words after it")
    (setf *requests* '())
    (genesispay-reply "{}")
    (nodecode-genesispay::run-command "wool runner" nil)
    (is (equal "genesispay_catalog_search" (genesispay-tool-name (first (genesispay-tool-calls)))) "anything else is a search")
    (is (equal "wool runner" (genesispay-argument (first (genesispay-tool-calls)) "query")) "of the words as typed")))
