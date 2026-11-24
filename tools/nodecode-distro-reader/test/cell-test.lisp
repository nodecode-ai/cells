;;;; cell-test.lisp --- lifecycle, the eight tools, and /distro-reader, against a stubbed wire.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; Every request is a stubbed NLK:HTTP, so nothing reaches Distro, needs an
;;;; account or touches the network. What is proved is the cell's own wiring:
;;;; the section (a healthy start, an endpoint that is not an address, values
;;;; outside their range refused), the eight tools through the seam the model
;;;; calls them by -- their JSON-RPC method, name, arguments and answer -- and
;;;; the operator's three command forms.

(in-package #:nodecode.test)

;;; --- fixtures -----------------------------------------------------------------

(defvar *requests* '()
  "Every stubbed request of the test, (METHOD URL . ARGS), newest first.")

(defvar *reply* nil
  "What the stubbed NLK:HTTP answers: a function of METHOD, URL, ARGS => (values TEXT STATUS).")

(defmacro with-distro-wire (&body body)
  "BODY with NLK:HTTP stubbed: a request is logged in *REQUESTS* and answered
by *REPLY*, so no test dials the reader."
  `(let ((*requests* '())
         (*reply* (lambda (method url args)
                    (declare (ignore method url args))
                    (error "unexpected distro request"))))
     (with-stubbed-fdefinition (nlk:http (method url &rest args)
                                (push (list* method url args) *requests*)
                                (funcall *reply* method url args))
       ,@body)))

(defun distro-asked ()
  "The stubbed requests, oldest first."
  (reverse *requests*))

(defun distro-reply (text &optional (status 200))
  (setf *reply* (lambda (method url args)
                  (declare (ignore method url args))
                  (values text status))))

(defun distro-answer (text &key error-p)
  "A JSON-RPC tools/call answer carrying TEXT as its one content part."
  (nlk:encode-json-object
   (nlk:json-object "jsonrpc" "2.0" "id" 1
                    "result" (nlk:json-object
                              "content" (vector (nlk:json-object "type" "text" "text" text))
                              "isError" (if error-p t :false)))))

(defun distro-payload (request)
  "REQUEST's JSON-RPC body, decoded."
  (nlk:decode-json (getf (cddr request) :content)))

(defun distro-method (request)
  (nlk:json-value (distro-payload request) :text "method"))

(defun distro-params (request)
  (nlk:json-value (distro-payload request) :object "params"))

(defun distro-arguments (request)
  "The tool call's arguments, decoded."
  (nlk:json-value (distro-params request) :object "arguments"))

(defun distro-request-header (request name)
  "REQUEST's header NAME, case-insensitively."
  (cdr (assoc name (getf (cddr request) :headers) :test #'string-equal)))

(defun distro-call (name arguments)
  "NAME called the way the model calls it, as the text the model reads back."
  (nle::message-content (execute-wire-call "distro-call" name arguments)))

;;; --- lifecycle -----------------------------------------------------------------

(define-test-slice "distro-reader" "DISTRO-READER-CELL-" :start nodecode-distro-reader:start-cell)

(define-cell-lifecycle-tests "distro-reader"
  (:config)
  (:help :distro-reader)
  (:command "distro-reader")
  (:running
   (is (nle::find-tool "distro_feed_latest") "the feed is registered")
   (is (nle::find-tool "distro_search") "and the search")
   (is (nle::find-tool "distro_story_read") "and the reader")
   (is (nle::find-tool "distro_media_series") "and the media series")
   (is (nle::find-tool "distro_story_render_hud") "and the story render")
   (is (nle::find-tool "distro_book_render_hud") "and the book render")
   (is (nle::find-tool "distro_payment_info") "and the payment information")
   (is (nle::find-tool "distro_tip") "and the tip, the original's eight")
   (is (= 8 (count-if (lambda (entry) (eql 0 (search "distro_" (first entry))))
                      nle::*tools*)) "eight of them, and no others"))
  (:stopped
   (is (null (nle::find-tool "distro_search")) "and stop takes every one back")
   (is (null (nle::find-tool "distro_tip"))))
  (:refused ("timeout_seconds" 0)
            ("page_limit" 0)
            ("html_delivery" "inline-please")))

(deftest distro-reader-cell-an-endpoint-that-is-no-address-refuses (with-distro-wire)
  (let ((text (refusal-text nlk:config-refusal (distro-reader-start "endpoint" "reader.distro.media"))))
    (is-carrying text "endpoint" "https://")
    (is (null (nle::find-tool "distro_search")) "nothing was installed"))
  (let ((text (refusal-text nlk:config-refusal (distro-reader-start "endpoint" "ftp://mirror.test/mcp"))))
    (is-carrying text "endpoint" "must be" "http(s)")))

(deftest distro-reader-cell-the-endpoint-may-be-a-mirror (with-distro-wire)
  (with-cell-stop ((distro-reader-start "endpoint" "https://mirror.test/mcp/reader/stream/"))
    (distro-reply (distro-answer "hi"))
    (distro-call "distro_search" "{\"query\": \"x\"}")
    (is (equal "https://mirror.test/mcp/reader/stream" (second (first (distro-asked)))) "the section's endpoint, without its trailing slash")))

;;; --- the transport ---------------------------------------------------------------

(deftest distro-reader-cell-a-call-is-one-json-rpc-post (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "hello from distro"))
    (let ((answer (distro-call "distro_feed_latest" "{\"limit\": 3}")))
      (let ((request (first (distro-asked))))
        (is (eq :post (first request)) "one POST")
        (is (equal "https://reader.distro.media/mcp/reader/stream" (second request)) "to the reader's own endpoint")
        (is (equal "application/json, text/event-stream" (distro-request-header request "accept")) "offering the stream MCP may answer with")
        (is (equal "2025-06-18" (distro-request-header request "mcp-protocol-version")) "and the protocol revision")
        (is (equal "tools/call" (distro-method request)) "a tools/call")
        (is (equal "distro_feed_latest" (nlk:json-value (distro-params request) :text "name")) "naming the server's own tool")
        (is (= 3 (nlk:json-value (distro-arguments request) :integer "limit")) "carrying the call's argument"))
      (is (equal "hello from distro" answer) "and the model reads the server's text"))))

(deftest distro-reader-cell-an-event-stream-answer-is-read (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply (format nil "event: message~%data: ~a~%~%" (distro-answer "streamed")))
    (let ((answer (distro-call "distro_search" "{\"query\": \"x\"}")))
      (is (equal "streamed" answer) "an SSE body decodes like a JSON one"))))

(deftest distro-reader-cell-a-structured-answer-with-no-text-still-reaches-the-model (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply
     (nlk:encode-json-object
      (nlk:json-object "jsonrpc" "2.0" "id" 1
                        "result" (nlk:json-object
                                  "content" (vector)
                                  "structuredContent" (nlk:json-object "stories" (vector))))))
    (let ((answer (distro-call "distro_feed_latest" "{}")))
      (is (search "stories" answer) "the structured answer is what the model reads"))))

(deftest distro-reader-cell-a-refused-answer-is-the-model-s-repair (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32602,\"message\":\"unknown tool\"}}")
    (let ((answer (distro-call "distro_search" "{\"query\": \"x\"}")))
      (is (search "ERROR: " answer) "a failure is the tool result, not a condition up the stack")
      (is (search "unknown tool" answer) "and carries the server's own words"))
    (distro-reply "upstream is down" 503)
    (let ((answer (distro-call "distro_search" "{\"query\": \"x\"}")))
      (is (search "HTTP 503" answer) "a refused status names the status")
      (is (search "upstream is down" answer) "and shows what the reader said"))
    (distro-reply "{}" 429)
    (let ((answer (distro-call "distro_search" "{\"query\": \"x\"}")))
      (is (search "429" answer) "a rate limit is named")
      (is (search "60 requests a minute" answer) "with the reader's own budget"))))

(deftest distro-reader-cell-a-tool-result-the-server-failed-is-not-a-condition (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "no such story" :error-p t))
    (let ((answer (distro-call "distro_story_read" "{\"storyId\": 7}")))
      (is (search "ERROR: no such story" answer) "the server's own failure text, marked"))))

;;; --- feed and search --------------------------------------------------------------

(deftest distro-reader-cell-the-feed-filters-by-publication (with-distro-wire)
  (with-cell-stop ((distro-reader-start "page_limit" 7))
    (distro-reply (distro-answer "# Latest Stories

### A headline
*An author · DeAI News*
A preview.
[Read on Distro](https://deainews.com/a)"))
    (let ((answer (distro-call "distro_feed_latest" "{\"publication\": \"deai-news\"}")))
      (let ((sent (distro-arguments (first (distro-asked)))))
        (is (equal "deai-news" (nlk:json-value sent :text "publication")) "the publication it asked for")
        (is (= 7 (nlk:json-value sent :integer "limit")) "and the section's page_limit when the call names none")
        (is (not (nlk:json-value sent :integer "publicationId")) "no id beside the slug"))
      (is (search "### A headline" answer) "the feed's markdown reaches the model")
      (is (search "https://deainews.com/a" answer) "with its canonical link")))
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "x"))
    (let ((answer (distro-call "distro_feed_latest" "{\"publication\": \"a\", \"publicationId\": 2}")))
      (is (search "not both" answer) "a slug and an id together are refused")
      (is (= 1 (length (distro-asked))) "and nothing more was sent"))))

(deftest distro-reader-cell-the-search-posts-the-query (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "### Found it\n[Read on Distro](https://x.test/1)"))
    (let ((answer (distro-call "distro_search" "{\"query\": \"decentralized ai\", \"limit\": 5}")))
      (let* ((request (first (distro-asked)))
             (sent (distro-arguments request)))
        (is (equal "distro_search" (nlk:json-value (distro-params request) :text "name")) "the search tool")
        (is (equal "decentralized ai" (nlk:json-value sent :text "query")) "carrying the query")
        (is (= 5 (nlk:json-value sent :integer "limit")) "and the count the call named"))
      (is (search "Found it" answer) "the matches reach the model"))))

(deftest distro-reader-cell-a-search-with-no-query-refuses-before-any-request (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (let ((answer (distro-call "distro_search" "{}")))
      (is (search "query" answer) "the refusal names the member the call left out")
      (is (null (distro-asked)) "and nothing was sent"))))

;;; --- reading, rendering, books -------------------------------------------------

(deftest distro-reader-cell-a-story-is-read-by-its-slug (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "# A story

The body."))
    (let ((answer (distro-call "distro_story_read" "{\"slug\": \"deai-news/a-story\"}")))
      (is (equal "deai-news/a-story"
                 (nlk:json-value (distro-arguments (first (distro-asked))) :text "slug")) "the slug pair goes to the server as it was given")
      (is (search "The body." answer) "and the story's text is the answer"))))

(deftest distro-reader-cell-a-premium-story-answers-its-instructions (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "This story is premium: 0.5 USDC on Base. GET the canonical resource to pay."))
    (let ((answer (distro-call "distro_story_read" "{\"storyId\": 42}")))
      (is (search "premium" answer) "the payment instructions are the answer")
      (is (search "USDC on Base" answer) "with the price and the rail named"))))

(deftest distro-reader-cell-a-story-with-no-locator-refuses (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (let ((answer (distro-call "distro_story_read" "{}")))
      (is (search "storyId" answer) "the refusal names the three ways in")
      (is (null (distro-asked)) "and nothing was sent"))))

(deftest distro-reader-cell-a-render-asks-for-a-link-and-is-clipped (with-distro-wire)
  (with-cell-stop ((distro-reader-start "max_chars" 40))
    (distro-reply (distro-answer (make-string 200 :initial-element #\x)))
    (let ((answer (distro-call "distro_story_render_hud" "{\"slug\": \"deai-news/a\"}")))
      (let ((sent (distro-arguments (first (distro-asked)))))
        (is (equal "link" (nlk:json-value sent :text "htmlDelivery")) "link delivery, so a page is a url")
        (is (nlk:json-value sent :boolean "includeSponsor") "with the sponsor block, the server's default"))
      (is (search "more characters" answer) "and a long page is cut to the section's max_chars")))
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "page"))
    (let ((answer (distro-call "distro_story_render_hud"
                               "{\"slug\": \"deai-news/a\", \"htmlDelivery\": \"inline\", \"includeSponsor\": false}")))
      (declare (ignore answer))
      (let ((sent (distro-arguments (first (reverse (distro-asked))))))
        (is (equal "inline" (nlk:json-value sent :text "htmlDelivery")) "the call may ask for the whole page")
        (is (not (nlk:json-value sent :boolean "includeSponsor")) "and may turn the sponsor block off")))))

(deftest distro-reader-cell-a-book-opens-by-its-publication (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "# The book

## Contents
1. One"))
    (let ((answer (distro-call "distro_book_render_hud" "{\"publication\": \"a-book\"}")))
      (is (equal "a-book" (nlk:json-value (distro-arguments (first (distro-asked))) :text "publication")) "the book's slug")
      (is (search "## Contents" answer) "and the table of contents reaches the model"))))

;;; --- money, and the server's own list ------------------------------------------

(deftest distro-reader-cell-payment-information-is-read-only (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "0.5 USDC on Base (x402), resource https://x.test/1"))
    (let ((answer (distro-call "distro_payment_info" "{\"contentId\": 1195}")))
      (let ((sent (distro-arguments (first (distro-asked)))))
        (is (= 1195 (nlk:json-value sent :integer "contentId")) "the content id")
        (is (equal "mcp_agent" (nlk:json-value sent :text "clientType")) "and this client's own tier"))
      (is (search "x402" answer) "the price and rail reach the model")))
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "x"))
    (let ((answer (distro-call "distro_payment_info" "{}")))
      (is (search "contentId" answer) "a call with no content id is refused, naming it")
      (is (= 1 (length (distro-asked))) "before anything is sent"))))

(deftest distro-reader-cell-a-tip-asks-for-the-menu-then-for-an-amount (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "Suggested tips: 0.5, 1, 5 USDC on Base"))
    (let ((answer (distro-call "distro_tip" "{\"storyId\": 1195}")))
      (let ((sent (distro-arguments (first (distro-asked)))))
        (is (= 1195 (nlk:json-value sent :integer "storyId")) "the story to tip on")
        (is (not (nlk:json-value sent :number "amount")) "with no amount, so the menu"))
      (is (search "Suggested tips" answer) "the menu is the answer"))
    (distro-reply (distro-answer "x402 handoff: sign 0.5 USDC on Base"))
    (let ((answer (distro-call "distro_tip" "{\"storyId\": 1195, \"amount\": 0.5}")))
      (is (= 1/2 (nlk:json-value (distro-arguments (first (reverse (distro-asked)))) :number "amount")) "and the amount the user approved")
      (is (search "x402 handoff" answer) "the handoff is the answer")))
  (with-cell-stop ((distro-reader-start))
    (distro-reply (distro-answer "x"))
    (let ((answer (distro-call "distro_tip" "{\"amount\": 1}")))
      (is (search "newslodeSlug" answer) "a tip with no story and no publication is refused"))))

(deftest distro-reader-cell-the-server-s-own-tool-list-is-readable (with-distro-wire)
  (with-cell-stop ((distro-reader-start))
    (distro-reply
     (nlk:encode-json-object
      (nlk:json-object "jsonrpc" "2.0" "id" 1
                        "result" (nlk:json-object
                                  "tools" (vector
                                           (nlk:json-object "name" "distro_search"
                                                             "description" "Search Distro.")
                                           (nlk:json-object "name" "distro_tip"
                                                             "description" "Tip a publisher."))))))
    (let ((answer (nodecode-distro-reader:server-tools)))
      (is (equal "tools/list" (distro-method (first (distro-asked)))) "it is a tools/list")
      (is (search "2 tools" answer) "the count")
      (is (search "distro_search - Search Distro." answer) "and each tool with its own line"))))

;;; --- the operator's command -----------------------------------------------------

(deftest distro-reader-cell-the-command-searches-or-shows-what-it-is (with-distro-wire)
  (with-cell-stop ((distro-reader-start "page_limit" 4))
    (let ((status (nodecode-distro-reader::run-command "" nil)))
      (is (search "https://reader.distro.media/mcp/reader/stream" status) "no argument reports the endpoint")
      (is (search "8 tools" status) "and what it offers")
      (is (null (distro-asked)) "without dialling anything"))
    (distro-reply (distro-answer "### Headline"))
    (let ((answer (nodecode-distro-reader::run-command "  feed deai-news " nil)))
      (is (search "Headline" answer) "feed runs the headlines")
      (let ((sent (distro-arguments (first (distro-asked)))))
        (is (equal "deai-news" (nlk:json-value sent :text "publication")) "of the publication it named")
        (is (= 4 (nlk:json-value sent :integer "limit")) "bounded by the section")))
    (distro-reply (distro-answer "### Match"))
    (let ((answer (nodecode-distro-reader::run-command "decentralized ai" nil)))
      (is (search "Match" answer) "any other text is a search")
      (is (equal "decentralized ai"
                 (nlk:json-value (distro-arguments (first (reverse (distro-asked)))) :text "query")) "with the query as typed, trimmed")))
  (with-cell-stop ((distro-reader-start))
    (distro-reply "{}" 500)
    (let ((answer (nodecode-distro-reader::run-command "x" nil)))
      (is (search "distro-reader: " answer) "a failure is one line for the operator")
      (is (search "HTTP 500" answer) "naming the status, not a stack trace"))))
