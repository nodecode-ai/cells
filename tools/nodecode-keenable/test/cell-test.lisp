;;;; cell-test.lisp --- lifecycle, the two tools, the wire, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Keenable: the config
;;;; surface, the two tools through the real dispatch seam against a stubbed
;;;; server, the JSON-RPC message each call posts, the text a model reads,
;;;; the event-stream form of an answer, and the refusals.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "keenable"
  (:config)
  (:help :keenable)
  (:command "keenable")
  (:running
   (is (nle::find-tool "keenable_search") "keenable_search is registered")
   (is (nle::find-tool "keenable_fetch") "keenable_fetch is registered")
   (is (equal "https://api.keenable.ai/mcp" (nodecode-keenable::setting :endpoint)) "the endpoint the section defaults to"))
  (:stopped
   (is (null (nle::find-tool "keenable_search")) "stop unregisters the search tool")
   (is (null (nle::find-tool "keenable_fetch")) "and the fetch tool"))
  (:refused
   ("timeout_seconds" 0)
   ("max_results" 0)))

(deftest keenable-cell-no-key-is-a-working-shape (with-keenable ()
  ;; The original worked with no key and no account: a section that names
  ;; nothing starts, and its calls carry no key header.
  (is (null (nodecode-keenable::configured-key)) "no key, by design")
  (keenable-call-tool "keenable_search" "{\"query\":\"keenable\"}")
  (is (null (header (first *calls*) "x-api-key")) "and none is sent")))

;;; --- the search tool ---------------------------------------------------------------

(deftest keenable-cell-the-search-tool-posts-one-tools-call (with-keenable ()
  (let ((text (keenable-call-tool "keenable_search" "{\"query\":\"keenable rate limits\"}")))
    (is (cl:search "Title: Keenable MCP server" text) "the server's text, as a model reads it")
    (is (cl:search "https://docs.keenable.ai/rate-limits" text) "every result it carried")
    (is (cl:search "1,000 requests an hour" text) "with its snippets"))
  (is (= 1 (length *calls*)) "one POST, not one a result")
  (let ((call (first *calls*)))
    (is (equal "tools/call" (request-method call)) "a tools/call")
    (is (equal "search_web_pages" (request-tool call)) "of the server's search tool")
    (is (equal "keenable rate limits" (argument call "query")) "carrying the query")
    (is (equal 10 (argument call "max_results")) "and the section's count when the call names none")
    (is (null (argument call "site")) "an unset member is left out, never sent empty")
    (is (equal "nodecode-cell" (header call "x-keenable-title")) "the static client label")
    (is (equal "https://api.keenable.ai/mcp" (getf call :url)) "to the section's endpoint")
    nil)))

(deftest keenable-cell-a-call-s-own-members-ride-the-request (with-keenable ()
  (keenable-call-tool
   "keenable_search"
   "{\"query\":\"release notes\",\"site\":\"docs.python.org\",\"published_after\":\"2026-10-01\",\"published_before\":\"2026-11-01\",\"max_results\":5,\"snippet_max_length\":400,\"mode\":\"realtime\"}")
  (let ((call (first *calls*)))
    (is (equal "docs.python.org" (argument call "site")))
    (is (equal "2026-10-01" (argument call "published_after")) "the recency filter")
    (is (equal "2026-11-01" (argument call "published_before")))
    (is (equal 5 (argument call "max_results")))
    (is (equal 400 (argument call "snippet_max_length")))
    (is (equal "realtime" (argument call "mode")) "a call's own mode")
    nil)))

(deftest keenable-cell-a-count-past-the-ceiling-is-clamped (with-keenable ()
  (keenable-call-tool "keenable_search" "{\"query\":\"anything\",\"max_results\":500}")
  (is (equal 50 (argument (first *calls*) "max_results")) "1 to 50, as the server takes")))

;;; --- the fetch tool ----------------------------------------------------------------

(deftest keenable-cell-the-fetch-tool-reads-one-page (with-keenable ()
  (let ((text (keenable-call-tool
               "keenable_fetch"
               "{\"url\":\"https://docs.keenable.ai/rate-limits\",\"prompt\":\"What are the unauthenticated rate limits?\",\"max_chars\":2000}")))
    (is (cl:search "1,000 requests an hour" text) "the answer the server read out of the page"))
  (let ((call (first *calls*)))
    (is (equal "fetch_page_content" (request-tool call)) "the server's fetch tool")
    (is (equal "https://docs.keenable.ai/rate-limits" (argument call "url")))
    (is (cl:search "unauthenticated" (argument call "prompt")) "the prompt rides with it")
    (is (equal 2000 (argument call "max_chars")) "and the cap")
    nil)))

(deftest keenable-cell-a-fetch-with-no-http-url-never-reaches-the-wire (with-keenable ()
  (let ((condition (signals-error nodecode-keenable:keenable-error
                                  (nodecode-keenable:fetch "docs.keenable.ai"))))
    (is (cl:search "http(s) url" (princ-to-string condition))))
  (is (null *calls*) "nothing was posted")))

(deftest keenable-cell-a-search-with-no-query-never-reaches-the-wire (with-keenable ()
  (let ((condition (signals-error nodecode-keenable:keenable-error
                                  (nodecode-keenable:search "   "))))
    (is (cl:search "needs a query" (princ-to-string condition))))
  (is (null *calls*) "nothing was posted")))

;;; --- what the server says when it refuses -------------------------------------------

(deftest keenable-cell-a-refused-argument-carries-the-server-s-sentence (with-keenable ()
  (setf *answer* (lambda (message)
                   (declare (ignore message))
                   (values +refusal+ 200)))
  (let ((condition (signals-error nodecode-keenable:keenable-error
                                  (nodecode-keenable:search "anything at all"))))
    (is (cl:search "Unknown parameter(s): numResults" (princ-to-string condition)) "the server's own sentence")
    (is (cl:search "-32602" (princ-to-string condition)) "and its code")
    nil)))

(deftest keenable-cell-a-rate-limit-names-the-status (with-keenable ()
  (setf *answer* (lambda (message)
                   (declare (ignore message))
                   (values "{\"message\":\"rate limit exceeded\"}" 429)))
  (let ((condition (signals-error nodecode-keenable:keenable-error
                                  (nodecode-keenable:search "anything at all"))))
    (is (cl:search "HTTP 429" (princ-to-string condition)) "the status")
    (is (cl:search "rate limit exceeded" (princ-to-string condition)) "and what the server said")
    nil)))

(deftest keenable-cell-an-event-stream-answer-is-read (with-keenable ()
  ;; Streamable HTTP lets a server answer a POST with one event instead of a
  ;; JSON body; the message inside it is the same one.
  (setf *answer*
        (lambda (message)
          (declare (ignore message))
          (values (format nil "event: message~%data: ~a~2%" (content-answer +page-text+))
                  200)))
  (is (cl:search "1,000 requests an hour"
                 (keenable-call-tool "keenable_fetch" "{\"url\":\"https://example.test/a\"}")) "the event's message, read as the answer")))

(deftest keenable-cell-a-server-wanting-an-initialize-is-handshaked-once (with-keenable ()
  (let ((asked nil))
    (setf *answer*
          (lambda (message)
            (let ((method (nlk:json-value message :string "method")))
              (cond
                ((equal "initialize" method)
                 (setf asked t)
                 (values (nlk:encode-json-object
                          (nlk:json-object "jsonrpc" "2.0" "id" 1
                                           "result" (nlk:json-object "protocolVersion" "2025-06-18")))
                         200))
                ((equal "notifications/initialized" method) (values "" 202))
                (asked (values (content-answer +search-text+) 200))
                (t (values "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32002,\"message\":\"initialize first\"}}"
                           200)))))))
  (is (cl:search "Title: Keenable MCP server"
                 (keenable-call-tool "keenable_search" "{\"query\":\"keenable\"}")) "the call answers after the handshake")
  (is (= 4 (length *calls*)) "the refused call, initialize, initialized, the call again")))

;;; --- the command ------------------------------------------------------------------

(deftest keenable-cell-the-command-searches-and-explains-itself (with-keenable ()
  (is (cl:search "Keenable MCP server"
                  (cell-entry "nodecode-keenable" "keenable" "keenable rate limits" "keenable-cell-1")) "/keenable searches")
  (let ((bare (cell-entry "nodecode-keenable" "keenable" "" "keenable-cell-1")))
    (is (cl:search "keenable_search" bare) "with no query it names the tools")
    (is (cl:search "10 results" bare) "and the count a bare call asks for")
    (is (cl:search "no key is needed" (string-downcase bare)) "and says no key is needed")
    nil)))

