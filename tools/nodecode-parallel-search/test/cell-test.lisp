;;;; cell-test.lisp --- lifecycle, the two tools, /parallel, and every refusal.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not Parallel: the config
;;;; surface (an address that is not an address refuses the start), each tool
;;;; through the real dispatch seam against a stubbed server, the session the
;;;; client claims and reuses, an answer framed as an event stream, and the
;;;; refusal a rate-limited free tier gets.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "parallel-search"
  (:fixture with-wire)
  (:help :parallel)
  (:command "parallel")
  (:running
   (is (nle::find-tool "web_search") "web_search is registered")
   (is (nle::find-tool "web_fetch") "web_fetch is registered")
   (is (equal "https://search.parallel.ai/mcp" (nodecode-parallel-search::endpoint))
       "with no section it dials Parallel's own server"))
  (:stopped
   (is (null (nle::find-tool "web_search")) "stop unregisters web_search")
   (is (null (nle::find-tool "web_fetch")) "stop unregisters web_fetch"))
  (:refused
   ("endpoint" "search.parallel.ai")
   ("timeout_seconds" 1)
   ("max_results" 0)))

;;; --- search ---------------------------------------------------------------------

(deftest parallel-search-cell-search-hands-back-the-server-s-answer (with-parallel ()
    (let ((text (parallel-call-tool "web_search" "{\"objective\":\"find the FastAPI response model docs\",\"search_queries\":[\"fastapi response model\"]}")))
      (is (search "response_model" text) "the excerpt, verbatim")
      (is (search "https://fastapi.tiangolo.com/tutorial/response-model/" text) "with its url"))
    (let* ((call (first (calls-of "tools/call")))
           (params (nlk:json-value (getf call :body) :object "params"))
           (arguments (nlk:json-value params :object "arguments")))
      (is (equal "web_search" (getf call :tool)))
      (is (equal "find the FastAPI response model docs"
                 (nlk:json-value arguments :string "objective")))
      (is (equal "fastapi response model"
                 (aref (nlk:json-value arguments :array "search_queries") 0)))
      (is (= 5 (nlk:json-value arguments :integer "max_results"))
          "the section's max_results answers when the call names none")
      (is (equal "sess-1" (cdr (assoc "mcp-session-id" (getf call :headers) :test #'string-equal)))
          "the session the initialize handed out rides the call"))))

(deftest parallel-search-cell-one-session-serves-every-call (with-parallel ()
    (parallel-call-tool "web_search" "{\"objective\":\"one\"}")
    (parallel-call-tool "web_fetch" "{\"urls\":[\"https://example.test/a\"]}")
    (is (= 1 (length (calls-of "initialize"))) "one handshake, not one per call")
    (is (= 1 (length (calls-of "notifications/initialized"))))
    (is (= 2 (length (calls-of "tools/call"))))
    (dolist (call (calls-of "tools/call"))
      (is (equal "sess-1" (cdr (assoc "mcp-session-id" (getf call :headers) :test #'string-equal)))))))

(deftest parallel-search-cell-a-lowercase-count-is-the-call-s-own (with-parallel ("max_results" 2)
    (parallel-call-tool "web_search" "{\"objective\":\"one\",\"max_results\":7}")
    (let* ((call (first (calls-of "tools/call")))
           (arguments (nlk:json-value (nlk:json-value (getf call :body) :object "params")
                                       :object "arguments")))
      (is (= 7 (nlk:json-value arguments :integer "max_results")) "a call's own count wins"))))

;;; --- fetch ----------------------------------------------------------------------

(deftest parallel-search-cell-fetch-reads-the-pages-a-call-names (with-parallel ()
    (let ((text (parallel-call-tool "web_fetch" "{\"urls\":[\"https://modelcontextprotocol.io/introduction\"],\"objective\":\"how MCP connects agents to tools\"}")))
      (is (search "Model Context Protocol" text))
      (is (search "JSON-RPC" text)))
    (let* ((call (first (calls-of "tools/call")))
           (arguments (nlk:json-value (nlk:json-value (getf call :body) :object "params")
                                       :object "arguments")))
      (is (equal "web_fetch" (getf call :tool)))
      (is (equal "https://modelcontextprotocol.io/introduction"
                 (aref (nlk:json-value arguments :array "urls") 0)))
      (is (equal "how MCP connects agents to tools"
                 (nlk:json-value arguments :string "objective"))))))

(deftest parallel-search-cell-the-eval-verbs-answer-as-the-tools-do (with-parallel ()
    (is (search "response_model" (nodecode-parallel-search:search "fastapi response model")))
    (is (search "Model Context Protocol" (nodecode-parallel-search:fetch "https://modelcontextprotocol.io/introduction")))))

;;; --- the wire -------------------------------------------------------------------

(deftest parallel-search-cell-an-event-stream-answers-as-a-body-does (with-parallel ()
    (script "web_search"
            (lambda (body)
              (values (format nil "event: message~%data: ~a~%~%" (answer-message body +search-text+))
                      200 '(("content-type" . "text/event-stream")))))
    (let ((text (parallel-call-tool "web_search" "{\"objective\":\"fastapi\"}")))
      (is (search "response_model" text) "the data line is the answer"))))

(deftest parallel-search-cell-a-rate-limited-free-tier-says-so (with-parallel ()
    (script "web_search"
            (lambda (body)
              (declare (ignore body))
              (values "{\"error\":{\"message\":\"too many requests\"}}" 429 '())))
    (let ((text (parallel-call-tool "web_search" "{\"objective\":\"fastapi\"}")))
      (is (search "rate limited" text))
      (is (search "HTTP 429" text)))))

(deftest parallel-search-cell-a-refused-call-carries-the-server-s-sentence (with-parallel ()
    (script "web_search"
            (lambda (body)
              (declare (ignore body))
              (values "{\"error\":{\"message\":\"unknown tool\"}}" 500 '())))
    (let ((text (parallel-call-tool "web_search" "{\"objective\":\"fastapi\"}")))
      (is (search "HTTP 500" text))
      (is (search "unknown tool" text)))))

;;; --- the boundary and the command --------------------------------------------------

(deftest parallel-search-cell-a-call-without-its-objective-never-reaches-the-wire (with-parallel ()
    (let ((text (parallel-call-tool "web_search" "{}")))
      (is (search "objective" text) "the refusal names the missing member"))
    (is (null (calls-of "tools/call")) "and nothing was dialled")))

(deftest parallel-search-cell-the-command-searches-and-explains-itself (with-parallel ()
    (is (search "response_model" (nodecode-parallel-search::run-slash "fastapi response model" "s1")))
    (let ((bare (nodecode-parallel-search::run-slash "" "s1")))
      (is (search "web_search" bare) "with no query it names what the cell gives")
      (is (search "web_fetch" bare)))))
