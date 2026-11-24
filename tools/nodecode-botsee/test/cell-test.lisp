;;;; cell-test.lisp --- lifecycle, the two tools, the exchange, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not BotSee's server: the
;;;; config surface (a section naming no key refuses the start), the two tools
;;;; through the real dispatch seam against a stubbed MCP server, the frames a
;;;; call puts on the wire (initialize, initialized, tools/call; the bearer
;;;; header; the session id echoed), the tool list lifted out of tools/list,
;;;; an event-stream answer read, and the refusal a refused key gets.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "botsee"
  (:config "api_key" +key+)
  (:help :botsee)
  (:command "botsee")
  (:running
   (is (nle::find-tool "botsee_tools") "botsee_tools is registered")
   (is (nle::find-tool "botsee_call") "botsee_call is registered")
   (is (equal +key+ (nodecode-botsee::setting :api-key)) "the key the section carries")
   (is (equal "https://botsee.io/mcp" (nodecode-botsee::setting :url)) "and the endpoint it defaults to"))
  (:stopped
   (is (null (nle::find-tool "botsee_call")) "stop unregisters the call tool")
   (is (null nodecode-botsee::*session*) "and drops the session"))
  (:refused
   ("api_key" "" "api_key_env" "UNSET_BOTSEE_KEY")
   ("api_key" +key+ "timeout_seconds" 0)))

(deftest botsee-cell-without-a-key-refuses (with-wire)
  ;; The one setting no call can work without: a section naming no key, over a
  ;; variable that holds none, is refused at start, as the original offered no
  ;; server without MCP_BOTSEE_API_KEY.
  (let ((condition (signals-error nlk:config-refusal
                                  (botsee-start "api_key" ""
                                                "api_key_env" "UNSET_BOTSEE_KEY"))))
    (is (cl:search "no key" (princ-to-string condition)) "the refusal says what is missing")
    (is (cl:search "api_key_env" (princ-to-string condition)) "and names the variable it looked in")
    (is (null (nle::find-tool "botsee_call")) "and installs nothing")
    (is (null *calls*) "nothing was dialled")))

;;; --- the exchange a call makes ---------------------------------------------------

(deftest botsee-cell-the-tool-initializes-then-calls (with-botsee ()
  (let ((text (botsee-call-tool "botsee_call"
                                 "{\"tool\":\"list_runs\",\"arguments\":{\"site_id\":\"s1\"}}")))
    (is (cl:search "3 completed runs" text) "the server's own text arrives as the answer"))
  (is (equal +handshake-and-call+ (call-methods)) "initialize, initialized, then the call")
  (let* ((init (first (reverse *calls*)))
         (params (params-of init)))
    (is (equal "Bearer botsee-test-key" (header init "authorization")) "the section's key rides as a bearer header")
    (is (cl:search "text/event-stream" (or (header init "accept") "")) "and an event-stream answer is accepted")
    (is (equal "2025-06-18" (nlk:json-value params :string "protocolVersion")) "the declared protocol version is asked for")
    (is (equal "nodecode-botsee" (nlk:json-value params :string "clientInfo" "name")) "under this cell's own name"))
  (let ((call (first *calls*)))
    (is (equal "session-1" (header call "mcp-session-id")) "the session the initialize answer issued is echoed")
    (is (equal "list_runs" (nlk:json-value (params-of call) :string "name")) "the call names the tool it was given")
    (let ((arguments (nlk:json-value (params-of call) :object "arguments")))
      (is (equal "s1" (nlk:json-value arguments :string "site_id")) "and its own arguments")))))

(deftest botsee-cell-a-second-call-reuses-the-session (with-botsee ()
  (botsee-call-tool "botsee_call" "{\"tool\":\"list_runs\",\"arguments\":{}}")
  (botsee-call-tool "botsee_call" "{\"tool\":\"list_runs\",\"arguments\":{}}")
  (is (= 1 (count "initialize" (call-methods) :test #'equal)) "one handshake, however many calls")
  (is (= 2 (count "tools/call" (call-methods) :test #'equal)) "and a call each")))

;;; --- the catalogue ---------------------------------------------------------------

(deftest botsee-cell-tools-lists-what-the-server-advertises (with-botsee ()
  (let ((text (botsee-call-tool "botsee_tools" "{}")))
    (is (cl:search "2 tools advertised" text) "the count the server answered")
    (is (cl:search "run_analysis" text) "the first tool's name")
    (is (cl:search "domain (required)" text) "with the argument its schema requires")
    (is (cl:search "dry_run" text) "and the one it does not")
    (is (cl:search "list_runs" text) "the second tool too"))
  (is (equal +handshake-and-list+ (call-methods)) "discovery is tools/list, not a call")))

(deftest botsee-cell-the-catalogue-is-read-once (with-botsee ()
  (botsee-call-tool "botsee_tools" "{}")
  (botsee-call-tool "botsee_tools" "{}")
  (is (= 1 (count "tools/list" (call-methods) :test #'equal)) "the second list answers from the session's copy")
  (botsee-call-tool "botsee_tools" "{\"refresh\":true}")
  (is (= 2 (count "tools/list" (call-methods) :test #'equal)) "and refresh asks the server again")))

;;; --- what can go wrong -----------------------------------------------------------

(deftest botsee-cell-a-failed-call-carries-the-server-s-own-words (with-botsee ()
  (setf *tool-text* "site s9 is not in this account" *tool-failed* t)
  (let ((condition (signals-error nodecode-botsee:botsee-error
                                  (nodecode-botsee:call "list_runs" "{\"site_id\":\"s9\"}"))))
    (is (cl:search "site s9 is not in this account" (princ-to-string condition)) "the server's sentence is the refusal")
    (is (cl:search "list_runs" (princ-to-string condition)) "naming the tool that failed"))))

(deftest botsee-cell-a-refused-key-names-the-setting (with-botsee ()
  (setf *reply* (lambda (method frame)
                  (declare (ignore method frame))
                  (values "{\"error\":\"invalid_token\",\"message\":\"The access token is missing or invalid.\"}"
                          401 '())))
  (let ((condition (signals-error nodecode-botsee:botsee-error (nodecode-botsee:tools))))
    (is (cl:search "HTTP 401" (princ-to-string condition)) "the status is named")
    (is (cl:search "The access token is missing or invalid." (princ-to-string condition)) "and the server's own reason")
    (is (cl:search "botsee.api_key" (princ-to-string condition)) "and the setting to check"))))

(deftest botsee-cell-an-event-stream-answer-is-read (with-botsee ()
  (setf *reply*
        (lambda (method frame)
          (if (equal method "tools/call")
              (values (format nil "event: message~%data: ~a~%~%event: message~%data: ~a~%~%"
                              (nlk:encode-json-object (nlk:json-object
                                                       "jsonrpc" "2.0"
                                                       "method" "notifications/message"))
                              (reply-json (nlk:json-value frame :integer "id")
                                          (nlk:json-object
                                           "content" (vector (nlk:json-object
                                                               "type" "text"
                                                               "text" "answered over the stream"))
                                           "isError" nil)))
                      200 '(("content-type" . "text/event-stream")))
              (canned method (nlk:json-value frame :integer "id")))))
  (let ((text (botsee-call-tool "botsee_call" "{\"tool\":\"list_runs\",\"arguments\":{}}")))
    (is (cl:search "answered over the stream" text) "the event carrying the answer is the one read"))))

;;; --- the boundary and the command --------------------------------------------------

(deftest botsee-cell-a-call-with-no-tool-never-reaches-the-wire (with-botsee ()
  (let ((condition (signals-error nodecode-botsee:botsee-error (nodecode-botsee:call "  "))))
    (is (cl:search "needs a tool name" (princ-to-string condition))))
  (let ((condition (signals-error nodecode-botsee:botsee-error
                                  (nodecode-botsee:call "list_runs" "not json"))))
    (is (cl:search "one JSON object" (princ-to-string condition)) "arguments that are not an object are refused"))
  (is (null *calls*) "and nothing was dialled")))

(deftest botsee-cell-the-command-lists-and-explains-itself (with-botsee ()
  (is (cl:search "run_analysis"
                 (cell-entry "nodecode-botsee" "botsee" "tools" "botsee-cell-1")) "/botsee tools lists the catalogue")
  (let ((bare (cell-entry "nodecode-botsee" "botsee" "" "botsee-cell-1")))
    (is (cl:search "botsee_tools" bare) "with no argument it names what the cell gives")
    (is (cl:search "dry_run" bare) "and the paid/free line an operator needs"))
  (let ((answer (cell-entry "nodecode-botsee" "botsee"
                            "list_runs {\"site_id\": \"s1\"}" "botsee-cell-1")))
    (is (cl:search "3 completed runs" answer) "/botsee <tool> [json] calls one"))))

