;;;; cell-test.lisp --- lifecycle, the JSON-RPC that goes out, the answers read.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not LovedOne: the config
;;;; surface (a section naming no key refuses the start), every tool through
;;;; the real dispatch seam against a stubbed server, the handshake and the
;;;; session header the transport wants, the arguments a call sends, the text
;;;; its answer is cleaned into, and the refusals a refused key and a
;;;; server-side error get.

(in-package #:nodecode.test)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "lovedone"
  (:config "api_key" +key+)
  (:help :lovedone)
  (:command "lovedone")
  (:running
   (is (nle::find-tool "lovedone_list_inbox_messages") "the inbox tool is registered")
   (is (nle::find-tool "lovedone_call") "and the escape hatch")
   (is (equal +key+ (nodecode-lovedone::setting :api-key)) "the key the section carries"))
  (:stopped
   (is (null (nle::find-tool "lovedone_list_inbox_messages")) "stop unregisters the tools")
   (is (null (nle::find-tool "lovedone_call")) "every one of them"))
  (:refused
   ("api_key" "" "api_key_env" "UNSET_LOVEDONE_KEY")
   ("api_key" +key+ "timeout_seconds" 0)
   ("api_key" +key+ "url" 42)))

(deftest lovedone-cell-without-a-key-refuses ()
  ;; The one setting no call can work without: a section naming no key, over a
  ;; variable that holds none, is refused at start, as the original offered no
  ;; tool without LOVEDONE_API_KEY.
  (with-wire
    (let ((condition (signals-error nlk:config-refusal
                                    (lovedone-start "api_key" ""
                                                     "api_key_env" "UNSET_LOVEDONE_KEY"))))
      (is (cl:search "no key" (princ-to-string condition)) "the refusal says what is missing")
      (is (null (nle::find-tool "lovedone_ping")) "and installs nothing"))))

;;; --- the wire: what a call sends --------------------------------------------------

(deftest lovedone-cell-the-tool-asks-the-server (with-lovedone)
  (let ((text (lovedone-call-tool "lovedone_list_loved_ones" "{}")))
    (is (cl:search "ran list_loved_ones" text) "the server's own text comes back"))
  (let ((handshake (request-method (getf (first (requests)) :body))))
    (is (equal "initialize" handshake) "the transport handshakes before any tool runs"))
  (let ((call (calls-of)))
    (is (cl:search "api.lovedone.app/mcp/v1" (getf call :url)) "to the Family Inbox endpoint")
    (let ((bearer (format nil "Bearer ~a" +key+)))
      (is (equal bearer (header call "authorization")) "the section's key rides as a bearer token"))
    (is (equal "session-1" (header call "mcp-session-id")) "and the session the handshake named")
    (is (equal "list_loved_ones" (called-tool call)) "the server's own tool name is what it calls")))

(deftest lovedone-cell-a-call-s-arguments-ride-the-request (with-lovedone)
  (lovedone-call-tool "lovedone_list_inbox_messages"
                       "{\"loved_one_id\":\"lo_1\",\"limit\":5}")
  (let* ((call (calls-of))
         (arguments (sent-arguments call)))
    (is (equal "list_inbox_messages" (called-tool call)) "the tool the call named")
    (is (equal "lo_1" (nlk:json-value arguments :string "loved_one_id")) "the loved one it named")
    (is (= 5 (nlk:json-value arguments :integer "limit")) "and the limit it named")))

(deftest lovedone-cell-the-escape-hatch-calls-any-tool (with-lovedone)
  (lovedone-call-tool "lovedone_call" "{\"tool\":\"list_care_events\",\"arguments\":{\"limit\":3}}")
  (let ((call (calls-of)))
    (is (equal "list_care_events" (called-tool call)) "a tool this cell does not list, by name")
    (is (= 3 (nlk:json-value (sent-arguments call) :integer "limit")) "with the arguments handed through")))

;;; --- the text a model reads ----------------------------------------------------------

(deftest lovedone-cell-an-event-stream-answer-is-read (with-lovedone)
  ;; The transport lets the server answer as text/event-stream; the payload is
  ;; the same JSON-RPC object, one `data:' line.
  (setf *answer* (lambda (request)
                   (declare (ignore request))
                   (values (format nil "event: message~%data: ~a~%~%" (result-body "two messages"))
                           200 nil)))
  (let ((text (lovedone-call-tool "lovedone_ping" "{}")))
    (is (cl:search "two messages" text) "an event-framed answer is read, not dropped")))

(deftest lovedone-cell-a-refused-key-carries-the-server-s-sentence (with-lovedone)
  (setf *answer* (lambda (request)
                   (declare (ignore request))
                   (values "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32001,\"message\":\"invalid bearer token\"}}"
                           401 (list (cons "content-type" "application/json")))))
  (let ((condition (signals-error nodecode-lovedone:lovedone-error
                                  (nodecode-lovedone:call "list_loved_ones"))))
    (is (cl:search "HTTP 401" (princ-to-string condition)) "the status is named")
    (is (cl:search "invalid bearer token" (princ-to-string condition)) "and the server's own reason")))

(deftest lovedone-cell-a-server-side-refusal-comes-back-as-error-text (with-lovedone)
  ;; A tool that answered with isError is something the model repairs from, so
  ;; it reads as text with ERROR in front, not as an exception.
  (setf *answer* (lambda (request)
                   (if (equal "tools/call" (request-method request))
                       (values (result-body "loved one not found" :is-error t) 200 nil)
                       (default-answer request))))
  (let ((text (lovedone-call-tool "lovedone_get_inbox_message" "{\"message_id\":\"m_1\"}")))
    (is (cl:search "ERROR: loved one not found" text) "the server's sentence, marked as a failure")))

(deftest lovedone-cell-a-header-table-is-read-too (with-lovedone)
  ;; The image's own HTTP call answers headers as a hash table, not the alist
  ;; the default stub hands back: the session has to survive both.
  (setf *answer* (lambda (request)
                   (if (equal "initialize" (request-method request))
                       (let ((headers (make-hash-table :test #'equal)))
                         (setf (gethash "mcp-session-id" headers) "session-2")
                         (values (lovedone-json-text (nlk:json-object
                                              "jsonrpc" "2.0" "id" 1
                                              "result" (nlk:json-object "protocolVersion" "2025-06-18")))
                                 200 headers))
                       (default-answer request))))
  (lovedone-call-tool "lovedone_ping" "{}")
  (let ((session (header (calls-of) "mcp-session-id")))
    (is (equal "session-2" session) "a session named in a header table rides the next request")))

;;; --- the command --------------------------------------------------------------------

(deftest lovedone-cell-the-command-reads-the-workspace (with-lovedone)
  (let ((text (cell-entry "nodecode-lovedone" "lovedone" "" "s1")))
    (is (cl:search "ran get_user_profile" text) "/lovedone names the person the key belongs to")
    (is (cl:search "ran list_loved_ones" text) "and the loved ones on the workspace"))
  (let ((text (cell-entry "nodecode-lovedone" "lovedone" "lo_1" "s1")))
    (is (cl:search "lo_1" text) "with an id it names that loved one")
    (let ((call (calls-of 2)))
      (is (equal "list_inbox_messages" (called-tool call)) "and reads that loved one's inbox"))))
