;;;; cell-test.lisp --- lifecycle, the two tools, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not TAM: the config surface
;;;; (a section naming no usable url refuses the start), the handshake, the
;;;; arguments each tool sends, the text each one cleans TAM's answer into, and
;;;; the refusal a refused token gets.

(in-package #:nodecode.test)

;;; --- lifecycle -------------------------------------------------------------------

(define-cell-lifecycle-tests "tam"
  (:config "url" +url+)
  (:help :tam)
  (:command "tam")
  (:running
   (is (nle::find-tool "tam_recall") "tam_recall is registered")
   (is (nle::find-tool "tam_save") "tam_save too")
   (is (equal +url+ (nodecode-tam::setting :url)) "the url the section carries")
   (is (= 6 (nodecode-tam::setting :recall-limit)) "recall_limit defaults to six"))
  (:stopped
   (is (null (nle::find-tool "tam_recall")) "stop unregisters the tool"))
  (:refused
   ("url" "")
   ("url" "ftp://127.0.0.1/mcp/")
   ("url" +url+ "recall_limit" 0)))

;;; --- recall ----------------------------------------------------------------------

(deftest tam-cell-recall-hands-the-query-through-the-handshake (with-tam ()
  (let ((text (tam-call-tool "tam_recall" "{\"query\":\"staging port\"}")))
    (is (cl:search "[fact] staging runs on port 8080" text) "the best memory, its type and content")
    (is (cl:search "[lesson] the flaky test" text) "and the next one")
    (is (< (cl:search "[fact]" text) (cl:search "[lesson]" text)) "best first"))
  (is (equal +url+ (first *messages*)) "the endpoint the section names")
  (is (equal +session+ (tam-header "mcp-session-id")) "the call rides the session the handshake opened")
  (is (equal "2025-06-18" (tam-header "mcp-protocol-version")) "and names the revision it speaks")
  (is (equal "session-1" (nodecode-tam::header (list (cons "Mcp-Session-Id" "session-1")) "mcp-session-id")) "a server's header reads however it capitalizes the name")))

(deftest tam-cell-a-recall-carries-the-originals-arguments (with-tam ("recall_limit" 3)
  (nodecode-tam:recall "staging port" :project "ops")
  (let* ((document (first *documents*))
         (params (gethash "params" document))
         (arguments (nlk:json-value params :object "arguments")))
    (is (equal "memory_recall" (nlk:json-value params :string "name")))
    (is (equal "staging port" (nlk:json-value arguments :string "query")))
    (is (= 3 (nlk:json-value arguments :integer "limit")) "the section's limit answers a call that names none")
    (is (equal "ops" (nlk:json-value arguments :string "project")))
    (is (equal "full" (nlk:json-value arguments :string "detail")) "a local server is asked for full content"))))

(deftest tam-cell-an-empty-store-says-so (with-tam ()
  (let ((*recall-payload* (nlk:json-object "results" (nlk:json-object))))
    (is (cl:search "no memory answering" (nodecode-tam:recall "nothing"))))))

;;; --- save ------------------------------------------------------------------------

(deftest tam-cell-save-sends-one-record-and-reports-what-tam-did (with-tam ()
  (let ((text (tam-call-tool "tam_save"
                              "{\"content\":\"staging runs on port 8080\",\"type\":\"fact\",\"tags\":[\"deploy\"]}")))
    (is (cl:search "already in TAM as record 12" text) "a deduplicated save says so"))
  (let* ((document (first *documents*))
         (params (gethash "params" document))
         (arguments (nlk:json-value params :object "arguments")))
    (is (equal "memory_save" (nlk:json-value params :string "name")))
    (is (equal "staging runs on port 8080" (nlk:json-value arguments :string "content")))
    (is (equal "fact" (nlk:json-value arguments :string "type")))
    (is (equal "nodecode" (nlk:json-value arguments :string "project")) "the section's project answers")
    (is (equal "medium" (nlk:json-value arguments :string "importance")))
    (is (equal "auto" (nlk:json-value arguments :string "source_format"))))))

(deftest tam-cell-a-save-tam-kept-nothing-of-names-its-reason (with-tam ()
  (let ((*save-payload*
          (nlk:json-object "saved" nil
                           "quality" (nlk:json-object "reason" "too vague to be useful"))))
    (is (cl:search "TAM kept nothing: too vague" (nodecode-tam:save "stuff"))))))

;;; --- the command, and a refused token ----------------------------------------------

(deftest tam-cell-the-command-asks-the-server-what-it-is (with-tam ()
  (let ((text (nodecode-tam:status)))
    (is (cl:search "total-agent-memory 14.7.0" text) "the server names itself")
    (is (cl:search "local server" text) "and which kind it is")
    (is (cl:search "memory_recall" text) "and what it exposes"))
  (is (cl:search "total-agent-memory" (cell-entry "nodecode-tam" "tam" "" "tam-cell-1")) "/tam is the same answer")))

(deftest tam-cell-a-refused-token-names-the-status (with-tam ()
  (setf *status* 401)
  (let ((condition (signals-error nodecode-tam:tam-error (nodecode-tam:recall "x"))))
    (is (cl:search "HTTP 401" (princ-to-string condition)))
    (is (cl:search "token refused" (princ-to-string condition)) "with TAM's own sentence"))))

