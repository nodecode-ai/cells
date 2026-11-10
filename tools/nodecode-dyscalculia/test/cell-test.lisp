;;;; cell-test.lisp --- lifecycle, the six tools, the command, the refusals.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; What is proved here is the cell's own wiring, not the server: the config
;;;; surface (a member of the wrong shape refuses the start), the six tools
;;;; through the real dispatch seam against a stubbed server, the command, the
;;;; refusals an unreachable or unhappy server produces, and the endpoint rule.
;;;;
;;;; The one seam that reaches outside is NODECODE-DYSCALCULIA::HTTP, the
;;;; cell's only way to the network. STUB-HTTP replaces it with canned MCP
;;;; answers and records every message, so nothing ever dials.

(in-package #:nodecode.test)

(define-test-slice "dyscalculia" "DYSCALCULIA-CELL-" :start nodecode-dyscalculia:start-cell)

;;; --- lifecycle -----------------------------------------------------------------

(define-cell-lifecycle-tests "dyscalculia"
  (:config "timeout_seconds" 30)
  (:help :dyscalculia)
  (:command "dyscalculia")
  (:running
   (is (nle::find-tool "get_prevalence") "get_prevalence is registered")
   (is (nle::find-tool "list_studies") "list_studies is registered")
   (is (nle::find-tool "get_reference") "get_reference is registered")
   (is (nle::find-tool "list_chart_data") "list_chart_data is registered")
   (is (nle::find-tool "explain_screening_test") "explain_screening_test is registered")
   (is (nle::find-tool "get_citation") "get_citation is registered")
   (is (= 30 (nodecode-dyscalculia::setting :timeout-seconds)) "the section's timeout_seconds is the one in force"))
  (:stopped
   (is (null (nle::find-tool "get_prevalence")) "stop unregisters the tools")
   (is (null (nle::find-tool "get_citation")) "every one of them"))
  (:refused
   ("timeout_seconds" 0)
   ("timeout_seconds" "soon")
   ("max_chars" 10))
  (:idle nodecode-dyscalculia:dyscalculia-error
   (nodecode-dyscalculia:get-citation)))

;;; --- the stubbed server ----------------------------------------------------------

(defparameter +prevalence-text+ "{\"count\": 1, \"entries\": [{\"study\": \"uk-morsanyi-2018\", \"pct\": \"6\", \"sampleSize\": 2489, \"referenceKey\": \"morsanyi2018\"}]}" "What the stub answers for get_prevalence: the server's own JSON text.")

(defvar *calls* '() "Every message the stub took, newest first: (:method :url :content :headers).")

(defvar *down* nil "When true the stub answers a host that could not be reached: (values NIL 0 NIL).")

(defvar *status* nil "When an integer, the stub answers that HTTP status with no message.")

(defvar *error-answer* nil "When a string, every tools/call answers it as an isError result.")

(defun stub-sse (message) "MESSAGE as a server-sent stream, the way the server frames one."
  (values (format nil "event: message~%data: ~a~%~%" (nlk:encode-json-object message))
          200 '(("content-type" . "text/event-stream"))))

(defun stub-result (id text &key (is-error nil)) "A JSON-RPC result carrying TEXT as a tool result."
  (nlk:make-json-object "jsonrpc" "2.0" "id" id "result" (nlk:make-json-object "content" (vector (nlk:make-json-object "type" "text" "text" text)) "isError" is-error)))

(defun stub-http (method url &key headers content timeout) "The cell's one network call, answered from canned MCP messages."
  (declare (ignore timeout))
  (push (list :method method :url url :content content :headers headers) *calls*)
  (when *down* (return-from stub-http (values nil 0 nil)))
  (when *status* (return-from stub-http (values "" *status* nil)))
  (let* ((request (nlk:decode-json content))
         (rpc-method (nlk:json-value request :string "method"))
         (id (nlk:json-value request :any "id"))
         (params (nlk:json-value request :any "params")))
    (cond
      ((equal rpc-method "initialize")
       (stub-sse (nlk:make-json-object "jsonrpc" "2.0" "id" id "result" (nlk:make-json-object "protocolVersion" "2025-06-18" "capabilities" (nlk:make-json-object) "serverInfo" (nlk:make-json-object "name" "dyscalculia" "version" "0.1.0")))))
      ((equal rpc-method "notifications/initialized") (values "" 202 nil))
      ((equal rpc-method "tools/call")
       (let ((name (nlk:json-value params :string "name")))
         (stub-sse (stub-result id (or *error-answer*
                                       (if (equal name "get_prevalence")
                                           +prevalence-text+
                                           (format nil "~a: the server's own text" name)))
                                :is-error (and *error-answer* t)))))
      (t (values "{}" 400 nil)))))

(defmacro with-server (&body body) "Run BODY with the cell's one HTTP seam stubbed and its messages recorded."
  `(let ((*calls* '()) (*down* nil) (*status* nil) (*error-answer* nil))
     (with-stubbed-fdefinition (nodecode-dyscalculia::http
                                (method url &key headers content timeout)
                                (stub-http method url :headers headers :content content
                                           :timeout timeout))
       ,@body)))

(defmacro with-dyscalculia ((&rest pairs) &body body) "Run BODY with the cell started on PAIRS, its server stubbed, stopped on unwind."
  `(with-server
     (with-cell-stop ((dyscalculia-start ,@pairs))
       ,@body)))

;;; --- reading the messages a call sent ----------------------------------------------

(defun sent-method (call) "The JSON-RPC method CALL carried."
  (nlk:json-value (nlk:decode-json (getf call :content)) :string "method"))

(defun sent-arguments (call) "The tools/call arguments CALL carried, as an object."
  (nlk:json-value (nlk:decode-json (getf call :content)) :any "params" "arguments"))

(defun tool-call (name &rest pairs) "The one tools/call the stub took for NAME, newest first."
  (find-if (lambda (call)
             (and (equal "tools/call" (sent-method call))
                  (equal name (nlk:json-value (nlk:decode-json (getf call :content))
                                               :string "params" "name"))))
           (reverse *calls*)))

(defun tool-answers (name &rest pairs) "NAME called through the real tool seam, as a provider sends it."
  (declare (ignore pairs))
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name "{}"))))

;;; --- the six tools ----------------------------------------------------------------

(deftest dyscalculia-cell-every-tool-answers-through-the-server (with-dyscalculia ()
  (let ((text (tool-answers "get_prevalence")))
    (is (cl:search "uk-morsanyi-2018" text) "get_prevalence answers the server's own text"))
  (dolist (name '("list_studies" "list_chart_data" "explain_screening_test" "get_citation"))
    (let ((text (tool-answers name)))
      (is (cl:search name text) (format nil "~a answers the server's text for it" name))))
  (let ((text (princ-to-string (nle::message-content
                                (execute-wire-call "call-2" "get_reference" "{\"key\": \"shalev2000\"}")))))
    (is (cl:search "get_reference" text) "get_reference answers with the key it was asked for"))
  (is (= 6 (count-if (lambda (call) (equal "tools/call" (sent-method call))) *calls*)) "one tools/call per tool, and none else")
  (is (every (lambda (call) (member (sent-method call)
                                    '("initialize" "notifications/initialized" "tools/call")
                                    :test #'equal))
              *calls*) "every message is the handshake or a tool call")))
(deftest dyscalculia-cell-prevalence-answers-the-servers-json (with-dyscalculia ()
  (let ((text (tool-answers "get_prevalence")))
    (is (cl:search "uk-morsanyi-2018" text) "the study the server named is in the answer")
    (is (cl:search "morsanyi2018" text) "with its reference key, so a model can cite it"))
  (let ((arguments (sent-arguments (tool-call "get_prevalence"))))
    (is (hash-table-p arguments) "the call carries an arguments object")
    (is (zerop (hash-table-count arguments)) "and no filter, because none was asked for"))))
(deftest dyscalculia-cell-filters-reach-the-server (with-dyscalculia ()
  (nodecode-dyscalculia:get-prevalence :age 11 :min-pct 3.5)
  (let ((arguments (sent-arguments (tool-call "get_prevalence"))))
    (is (equal 11 (nlk:json-value arguments :any "age")) "the age filter is sent")
    (is (= 3.5 (nlk:json-value arguments :any "min_pct")) "and the percent window")
    (is (null (nlk:json-value arguments :any "study")) "and no absent filter is sent as a null"))
  (nodecode-dyscalculia:explain-screening-test :game "numberLine" :include-items t)
  (let ((arguments (sent-arguments (tool-call "explain_screening_test"))))
    (is (equal "numberLine" (nlk:json-value arguments :any "game")) "a game key is sent as given")
    (is (equal t (nlk:json-value arguments :any "include_items")) "and include_items as a boolean"))))

;;; --- the handshake -----------------------------------------------------------------
(deftest dyscalculia-cell-a-call-initializes-then-asks (with-dyscalculia ()
  (nodecode-dyscalculia:get-citation)
  (let ((methods (mapcar #'sent-method (reverse *calls*))))
    (is (equal '("initialize" "notifications/initialized" "tools/call") methods) "a call opens a session, says so, then calls the tool"))
  (let ((call (first (reverse *calls*))))
    (is (equal :post (getf call :method)) "every message is a POST")
    (is (cl:search "application/json" (cdr (assoc "content-type" (getf call :headers) :test #'string-equal))) "of JSON")
    (is (cl:search "text/event-stream" (cdr (assoc "accept" (getf call :headers) :test #'string-equal))) "accepting the server's own stream")
    (is (null (assoc "authorization" (getf call :headers) :test #'string-equal)) "with no credential: the server needs no key"))))

;;; --- the command ------------------------------------------------------------------
(deftest dyscalculia-cell-the-command-answers (with-dyscalculia ()
  (let ((text (nodecode-dyscalculia:run-slash "citation" "s1")))
    (is (cl:search "get_citation" text) "a subcommand answers that tool's text"))
  (let ((text (nodecode-dyscalculia:run-slash "prevalence 11" "s1")))
    (is (cl:search "uk-morsanyi-2018" text) "an age typed after prevalence is the filter")
    (is (equal 11 (nlk:json-value (sent-arguments (tool-call "get_prevalence")) :any "age")) "which reaches the server as a number"))
  (let ((text (nodecode-dyscalculia:run-slash "" "s1")))
    (is (cl:search "/dyscalculia prevalence" text) "bare, it says what it can do"))
  (multiple-value-bind (text dialog) (nodecode-dyscalculia:run-slash "help" "s1")
    (is (cl:search "/dyscalculia" text) "help answers the usage line")
    (is (equal "Dyscalculia" (getf dialog :title)) "and a panel titled after the tool")
    (is (plusp (length (getf dialog :rows))) "with a row per thing it can answer"))
  (is (cl:search "/dyscalculia" (nodecode-dyscalculia:run-slash "nonsense" "s1")) "an unknown word answers the usage line")))

;;; --- the refusals ------------------------------------------------------------------
(deftest dyscalculia-cell-a-server-error-arrives-as-a-refusal (with-dyscalculia ()
  (setf *error-answer* "unknown study id; valid: worldwide-range, german-guideline")
  (handler-case
      (progn (nodecode-dyscalculia:get-prevalence :study "nope") (is nil "the call refuses"))
    (nodecode-dyscalculia:dyscalculia-error (condition)
      (is (cl:search "unknown study id" (princ-to-string condition)) "the server's own sentence is kept")))))
(deftest dyscalculia-cell-an-unreachable-server-refuses (with-dyscalculia ()
  (setf *down* t)
  (handler-case
      (progn (nodecode-dyscalculia:get-citation) (is nil "the call refuses"))
    (nodecode-dyscalculia:dyscalculia-error (condition)
      (is (cl:search "could not be reached" (princ-to-string condition)) "saying so")))
  (setf *down* nil *status* 503)
  (handler-case
      (progn (nodecode-dyscalculia:list-studies) (is nil "a 503 refuses too"))
    (nodecode-dyscalculia:dyscalculia-error (condition)
      (is (cl:search "503" (princ-to-string condition)) "naming the status")))))
(deftest dyscalculia-cell-the-endpoint-must-be-https (with-dyscalculia ("base_url" "http://example.com/mcp")
  (handler-case
      (progn (nodecode-dyscalculia:get-citation) (is nil "a plain-http endpoint refuses"))
    (nodecode-dyscalculia:dyscalculia-error (condition)
      (is (cl:search "https" (princ-to-string condition)) "saying what it wants instead")))
  (is (null *calls*) "and dials nothing")))
(deftest dyscalculia-cell-a-blank-endpoint-falls-back-to-the-published-one (with-dyscalculia ("base_url" "  ")
  (is (cl:search "get_citation" (nodecode-dyscalculia:run-slash "citation" "s1")) "a blank base_url still answers")
  (is (cl:search "mcp.dyscalculia.io" (getf (first (last *calls*)) :url)) "from the published endpoint")))
