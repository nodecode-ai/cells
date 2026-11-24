;;;; cell-test.lisp --- the loci cell against a stubbed Loci endpoint.
;;;;
;;;; SPDX-License-Identifier: MIT
;;;;
;;;; The tests register into the shared nodecode.test registry under a
;;;; LOCI-CELL- name prefix; RUN-LOCI-TESTS runs exactly that slice.
;;;;
;;;; The one seam that reaches outside is NODECODE-LOCI::POST, and the one
;;;; that reads the operator's disk is NODECODE-LOCI::FILE-TEXT. Both are
;;;; stubbed, so a test asserts on the JSON-RPC it would have sent and nothing
;;;; ever dials or reads a file.

(in-package #:nodecode.test)

(define-test-slice "loci" "LOCI-CELL-" :start nodecode-loci:start-cell)

(define-cell-lifecycle-tests "loci"
  (:config "token" "t0k")
  (:help :loci)
  (:command "loci")
  (:refused ("endpoint" 5) ("request_timeout" 0))
  (:idle nodecode-loci:loci-error
         (nodecode-loci:status-text)
         (nodecode-loci:call-tool "loci_generate" nil)))

;;; --- the canned app ---------------------------------------------------------------

(defparameter +init+
  "{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"serverInfo\":{\"name\":\"Loci\"}}"
  "What the handshake answers.")

(defparameter +tools+
  "{\"tools\":[{\"name\":\"loci_status\"},{\"name\":\"loci_generate\"},{\"name\":\"loci_transcribe\"}]}"
  "The three tools a test's app admits to.")

(defparameter +ready+
  "{\"content\":[{\"type\":\"text\",\"text\":\"ready: qwen3-4b answers, 2 models installed\"}]}"
  "The app's own readiness answer.")

(defvar *sent* '()
  "Every request the stub took, newest first: (:url :content :headers).")

(defvar *routes* '()
  "A (FRAGMENT . ANSWER) list: the first whose fragment the body carries answers.")

(defun mcp-reply (id result)
  "One JSON-RPC reply carrying RESULT."
  (format nil "{\"jsonrpc\":\"2.0\",\"id\":~a,\"result\":~a}" id result))

(defun reply-headers (content-type session)
  "A response's header table, as the HTTP seam hands one back."
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "content-type" table) content-type)
    (when session (setf (gethash "mcp-session-id" table) session))
    table))

(defun stub-post (url content headers timeout)
  (declare (ignore timeout))
  (push (list :url url :content content :headers headers) *sent*)
  (let ((route (find-if (lambda (route) (search (car route) (or content ""))) *routes*)))
    (if route
        (funcall (cdr route) content)
        (values "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}" 200
                (reply-headers "application/json" nil)))))

(defun route (fragment text &key (status 200) (type "application/json") session)
  "Have the stub answer a body mentioning FRAGMENT with TEXT."
  (push (cons fragment
              (lambda (body)
                (declare (ignore body))
                (values text status (reply-headers type session))))
        *routes*))

(defmacro with-loci ((&rest pairs) &body body)
  "BODY with the cell started on PAIRS, the wire stubbed by the routes above
and no operator file readable."
  `(with-cell-stop ((loci-start ,@pairs))
     (with-stubbed-fdefinitions
         ((nodecode-loci::post (url content headers timeout)
            (stub-post url content headers timeout))
          (nodecode-loci::file-text (path)
            nil))
       ,@body)))

(defun sent (fragment)
  "The requests whose body mentions FRAGMENT, newest first."
  (remove-if-not (lambda (request) (search fragment (or (getf request :content) ""))) *sent*))

(defun header-of (request name)
  "The value REQUEST carried for header NAME, case-insensitively."
  (cdr (assoc name (getf request :headers) :test #'string-equal)))

(defun body-of (request)
  "REQUEST's JSON body, decoded."
  (nlk:decode-json (getf request :content)))

(defun app-call (name arguments)
  "NAME called through the wire, as a provider sends it; => the result text."
  (princ-to-string (nle::message-content (execute-wire-call "call-1" name arguments))))

(defun app-methods (fragment)
  "The method names of the requests mentioning FRAGMENT, oldest first."
  (mapcar (lambda (request) (nlk:json-value (body-of request) :string "method"))
          (reverse (sent fragment))))

;;; --- status ------------------------------------------------------------------

(deftest loci-cell-status-reads-the-apps-tools-and-readiness ()
  (with-loci ("token" "t0k")
    (route "\"method\":\"initialize\"" (mcp-reply 1 +init+) :session "sess-1")
    (route "\"method\":\"tools/list\"" (mcp-reply 2 +tools+))
    (route "\"method\":\"tools/call\"" (mcp-reply 3 +ready+))
    (let ((answer (app-call "loci_status" "{}")))
      (is (search "connected at http://127.0.0.1:5624/mcp" answer) "the answer names where it connected")
      (is (search "qwen3-4b answers" answer) "the app's own readiness answer is repeated")
      (is (search "loci_transcribe" answer) "every advertised tool is named")
      (is (equal "Bearer t0k" (header-of (first (sent "\"method\":\"initialize\"")) "authorization")) "the token travels as a bearer header")
      (is (equal "sess-1" (header-of (first (sent "\"method\":\"tools/list\"")) "mcp-session-id")) "the session the handshake issued is echoed")
      (is (equal '("initialize" "notifications/initialized" "tools/list" "tools/call")
                 (app-methods "\"jsonrpc\"")) "one handshake, its notification, the catalog, then the app's own status"))))

(deftest loci-cell-reads-an-event-stream-reply ()
  (with-loci ("token" "t0k")
    (route "\"method\":\"initialize\"" (mcp-reply 1 +init+))
    (route "\"method\":\"tools/call\""
           (format nil "event: message~%data: ~a~%~%"
                   (mcp-reply 3 "{\"content\":[{\"type\":\"text\",\"text\":\"a summary\"}]}"))
           :type "text/event-stream")
    (is (equal "a summary" (app-call "loci_generate" "{\"prompt\":\"x\"}")) "the answer event of a server-sent stream is the reply")))

;;; --- the two verbs -------------------------------------------------------------

(deftest loci-cell-forwards-a-tools-own-arguments ()
  (with-loci ("token" "t0k")
    (route "\"method\":\"initialize\"" (mcp-reply 1 +init+))
    (route "\"method\":\"tools/call\""
           (mcp-reply 3 "{\"content\":[{\"type\":\"text\",\"text\":\"three files, one failed\"}]}"))
    (is (equal "three files, one failed"
               (app-call "loci_call"
                         "{\"tool\":\"loci_files\",\"arguments\":{\"instruction\":\"summarize\",\"paths\":[\"a.pdf\"]}}")) "the app's own text is the tool's answer")
    (let ((body (body-of (first (sent "\"method\":\"tools/call\"")))))
      (is (equal "loci_files" (nlk:json-value body :string "params" "name")) "the app's tool name is passed on")
      (is (equal "summarize" (nlk:json-value body :string "params" "arguments" "instruction")) "and its arguments verbatim"))))

(deftest loci-cell-a-failed-call-reads-as-a-failure ()
  (with-loci ("token" "t0k")
    (route "\"method\":\"initialize\"" (mcp-reply 1 +init+))
    (route "\"method\":\"tools/call\""
           (mcp-reply 3 "{\"content\":[{\"type\":\"text\",\"text\":\"no such model\"}],\"isError\":true}"))
    (let ((answer (app-call "loci_generate" "{\"prompt\":\"hi\"}")))
      (is (search "ERROR:" answer) "a failed call is marked failed")
      (is (search "no such model" answer) "with the app's own words"))))

(deftest loci-cell-generate-passes-the-model-and-needs-a-prompt ()
  (with-loci ("token" "t0k")
    (route "\"method\":\"initialize\"" (mcp-reply 1 +init+))
    (route "\"method\":\"tools/call\"" (mcp-reply 3 "{\"content\":[{\"type\":\"text\",\"text\":\"Ready\"}]}"))
    (is (equal "Ready" (app-call "loci_generate"
                                  "{\"prompt\":\"Reply with Ready\",\"model\":\"qwen3-4b\",\"max_tokens\":64}")) "one prompt, one local answer")
    (let ((body (body-of (first (sent "\"method\":\"tools/call\"")))))
      (is (equal "qwen3-4b" (nlk:json-value body :string "params" "arguments" "model")) "the reported model id is passed explicitly")
      (is (= 64 (nlk:json-value body :integer "params" "arguments" "max_tokens")) "and the answer's ceiling"))
    (is (search "prompt" (app-call "loci_generate" "{}")) "a call without its prompt is refused before any request")))

;;; --- what the operator sees ---------------------------------------------------

(deftest loci-cell-slash-answers-status-and-one-prompt ()
  (with-loci ("token" "t0k")
    (route "\"method\":\"initialize\"" (mcp-reply 1 +init+))
    (route "\"method\":\"tools/list\"" (mcp-reply 2 +tools+))
    (route "\"method\":\"tools/call\"" (mcp-reply 3 "{\"content\":[{\"type\":\"text\",\"text\":\"Ready\"}]}"))
    (is (search "connected at" (cell-entry "nodecode-loci" "loci" "")) "bare /loci is the status check")
    (is (search "Usage: /loci" (cell-entry "nodecode-loci" "loci" "help")) "help explains the command")
    (is (equal "Ready" (cell-entry "nodecode-loci" "loci" "generate Reply with Ready")) "/loci generate asks the local model one prompt")))

(deftest loci-cell-a-missing-token-refuses-and-says-what-to-do ()
  (with-stubbed-fdefinitions
      ((nodecode-loci::post (url content headers timeout)
         (declare (ignore url content headers timeout))
         (error "no wire in this test"))
       (nodecode-loci::file-text (path)
         nil))
    (is (signals-error nlk:config-refusal (loci-start)) "no token anywhere refuses the start")
    (is (signals-error nlk:config-refusal (loci-start "token_file" "/nonexistent/loci-token")) "a token_file that holds nothing refuses it too")
    (is (null nodecode-loci::*token*) "and no token is left behind")))

(deftest loci-cell-a-stale-token-or-a-closed-app-says-what-to-fix ()
  (with-loci ("token" "t0k")
    (route "\"method\":\"initialize\"" "{\"error\":{\"code\":-32001,\"message\":\"unauthorized\"}}" :status 401)
    (let ((answer (app-call "loci_status" "{}")))
      (is (search "401" answer) "the refused status is named")
      (is (search "Agent Access" answer) "and the setting to repair")
      (is (search "restart-cells" answer) "and how to make the cell re-read it")))
  (with-cell-stop ((loci-start "token" "t0k"))
    (with-stubbed-fdefinition (nodecode-loci::post (url content headers timeout)
                               (declare (ignore url content headers timeout))
                               (error "connection refused"))
      (let ((answer (app-call "loci_status" "{}")))
        (is (search "open the Loci app" answer) "a closed app is named as the thing to open")
        (is (search "Agent Access" answer) "with the setting to turn on")))))
